/*  Copyright (C) 2022 Andrea G. Monaco
 *
 *  This file is part of al.
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */



#include "config.h"



#define _GNU_SOURCE



#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <locale.h>
#include <errno.h>

#include <getopt.h>

#include <gmp.h>
#include <readline/readline.h>
#include <readline/history.h>


#define CAR(list) ((list) == &nil_object ? &nil_object :\
		   (list)->value_ptr.cons_pair->car)

#define CDR(list) ((list) == &nil_object ? &nil_object :		\
		   (list)->value_ptr.cons_pair->cdr ?			\
		   (list)->value_ptr.cons_pair->cdr : &nil_object)

#define SYMBOL(s) ((s)->type == TYPE_SYMBOL ? (s) :	\
		   (s)->value_ptr.symbol_name->sym) 


#define TERMINATING_MACRO_CHARS "()';\"`,"



/* not a C string. not null-terminated and explicit size. null bytes are
   allowed inside */

struct
string
{
  char *value;
  size_t alloc_size;
  size_t used_size;
};


struct
global_environment
{
  struct binding *dyn_vars;
  struct binding *funcs;
  struct binding *macros;
  struct binding *spec_ops;
  struct binding *types;
  struct binding *class_names;
  struct binding *procls;
};


struct
dynamic_environment
{
  struct binding *dyn_vars;
};


struct
lexical_environment
{
  struct binding *lex_vars;
  struct binding *sym_macros;
  struct binding *funcs;
  struct binding *macros;
  struct binding *block_tags;
  struct binding *go_tags;
};


struct
environment
{
  struct binding *vars;
  struct binding *funcs;

  struct binding *packages;

  struct object *keyword_package;
  struct object *current_package;

  /*struct global_environment *glob_env;
  struct dynamic_environment *dyn_env;
  struct lexical_environment *lex_env;*/
};


enum
read_outcome
  {
    NO_OBJECT = 0,

    COMPLETE_OBJECT = 1,

    OPENING_PARENTHESIS = 1 << 2,
    CLOSING_PARENTHESIS = 1 << 3,
    CLOSING_PARENTHESIS_AFTER_PREFIX = 1 << 4,

    JUST_PREFIX = 1 << 5,
    INCOMPLETE_EMPTY_LIST = 1 << 6,
    INCOMPLETE_NONEMPTY_LIST = 1 << 7,
    INCOMPLETE_STRING = 1 << 8,
    INCOMPLETE_SYMBOL_NAME = 1 << 9,
    INCOMPLETE_SHARP_MACRO_CALL = 1 << 10,

    INVALID_SHARP_DISPATCH = 1 << 11,
    UNKNOWN_SHARP_DISPATCH = 1 << 12,
    WRONG_OBJECT_TYPE_TO_SHARP_MACRO = 1 << 13,
    UNKNOWN_CHARACTER_NAME = 1 << 14,
    FUNCTION_NOT_FOUND = 1 << 15,

    UNFINISHED_SINGLELINE_COMMENT = 1 << 16,
    UNFINISHED_MULTILINE_COMMENT = 1 << 17,

    COMMA_WITHOUT_BACKQUOTE = 1 << 18,
    TOO_MANY_COMMAS = 1 << 19,

    SINGLE_DOT = 1 << 20,

    MULTIPLE_DOTS = 1 << 21,

    NO_OBJ_BEFORE_DOT_IN_LIST = 1 << 22,
    NO_OBJ_AFTER_DOT_IN_LIST = 1 << 23,
    MULTIPLE_OBJS_AFTER_DOT_IN_LIST = 1 << 24,

    TOO_MANY_COLONS = 1 << 25,
    CANT_BEGIN_WITH_TWO_COLONS_OR_MORE = 1 << 26,
    CANT_END_WITH_PACKAGE_SEPARATOR = 1 << 27,
    MORE_THAN_A_PACKAGE_SEPARATOR = 1 << 28,
    PACKAGE_NOT_FOUND = 1 << 29
  };


#define INCOMPLETE_OBJECT (JUST_PREFIX | INCOMPLETE_NONEMPTY_LIST |	\
			   INCOMPLETE_STRING | INCOMPLETE_SYMBOL_NAME | \
			   INCOMPLETE_SHARP_MACRO_CALL | INCOMPLETE_EMPTY_LIST)

#define READ_ERROR (CLOSING_PARENTHESIS_AFTER_PREFIX | CLOSING_PARENTHESIS \
		    | INVALID_SHARP_DISPATCH | UNKNOWN_SHARP_DISPATCH	\
		    | WRONG_OBJECT_TYPE_TO_SHARP_MACRO | UNKNOWN_CHARACTER_NAME	\
		    | FUNCTION_NOT_FOUND				\
		    | COMMA_WITHOUT_BACKQUOTE | TOO_MANY_COMMAS | SINGLE_DOT \
		    | MULTIPLE_DOTS | NO_OBJ_BEFORE_DOT_IN_LIST		\
		    | NO_OBJ_AFTER_DOT_IN_LIST				\
		    | MULTIPLE_OBJS_AFTER_DOT_IN_LIST | TOO_MANY_COLONS \
		    | CANT_BEGIN_WITH_TWO_COLONS_OR_MORE		\
		    | CANT_END_WITH_PACKAGE_SEPARATOR			\
		    | MORE_THAN_A_PACKAGE_SEPARATOR | PACKAGE_NOT_FOUND)


enum
eval_outcome_type
  {
    EVAL_OK,
    UNBOUND_SYMBOL,
    INVALID_FUNCTION_CALL,
    DOTTED_LIST_NOT_ALLOWED_HERE,
    COMMA_AT_OR_DOT_NOT_ALLOWED_AT_TOP_LEVEL,
    WRONG_NUMBER_OF_ARGUMENTS,
    UNKNOWN_FUNCTION,
    MALFORMED_IF,
    INCORRECT_SYNTAX_IN_LET,
    INCORRECT_SYNTAX_IN_DEFUN,
    CANT_REDEFINE_CONSTANT,
    CANT_REBIND_CONSTANT,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS,
    WRONG_TYPE_OF_ARGUMENT,
    COULD_NOT_OPEN_FILE_FOR_READING,
    COULD_NOT_SEEK_FILE,
    COULD_NOT_TELL_FILE,
    ERROR_READING_FILE,
    UNKNOWN_TYPE
  };


struct
eval_outcome
{
  enum eval_outcome_type type;

  struct object *obj;
  struct typespec *typesp;
};


struct
object_list
{
  struct object *obj;
  struct object_list *next;
};


enum
package_record_visibility
  {
    INTERNAL_VISIBILITY,
    EXTERNAL_VISIBILITY
  };


struct
package
{
  struct object *name;

  struct object_list *nicks;

  struct object_list *symlist;

  struct object_list *uses;
};


struct
symbol
{
  char *name;
  size_t name_len;

  int is_type;
  int is_builtin_type;
  int is_standard_type;
  int (*builtin_type) (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct eval_outcome *outcome);
  struct object *typespec;

  int is_builtin_form;
  struct parameter *lambda_list;
  int evaluate_args;
  struct object *(*builtin_form)
    (struct object *list, struct environment *env, struct eval_outcome *outcome);

  int is_const;
  int is_parameter;
  int is_special;

  struct object *value_cell;
  struct object *function_cell;

  struct object *home_package;
  enum package_record_visibility visibility;
};


struct
symbol_name
{
  char *value;
  size_t alloc_size;
  size_t used_size;

  int packname_present;
  char *actual_symname;
  size_t actual_symname_alloc_s;
  size_t actual_symname_used_s;

  struct object *sym;
};


enum
binding_type
  {
    LEXICAL_BINDING = 1,
    DYNAMIC_BINDING = 2
  };


struct
binding
{
  enum binding_type type;
  struct object *sym;
  struct object *obj;
  struct binding *next;
};


enum
parse_lambda_list_outcome
  {
    EMPTY_LAMBDA_LIST,
    NOT_NIL_NOR_CONS,
    LAMBDA_LIST_OK
  };


enum
parameter_type
  {
    REQUIRED_PARAM,
    OPTIONAL_PARAM,
    REST_PARAM,
    KEYWORD_PARAM
  };


struct
parameter
{
  enum parameter_type type;
  struct object *name;

  struct object *init_form;
  struct object *supplied_p_param;
  
  struct parameter *next;
};


struct
function
{
  struct parameter *lambda_list;
  int allow_other_keys;

  int eval_args;

  struct object *body;
};


struct
cons_pair
{
  int filling_car;  /* when car is incomplete but already partly allocated */
  int empty_list_in_car;  /* when car is a still empty list so nothing
			     allocated yet */
  struct object *car;
  struct object *cdr;
};


struct
array_size
{
  size_t size;

  struct array_size *next;
};


struct
array
{
  struct array_size *alloc_size;

  int fill_pointer;

  struct object **value;
};


struct
filename
{
  struct object *value;
};


enum
stream_type
  {
    CHARACTER_STREAM,
    BINARY_STREAM
  };


enum
stream_direction
  {
    NO_DIRECTION,
    INPUT_STREAM,
    OUTPUT_STREAM,
    BIDIRECTIONAL_STREAM
  };


struct
stream
{
  enum stream_type type;

  enum stream_direction direction;

  int is_open;
};


/* a slight abuse of terminology: commas, quotes, backquotes, ats and dots
 * are not Lisp objects.  But treating them as objects of type prefix,
 * we can implement them as a linked list before the proper object */

enum
object_type
  {
    TYPE_QUOTE = 1,
    TYPE_BACKQUOTE = 1 << 1,
    TYPE_COMMA = 1 << 2,
    TYPE_AT = 1 << 3,
    TYPE_DOT = 1 << 4,
    TYPE_SYMBOL_NAME = 1 << 5,
    TYPE_SYMBOL = 1 << 6,
    TYPE_INTEGER = 1 << 7,
    TYPE_RATIO = 1 << 8,
    TYPE_FLOAT = 1 << 9,
    TYPE_CONS_PAIR = 1 << 10,
    TYPE_STRING = 1 << 11,
    TYPE_CHARACTER = 1 << 12,
    TYPE_ARRAY = 1 << 13,
    TYPE_HASHTABLE = 1 << 14,
    TYPE_ENVIRONMENT = 1 << 15,
    TYPE_PACKAGE = 1 << 16,
    TYPE_FILENAME = 1 << 17,
    TYPE_STREAM = 1 << 18,
    TYPE_STRUCTURE = 1 << 19,
    TYPE_CONDITION = 1 << 20,
    TYPE_FUNCTION = 1 << 21,
    TYPE_MACRO = 1 << 22,
    TYPE_SHARP_MACRO_CALL = 1 << 23,
    TYPE_T = 1 << 24,
    TYPE_NIL = 1 << 25
  };


#define TYPE_PREFIX (TYPE_QUOTE | TYPE_BACKQUOTE | TYPE_COMMA | TYPE_AT | TYPE_DOT)
#define TYPE_LIST (TYPE_NIL | TYPE_CONS_PAIR)
#define TYPE_REAL (TYPE_INTEGER | TYPE_RATIO | TYPE_FLOAT)
#define TYPE_NUMBER (TYPE_REAL)


union
object_ptr_union
{
  struct object *next;
  struct symbol_name *symbol_name;
  struct symbol *symbol;
  mpz_t integer;
  mpq_t ratio;
  mpf_t floating;
  struct cons_pair *cons_pair;
  struct string *string;
  char *character;
  struct array *array;
  struct environment *environment;
  struct package *package;
  struct filename *filename;
  struct stream *stream;
  struct structure *structure;
  struct function *function;
  struct function *macro;
  struct sharp_macro_call *sharp_macro_call;
};


struct
object
{
  int refcount;
  const char *begin;
  const char *end;
  enum object_type type;
  union object_ptr_union value_ptr;  /* only when type is TYPE_NIL, this is
					allowed to be NULL */
};  


struct
structure_slot
{
  struct object *name;
  struct object *initform;
  struct typespec *type;
  int read_only;

  struct structure_slot *next;
};


struct
structure
{
  char *conc_name;
  size_t initial_offset;
  int named;

  struct structure_slot *slots;
};


struct
sharp_macro_call
{
  int arg;
  int dispatch_ch;

  int is_empty_list;
  struct object *obj;
};


enum
typespec_type
  {
    TYPESPEC_SYMBOL,
    TYPESPEC_CLASS,
    TYPESPEC_LIST,
    TYPESPEC_INTERNAL_OR,
    TYPESPEC_INTERNAL_AND,
    TYPESPEC_OR,
    TYPESPEC_AND,
    TYPESPEC_NOT,
    TYPESPEC_SATISFIES
  };


struct
typespec
{
  enum typespec_type type;
  enum object_type int_value;
  struct object *obj_value;
  struct typespec *first;
  struct typespec *second;
};


enum
element
  {
    NONE,
    BEGIN_LIST,
    END_LIST,
    STRING_DELIMITER,
    QUOTE,
    BACKQUOTE,
    COMMA,
    AT,
    SEMICOLON,
    DOT,
    TOKEN,
    BEGIN_MULTILINE_COMMENT,
    VERTICAL_BAR,
    SHARP
  };


enum
readtable_case
  {
    CASE_UPCASE,
    CASE_DOWNCASE,
    CASE_PRESERVE,
    CASE_INVERT
  };


struct
line_list
{
  int refcount;
  char *line;
  size_t size;
  struct line_list *next;
};



void add_standard_definitions (struct environment *env);

char *read_line_interactively (const char prompt []);

enum read_outcome read_object_continued
(struct object **obj, int backts_commas_balance, int is_empty_list,
 const char *input, size_t size, struct environment *env,
 struct eval_outcome *outcome,  const char **obj_begin, const char **obj_end,
 size_t *mult_comm_depth);
struct object *complete_object_interactively
(struct object *obj, int is_empty_list, struct environment *env,
 struct eval_outcome *outcome, size_t multiline_comm_depth,
 const char **input_left, size_t *input_left_size);
struct object *read_object_interactively_continued
(const char *input, size_t input_size, struct environment *env,
 struct eval_outcome *outcome, const char **input_left, size_t *input_left_size);
struct object *read_object_interactively
(struct environment *env, struct eval_outcome *outcome, const char **input_left,
 size_t *input_left_size);

const char *skip_space_block
(const char *input, size_t size, size_t *new_size);
const char *jump_to_end_of_line
(const char *input, size_t size, size_t *new_size);
const char *find_multiline_comment_delimiter
(const char *input, size_t size, size_t *new_size);
const char *jump_to_end_of_multiline_comment
(const char *input, size_t size, size_t depth, size_t *depth_or_new_size);

struct line_list *append_line_to_list
(char *line, size_t size, struct line_list *list, int do_copy);
struct object_list *append_object_to_list
(struct object *obj, struct object_list *list);

enum read_outcome read_object
(struct object **obj, int backt_commas_balance, const char *input, size_t size,
 struct environment *env, struct eval_outcome *outcome, const char **obj_begin,
 const char **obj_end, size_t *out_arg);

enum read_outcome read_list
(struct object **obj, int backts_commas_balance, const char *input, size_t size,
 struct environment *env, struct eval_outcome *outcome, const char **list_end,
 size_t *out_arg);

enum read_outcome read_string
(struct object **obj, const char *input, size_t size, const char **string_end);

enum read_outcome read_symbol_name
(struct object **obj, const char *input, size_t size, const char **symname_end,
 enum readtable_case read_case);

enum read_outcome read_prefix
(struct object **obj, const char *input, size_t size, int *backt_commas_balance,
 struct object **last, const char **prefix_end);

enum read_outcome read_sharp_macro_call
(struct object **obj, const char *input, size_t size, struct environment *env,
 struct eval_outcome *e_outcome, const char **macro_end, size_t *out_arg);
struct object *call_sharp_macro
(struct sharp_macro_call *macro_call, struct environment *env,
 struct eval_outcome *e_outcome, enum read_outcome *r_outcome);

enum element find_next_element
(const char *input, size_t size, const char **elem_begin);

int is_number
(const char *token, size_t size, int radix, enum object_type *numtype,
 const char **number_end, const char **token_end);
struct object *alloc_number
(const char *token, size_t size, int radix, enum object_type numtype);

void print_range (const char *begin, const char *end);

char *append_newline (char *string);
char *append_zero_byte (char *string, size_t size);
char *copy_token_to_buffer (const char *input, size_t size);

size_t mbslen (const char *string);

void *malloc_and_check (size_t size);
void *realloc_and_check (void *ptr, size_t size);

struct object *alloc_object (void);
struct object *alloc_prefix (enum element type);
struct object *alloc_empty_cons_pair (void);
struct object *alloc_function (void);
struct object *alloc_sharp_macro_call (void);

struct object *create_function (struct object *lambda_list, struct object *body);
struct object *create_package (char *name, size_t name_len);

const char *find_end_of_string
(const char *input, size_t size, size_t *new_size, size_t *string_length);
void normalize_string (char *output, const char *input, size_t size);

struct object *alloc_string (size_t size);
void resize_string (struct object *string, size_t size);

struct object *alloc_symbol_name (size_t value_s, size_t actual_symname_s);
void resize_symbol_name (struct object *symname, size_t value_s,
			 size_t actual_symname_s);

const char *find_end_of_symbol_name
(const char *input, size_t size, int found_package_sep, size_t *new_size,
 const char **start_of_package_separator,
 enum package_record_visibility *sym_visibility, size_t *name_length,
 size_t *act_name_length, enum read_outcome *outcome);
void normalize_symbol_name (char *output, const char *input, size_t size,
			    enum readtable_case read_case);

struct object *create_symbol (char *name, size_t size);
struct object *create_character (char *character, int do_copy);
struct object *create_filename (struct object *string);
struct object *create_vector (struct object *list);

struct object *find_package (const char *name, size_t len,
			     struct environment *env);
struct object *intern_symbol (char *name, size_t len,
			      struct object_list **symlist);
struct object *intern_symbol_name (struct object *symname,
				   struct environment *env);

struct binding *create_binding (struct object *sym, struct object *obj,
				enum binding_type type);
struct binding *add_binding (struct binding *bin, struct binding *env);
struct binding *chain_bindings (struct binding *bin, struct binding *env);
struct binding *remove_bindings (struct binding *env, int num);
struct binding *find_binding (struct symbol *sym, struct binding *env,
			      enum binding_type type);


void add_builtin_type (char *name, struct environment *env,
		       int (*builtin_type)
		       (const struct object *obj, const struct object *typespec,
			struct environment *env, struct eval_outcome *outcome),
		       int is_standard);
void add_builtin_form (char *name, struct environment *env,
		       struct object *(*builtin_form)
		       (struct object *list, struct environment *env,
			struct eval_outcome *outcome), int eval_args);

struct object *define_constant
(struct object *sym, struct object *form, struct environment *env,
 struct eval_outcome *outcome);
struct object *define_constant_by_name
(char *name, size_t size, struct object *form, struct environment *env,
 struct eval_outcome *outcome);
struct object *define_parameter
(struct object *sym, struct object *form, struct environment *env,
 struct eval_outcome *outcome);
struct object *define_parameter_by_name
(char *name, size_t size, struct object *form, struct environment *env,
 struct eval_outcome *outcome);

struct object *skip_prefix
(struct object *prefix, int *num_backticks_before_last_comma, int *num_commas,
 struct object **last_prefix, struct object **last_comma,
 struct object **before_last_comma);
struct object *append_prefix (struct object *obj, enum element type);

struct object *nth (unsigned int ind, struct object *list);
struct object *nthcdr (unsigned int ind, struct object *list);

unsigned int list_length (const struct object *list);
struct object *last_cons_pair (struct object *list);
int is_dotted_list (const struct object *list);
struct object *copy_prefix (const struct object *begin, const struct object *end,
			    struct object **last_prefix);
struct object *copy_list_structure (const struct object *list,
				    const struct object *prefix);

int array_rank (const struct array *array);

struct parameter *alloc_parameter (enum parameter_type type,
				   struct object *sym);
struct parameter *parse_required_parameters
(struct object *obj, struct parameter **last, struct object **next,
 enum parse_lambda_list_outcome *out);
struct parameter *parse_optional_parameters
(struct object *obj, struct parameter **last, struct object **next,
 enum parse_lambda_list_outcome *out);
struct parameter *parse_lambda_list (struct object *obj,
				     enum parse_lambda_list_outcome *out);

struct object *evaluate_body
(struct object *body, int eval_twice, struct environment *env,
 struct eval_outcome *outcome);
struct object *call_function
(struct object *func, struct object *arglist, int eval_args, int eval_twice,
 struct environment *env, struct eval_outcome *outcome);

int check_type (const struct object *obj, const struct object *typespec,
		struct environment *env, struct eval_outcome *outcome);

struct object *evaluate_object
(struct object *obj, struct environment *env, struct eval_outcome *outcome);
struct object *apply_backquote
(struct object *form, struct object *cons, int first_cons,
 struct object *prev_prefix, int backts_commas_balance, struct environment *env,
 struct eval_outcome *outcome);
struct object *evaluate_list
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_through_list
(struct object *list, struct environment *env, struct eval_outcome *outcome);

int type_string (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome);

struct object *builtin_car
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_cdr
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_cons
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_list
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_write
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_load
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_eq
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_not
(struct object *list, struct environment *env, struct eval_outcome *outcome);

enum object_type highest_num_type (enum object_type t1, enum object_type t2);
struct object *copy_number (const struct object *num);
struct object *promote_number (struct object *num, enum object_type type);
struct object *apply_arithmetic_operation
(struct object *list, void (*opz) (mpz_t, const mpz_t, const mpz_t),
 void (*opq) (mpq_t, const mpq_t, const mpq_t),
 void (*opf) (mpf_t, const mpf_t, const mpf_t),  struct environment *env,
 struct eval_outcome *outcome);
struct object *builtin_plus (struct object *list, struct environment *env,
			     struct eval_outcome *outcome);
struct object *builtin_minus (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);
struct object *builtin_multiply (struct object *list, struct environment *env,
				 struct eval_outcome *outcome);
struct object *builtin_divide (struct object *list, struct environment *env,
			       struct eval_outcome *outcome);
struct object *builtin_typep (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);

struct binding *create_binding_from_let_form
(struct object *form, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_let
(struct object *bind_forms, struct object *body, struct environment *env,
 struct eval_outcome *outcome);
struct object *evaluate_let_star
(struct object *bind_forms, struct object *body, struct environment *env,
 struct eval_outcome *outcome);

struct object *evaluate_if
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_progn
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_defconstant
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_defparameter
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_defvar
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_defun
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_defmacro
(struct object *list, struct environment *env, struct eval_outcome *outcome);

int eqmem (const char *s1, size_t n1, const char *s2, size_t n2);
int symname_equals (const struct symbol_name *sym, const char *s);
int symname_is_among (const struct symbol_name *sym, ...);
int equal_strings (const struct string *s1, const struct string *s2);

void print_symbol (const struct symbol *sym, struct environment *env);
void print_string (const struct string *str);
void print_character (const char *character);
void print_filename (const struct filename *fn);
void print_list (const struct cons_pair *list, struct environment *env);
void print_array (const struct array *array, struct environment *env);
void print_object (const struct object *obj, struct environment *env);

void print_read_error (enum read_outcome err, const char *input, size_t size,
		       const char *begin, const char *end);
void print_eval_error (struct eval_outcome *err, struct environment *env);

void increment_refcount (struct object *obj);
int decrement_refcount (struct object *obj);

void free_object (struct object *obj);
void free_string (struct object *obj);
void free_symbol (struct object *obj);
void free_cons_pair (struct object *obj);
void free_array_size (struct array_size *size);
void free_array (struct object *obj);
void free_list_structure (struct object *obj);
void free_list_recursively (struct object *obj);

void print_welcome_message (void);
void print_version (void);
void print_help (void);



const struct option long_options[] =
  {
    {"version", no_argument, NULL, 'v'},
    {"help", no_argument, NULL, 'h'},
    {0, 0, 0, 0}
  };



struct object nil_object = {1, NULL, NULL, TYPE_NIL};

struct symbol _nil_symbol = {"NIL", 3};

struct object nil_symbol = {1, NULL, NULL, TYPE_SYMBOL,
  .value_ptr.symbol = &_nil_symbol};


struct object t_object = {1, NULL, NULL, TYPE_T};



int
main (int argc, char *argv [])
{
  int end_repl = 0;

  struct object *result, *obj;
  struct environment env = {NULL};
  
  struct eval_outcome eval_out = {EVAL_OK};

  const char *input_left = NULL;
  size_t input_left_s = 0;
  
  int c, option_index = 0, err;

  
  while ((c = getopt_long (argc, argv, "vh",
			   long_options, &option_index)) != -1)
    {
      switch (c)
	{
	case 'v':
	  print_version ();
	  exit (0);
	case 'h':
	  print_help ();
	  exit (0);
	}
    }

  setlocale (LC_CTYPE, "");

  add_standard_definitions (&env);

  print_welcome_message ();

  err = read_history ("al_history");

  if (err && err != ENOENT)
    printf ("could not read line history from al_history: %s\n", strerror (err));

  while (!end_repl)
    {
      obj = read_object_interactively (&env, &eval_out, &input_left,
				       &input_left_s);
      
      while (obj && input_left && input_left_s > 0)
	{
	  result = evaluate_object (obj, &env, &eval_out);

	  if (result)
	    {
	      print_object (result, &env);
	      printf ("\n");
	    }
	  else
	    print_eval_error (&eval_out, &env);

	  decrement_refcount (result);
	  decrement_refcount (obj);

	  obj = read_object_interactively_continued (input_left, input_left_s,
						     &env, &eval_out,
						     &input_left,
						     &input_left_s);
	}

      if (obj)
	{
	  result = evaluate_object (obj, &env, &eval_out);

	  if (result)
	    {
	      print_object (result, &env);
	      printf ("\n");
	    }
	  else
	    print_eval_error (&eval_out, &env);

	  decrement_refcount (result);
	  decrement_refcount (obj);
	}
    }
  
  return 0;
}


void
add_standard_definitions (struct environment *env)
{
  struct object *cluser_package;
  struct eval_outcome eval_out;

  env->keyword_package = create_package ("KEYWORD",
					 strlen ("KEYWORD"));
  env->packages = create_binding (env->keyword_package->value_ptr.package->name,
				  env->keyword_package, DYNAMIC_BINDING);

  env->current_package = create_package ("COMMON-LISP",
					 strlen ("COMMON-LISP"));
  env->packages = add_binding (create_binding
			       (env->current_package->value_ptr.package->name,
				env->current_package, DYNAMIC_BINDING),
			       env->packages);

  cluser_package = create_package ("COMMON-LISP-USER",
				   strlen ("COMMON-LISP-USER"));
  env->packages = add_binding (create_binding
			       (cluser_package->value_ptr.package->name,
				cluser_package, DYNAMIC_BINDING),
			       env->packages);

  define_constant_by_name ("NIL", strlen ("NIL"), &nil_object, env, &eval_out);
  define_constant_by_name ("T", strlen ("T"), &t_object, env, &eval_out);

  add_builtin_form ("CAR", env, builtin_car, 1);
  add_builtin_form ("CDR", env, builtin_cdr, 1);
  add_builtin_form ("CONS", env, builtin_cons, 1);
  add_builtin_form ("LIST", env, builtin_list, 1);
  add_builtin_form ("WRITE", env, builtin_write, 1);
  add_builtin_form ("LOAD", env, builtin_load, 1);
  add_builtin_form ("EQ", env, builtin_eq, 1);
  add_builtin_form ("NOT", env, builtin_not, 1);
  add_builtin_form ("NULL", env, builtin_not, 1);
  add_builtin_form ("+", env, builtin_plus, 1);
  add_builtin_form ("-", env, builtin_minus, 1);
  add_builtin_form ("*", env, builtin_multiply, 1);
  add_builtin_form ("/", env, builtin_divide, 1);
  add_builtin_form ("IF", env, evaluate_if, 0);
  add_builtin_form ("PROGN", env, evaluate_progn, 0);
  add_builtin_form ("DEFCONSTANT", env, evaluate_defconstant, 0);
  add_builtin_form ("DEFPARAMETER", env, evaluate_defparameter, 0);
  add_builtin_form ("DEFVAR", env, evaluate_defvar, 0);
  add_builtin_form ("DEFUN", env, evaluate_defun, 0);
  add_builtin_form ("DEFMACRO", env, evaluate_defmacro, 0);
  add_builtin_form ("TYPEP", env, builtin_typep, 1);

  add_builtin_type ("STRING", env, type_string, 1);
}


char *
read_line_interactively (const char prompt [])
{
  char *line = readline (prompt);
  int err;

  if (line && *line)
    add_history (line);

  err = append_history (1, "al_history");

  if (err == ENOENT)
    err = write_history ("al_history");

  if (err)
    printf ("could not write line history to al_history: %s\n", strerror (err));

  line = append_newline (line);

  return line;
}


enum read_outcome
read_object_continued (struct object **obj, int backts_commas_balance,
		       int is_empty_list, const char *input, size_t size,
		       struct environment *env, struct eval_outcome *outcome,
		       const char **obj_begin, const char **obj_end,
		       size_t *mult_comm_depth)
{
  enum read_outcome out;
  int bts, cs;
  struct object *last_pref, *ob = skip_prefix (*obj, &bts, &cs, &last_pref,
					       NULL, NULL);
  struct object *l;

  backts_commas_balance += (bts - cs);

  if (*mult_comm_depth)
    {
      input = jump_to_end_of_multiline_comment (input, size, *mult_comm_depth,
						mult_comm_depth);

      if (!input)
	return NO_OBJECT;

      input++;
      size = --(*mult_comm_depth);
      *mult_comm_depth = 0;
    }

  if (is_empty_list)
    {
      l = NULL;

      out = read_list (&l, backts_commas_balance, input, size, env, outcome,
		       obj_end, mult_comm_depth);

      ob = l;
    }
  else if (!ob)
    {
      out = read_object (&ob, backts_commas_balance, input, size, env,
			 outcome, obj_begin, obj_end, mult_comm_depth);

      if (out == NO_OBJECT && last_pref)
	out = JUST_PREFIX;
    }
  else if (ob->type == TYPE_CONS_PAIR)
    {
      out = read_list (&ob, backts_commas_balance, input, size, env, outcome,
		       obj_end, mult_comm_depth);
    }
  else if (ob->type == TYPE_STRING)
    {
      out = read_string (&ob, input, size, obj_end);
    }
  else if (ob->type == TYPE_SYMBOL_NAME)
    {
      out = read_symbol_name (&ob, input, size, obj_end, CASE_UPCASE);

      if (out == COMPLETE_OBJECT && !intern_symbol_name (ob, env))
	return PACKAGE_NOT_FOUND;
    }
  else if (ob->type == TYPE_SHARP_MACRO_CALL)
    {
      out = read_sharp_macro_call (&ob, input, size, env, outcome, obj_end,
				   mult_comm_depth);

      if (out == COMPLETE_OBJECT)
	ob = call_sharp_macro (ob->value_ptr.sharp_macro_call, env, outcome,
			       &out);
    }

  if (last_pref)
    last_pref->value_ptr.next = ob;
  else
    *obj = ob;

  return out;
}


struct object *
complete_object_interactively (struct object *obj, int is_empty_list,
			       struct environment *env,
			       struct eval_outcome *outcome,
			       size_t multiline_comm_depth,
			       const char **input_left, size_t *input_left_size)
{
  char *line;
  enum read_outcome read_out;
  const char *begin, *end;
  size_t len;

  
  line = read_line_interactively ("> ");
  len = strlen (line);
  
  read_out = read_object_continued (&obj, 0, is_empty_list, line, len, env,
				    outcome, &begin, &end, &multiline_comm_depth);

  while (read_out & (INCOMPLETE_NONEMPTY_LIST | INCOMPLETE_STRING |
		     INCOMPLETE_SYMBOL_NAME | JUST_PREFIX
		     | INCOMPLETE_SHARP_MACRO_CALL |
		     UNFINISHED_MULTILINE_COMMENT | INCOMPLETE_EMPTY_LIST)
	 || read_out & READ_ERROR
	 || multiline_comm_depth)
    {
      if (read_out & READ_ERROR)
	{
	  print_read_error (read_out, line, len, begin, end);
	  return NULL;
	}

      line = read_line_interactively ("> ");
      len = strlen (line);

      if (read_out & INCOMPLETE_EMPTY_LIST)
	read_out = read_object_continued (&obj, 0, 1, line, len, env, outcome,
					  &begin, &end,
					  &multiline_comm_depth);
      else
	read_out = read_object_continued (&obj, 0, 0, line, len, env, outcome,
					  &begin, &end,
					  &multiline_comm_depth);
    }

  *input_left = end + 1;
  *input_left_size = (line + len) - end - 1;
  
  return obj;
}


struct object *
read_object_interactively_continued (const char *input, size_t input_size,
				     struct environment *env,
				     struct eval_outcome *outcome,
				     const char **input_left,
				     size_t *input_left_size)
{
  enum read_outcome read_out;
  struct object *obj = NULL;
  const char *begin, *end;
  size_t mult_comm_depth = 0;

  
  read_out = read_object (&obj, 0, input, input_size, env, outcome, &begin,
			  &end, &mult_comm_depth);
  
  if (read_out == COMPLETE_OBJECT  && !mult_comm_depth)
    {
      *input_left = end + 1;
      *input_left_size = (input + input_size) - end - 1;
      
      return obj;
    }
  else if (read_out == NO_OBJECT && !mult_comm_depth)
    {
      *input_left = NULL;
      *input_left_size = 0;
      
      return NULL;
    }
  else if (read_out & READ_ERROR)
    {
      print_read_error (read_out, input, input_size, begin, end);

      return NULL;
    }
  else if (read_out == INCOMPLETE_EMPTY_LIST)
    {
      return complete_object_interactively (obj, 1, env, outcome,
					    mult_comm_depth, input_left,
					    input_left_size);
    }
  else
    return complete_object_interactively (obj, 0, env, outcome, mult_comm_depth,
					  input_left, input_left_size);
}


struct object *
read_object_interactively (struct environment *env, struct eval_outcome *outcome,
			   const char **input_left, size_t *input_left_size)
{
  char *line = read_line_interactively ("al> ");

  return read_object_interactively_continued (line, strlen (line), env,
					      outcome, input_left,
					      input_left_size);
}


const char *
skip_space_block (const char *input, size_t size, size_t *new_size)
{
  size_t i;

  for (i = 0; i < size; i++)
    {
      if (!isspace (input [i]))
	{
	  *new_size = size-i;
	  return input+i;
	}
    }
  
  return NULL;
}


const char *
jump_to_end_of_line (const char *input, size_t size, size_t *new_size)
{
  const char *eol;
  
  if (!size)
    return NULL;
  
  eol =  memmem (input, size, "\n", 1);
  
  if (!eol)
    return NULL;

  *new_size = size - (sizeof (char) * (eol - input));

  return eol;
}


const char *
find_multiline_comment_delimiter (const char *input, size_t size,
				  size_t *new_size)
{
  const char *comm_begin, *comm_end, *delim;

  if (!size)
    return NULL;
  
  comm_begin = memmem (input, size, "#|", 2);
  comm_end = memmem (input, size, "|#", 2);
  
  if (comm_begin && comm_end)
    {
      if (comm_begin < comm_end)
	delim = comm_begin;
      else
	delim =  comm_end;
    }
  else if (comm_begin)
    delim = comm_begin;
  else if (comm_end)
    delim = comm_end;
  else
    return NULL;

  *new_size = size - (sizeof (char) * (delim - input));
  
  return delim;
}


const char *
jump_to_end_of_multiline_comment (const char *input, size_t size, size_t depth,
				  size_t *depth_or_new_size)
{
  const char *delim, *ret;

  if (!size)
    return NULL;
    
  delim = find_multiline_comment_delimiter (input, size, &size);

  while (delim && depth)
    {
      ret = delim;
      
      if (*delim == '#')
	depth++;
      else
	depth--;

      delim = find_multiline_comment_delimiter (delim+2, size-2, &size);
    }

  if (!depth)
    {
      *depth_or_new_size = size-1;
      return ret+1;
    }

  *depth_or_new_size = depth;
  
  return NULL;    
}


struct line_list *
append_line_to_list (char *line, size_t size, struct line_list *list,
		     int do_copy)
{
  struct line_list *l, *prev, *beg = list;

  l = malloc_and_check (sizeof (*l));

  if (do_copy)
    {
      l->line = malloc_and_check (size);
      memcpy (l->line, line, size);
    }
  else
    l->line = line;

  l->size = size;
  l->refcount = 0;
  l->next = NULL;
  
  if (!list)
    return l;      

  while (list)
    {
      prev = list;
      list = list->next;
    }

  prev->next = l;
  
  return beg;
}


struct object_list *
append_object_to_list (struct object *obj, struct object_list *list)
{
  struct object_list *l, *prev, *beg = list;

  l = malloc_and_check (sizeof (*l));
  l->obj = obj;
  l->next = NULL;
  
  if (!list)
    return l;      

  while (list)
    {
      prev = list;
      list = list->next;
    }

  prev->next = l;
  
  return beg;
}


enum read_outcome
read_object (struct object **obj, int backts_commas_balance, const char *input,
	     size_t size, struct environment *env, struct eval_outcome *outcome,
	     const char **obj_begin, const char **obj_end, size_t *out_arg)
{
  int found_prefix = 0;
  struct object *last_pref, *ob = NULL;
  enum object_type numtype;
  enum read_outcome out = NO_OBJECT;
  const char *num_end;

  input = skip_space_block (input, size, &size);

  if (!input)
    return NO_OBJECT;

  while (1)
    {
      if (*input == ';')
	{
	  if (!(input = jump_to_end_of_line (input, size, &size)))
	    return UNFINISHED_SINGLELINE_COMMENT;
	}
      else if (*input == '#' && size > 1 && *(input+1) == '|')
	{
	  if (!(input = jump_to_end_of_multiline_comment (input+2, size-2, 1,
							  out_arg)))
	    return NO_OBJECT;
	  else
	    {
	      size = *out_arg;
	      *out_arg = 0;
	    }
	}
      else if (*input == '\'' || *input == '`' || *input == ',')
 	{
	  out = read_prefix (obj, input, size, &backts_commas_balance,
			     &last_pref, obj_end);

	  if (out == TOO_MANY_COMMAS)
	    return out;

	  size = size - (*obj_end - input);
	  input = *obj_end;
	  found_prefix = 1;
	}
      else if (*input == ')')
	{
	  *obj_end = input;
	  return found_prefix ? CLOSING_PARENTHESIS_AFTER_PREFIX :
	    CLOSING_PARENTHESIS;
	}
      else if (*input == '(')
	{
	  *obj_begin = input;
	  out = read_list (&ob, backts_commas_balance, input+1, size-1, env,
			   outcome, obj_end, out_arg);
	  break;
	}
      else if (*input == '"')
	{
	  *obj_begin = input;
	  out = read_string (&ob, input+1, size-1, obj_end);
	  break;
	}
      else if (*input == '#')
	{
	  *obj_begin = input;
	  out = read_sharp_macro_call (&ob, input+1, size-1, env, outcome,
				       obj_end, out_arg);

	  if (out == COMPLETE_OBJECT)
	    ob = call_sharp_macro (ob->value_ptr.sharp_macro_call, env, outcome,
				   &out);

	  break;
	}
      else
	{
	  *obj_begin = input;
	  
	  if (is_number (input, size, 10, &numtype, &num_end, obj_end))
	    {
	      ob = alloc_number (input, num_end - input + 1, 10, numtype);
	      out = COMPLETE_OBJECT;
	    }
	  else
	    {
	      out = read_symbol_name (&ob, input, size, obj_end, CASE_UPCASE);

	      if (out == COMPLETE_OBJECT && !intern_symbol_name (ob, env))
		return PACKAGE_NOT_FOUND;
	    }

	  break;
	}

      input = skip_space_block (++input, --size, &size);

      if (!input)
	return found_prefix ? JUST_PREFIX : NO_OBJECT;
    }

  if (found_prefix)
    last_pref->value_ptr.next = ob;
  else
    *obj = ob;
    
  return out;
}


enum read_outcome
read_list (struct object **obj, int backts_commas_balance, const char *input,
	   size_t size, struct environment *env, struct eval_outcome *outcome,
	   const char **list_end, size_t *out_arg)
{
  struct object *last_cons = NULL, *car = NULL, *ob = *obj, *cons;
  const char *obj_beg, *obj_end = NULL;
  enum read_outcome out;
  int found_dot = 0, dotted_list_full = 0;


  if (!size)
    return INCOMPLETE_EMPTY_LIST;

  while (ob && ob != &nil_object)
    {
      if (ob->value_ptr.cons_pair->filling_car)
	{
	  out = read_object_continued (&ob->value_ptr.cons_pair->car,
				       backts_commas_balance, 0, input,
				       size, env, outcome, &obj_beg,
				       &obj_end, out_arg);

	  if (out == COMPLETE_OBJECT || out == CLOSING_PARENTHESIS)
	    ob->value_ptr.cons_pair->filling_car = 0;
	  else if (out == NO_OBJECT || out == INCOMPLETE_EMPTY_LIST
		   || out == UNFINISHED_MULTILINE_COMMENT || out & READ_ERROR
		   || out & INCOMPLETE_OBJECT)
	    return out;
	}
      else if (ob->value_ptr.cons_pair->empty_list_in_car)
	{
	  out = read_object_continued (&ob->value_ptr.cons_pair->car,
				       backts_commas_balance, 1, input,
				       size, env, outcome, &obj_beg,
				       &obj_end, out_arg);

	  if (out != INCOMPLETE_EMPTY_LIST)
	    ob->value_ptr.cons_pair->empty_list_in_car = 0;

	  if (out & INCOMPLETE_OBJECT)
	    {
	      ob->value_ptr.cons_pair->filling_car = 1;
	      return out;
	    }

	  if (out == NO_OBJECT || out == INCOMPLETE_EMPTY_LIST
	      || out == UNFINISHED_MULTILINE_COMMENT || out & READ_ERROR)
	    return out;
	}

      last_cons = ob;
      ob = ob->value_ptr.cons_pair->cdr;      
    }

  if (obj_end)
    {
      out = read_object (&car, backts_commas_balance, obj_end + 1,
			 size - (obj_end + 1 - input), env, outcome, &obj_beg,
			 &obj_end, out_arg);
    }
  else
    {
      out = read_object (&car, backts_commas_balance, input, size, env,
			 outcome, &obj_beg, &obj_end, out_arg);
    }

  if (out == NO_OBJECT && !last_cons)
    return INCOMPLETE_EMPTY_LIST;

  if (out == CLOSING_PARENTHESIS && !last_cons)
    {
      *list_end = obj_end;
      *obj = &nil_object;
      return COMPLETE_OBJECT;
    }

  while (out != NO_OBJECT && out != INCOMPLETE_EMPTY_LIST)
    {
      if (out == CLOSING_PARENTHESIS)
	{
	  if (found_dot && !dotted_list_full)
	    return NO_OBJ_AFTER_DOT_IN_LIST;

	  *list_end = obj_end;
	  return COMPLETE_OBJECT;
	}
      else if (out == SINGLE_DOT)
	{
	  found_dot = 1;
	}
      else if (out & READ_ERROR)
	{
	  return out;
	}
      else if (out == COMPLETE_OBJECT || out & INCOMPLETE_OBJECT)
	{
	  if (dotted_list_full)
	    return MULTIPLE_OBJS_AFTER_DOT_IN_LIST;
	  else if (found_dot)
	    {
	      if (last_cons)
		{
		  last_cons->value_ptr.cons_pair->cdr = car;
		  dotted_list_full = 1;
		}
	      else
		return NO_OBJ_BEFORE_DOT_IN_LIST;
	    }
	  else
	    {
	      cons = alloc_empty_cons_pair ();
	      cons->value_ptr.cons_pair->car = car;

	      if (last_cons)
		last_cons = last_cons->value_ptr.cons_pair->cdr = cons;
	      else
		*obj = last_cons = cons;

	      if (out & INCOMPLETE_OBJECT)
		{
		  cons->value_ptr.cons_pair->filling_car = 1;

		  return INCOMPLETE_NONEMPTY_LIST;
		}
	    }
	}

      if (obj_end == input + size)
	break;

      car = NULL;
      out = read_object (&car, backts_commas_balance, obj_end + 1,
			 size - (obj_end + 1 - input), env, outcome, &obj_beg,
			 &obj_end, out_arg);
    }

  if (out == INCOMPLETE_EMPTY_LIST)
    {
      cons = alloc_empty_cons_pair ();
      cons->value_ptr.cons_pair->empty_list_in_car = 1;

      if (last_cons)
	last_cons->value_ptr.cons_pair->cdr = cons;
      else
	*obj = cons;
    }

  return INCOMPLETE_NONEMPTY_LIST;
}


enum read_outcome 
read_string (struct object **obj, const char *input, size_t size,
	     const char **string_end)
{
  size_t length, new_size;
  struct string *str;
  enum read_outcome out = COMPLETE_OBJECT;
  struct object *ob = *obj;


  *string_end = find_end_of_string (input, size, &new_size, &length);

  if (!*string_end)
    out = INCOMPLETE_STRING;
    
  if (!ob)
    {
      ob = alloc_string (length);
      *obj = ob;
    }
  else
    resize_string (ob, ob->value_ptr.string->used_size + length);  

  if (!length)
    return COMPLETE_OBJECT;
    
  str = ob->value_ptr.string;
  
  normalize_string (str->value + str->used_size, input, size);
  
  str->used_size += length;
  
  return out;
}


enum read_outcome
read_symbol_name (struct object **obj, const char *input, size_t size,
		  const char **symname_end, enum readtable_case read_case)
{
  struct symbol_name *sym;
  size_t name_l, act_name_l, new_size;
  struct object *ob = *obj;
  enum read_outcome out = NO_OBJECT;
  const char *start_of_pack_s;
  enum package_record_visibility visib;


  *symname_end = find_end_of_symbol_name
    (input, size, ob && ob->value_ptr.symbol_name->packname_present ? 1 : 0,
     &new_size, &start_of_pack_s, &visib, &name_l, &act_name_l, &out);

  if (out & READ_ERROR)
    return out;

  if (!name_l && !act_name_l)
    return COMPLETE_OBJECT;

  if (!ob)
    {
      ob = alloc_symbol_name (name_l, act_name_l);
      *obj = ob;
    }
  else
    resize_symbol_name (ob, ob->value_ptr.symbol_name->used_size + name_l,
			ob->value_ptr.symbol_name->actual_symname_used_s +
			act_name_l);

  sym = ob->value_ptr.symbol_name;

  if (sym->packname_present)
    normalize_symbol_name (sym->actual_symname + sym->actual_symname_used_s,
			   input, size, read_case);
  else if (start_of_pack_s)
    {
      normalize_symbol_name (sym->value + sym->used_size, input,
			     start_of_pack_s - input, read_case);
      sym->packname_present = 1;
      normalize_symbol_name (sym->actual_symname + sym->actual_symname_used_s,
			     visib == EXTERNAL_VISIBILITY ?
			     start_of_pack_s + 1 : start_of_pack_s + 2, size,
			     read_case);
    }
  else
    normalize_symbol_name (sym->value + sym->used_size, input, size, read_case);

  sym->used_size += name_l;
  sym->actual_symname_used_s += act_name_l;

  if (!*symname_end)
    return INCOMPLETE_SYMBOL_NAME;
  else
    return COMPLETE_OBJECT;
}


enum read_outcome
read_prefix (struct object **obj, const char *input, size_t size,
	     int *backts_commas_balance, struct object **last,
	     const char **prefix_end)
{
  const char *n = input;
  enum element el;
  int num_backts, num_commas;
  int found_comma = 0;
  
  if (!size)
    return NO_OBJECT;

  skip_prefix (*obj, &num_backts, &num_commas, last, NULL, NULL);

  *backts_commas_balance += (num_backts - num_commas);

  if (*backts_commas_balance < 0)
    return TOO_MANY_COMMAS;
  
  el = find_next_element (input, size, &n);
  
  while (el == QUOTE || el == BACKQUOTE || el == COMMA || el == AT || el == DOT)
    {
      if ((el == AT || el == DOT) && !found_comma)
	return JUST_PREFIX;
      
      *prefix_end = n;

      if (!*last)
	*obj = *last = alloc_prefix (el);
      else
	*last = (*last)->value_ptr.next = alloc_prefix (el);

      if (el == BACKQUOTE)
	(*backts_commas_balance)++;
      else if (el == COMMA)
	(*backts_commas_balance)--;

      if (*backts_commas_balance < 0)
	return TOO_MANY_COMMAS;

      if (el == COMMA)
	found_comma = 1;
      else
	found_comma = 0;

      el = find_next_element (n+1, size - (n + 1 - input), &n);
    }

  return JUST_PREFIX;
}


enum read_outcome
read_sharp_macro_call (struct object **obj, const char *input, size_t size,
		       struct environment *env, struct eval_outcome *e_outcome,
		       const char **macro_end, size_t *out_arg)
{
  int arg;
  size_t i = 0;
  const char *obj_b;
  struct sharp_macro_call *call;
  char *buf;
  enum read_outcome out;

  if (!size)
    return NO_OBJECT;

  if (*obj)
    {
      return read_object_continued (&(*obj)->value_ptr.sharp_macro_call->obj, 0,
				    (*obj)->value_ptr.sharp_macro_call->
				    is_empty_list, input, size, env, e_outcome,
				    &obj_b, macro_end, out_arg);
    }

  *obj = alloc_sharp_macro_call ();

  call = (*obj)->value_ptr.sharp_macro_call;

  if (isdigit (input [i]))
    arg = input [i++] - '0';
  else
    arg = -1;

  while (i < size && isdigit (input [i]))
    {
      arg *= 10;
      arg += input [i++] - '0';
    }

  call->arg = arg;

  if (i < size)
    call->dispatch_ch = input [i];
  else
    return NO_OBJECT;

  if (strchr ("\b\t\n\r\f <)", call->dispatch_ch))
    {
      return INVALID_SHARP_DISPATCH;
    }

  if (!strchr ("'\\.pP(", call->dispatch_ch))
    {
      return UNKNOWN_SHARP_DISPATCH;
    }

  if (call->dispatch_ch == '\\')
    {
      buf = copy_token_to_buffer (input+i+1, size-i-1);

      if (mbslen (buf) == 1)
	{
	  call->obj = create_character (buf, 0);

	  *macro_end = input+i + strlen (buf);

	  return COMPLETE_OBJECT;
	}

      free (buf);
    }
  else if (call->dispatch_ch == '(')
    {
      call->obj = NULL;
      out = read_list (&call->obj, 0, input+i+1, size-i-1, env, e_outcome,
		       macro_end, out_arg);

      if (out == INCOMPLETE_EMPTY_LIST)
	call->is_empty_list = 1;

      if (out & INCOMPLETE_OBJECT)
	return INCOMPLETE_SHARP_MACRO_CALL;

      return COMPLETE_OBJECT;
    }

  call->obj = NULL;
  out = read_object (&call->obj, 0, input+i+1, size-i-1, env, e_outcome, &obj_b,
		     macro_end, out_arg);

  if (out == INCOMPLETE_EMPTY_LIST)
    call->is_empty_list = 1;

  if (out & INCOMPLETE_OBJECT)
    return INCOMPLETE_SHARP_MACRO_CALL;

  return COMPLETE_OBJECT;
}


struct object *
call_sharp_macro (struct sharp_macro_call *macro_call, struct environment *env,
		  struct eval_outcome *e_outcome, enum read_outcome *r_outcome)
{
  struct binding *bind;
  struct object *obj = macro_call->obj;
  struct symbol_name *s;

  if (macro_call->dispatch_ch == '\'')
    {
      if (obj->type != TYPE_SYMBOL && obj->type != TYPE_SYMBOL_NAME)
	{
	  *r_outcome = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

	  return NULL;
	}

      bind = find_binding (SYMBOL (obj)->value_ptr.symbol, env->funcs,
			   DYNAMIC_BINDING);

      if (!bind)
	{
	  *r_outcome = FUNCTION_NOT_FOUND;
	  return NULL;
	}

      return bind->obj;
    }
  else if (macro_call->dispatch_ch == '\\')
    {
      if (obj->type == TYPE_CHARACTER)
	return obj;

      if (obj->type != TYPE_SYMBOL_NAME)
	{
	  *r_outcome = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

	  return NULL;
	}

      s = obj->value_ptr.symbol_name;

      if (s->packname_present)
	{
	  *r_outcome = UNKNOWN_CHARACTER_NAME;

	  return NULL;
	}

      if (eqmem (s->value, s->used_size, "NEWLINE", strlen ("NEWLINE")))
	return create_character ("\n", 1);
      else if (eqmem (s->value, s->used_size, "SPACE", strlen ("SPACE")))
	return create_character (" ", 1);
      else if (eqmem (s->value, s->used_size, "TAB", strlen ("TAB")))
	return create_character ("\t", 1);
      else if (eqmem (s->value, s->used_size, "BACKSPACE", strlen ("BACKSPACE")))
	return create_character ("\b", 1);
      else if (eqmem (s->value, s->used_size, "PAGE", strlen ("PAGE")))
	return create_character ("\f", 1);
      else if (eqmem (s->value, s->used_size, "RETURN", strlen ("RETURN")))
	return create_character ("\r", 1);
      else
	{
	  *r_outcome = UNKNOWN_CHARACTER_NAME;

	  return NULL;
	}
    }
  else if (macro_call->dispatch_ch == '.')
    {
      return evaluate_object (obj, env, e_outcome);
    }
  else if (macro_call->dispatch_ch == 'p' || macro_call->dispatch_ch == 'P')
    {
      if (obj->type != TYPE_STRING)
	{
	  *r_outcome = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

	  return NULL;
	}

      return create_filename (obj);
    }
  else if (macro_call->dispatch_ch == '(')
    return create_vector (obj);

  return NULL;
}


enum element
find_next_element (const char *input, size_t size, const char **elem_begin)
{
  input = skip_space_block (input, size, &size);

  if (!input)
    return NONE;

  *elem_begin = input;
  
  switch (*input)
    {
    case '(':
      return BEGIN_LIST;
    case ')':
      return END_LIST;
    case '"':
      return STRING_DELIMITER;
    case '\'':
      return QUOTE;
    case '`':
      return BACKQUOTE;
    case ',':
      return COMMA;
    case '@':
      return AT;
    case ';':
      return SEMICOLON;
    case '.':
      return DOT;
    case '#':
      if (size > 1 && *(++input) == '|')
	return BEGIN_MULTILINE_COMMENT;
      else
	return SHARP;
    case '|':
      return VERTICAL_BAR;
    case '\\':
    default:
      return TOKEN;
    }
}


int
is_number (const char *token, size_t size, int radix, enum object_type *numtype,
	   const char **number_end, const char **token_end)
{
  size_t i = 0;
  
  int found_dec_point = 0, found_exp_marker = 0, exp_marker_pos, found_slash = 0,
    found_dec_digit = 0, found_digit = 0, found_digit_after_slash = 0,
    found_digit_after_exp_marker = 0, found_digit_after_dec_point = 0,
    need_decimal_digit = 0;
  
  char decimal_digits [] = "0123456789";
  char digits [] = "00112233445566778899aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRs"
    "StTuUvVwWxXyYzZ";
  char exponent_markers [] = "dDeEfFlLsS";

  int digits_len = radix * 2;
  
  *numtype = TYPE_INTEGER;
 
  while (i < size)
    {
      if (strchr (decimal_digits, token [i]))
	{
	  found_dec_digit = 1, need_decimal_digit = 0;
	}
      
      if (memchr (digits, token [i], digits_len))
	{
	  found_digit = 1;

	  found_exp_marker && (found_digit_after_exp_marker = 1);
	  found_slash && (found_digit_after_slash = 1);
	  found_dec_point && (found_digit_after_dec_point = 1);
	}
      else if (strchr (exponent_markers, token [i]))
	{
	  if (found_exp_marker || !found_dec_digit)
	    return 0;
	  else
	    {
	      found_exp_marker = 1;
	      exp_marker_pos = i;
	      *numtype = TYPE_FLOAT;
	    }
	}      
      else if (token [i] == '+' || token [i] == '-')
	{
	  if (i > 0 && (!found_exp_marker || (i - exp_marker_pos > 1)))
	    return 0;
	}
      else if (token [i] == '/')
	{
	  if (found_slash || found_dec_point || found_exp_marker || !found_digit)
	    return 0;
	  else
	    {
	      found_slash = 1;
	      *numtype = TYPE_RATIO;
	    }
	}
      else if (token [i] == '.')
	{
	  if (found_dec_point || found_exp_marker || found_slash)
	    return 0;
	  else if (found_digit && !found_dec_digit)
	    return 0;
	  else
	    {
	      found_dec_point = 1;

	      if (!found_dec_digit)
		{
		  *numtype = TYPE_FLOAT;
		  need_decimal_digit = 1;
		}
	    }
	}
      else if (isspace (token [i]) || strchr (TERMINATING_MACRO_CHARS, token [i]))
	break;
      else
	return 0;
      
      i++;
    }

  if (!i)
    return 0;
  if (!found_digit)
    return 0;
  if (found_slash && !found_digit_after_slash)
    return 0;
  if (found_exp_marker && !found_digit_after_exp_marker)
    return 0;
  if (need_decimal_digit)
    return 0;

  if (found_digit_after_dec_point)
    *numtype = TYPE_FLOAT;

  if (token [i-1] == '.')
    {
      *number_end = token + i - 2;
      *token_end = *number_end + 1;
    }
  else
    *number_end = *token_end = token + i - 1;
  
  
  return 1;
}


struct object *
alloc_number (const char *token, size_t size, int radix, enum object_type numtype)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  char *buf = malloc_and_check (size + 1);
  
  obj->type = numtype;
  obj->refcount = 1;
  
  memcpy (buf, token, size);
  buf [size] = 0;
  
  if (numtype == TYPE_INTEGER)
    {
      mpz_init (obj->value_ptr.integer);
      mpz_set_str (obj->value_ptr.integer, buf, radix);
    }
  else if (numtype == TYPE_RATIO)
    {
      mpq_init (obj->value_ptr.ratio);
      mpq_set_str (obj->value_ptr.ratio, buf, radix);
    }
  else if (numtype == TYPE_FLOAT)
    {
      mpf_init (obj->value_ptr.floating);
      mpf_set_str (obj->value_ptr.floating, buf, radix);
    }

  free (buf);
  
  return obj;
}


void
print_range (const char *begin, const char *end)
{
  while (begin < end)
    {
      putchar (*begin);
      begin++;
    }
}


char *
append_newline (char *string)
{
  size_t len = strlen (string);
  string = realloc_and_check (string, len + 2);
  
  string [len] = '\n';
  string [len+1] = 0;

  return string;
}


char *
append_zero_byte (char *string, size_t size)
{
  string = realloc_and_check (string, size + 1);

  string [size] = 0;

  return string;
}


char *
copy_token_to_buffer (const char *input, size_t size)
{
  size_t i;
  int single_esc = 0, multiple_esc = 0;
  char *buf;

  for (i = 0; i < size; i++)
    {
      if (input [i] == '\\')
	single_esc = single_esc ? 0 : 1;
      else if (input [i] == '|')
	multiple_esc = multiple_esc ? 0 : 1;
      else if (!single_esc && !multiple_esc
	       && (isspace (input [i])
		   || strchr (TERMINATING_MACRO_CHARS, input [i])))
	break;
    }

  buf = malloc_and_check (i + 1);

  strncpy (buf, input, i);

  buf [i] = '\0';

  return buf;
}


size_t
mbslen (const char *string)
{
  size_t s = 0;

  for (; *string != '\0'; string++)
    {
      if ((*string & 0xc0) >> 6 != 2)
	s++;
    }

  return s;
}


void *
malloc_and_check (size_t size)
{
  void *mem = malloc (size);

  if (size && !mem)
    {
      fprintf (stderr, "could not allocate %lu bytes. Aborting...\n", size);
      exit (1);
    }

  return mem;
}


void *
realloc_and_check (void *ptr, size_t size)
{
  void *mem = realloc (ptr, size);

  if (size && !mem)
    {
      fprintf (stderr, "could not allocate %lu bytes. Aborting...\n", size);
      exit (1);
    }

  return mem;
}


struct object *
alloc_object (void)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  
  obj->refcount = 1;
  
  return obj;
}


struct object *
alloc_prefix (enum element type)
{
  struct object *obj = malloc_and_check (sizeof (*obj));

  switch (type)
    {
    case QUOTE:
      obj->type = TYPE_QUOTE;
      break;
    case BACKQUOTE:
      obj->type = TYPE_BACKQUOTE;
      break;
    case COMMA:
      obj->type = TYPE_COMMA;
      break;
    case AT:
      obj->type = TYPE_AT;
      break;
    case DOT:
      obj->type = TYPE_DOT;
      break;
    default:
      break;
    }
	
  obj->refcount = 1;

  obj->value_ptr.next = NULL;

  return obj;
}


struct object *
alloc_empty_cons_pair (void)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct cons_pair *cons = malloc_and_check (sizeof (*cons));
  struct object *car = malloc_and_check (sizeof (*car));

  car->type = TYPE_NIL;
  car->refcount = 1;

  cons->filling_car = 0;
  cons->empty_list_in_car = 0;
  cons->car = car;
  cons->cdr = &nil_object;

  obj->type = TYPE_CONS_PAIR;
  obj->refcount = 1;
  obj->value_ptr.cons_pair = cons;

  return obj;
}


struct object *
alloc_function (void)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct function *fun = malloc_and_check (sizeof (*fun));

  fun->lambda_list = NULL;
  fun->allow_other_keys = 0;
  fun->eval_args = 0;
  fun->body = NULL;

  obj->type = TYPE_FUNCTION;
  obj->refcount = 1;
  obj->value_ptr.function = fun;

  return obj;
}


struct object *
alloc_sharp_macro_call (void)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct sharp_macro_call *call = malloc_and_check (sizeof (*call));

  obj->type = TYPE_SHARP_MACRO_CALL;
  obj->refcount = 1;
  obj->value_ptr.sharp_macro_call = call;

  call->is_empty_list = 0;

  return obj;
}


struct object *
create_function (struct object *lambda_list, struct object *body)
{
  enum parse_lambda_list_outcome out;
  struct object *fun = alloc_function ();

  fun->value_ptr.function->lambda_list = parse_lambda_list (lambda_list, &out);

  fun->value_ptr.function->body = body;

  return fun;
}


struct object *
create_package (char *name, size_t name_len)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct package *pack = malloc_and_check (sizeof (*pack));
  struct object *s = create_symbol (name, name_len);

  pack->name = s;

  obj->type = TYPE_PACKAGE;
  obj->refcount = 1;
  obj->value_ptr.package = pack;

  return obj;
}


const char *
find_end_of_string (const char *input, size_t size, size_t *new_size,
		    size_t *string_length)
{
  size_t i = 0, escape = 0;

  *string_length = 0;
  
  while (i < size)
    {
      if (input [i] == '"' && !escape)
	break;

      if (input [i] != '\\' || escape)
	(*string_length)++, escape = 0;
      else
	escape = 1;

      i++;
    }

  if (i == size)
    return NULL;

  *new_size = size-i+1;
 
  return input+i;
}


void
normalize_string (char *output, const char *input, size_t size)
{
  int escape = 0, j = 0;
  size_t i = 0;

  while (i < size)
    {
      if (input [i] == '\\' && !escape)
	escape = 1;
      else if (input [i] == '"' && !escape)
	break;
      else
	{
	  output [j++] = input [i];
	  escape = 0;
	}
      
      i++;
    }
}


struct object *
alloc_string (size_t size)
{
  struct object *obj = malloc_and_check (sizeof (*obj));

  obj->type = TYPE_STRING;
  obj->refcount = 1;

  obj->value_ptr.string = malloc_and_check (sizeof (*obj->value_ptr.string));

  obj->value_ptr.string->value = malloc_and_check (sizeof (char) * size);
  obj->value_ptr.string->alloc_size = sizeof (char) * size;
  obj->value_ptr.string->used_size = 0;

  return obj;
}


void
resize_string (struct object *string, size_t size)
{
  string->value_ptr.string->value =
    realloc_and_check (string->value_ptr.string->value, size);

  if (size < string->value_ptr.string->used_size)
    string->value_ptr.string->used_size = size;

  string->value_ptr.string->alloc_size = size;
}


struct object *
alloc_symbol_name (size_t value_s, size_t actual_symname_s)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct symbol_name *s;

  obj->type = TYPE_SYMBOL_NAME;
  obj->refcount = 1;

  obj->value_ptr.symbol_name =
    malloc_and_check (sizeof (*obj->value_ptr.symbol_name));
  s = obj->value_ptr.symbol_name;

  s->value = malloc_and_check (value_s);
  s->alloc_size = value_s;
  s->used_size = 0;

  s->packname_present = 0;

  s->actual_symname = malloc_and_check (actual_symname_s);
  s->actual_symname_alloc_s = actual_symname_s;
  s->actual_symname_used_s = 0;
  s->sym = NULL;

  return obj;
}


void
resize_symbol_name (struct object *symname, size_t value_s,
		    size_t actual_symname_s)
{
  struct symbol_name *s = symname->value_ptr.symbol_name;

  s->value = realloc_and_check (s->value, value_s);

  if (value_s < s->used_size)
    s->used_size = value_s;

  s->alloc_size = value_s;

  s->actual_symname = realloc_and_check (s->actual_symname, actual_symname_s);

  if (actual_symname_s < s->actual_symname_used_s)
    s->actual_symname_used_s = actual_symname_s;

  s->actual_symname_alloc_s = actual_symname_s;
}


const char *
find_end_of_symbol_name (const char *input, size_t size, int found_package_sep,
			 size_t *new_size,
			 const char **start_of_package_separator,
			 enum package_record_visibility *sym_visibility,
			 size_t *name_length, size_t *act_name_length,
			 enum read_outcome *outcome)
{
  size_t i = 0;
  int single_escape = 0, multiple_escape = 0, just_dots = 1, colons = 0;
  size_t **length;

  *start_of_package_separator = NULL;

  *name_length = 0, *act_name_length = 0, *outcome = NO_OBJECT;

  length = found_package_sep ? &act_name_length : &name_length;

  while (i < size)
    {
      if (input [i] == '\\')
	{
	  just_dots = 0;

	  if (!single_escape)
	    {
	      single_escape = 1;
	    }
	  else
	    {
	      single_escape = 0;
	      (**length)++;
	    }
	}
      else if (input [i] == '|' && !single_escape)
	{
	  just_dots = 0;

	  multiple_escape = (multiple_escape ? 0 : 1);
	}
      else if (input [i] == ':' && !single_escape && !multiple_escape)
	{
	  if (found_package_sep
	      || (*start_of_package_separator
		  && (input + i > *start_of_package_separator + colons)))
	    {
	      *outcome = MORE_THAN_A_PACKAGE_SEPARATOR;

	      return NULL;
	    }

	  if (colons == 1 && (i == 1))
	    {
	      *outcome = CANT_BEGIN_WITH_TWO_COLONS_OR_MORE;

	      return NULL;
	    }

	  if (!colons)
	    {
	      *start_of_package_separator = input + i;
	      *sym_visibility = EXTERNAL_VISIBILITY;
	      length = &act_name_length;
	    }

	  colons++;

	  if (colons > 2)
	    {
	      *outcome = TOO_MANY_COLONS;

	      return NULL;
	    }
	  else if (colons == 2)
	    {
	      *sym_visibility = INTERNAL_VISIBILITY;
	    }
	}
      else
	{
	  if ((isspace (input [i]) || strchr (TERMINATING_MACRO_CHARS, input [i]))
	      && !single_escape && !multiple_escape)
	    {
	      if (just_dots && **length == 1)
		*outcome = SINGLE_DOT;
	      else if (just_dots && **length)
		*outcome = MULTIPLE_DOTS;
	      else if (*start_of_package_separator
		       && (input + i == *start_of_package_separator + colons))
		*outcome = CANT_END_WITH_PACKAGE_SEPARATOR;

	      *new_size = size-i+1;
	      return input+i-1;
	    }

	  if (input [i] != '.')
	    just_dots = 0;

	  (**length)++;
	  single_escape = 0;
	}
      i++;
    }

  return NULL;
}


void
normalize_symbol_name (char *output, const char *input, size_t size,
		       enum readtable_case read_case)
{
  size_t i;
  int j, single_escape = 0, multiple_escape = 0;
  
  for (i = 0, j = 0; i < size; i++)
    {
      if (input [i] == '\\')
	{
	  if (!single_escape)
	    {
	      single_escape = 1;
	    }
	  else
	    {
	      output [j++] = '\\';
	      single_escape = 0;
	    }
	}
      else if (input [i] == '|' && !single_escape)
	{
	  multiple_escape = (multiple_escape ? 0 : 1);
	}
      else if ((isspace (input [i]) || strchr (TERMINATING_MACRO_CHARS, input [i]))
	       && !single_escape && !multiple_escape)
	{
	  break;
	}
      else
	{
	  if (single_escape || multiple_escape)
	    {
	      output [j++] = input [i];
	      single_escape = 0;
	    }
	  else
	    switch (read_case)
	      {
	      case CASE_UPCASE:
		output [j++] = toupper (input [i]);
		break;
	      case CASE_DOWNCASE:
		output [j++] = tolower (input [i]);
		break;
	      case CASE_PRESERVE:
		output [j++] = input [i];
		break;
	      case CASE_INVERT:
		output [j++] = isupper (input [i]) ? tolower (input [i])
		  : toupper (input [i]);
		break;
	      }
	}
    }
}


struct object *
create_symbol (char *name, size_t size)
{
  struct object *obj;
  struct symbol *sym;

  obj = malloc_and_check (sizeof (*obj));
  obj->type = TYPE_SYMBOL;
  obj->refcount = 1;

  sym = malloc_and_check (sizeof (*sym));
  sym->name = name;
  sym->name_len = size;
  sym->is_builtin_form = 0;
  sym->is_const = 0;
  sym->is_parameter = 0;
  sym->is_special = 0;
  sym->value_cell = NULL;
  sym->function_cell = NULL;

  obj->value_ptr.symbol = sym;

  return obj;
}


struct object *
create_character (char *character, int do_copy)
{
  struct object *obj = malloc_and_check (sizeof (*obj));

  obj->type = TYPE_CHARACTER;
  obj->refcount = 1;

  if (do_copy)
    {
      obj->value_ptr.character = malloc_and_check (strlen (character) + 1);
      strcpy (obj->value_ptr.character, character);
    }
  else
    obj->value_ptr.character = character;

  return obj;
}


struct object *
create_filename (struct object *string)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct filename *fn = malloc_and_check (sizeof (*fn));

  obj->type = TYPE_FILENAME;
  obj->refcount = 1;

  fn->value = string;

  obj->value_ptr.filename = fn;

  return obj;
}


struct object *
create_vector (struct object *list)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct array *vec = malloc_and_check (sizeof (*vec));
  struct array_size *sz = malloc_and_check (sizeof (*sz));
  int i;

  sz->size = list_length (list);
  sz->next = NULL;

  vec->alloc_size = sz;
  vec->fill_pointer = -1;
  vec->value = malloc_and_check (sizeof (*vec->value) * sz->size);

  for (i = 0; i < sz->size; i++)
    vec->value [i] = nth (i, list);

  obj->type = TYPE_ARRAY;
  obj->refcount = 1;
  obj->value_ptr.array = vec;

  return obj;
}


struct object *
find_package (const char *name, size_t len, struct environment *env)
{
  struct binding *bind = env->packages;

  while (bind)
    {
      if (eqmem (bind->sym->value_ptr.symbol->name,
		 bind->sym->value_ptr.symbol->name_len, name, len))
	return bind->obj;

      bind = bind->next;
    }

  return NULL;
}


struct object *
intern_symbol (char *name, size_t len, struct object_list **symlist)
{
  struct object *sym;
  struct object_list *cur = *symlist, *new_sym;

  while (cur)
    {
      if (eqmem (cur->obj->value_ptr.symbol->name,
		 cur->obj->value_ptr.symbol->name_len, name, len))
	{
	  increment_refcount (cur->obj);
	  return cur->obj;
	}

      cur = cur->next;
    }

  sym = create_symbol (name, len);

  new_sym = malloc_and_check (sizeof (*new_sym));
  new_sym->obj = sym;
  new_sym->next = *symlist;

  *symlist = new_sym;

  return sym;  
}


struct object *
intern_symbol_name (struct object *symname, struct environment *env)
{
  struct symbol_name *s = symname->value_ptr.symbol_name;
  struct object *pack;

  if (s->packname_present)
    {
      if (!s->used_size)
	{
	  s->sym = intern_symbol (s->actual_symname, s->actual_symname_used_s,
				  &env->keyword_package->value_ptr.package->symlist);
	  s->sym->value_ptr.symbol->home_package = env->keyword_package;

	  return s->sym;
	}

      pack = find_package (s->value, s->used_size, env);

      if (!pack)
	return NULL;
      else
	{
	  s->sym = intern_symbol (s->actual_symname, s->actual_symname_used_s,
				  &pack->value_ptr.package->symlist);
	  s->sym->value_ptr.symbol->home_package = pack;
	  return s->sym;
	}
    }

  pack = env->current_package;
  return (s->sym = intern_symbol (s->value, s->used_size,
				  &pack->value_ptr.package->symlist));
}


struct binding *
create_binding (struct object *sym, struct object *obj, enum binding_type type)
{
  struct binding *bin = malloc_and_check (sizeof (*bin));

  bin->type = type;
  bin->sym = sym;
  bin->obj = obj;
  bin->next = NULL;

  increment_refcount (sym);
  increment_refcount (obj);

  return bin;
}


struct binding *
add_binding (struct binding *bin, struct binding *env)
{
  bin->next = env;

  return bin;
}


struct binding *
chain_bindings (struct binding *bin, struct binding *env)
{
  struct binding *last = bin, *b = bin;
  
  if (!bin)
    return env;
  
  while (b)
    {
      last = b;
      b = b->next;
    }

  last->next = env;
  
  return bin;
}


struct binding *
remove_bindings (struct binding *env, int num)
{
  struct binding *b;

  if (!num)
    return env;  
  else if (num == 1)
    {
      b = env->next;
      free (env);
      return b;
    }
  else
    {
      return remove_bindings (env->next, num-1);
    }
}


struct binding *
find_binding (struct symbol *sym, struct binding *env, enum binding_type type)
{
  while (env)
    {
      if (env->sym->value_ptr.symbol == sym && env->type & type)
	return env;

      env = env->next;
    }

  return NULL;
}


void
add_builtin_type (char *name, struct environment *env,
		  int (*builtin_type) (const struct object *obj,
				       const struct object *typespec,
				       struct environment *env,
				       struct eval_outcome *outcome),
		  int is_standard)
{
  struct object *sym = intern_symbol (name, strlen (name),
				      &env->current_package->
				      value_ptr.package->symlist);

  sym->value_ptr.symbol->is_type = 1;
  sym->value_ptr.symbol->is_builtin_type = 1;
  sym->value_ptr.symbol->is_standard_type = is_standard;
  sym->value_ptr.symbol->builtin_type = builtin_type;

  increment_refcount (sym);
}


void
add_builtin_form (char *name, struct environment *env,
		  struct object *(*builtin_form) (struct object *list,
						  struct environment *env,
						  struct eval_outcome *outcome),
		  int eval_args)
{
  struct object *sym = intern_symbol (name, strlen (name),
				      &env->current_package->
				      value_ptr.package->symlist);

  sym->value_ptr.symbol->is_builtin_form = 1;
  sym->value_ptr.symbol->builtin_form = builtin_form;
  sym->value_ptr.symbol->evaluate_args = eval_args;

  increment_refcount (sym);
}


struct object *
define_constant (struct object *sym, struct object *form,
		 struct environment *env, struct eval_outcome *outcome)
{
  struct object *val = evaluate_object (form, env, outcome);
  
  if (!val)
    return NULL;

  if (SYMBOL (sym)->value_ptr.symbol->is_const)
    {
      outcome->type = CANT_REDEFINE_CONSTANT;
      return NULL;
    }

  SYMBOL (sym)->value_ptr.symbol->is_const = 1;
  SYMBOL (sym)->value_ptr.symbol->value_cell = val;
  increment_refcount (SYMBOL (sym));

  return sym;
}


struct object *
define_constant_by_name (char *name, size_t size, struct object *form,
			 struct environment *env, struct eval_outcome *outcome)
{
  struct object *sym = intern_symbol (name, size,
				      &env->current_package->value_ptr.package->
				      symlist);

  return define_constant (sym, form, env, outcome);
}


struct object *
define_parameter (struct object *sym, struct object *form,
		  struct environment *env, struct eval_outcome *outcome)
{
  struct object *s;
  struct object *val = evaluate_object (form, env, outcome);

  if (!val)
    return NULL;
  
  if (sym->type == TYPE_SYMBOL_NAME)
    s = sym->value_ptr.symbol_name->sym;
  else
    s = sym;
  
  s->value_ptr.symbol->is_special = 1;
  s->value_ptr.symbol->value_cell = val;

  return s;
}


struct object *
define_parameter_by_name (char *name, size_t size, struct object *form,
			  struct environment *env, struct eval_outcome *outcome)
{
  struct object *sym = intern_symbol (name, size,
				      &env->current_package->value_ptr.package->
				      symlist);

  return define_parameter (sym, form, env, outcome);
}


struct object *
skip_prefix (struct object *prefix, int *num_backticks_before_last_comma,
	     int *num_commas, struct object **last_prefix,
	     struct object **last_comma, struct object **before_last_comma)
{
  int num_backticks = 0;

  if (last_prefix)
    *last_prefix = NULL;
  if (num_backticks_before_last_comma)
    *num_backticks_before_last_comma = 0;
  if (num_commas)
    *num_commas = 0;
  if (last_comma)
    *last_comma = NULL;
  if (before_last_comma)
    *before_last_comma = NULL;

  while (prefix &&
	 (prefix->type == TYPE_QUOTE
	  || prefix->type == TYPE_BACKQUOTE
	  || prefix->type == TYPE_COMMA))
    {
      if (last_prefix)
	*last_prefix = prefix;

      if (prefix->type == TYPE_BACKQUOTE)
	num_backticks++;

      if (num_commas && prefix->type == TYPE_COMMA)
	{
	  (*num_commas)++;

	  if (num_backticks_before_last_comma)
	    *num_backticks_before_last_comma = num_backticks;
	}

      if (last_comma && prefix->type == TYPE_COMMA)
	*last_comma = prefix;

      if (before_last_comma && prefix->value_ptr.next
	  && prefix->value_ptr.next->type == TYPE_COMMA)
	*before_last_comma = prefix;

      prefix = prefix->value_ptr.next;
    }

  if (num_backticks_before_last_comma && (!num_commas || !*num_commas))
    *num_backticks_before_last_comma = num_backticks;

  return prefix;
}


struct object *
append_prefix (struct object *obj, enum element type)
{
  struct object *prev, *curr;
  
  if (!obj)
    return alloc_prefix (type);

  prev = curr = obj;
  
  if (curr->type == TYPE_QUOTE
      || curr->type == TYPE_BACKQUOTE
      || curr->type == TYPE_COMMA)
    curr = curr->value_ptr.next;

  while (curr &&
	 (curr->type == TYPE_QUOTE
	  || curr->type == TYPE_BACKQUOTE
	  || curr->type == TYPE_COMMA))
    {
      prev = curr;
      curr = curr->value_ptr.next;
    }
  
  prev->value_ptr.next = alloc_prefix (type);
  prev->value_ptr.next->value_ptr.next = curr;
  
  return obj;
}


struct object *
nth (unsigned int ind, struct object *list)
{
  size_t i;

  for (i = 0; i < ind; i++)
    if (list->type != TYPE_CONS_PAIR)
      return &nil_object;
    else
      list = list->value_ptr.cons_pair->cdr;

  if (list->type == TYPE_CONS_PAIR)
    return list->value_ptr.cons_pair->car;
  else
    return list;
}


struct object *
nthcdr (unsigned int ind, struct object *list)
{
  size_t i;

  for (i = 0; i < ind; i++)
    {
      list = list->value_ptr.cons_pair->cdr;

      if (!list)
	return &nil_object;
    }

  return list;
}


unsigned int
list_length (const struct object *list)
{
  unsigned int l = 0;
  
  while (list && list->type == TYPE_CONS_PAIR)
    {
      l++;
      list = list->value_ptr.cons_pair->cdr;
    }

  if (list && list->type != TYPE_NIL)
    l++;

  return l;
}


struct object *
last_cons_pair (struct object *list)
{
  struct object *prev;

  while (list && list->type != TYPE_NIL)
    {
      prev = list;
      list = list->value_ptr.cons_pair->cdr;
    }

  return prev;
}


int
is_dotted_list (const struct object *list)
{
  while (list && list->type == TYPE_CONS_PAIR)
    {
      list = list->value_ptr.cons_pair->cdr;
    }

  if (list && list != &nil_object)
    return 1;

  return 0;
}


struct object *
copy_prefix (const struct object *begin, const struct object *end,
	     struct object **last_prefix)
{
  struct object *out = NULL, *pr = NULL, *tmp;

  while (begin && begin != end)
    {
      tmp = alloc_prefix (begin->type == TYPE_QUOTE ? QUOTE :
			  begin->type == TYPE_BACKQUOTE ? BACKQUOTE :
			  begin->type == TYPE_COMMA ? COMMA :
			  begin->type == TYPE_AT ? AT :
			  begin->type == TYPE_DOT ? DOT
			  : NONE);

      if (pr)
	pr->value_ptr.next = tmp;
      else
	out = pr = tmp;

      begin = begin->value_ptr.next;
    }

  if (begin)
    tmp = alloc_prefix (begin->type == TYPE_QUOTE ? QUOTE :
			begin->type == TYPE_BACKQUOTE ? BACKQUOTE :
			begin->type == TYPE_COMMA ? COMMA :
			begin->type == TYPE_AT ? AT :
			begin->type == TYPE_DOT ? DOT : NONE);

  if (pr)
    pr->value_ptr.next = tmp;
  else
    out = tmp;

  if (last_prefix)
    *last_prefix = pr->value_ptr.next;

  return out;
}


struct object *
copy_list_structure (const struct object *list, const struct object *prefix)
{
  struct object *cons, *out, *lastpref;

  out = cons = alloc_empty_cons_pair ();

  cons->value_ptr.cons_pair->car = list->value_ptr.cons_pair->car;

  list = list->value_ptr.cons_pair->cdr;

  while (list->type == TYPE_CONS_PAIR)
    {
      cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

      if (prefix)
	{
	  cons->value_ptr.cons_pair->car = copy_prefix (prefix, NULL, &lastpref);
	  lastpref->value_ptr.next = list->value_ptr.cons_pair->car;
	}
      else
	cons->value_ptr.cons_pair->car = list->value_ptr.cons_pair->car;

      list = list->value_ptr.cons_pair->cdr;
    }

  return out;
}


int
array_rank (const struct array *array)
{
  struct array_size *as = array->alloc_size;
  int rank = 0;

  while (as)
    {
      rank++;

      as = as->next;
    }

  return rank;
}


struct parameter *
alloc_parameter (enum parameter_type type, struct object *sym)
{
  struct parameter *par = malloc_and_check (sizeof (*par));
  par->type = type;
  par->name = sym;
  par->next = NULL;
  
  return par;
}


struct parameter *
parse_required_parameters (struct object *obj, struct parameter **last,
			   struct object **rest, enum parse_lambda_list_outcome *out)
{
  struct object *car;
  struct parameter *first = NULL;

  *last = NULL;
  
  while (obj && (car = CAR (obj))
	 && car->type == TYPE_SYMBOL_NAME 
	 && !symname_is_among (car->value_ptr.symbol_name, "&OPTIONAL", "&REST",
			       "&KEYWORD", "&AUX", "&ALLOW_OTHER_KEYS", NULL))
    {
      if (!first)
	*last = first = alloc_parameter (REQUIRED_PARAM, SYMBOL (car));
      else
	*last = (*last)->next = alloc_parameter (REQUIRED_PARAM, SYMBOL (car));
      
      obj = CDR (obj);
    }

  *rest = obj;
  
  return first;
}


struct parameter *
parse_optional_parameters (struct object *obj, struct parameter **last,
			   struct object **next, enum parse_lambda_list_outcome *out)
{
  struct object *car;
  struct parameter *first = NULL;

  *last = NULL;
  
  while (obj && (car = obj->value_ptr.cons_pair->car))
    {
      if (car->type == TYPE_SYMBOL_NAME 
	  && symname_is_among (car->value_ptr.symbol_name, "&OPTIONAL", "&REST",
			       "&KEYWORD", "&AUX", "&ALLOW_OTHER_KEYS", NULL))
	{
	  break;
	}
      else if (car->type == TYPE_SYMBOL_NAME)
	{
	  if (!first)
	    *last = first = alloc_parameter (OPTIONAL_PARAM, SYMBOL (car));
	  else
	    *last = (*last)->next = alloc_parameter (OPTIONAL_PARAM, SYMBOL (car));

	  (*last)->init_form = NULL;
	  (*last)->supplied_p_param = NULL;
	}
      else if (car->type == TYPE_CONS_PAIR)
	{
	  if (!first)
	    *last = first = alloc_parameter (OPTIONAL_PARAM, SYMBOL (CAR (car)));
	  else
	    *last = (*last)->next = alloc_parameter (OPTIONAL_PARAM, SYMBOL (CAR (car)));

	  (*last)->init_form = NULL;
	  (*last)->supplied_p_param = NULL;
	  
	  if (list_length (car) == 2)
	    (*last)->init_form = nth (1, car);
	  
	  if (list_length (car) == 3)
	    (*last)->supplied_p_param = SYMBOL (nth (2, car));
	}
      
      obj = obj->value_ptr.cons_pair->cdr;
    }
  
  *next = obj;
  
  return first;
}


struct parameter *
parse_lambda_list (struct object *obj, enum parse_lambda_list_outcome *out)
{
  struct parameter *first = NULL, *last = NULL, *newlast = NULL;
  struct object *car;
  
  if (obj->type == TYPE_NIL)
    return NULL;
  
  if (obj->type != TYPE_CONS_PAIR)
    return NULL;

  first = parse_required_parameters (obj, &last, &obj, out);

  if (obj && obj->type == TYPE_CONS_PAIR && (car = obj->value_ptr.cons_pair->car)
      && symname_equals (car->value_ptr.symbol_name, "&OPTIONAL"))
    {
      if (first)
	last->next =
	  parse_optional_parameters (obj->value_ptr.cons_pair->cdr, &newlast,
				     &obj, out);
      else
	first = parse_optional_parameters (obj->value_ptr.cons_pair->cdr,
					   &last, &obj, out);
    }

  if (obj && obj->type == TYPE_CONS_PAIR && (car = obj->value_ptr.cons_pair->car)
      && symname_equals (car->value_ptr.symbol_name, "&REST"))
    {
      if (first)
	last->next = alloc_parameter (REST_PARAM, SYMBOL (CAR (CDR (obj))));
      else
	first = alloc_parameter (REST_PARAM, SYMBOL (CAR (CDR (obj))));
    }

  return first;
}


struct object *
evaluate_body (struct object *body, int eval_twice, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct object *res;

  do
    {
      res = evaluate_object (CAR (body), env, outcome);

      if (eval_twice)
	{
	  if (!res)
	    return NULL;

	  res = evaluate_object (res, env, outcome);
	}

      body = CDR (body);

    } while (res && body->type != TYPE_NIL);

  return res;
}


struct object *
call_function (struct object *func, struct object *arglist, int eval_args,
	       int eval_twice, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct parameter *par = func->value_ptr.function->lambda_list;
  struct binding *bins = NULL;
  struct object *val, *res;
  int args = 0;

  while (arglist != &nil_object && par
	 && (par->type == REQUIRED_PARAM || par->type == OPTIONAL_PARAM))
    {
      if (eval_args)
	{
	  val = evaluate_object (CAR (arglist), env, outcome);

	  if (!val)
	    return NULL;
	}
      else
	val = CAR (arglist);

      bins = add_binding (create_binding (par->name, val, LEXICAL_BINDING), bins);
      args++;

      if (par->type == OPTIONAL_PARAM && par->supplied_p_param)
	{
	  bins = add_binding (create_binding (par->supplied_p_param, &t_object,
					      LEXICAL_BINDING), bins);
	  args++;
	}

      par = par->next;

      arglist = CDR (arglist);
    }

  if (par && par->type == REQUIRED_PARAM)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (arglist != &nil_object && (!par || (par && par->type != REST_PARAM)))
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  while (par && par->type == OPTIONAL_PARAM)
    {
      if (par->init_form)
	{
	  val = evaluate_object (par->init_form, env, outcome);

	  if (!val)
	    return NULL;

	  bins = add_binding (create_binding (par->name, val, LEXICAL_BINDING), bins);
	  args++;
	}
      else
	{
	  bins = add_binding (create_binding
			      (par->name, &nil_object, LEXICAL_BINDING), bins);
	  args++;
	}

      if (par->supplied_p_param)
	{
	  bins = add_binding (create_binding
			      (par->supplied_p_param, &nil_object, LEXICAL_BINDING),
			      bins);
	  args++;
	}

      par = par->next;
    }

  if (par && par->type == REST_PARAM)
    bins = add_binding (create_binding (par->name, arglist, LEXICAL_BINDING), bins);

  env->vars = chain_bindings (bins, env->vars);

  res = evaluate_body (func->value_ptr.function->body, eval_twice, env, outcome);

  env->vars = remove_bindings (env->vars, args);

  return res;
}


int
check_type (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct eval_outcome *outcome)
{
  if (typespec->type != TYPE_SYMBOL_NAME
      || !typespec->value_ptr.symbol_name->sym->value_ptr.symbol->is_type)
    {
      outcome->type = UNKNOWN_TYPE;

      return -1;
    }

  return typespec->value_ptr.symbol_name->sym->value_ptr.symbol->builtin_type
    (obj, typespec, env, outcome);
}


struct object *
evaluate_object (struct object *obj, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct binding *bind;
  struct object *sym;

  if (obj->type == TYPE_QUOTE)
    {
      increment_refcount (obj->value_ptr.next);
      return obj->value_ptr.next;
    }
  else if (obj->type == TYPE_BACKQUOTE)
    {
      increment_refcount (obj->value_ptr.next);

      return apply_backquote (obj->value_ptr.next, NULL, 0, NULL, 1, env,
			      outcome);
    }
  else if (obj->type == TYPE_SYMBOL || obj->type == TYPE_SYMBOL_NAME)
    {
      sym = SYMBOL (obj);

      if (sym->value_ptr.symbol->home_package == env->keyword_package)
	{
	  increment_refcount (sym);
	  return sym;
	}

      if (sym->value_ptr.symbol->is_const)
	{
	  increment_refcount (sym);
	  return sym->value_ptr.symbol->value_cell;
	}
      else if (sym->value_ptr.symbol->is_parameter
	       || sym->value_ptr.symbol->is_special)
	{
	  bind = find_binding (sym->value_ptr.symbol, env->vars, DYNAMIC_BINDING);

	  if (bind)
	    return bind->obj;

	  return sym->value_ptr.symbol->value_cell;
	}
      else
	{
	  bind = find_binding (sym->value_ptr.symbol, env->vars, LEXICAL_BINDING);

	  if (bind)
	    return bind->obj;
	  else
	    {
	      outcome->type = UNBOUND_SYMBOL;
	      outcome->obj = sym;
	      return NULL;
	    }
	}
    }
  else if (obj->type == TYPE_CONS_PAIR)
    {
      return evaluate_list (obj, env, outcome);
    }
  else
    {
      increment_refcount (obj);
      return obj;
    }
}


struct object *
apply_backquote (struct object *form, struct object *cons, int first_cons,
		 struct object *prev_prefix, int backts_commas_balance,
		 struct environment *env, struct eval_outcome *outcome)
{
  struct object *prev_cons, *cur_cons, *next_cons, *ret, *pref_copy;

  if (!backts_commas_balance)
    {
      return evaluate_object (form, env, outcome);
    }
  else if (form->type == TYPE_BACKQUOTE)
    {
      ret = apply_backquote (form->value_ptr.next, cons, first_cons, form,
			     backts_commas_balance + 1, env, outcome);

      if (!ret)
	return NULL;

      form->value_ptr.next = ret;

      return form;
    }
  else if (form->type == TYPE_COMMA)
    {
      if (backts_commas_balance == 1)
	{
	  if (form->value_ptr.next->type == TYPE_AT
	      || form->value_ptr.next->type == TYPE_DOT)
	    {
	      if (!cons)
		{
		  outcome->type = COMMA_AT_OR_DOT_NOT_ALLOWED_AT_TOP_LEVEL;

		  return NULL;
		}

	      ret = evaluate_object (form->value_ptr.next->value_ptr.next, env,
				     outcome);

	      if (!ret)
		return NULL;

	      pref_copy = prev_prefix
		? (first_cons
		   ? copy_prefix (CAR (cons), prev_prefix, NULL)
		   : copy_prefix (CAR (CDR (cons)), prev_prefix, NULL))
		: NULL;

	      if (form->value_ptr.next->type == TYPE_AT || prev_prefix)
		ret = copy_list_structure (ret, pref_copy);

	      if (first_cons)
		{
		  cons->value_ptr.cons_pair->car = CAR (ret);
		  last_cons_pair (ret)->value_ptr.cons_pair->cdr = CDR (cons);
		  cons->value_ptr.cons_pair->cdr = CDR (ret);
		}
	      else
		{
		  last_cons_pair (ret)->value_ptr.cons_pair->cdr =
		    CDR (CDR (cons));

		  cons->value_ptr.cons_pair->cdr = ret;
		}

	      return CAR (ret);
	    }

	  return evaluate_object (form->value_ptr.next, env, outcome);
	}
      else
	{
	  ret = apply_backquote (form->value_ptr.next, cons, first_cons, form, 
				 backts_commas_balance - 1, env, outcome);

	  if (!ret)
	    return NULL;

	  form->value_ptr.next = ret;

	  return form;
	}
    }
  else if (form->type == TYPE_CONS_PAIR)
    {
      prev_cons = cur_cons = form;
      first_cons = 1;

      while (cur_cons->type == TYPE_CONS_PAIR)
	{
	  next_cons = CDR (cur_cons);

	  ret = apply_backquote (CAR (cur_cons), prev_cons, first_cons, NULL,
				 backts_commas_balance, env, outcome);

	  if (!ret)
	    return NULL;

	  cur_cons->value_ptr.cons_pair->car = ret;

	  prev_cons = cur_cons;
	  cur_cons = next_cons;
	  first_cons = 0;
	}

      if (cur_cons->type != TYPE_NIL)
	{
	  ret = apply_backquote (cur_cons, cons, 0, NULL, backts_commas_balance,
				 env, outcome);

	  if (!ret)
	    return NULL;

	  prev_cons->value_ptr.cons_pair->cdr = ret;
	}
    }

  return form;
}


struct object *
evaluate_list (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct symbol_name *symname;
  struct binding *bind;
  struct object *args;

  if (is_dotted_list (list))
    {
      outcome->type = DOTTED_LIST_NOT_ALLOWED_HERE;

      return NULL;
    }

  if (CAR (list)->type != TYPE_SYMBOL_NAME)
    {
      outcome->type = INVALID_FUNCTION_CALL;
      outcome->obj = CAR (list);
      return NULL;
    }

  symname = CAR (list)->value_ptr.symbol_name;

  if (symname->sym->value_ptr.symbol->is_builtin_form)
    {
      if (symname->sym->value_ptr.symbol->evaluate_args)
	{
	  args = evaluate_through_list (CDR (list), env, outcome);

	  if (!args)
	    return NULL;
	}
      else
	args = CDR (list);

      return symname->sym->value_ptr.symbol->builtin_form (args, env, outcome);
    }

  if (symname_equals (symname, "LET"))
    {
      if (!(CDR (list)->type & TYPE_LIST))
	{
	  outcome->type = INCORRECT_SYNTAX_IN_LET;
	  return NULL;
	}

      return evaluate_let (CAR (CDR (list)), CDR (CDR (list)), env, outcome);
    }
  else if (symname_equals (symname, "LET*"))
    {
      if (!(CDR (list)->type & TYPE_LIST))
	{
	  outcome->type = INCORRECT_SYNTAX_IN_LET;
	  return NULL;
	}

      return evaluate_let_star (CAR (CDR (list)), CDR (CDR (list)), env, outcome);
    }
  else if (symname_equals (symname, "QUOTE"))
    {
      increment_refcount (CAR (CDR (list)));
      return CAR (CDR (list));
    }

  bind = find_binding (symname->sym->value_ptr.symbol, env->funcs, DYNAMIC_BINDING);

  if (bind)
    {
      if (bind->obj->type == TYPE_FUNCTION)
	return call_function (bind->obj, CDR (list), 1, 0, env, outcome);
      else
	return call_function (bind->obj, CDR (list), 0, 1, env, outcome);
    }

  outcome->type = UNKNOWN_FUNCTION;
  outcome->obj = CAR (list);
  return NULL;
}


struct object *
evaluate_through_list (struct object *list, struct environment *env,
		       struct eval_outcome *outcome)
{
  struct object *args = NULL, *cons, *last_cons, *obj;

  while (list != &nil_object)
    {
      obj = evaluate_object (CAR (list), env, outcome);

      if (!obj)
	return NULL;

      cons = alloc_empty_cons_pair ();
      cons->value_ptr.cons_pair->car = obj;

      if (!args)
	args = last_cons = cons;
      else
	last_cons = last_cons->value_ptr.cons_pair->cdr = cons;

      list = CDR (list);
    }

  if (!args)
    return &nil_object;

  return args;
}


int
type_string (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_STRING;
}


struct object *
builtin_car (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }
  if (list_length (list) > 1)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (!(list->value_ptr.cons_pair->car->type & TYPE_LIST))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return CAR (CAR (list));
}


struct object *
builtin_cdr (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }
  if (list_length (list) > 1)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (!(list->value_ptr.cons_pair->car->type & TYPE_LIST))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return CDR (CAR (list));
}


struct object *
builtin_cons (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  struct object *cons;

  if (list_length (list) < 2)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }
  if (list_length (list) > 2)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  cons = alloc_empty_cons_pair ();
  cons->value_ptr.cons_pair->car = CAR (list);
  cons->value_ptr.cons_pair->cdr = CAR (CDR (list));

  return cons;
}


struct object *
builtin_list (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  struct object *l = NULL, *cons, *last_cons;

  while (list != &nil_object)
    {
      cons = alloc_empty_cons_pair ();
      cons->value_ptr.cons_pair->car = list->value_ptr.cons_pair->car;

      if (!l)
	l = last_cons = cons;
      else
	last_cons = last_cons->value_ptr.cons_pair->cdr = cons;

      list = CDR (list);
    }

  return l;
}


struct object *
builtin_write (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  print_object (CAR (list), env);
  printf ("\n");

  return CAR (list);
}


struct object *
builtin_load (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  FILE *f;
  long l;
  char *buf;
  int end_loop = 0;
  enum read_outcome out;
  const char *in, *obj_b, *obj_e;
  size_t sz;
  size_t mult_comm_depth = 0;
  struct object *obj = NULL, *res;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }
  if (list_length (list) > 1)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  f = fopen (CAR (list)->value_ptr.string->value, "r");

  if (!f)
    {
      outcome->type = COULD_NOT_OPEN_FILE_FOR_READING;
      return NULL;
    }

  if (fseek (f, 0l, SEEK_END))
    {
      outcome->type = COULD_NOT_SEEK_FILE;
      return NULL;
    }

  if ((l = ftell (f)) == -1)
    {
      outcome->type = COULD_NOT_TELL_FILE;
      return NULL;
    }

  if (fseek (f, 0l, SEEK_SET))
    {
      outcome->type = COULD_NOT_SEEK_FILE;
      return NULL;
    }

  buf = malloc_and_check (l);

  if (!fread (buf, l, 1, f))
    {
      outcome->type = ERROR_READING_FILE;
      return NULL;
    }

  out = read_object (&obj, 0, buf, l, env, outcome, &obj_b, &obj_e,
		     &mult_comm_depth);
  sz = l - (obj_e + 1 - buf);
  in = obj_e + 1;

  while (!end_loop)
    {
      if (mult_comm_depth)
	{
	  free (buf);
	  fclose (f);

	  return &nil_object;
	}
      else if (out == COMPLETE_OBJECT)
	{
	  res = evaluate_object (obj, env, outcome);

	  if (res)
	    {
	      out = read_object (&obj, 0, in, sz, env, outcome, &obj_b, &obj_e,
				 &mult_comm_depth);
	      sz = sz - (obj_e + 1 - in);
	      in = obj_e + 1;
	    }
	  else
	    {
	      print_eval_error (outcome, env);

	      free (buf);
	      fclose (f);

	      return &nil_object;
	    }
	}
      else if (out == NO_OBJECT)
	{
	  free (buf);
	  fclose (f);

	  return &t_object;
	}
      else if (out & READ_ERROR || out & INCOMPLETE_OBJECT)
	{
	  print_read_error (out, buf, l, obj_b, obj_e);

	  free (buf);
	  fclose (f);

	  return &nil_object;
	}
    }

  free (buf);
  fclose (f);

  return NULL;
}


struct object *
builtin_eq (struct object *list, struct environment *env,
	    struct eval_outcome *outcome)
{
  struct object *arg1, *arg2;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (nth (0, list)->type == TYPE_SYMBOL_NAME)
    arg1 = SYMBOL (nth (0, list));
  else
    arg1 = nth (0, list);

  if (nth (1, list)->type == TYPE_SYMBOL_NAME)
    arg2 = SYMBOL (nth (1, list));
  else
    arg2 = nth (1, list);

  if (arg1 == arg2)
    return &t_object;

  return &nil_object;
}


struct object *
builtin_not (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_NIL)
    return &t_object;

  return &nil_object;
}


enum object_type
highest_num_type (enum object_type t1, enum object_type t2)
{
  if (t1 == TYPE_FLOAT || t2 == TYPE_FLOAT)
    return TYPE_FLOAT;

  if (t1 == TYPE_RATIO || t2 == TYPE_RATIO)
    return TYPE_RATIO;

  return TYPE_INTEGER;
}


struct object *
copy_number (const struct object *num)
{
  struct object *ret = malloc_and_check (sizeof (*ret));

  ret->refcount = 1;
  ret->type = num->type;

  if (num->type == TYPE_INTEGER)
    {
      mpz_init (ret->value_ptr.integer);
      mpz_set (ret->value_ptr.integer, num->value_ptr.integer);
    }
  else if (num->type == TYPE_RATIO)
    {
      mpq_init (ret->value_ptr.ratio);
      mpq_set (ret->value_ptr.ratio, num->value_ptr.ratio);
    }
  else
    {
      mpf_init (ret->value_ptr.floating);
      mpf_set (ret->value_ptr.floating, num->value_ptr.floating);
    }

  return ret;
}


struct object *
promote_number (struct object *num, enum object_type type)
{
  struct object *ret;

  if (num->type == type)
    return num;

  ret = malloc_and_check (sizeof (*ret));
  ret->type = type;

  if (type == TYPE_RATIO)
    {
      mpq_init (ret->value_ptr.ratio);
      mpq_set_z (ret->value_ptr.ratio, num->value_ptr.integer);
    }
  else if (type == TYPE_FLOAT)
    {
      mpf_init (ret->value_ptr.floating);

      if (num->type == TYPE_INTEGER)
	mpf_set_z (ret->value_ptr.floating, num->value_ptr.integer);
      else if (num->type == TYPE_RATIO)
	mpf_set_q (ret->value_ptr.floating, num->value_ptr.ratio);
    }

  return ret;
}


struct object *
apply_arithmetic_operation (struct object *list,
			    void (*opz) (mpz_t, const mpz_t, const mpz_t),
			    void (*opq) (mpq_t, const mpq_t, const mpq_t),
			    void (*opf) (mpf_t, const mpf_t, const mpf_t),
			    struct environment *env, struct eval_outcome *outcome)
{
  struct object *ret, *op;

  if (!(CAR (list)->type & TYPE_NUMBER) || !(CAR (CDR (list))->type & TYPE_NUMBER))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (highest_num_type (CAR (list)->type, CAR (CDR (list))->type) == CAR (list)->type)
    ret = copy_number (CAR (list));
  else
    ret = promote_number (CAR (list), highest_num_type (CAR (list)->type,
							CAR (CDR (list))->type));

  list = CDR (list);

  do
    {
      op = promote_number (CAR (list), highest_num_type (ret->type, CAR (list)->type));

      if (ret->type == TYPE_INTEGER)
	{
	  opz (ret->value_ptr.integer, ret->value_ptr.integer, op->value_ptr.integer);
	}
      else if (ret->type == TYPE_RATIO)
	{
	  opq (ret->value_ptr.ratio, ret->value_ptr.ratio, op->value_ptr.ratio);
	}
      else if (ret->type == TYPE_FLOAT)
	{
	  opf (ret->value_ptr.floating, ret->value_ptr.floating, op->value_ptr.floating);
	}

      list = CDR (list);

    } while (list != &nil_object);

  return ret;
}


struct object *
builtin_plus (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  struct object *ret;

  if (!list_length (list))
    {
      ret = malloc_and_check (sizeof (*ret));

      ret->type = TYPE_INTEGER;
      ret->refcount = 1;

      mpz_init (ret->value_ptr.integer);
      mpz_set_si (ret->value_ptr.integer, 0);

      return ret;
    }
  else if (list_length (list) == 1)
    {
      if (!(CAR (list)->type & TYPE_NUMBER))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      return CAR (list);
    }

  return apply_arithmetic_operation (list, mpz_add, mpq_add, mpf_add, env, outcome);
}


struct object *
builtin_minus (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct object *ret;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (list_length (list) == 1)
    {
      if (!(CAR (list)->type & TYPE_NUMBER))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = copy_number (CAR (list));

      if (ret->type == TYPE_INTEGER)
	{
	  mpz_neg (ret->value_ptr.integer, ret->value_ptr.integer);
	}
      else if (ret->type == TYPE_RATIO)
	{
	  mpq_neg (ret->value_ptr.ratio, ret->value_ptr.ratio);
	}
      else if (ret->type == TYPE_FLOAT)
	{
	  mpf_neg (ret->value_ptr.floating, ret->value_ptr.floating);
	}

      return ret;
    }

  return apply_arithmetic_operation (list, mpz_sub, mpq_sub, mpf_sub, env, outcome);
}


struct object *
builtin_multiply (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  struct object *ret;

  if (!list_length (list))
    {
      ret = malloc_and_check (sizeof (*ret));

      ret->type = TYPE_INTEGER;
      ret->refcount = 1;

      mpz_init (ret->value_ptr.integer);
      mpz_set_si (ret->value_ptr.integer, 1);

      return ret;
    }
  else if (list_length (list) == 1)
    {
      if (!(CAR (list)->type & TYPE_NUMBER))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      return CAR (list);
    }

  return apply_arithmetic_operation (list, mpz_mul, mpq_mul, mpf_mul, env, outcome);
}


struct object *
builtin_divide (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  return NULL;
}


struct object *
builtin_typep (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  int ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  ret = check_type
    (nth (0, list), nth (1, list), env, outcome);

  if (ret == -1)
    return NULL;
  else if (ret)
    return &t_object;
  else
    return &nil_object;
}


struct binding *
create_binding_from_let_form (struct object *form, struct environment *env,
			      struct eval_outcome *outcome)
{
  struct object *sym, *val;

  if (form->type == TYPE_SYMBOL_NAME || form->type == TYPE_SYMBOL)
    {
      sym = SYMBOL (form);

      if (sym->value_ptr.symbol->is_const)
	{
	  outcome->type = CANT_REBIND_CONSTANT;
	  return NULL;
	}

      if (sym->value_ptr.symbol->is_parameter)
	return create_binding (sym, &nil_object, DYNAMIC_BINDING);
      else
	return create_binding (sym, &nil_object, LEXICAL_BINDING);
    }
  else if (form->type == TYPE_CONS_PAIR)
    {
      if (list_length (form) != 2
	  || (CAR (form)->type != TYPE_SYMBOL_NAME && CAR (form)->type != TYPE_SYMBOL)) 
	{
	  outcome->type = INCORRECT_SYNTAX_IN_LET;
	  return NULL;
	}

      sym = SYMBOL (CAR (form));

      if (sym->value_ptr.symbol->is_const)
	{
	  outcome->type = CANT_REBIND_CONSTANT;
	  return NULL;
	}

      val = evaluate_object (CAR (CDR (form)), env, outcome);

      if (!val)
	return NULL;

      if (sym->value_ptr.symbol->is_parameter)
	return create_binding (sym, val, DYNAMIC_BINDING);
      else
	return create_binding (sym, val, LEXICAL_BINDING);
    }
  else
    {
      outcome->type = INCORRECT_SYNTAX_IN_LET;
      return NULL;
    }
}


struct object *
evaluate_let (struct object *bind_forms, struct object *body,
	      struct environment *env, struct eval_outcome *outcome)
{
  struct object *res;
  int binding_num = 0;
  struct binding *bins = NULL, *bin;

  while (bind_forms != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome);

      if (!bin)
	return NULL;

      bins = add_binding (bin, bins);
      binding_num++;

      bind_forms = CDR (bind_forms);
    }

  env->vars = chain_bindings (bins, env->vars);

  res = evaluate_progn (body, env, outcome);

  env->vars = remove_bindings (env->vars, binding_num);

  return res;
}


struct object *
evaluate_let_star (struct object *bind_forms, struct object *body,
		   struct environment *env, struct eval_outcome *outcome)
{
  struct object *res;
  int binding_num = 0;
  struct binding *bin;

  while (bind_forms != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome);

      if (!bin)
	return NULL;

      env->vars = add_binding (bin, env->vars);
      binding_num++;

      bind_forms = CDR (bind_forms);
    }

  res = evaluate_progn (body, env, outcome);

  env->vars = remove_bindings (env->vars, binding_num);

  return res;
}


struct object *
evaluate_if (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  struct object *if_clause;

  if (!list)
    {
      outcome->type = MALFORMED_IF;
      return NULL;
    }

  if_clause = evaluate_object (CAR (list), env, outcome);

  if (!if_clause)
    return NULL;

  if (if_clause != &nil_object)
    {
      if (CDR (list) == &nil_object)
	{
	  outcome->type = MALFORMED_IF;
	  return NULL;
	}

      return evaluate_object (CAR (CDR (list)), env, outcome);
    }
  else
    {
      if (!nth (2, list))
	{
	  return &nil_object;
	}
      else
	return evaluate_object (nth (2, list), env, outcome);
    }
}


struct object *
evaluate_progn (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  return evaluate_body (list, 0, env, outcome);
}


struct object *
evaluate_defconstant (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  return define_constant (CAR (list)->value_ptr.symbol_name->sym, CAR (CDR (list)),
			  env, outcome);
}


struct object *
evaluate_defparameter (struct object *list, struct environment *env,
		       struct eval_outcome *outcome)
{
  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  return define_parameter (CAR (list), CAR (CDR (list)), env, outcome);
}


struct object *
evaluate_defvar (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct object *s = CAR (list);
  
  if (s->type == TYPE_SYMBOL_NAME)
    s = s->value_ptr.symbol_name->sym;
  
  if (list_length (list) == 1)
    {
      s->value_ptr.symbol->is_special = 1;
    }
  else if (list_length (list) == 2)
    {
      s->value_ptr.symbol->is_special = 1;
      
      if (!s->value_ptr.symbol->value_cell)
	return define_parameter (CAR (list), CAR (CDR (list)), env, outcome);
    }
  else
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  return CAR (list);
}


struct object *
evaluate_defun (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *fun;

  if (CAR (list)->type != TYPE_SYMBOL_NAME
      || !(CAR (CDR (list))->type & TYPE_LIST))
    {
      outcome->type = INCORRECT_SYNTAX_IN_DEFUN;
      return NULL;
    }

  fun = create_function (CAR (CDR (list)), CDR (CDR (list)));

  env->funcs = add_binding (create_binding (SYMBOL (CAR (list)), fun,
					    DYNAMIC_BINDING), env->funcs);

  return CAR (list);
}


struct object *
evaluate_defmacro (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *fun;

  if (CAR (list)->type != TYPE_SYMBOL_NAME
      || !(CAR (CDR (list))->type & TYPE_LIST))
    {
      outcome->type = INCORRECT_SYNTAX_IN_DEFUN;
      return NULL;
    }

  fun = create_function (CAR (CDR (list)), CDR (CDR (list)));

  fun->type = TYPE_MACRO;

  env->funcs = add_binding (create_binding (SYMBOL (CAR (list)), fun,
					    DYNAMIC_BINDING), env->funcs);

  return CAR (list);
}


int
eqmem (const char *s1, size_t n1, const char *s2, size_t n2)
{
  size_t i;
  
  if (n1 != n2)
    return 0;

  for (i = 0; i < n1; i++)
    if (s1 [i] != s2 [i])
      return 0;

  return 1;
}


int
symname_equals (const struct symbol_name *sym, const char *s)
{
  return eqmem (sym->value, sym->used_size, s, strlen (s));
}


int
symname_is_among (const struct symbol_name *sym, ...)
{
  va_list valist;
  char *s;
  
  va_start (valist, sym);

  while ((s = va_arg (valist, char *)))
    {
      if (symname_equals (sym, s))
	{
	  va_end (valist);
	  return 1;
	}
    }
  
  va_end (valist);
  return 0;
}

  
int
equal_strings (const struct string *s1, const struct string *s2)
{
  size_t i;
  
  if (s1->used_size != s2->used_size)
    return 0;

  for (i = 0; i < s1->used_size; i++)
    if (s1->value [i] != s2->value [i])
      return 0;

  return 1;
}


void
print_symbol (const struct symbol *sym, struct environment *env)
{  
  size_t i;
  char *nm = sym->name;
  char need_escape [] = "().,;'#\"\n\\";
  int do_need_escape = 0;

  if (sym->home_package == env->keyword_package)
    printf (":");

  /* first we make a pass to understand if we need vertical escape */
  for (i = 0; i < sym->name_len && !do_need_escape; i++)
    {
      if (strchr (need_escape, nm [i]) || !nm [i] || islower (nm [i]))
	do_need_escape = 1;
    }

  if (do_need_escape)
    putchar ('|');

  for (i = 0; i < sym->name_len; i++)
    {
      if (nm [i] == '|' || nm [i] == '\\')
	putchar ('\\');
      
      putchar (nm [i]);
    }

  if (do_need_escape)
    putchar ('|');
}


void
print_string (const struct string *str)
{
  size_t i;
  
  putchar ('"');

  for (i = 0; i < str->used_size; i++)
    {
      if (str->value [i] == '"' || str->value [i] == '\\')
	putchar ('\\');
      
      putchar (str->value [i]);
    }

  putchar ('"');  
}


void
print_character (const char *character)
{
  printf ("#\\");

  if (strlen (character) == 1)
    {
      switch (character [0])
	{
	case '\n':
	  printf ("Newline");
	  break;
	case ' ':
	  printf ("Space");
	  break;
	case '\t':
	  printf ("Tab");
	  break;
	case '\b':
	  printf ("Backspace");
	  break;
	case '\f':
	  printf ("Page");
	  break;
	case '\r':
	  printf ("Return");
	  break;
	default:
	  printf ("%s", character);
	  break;
	}
    }
  else
    printf ("%s", character);
}


void
print_filename (const struct filename *fn)
{
  printf ("#P");
  print_string (fn->value->value_ptr.string);
}


void
print_list (const struct cons_pair *list, struct environment *env)
{
  struct object *cdr;
  
  printf ("(");

  print_object (list->car, env);

  cdr = list->cdr;
  
  while (cdr && cdr->type != TYPE_NIL)
    {
      if (cdr->type == TYPE_CONS_PAIR)
	{
	  printf (" ");
	  print_object (cdr->value_ptr.cons_pair->car, env);
	  cdr = cdr->value_ptr.cons_pair->cdr;
	}
      else
	{
	  printf (" . ");
	  print_object (cdr, env);
	  break;
	}
    }
  
  printf (")");
}


void
print_array (const struct array *array, struct environment *env)
{
  int rk = array_rank (array);
  size_t i;

  if (rk == 1)
    {
      printf ("#(");

      for (i = 0; i < (array->fill_pointer > 0 ? array->fill_pointer :
		       array->alloc_size->size); i++)
	{
	  if (i)
	    printf (" ");

	  print_object (array->value [i], env);
	}

      printf (")");
    }
}


void
print_object (const struct object *obj, struct environment *env)
{
  if (obj->type == TYPE_NIL)
    printf ("()");
  else if (obj->type == TYPE_T)
    printf ("T");
  else if (obj->type == TYPE_QUOTE)
    {
      printf ("'");
      print_object (obj->value_ptr.next, env);
    }
  else if (obj->type == TYPE_BACKQUOTE)
    {
      printf ("`");
      print_object (obj->value_ptr.next, env);
    }
  else if (obj->type == TYPE_COMMA)
    {
      printf (",");
      print_object (obj->value_ptr.next, env);
    }
  else if (obj->type == TYPE_INTEGER)
    mpz_out_str (NULL, 10, obj->value_ptr.integer);
  else if (obj->type == TYPE_RATIO)
    mpq_out_str (NULL, 10, obj->value_ptr.ratio);
  else if (obj->type == TYPE_FLOAT)
    mpf_out_str (NULL, 10, 0, obj->value_ptr.floating);
  else if (obj->type == TYPE_STRING)
    print_string (obj->value_ptr.string);
  else if (obj->type == TYPE_CHARACTER)
    print_character (obj->value_ptr.character);
  else if (obj->type == TYPE_FILENAME)
    print_filename (obj->value_ptr.filename);
  else if (obj->type == TYPE_SYMBOL_NAME)
    print_symbol (obj->value_ptr.symbol_name->sym->value_ptr.symbol, env);
  else if (obj->type == TYPE_SYMBOL)
    print_symbol (obj->value_ptr.symbol, env);
  else if (obj->type == TYPE_CONS_PAIR)
    print_list (obj->value_ptr.cons_pair, env);
  else if (obj->type == TYPE_ARRAY)
    print_array (obj->value_ptr.array, env);
  else if (obj->type == TYPE_FUNCTION)
    printf ("#<FUNCTION %p>", obj);
  else if (obj->type == TYPE_PACKAGE)
    {
      printf ("#<PACKAGE \"");
      print_symbol (obj->value_ptr.package->name->value_ptr.symbol, env);
      printf ("\">");
    }
  else
    printf ("#<print not implemented>");
}


void
print_read_error (enum read_outcome err, const char *input, size_t size,
		  const char *begin, const char *end)
{
  if (err == CLOSING_PARENTHESIS)
    {
      printf ("read error: mismatched closing parenthesis\n");
    }
  else if (err == CLOSING_PARENTHESIS_AFTER_PREFIX)
    {
      printf ("read error: closing parenthesis can't follows commas, ticks, "
	      "backticks\n");
    }
  else if (err == INVALID_SHARP_DISPATCH)
    {
      printf ("read error: invalid character used as a sharp dispatch\n");
    }
  else if (err == UNKNOWN_SHARP_DISPATCH)
    {
      printf ("read error: character not known as a sharp dispatch\n");
    }
  else if (err == WRONG_OBJECT_TYPE_TO_SHARP_MACRO)
    {
      printf ("read error: wrong type of object as content of a sharp macro\n");
    }
  else if (err == UNKNOWN_CHARACTER_NAME)
    {
      printf ("read error: unknown character name\n");
    }
  else if (err == FUNCTION_NOT_FOUND)
    {
      printf ("read error: function not found\n");
    }
  else if (err == COMMA_WITHOUT_BACKQUOTE)
    {
      printf ("read error: comma can appear only inside a backquoted form\n");
    }
  else if (err == TOO_MANY_COMMAS)
    {
      printf ("read error: number of commas can't exceed number of pending "
	      "backquotes\n");
    }
  else if (err == SINGLE_DOT)
    {
      printf ("read error: single dot is only allowed inside a list and must "
	      "be followed by exactly one object\n");
    }
  else if (err == MULTIPLE_DOTS)
    {
      printf ("read error: symbol names made of non-escaped dots only are "
	      "not allowed\n");
    }
  else if (err == NO_OBJ_BEFORE_DOT_IN_LIST)
    {
      printf ("read error: no object before dot in list\n");
    }
  else if (err == NO_OBJ_AFTER_DOT_IN_LIST)
    {
      printf ("read error: no object follows dot in list\n");
    }
  else if (err == MULTIPLE_OBJS_AFTER_DOT_IN_LIST)
    {
      printf ("read error: more than one object follows dot in list\n");
    }
  else if (err == TOO_MANY_COLONS)
    {
      printf ("read error: more than two consecutive colons cannot appear in a token\n");
    }
  else if (err == CANT_BEGIN_WITH_TWO_COLONS_OR_MORE)
    {
      printf ("read error: a token can't begin with two colons or more\n");
    }
  else if (err == CANT_END_WITH_PACKAGE_SEPARATOR)
    {
      printf ("read error: a token can't end with a package separator\n");
    }
  else if (err == MORE_THAN_A_PACKAGE_SEPARATOR)
    {
      printf ("read error: more than a package separator not allowed in token\n");
    }
  else if (err == PACKAGE_NOT_FOUND)
    {
      printf ("read error: package not found\n");
    }
}


void
print_eval_error (struct eval_outcome *err, struct environment *env)
{
  if (err->type == UNBOUND_SYMBOL)
    {
      printf ("eval error: symbol ");
      print_symbol (err->obj->value_ptr.symbol, env);
      printf (" not bound to any object\n");
    }
  else if (err->type == UNKNOWN_FUNCTION)
    {
      printf ("eval error: symbol ");
      print_symbol (err->obj->value_ptr.symbol, env);
      printf (" not bound to any function\n");
    }
  else if (err->type == INVALID_FUNCTION_CALL)
    {
      printf ("eval error: invalid function call\n");
    }
  else if (err->type == DOTTED_LIST_NOT_ALLOWED_HERE)
    {
      printf ("eval error: dotted list not allowed here\n");
    }
  else if (err->type == COMMA_AT_OR_DOT_NOT_ALLOWED_AT_TOP_LEVEL)
    {
      printf ("eval error: comma-at and comma-dot syntax are not allowed on "
	      "top-level forms\n");
    }
  else if (err->type == WRONG_NUMBER_OF_ARGUMENTS)
    {
      printf ("eval error: wrong number of arguments\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_LET)
    {
      printf ("eval error: incorrect syntax in let\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_DEFUN)
    {
      printf ("eval error: incorrect syntax in defun\n");
    }
  else if (err->type == CANT_REDEFINE_CONSTANT)
    {
      printf ("eval error: redefining constants is not allowed\n");
    }
  else if (err->type == CANT_REBIND_CONSTANT)
    {
      printf ("eval error: rebinding constants is not allowed\n");
    }
  else if (err->type == TOO_FEW_ARGUMENTS)
    {
      printf ("eval error: too few arguments to function call\n");
    }
  else if (err->type == TOO_MANY_ARGUMENTS)
    {
      printf ("eval error: too many arguments to function call\n");
    }
  else if (err->type == WRONG_TYPE_OF_ARGUMENT)
    {
      printf ("type error: wrong type of argument\n");
    }
  else if (err->type == COULD_NOT_OPEN_FILE_FOR_READING)
    {
      printf ("file error: could not open file for reading\n");
    }
  else if (err->type == COULD_NOT_SEEK_FILE)
    {
      printf ("file error: could not seek file\n");
    }
  else if (err->type == COULD_NOT_TELL_FILE)
    {
      printf ("file error: could not tell file\n");
    }
  else if (err->type == ERROR_READING_FILE)
    {
      printf ("file error: could not read file\n");
    }
  else if (err->type == UNKNOWN_TYPE)
    {
      printf ("eval error: type not known\n");
    }
}


void
increment_refcount (struct object *obj)
{
  if (!obj || obj == &nil_object  || obj == &nil_symbol || obj == &t_object)
    return;

  obj->refcount++;

  if (obj->type & TYPE_PREFIX)
    increment_refcount (obj->value_ptr.next);
  else if (obj->type == TYPE_CONS_PAIR)
    {
      increment_refcount (obj->value_ptr.cons_pair->car);
      increment_refcount (obj->value_ptr.cons_pair->cdr);
    }
}


int
decrement_refcount (struct object *obj)
{
  if (!obj)
    return 0;

  if (obj == &nil_object || obj == &nil_symbol || obj == &t_object)
    return 0;

  obj->refcount--;

  if (obj->type & TYPE_PREFIX && decrement_refcount (obj->value_ptr.next))
    obj->value_ptr.next = NULL;

  if (obj->type == TYPE_CONS_PAIR)
    {
      if (decrement_refcount (obj->value_ptr.cons_pair->car))
	obj->value_ptr.cons_pair->car = NULL;

      if (decrement_refcount (obj->value_ptr.cons_pair->cdr))
	obj->value_ptr.cons_pair->cdr = NULL;
    }

  if (!obj->refcount)
    {
      free_object (obj);
      return 1;
    }

  return 0;
}


void
free_object (struct object *obj)
{
  if (!obj || obj->refcount < 0)
    return;

  if (obj->type == TYPE_STRING)
    free_string (obj);
  else if (obj->type == TYPE_SYMBOL && !obj->value_ptr.symbol->is_special)
    free_symbol (obj);
  else if (obj->type & TYPE_PREFIX)
    {
      if (decrement_refcount (obj->value_ptr.next))
	obj->value_ptr.next = NULL;

      free (obj);
    }
  else if (obj->type == TYPE_CONS_PAIR)
    free_cons_pair (obj);
  else if (obj->type == TYPE_FILENAME)
    {
      free_string (obj->value_ptr.filename->value);
      free (obj->value_ptr.filename);
      free (obj);
    }
  else if (obj->type == TYPE_ARRAY)
    free_array (obj);
  else if (obj->type == TYPE_CHARACTER)
    {
      free (obj->value_ptr.character);
      free (obj);
    }
}


void
free_string (struct object *obj)
{
  free (obj->value_ptr.string->value);
  free (obj->value_ptr.string);
  free (obj);
}


void
free_symbol (struct object *obj)
{
  free (obj->value_ptr.symbol->name);
  free (obj);
}


void
free_cons_pair (struct object *obj)
{
  decrement_refcount (obj->value_ptr.cons_pair->car);
  decrement_refcount (obj->value_ptr.cons_pair->cdr);

  free (obj->value_ptr.cons_pair);
  free (obj);
}


void
free_array_size (struct array_size *size)
{
  if (size->next)
    free_array_size (size->next);

  free (size);
}


void
free_array (struct object *obj)
{
  int r = array_rank (obj->value_ptr.array);
  size_t sz;

  if (r == 1)
    {
      sz = obj->value_ptr.array->alloc_size->size;

      free_array_size (obj->value_ptr.array->alloc_size);

      for (; sz; sz--)
	free_object (obj->value_ptr.array->value [sz - 1]);

      free (obj->value_ptr.array->value);
      free (obj->value_ptr.array);
      free (obj);
    }
}


void
free_list_structure (struct object *obj)
{
  if (!obj->value_ptr.cons_pair->cdr)
    free (obj);
  else
    free_list_structure (obj->value_ptr.cons_pair->cdr);
}


void
free_list_recursively (struct object *obj)
{
  free_object (obj->value_ptr.cons_pair->car);
  free_object (obj->value_ptr.cons_pair->cdr);
  free (obj);
}


void
print_welcome_message (void)
{
  puts ("al Copyright (C) 2022 Andrea G. Monaco\n"
	"This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\n"
	"This is free software, and you are welcome to redistribute it\n"
	"under certain conditions; type `show c' for details.\n");
}


void
print_version (void)
{
  puts ("al " PACKAGE_VERSION "\n"
	"Copyright (C) 2022 Andrea G. Monaco\n"
	"License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>\n"
	"This is free software: you are free to change and redistribute it.\n"
	"There is NO WARRANTY, to the extent permitted by law.");
}


void
print_help (void)
{

}
