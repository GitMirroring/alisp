/*  Copyright (C) 2022 Andrea G. Monaco
 *
 *  This file is part of alisp, a lisp implementation.
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

#include <gmp.h>

#ifdef HAVE_LIBREADLINE
#include <errno.h>
#include <readline/readline.h>
#include <readline/history.h>
#endif


#ifndef HAVE_MEMMEM
#define memmem al_memmem
#endif


#define CAR(list) ((list) == &nil_object ? &nil_object :\
		   (list)->value_ptr.cons_pair->car)

#define CDR(list) ((list) == &nil_object ? &nil_object :		\
		   (list)->value_ptr.cons_pair->cdr ?			\
		   (list)->value_ptr.cons_pair->cdr : &nil_object)


#define IS_LIST(s) ((s)->type == TYPE_CONS_PAIR || (s) == &nil_object)

#define IS_SYMBOL(s) ((s)->type == TYPE_SYMBOL || (s)->type == TYPE_SYMBOL_NAME)

#define IS_NUMBER(s) ((s)->type == TYPE_INTEGER || (s)->type == TYPE_RATIO \
		      || (s)->type == TYPE_FLOAT)

#define SYMBOL(s) ((s)->type == TYPE_SYMBOL ? (s) :	\
		   (s)->value_ptr.symbol_name->sym) 


#define HAS_LEAF_TYPE(obj) ((obj)->type & (TYPE_INTEGER | TYPE_RATIO	\
					   | TYPE_FLOAT | TYPE_STRING	\
					   | TYPE_CHARACTER | TYPE_FILENAME \
					   | TYPE_STREAM))


#define CLEAR_MULTIPLE_OR_NO_VALUES(out)	\
  do						\
    {						\
      (out).no_value = 0;			\
      free_object_list ((out).other_values);	\
      (out).other_values = NULL;		\
    } while (0)


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
go_tag
{
  struct object *name;

  struct object *dest;

  struct go_tag *next;
};


struct
go_tag_frame
{
  struct go_tag *frame;

  struct go_tag_frame *next;
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
  int var_lex_bin_num;

  struct binding *funcs;
  int func_lex_bin_num;

  struct binding *packages;

  struct object *keyword_package;
  struct object *current_package;

  struct go_tag_frame *go_tag_stack;

  /*struct global_environment *glob_env;
  struct dynamic_environment *dyn_env;
  struct lexical_environment *lex_env;*/
};


struct
read_outcome_args
{
  size_t multiline_comment_depth;
  struct object *obj;
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
    FUNCTION_NOT_FOUND_IN_READ = 1 << 15,

    UNFINISHED_SINGLELINE_COMMENT = 1 << 16,
    UNFINISHED_MULTILINE_COMMENT = 1 << 17,

    COMMA_WITHOUT_BACKQUOTE = 1 << 18,
    TOO_MANY_COMMAS = 1 << 19,

    SINGLE_DOT = 1 << 20,

    MULTIPLE_DOTS = 1 << 21,

    NO_OBJ_BEFORE_DOT_IN_LIST = 1 << 22,
    NO_OBJ_AFTER_DOT_IN_LIST = 1 << 23,
    MULTIPLE_OBJS_AFTER_DOT_IN_LIST = 1 << 24,
    MORE_THAN_A_CONSING_DOT_NOT_ALLOWED = 1 << 25,

    TOO_MANY_COLONS = 1 << 26,
    CANT_BEGIN_WITH_TWO_COLONS_OR_MORE = 1 << 27,
    CANT_END_WITH_PACKAGE_SEPARATOR = 1 << 28,
    MORE_THAN_A_PACKAGE_SEPARATOR = 1 << 29,
    PACKAGE_NOT_FOUND = 1 << 30,
    PACKAGE_MARKER_IN_SHARP_COLON = 1 << 31
  };


#define INCOMPLETE_OBJECT (JUST_PREFIX | INCOMPLETE_NONEMPTY_LIST |	\
			   INCOMPLETE_STRING | INCOMPLETE_SYMBOL_NAME | \
			   INCOMPLETE_SHARP_MACRO_CALL | INCOMPLETE_EMPTY_LIST)

#define READ_ERROR (CLOSING_PARENTHESIS_AFTER_PREFIX | CLOSING_PARENTHESIS \
		    | INVALID_SHARP_DISPATCH | UNKNOWN_SHARP_DISPATCH	\
		    | WRONG_OBJECT_TYPE_TO_SHARP_MACRO | UNKNOWN_CHARACTER_NAME	\
		    | FUNCTION_NOT_FOUND_IN_READ			\
		    | COMMA_WITHOUT_BACKQUOTE | TOO_MANY_COMMAS | SINGLE_DOT \
		    | MULTIPLE_DOTS | NO_OBJ_BEFORE_DOT_IN_LIST		\
		    | NO_OBJ_AFTER_DOT_IN_LIST				\
		    | MULTIPLE_OBJS_AFTER_DOT_IN_LIST			\
		    | MORE_THAN_A_CONSING_DOT_NOT_ALLOWED | TOO_MANY_COLONS \
		    | CANT_BEGIN_WITH_TWO_COLONS_OR_MORE		\
		    | CANT_END_WITH_PACKAGE_SEPARATOR			\
		    | MORE_THAN_A_PACKAGE_SEPARATOR | PACKAGE_NOT_FOUND \
		    | PACKAGE_MARKER_IN_SHARP_COLON)


enum
eval_outcome_type
  {
    EVAL_OK,
    UNBOUND_SYMBOL,
    INVALID_FUNCTION_CALL,
    KEY_NOT_FOUND_IN_FUNCALL,
    ODD_NUMBER_OF_ARGUMENTS,
    ODD_NUMBER_OF_KEYWORD_ARGUMENTS,
    DOTTED_LIST_NOT_ALLOWED_HERE,
    COMMA_AT_OR_DOT_NOT_ALLOWED_AT_TOP_LEVEL,
    CANT_SPLICE_AFTER_CONSING_DOT,
    SPLICING_OF_ATOM_NOT_ALLOWED_HERE,
    NOTHING_EXPANDED_BEFORE_CONSING_DOT,
    UNKNOWN_FUNCTION,
    MALFORMED_IF,
    INCORRECT_SYNTAX_IN_LET,
    INCORRECT_SYNTAX_IN_FLET,
    INCORRECT_SYNTAX_IN_DEFUN,
    INCORRECT_SYNTAX_IN_DEFMACRO,
    INVALID_LAMBDA_LIST,
    CANT_REDEFINE_SPECIAL_OPERATOR,
    CANT_REDEFINE_CONSTANT,
    CANT_REBIND_CONSTANT,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS,
    WRONG_NUMBER_OF_ARGUMENTS,
    WRONG_TYPE_OF_ARGUMENT,
    WRONG_NUMBER_OF_AXIS,
    OUT_OF_BOUND_INDEX,
    COULD_NOT_OPEN_FILE,
    COULD_NOT_OPEN_FILE_FOR_READING,
    COULD_NOT_SEEK_FILE,
    COULD_NOT_TELL_FILE,
    ERROR_READING_FILE,
    UNKNOWN_TYPE,
    CANT_GO_OUTSIDE_TAGBODY,
    INVALID_GO_TAG,
    CANT_GO_TO_NONEXISTENT_TAG,
    INVALID_ACCESSOR,
    FUNCTION_NOT_FOUND_IN_EVAL,
    DECLARE_NOT_ALLOWED_HERE,
    CANT_DIVIDE_BY_ZERO,
    INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT
  };


struct
eval_outcome
{
  enum eval_outcome_type type;

  int no_value;
  struct object_list *other_values;

  struct object *obj;

  struct object *tag_to_find;
  int find_tag_now;

  struct object *cont;
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
  int is_standard_type;
  int (*builtin_type) (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct eval_outcome *outcome);
  struct object *typespec;
  struct object_list *parent_types;

  struct object *(*builtin_accessor) (struct object *list,
				      struct object *newvalform,
				      struct environment *env,
				      struct eval_outcome *outcome);

  int is_const;
  int is_parameter;
  int is_special;

  int value_dyn_bins_num;
  struct object *value_cell;

  int function_dyn_bins_num;
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
  enum package_record_visibility visibility;

  char *actual_symname;
  size_t actual_symname_alloc_s;
  size_t actual_symname_used_s;

  struct object *sym;
};


enum
binding_type
  {
    LEXICAL_BINDING,
    DYNAMIC_BINDING
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
parameter_type
  {
    REQUIRED_PARAM,
    OPTIONAL_PARAM,
    REST_PARAM,
    KEYWORD_PARAM,
    AUXILIARY_VAR
  };


struct
parameter
{
  enum parameter_type type;

  struct object *name;
  struct object *key;
  struct object *typespec;

  struct object *init_form;
  struct object *supplied_p_param;

  int key_passed;
  
  struct parameter *next;
};


struct
function
{
  struct object *name;

  struct parameter *lambda_list;
  int allow_other_keys;
  int min_args;
  int max_args;

  struct binding *lex_vars;
  struct binding *lex_funcs;

  int is_special_operator;
  struct object *(*builtin_form)
    (struct object *list, struct environment *env, struct eval_outcome *outcome);

  struct object *body;
};


struct
cons_pair
{
  int filling_car;  /* when car is incomplete but already partly allocated */
  int empty_list_in_car;  /* when car is a still empty list so nothing
			     allocated yet */

  int found_dot;

  int filling_cdr;
  int empty_list_in_cdr;

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

  FILE *file;

  int is_open;
};


enum
rounding_behavior
  {
    FLOOR,
    CEILING,
    TRUNCATE,
    ROUND_TO_NEAREST
  };


enum
number_comparison
  {
    EQUAL,
    LESS_THAN,
    LESS_THAN_OR_EQUAL,
    MORE_THAN,
    MORE_THAN_OR_EQUAL
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
  };


#define TYPE_PREFIX (TYPE_QUOTE | TYPE_BACKQUOTE | TYPE_COMMA | TYPE_AT | TYPE_DOT)
#define TYPE_REAL (TYPE_INTEGER | TYPE_RATIO | TYPE_FLOAT)
#define TYPE_NUMBER (TYPE_REAL)


union
object_ptr_union
{
  struct symbol *symbol;
  struct symbol_name *symbol_name;
  struct object *next;
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
  union object_ptr_union value_ptr;
};  


struct
structure_slot
{
  struct object *name;
  struct object *initform;
  struct object *type;
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


struct
command_line_options
{
  int load_cl;

  char *load_before_repl, *load_and_exit;
};



void parse_command_line (struct command_line_options *opts, int argc,
			 char *argv []);

void add_standard_definitions (struct environment *env);

#ifndef HAVE_MEMMEM
void *al_memmem (const void *haystack, size_t haystacklen, const void *needle,
		 size_t needlelen);
#endif

char *al_readline (const char prompt []);
char *read_line_interactively (const char prompt []);

enum read_outcome read_object_continued
(struct object **obj, int backts_commas_balance, int is_empty_list,
 const char *input, size_t size, struct environment *env,
 struct eval_outcome *outcome,  const char **obj_begin, const char **obj_end,
 struct read_outcome_args *args);
struct object *complete_object_interactively
(struct object *obj, int is_empty_list, struct environment *env,
 struct eval_outcome *outcome, size_t multiline_comment_depth,
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

void prepend_object_to_obj_list (struct object *obj, struct object_list **list);
struct object_list *copy_list_to_obj_list (struct object *list);
struct object *pop_object_from_obj_list (struct object_list **list);
int is_object_in_obj_list (const struct object *obj,
			   const struct object_list *list);
void free_object_list (struct object_list *list);

enum read_outcome read_object
(struct object **obj, int backt_commas_balance, const char *input, size_t size,
 struct environment *env, struct eval_outcome *outcome, const char **obj_begin,
 const char **obj_end, struct read_outcome_args *args);

enum read_outcome read_list
(struct object **obj, int backts_commas_balance, const char *input, size_t size,
 struct environment *env, struct eval_outcome *outcome, const char **list_end,
 struct read_outcome_args *args);

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
 struct eval_outcome *e_outcome, const char **macro_end,
 struct read_outcome_args *args);
struct object *call_sharp_macro
(struct sharp_macro_call *macro_call, struct environment *env,
 struct eval_outcome *e_outcome, enum read_outcome *r_outcome);

enum element find_next_element
(const char *input, size_t size, const char **elem_begin);

int is_number (const char *token, size_t size, int radix,
	       enum object_type *numtype, const char **number_end,
	       size_t *exp_marker_pos, const char **token_end);
struct object *alloc_number (enum object_type numtype);
struct object *create_number (const char *token, size_t size,
			      size_t exp_marker_pos, int radix,
			      enum object_type numtype);
struct object *create_integer_from_int (int num);
struct object *create_floating_from_double (double d);

void print_range (const char *begin, const char *end);

char *append_newline (char *string);
char *append_zero_byte (char *string, size_t size);
char *copy_token_to_buffer (const char *input, size_t size);

size_t mbslen (const char *string);
size_t next_utf8_char (char *str, size_t sz);

void *malloc_and_check (size_t size);
void *realloc_and_check (void *ptr, size_t size);
void *calloc_and_check (size_t nmemb, size_t size);

struct object *alloc_object (void);
struct object *alloc_prefix (enum element type);
struct object *alloc_empty_cons_pair (void);
struct object *alloc_empty_list (size_t sz);
struct object *alloc_function (void);
struct object *alloc_sharp_macro_call (void);

struct object_list **alloc_empty_hash_table (size_t table_size);
struct object_list **clone_hash_table (struct object_list **hash_table,
				       size_t table_size);
void free_hash_table (struct object_list **hash_table, size_t table_size);
int hash_object (const struct object *object, size_t table_size);
int is_object_in_hash_table (const struct object *object,
			     struct object_list **hash_table,
			     size_t table_size);

void clone_lexical_environment (struct binding **lex_vars,
				struct binding **lex_funcs, struct binding *vars,
				int var_num, struct binding *funcs, int func_num);

struct object *create_function (struct object *lambda_list, struct object *body,
				struct environment *env,
				struct eval_outcome *outcome);
struct object *create_package (char *name, size_t name_len);

const char *find_end_of_string
(const char *input, size_t size, size_t *new_size, size_t *string_length);
void normalize_string (char *output, const char *input, size_t size);

struct object *alloc_string (size_t size);
void resize_string (struct object *string, size_t size);
char *copy_to_c_string (struct string *str);

struct object *alloc_symbol_name (size_t value_s, size_t actual_symname_s);
void resize_symbol_name (struct object *symname, size_t value_s,
			 size_t actual_symname_s);

const char *find_end_of_symbol_name
(const char *input, size_t size, int found_package_sep, size_t *new_size,
 const char **start_of_package_separator,
 enum package_record_visibility *sym_visibility, size_t *name_length,
 size_t *act_name_length, enum read_outcome *outcome);
void copy_symname_with_case_conversion (char *output, const char *input,
					size_t size,
					enum readtable_case read_case);

struct object *create_symbol (char *name, size_t size, int do_copy);
struct object *create_character (char *character, int do_copy);
struct object *create_character_from_utf8 (char *character, size_t size);
struct object *get_nth_character (int ind, struct object *str);
struct object *create_filename (struct object *string);
struct object *create_vector (struct object *list);

struct object *create_stream (enum stream_type type,
			      enum stream_direction direction,
			      struct string *filename,
			      struct eval_outcome *outcome);
struct object *create_stream_from_open_file (enum stream_type type,
					     enum stream_direction direction,
					     FILE *file);

struct object *load_file (const char *filename, struct environment *env,
			  struct eval_outcome *outcome);

struct object *find_package (const char *name, size_t len,
			     struct environment *env);
struct object_list *find_package_entry (struct object *symbol,
					struct object_list *symlist,
					struct object_list **prev);

struct object *intern_symbol_from_char_vector (char *name, size_t len,
					       int do_copy,
					       struct object_list **symlist);
struct object *intern_symbol_name (struct object *symname,
				   struct environment *env);
void unintern_symbol (struct object *sym);

struct binding *create_binding (struct object *sym, struct object *obj,
				enum binding_type type, int inc_refcs);
struct binding *add_binding (struct binding *bin, struct binding *env);
struct binding *chain_bindings (struct binding *bin, struct binding *env,
				int *num);
struct binding *remove_bindings (struct binding *env, int num);
struct binding *find_binding (struct symbol *sym, struct binding *bins,
			      enum binding_type type, int bin_num);

struct binding *bind_variable (struct object *sym, struct object *val,
			       struct binding *bins);

struct go_tag_frame *add_go_tag_frame (struct go_tag_frame *stack);
struct go_tag_frame *add_go_tag (struct object *tagname, struct object *tagdest,
				 struct go_tag_frame *frame);
struct go_tag_frame *remove_go_tag_frame (struct go_tag_frame *stack);
struct go_tag *find_go_tag (struct object *tagname, struct go_tag_frame *frame);

void add_builtin_type (char *name, struct environment *env,
		       int (*builtin_type)
		       (const struct object *obj, const struct object *typespec,
			struct environment *env, struct eval_outcome *outcome),
		       int is_standard, ...);
void add_builtin_form (char *name, struct environment *env,
		       struct object *(*builtin_form)
		       (struct object *list, struct environment *env,
			struct eval_outcome *outcome), enum object_type type,
		       struct object *(*builtin_accessor)
		       (struct object *list, struct object *newvalform,
			struct environment *env, struct eval_outcome *outcome),
		       int is_special_operator);

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

struct object *define_variable (char *name, struct object *value,
				struct environment *env);

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
int is_circular_list (struct object *list);
int is_proper_list (struct object *list);

struct object *copy_prefix (const struct object *begin, const struct object *end,
			    struct object **last_prefix);
struct object *copy_list_structure (struct object *list,
				    const struct object *prefix, int cell_num,
				    struct object **last_cell);

int array_rank (const struct array *array);

struct parameter *alloc_parameter (enum parameter_type type,
				   struct object *sym);
struct parameter *parse_required_parameters
(struct object *obj, struct parameter **last, struct object **next,
 struct eval_outcome *outcome);
struct parameter *parse_optional_parameters
(struct object *obj, struct parameter **last, struct object **next,
 struct eval_outcome *outcome);
struct parameter *parse_keyword_parameters
(struct object *obj, struct parameter **last, struct object **next,
 struct environment *env, struct eval_outcome *outcome);
struct parameter *parse_lambda_list (struct object *obj, struct environment *env,
				     struct eval_outcome *outcome);

struct object *evaluate_body
(struct object *body, int eval_body_twice, struct environment *env,
 struct eval_outcome *outcome);
struct object *call_function
(struct object *func, struct object *arglist, int eval_args, int eval_body_twice,
 struct environment *env, struct eval_outcome *outcome);

int check_type (const struct object *obj, const struct object *typespec,
		struct environment *env, struct eval_outcome *outcome);
int check_type_by_char_vector (const struct object *obj, char *type,
			       struct environment *env,
			       struct eval_outcome *outcome);
int type_starts_with (const struct object *typespec, const char *type,
		      struct environment *env);
int is_subtype_by_char_vector (const struct object *first, char *second,
			       struct environment *env,
			       struct eval_outcome *outcome);
int is_subtype (const struct object *first, const struct object *second,
		struct environment *env, struct eval_outcome *outcome);

struct object *evaluate_object
(struct object *obj, struct environment *env, struct eval_outcome *outcome);
struct object *apply_backquote
(struct object *form, struct object *reading_cons,
 struct object *first_or_prev_written_cons, int writing_first_cons,
 int passed_dot, struct object *prev_prefix, int backts_commas_balance,
 struct environment *env, struct eval_outcome *outcome,
 int *expanded_into_empty_list);
struct object *evaluate_list
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_through_list
(struct object *list, struct environment *env, struct eval_outcome *outcome);

int type_t (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct eval_outcome *outcome);
int type_nil (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct eval_outcome *outcome);
int type_null (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome);
int type_cons (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome);
int type_list (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome);  
int type_symbol (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome);
int type_function (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct eval_outcome *outcome);
int type_package (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct eval_outcome *outcome);
int type_number (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome);
int type_real (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome);
int type_rational (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct eval_outcome *outcome);
int type_integer (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct eval_outcome *outcome);
int type_ratio (const struct object *obj, const struct object *typespec,
		struct environment *env, struct eval_outcome *outcome);
int type_float (const struct object *obj, const struct object *typespec,
		struct environment *env, struct eval_outcome *outcome);
int type_short_float (const struct object *obj, const struct object *typespec,
		      struct environment *env, struct eval_outcome *outcome);
int type_single_float (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct eval_outcome *outcome);
int type_double_float (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct eval_outcome *outcome);
int type_long_float (const struct object *obj, const struct object *typespec,
		     struct environment *env, struct eval_outcome *outcome);
int type_character (const struct object *obj, const struct object *typespec,
		    struct environment *env, struct eval_outcome *outcome);
int type_vector (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome);
int type_array (const struct object *obj, const struct object *typespec,
		struct environment *env, struct eval_outcome *outcome);
int type_sequence (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct eval_outcome *outcome);
int type_string (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome);
int type_pathname (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct eval_outcome *outcome);
int type_stream (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome);

struct object *builtin_car
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_cdr
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_cons
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_list
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_append
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_nth
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_nthcdr
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_elt
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_aref
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_list_length
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_length
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_array_dimensions
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_last
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_write
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_write_string
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_load
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_open
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_close
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_open_stream_p
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_eq
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_not
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_concatenate
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_dotimes
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_dolist
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_mapcar
(struct object *list, struct environment *env, struct eval_outcome *outcome);

struct object *accessor_car (struct object *list, struct object *newvalform,
			     struct environment *env,
			     struct eval_outcome *outcome);
struct object *accessor_cdr (struct object *list, struct object *newvalform,
			     struct environment *env,
			     struct eval_outcome *outcome);

int compare_two_numbers (struct object *num1, struct object *num2);
struct object *compare_any_numbers (struct object *list, struct environment *env,
				    struct eval_outcome *outcome,
				    enum number_comparison comp);
int is_zero (struct object *num);
struct object *divide_two_numbers (struct object *n1, struct object *n2,
				   struct environment *env,
				   struct eval_outcome *outcome);

enum object_type highest_num_type (enum object_type t1, enum object_type t2);
struct object *copy_number (const struct object *num);
struct object *promote_number (struct object *num, enum object_type type);
struct object *apply_arithmetic_operation
(struct object *list, void (*opz) (mpz_t, const mpz_t, const mpz_t),
 void (*opq) (mpq_t, const mpq_t, const mpq_t),
 void (*opf) (mpf_t, const mpf_t, const mpf_t),  struct environment *env,
 struct eval_outcome *outcome);
struct object *perform_division_with_remainder
(struct object *args, enum rounding_behavior round_behavior,
 struct eval_outcome *outcome);

struct object *builtin_plus (struct object *list, struct environment *env,
			     struct eval_outcome *outcome);
struct object *builtin_minus (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);
struct object *builtin_multiply (struct object *list, struct environment *env,
				 struct eval_outcome *outcome);
struct object *builtin_divide (struct object *list, struct environment *env,
			       struct eval_outcome *outcome);
struct object *builtin_floor (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);
struct object *builtin_ceiling (struct object *list, struct environment *env,
				struct eval_outcome *outcome);
struct object *builtin_truncate (struct object *list, struct environment *env,
				 struct eval_outcome *outcome);
struct object *builtin_round (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);
struct object *builtin_sqrt (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);
struct object *builtin_numbers_equal (struct object *list,
				      struct environment *env,
				      struct eval_outcome *outcome);
struct object *builtin_numbers_different (struct object *list,
					  struct environment *env,
					  struct eval_outcome *outcome);
struct object *builtin_numbers_less_than (struct object *list,
					  struct environment *env,
					  struct eval_outcome *outcome);
struct object *builtin_numbers_less_than_or_equal (struct object *list,
						   struct environment *env,
						   struct eval_outcome *outcome);
struct object *builtin_numbers_more_than (struct object *list,
					  struct environment *env,
					  struct eval_outcome *outcome);
struct object *builtin_numbers_more_than_or_equal (struct object *list,
						   struct environment *env,
						   struct eval_outcome *outcome);

struct object *builtin_typep (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);

struct object *builtin_make_symbol (struct object *list, struct environment *env,
				    struct eval_outcome *outcome);
struct object *builtin_boundp (struct object *list, struct environment *env,
			       struct eval_outcome *outcome);
struct object *builtin_symbol_value (struct object *list,
				     struct environment *env,
				     struct eval_outcome *outcome);
struct object *builtin_fboundp (struct object *list, struct environment *env,
				struct eval_outcome *outcome);
struct object *builtin_symbol_function (struct object *list,
					struct environment *env,
					struct eval_outcome *outcome);
struct object *builtin_special_operator_p (struct object *list,
					   struct environment *env,
					   struct eval_outcome *outcome);

struct binding *create_binding_from_let_form
(struct object *form, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_let
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_let_star
(struct object *list, struct environment *env, struct eval_outcome *outcome);

struct binding *create_binding_from_flet_form
(struct object *form, struct environment *env, struct eval_outcome *outcome,
 enum object_type type);
struct object *evaluate_flet
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_labels
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_macrolet
(struct object *list, struct environment *env, struct eval_outcome *outcome);

struct object *get_dynamic_value (struct object *sym, struct environment *env);

struct object *get_function (struct object *sym, struct environment *env,
			     int only_functions);

struct object *set_value (struct object *sym, struct object *valueform,
			  struct environment *env, struct eval_outcome *outcome);

struct object *evaluate_quote
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_if
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_progn
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_values
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
struct object *evaluate_setq
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_setf
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_function
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_lambda
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_defstruct
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_apply
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_declare
(struct object *list, struct environment *env, struct eval_outcome *outcome);

struct object *execute_body_of_tagbody (struct object *body,
					struct environment *env,
					struct eval_outcome *outcome);
struct object *evaluate_tagbody
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_go
(struct object *list, struct environment *env, struct eval_outcome *outcome);

struct object *builtin_al_print_no_warranty
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_al_print_terms_and_conditions
(struct object *list, struct environment *env, struct eval_outcome *outcome);

int eqmem (const char *s1, size_t n1, const char *s2, size_t n2);
int symname_equals (const struct symbol_name *sym, const char *s);
int symname_is_among (const struct symbol_name *sym, ...);
int symbol_equals (const struct object *sym, const char *str,
		   struct environment *env);
int symbol_is_among (const struct object *sym, struct environment *env, ...);
int equal_strings (const struct string *s1, const struct string *s2);

void print_as_symbol (const char *sym, size_t len);
void print_symbol_name (const struct symbol_name *sym, struct environment *env);
void print_symbol (const struct symbol *sym, struct environment *env);
void print_string (const struct string *str);
void print_character (const char *character);
void print_filename (const struct filename *fn);
void print_list (const struct cons_pair *list, struct environment *env);
void print_array (const struct array *array, struct environment *env);
void print_function_or_macro (const struct object *obj, struct environment *env);
void print_object (const struct object *obj, struct environment *env);

void print_read_error (enum read_outcome err, const char *input, size_t size,
		       const char *begin, const char *end,
		       const struct read_outcome_args *args);
void print_eval_error (struct eval_outcome *err, struct environment *env);

void increment_refcount_by (struct object *obj, int count, struct object *parent);
void increment_refcount (struct object *obj,
			 struct object_list **antiloop_hash_t);
int decrement_refcount_by (struct object *obj, int count, struct object *parent);
int decrement_refcount (struct object *obj,
			struct object_list **antiloop_hash_t);

void free_object (struct object *obj);
void free_string (struct object *obj);
void free_symbol_name (struct object *obj);
void free_symbol (struct object *obj);
void free_cons_pair (struct object *obj);
void free_array_size (struct array_size *size);
void free_array (struct object *obj);
void free_integer (struct object *obj);
void free_ratio (struct object *obj);
void free_float (struct object *obj);
void free_function_or_macro (struct object *obj);

void free_list_structure (struct object *list);

void print_welcome_message (void);
void print_version (void);
void print_help (void);



struct symbol nil_symbol = {"NIL", 3, 1, 1, type_nil, NULL, NULL, NULL, 1};

struct object nil_object = {1, NULL, NULL, TYPE_SYMBOL, {&nil_symbol}};


struct symbol t_symbol = {"T", 1, 1, 1, type_t, NULL, NULL, NULL, 1};

struct object t_object = {1, NULL, NULL, TYPE_SYMBOL, {&t_symbol}};



int
main (int argc, char *argv [])
{
  int end_repl = 0;

#ifdef HAVE_LIBREADLINE
  int c;
#endif

  struct object *result, *obj;
  struct object_list *vals;
  struct environment env = {NULL};

  struct eval_outcome eval_out = {EVAL_OK};

  const char *input_left = NULL;
  size_t input_left_s = 0;

  struct command_line_options opts = {1};


  parse_command_line (&opts, argc, argv);

  add_standard_definitions (&env);

  if (!opts.load_and_exit)
    print_welcome_message ();

  if (opts.load_cl)
    {
      if (!opts.load_and_exit)
	printf ("Loading cl.lisp... ");

      result = load_file ("cl.lisp", &env, &eval_out);

      if (result && !opts.load_and_exit)
	print_object (result, &env);
      else if (!opts.load_and_exit)
	print_eval_error (&eval_out, &env);

      eval_out.type = EVAL_OK;

      if (!opts.load_and_exit)
	printf ("\n");
    }

  if (opts.load_before_repl)
    {
      if (!opts.load_and_exit)
	printf ("Loading %s...\n", opts.load_and_exit);

      result = load_file (opts.load_before_repl, &env, &eval_out);

      if (result && !opts.load_and_exit)
	print_object (result, &env);
      else if (!opts.load_and_exit)
	print_eval_error (&eval_out, &env);

      eval_out.type = EVAL_OK;

      if (!opts.load_and_exit)
	printf ("\n");
    }

  if (opts.load_and_exit)
    {
      result = load_file (opts.load_and_exit, &env, &eval_out);

      exit (0);
    }

#ifdef HAVE_LIBREADLINE
  c = read_history ("al_history");

  if (c && c != ENOENT)
    printf ("could not read line history from al_history: %s\n", strerror (c));
#endif


  while (!end_repl)
    {
      obj = read_object_interactively (&env, &eval_out, &input_left,
				       &input_left_s);
      
      while (obj && input_left && input_left_s > 0)
	{
	  result = evaluate_object (obj, &env, &eval_out);

	  if (!result)
	    print_eval_error (&eval_out, &env);
	  else if (eval_out.no_value)
	    eval_out.no_value = 0;
	  else
	    {
	      print_object (result, &env);
	      printf ("\n");

	      vals = eval_out.other_values;

	      while (vals)
		{
		  print_object (vals->obj, &env);
		  printf ("\n");
		  vals = vals->next;
		}

	      free_object_list (eval_out.other_values);

	      eval_out.other_values = NULL;
	    }

	  decrement_refcount (result, NULL);
	  decrement_refcount (obj, NULL);

	  obj = read_object_interactively_continued (input_left, input_left_s,
						     &env, &eval_out,
						     &input_left,
						     &input_left_s);
	}
    }
  
  return 0;
}


void
parse_command_line (struct command_line_options *opts, int argc, char *argv [])
{
  int i = 1, options_finished = 0, found_file_to_load_and_exit = 0,
    file_to_load_before_repl_expected = 0;
  char *s;

  while (i < argc)
    {
      if (options_finished && found_file_to_load_and_exit)
	{
	  puts ("at most one file to be load and exit can be provided\n"
		"Try 'al --help' for a summary of options");
	  exit (1);
	}
      else if (options_finished)
	{
	  opts->load_and_exit = argv [i];
	  found_file_to_load_and_exit = 1;
	}
      else if (file_to_load_before_repl_expected)
	{
	  opts->load_before_repl = argv [i];
	  file_to_load_before_repl_expected = 0;
	}
      else if (!strcmp (argv [i], "--help"))
	{
	  print_help ();
	  exit (0);
	}
      else if (!strcmp (argv [i], "--version"))
	{
	  print_version ();
	  exit (0);
	}
      else if (!strcmp (argv [i], "--dont-load-cl"))
	{
	  opts->load_cl = 0;
	}
      else if (!strcmp (argv [i], "--"))
	{
	  options_finished = 1;
	}
      else if (argv [i][0] == '-' && argv [i][1] == '-')
	{
	  printf ("unrecognized long option '%s'\n", argv [i]);
	  puts ("Try 'al --help' for a summary of options");
	  exit (1);
	}
      else if (argv [i][0] == '-')
	{
	  s = argv [i] + 1;

	  while (*s)
	    {
	      if (*s == 'h')
		{
		  print_help ();
		  exit (0);
		}
	      else if (*s == 'v')
		{
		  print_version ();
		  exit (0);
		}

	      if (file_to_load_before_repl_expected)
		{
		  puts ("option 'l' requires an argument\n"
			"Try 'al --help' for a summary of options");
		  exit (1);
		}

	      if (*s == 'q')
		{
		  opts->load_cl = 0;
		}
	      else if (*s == 'l')
		{
		  file_to_load_before_repl_expected = 1;
		}
	      else
		{
		  printf ("unrecognized short option '%c'\n", *s);
		  puts ("Try 'al --help' for a summary of options");
		  exit (1);
		}

	      s++;
	    }
	}
      else
	{
	  if (found_file_to_load_and_exit)
	    {
	      puts ("at most one file to be load and exit can be provided\n"
		    "Try 'al --help' for a summary of options");
	      exit (1);
	    }

	  opts->load_and_exit = argv [i];
	  found_file_to_load_and_exit = 1;
	}

      i++;
    }

  if (file_to_load_before_repl_expected)
    {
      puts ("option 'l' requires an argument\n"
	    "Try 'al --help' for a summary of options");
      exit (1);
    }
}


void
add_standard_definitions (struct environment *env)
{
  struct object *cluser_package;

  env->keyword_package = create_package ("KEYWORD",
					 strlen ("KEYWORD"));
  env->packages = create_binding (env->keyword_package->value_ptr.package->name,
				  env->keyword_package, DYNAMIC_BINDING, 1);

  env->current_package = create_package ("COMMON-LISP",
					 strlen ("COMMON-LISP"));
  env->packages = add_binding (create_binding
			       (env->current_package->value_ptr.package->name,
				env->current_package, DYNAMIC_BINDING, 1),
			       env->packages);
  env->packages = add_binding (create_binding
			       (create_symbol ("CL", strlen ("CL"), 1),
				env->current_package, DYNAMIC_BINDING, 1),
			       env->packages);

  cluser_package = create_package ("COMMON-LISP-USER",
				   strlen ("COMMON-LISP-USER"));
  env->packages = add_binding (create_binding
			       (cluser_package->value_ptr.package->name,
				cluser_package, DYNAMIC_BINDING, 1),
			       env->packages);
  env->packages = add_binding (create_binding
			       (create_symbol ("CL-USER", strlen ("CL-USER"), 1),
				cluser_package, DYNAMIC_BINDING, 1),
			       env->packages);

  t_symbol.value_cell = &t_object;
  t_symbol.home_package = env->current_package;

  nil_symbol.value_cell = &nil_object;
  nil_symbol.home_package = env->current_package;

  prepend_object_to_obj_list (&t_object,
			      &env->current_package->value_ptr.package->symlist);
  prepend_object_to_obj_list (&nil_object,
			      &env->current_package->value_ptr.package->symlist);

  add_builtin_form ("CAR", env, builtin_car, TYPE_FUNCTION, accessor_car, 0);
  add_builtin_form ("CDR", env, builtin_cdr, TYPE_FUNCTION, accessor_cdr, 0);
  add_builtin_form ("CONS", env, builtin_cons, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LIST", env, builtin_list, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("APPEND", env, builtin_append, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NTH", env, builtin_nth, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NTHCDR", env, builtin_nthcdr, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ELT", env, builtin_elt, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AREF", env, builtin_aref, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LIST-LENGTH", env, builtin_list_length, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("LENGTH", env, builtin_length, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ARRAY-DIMENSIONS", env, builtin_array_dimensions,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LAST", env, builtin_last, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("WRITE", env, builtin_write, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("WRITE-STRING", env, builtin_write_string, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("LOAD", env, builtin_load, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("OPEN", env, builtin_open, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CLOSE", env, builtin_close, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("OPEN-STREAM-P", env, builtin_open_stream_p,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("EQ", env, builtin_eq, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NOT", env, builtin_not, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NULL", env, builtin_not, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CONCATENATE", env, builtin_concatenate, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("DOTIMES", env, builtin_dotimes, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DOLIST", env, builtin_dolist, TYPE_MACRO, NULL, 0);
  add_builtin_form ("MAPCAR", env, builtin_mapcar, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("+", env, builtin_plus, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("-", env, builtin_minus, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("*", env, builtin_multiply, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("/", env, builtin_divide, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FLOOR", env, builtin_floor, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CEILING", env, builtin_ceiling, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("TRUNCATE", env, builtin_truncate, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ROUND", env, builtin_round, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SQRT", env, builtin_sqrt, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("=", env, builtin_numbers_equal, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("/=", env, builtin_numbers_different, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("<", env, builtin_numbers_less_than, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("<=", env, builtin_numbers_less_than_or_equal,TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form (">", env, builtin_numbers_more_than, TYPE_FUNCTION, NULL, 0);
  add_builtin_form (">=", env, builtin_numbers_more_than_or_equal, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("QUOTE", env, evaluate_quote, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LET", env, evaluate_let, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LET*", env, evaluate_let_star, TYPE_MACRO, NULL, 1);
  add_builtin_form ("FLET", env, evaluate_flet, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LABELS", env, evaluate_labels, TYPE_MACRO, NULL, 1);
  add_builtin_form ("MACROLET", env, evaluate_macrolet, TYPE_MACRO, NULL, 1);
  add_builtin_form ("IF", env, evaluate_if, TYPE_MACRO, NULL, 1);
  add_builtin_form ("PROGN", env, evaluate_progn, TYPE_MACRO, NULL, 1);
  add_builtin_form ("VALUES", env, evaluate_values, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DEFCONSTANT", env, evaluate_defconstant, TYPE_MACRO, NULL,
		    0);
  add_builtin_form ("DEFPARAMETER", env, evaluate_defparameter, TYPE_MACRO, NULL,
		    0);
  add_builtin_form ("DEFVAR", env, evaluate_defvar, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFUN", env, evaluate_defun, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFMACRO", env, evaluate_defmacro, TYPE_MACRO, NULL, 0);
  add_builtin_form ("SETQ", env, evaluate_setq, TYPE_MACRO, NULL, 1);
  add_builtin_form ("SETF", env, evaluate_setf, TYPE_MACRO, NULL, 0);
  add_builtin_form ("FUNCTION", env, evaluate_function, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LAMBDA", env, evaluate_lambda, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFSTRUCT", env, evaluate_defstruct, TYPE_MACRO, NULL, 0);
  add_builtin_form ("APPLY", env, evaluate_apply, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DECLARE", env, evaluate_declare, TYPE_MACRO, NULL, 0);
  add_builtin_form ("TAGBODY", env, evaluate_tagbody, TYPE_MACRO, NULL, 1);
  add_builtin_form ("GO", env, evaluate_go, TYPE_MACRO, NULL, 1);
  add_builtin_form ("TYPEP", env, builtin_typep, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAKE-SYMBOL", env, builtin_make_symbol, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("BOUNDP", env, builtin_boundp, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SYMBOL-VALUE", env, builtin_symbol_value, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("FBOUNDP", env, builtin_fboundp, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SYMBOL-FUNCTION", env, builtin_symbol_function,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SPECIAL-OPERATOR-P", env, builtin_special_operator_p,
		    TYPE_FUNCTION, NULL, 0);

  add_builtin_form ("AL-PRINT-NO-WARRANTY", env, builtin_al_print_no_warranty,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-PRINT-TERMS-AND-CONDITIONS", env,
		    builtin_al_print_terms_and_conditions, TYPE_FUNCTION, NULL,
		    0);

  add_builtin_type ("T", env, type_t, 1, NULL);
  add_builtin_type ("NIL", env, type_nil, 1, NULL);
  add_builtin_type ("SYMBOL", env, type_symbol, 1, NULL);
  add_builtin_type ("FUNCTION", env, type_function, 1, NULL);
  add_builtin_type ("PACKAGE", env, type_package, 1, NULL);
  add_builtin_type ("NUMBER", env, type_number, 1, NULL);
  add_builtin_type ("REAL", env, type_real, 1, "NUMBER", NULL);
  add_builtin_type ("RATIONAL", env, type_rational, 1, "REAL", NULL);
  add_builtin_type ("INTEGER", env, type_integer, 1, "RATIONAL", NULL);
  add_builtin_type ("RATIO", env, type_ratio, 1, "RATIONAL", NULL);
  add_builtin_type ("FLOAT", env, type_float, 1, NULL);
  add_builtin_type ("SHORT-FLOAT", env, type_short_float, 1, "SINGLE-FLOAT",
		    NULL);
  add_builtin_type ("SINGLE-FLOAT", env, type_single_float, 1, "FLOAT",
		    "SHORT-FLOAT", "DOUBLE-FLOAT", "LONG-FLOAT", NULL);
  add_builtin_type ("DOUBLE-FLOAT", env, type_double_float, 1, "SINGLE-FLOAT",
		    NULL);
  add_builtin_type ("LONG-FLOAT", env, type_long_float, 1, "SINGLE-FLOAT", NULL);
  add_builtin_type ("CHARACTER", env, type_character, 1, NULL);
  add_builtin_type ("SEQUENCE", env, type_sequence, 1, NULL);
  add_builtin_type ("LIST", env, type_list, 1, "SEQUENCE", NULL);
  add_builtin_type ("CONS", env, type_cons, 1, "LIST", "SEQUENCE", NULL);
  add_builtin_type ("ARRAY", env, type_array, 1, NULL);
  add_builtin_type ("VECTOR", env, type_vector, 1, "ARRAY", "SEQUENCE", NULL);
  add_builtin_type ("STRING", env, type_string, 1, "VECTOR", "ARRAY", "SEQUENCE",
		    NULL);
  add_builtin_type ("NULL", env, type_null, 1, "SYMBOL", "LIST", "SEQUENCE",
		    NULL);
  add_builtin_type ("PATHNAME", env, type_pathname, 1, NULL);
  add_builtin_type ("STREAM", env, type_stream, 1, NULL);


  define_variable ("*STANDARD-INPUT*",
		   create_stream_from_open_file (CHARACTER_STREAM, INPUT_STREAM,
						 stdin), env);
  define_variable ("*STANDARD-OUTPUT*",
		   create_stream_from_open_file (CHARACTER_STREAM, INPUT_STREAM,
						 stdout), env);
}


#ifndef HAVE_MEMMEM
void *
al_memmem (const void *haystack, size_t haystacklen, const void *needle,
	   size_t needlelen)
{
  size_t i, j;
  char *hayst = (char *)haystack, *needl = (char *)needle;

  for (i = 0; i < haystacklen; i++)
    {
      if (hayst [i] == needl [0])
	{
	  for (j = 1; j < needlelen && j + i < haystacklen; j++)
	    {
	      if (hayst [i+j] != needl [j])
		break;
	    }

	  if (j == needlelen)
	    return hayst+i;
	}
    }

  return NULL;
}
#endif


char *
al_readline (const char prompt [])
{
  int sz = 32;
  char *line = malloc_and_check (sz);
  int i = 0, c;

  printf (prompt);

  c = getchar ();

  while (c && c != '\n')
    {
      if (c == EOF)
	exit (0);

      if (i == sz)
	{
	  sz *= 2;
	  line = realloc_and_check (line, sz);
	}

      line [i++] = c;

      c = getchar ();
    }

  if (i == sz || i+1 == sz)
    line = realloc_and_check (line, sz + 2);

  line [i++] = '\n';
  line [i] = 0;

  return line;
}


char *
read_line_interactively (const char prompt [])
{
#ifdef HAVE_LIBREADLINE
  char *line = readline (prompt);
  int err;

  if (!line)
    exit (0);

  if (line && *line)
    add_history (line);

  err = append_history (1, "al_history");

  if (err == ENOENT)
    err = write_history ("al_history");

  if (err)
    printf ("could not write line history to al_history: %s\n", strerror (err));

  line = append_newline (line);

  return line;
#else
  return al_readline (prompt);
#endif
}


enum read_outcome
read_object_continued (struct object **obj, int backts_commas_balance,
		       int is_empty_list, const char *input, size_t size,
		       struct environment *env, struct eval_outcome *outcome,
		       const char **obj_begin, const char **obj_end,
		       struct read_outcome_args *args)
{
  enum read_outcome out;
  int bts, cs;
  struct object *last_pref, *ob = skip_prefix (*obj, &bts, &cs, &last_pref,
					       NULL, NULL);
  struct object *l;

  backts_commas_balance += (bts - cs);

  if (args->multiline_comment_depth)
    {
      input = jump_to_end_of_multiline_comment (input, size,
						args->multiline_comment_depth,
						&args->multiline_comment_depth);

      if (!input)
	return NO_OBJECT;

      input++;
      size = --(args->multiline_comment_depth);
      args->multiline_comment_depth = 0;
    }

  if (is_empty_list)
    {
      l = NULL;

      out = read_list (&l, backts_commas_balance, input, size, env, outcome,
		       obj_end, args);

      ob = l;
    }
  else if (!ob)
    {
      out = read_object (&ob, backts_commas_balance, input, size, env,
			 outcome, obj_begin, obj_end, args);

      if (out == NO_OBJECT && last_pref)
	out = JUST_PREFIX;
    }
  else if (ob->type == TYPE_CONS_PAIR)
    {
      out = read_list (&ob, backts_commas_balance, input, size, env, outcome,
		       obj_end, args);
    }
  else if (ob->type == TYPE_STRING)
    {
      out = read_string (&ob, input, size, obj_end);
    }
  else if (ob->type == TYPE_SYMBOL_NAME)
    {
      out = read_symbol_name (&ob, input, size, obj_end, CASE_UPCASE);

      if (out == COMPLETE_OBJECT && !intern_symbol_name (ob, env))
	{
	  args->obj = ob;
	  return PACKAGE_NOT_FOUND;
	}
    }
  else if (ob->type == TYPE_SHARP_MACRO_CALL)
    {
      out = read_sharp_macro_call (&ob, input, size, env, outcome, obj_end,
				   args);

      if (out == COMPLETE_OBJECT)
	{
	  ob = call_sharp_macro (ob->value_ptr.sharp_macro_call, env, outcome,
				 &out);

	  if (out & READ_ERROR)
	    {
	      print_read_error (out, input, size, *obj_begin, *obj_end, NULL);
	    }

	  if (!ob)
	    {
	      print_eval_error (outcome, env);

	      out = NO_OBJECT;
	    }
	}
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
			       size_t multiline_comment_depth,
			       const char **input_left, size_t *input_left_size)
{
  char *line;
  enum read_outcome read_out;
  struct read_outcome_args args;
  const char *begin, *end;
  size_t len;

  args.multiline_comment_depth = multiline_comment_depth;
  args.obj = NULL;
  
  line = read_line_interactively ("> ");
  len = strlen (line);
  
  read_out = read_object_continued (&obj, 0, is_empty_list, line, len, env,
				    outcome, &begin, &end,
				    &args);

  while (read_out & (INCOMPLETE_NONEMPTY_LIST | INCOMPLETE_STRING |
		     INCOMPLETE_SYMBOL_NAME | JUST_PREFIX
		     | INCOMPLETE_SHARP_MACRO_CALL |
		     UNFINISHED_MULTILINE_COMMENT | INCOMPLETE_EMPTY_LIST)
	 || read_out & READ_ERROR
	 || args.multiline_comment_depth)
    {
      if (read_out & READ_ERROR)
	{
	  print_read_error (read_out, line, len, begin, end, &args);
	  return NULL;
	}

      line = read_line_interactively ("> ");
      len = strlen (line);

      if (read_out & INCOMPLETE_EMPTY_LIST)
	read_out = read_object_continued (&obj, 0, 1, line, len, env, outcome,
					  &begin, &end, &args);
      else
	read_out = read_object_continued (&obj, 0, 0, line, len, env, outcome,
					  &begin, &end, &args);
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
  struct read_outcome_args args = {0};

  
  read_out = read_object (&obj, 0, input, input_size, env, outcome, &begin,
			  &end, &args);
  
  if (read_out == COMPLETE_OBJECT  && !args.multiline_comment_depth)
    {
      *input_left = end + 1;
      *input_left_size = (input + input_size) - end - 1;
      
      return obj;
    }
  else if (read_out == NO_OBJECT && !args.multiline_comment_depth)
    {
      *input_left = NULL;
      *input_left_size = 0;
      
      return NULL;
    }
  else if (read_out & READ_ERROR)
    {
      print_read_error (read_out, input, input_size, begin, end, &args);

      return NULL;
    }
  else if (read_out == INCOMPLETE_EMPTY_LIST)
    {
      return complete_object_interactively (obj, 1, env, outcome,
					    args.multiline_comment_depth,
					    input_left, input_left_size);
    }
  else
    return complete_object_interactively (obj, 0, env, outcome,
					  args.multiline_comment_depth,
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


void
prepend_object_to_obj_list (struct object *obj, struct object_list **list)
{
  struct object_list *l = malloc_and_check (sizeof (*l));

  l->obj = obj;
  l->next = *list;

  *list = l;
}


struct object_list *
copy_list_to_obj_list (struct object *list)
{
  struct object_list *ret = NULL, *curr = NULL;

  while (list != &nil_object)
    {
      if (!ret)
	ret = curr = malloc_and_check (sizeof (*ret));
      else
	curr = curr->next = malloc_and_check (sizeof (*curr));

      increment_refcount (CAR (list), NULL);
      curr->obj = CAR (list);

      list = CDR (list);
    }

  if (curr)
    curr->next = NULL;

  return ret;
}


struct object *
pop_object_from_obj_list (struct object_list **list)
{
  struct object *ret;
  struct object_list *first;

  if (!*list)
    return NULL;

  ret = (*list)->obj;

  first = (*list);

  *list = (*list)->next;

  free (first);

  return ret;
}


int
is_object_in_obj_list (const struct object *obj, const struct object_list *list)
{
  while (list)
    {
      if (obj == list->obj)
	return 1;

      list = list->next;
    }

  return 0;
}


void
free_object_list (struct object_list *list)
{
  struct object_list *next;

  while (list)
    {
      next = list->next;
      decrement_refcount (list->obj, NULL);
      free (list);
      list = next;
    }
}


enum read_outcome
read_object (struct object **obj, int backts_commas_balance, const char *input,
	     size_t size, struct environment *env, struct eval_outcome *outcome,
	     const char **obj_begin, const char **obj_end,
	     struct read_outcome_args *args)
{
  int found_prefix = 0;
  struct object *last_pref, *ob = NULL;
  enum object_type numtype;
  enum read_outcome out = NO_OBJECT;
  const char *num_end;
  size_t exp_mark_pos;

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
							  &args->
							  multiline_comment_depth)))
	    return NO_OBJECT;
	  else
	    {
	      size = args->multiline_comment_depth;
	      args->multiline_comment_depth = 0;
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
			   outcome, obj_end, args);
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
				       obj_end, args);

	  if (out == COMPLETE_OBJECT)
	    {
	      ob = call_sharp_macro (ob->value_ptr.sharp_macro_call, env, outcome,
				     &out);

	      if (out & READ_ERROR)
		{
		  print_read_error (out, input, size, *obj_begin, *obj_end, NULL);
		}

	      if (!ob)
		{
		  print_eval_error (outcome, env);

		  out = NO_OBJECT;
		}
	    }

	  break;
	}
      else
	{
	  *obj_begin = input;
	  
	  if (is_number (input, size, 10, &numtype, &num_end, &exp_mark_pos,
			 obj_end))
	    {
	      ob = create_number (input, num_end - input + 1, exp_mark_pos, 10,
				  numtype);
	      out = COMPLETE_OBJECT;
	    }
	  else
	    {
	      out = read_symbol_name (&ob, input, size, obj_end, CASE_UPCASE);

	      if (out == COMPLETE_OBJECT && !intern_symbol_name (ob, env))
		{
		  args->obj = ob;
		  return PACKAGE_NOT_FOUND;
		}
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
	   const char **list_end, struct read_outcome_args *args)
{
  struct object *last_cons = *obj, *car = NULL, *ob = *obj, *cons;
  const char *obj_beg, *obj_end = input;
  enum read_outcome out;


  if (!size)
    return INCOMPLETE_EMPTY_LIST;

  while (ob && ob->type == TYPE_CONS_PAIR)
    {
      if (ob->value_ptr.cons_pair->filling_car
	  || ob->value_ptr.cons_pair->filling_cdr)
	{
	  out = read_object_continued (ob->value_ptr.cons_pair->filling_car
				       ? &ob->value_ptr.cons_pair->car
				       : &ob->value_ptr.cons_pair->cdr,
				       backts_commas_balance, 0, input,
				       size, env, outcome, &obj_beg,
				       &obj_end, args);

	  if (out == COMPLETE_OBJECT || out == CLOSING_PARENTHESIS)
	    ob->value_ptr.cons_pair->filling_car =
	      ob->value_ptr.cons_pair->filling_cdr = 0;
	  else if (out == NO_OBJECT || out & INCOMPLETE_OBJECT
		   || out == UNFINISHED_MULTILINE_COMMENT || out & READ_ERROR)
	    return out;

	  obj_end++;
	}
      else if (ob->value_ptr.cons_pair->empty_list_in_car
	       || ob->value_ptr.cons_pair->empty_list_in_cdr)
	{
	  out = read_object_continued (ob->value_ptr.cons_pair->empty_list_in_car
				       ? &ob->value_ptr.cons_pair->car
				       : &ob->value_ptr.cons_pair->cdr,
				       backts_commas_balance, 1, input,
				       size, env, outcome, &obj_beg,
				       &obj_end, args);

	  if (out != INCOMPLETE_EMPTY_LIST)
	    ob->value_ptr.cons_pair->empty_list_in_car =
	      ob->value_ptr.cons_pair->empty_list_in_cdr = 0;

	  if (out & INCOMPLETE_OBJECT)
	    {
	      ob->value_ptr.cons_pair->empty_list_in_car
		? (ob->value_ptr.cons_pair->filling_car = 1)
		: (ob->value_ptr.cons_pair->filling_cdr = 1);
	      return out;
	    }

	  if (out == NO_OBJECT || out == INCOMPLETE_EMPTY_LIST
	      || out == UNFINISHED_MULTILINE_COMMENT || out & READ_ERROR)
	    return out;

	  obj_end++;
	}

      last_cons = ob;
      ob = ob->value_ptr.cons_pair->cdr;      
    }

  out = read_object (&car, backts_commas_balance, obj_end,
		     size - (obj_end - input), env, outcome, &obj_beg, &obj_end,
		     args);

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
	  if (last_cons->value_ptr.cons_pair->found_dot
	      && !last_cons->value_ptr.cons_pair->cdr)
	    return NO_OBJ_AFTER_DOT_IN_LIST;

	  if (!last_cons->value_ptr.cons_pair->found_dot)
	    last_cons->value_ptr.cons_pair->cdr = &nil_object;

	  *list_end = obj_end;
	  return COMPLETE_OBJECT;
	}
      else if (out == SINGLE_DOT)
	{
	  if (!last_cons)
	    return NO_OBJ_BEFORE_DOT_IN_LIST;

	  if (last_cons->value_ptr.cons_pair->found_dot)
	    return MORE_THAN_A_CONSING_DOT_NOT_ALLOWED;

	  last_cons->value_ptr.cons_pair->found_dot = 1;
	}
      else if (out & READ_ERROR)
	{
	  return out;
	}
      else if (out == COMPLETE_OBJECT || out & INCOMPLETE_OBJECT)
	{
	  if (last_cons && last_cons->value_ptr.cons_pair->found_dot
	      && last_cons->value_ptr.cons_pair->cdr
	      && !last_cons->value_ptr.cons_pair->filling_cdr)
	    return MULTIPLE_OBJS_AFTER_DOT_IN_LIST;
	  else if (last_cons && last_cons->value_ptr.cons_pair->found_dot)
	    {
	      last_cons->value_ptr.cons_pair->cdr = car;

	      if (out & INCOMPLETE_OBJECT)
		{
		  last_cons->value_ptr.cons_pair->filling_cdr = 1;

		  return INCOMPLETE_NONEMPTY_LIST;
		}
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
			 &obj_end, args);
    }

  if (out == INCOMPLETE_EMPTY_LIST)
    {
      if (last_cons && last_cons->value_ptr.cons_pair->found_dot)
	last_cons->value_ptr.cons_pair->empty_list_in_cdr = 1;
      else
	{
	  cons = alloc_empty_cons_pair ();
	  cons->value_ptr.cons_pair->empty_list_in_car = 1;

	  if (last_cons)
	    last_cons->value_ptr.cons_pair->cdr = cons;
	  else
	    *obj = cons;
	}
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
  const char *start_of_pack_sep;
  enum package_record_visibility visib;


  *symname_end = find_end_of_symbol_name
    (input, size, ob && ob->value_ptr.symbol_name->packname_present ? 1 : 0,
     &new_size, &start_of_pack_sep, &visib, &name_l, &act_name_l, &out);

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
    copy_symname_with_case_conversion (sym->actual_symname
				       + sym->actual_symname_used_s, input, size,
				       read_case);
  else if (start_of_pack_sep)
    {
      copy_symname_with_case_conversion (sym->value + sym->used_size, input,
					 start_of_pack_sep - input, read_case);
      sym->packname_present = 1;
      copy_symname_with_case_conversion (sym->actual_symname
					 + sym->actual_symname_used_s,
					 visib == EXTERNAL_VISIBILITY ?
					 start_of_pack_sep + 1
					 : start_of_pack_sep + 2, size, read_case);
    }
  else
    copy_symname_with_case_conversion (sym->value + sym->used_size, input, size,
				       read_case);

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
		       const char **macro_end, struct read_outcome_args *args)
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
				    &obj_b, macro_end, args);
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

  if (!strchr ("'\\.pP(:", call->dispatch_ch))
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
		       macro_end, args);

      if (out == INCOMPLETE_EMPTY_LIST)
	call->is_empty_list = 1;

      if (out & INCOMPLETE_OBJECT)
	return INCOMPLETE_SHARP_MACRO_CALL;

      return COMPLETE_OBJECT;
    }

  call->obj = NULL;
  out = read_object (&call->obj, 0, input+i+1, size-i-1, env, e_outcome, &obj_b,
		     macro_end, args);

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
  struct object *obj = macro_call->obj, *fun, *ret;
  struct symbol_name *s;

  if (macro_call->dispatch_ch == '\'')
    {
      if (obj->type != TYPE_SYMBOL && obj->type != TYPE_SYMBOL_NAME)
	{
	  *r_outcome = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

	  return NULL;
	}

      fun = get_function (SYMBOL (obj), env, 1);

      if (!fun)
	*r_outcome = FUNCTION_NOT_FOUND_IN_READ;

      return fun;
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
      ret = evaluate_object (obj, env, e_outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*e_outcome);

      return ret;
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
  else if (macro_call->dispatch_ch == ':')
    {
      if (!obj)
	{
	  *r_outcome = PACKAGE_MARKER_IN_SHARP_COLON;

	  return NULL;
	}

      if (obj->type != TYPE_SYMBOL_NAME)
	{
	  *r_outcome = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

	  return NULL;
	}

      if (obj->value_ptr.symbol_name->packname_present)
	{
	  *r_outcome = PACKAGE_MARKER_IN_SHARP_COLON;

	  return NULL;
	}

      unintern_symbol (obj->value_ptr.symbol_name->sym);

      return obj;
    }

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
	   const char **number_end, size_t *exp_marker_pos,
	   const char **token_end)
{
  size_t i = 0;
  
  int found_dec_point = 0, found_exp_marker = 0, found_slash = 0,
    found_dec_digit = 0, found_digit = 0, found_digit_after_slash = 0,
    found_digit_after_exp_marker = 0, found_digit_after_dec_point = 0,
    need_decimal_digit = 0;
  
  char decimal_digits [] = "0123456789";
  char digits [] = "00112233445566778899aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRs"
    "StTuUvVwWxXyYzZ";
  char exponent_markers [] = "dDeEfFlLsS";

  int digits_len = radix * 2;
  
  *numtype = TYPE_INTEGER;
  *exp_marker_pos = 0;

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
	      *exp_marker_pos = i;
	      *numtype = TYPE_FLOAT;
	    }
	}      
      else if (token [i] == '+' || token [i] == '-')
	{
	  if (i > 0 && (!found_exp_marker || (i - *exp_marker_pos > 1)))
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
alloc_number (enum object_type numtype)
{
  struct object *obj = malloc_and_check (sizeof (*obj));

  obj->type = numtype;
  obj->refcount = 1;

  if (numtype == TYPE_INTEGER)
    {
      mpz_init (obj->value_ptr.integer);
    }
  else if (numtype == TYPE_RATIO)
    {
      mpq_init (obj->value_ptr.ratio);
    }
  else if (numtype == TYPE_FLOAT)
    {
      mpf_init (obj->value_ptr.floating);
    }

  return obj;
}


struct object *
create_number (const char *token, size_t size, size_t exp_marker_pos, int radix,
	       enum object_type numtype)
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

      mpq_canonicalize (obj->value_ptr.ratio);
    }
  else if (numtype == TYPE_FLOAT)
    {
      if (exp_marker_pos > 0)
	buf [exp_marker_pos] = 'e';

      mpf_init (obj->value_ptr.floating);
      mpf_set_str (obj->value_ptr.floating, buf, radix);
    }

  free (buf);
  
  return obj;
}


struct object *
create_integer_from_int (int num)
{
  struct object *obj = malloc_and_check (sizeof (*obj));

  obj->type = TYPE_INTEGER;
  obj->refcount = 1;

  mpz_init (obj->value_ptr.integer);
  mpz_set_si (obj->value_ptr.integer, num);

  return obj;
}


struct object *
create_floating_from_double (double d)
{
  struct object *obj = malloc_and_check (sizeof (*obj));

  obj->type = TYPE_FLOAT;
  obj->refcount = 1;

  mpf_init (obj->value_ptr.floating);
  mpf_set_d (obj->value_ptr.floating, d);

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


size_t
next_utf8_char (char *str, size_t sz)
{
  size_t off;

  for (off = 1; off < sz; off++)
    {
      if ((str [off] & 0xc0) >> 6 != 2)
	return off;
    }

  return 0;
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


void *
calloc_and_check (size_t nmemb, size_t size)
{
  void *mem = calloc (nmemb, size);

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

  cons->filling_car = 0;
  cons->empty_list_in_car = 0;
  cons->found_dot = 0;
  cons->filling_cdr = 0;
  cons->empty_list_in_cdr = 0;
  cons->car = NULL;
  cons->cdr = NULL;

  obj->type = TYPE_CONS_PAIR;
  obj->refcount = 1;
  obj->value_ptr.cons_pair = cons;

  return obj;
}


struct object *
alloc_empty_list (size_t sz)
{
  struct object *ret, *cons;

  ret = cons = alloc_empty_cons_pair ();

  for (sz--; sz; sz--)
    cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

  cons->value_ptr.cons_pair->cdr = &nil_object;

  return ret;
}


struct object *
alloc_function (void)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct function *fun = malloc_and_check (sizeof (*fun));

  fun->name = NULL;
  fun->lambda_list = NULL;
  fun->allow_other_keys = 0;

  fun->is_special_operator = 0;
  fun->builtin_form = NULL;
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


struct object_list **
alloc_empty_hash_table (size_t table_size)
{
  struct object_list **ret = malloc_and_check (table_size * sizeof (*ret));
  size_t i;

  for (i = 0; i < table_size; i++)
    ret [i] = NULL;

  return ret;
}


struct object_list **
clone_hash_table (struct object_list **hash_table, size_t table_size)
{
  struct object_list **ret = malloc_and_check (table_size * sizeof (*ret));
  struct object_list *orig, *cp;
  size_t i;

  for (i = 0; i < table_size; i++)
    {
      if (hash_table [i])
	{
	  ret [i] = malloc_and_check (sizeof (*cp));

	  ret [i]->obj = hash_table [i]->obj;

	  orig = hash_table [i];
	  cp = ret [i];

	  orig = orig->next;

	  while (orig)
	    {
	      cp = cp->next = malloc_and_check (sizeof (*cp));

	      cp->obj = orig->obj;

	      orig = orig->next;
	    }

	  cp->next = NULL;
	}
      else
	ret [i] = NULL;
    }

  return ret;
}


void
free_hash_table (struct object_list **hash_table, size_t table_size)
{
  size_t i;
  struct object_list *l, *n;

  for (i = 0; i < table_size; i++)
    {
      if (hash_table [i])
	{
	  l = hash_table [i];

	  while (l)
	    {
	      n = l->next;
	      free (l);
	      l = n;
	    }
	}
    }

  free (hash_table);
}


int
hash_object (const struct object *object, size_t table_size)
{
  return (long int)object % table_size;  /* FIXME the cast works on common
					  platforms, but is not allowed by ansi.
					  we should probably change to a hash
					  function on object fields */
}


int
is_object_in_hash_table (const struct object *object,
			 struct object_list **hash_table, size_t table_size)
{
  return is_object_in_obj_list (object,
				hash_table [hash_object (object, table_size)]);
}


void
clone_lexical_environment (struct binding **lex_vars, struct binding **lex_funcs,
			   struct binding *vars, int var_num,
			   struct binding *funcs, int func_num)
{
  struct binding *bin;

  *lex_vars = NULL;
  *lex_funcs = NULL;

  while (vars && var_num)
    {
      if (vars->type == LEXICAL_BINDING)
	{
	  if (!*lex_vars)
	    *lex_vars = bin = create_binding (vars->sym, vars->obj,
					      LEXICAL_BINDING, 1);
	  else
	    bin = bin->next = create_binding (vars->sym, vars->obj,
					      LEXICAL_BINDING, 1);
	}

      vars = vars->next;

      if (var_num > 0)
	var_num--;
    }

  while (funcs && func_num)
    {
      if (funcs->type == LEXICAL_BINDING)
	{
	  if (!*lex_funcs)
	    *lex_funcs = bin = create_binding (funcs->sym, funcs->obj,
					       LEXICAL_BINDING, 1);
	  else
	    bin = bin->next = create_binding (funcs->sym, funcs->obj,
					      LEXICAL_BINDING, 1);
	}

      funcs = funcs->next;

      if (func_num > 0)
	func_num--;
    }
}


struct object *
create_function (struct object *lambda_list, struct object *body,
		 struct environment *env, struct eval_outcome *outcome)
{
  struct object *fun = alloc_function ();
  struct function *f = fun->value_ptr.function;

  f->lambda_list = parse_lambda_list (lambda_list, env, outcome);

  if (outcome->type == INVALID_LAMBDA_LIST)
    {
      free_function_or_macro (fun);

      return NULL;
    }

  clone_lexical_environment (&f->lex_vars, &f->lex_funcs, env->vars,
			     env->var_lex_bin_num, env->funcs,
			     env->func_lex_bin_num);

  increment_refcount (body, NULL);
  f->body = body;

  return fun;
}


struct object *
create_package (char *name, size_t name_len)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct package *pack = malloc_and_check (sizeof (*pack));
  struct object *s = create_symbol (name, name_len, 0);

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


char *
copy_to_c_string (struct string *str)
{
  char *ret = malloc_and_check (str->used_size + 1);

  memcpy (ret, str->value, str->used_size);

  ret [str->used_size] = 0;

  return ret;
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
copy_symname_with_case_conversion (char *output, const char *input, size_t size,
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
create_symbol (char *name, size_t size, int do_copy)
{
  struct object *obj;
  struct symbol *sym;

  obj = malloc_and_check (sizeof (*obj));
  obj->type = TYPE_SYMBOL;
  obj->refcount = 1;

  sym = malloc_and_check (sizeof (*sym));

  if (do_copy)
    {
      sym->name = malloc_and_check (size);
      memcpy (sym->name, name, size);
    }
  else
    sym->name = name;

  sym->name_len = size;
  sym->is_type = 0;
  sym->builtin_type = NULL;
  sym->typespec = NULL;
  sym->parent_types = NULL;
  sym->builtin_accessor = NULL;
  sym->is_const = 0;
  sym->is_parameter = 0;
  sym->is_special = 0;
  sym->value_dyn_bins_num = 0;
  sym->value_cell = NULL;
  sym->function_dyn_bins_num = 0;
  sym->function_cell = NULL;
  sym->home_package = NULL;

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
create_character_from_utf8 (char *character, size_t size)
{
  size_t sz;
  char *ch;

  for (sz = 1; sz < size; sz++)
    {
      if ((character [sz] & 0xc0) >> 6 != 2)
	break;
    }

  ch = malloc_and_check (sz + 1);

  strncpy (ch, character, sz);
  ch [sz] = 0;

  return create_character (ch, 0);
}


struct object *
get_nth_character (int ind, struct object *str)
{
  char *ch = str->value_ptr.string->value;
  size_t s = str->value_ptr.string->used_size, off;

  for (off = 0; ind; ind--)
    {
      off = next_utf8_char (ch, s);

      if (!off)
	return NULL;

      ch += off;
      s -= off;
    }

  return create_character_from_utf8 (ch, s);
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
  size_t i;

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
create_stream (enum stream_type type, enum stream_direction direction,
	       struct string *filename, struct eval_outcome *outcome)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct stream *str = malloc_and_check (sizeof (*str));
  char *fn = copy_to_c_string (filename);

  if (direction == INPUT_STREAM)
    str->file = fopen (fn, "r");
  else if (direction == OUTPUT_STREAM)
    str->file = fopen (fn, "w");
  else if (direction == BIDIRECTIONAL_STREAM)
    str->file = fopen (fn, "r+");

  free (fn);

  if (!str->file)
    {
      free (str);
      free (obj);
      outcome->type = COULD_NOT_OPEN_FILE;
      return NULL;
    }

  str->type = type;
  str->direction = direction;
  str->is_open = 1;

  obj->type = TYPE_STREAM;
  obj->refcount = 1;
  obj->value_ptr.stream = str;

  return obj;
}


struct object *
create_stream_from_open_file (enum stream_type type,
			      enum stream_direction direction, FILE *file)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct stream *str = malloc_and_check (sizeof (*str));

  str->type = type;
  str->direction = direction;
  str->is_open = 1;
  str->file = file;

  obj->type = TYPE_STREAM;
  obj->refcount = 1;
  obj->value_ptr.stream = str;

  return obj;
}


struct object *
load_file (const char *filename, struct environment *env,
	   struct eval_outcome *outcome)
{
  FILE *f;
  long l;
  char *buf;
  enum read_outcome out;
  struct read_outcome_args args = {0};
  const char *in, *obj_b, *obj_e;
  size_t sz;
  struct object *obj = NULL, *res;


  f = fopen (filename, "r");

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
		     &args);
  sz = l - (obj_e + 1 - buf);
  in = obj_e + 1;

  while (1)
    {
      if (args.multiline_comment_depth)
	{
	  free (buf);
	  fclose (f);

	  return &nil_object;
	}
      else if (out == COMPLETE_OBJECT)
	{
	  res = evaluate_object (obj, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (res)
	    {
	      out = read_object (&obj, 0, in, sz, env, outcome, &obj_b, &obj_e,
				 &args);
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
	  print_read_error (out, buf, l, obj_b, obj_e, &args);

	  free (buf);
	  fclose (f);

	  return &nil_object;
	}
    }
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


struct object_list *
find_package_entry (struct object *symbol, struct object_list *symlist,
		    struct object_list **prev)
{
  *prev = NULL;

  while (symlist && symlist->obj != symbol)
    {
      *prev = symlist;
      symlist = symlist->next;
    }

  return symlist;
}


struct object *
intern_symbol_from_char_vector (char *name, size_t len, int do_copy,
				struct object_list **symlist)
{
  struct object *sym;
  struct object_list *cur = *symlist, *new_sym;

  while (cur)
    {
      if (eqmem (cur->obj->value_ptr.symbol->name,
		 cur->obj->value_ptr.symbol->name_len, name, len))
	{
	  increment_refcount (cur->obj, NULL);
	  return cur->obj;
	}

      cur = cur->next;
    }

  sym = create_symbol (name, len, do_copy);

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
	  s->sym = intern_symbol_from_char_vector (s->actual_symname,
						   s->actual_symname_used_s, 1,
						   &env->keyword_package->
						   value_ptr.package->symlist);
	  s->sym->value_ptr.symbol->home_package = env->keyword_package;

	  s->sym->value_ptr.symbol->is_const = 1;
	  s->sym->value_ptr.symbol->value_cell = s->sym;

	  return s->sym;
	}

      pack = find_package (s->value, s->used_size, env);

      if (!pack)
	return NULL;
      else
	{
	  s->sym = intern_symbol_from_char_vector (s->actual_symname,
						   s->actual_symname_used_s, 1,
						   &pack->
						   value_ptr.package->symlist);
	  s->sym->value_ptr.symbol->home_package = pack;
	  return s->sym;
	}
    }

  pack = env->current_package;
  s->sym = intern_symbol_from_char_vector (s->value, s->used_size, 1,
					   &pack->value_ptr.package->symlist);
  s->sym->value_ptr.symbol->home_package = pack;

  return s->sym;
}


void
unintern_symbol (struct object *sym)
{
  struct object *s = SYMBOL (sym);
  struct object *p = s->value_ptr.symbol->home_package;
  struct object_list *prev, *entry;

  entry = find_package_entry (s, p->value_ptr.package->symlist, &prev);

  if (prev)
    prev->next = entry->next;
  else
    p->value_ptr.package->symlist = entry->next;

  free (entry);

  /*decrement_refcount (s->value_ptr.symbol->home_package, NULL);*/
  s->value_ptr.symbol->home_package = NULL;
}


struct binding *
create_binding (struct object *sym, struct object *obj, enum binding_type type,
		int inc_refcs)
{
  struct binding *bin = malloc_and_check (sizeof (*bin));

  bin->type = type;
  bin->sym = sym;
  bin->obj = obj;
  bin->next = NULL;

  if (inc_refcs)
    {
      increment_refcount (sym, NULL);
      increment_refcount (obj, NULL);
    }

  return bin;
}


struct binding *
add_binding (struct binding *bin, struct binding *env)
{
  bin->next = env;

  return bin;
}


struct binding *
chain_bindings (struct binding *bin, struct binding *env, int *num)
{
  struct binding *last = bin, *b = bin;

  if (num)
    *num = 0;

  if (!bin)
    return env;

  while (b)
    {
      last = b;

      if (num)
	(*num)++;

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

  b = env->next;

  if (env->type == DYNAMIC_BINDING)
    env->sym->value_ptr.symbol->value_dyn_bins_num--;

  decrement_refcount (env->sym, NULL);
  decrement_refcount (env->obj, NULL);

  free (env);

  if (num == 1)
    return b;
  else
    return remove_bindings (env->next, num-1);
}


struct binding *
find_binding (struct symbol *sym, struct binding *bins,
	      enum binding_type type, int bin_num)
{
  while (bins && bin_num)
    {
      if (bins->sym->value_ptr.symbol == sym && bins->type == type)
	return bins;

      bins = bins->next;

      if (bin_num)
	bin_num--;
    }

  return NULL;
}


struct binding *
bind_variable (struct object *sym, struct object *val, struct binding *bins)
{
  if (sym->value_ptr.symbol->is_parameter)
    {
      sym->value_ptr.symbol->value_dyn_bins_num++;

      return add_binding (create_binding (sym, val, DYNAMIC_BINDING, 1),
			  bins);
    }
  else
    return add_binding (create_binding (sym, val, LEXICAL_BINDING, 1), bins);
}


struct go_tag_frame *
add_go_tag_frame (struct go_tag_frame *stack)
{
  struct go_tag_frame *f = malloc_and_check (sizeof (*f));

  f->frame = NULL;
  f->next = stack;

  return f;
}


struct go_tag_frame *
add_go_tag (struct object *tagname, struct object *tagdest,
	    struct go_tag_frame *frame)
{
  struct go_tag *new = malloc_and_check (sizeof (*new));

  new->name = tagname;
  new->dest = tagdest;
  new->next = frame->frame;

  frame->frame = new;

  return frame;
}


struct go_tag_frame *
remove_go_tag_frame (struct go_tag_frame *stack)
{
  struct go_tag_frame *next = stack->next;
  struct go_tag *n, *t = stack->frame;

  while (t)
    {
      n = t->next;
      free (t);
      t = n;
    }

  free (stack);

  return next;
}


struct go_tag *
find_go_tag (struct object *tagname, struct go_tag_frame *frame)
{
  struct go_tag *t = frame->frame;

  while (t)
    {
      if (tagname->type == t->name->type)
	{
	  if ((tagname->type == TYPE_SYMBOL_NAME
	       && tagname->value_ptr.symbol_name->sym
	       == t->name->value_ptr.symbol_name->sym)
	      || (tagname->type == TYPE_INTEGER
		  && !mpz_cmp (tagname->value_ptr.integer,
			       t->name->value_ptr.integer)))
	    return t;
	}

      t = t->next;
    }

  return NULL;
}


void
add_builtin_type (char *name, struct environment *env,
		  int (*builtin_type) (const struct object *obj,
				       const struct object *typespec,
				       struct environment *env,
				       struct eval_outcome *outcome),
		  int is_standard, ...)
{
  va_list valist;
  char *s;
  struct object *sym =
    intern_symbol_from_char_vector (name, strlen (name), 1, &env->
				    current_package->value_ptr.package->symlist);
  struct object *par;

  va_start (valist, is_standard);

  sym->value_ptr.symbol->is_type = 1;
  sym->value_ptr.symbol->is_standard_type = is_standard;
  sym->value_ptr.symbol->builtin_type = builtin_type;

  increment_refcount (sym, NULL);

  while ((s = va_arg (valist, char *)))
    {
      par = intern_symbol_from_char_vector (s, strlen (s), 1, &env->
					    current_package->
					    value_ptr.package->symlist);

      prepend_object_to_obj_list (par, &sym->value_ptr.symbol->parent_types);
    }

  va_end (valist);
}


void
add_builtin_form (char *name, struct environment *env,
		  struct object *(*builtin_form)
		  (struct object *list, struct environment *env,
		   struct eval_outcome *outcome), enum object_type type,
		  struct object *(*builtin_accessor)
		  (struct object *list, struct object *newvalform,
		   struct environment *env, struct eval_outcome *outcome),
		  int is_special_operator)
{
  struct object *sym = create_symbol (name, strlen (name), 1);
  struct object *fun = alloc_function ();
  struct function *f = fun->value_ptr.function;

  fun->type = type;

  f->name = sym;
  f->is_special_operator = is_special_operator;
  f->builtin_form = builtin_form;

  sym->value_ptr.symbol->function_cell = fun;
  sym->value_ptr.symbol->builtin_accessor = builtin_accessor;

  prepend_object_to_obj_list (sym,
			      &env->current_package->value_ptr.package->symlist);
}


struct object *
define_constant (struct object *sym, struct object *form, 
		 struct environment *env, struct eval_outcome *outcome)
{
  struct object *val;

  val = evaluate_object (form, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!val)
    return NULL;

  if (SYMBOL (sym)->value_ptr.symbol->is_const)
    {
      outcome->type = CANT_REDEFINE_CONSTANT;
      return NULL;
    }

  SYMBOL (sym)->value_ptr.symbol->is_const = 1;
  SYMBOL (sym)->value_ptr.symbol->value_cell = val;

  increment_refcount (SYMBOL (sym), NULL);
  increment_refcount (SYMBOL (sym), NULL);

  return SYMBOL (sym);
}


struct object *
define_constant_by_name (char *name, size_t size, struct object *form,
			 struct environment *env, struct eval_outcome *outcome)
{
  struct object *sym =
    intern_symbol_from_char_vector (name, size, 1, &env->current_package->
				    value_ptr.package->symlist);

  return define_constant (sym, form, env, outcome);
}


struct object *
define_parameter (struct object *sym, struct object *form,
		  struct environment *env, struct eval_outcome *outcome)
{
  struct object *s;
  struct object *val = evaluate_object (form, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!val)
    return NULL;
  
  s = SYMBOL (sym);
  
  s->value_ptr.symbol->is_parameter = 1;

  increment_refcount_by (val, s->refcount - 1, NULL);
  s->value_ptr.symbol->value_cell = val;

  increment_refcount (s, NULL);
  return s;
}


struct object *
define_parameter_by_name (char *name, size_t size, struct object *form,
			  struct environment *env, struct eval_outcome *outcome)
{
  struct object *sym =
    intern_symbol_from_char_vector (name, size, 1, &env->current_package->
				    value_ptr.package->symlist);

  return define_parameter (sym, form, env, outcome);
}


struct object *
define_variable (char *name, struct object *value, struct environment *env)
{
  struct object *sym =
    intern_symbol_from_char_vector (name, strlen (name), 1, &env->
				    current_package->value_ptr.package->symlist);

  sym->value_ptr.symbol->is_parameter = 1;
  sym->value_ptr.symbol->value_cell = value;

  return value;
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
      if (list->type == TYPE_CONS_PAIR)
	list = list->value_ptr.cons_pair->cdr;
      else
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

  return l;
}


struct object *
last_cons_pair (struct object *list)
{
  struct object *prev = &nil_object;

  while (list && list != &nil_object)
    {
      prev = list;
      list = CDR (list);
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


int
is_circular_list (struct object *list)
{
  struct object_list **hash_t = alloc_empty_hash_table (1024);

  while (list != &nil_object)
    {
      if (is_object_in_hash_table (list, hash_t, 1024))
	{
	  free_hash_table (hash_t, 1024);
	  return 1;
	}

      prepend_object_to_obj_list (list, &hash_t [hash_object (list, 1024)]);

      list = CDR (list);
    }

  free_hash_table (hash_t, 1024);
  return 0;
}


int
is_proper_list (struct object *list)
{
  return !is_circular_list (list) && !is_dotted_list (list);
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
copy_list_structure (struct object *list, const struct object *prefix,
		     int cell_num, struct object **last_cell)
{
  struct object *cons, *out, *lastpref;
  int i = 1;

  out = cons = alloc_empty_cons_pair ();

  increment_refcount (CAR (list), NULL);
  out->value_ptr.cons_pair->car = CAR (list);

  list = CDR (list);

  while (list->type == TYPE_CONS_PAIR && (i < cell_num || cell_num < 0))
    {
      cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

      increment_refcount (CAR (list), NULL);

      if (prefix)
	{
	  cons->value_ptr.cons_pair->car = copy_prefix (prefix, NULL, &lastpref);
	  lastpref->value_ptr.next = CAR (list);
	}
      else
	cons->value_ptr.cons_pair->car = CAR (list);

      list = CDR (list);
      i++;
    }

  if (list != &nil_object && cell_num < 0)
    increment_refcount (list, NULL);

  if (cell_num < 0)
    cons->value_ptr.cons_pair->cdr = list;

  if (last_cell)
    *last_cell = cons;

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
  par->init_form = NULL;
  par->supplied_p_param = NULL;
  par->next = NULL;
  
  return par;
}


struct parameter *
parse_required_parameters (struct object *obj, struct parameter **last,
			   struct object **rest, struct eval_outcome *outcome)
{
  struct object *car;
  struct parameter *first = NULL;

  *last = NULL;

  while (obj && obj != &nil_object && (car = CAR (obj))
	 && car->type == TYPE_SYMBOL_NAME 
	 && !symname_is_among (car->value_ptr.symbol_name, "&OPTIONAL", "&REST",
			       "&BODY", "&KEY", "&AUX", "&ALLOW_OTHER_KEYS",
			       NULL))
    {
      increment_refcount (SYMBOL (car), NULL);

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
			   struct object **next, struct eval_outcome *outcome)
{
  struct object *car;
  struct parameter *first = NULL;

  *last = NULL;

  while (obj && obj != &nil_object && (car = CAR (obj)))
    {
      if (car->type == TYPE_SYMBOL_NAME 
	  && symname_is_among (car->value_ptr.symbol_name, "&OPTIONAL", "&REST",
			       "&BODY", "&KEYWORD", "&AUX", "&ALLOW_OTHER_KEYS",
			       NULL))
	{
	  break;
	}
      else if (car->type == TYPE_SYMBOL_NAME)
	{
	  increment_refcount (SYMBOL (car), NULL);

	  if (!first)
	    *last = first = alloc_parameter (OPTIONAL_PARAM, SYMBOL (car));
	  else
	    *last = (*last)->next =
	      alloc_parameter (OPTIONAL_PARAM, SYMBOL (car));
	}
      else if (car->type == TYPE_CONS_PAIR)
	{
	  increment_refcount (SYMBOL (CAR (car)), NULL);

	  if (!first)
	    *last = first = alloc_parameter (OPTIONAL_PARAM, SYMBOL (CAR (car)));
	  else
	    *last = (*last)->next =
	      alloc_parameter (OPTIONAL_PARAM, SYMBOL (CAR (car)));

	  if (list_length (car) == 2)
	    {
	      increment_refcount (nth (1, car), NULL);
	      (*last)->init_form = nth (1, car);
	    }

	  if (list_length (car) == 3)
	    {
	      increment_refcount (SYMBOL (nth (2, car)), NULL);
	      (*last)->supplied_p_param = SYMBOL (nth (2, car));
	    }
	}

      obj = CDR (obj);
    }

  *next = obj;

  return first;
}


struct parameter *
parse_keyword_parameters (struct object *obj, struct parameter **last,
			  struct object **next, struct environment *env,
			  struct eval_outcome *outcome)
{
  struct object *car, *caar, *key, *var;
  struct parameter *first = NULL;

  *last = NULL;

  while (obj && obj != &nil_object && (car = CAR (obj)))
    {
      if (car->type == TYPE_SYMBOL_NAME
	  && symname_is_among (car->value_ptr.symbol_name, "&OPTIONAL", "&REST",
			       "&BODY", "&KEYWORD", "&AUX", "&ALLOW_OTHER_KEYS",
			       NULL))
	{
	  break;
	}
      else if (car->type == TYPE_SYMBOL_NAME)
	{
	  var = SYMBOL (car);
	  increment_refcount (var, NULL);

	  key = intern_symbol_from_char_vector (var->value_ptr.symbol->name,
						var->value_ptr.symbol->name_len,
						1, &env->keyword_package->
						value_ptr.package->symlist);

	  if (!first)
	    *last = first = alloc_parameter (KEYWORD_PARAM, var);
	  else
	    *last = (*last)->next =
	      alloc_parameter (KEYWORD_PARAM, var);

	  (*last)->key = key;
	}
      else if (car->type == TYPE_CONS_PAIR)
	{
	  caar = CAR (car);

	  if (IS_SYMBOL (caar))
	    key = var = SYMBOL (caar);
	  else if (caar->type == TYPE_CONS_PAIR)
	    {
	      key = CAR (caar);
	      var = CAR (CDR (caar));
	    }

	  increment_refcount (key, NULL);
	  increment_refcount (var, NULL);

	  if (!first)
	    *last = first = alloc_parameter (KEYWORD_PARAM, var);
	  else
	    *last = (*last)->next =
	      alloc_parameter (KEYWORD_PARAM, var);

	  (*last)->key = key;

	  if (list_length (car) == 2)
	    {
	      increment_refcount (nth (1, car), NULL);
	      (*last)->init_form = nth (1, car);
	    }

	  if (list_length (car) == 3)
	    {
	      increment_refcount (SYMBOL (nth (2, car)), NULL);
	      (*last)->supplied_p_param = SYMBOL (nth (2, car));
	    }
	}

      obj = CDR (obj);
    }

  *next = obj;

  return first;
}


struct parameter *parse_lambda_list (struct object *obj, struct environment *env,
				     struct eval_outcome *outcome)
{
  struct parameter *first = NULL, *last = NULL;
  struct object *car;

  if (obj == &nil_object)
    {
      return NULL;
    }

  if (obj->type != TYPE_CONS_PAIR)
    {
      outcome->type = INVALID_LAMBDA_LIST;
      return NULL;
    }

  first = parse_required_parameters (obj, &last, &obj, outcome);

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && car->type == TYPE_SYMBOL_NAME
      && symname_equals (car->value_ptr.symbol_name, "&OPTIONAL"))
    {
      if (first)
	last->next =
	  parse_optional_parameters (CDR (obj), &last, &obj, outcome);
      else
	first = parse_optional_parameters (CDR (obj), &last, &obj, outcome);
    }

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && car->type == TYPE_SYMBOL_NAME
      && symname_is_among (car->value_ptr.symbol_name, "&REST", "&BODY", NULL))
    {
      increment_refcount (SYMBOL (CAR (CDR (obj))), NULL);

      if (first)
	last->next = alloc_parameter (REST_PARAM, SYMBOL (CAR (CDR (obj))));
      else
	first = alloc_parameter (REST_PARAM, SYMBOL (CAR (CDR (obj))));

      obj = CDR (CDR (obj));
    }

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && car->type == TYPE_SYMBOL_NAME
      && symname_equals (car->value_ptr.symbol_name, "&KEY"))
    {
      if (first)
	last->next =
	  parse_keyword_parameters (CDR (obj), &last, &obj, env, outcome);
      else
	first = parse_keyword_parameters (CDR (obj), &last, &obj, env, outcome);
    }

  if (obj != &nil_object)
    {
      outcome->type = INVALID_LAMBDA_LIST;
      return NULL;
    }

  return first;
}


struct object *
evaluate_body (struct object *body, int eval_body_twice, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct object *res, *res2;

  do
    {
      res = evaluate_object (CAR (body), env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!res)
	return NULL;

      body = CDR (body);

      if (eval_body_twice)
	{
	  res2 = evaluate_object (res, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  decrement_refcount (res, NULL);

	  if (!res2)
	    return NULL;

	  if (body != &nil_object)
	    decrement_refcount (res2, NULL);
	}
      else if (body != &nil_object)
	decrement_refcount (res, NULL);

    } while (body != &nil_object);

  if (eval_body_twice)
    return res2;

  return res;
}


struct object *
call_function (struct object *func, struct object *arglist, int eval_args,
	       int eval_body_twice, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct parameter *par = func->value_ptr.function->lambda_list, *findk;
  struct binding *bins = NULL, *b;
  struct object *val, *res, *ret, *args;
  int argsnum = 0, closnum, prev_lex_bin_num; /*, rest_found = 0;*/

  if (func->value_ptr.function->builtin_form)
    {
      if (eval_args)
	{
	  args = evaluate_through_list (arglist, env, outcome);

	  if (!args)
	    return NULL;
	}
      else
	args = arglist;

      ret = func->value_ptr.function->builtin_form (args, env, outcome);

      if (eval_args)
	decrement_refcount (args, NULL);

      return ret;
    }

  prev_lex_bin_num = env->var_lex_bin_num, env->var_lex_bin_num = 0;

  env->vars = chain_bindings (func->value_ptr.function->lex_vars, env->vars,
			      &closnum);
  env->var_lex_bin_num += closnum;

  while (arglist != &nil_object && par
	 && (par->type == REQUIRED_PARAM || par->type == OPTIONAL_PARAM))
    {
      if (eval_args)
	{
	  val = evaluate_object (CAR (arglist), env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!val)
	    {
	      res = NULL;
	      goto clean_lex_env;
	    }
	}
      else
	{
	  increment_refcount (CAR (arglist), NULL);
	  val = CAR (arglist);
	}

      bins = bind_variable (par->name, val, bins);

      argsnum++;

      if (bins->type == LEXICAL_BINDING)
	env->var_lex_bin_num++;

      if (par->type == OPTIONAL_PARAM && par->supplied_p_param)
	{
	  bins = bind_variable (par->supplied_p_param, &t_object, bins);

	  argsnum++;

	  if (bins->type == LEXICAL_BINDING)
	    env->var_lex_bin_num++;
	}

      par = par->next;

      arglist = CDR (arglist);
    }

  if (par && par->type == REQUIRED_PARAM)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      res = NULL;
      goto clean_lex_env;
    }

  if (arglist != &nil_object && (!par || (par && par->type != REST_PARAM
					  && par->type != KEYWORD_PARAM)))
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      res = NULL;
      goto clean_lex_env;
    }

  while (par && par->type == OPTIONAL_PARAM)
    {
      if (par->init_form)
	{
	  val = evaluate_object (par->init_form, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!val)
	    {
	      res = NULL;
	      goto clean_lex_env;
	    }

	  bins = bind_variable (par->name, val, bins);

	  argsnum++;

	  if (bins->type == LEXICAL_BINDING)
	    env->var_lex_bin_num++;
	}
      else
	{
	  bins = bind_variable (par->name, &nil_object, bins);

	  argsnum++;

	  if (bins->type == LEXICAL_BINDING)
	    env->var_lex_bin_num++;
	}

      if (par->supplied_p_param)
	{
	  bins = bind_variable (par->supplied_p_param, &nil_object, bins);

	  argsnum++;

	  if (bins->type == LEXICAL_BINDING)
	    env->var_lex_bin_num++;
	}

      par = par->next;
    }

  if (par && par->type == REST_PARAM)
    {
      /*rest_found = 1;*/

      if (eval_args)
	{
	  arglist = evaluate_through_list (arglist, env, outcome);

	  if (!arglist)
	    {
	      res = NULL;
	      goto clean_lex_env;
	    }
	}
      else
	increment_refcount (arglist, NULL);

      bins = bind_variable (par->name, arglist, bins);

      argsnum++;

      if (bins->type == LEXICAL_BINDING)
	env->var_lex_bin_num++;

      par = par->next;
    }

  if (par && par->type == KEYWORD_PARAM)
    {
      findk = par;

      while (findk && findk->type == KEYWORD_PARAM)
	{
	  findk->key_passed = 0;

	  findk = findk->next;
	}

      while (arglist != &nil_object)
	{
	  findk = par;

	  while (findk && findk->type == KEYWORD_PARAM)
	    {
	      if (findk->key == SYMBOL (CAR (arglist)))
		break;

	      findk = findk->next;
	    }

	  if (!findk || findk->type != KEYWORD_PARAM)
	    {
	      outcome->type = KEY_NOT_FOUND_IN_FUNCALL;
	      res = NULL;
	      goto clean_lex_env;
	    }

	  arglist = CDR (arglist);

	  if (arglist == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      res = NULL;
	      goto clean_lex_env;
	    }

	  bins = bind_variable (findk->name, CAR (arglist), bins);
	  findk->key_passed = 1;
	  argsnum++;

	  if (bins->type == LEXICAL_BINDING)
	    env->var_lex_bin_num++;

	  arglist = CDR (arglist);
	}

      findk = par;

      while (findk && findk->type == KEYWORD_PARAM)
	{
	  if (!findk->key_passed)
	    {
	      if (findk->init_form)
		{
		  val = evaluate_object (findk->init_form, env, outcome);
		  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

		  if (!val)
		    {
		      res = NULL;
		      goto clean_lex_env;
		    }
		}
	      else
		val = &nil_object;

	      bins = bind_variable (findk->name, CAR (arglist), bins);
	      argsnum++;

	      if (bins->type == LEXICAL_BINDING)
		env->var_lex_bin_num++;
	    }

	  findk = findk->next;
	}
    }

  env->vars = chain_bindings (bins, env->vars, NULL);

  res = evaluate_body (func->value_ptr.function->body, eval_body_twice, env,
		       outcome);

 clean_lex_env:
  env->vars = remove_bindings (env->vars, argsnum);

  for (; closnum; closnum--)
    {
      b = env->vars;

      env->vars = env->vars->next;

      if (closnum == 1)
	b->next = NULL;
    }

  env->var_lex_bin_num = prev_lex_bin_num;

  /*if (rest_found)
    decrement_refcount (arglist, NULL);*/

  return res;
}


int
check_type (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct eval_outcome *outcome)
{
  const struct object *car, *sym;

  if ((typespec->type == TYPE_CONS_PAIR
       && (car = CAR (typespec))
       && car->type & (TYPE_SYMBOL_NAME | TYPE_SYMBOL)
       && (sym = SYMBOL (car))
       && sym->value_ptr.symbol->is_type)
      || (typespec->type & (TYPE_SYMBOL_NAME | TYPE_SYMBOL)
	  && (sym = SYMBOL (typespec))
	  && sym->value_ptr.symbol->is_type))
    {
      return sym->value_ptr.symbol->builtin_type (obj, sym, env, outcome);
    }

  outcome->type = UNKNOWN_TYPE;

  return -1;
}


int
check_type_by_char_vector (const struct object *obj, char *type,
			   struct environment *env,
			   struct eval_outcome *outcome)
{
  return check_type (obj,
		     intern_symbol_from_char_vector (type, strlen (type), 1,
						     &env->current_package->
						     value_ptr.package->symlist),
		     env, outcome);
}


int
type_starts_with (const struct object *typespec, const char *type,
		  struct environment *env)
{
  if ((typespec->type == TYPE_SYMBOL || typespec->type == TYPE_SYMBOL_NAME)
      && symbol_equals (typespec, type, env))
    return 1;

  if (typespec->type == TYPE_CONS_PAIR
      && (CAR (typespec)->type == TYPE_SYMBOL_NAME
	  || CAR (typespec)->type == TYPE_SYMBOL)
      && symbol_equals (CAR (typespec), type, env))
    return 1;

  return 0;
}


int
is_subtype_by_char_vector (const struct object *first, char *second,
			   struct environment *env,
			   struct eval_outcome *outcome)
{
  return is_subtype (first,
		     intern_symbol_from_char_vector (second, strlen (second), 1,
						     &env->current_package->
						     value_ptr.package->symlist),
		     env, outcome);
}


int
is_subtype (const struct object *first, const struct object *second,
	    struct environment *env, struct eval_outcome *outcome)
{
  struct object_list *p;
  int ret;

  if (first == &nil_object || second == &t_object)
    return 1;

  if (IS_SYMBOL (second)
      && type_starts_with (first, SYMBOL (second)->value_ptr.symbol->name, env))
    return 1;

  if (IS_SYMBOL (first) && IS_SYMBOL (second))
    {
      p = SYMBOL (first)->value_ptr.symbol->parent_types;

      while (p)
	{
	  if (p->obj == SYMBOL (second))
	    return 1;

	  p = p->next;
	}

      p = SYMBOL (first)->value_ptr.symbol->parent_types;

      while (p)
	{
	  ret = is_subtype (p->obj, SYMBOL (second), env, outcome);

	  if (ret)
	    return 1;

	  p = p->next;
	}
    }

  return 0;
}


struct object *
evaluate_object (struct object *obj, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct binding *bind;
  struct object *sym, *ret;

  if (obj->type == TYPE_QUOTE)
    {
      increment_refcount (obj->value_ptr.next, NULL);
      return obj->value_ptr.next;
    }
  else if (obj->type == TYPE_BACKQUOTE)
    {
      return apply_backquote (obj->value_ptr.next, NULL, NULL, 0, 0, NULL, 1,
			      env, outcome, NULL);
    }
  else if (obj->type == TYPE_SYMBOL || obj->type == TYPE_SYMBOL_NAME)
    {
      sym = SYMBOL (obj);

      if (sym->value_ptr.symbol->is_const || sym->value_ptr.symbol->is_parameter)
	{
	  ret = get_dynamic_value (sym, env);

	  if (!ret)
	    {
	      outcome->type = UNBOUND_SYMBOL;
	      outcome->obj = sym;
	      return NULL;
	    }

	  return ret;
	}
      else
	{
	  bind = find_binding (sym->value_ptr.symbol, env->vars,
			       LEXICAL_BINDING, env->var_lex_bin_num);

	  if (bind)
	    {
	      increment_refcount (bind->obj, NULL);
	      return bind->obj;
	    }
	  else if (sym->value_ptr.symbol->value_dyn_bins_num)
	    {
	      bind = find_binding (sym->value_ptr.symbol, env->vars,
				   DYNAMIC_BINDING, -1);
	      increment_refcount (bind->obj, NULL);
	      return bind->obj;
	    }
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
      increment_refcount (obj, NULL);
      return obj;
    }
}


struct object *
apply_backquote (struct object *form, struct object *reading_cons,
		 struct object *first_or_prev_written_cons,
		 int writing_first_cons, int passed_dot,
		 struct object *prev_prefix, int backts_commas_balance,
		 struct environment *env, struct eval_outcome *outcome,
		 int *expanded_into_empty_list)
{
  struct object *writing_cons, *ret, *pref_copy, *retform, *tmp;
  int exp_to_empty_list, expanded_something;

  if (!backts_commas_balance)
    {
      ret = evaluate_object (form, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
      return ret;
    }
  else if (form->type == TYPE_BACKQUOTE)
    {
      ret = apply_backquote (form->value_ptr.next, reading_cons,
			     first_or_prev_written_cons,
			     writing_first_cons, passed_dot, form,
			     backts_commas_balance + 1, env, outcome,
			     expanded_into_empty_list);

      if (!ret)
	return NULL;

      retform = alloc_prefix (BACKQUOTE);
      retform->value_ptr.next = ret;

      return retform;
    }
  else if (form->type == TYPE_COMMA)
    {
      if (backts_commas_balance == 1)
	{
	  if (form->value_ptr.next->type == TYPE_AT
	      || form->value_ptr.next->type == TYPE_DOT)
	    {
	      if (passed_dot)
		{
		  outcome->type = CANT_SPLICE_AFTER_CONSING_DOT;

		  return NULL;
		}

	      if (!first_or_prev_written_cons)
		{
		  outcome->type = COMMA_AT_OR_DOT_NOT_ALLOWED_AT_TOP_LEVEL;

		  return NULL;
		}

	      ret = evaluate_object (form->value_ptr.next->value_ptr.next, env,
				     outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!ret)
		return NULL;

	      if (ret == &nil_object)
		{
		  *expanded_into_empty_list = 1;

		  return &nil_object;
		}

	      if (ret->type != TYPE_CONS_PAIR)
		{
		  if (writing_first_cons
		      || list_length (first_or_prev_written_cons) > 2)
		    {
		      outcome->type = SPLICING_OF_ATOM_NOT_ALLOWED_HERE;

		      return NULL;
		    }

		  first_or_prev_written_cons->value_ptr.cons_pair->cdr = ret;

		  return ret;
		}

	      pref_copy = prev_prefix
		? copy_prefix (CAR (reading_cons), prev_prefix, NULL) : NULL;

	      /*if (prev_prefix || form->value_ptr.next->type == TYPE_AT)
		{*/
	      tmp = copy_list_structure (ret, pref_copy, -1, NULL);
	      decrement_refcount (ret, NULL);
	      ret = tmp;
		  /*}*/

	      if (writing_first_cons)
		{
		  first_or_prev_written_cons->value_ptr.cons_pair->car = CAR (ret);
		  last_cons_pair (ret)->value_ptr.cons_pair->cdr
		    = CDR (first_or_prev_written_cons);
		  first_or_prev_written_cons->value_ptr.cons_pair->cdr = CDR (ret);
		}
	      else
		{
		  last_cons_pair (ret)->value_ptr.cons_pair->cdr =
		    CDR (CDR (first_or_prev_written_cons));

		  first_or_prev_written_cons->value_ptr.cons_pair->cdr = ret;
		}

	      return CAR (ret);
	    }
	  else
	    {
	      ret = evaluate_object (form->value_ptr.next, env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
	      return ret;
	    }
	}
      else
	{
	  ret = apply_backquote (form->value_ptr.next, reading_cons,
				 first_or_prev_written_cons, writing_first_cons,
				 passed_dot, form, backts_commas_balance - 1,
				 env, outcome, expanded_into_empty_list);

	  if (!ret)
	    return NULL;

	  retform = alloc_prefix (COMMA);
	  retform->value_ptr.next = ret;

	  return retform;
	}
    }
  else if (form->type == TYPE_CONS_PAIR)
    {
      retform = first_or_prev_written_cons = writing_cons
	= alloc_empty_cons_pair ();
      reading_cons = form;
      writing_first_cons = 1;
      expanded_something = 0;

      while (reading_cons->type == TYPE_CONS_PAIR)
	{
	  exp_to_empty_list = 0;

	  ret = apply_backquote (CAR (reading_cons), reading_cons,
				 first_or_prev_written_cons,
				 writing_first_cons, passed_dot, NULL,
				 backts_commas_balance, env, outcome,
				 &exp_to_empty_list);

	  if (!ret)
	    return NULL;

	  if (!exp_to_empty_list)
	    {
	      writing_cons->value_ptr.cons_pair->car = ret;
	      expanded_something = 1;
	    }

	  reading_cons = CDR (reading_cons);

	  if (reading_cons->type == TYPE_CONS_PAIR && !exp_to_empty_list)
	    {
	      writing_cons = last_cons_pair (first_or_prev_written_cons);

	      first_or_prev_written_cons = writing_cons;

	      writing_cons =
		writing_cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

	      writing_first_cons = 0;
	    }
	}

      if (reading_cons != &nil_object && writing_first_cons && exp_to_empty_list)
	{
	  outcome->type = NOTHING_EXPANDED_BEFORE_CONSING_DOT;

	  return NULL;
	}

      if (reading_cons != &nil_object)
	{
	  ret = apply_backquote (reading_cons, reading_cons,
				 first_or_prev_written_cons,
				 writing_first_cons, 1, NULL,
				 backts_commas_balance, env, outcome,
				 &exp_to_empty_list);

	  if (!ret)
	    return NULL;

	  if (!exp_to_empty_list)
	    writing_cons->value_ptr.cons_pair->cdr = ret;
	  else
	    writing_cons->value_ptr.cons_pair->cdr = &nil_object;
	}
      else if (exp_to_empty_list)
	first_or_prev_written_cons->value_ptr.cons_pair->cdr = &nil_object;
      else
	writing_cons->value_ptr.cons_pair->cdr = &nil_object;

      if (!expanded_something)
	return &nil_object;

      return retform;
    }

  increment_refcount (form, NULL);

  return form;
}


struct object *
evaluate_list (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct binding *bind;
  struct object *sym, *fun = NULL;

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


  sym = SYMBOL (CAR (list));

  if (sym->value_ptr.symbol->function_dyn_bins_num)
    {
      bind = find_binding (sym->value_ptr.symbol, env->funcs, DYNAMIC_BINDING,
			   -1);

      fun = bind->obj;
    }
  else if ((fun = sym->value_ptr.symbol->function_cell));
  else
    {
      bind = find_binding (sym->value_ptr.symbol, env->funcs, LEXICAL_BINDING,
			   env->func_lex_bin_num);

      if (bind)
	fun = bind->obj;
    }

  if (fun && fun->type == TYPE_FUNCTION)
    return call_function (fun, CDR (list), 1, 0, env, outcome);
  else if (fun)
    return call_function (fun, CDR (list), 0, 1, env, outcome);

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
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

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

  last_cons->value_ptr.cons_pair->cdr = &nil_object;

  return args;
}


int
type_t (const struct object *obj, const struct object *typespec,
	struct environment *env, struct eval_outcome *outcome)
{
  return 1;
}


int
type_nil (const struct object *obj, const struct object *typespec,
	  struct environment *env, struct eval_outcome *outcome)
{
  return 0;
}


int type_null (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome)
{
  return obj == &nil_object;
}


int
type_cons (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_CONS_PAIR;
}


int
type_list (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_CONS_PAIR || obj == &nil_object;
}


int
type_symbol (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_SYMBOL || obj->type == TYPE_SYMBOL_NAME;
}


int
type_function (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_FUNCTION;
}


int
type_package (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_PACKAGE;
}


int
type_number (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_INTEGER || obj->type == TYPE_RATIO
    || obj->type == TYPE_FLOAT;
}


int
type_real (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_INTEGER || obj->type == TYPE_RATIO
    || obj->type == TYPE_FLOAT;
}


int
type_rational (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_INTEGER || obj->type == TYPE_RATIO;
}


int
type_integer (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_INTEGER;
}


int
type_ratio (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct eval_outcome *outcome)
{
  mpz_t den;
  mpz_t one;
  int ret = 0;

  if (obj->type != TYPE_RATIO)
    return 0;

  mpz_init (den);
  mpq_get_den (den, obj->value_ptr.ratio);

  mpz_init (one);
  mpz_set_si (one, 1);

  if (mpz_cmp (den, one) > 0)
    ret = 1;

  mpz_clear (den);
  mpz_clear (one);

  return ret;
}


int
type_float (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_FLOAT;
}


int
type_short_float (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_FLOAT;
}


int
type_single_float (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_FLOAT;
}


int
type_double_float (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_FLOAT;
}


int
type_long_float (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_FLOAT;
}


int
type_character (const struct object *obj, const struct object *typespec,
		struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_CHARACTER;
}


int
type_vector (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct eval_outcome *outcome)
{
  return (obj->type == TYPE_ARRAY && array_rank (obj->value_ptr.array) == 1)
    || obj->type == TYPE_STRING;
}


int
type_array (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_ARRAY || obj->type == TYPE_STRING;
}


int
type_sequence (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_ARRAY || obj->type == TYPE_STRING
    || obj->type == TYPE_CONS_PAIR || obj == &nil_object;
}


int
type_string (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_STRING;
}


int
type_pathname (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_FILENAME;
}


int
type_stream (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_STREAM;
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

  if (CAR (list) == &nil_object)
    return &nil_object;

  if (CAR (list)->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  increment_refcount (CAR (CAR (list)), NULL);

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

  if (CAR (list) == &nil_object)
    return &nil_object;

  if (CAR (list)->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  increment_refcount (CDR (CAR (list)), NULL);

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

  increment_refcount (CAR (list), NULL);
  cons->value_ptr.cons_pair->car = CAR (list);

  increment_refcount (CAR (CDR (list)), NULL);
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

      increment_refcount (CAR (list), NULL);
      cons->value_ptr.cons_pair->car = CAR (list);

      if (!l)
	l = last_cons = cons;
      else
	last_cons = last_cons->value_ptr.cons_pair->cdr = cons;

      list = CDR (list);
    }

  return l;
}


struct object *
builtin_append (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  int length = list_length (list), i;
  struct object *obj;
  struct object *ret = &nil_object, *last = NULL;

  if (!length)
    return &nil_object;

  for (i = 0; i < length - 1; i++)
    {
      obj = nth (i, list);

      if (obj->type != TYPE_CONS_PAIR && obj != &nil_object)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}
    }

  for (i = 0; i < length - 1; i++)
    {
      obj = nth (i, list);

      if (last)
	last->value_ptr.cons_pair->cdr = copy_list_structure (obj, NULL, -1,
							      &last);
      else
	ret = copy_list_structure (obj, NULL, -1, &last);
    }

  obj = nth (length - 1, list);

  if (last)
    last->value_ptr.cons_pair->cdr = obj;
  else
    ret = obj;

  increment_refcount (obj, NULL);

  return ret;
}


struct object *
builtin_nth (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_INTEGER
      || (CAR (CDR (list))->type != TYPE_CONS_PAIR
	  && CAR (CDR (list)) != &nil_object))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = nth (mpz_get_ui (CAR (list)->value_ptr.integer), CAR (CDR (list)));

  increment_refcount (ret, NULL);

  return ret;
}


struct object *
builtin_nthcdr (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_INTEGER
      || (CAR (CDR (list))->type != TYPE_CONS_PAIR
	  && CAR (CDR (list)) != &nil_object))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = nthcdr (mpz_get_ui (CAR (list)->value_ptr.integer), CAR (CDR (list)));

  increment_refcount (ret, NULL);

  return ret;
}


struct object *
builtin_elt (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  int ind;
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (CDR (list))->type != TYPE_INTEGER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ind = mpz_get_si (CAR (CDR (list))->value_ptr.integer);

  if (ind < 0)
    {
      outcome->type = OUT_OF_BOUND_INDEX;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      ret = get_nth_character (ind, CAR (list));

      if (!ret)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      return ret;
    }
  else if (CAR (list)->type == TYPE_ARRAY)
    {
      if (array_rank (CAR (list)->value_ptr.array) != 1)
	{
	  outcome->type = WRONG_NUMBER_OF_AXIS;
	  return NULL;
	}

      if (ind >= CAR (list)->value_ptr.array->alloc_size->size)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      ret = CAR (list)->value_ptr.array->value [ind];

      increment_refcount (ret, NULL);
      return ret;
    }
  else if (CAR (list)->type == TYPE_CONS_PAIR || CAR (list) == &nil_object)
    {
      if (is_dotted_list (CAR (list)) || is_circular_list (CAR (list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (ind >= list_length (CAR (list)))
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      ret = nth (ind, CAR (list));

      increment_refcount (ret, NULL);
      return ret;
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }
}


struct object *
builtin_aref (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  struct object *arr, *ret;
  struct array_size *sz;
  int ind;

  if (!list_length (list))
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  arr = CAR (list);
  list = CDR (list);

  if (arr->type == TYPE_STRING)
    {
      if (CAR (list)->type != TYPE_INTEGER)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ind = mpz_get_si (CAR (list)->value_ptr.integer);

      if (ind < 0)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      ret = get_nth_character (ind, arr);

      if (!ret)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      return ret;
    }
  else if (arr->type == TYPE_ARRAY)
    {
      sz = arr->value_ptr.array->alloc_size;

      while (sz)
	{
	  if (list == &nil_object)
	    {
	      outcome->type = WRONG_NUMBER_OF_AXIS;
	      return NULL;
	    }

	  if (CAR (list)->type != TYPE_INTEGER)
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  ind = mpz_get_si (CAR (list)->value_ptr.integer);

	  if (ind < 0 || ind >= sz->size)
	    {
	      outcome->type = OUT_OF_BOUND_INDEX;
	      return NULL;
	    }

	  arr = arr->value_ptr.array->value [ind];

	  sz = sz->next;
	  list = CDR (list);
	}

      if (list != &nil_object)
	{
	  outcome->type = WRONG_NUMBER_OF_AXIS;
	  return NULL;
	}
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  increment_refcount (arr, NULL);
  return arr;
}


struct object *
builtin_list_length (struct object *list, struct environment *env,
		     struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CONS_PAIR && CAR (list) != &nil_object)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (is_dotted_list (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (is_circular_list (CAR (list)))
    return &nil_object;

  return create_integer_from_int (list_length (CAR (list)));
}


struct object *
builtin_length (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *seq;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  seq = CAR (list);

  if (seq->type != TYPE_STRING && seq->type != TYPE_CONS_PAIR
      && seq->type != TYPE_ARRAY && seq != &nil_object)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (seq->type == TYPE_STRING)
    {
      return create_integer_from_int (seq->value_ptr.string->used_size);
    }
  else if (seq->type == TYPE_CONS_PAIR || seq == &nil_object)
    {
      return create_integer_from_int (list_length (seq));
    }
  else
    {
      if (array_rank (seq->value_ptr.array) != 1)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (seq->value_ptr.array->fill_pointer >= 0)
	return create_integer_from_int (seq->value_ptr.array->fill_pointer);

      return create_integer_from_int (seq->value_ptr.array->alloc_size->size);
    }
}


struct object *
builtin_array_dimensions (struct object *list, struct environment *env,
			  struct eval_outcome *outcome)
{
  struct object *arr, *ret = NULL, *cons, *num;
  struct array_size *sz;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  arr = CAR (list);

  if (arr->type == TYPE_STRING)
    {
      ret = alloc_empty_cons_pair ();

      num = alloc_number (TYPE_INTEGER);

      mpz_set_ui (num->value_ptr.integer, arr->value_ptr.string->used_size);
      ret->value_ptr.cons_pair->car = num;
      ret->value_ptr.cons_pair->cdr = &nil_object;
    }
  else if (arr->type == TYPE_ARRAY)
    {
      sz = arr->value_ptr.array->alloc_size;

      while (sz)
	{
	  if (!ret)
	    ret = cons = alloc_empty_cons_pair ();
	  else
	    cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

	  num = alloc_number (TYPE_INTEGER);

	  mpz_set_ui (num->value_ptr.integer, sz->size);
	  cons->value_ptr.cons_pair->car = num;

	  sz = sz->next;
	}

      cons->value_ptr.cons_pair->cdr = &nil_object;
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return ret;
}


struct object *
builtin_last (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  int length = list_length (list);
  int n = 1;
  struct object *ret;

  if (!length || length > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if ((CAR (list)->type != TYPE_CONS_PAIR && CAR (list) != &nil_object)
      || (length == 2 && CAR (CDR (list))->type != TYPE_INTEGER))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (length == 2)
    {
      n = mpz_get_si (CAR (CDR (list))->value_ptr.integer);

      if (n < 0)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}
    }

  ret = nthcdr (list_length (CAR (list)) - n, CAR (list));

  increment_refcount (ret, NULL);
  return ret;
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

  increment_refcount (CAR (list), NULL);
  return CAR (list);
}


struct object *
builtin_write_string (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  size_t i;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  for (i = 0; i < CAR (list)->value_ptr.string->used_size; i++)
    putchar (CAR (list)->value_ptr.string->value [i]);

  increment_refcount (CAR (list), NULL);
  return CAR (list);
}


struct object *
builtin_load (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  int l = list_length (list);
  char *fn;
  struct object *ret;

  if (!l)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (l > 1)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  fn = copy_to_c_string (CAR (list)->value_ptr.string);

  ret = load_file (fn, env, outcome);

  free (fn);

  return ret;
}


struct object *
builtin_open (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  enum stream_direction dir = INPUT_STREAM;
  struct filename *f;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_FILENAME)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  f = CAR (list)->value_ptr.filename;
  list = CDR (list);

  while (list != &nil_object)
    {
      if (symbol_equals (CAR (list), ":DIRECTION", env))
	{
	  if (symbol_equals (CAR (CDR (list)), ":INPUT", env))
	    dir = INPUT_STREAM;
	  else if (symbol_equals (CAR (CDR (list)), ":OUTPUT", env))
	    dir = OUTPUT_STREAM;
	  else if (symbol_equals (CAR (CDR (list)), ":IO", env))
	    dir = BIDIRECTIONAL_STREAM;
	  else if (symbol_equals (CAR (CDR (list)), ":PROBE", env))
	    ;
	  else
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      list = CDR (list);
    }

  return create_stream (BINARY_STREAM, dir, f->value->value_ptr.string, outcome);
}


struct object *
builtin_close (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct stream *s;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  s = CAR (list)->value_ptr.stream;

  if (!s->is_open)
    return &nil_object;

  fclose (s->file);

  s->is_open = 0;

  return &t_object;
}


struct object *
builtin_open_stream_p (struct object *list, struct environment *env,
		       struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (list)->value_ptr.stream->is_open)
    return &t_object;
  else
    return &nil_object;
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

  if (CAR (list) == &nil_object)
    return &t_object;

  return &nil_object;
}


struct object *
builtin_concatenate (struct object *list, struct environment *env,
		     struct eval_outcome *outcome)
{
  int l = list_length (list), i;
  struct object *ret;

  if (!l)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (!is_subtype_by_char_vector (CAR (list), "SEQUENCE", env, outcome))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  for (i = 1; i < l; i++)
    {
      if (!check_type (nth (i, list), CAR (list), env, outcome))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}
    }

  if (symbol_equals (CAR (list), "STRING", env))
    {
      ret = alloc_string (0);

      for (i = 1; i < l; i++)
	{
	  resize_string (ret, ret->value_ptr.string->alloc_size
			 + nth (i, list)->value_ptr.string->used_size);
	  memcpy (ret->value_ptr.string->value + ret->value_ptr.string->used_size,
		  nth (i, list)->value_ptr.string->value,
		  nth(i, list)->value_ptr.string->used_size);
	  ret->value_ptr.string->used_size += nth (i, list)->
	    value_ptr.string->used_size;
	}

      return ret;
    }

  return &nil_object;
}


struct object *
builtin_dotimes (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct object *var, *count, *ret;
  int cnt, l, i;

  if (list_length (list) < 1 || CAR (list)->type != TYPE_CONS_PAIR
      || (l = list_length (CAR (list))) < 2 || l > 3
      || !IS_SYMBOL (CAR (CAR (list))))
    {
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      return NULL;
    }

  count = evaluate_object (CAR (CDR (CAR (list))), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!count)
    return NULL;

  if (count->type != TYPE_INTEGER)
    {
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      return NULL;
    }

  cnt = mpz_get_si (count->value_ptr.integer);
  var = SYMBOL (CAR (CAR (list)));

  for (i = 0; i < cnt; i++)
    {
      env->vars = bind_variable (var, create_integer_from_int (i), env->vars);

      if (env->vars->type == LEXICAL_BINDING)
	env->var_lex_bin_num++;

      evaluate_body (CDR (list), 0, env, outcome);

      env->vars = remove_bindings (env->vars, 1);

      if (env->vars->type == LEXICAL_BINDING)
	env->var_lex_bin_num--;
    }

  if (l == 3)
    {
      env->vars = bind_variable (var, create_integer_from_int (i), env->vars);

      if (env->vars->type == LEXICAL_BINDING)
	env->var_lex_bin_num++;

      ret = evaluate_object (nth (2, CAR (list)), env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      env->vars = remove_bindings (env->vars, 1);

      if (env->vars->type == LEXICAL_BINDING)
	env->var_lex_bin_num--;

      return ret;
    }

  return &nil_object;
}


struct object *
builtin_dolist (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *var, *cons, *ret;
  int l;

  if (list_length (list) < 1 || CAR (list)->type != TYPE_CONS_PAIR
      || (l = list_length (CAR (list))) < 2 || l > 3
      || !IS_SYMBOL (CAR (CAR (list))))
    {
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      return NULL;
    }

  cons = evaluate_object (CAR (CDR (CAR (list))), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!cons)
    return NULL;

  if (cons->type != TYPE_CONS_PAIR && cons != &nil_object)
    {
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      return NULL;
    }

  var = SYMBOL (CAR (CAR (list)));

  while (cons != &nil_object)
    {
      env->vars = bind_variable (var, CAR (cons), env->vars);

      if (env->vars->type == LEXICAL_BINDING)
	env->var_lex_bin_num++;

      evaluate_body (CDR (list), 0, env, outcome);

      env->vars = remove_bindings (env->vars, 1);

      if (env->vars->type == LEXICAL_BINDING)
	env->var_lex_bin_num--;

      cons = CDR (cons);
    }

  if (l == 3)
    {
      env->vars = bind_variable (var, &nil_object, env->vars);

      if (env->vars->type == LEXICAL_BINDING)
	env->var_lex_bin_num++;

      ret = evaluate_object (nth (2, CAR (list)), env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      env->vars = remove_bindings (env->vars, 1);

      if (env->vars->type == LEXICAL_BINDING)
	env->var_lex_bin_num--;

      return ret;
    }

  return &nil_object;
}


struct object *
builtin_mapcar (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  int i, l = list_length (list), finished = 0;
  struct object *cdrlist, *cdrlistcons, *args, *argscons, *ret, *retcons, *val;

  if (l < 2)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_FUNCTION)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  for (i = 1; i < l; i++)
    {
      if (!IS_LIST (nth (i, list)) || !is_proper_list (nth (i, list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (nth (i, list) == &nil_object)
	return &nil_object;
    }

  cdrlist = cdrlistcons = alloc_empty_list (l-1);

  for (i = 1; i < l; i++)
    {
      cdrlistcons->value_ptr.cons_pair->car = nth (i, list);
      cdrlistcons = CDR (cdrlistcons);
    }

  args = alloc_empty_list (l-1);
  ret = retcons = alloc_empty_cons_pair ();

  while (!finished)
    {
      argscons = args;
      cdrlistcons = cdrlist;

      for (i = 1; i < l; i++)
	{
	  argscons->value_ptr.cons_pair->car = CAR (CAR (cdrlistcons));
	  argscons = CDR (argscons);
	  cdrlistcons = CDR (cdrlistcons);
	}

      val = call_function (CAR (list), args, 0, 0, env, outcome);

      if (!val)
	{
	  free_list_structure (cdrlist);
	  free_list_structure (args);
	  return NULL;
	}

      retcons->value_ptr.cons_pair->car = val;

      cdrlistcons = cdrlist;

      for (i = 1; i < l; i++)
	{
	  cdrlistcons->value_ptr.cons_pair->car =
	    CDR (cdrlistcons->value_ptr.cons_pair->car);

	  if (CAR (cdrlistcons) == &nil_object)
	    finished = 1;

	  cdrlistcons = CDR (cdrlistcons);
	}

      if (!finished)
	retcons = retcons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
    }

  retcons->value_ptr.cons_pair->cdr = &nil_object;

  free_list_structure (cdrlist);
  free_list_structure (args);

  return ret;
}


struct object *
accessor_car (struct object *list, struct object *newvalform,
	      struct environment *env, struct eval_outcome *outcome)
{
  struct object *obj, *val;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (IS_SYMBOL (CAR (CDR (list)))
      && SYMBOL (CAR (CDR (list)))->value_ptr.symbol->is_const)
    {
      outcome->type = CANT_REDEFINE_CONSTANT;
      return NULL;
    }

  obj = evaluate_object (CAR (CDR (list)), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!obj)
    return NULL;

  if (obj->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  val = evaluate_object (newvalform, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!val)
    return NULL;

  decrement_refcount (obj->value_ptr.cons_pair->car, NULL);

  obj->value_ptr.cons_pair->car = val;

  return val;
}


struct object *
accessor_cdr (struct object *list, struct object *newvalform,
	      struct environment *env, struct eval_outcome *outcome)
{
  struct object *obj, *val;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (IS_SYMBOL (CAR (CDR (list)))
      && SYMBOL (CAR (CDR (list)))->value_ptr.symbol->is_const)
    {
      outcome->type = CANT_REDEFINE_CONSTANT;
      return NULL;
    }

  obj = evaluate_object (CAR (CDR (list)), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!obj)
    return NULL;

  if (obj->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  val = evaluate_object (newvalform, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!val)
    return NULL;

  decrement_refcount (obj->value_ptr.cons_pair->car, NULL);

  obj->value_ptr.cons_pair->cdr = val;

  return val;
}


int
compare_two_numbers (struct object *num1, struct object *num2)
{
  enum object_type tp = highest_num_type (num1->type, num2->type);
  struct object *first_p = promote_number (num1, tp);
  struct object *second_p = promote_number (num2, tp);
  int eq;

  if (tp == TYPE_INTEGER)
    eq = mpz_cmp (first_p->value_ptr.integer, second_p->value_ptr.integer);
  else if (tp == TYPE_RATIO)
    eq = mpq_cmp (first_p->value_ptr.ratio, second_p->value_ptr.ratio);
  else
    eq = mpf_cmp (first_p->value_ptr.floating, second_p->value_ptr.floating);

  decrement_refcount (first_p, NULL);
  decrement_refcount (second_p, NULL);

  return eq;
}


struct object *
compare_any_numbers (struct object *list, struct environment *env,
		     struct eval_outcome *outcome, enum number_comparison comp)
{
  int l = list_length (list), i, eq;
  struct object *first, *second;

  if (!l)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (l == 1 && CAR (list)->type & TYPE_NUMBER)
    return &t_object;
  else if (l == 1)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;

      return NULL;
    }

  first = CAR (list);
  second = CAR (CDR (list));

  for (i = 0; i + 1 < l; i++)
    {
      if (!(first->type & TYPE_NUMBER) || !(second->type & TYPE_NUMBER))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;

	  return NULL;
	}

      eq = compare_two_numbers (first, second);

      if ((comp == EQUAL && eq) || (comp == LESS_THAN && eq >= 0)
	  || (comp == LESS_THAN_OR_EQUAL && eq > 0)
	  || (comp == MORE_THAN && eq <= 0)
	  || (comp == MORE_THAN_OR_EQUAL && eq < 0))
	return &nil_object;

      first = second;
      second = nth (i+2, list);
    }

  return &t_object;
}


int
is_zero (struct object *num)
{
  return (num->type == TYPE_INTEGER && !mpz_sgn (num->value_ptr.integer))
    || (num->type == TYPE_RATIO && !mpq_sgn (num->value_ptr.ratio))
    || (num->type == TYPE_FLOAT && !mpf_sgn (num->value_ptr.floating));
}


struct object *
divide_two_numbers (struct object *n1, struct object *n2, struct environment *env,
		    struct eval_outcome *outcome)
{
  enum object_type t = highest_num_type (n1->type, n2->type);
  struct object *ret, *pn1, *pn2;

  if (is_zero (n2))
    {
      outcome->type = CANT_DIVIDE_BY_ZERO;
      return NULL;
    }

  if (t == TYPE_INTEGER
      && mpz_divisible_p (n1->value_ptr.integer, n2->value_ptr.integer))
    {
      ret = alloc_number (TYPE_INTEGER);
      mpz_divexact (ret->value_ptr.integer, n1->value_ptr.integer,
		    n2->value_ptr.integer);
    }
  else if (t == TYPE_INTEGER || t == TYPE_RATIO)
    {
      pn1 = promote_number (n1, TYPE_RATIO);
      pn2 = promote_number (n2, TYPE_RATIO);

      ret = alloc_number (TYPE_RATIO);

      mpq_div (ret->value_ptr.ratio, pn1->value_ptr.ratio, pn2->value_ptr.ratio);

      decrement_refcount (pn1, NULL);
      decrement_refcount (pn2, NULL);
    }
  else
    {
      pn1 = promote_number (n1, TYPE_FLOAT);
      pn2 = promote_number (n2, TYPE_FLOAT);

      ret = alloc_number (TYPE_FLOAT);

      mpf_div (ret->value_ptr.floating, pn1->value_ptr.floating,
	       pn2->value_ptr.floating);

      decrement_refcount (pn1, NULL);
      decrement_refcount (pn2, NULL);
    }

  return ret;
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
    {
      increment_refcount (num, NULL);
      return num;
    }

  ret = malloc_and_check (sizeof (*ret));
  ret->refcount = 1;
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
			    struct environment *env,
			    struct eval_outcome *outcome)
{
  struct object *ret, *op;

  if (!(CAR (list)->type & TYPE_NUMBER) || !(CAR (CDR (list))->type & TYPE_NUMBER))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (highest_num_type (CAR (list)->type, CAR (CDR (list))->type)
      == CAR (list)->type)
    ret = copy_number (CAR (list));
  else
    ret = promote_number (CAR (list), highest_num_type (CAR (list)->type,
							CAR (CDR (list))->type));

  list = CDR (list);

  do
    {
      if (!(CAR (list)->type & TYPE_NUMBER))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      op = promote_number (CAR (list), highest_num_type (ret->type,
							 CAR (list)->type));

      if (ret->type == TYPE_INTEGER)
	{
	  opz (ret->value_ptr.integer, ret->value_ptr.integer,
	       op->value_ptr.integer);
	}
      else if (ret->type == TYPE_RATIO)
	{
	  opq (ret->value_ptr.ratio, ret->value_ptr.ratio, op->value_ptr.ratio);
	}
      else if (ret->type == TYPE_FLOAT)
	{
	  opf (ret->value_ptr.floating, ret->value_ptr.floating,
	       op->value_ptr.floating);
	}

      decrement_refcount (op, NULL);

      list = CDR (list);

    } while (list != &nil_object);

  return ret;
}


struct object *
perform_division_with_remainder (struct object *args,
				 enum rounding_behavior round_behavior,
				 struct eval_outcome *outcome)
{
  int l = list_length (args);
  enum object_type ret_type, op_type;
  struct object *div_, *div, *num, *half, *ret, *ret2;
  mpz_t tmp;
  mpf_t q, r;

  if (!l)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (l > 2)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (!IS_NUMBER (CAR (args)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 2)
    {
      if (!IS_NUMBER (CAR (CDR (args))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      div_ = CAR (CDR (args));
    }
  else
    div_ = create_integer_from_int (1);

  ret_type = highest_num_type (CAR (args)->type, div_->type);

  if (ret_type == TYPE_INTEGER && round_behavior != ROUND_TO_NEAREST)
    op_type = TYPE_INTEGER;
  else
    op_type = TYPE_FLOAT;

  num = promote_number (CAR (args), op_type);
  div = promote_number (div_, op_type);

  if (op_type == TYPE_INTEGER)
    {
      ret = alloc_number (TYPE_INTEGER);
      mpz_init (ret->value_ptr.integer);

      ret2 = alloc_number (TYPE_INTEGER);
      mpz_init (ret2->value_ptr.integer);

      if (round_behavior == FLOOR)
	mpz_fdiv_qr (ret->value_ptr.integer, ret2->value_ptr.integer,
		     num->value_ptr.integer, div->value_ptr.integer);
      else if (round_behavior == CEILING)
	mpz_cdiv_qr (ret->value_ptr.integer, ret2->value_ptr.integer,
		     num->value_ptr.integer, div->value_ptr.integer);
      else if (round_behavior == TRUNCATE)
	mpz_tdiv_qr (ret->value_ptr.integer, ret2->value_ptr.integer,
		     num->value_ptr.integer, div->value_ptr.integer);
    }
  else
    {
      mpf_init (q);
      mpf_div (q, num->value_ptr.floating, div->value_ptr.floating);

      if (round_behavior == FLOOR)
	mpf_floor (q, q);
      else if (round_behavior == CEILING)
	mpf_ceil (q, q);
      else if (round_behavior == TRUNCATE)
	mpf_trunc (q, q);
      else if (round_behavior == ROUND_TO_NEAREST)
	{
	  half = create_floating_from_double (.5);

	  mpf_add (q, q, half->value_ptr.floating);

	  if (mpf_integer_p (q))
	    {
	      mpz_init (tmp);
	      mpz_set_f (tmp, q);

	      if (!mpz_divisible_ui_p (tmp, 2))
		{
		  mpf_sub_ui (q, q, 1);
		}

	      mpz_clear (tmp);
	    }
	  else
	    {
	      mpf_floor (q, q);
	    }

	  free_float (half);
	}

      mpf_init (r);
      mpf_mul (r, div->value_ptr.floating, q);
      mpf_sub (r, num->value_ptr.floating, r);

      ret = alloc_number (ret_type);
      ret2 = alloc_number (ret_type);

      if (ret_type == TYPE_RATIO)
	{
	  mpq_set_f (ret->value_ptr.ratio, q);
	  mpq_set_f (ret2->value_ptr.ratio, r);
	}
      else if (ret_type == TYPE_FLOAT)
	{
	  mpf_set (ret->value_ptr.floating, q);
	  mpf_set (ret2->value_ptr.floating, r);
	}

      mpf_clear (q);
      mpf_clear (r);
    }

  decrement_refcount (num, NULL);
  decrement_refcount (div, NULL);

  prepend_object_to_obj_list (ret2, &outcome->other_values);

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

  return apply_arithmetic_operation (list, mpz_add, mpq_add, mpf_add, env,
				     outcome);
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
  struct object *ret;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (!(CAR (list)->type & TYPE_NUMBER))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (list_length (list) == 1)
    {
      ret = alloc_number (TYPE_INTEGER);
      mpz_set_si (ret->value_ptr.integer, 1);

      return divide_two_numbers (ret, CAR (list), env, outcome);
    }

  ret = copy_number (CAR (list));
  list = CDR (list);

  do
    {
      if (!(CAR (list)->type & TYPE_NUMBER))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = divide_two_numbers (ret, CAR (list), env, outcome);

      if (!ret)
	return NULL;

      list = CDR (list);
    } while (list != &nil_object);

  return ret;
}


struct object *
builtin_floor (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, FLOOR, outcome);
}


struct object *
builtin_ceiling (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, CEILING, outcome);
}


struct object *
builtin_truncate (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, TRUNCATE, outcome);
}


struct object *
builtin_round (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, ROUND_TO_NEAREST, outcome);
}


struct object *
builtin_sqrt (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  struct object *num, *ret;
  mpf_t rt;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_NUMBER (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
    }

  num = promote_number (CAR (list), TYPE_FLOAT);

  mpf_init (rt);
  mpf_sqrt (rt, num->value_ptr.floating);

  ret = alloc_number (TYPE_FLOAT);
  mpf_set (ret->value_ptr.floating, rt);

  decrement_refcount (num, NULL);

  return ret;
}


struct object *
builtin_numbers_equal (struct object *list, struct environment *env,
		       struct eval_outcome *outcome)
{
  return compare_any_numbers (list, env, outcome, EQUAL);
}


struct object *
builtin_numbers_different (struct object *list, struct environment *env,
			   struct eval_outcome *outcome)
{
  int l = list_length (list), i, j;
  struct object *first, *second;

  if (l == 1 && CAR (list)->type & TYPE_NUMBER)
    return &t_object;
  else if (l == 1)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;

      return NULL;
    }

  for (i = 0; i + 1 < l; i++)
    {
      first = nth (i, list);

      if (!(first->type & TYPE_NUMBER))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;

	  return NULL;
	}

      for (j = i + 1; j < l; j++)
	{
	  second = nth (j, list);

	  if (!(second->type & TYPE_NUMBER))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;

	      return NULL;
	    }

	  if (!compare_two_numbers (first, second))
	    return &nil_object;
	}
    }

  return &t_object;
}


struct object *
builtin_numbers_less_than (struct object *list, struct environment *env,
			   struct eval_outcome *outcome)
{
  return compare_any_numbers (list, env, outcome, LESS_THAN);
}


struct object *
builtin_numbers_less_than_or_equal (struct object *list, struct environment *env,
				    struct eval_outcome *outcome)
{
  return compare_any_numbers (list, env, outcome, LESS_THAN_OR_EQUAL);
}


struct object *
builtin_numbers_more_than (struct object *list, struct environment *env,
			   struct eval_outcome *outcome)
{
  return compare_any_numbers (list, env, outcome, MORE_THAN);
}


struct object *
builtin_numbers_more_than_or_equal (struct object *list, struct environment *env,
				    struct eval_outcome *outcome)
{
  return compare_any_numbers (list, env, outcome, MORE_THAN_OR_EQUAL);
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


struct object *
builtin_make_symbol (struct object *list, struct environment *env,
		     struct eval_outcome *outcome)
{
  struct string *s;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  s = CAR (list)->value_ptr.string;

  return create_symbol (s->value, s->used_size, 1);
}


struct object *
builtin_boundp (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *s, *sym;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  sym = SYMBOL (s);

  if (sym->value_ptr.symbol->value_cell
      || sym->value_ptr.symbol->value_dyn_bins_num)
    return &t_object;

  return &nil_object;
}


struct object *
builtin_symbol_value (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  struct object *s, *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = get_dynamic_value (SYMBOL (s), env);

  if (!ret)
    {
      outcome->type = UNBOUND_SYMBOL;
      outcome->obj = s;
      return NULL;
    }

  return ret;
}


struct object *
builtin_fboundp (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct object *s, *sym;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  sym = SYMBOL (s);

  if (sym->value_ptr.symbol->function_cell
      || sym->value_ptr.symbol->function_dyn_bins_num)
    return &t_object;

  return &nil_object;
}


struct object *
builtin_symbol_function (struct object *list, struct environment *env,
			 struct eval_outcome *outcome)
{
  struct object *s, *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = get_function (SYMBOL (s), env, 0);

  if (!ret)
    {
      outcome->type = UNKNOWN_FUNCTION;
      outcome->obj = s;
      return NULL;
    }

  return ret;
}


struct object *
builtin_special_operator_p (struct object *list, struct environment *env,
			    struct eval_outcome *outcome)
{
  struct symbol *s;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  s = SYMBOL (CAR (list))->value_ptr.symbol;

  if (s->function_cell && s->function_cell->value_ptr.function->
      is_special_operator)
    return &t_object;

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
      val = &nil_object;
    }
  else if (form->type == TYPE_CONS_PAIR)
    {
      if (list_length (form) != 2
	  || (CAR (form)->type != TYPE_SYMBOL_NAME
	      && CAR (form)->type != TYPE_SYMBOL))
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
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!val)
	return NULL;
    }
  else
    {
      outcome->type = INCORRECT_SYNTAX_IN_LET;
      return NULL;
    }

  if (sym->value_ptr.symbol->is_const)
    {
      outcome->type = CANT_REBIND_CONSTANT;
      return NULL;
    }

  if (sym->value_ptr.symbol->is_parameter)
    {
      sym->value_ptr.symbol->value_dyn_bins_num++;

      return create_binding (sym, val, DYNAMIC_BINDING, 1);
    }
  else
    return create_binding (sym, val, LEXICAL_BINDING, 1);
}


struct object *
evaluate_let (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  struct object *res, *bind_forms, *body;
  int bin_num = 0, lex_bin_num = 0;
  struct binding *bins = NULL, *bin;

  if (!list_length (list) || (CAR (list)->type != TYPE_CONS_PAIR
			      && CAR (list) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_LET;
      return NULL;
    }

  bind_forms = CAR (list);
  body = CDR (list);

  while (bind_forms != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome);

      if (!bin)
	return NULL;

      bins = add_binding (bin, bins);
      bin_num++;

      if (bin->type == LEXICAL_BINDING)
	env->var_lex_bin_num++, lex_bin_num++;

      bind_forms = CDR (bind_forms);
    }

  env->vars = chain_bindings (bins, env->vars, NULL);

  res = evaluate_progn (body, env, outcome);

  env->vars = remove_bindings (env->vars, bin_num);

  env->var_lex_bin_num -= lex_bin_num;

  return res;
}


struct object *
evaluate_let_star (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *res, *bind_forms, *body;
  int bin_num = 0, lex_bin_num = 0;
  struct binding *bin;

  if (!list_length (list) || (CAR (list)->type != TYPE_CONS_PAIR
			      && CAR (list) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_LET;
      return NULL;
    }

  bind_forms = CAR (list);
  body = CDR (list);

  while (bind_forms != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome);

      if (!bin)
	return NULL;

      env->vars = add_binding (bin, env->vars);
      bin_num++;

      if (bin->type == LEXICAL_BINDING)
	env->var_lex_bin_num++, lex_bin_num++;

      bind_forms = CDR (bind_forms);
    }

  res = evaluate_progn (body, env, outcome);

  env->vars = remove_bindings (env->vars, bin_num);

  env->var_lex_bin_num -= lex_bin_num;

  return res;
}


struct binding *
create_binding_from_flet_form (struct object *form, struct environment *env,
			       struct eval_outcome *outcome,
			       enum object_type type)
{
  struct object *sym, *fun;

  if (form->type == TYPE_CONS_PAIR)
    {
      if (list_length (form) < 2 || !IS_SYMBOL (CAR (form)))
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

      fun = create_function (CAR (CDR (form)), CDR (CDR (form)), env, outcome);

      if (!fun)
	return NULL;

      fun->type = type;
    }
  else
    {
      outcome->type = INCORRECT_SYNTAX_IN_FLET;
      return NULL;
    }

  return create_binding (sym, fun, LEXICAL_BINDING, 1);
}


struct object *
evaluate_flet (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct object *res, *bind_forms, *body;
  int bin_num = 0, lex_bin_num = 0;
  struct binding *bins = NULL, *bin;

  if (!list_length (list) || (CAR (list)->type != TYPE_CONS_PAIR
			      && CAR (list) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_FLET;
      return NULL;
    }

  bind_forms = CAR (list);
  body = CDR (list);

  while (bind_forms != &nil_object)
    {
      bin = create_binding_from_flet_form (CAR (bind_forms), env, outcome,
					   TYPE_FUNCTION);

      if (!bin)
	return NULL;

      bins = add_binding (bin, bins);
      bin_num++;

      if (bin->type == LEXICAL_BINDING)
	env->func_lex_bin_num++, lex_bin_num++;

      bind_forms = CDR (bind_forms);
    }

  env->funcs = chain_bindings (bins, env->funcs, NULL);

  res = evaluate_progn (body, env, outcome);

  env->funcs = remove_bindings (env->funcs, bin_num);

  env->func_lex_bin_num -= lex_bin_num;

  return res;
}


struct object *
evaluate_labels (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct object *res, *bind_forms, *body;
  int bin_num = 0, lex_bin_num = 0;
  struct binding *bin;

  if (!list_length (list) || (CAR (list)->type != TYPE_CONS_PAIR
			      && CAR (list) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_FLET;
      return NULL;
    }

  bind_forms = CAR (list);
  body = CDR (list);

  while (bind_forms != &nil_object)
    {
      bin = create_binding_from_flet_form (CAR (bind_forms), env, outcome,
					   TYPE_FUNCTION);

      if (!bin)
	return NULL;

      env->funcs = add_binding (bin, env->funcs);
      bin_num++;

      if (bin->type == LEXICAL_BINDING)
	env->func_lex_bin_num++, lex_bin_num++;

      bind_forms = CDR (bind_forms);
    }

  res = evaluate_progn (body, env, outcome);

  env->funcs = remove_bindings (env->funcs, bin_num);

  env->func_lex_bin_num -= lex_bin_num;

  return res;
}


struct object *
evaluate_macrolet (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *res, *bind_forms, *body;
  int bin_num = 0, lex_bin_num = 0;
  struct binding *bins = NULL, *bin;

  if (!list_length (list) || (CAR (list)->type != TYPE_CONS_PAIR
			      && CAR (list) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_FLET;
      return NULL;
    }

  bind_forms = CAR (list);
  body = CDR (list);

  while (bind_forms != &nil_object)
    {
      bin = create_binding_from_flet_form (CAR (bind_forms), env, outcome,
					   TYPE_MACRO);

      if (!bin)
	return NULL;

      bins = add_binding (bin, bins);
      bin_num++;

      if (bin->type == LEXICAL_BINDING)
	env->func_lex_bin_num++, lex_bin_num++;

      bind_forms = CDR (bind_forms);
    }

  env->funcs = chain_bindings (bins, env->funcs, NULL);

  res = evaluate_progn (body, env, outcome);

  env->funcs = remove_bindings (env->funcs, bin_num);

  env->func_lex_bin_num -= lex_bin_num;

  return res;
}


struct object *
get_dynamic_value (struct object *sym, struct environment *env)
{
  struct symbol *s = sym->value_ptr.symbol;
  struct binding *b;

  if (!s->value_dyn_bins_num && !s->value_cell)
    return NULL;

  if (s->is_const || !s->value_dyn_bins_num)
    {
      increment_refcount (s->value_cell, NULL);

      return s->value_cell;
    }

  b = find_binding (s, env->vars, DYNAMIC_BINDING, -1);

  if (b)
    {
      increment_refcount (b->obj, NULL);

      return b->obj;
    }

  return NULL;
}


struct object *
get_function (struct object *sym, struct environment *env, int only_functions)
{
  struct object *f;
  struct binding *bind = find_binding (SYMBOL (sym)->value_ptr.symbol, env->funcs,
				       DYNAMIC_BINDING, -1);

  if (!bind && !SYMBOL (sym)->value_ptr.symbol->function_cell)
    return NULL;

  if (bind)
    f = bind->obj;
  else
    f = SYMBOL (sym)->value_ptr.symbol->function_cell;

  if (f->type != TYPE_FUNCTION && only_functions)
    return NULL;

  increment_refcount (f, NULL);
  return f;
}


struct object *
set_value (struct object *sym, struct object *valueform, struct environment *env,
	   struct eval_outcome *outcome)
{
  struct symbol *s = sym->value_ptr.symbol;
  struct object *val;
  struct binding *b;

  if (s->is_const)
    {
      outcome->type = CANT_REDEFINE_CONSTANT;
      return NULL;
    }

  val = evaluate_object (valueform, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!val)
    return NULL;

  if (s->is_parameter || s->is_special)
    {
      if (!s->value_dyn_bins_num)
	{
	  if (s->value_cell)
	    decrement_refcount_by (s->value_cell,
				   sym->refcount, NULL);

	  increment_refcount_by (val, sym->refcount - 1, NULL);
	  s->value_cell = val;
	}
      else
	{
	  b = find_binding (s, env->vars, DYNAMIC_BINDING, -1);

	  if (b)
	    {
	      b->obj = val;
	    }
	  else
	    {
	      env->vars = add_binding (create_binding (sym, val,
						       DYNAMIC_BINDING, 1),
				       env->vars);
	    }
	}
    }
  else
    {
      b = find_binding (s, env->vars, LEXICAL_BINDING, env->var_lex_bin_num);

      if (b)
	{
	  b->obj = val;
	}
      else
	{
	  s->value_dyn_bins_num++;
	  s->is_special = 1;
	  increment_refcount (sym, NULL);
	  env->vars = add_binding (create_binding (sym, val, DYNAMIC_BINDING, 0),
				   env->vars);
	}
    }

  return val;
}


struct object *
evaluate_quote (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  increment_refcount (CAR (list), NULL);
  return CAR (list);
}


struct object *
evaluate_if (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  struct object *if_clause, *ret;

  if (!list)
    {
      outcome->type = MALFORMED_IF;
      return NULL;
    }

  if_clause = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!if_clause)
    return NULL;

  if (if_clause != &nil_object)
    {
      if (CDR (list) == &nil_object)
	{
	  outcome->type = MALFORMED_IF;
	  return NULL;
	}

      ret = evaluate_object (CAR (CDR (list)), env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
      return ret;
    }
  else
    {
      if (!nth (2, list))
	{
	  return &nil_object;
	}
      else
	{
	  ret = evaluate_object (nth (2, list), env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
	  return ret;
	}
    }
}


struct object *
evaluate_progn (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  return evaluate_body (list, 0, env, outcome);
}


struct object *
evaluate_values (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  if (list == &nil_object)
    {
      outcome->no_value = 1;
      return &nil_object;
    }

  outcome->other_values = copy_list_to_obj_list (CDR (list));

  increment_refcount (CAR (list), NULL);
  return CAR (list);
}


struct object *
evaluate_defconstant (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  return define_constant (CAR (list)->value_ptr.symbol_name->sym,
			  CAR (CDR (list)), env, outcome);
}


struct object *
evaluate_defparameter (struct object *list, struct environment *env,
		       struct eval_outcome *outcome)
{
  struct object *s;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL && s->type != TYPE_SYMBOL_NAME)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  s = SYMBOL (s);

  if (s->value_ptr.symbol->is_const)
    {
      outcome->type = CANT_REBIND_CONSTANT;
      return NULL;
    }

  return define_parameter (CAR (list), CAR (CDR (list)), env, outcome);
}


struct object *
evaluate_defvar (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct object *s = CAR (list);
  unsigned int l = list_length (list);

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }
  
  s = SYMBOL (s);

  if (s->value_ptr.symbol->is_const)
    {
      outcome->type = CANT_REBIND_CONSTANT;
      return NULL;
    }

  if (l == 1)
    {
      s->value_ptr.symbol->is_parameter = 1;
    }
  else if (l == 2)
    {
      s->value_ptr.symbol->is_parameter = 1;
      
      if (!s->value_ptr.symbol->value_cell)
	return define_parameter (CAR (list), CAR (CDR (list)), env, outcome);
    }
  else
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  increment_refcount (CAR (list), NULL);
  return CAR (list);
}


struct object *
evaluate_defun (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *fun, *sym;

  if (list_length (list) < 2 || CAR (list)->type != TYPE_SYMBOL_NAME
      || (CAR (CDR (list))->type != TYPE_CONS_PAIR
	  && CAR (CDR (list)) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_DEFUN;
      return NULL;
    }

  sym = SYMBOL (CAR (list));

  if (sym->value_ptr.symbol->function_cell
      && sym->value_ptr.symbol->function_cell->type == TYPE_MACRO
      && sym->value_ptr.symbol->function_cell->value_ptr.macro->is_special_operator)
    {
      outcome->type = CANT_REDEFINE_SPECIAL_OPERATOR;
      return NULL;
    }

  fun = create_function (CAR (CDR (list)), CDR (CDR (list)), env, outcome);

  if (!fun)
    return NULL;

  if (sym->value_ptr.symbol->function_cell)
    {
      decrement_refcount_by (sym->value_ptr.symbol->function_cell, sym->refcount,
			     sym);
    }
  else
    increment_refcount (sym, NULL);

  fun->value_ptr.function->name = sym;

  sym->value_ptr.symbol->function_cell = fun;
  increment_refcount_by (fun, sym->refcount - 1, sym);

  increment_refcount (sym, NULL);
  return sym;
}


struct object *
evaluate_defmacro (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *mac, *sym;

  if (list_length (list) < 2 || CAR (list)->type != TYPE_SYMBOL_NAME
      || (CAR (CDR (list))->type != TYPE_CONS_PAIR
	  && CAR (CDR (list)) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_DEFMACRO;
      return NULL;
    }

  sym = SYMBOL (CAR (list));

  if (sym->value_ptr.symbol->function_cell
      && sym->value_ptr.symbol->function_cell->type == TYPE_MACRO
      && sym->value_ptr.symbol->function_cell->value_ptr.macro->is_special_operator)
    {
      outcome->type = CANT_REDEFINE_SPECIAL_OPERATOR;
      return NULL;
    }

  mac = create_function (CAR (CDR (list)), CDR (CDR (list)), env, outcome);

  if (!mac)
    return NULL;

  mac->type = TYPE_MACRO;

  if (sym->value_ptr.symbol->function_cell)
    {
      decrement_refcount_by (sym->value_ptr.symbol->function_cell, sym->refcount,
			     sym);
    }
  else
    increment_refcount (sym, NULL);

  mac->value_ptr.function->name = sym;

  sym->value_ptr.symbol->function_cell = mac;
  increment_refcount_by (mac, sym->refcount - 1, sym);

  increment_refcount (sym, NULL);
  return sym;
}


struct object *
evaluate_setq (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct object *ret = &nil_object;

  if (list_length (list) % 2)
    {
      outcome->type = ODD_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  while (list != &nil_object)
    {
      if (!IS_SYMBOL (CAR (list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = set_value (SYMBOL (CAR (list)), CAR (CDR (list)), env, outcome);

      if (!ret)
	return NULL;

      list = CDR (CDR (list));
    }

  increment_refcount (ret, NULL);
  return ret;
}


struct object *
evaluate_setf (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct object *val = &nil_object;

  if (list_length (list) % 2)
    {
      outcome->type = ODD_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  while (list != &nil_object)
    {
      if (IS_SYMBOL (CAR (list)))
	{
	  val = set_value (SYMBOL (CAR (list)), nth (1, list), env, outcome);

	  if (!val)
	    return NULL;
	}
      else if (CAR (list)->type == TYPE_CONS_PAIR)
	{
	  if (!IS_SYMBOL (CAR (CAR (list)))
	      || !SYMBOL (CAR (CAR (list)))->value_ptr.symbol->builtin_accessor)
	    {
	      outcome->type = INVALID_ACCESSOR;
	      return NULL;
	    }

	  val = SYMBOL (CAR (CAR (list)))->value_ptr.symbol->builtin_accessor
	    (CAR (list), CAR (CDR (list)), env, outcome);

	  if (!val)
	    return NULL;
	}
      else
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;

	  return NULL;
	}

      list = CDR (CDR (list));
    }

  increment_refcount (val, NULL);
  return val;
}


struct object *
evaluate_function (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *f;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (CAR (list)->type != TYPE_SYMBOL_NAME && CAR (list)->type != TYPE_SYMBOL)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;

      return NULL;
    }

  f = get_function (SYMBOL (CAR (list)), env, 1);

  if (!f)
    {
      outcome->type = FUNCTION_NOT_FOUND_IN_EVAL;

      return NULL;
    }

  return f;
}


struct object *
evaluate_lambda (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct object *fun;

  if (list_length (list) < 1 || (CAR (list)->type != TYPE_CONS_PAIR
				 && CAR (list) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_DEFUN;
      return NULL;
    }

  fun = create_function (CAR (list), CDR (list), env, outcome);

  if (!fun)
    return NULL;

  return fun;
}


struct object *
evaluate_defstruct (struct object *list, struct environment *env,
		    struct eval_outcome *outcome)
{
  return &nil_object;
}


struct object *
evaluate_apply (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *s, *fun, *last, *l, *args;
  int length = list_length (list);

  if (length < 2)
    {
      outcome->type = TOO_FEW_ARGUMENTS;

      return NULL;
    }

  if (CAR (list)->type != TYPE_SYMBOL_NAME && CAR (list)->type != TYPE_SYMBOL
      && CAR (list)->type != TYPE_FUNCTION)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;

      return NULL;
    }

  if (CAR (list)->type == TYPE_SYMBOL_NAME || CAR (list)->type == TYPE_SYMBOL)
    {
      s = SYMBOL (CAR (list));

      fun = get_function (s, env, 1);

      if (!fun)
	{
	  outcome->type = UNKNOWN_FUNCTION;
	  outcome->obj = s;
	  return NULL;
	}
    }
  else
    fun = CAR (list);

  list = CDR (list);
  length--;
  last = nth (length - 1, list);

  if (last->type != TYPE_CONS_PAIR && last != &nil_object)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;

      return NULL;
    }

  if (length == 1)
    args = CAR (list);
  else
    {
      args = copy_list_structure (list, NULL, length - 1, &l);
      l->value_ptr.cons_pair->cdr = last;
    }

  return call_function (fun, args, 0, 0, env, outcome);
}


struct object *
evaluate_declare (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  outcome->type = DECLARE_NOT_ALLOWED_HERE;

  return NULL;
}


struct object *
execute_body_of_tagbody (struct object *body, struct environment *env,
			 struct eval_outcome *outcome)
{
  struct object *car, *ret;
  struct go_tag *t;

  while (body != &nil_object)
    {
      car = CAR (body);

      if (car->type == TYPE_CONS_PAIR)
	{
	  ret = evaluate_object (car, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!ret)
	    {
	      if (!outcome->tag_to_find && !outcome->cont)
		return NULL;
	      else if (outcome->tag_to_find && outcome->find_tag_now)
		{
		  t = find_go_tag (outcome->tag_to_find, env->go_tag_stack);

		  if (!t)
		    {
		      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);

		      if (!env->go_tag_stack)
			{
			  outcome->tag_to_find = NULL;
			  outcome->find_tag_now = 0;
			  outcome->type = CANT_GO_TO_NONEXISTENT_TAG;
			}

		      return NULL;
		    }

		  outcome->tag_to_find = NULL;
		  outcome->find_tag_now = 0;

		  body = t->dest;
		}
	      else if (outcome->tag_to_find)
		{
		  outcome->find_tag_now = 1;
		  return NULL;
		}
	      else if (outcome->cont)
		{
		  body = outcome->cont;

		  outcome->cont = NULL;
		}
	    }
	  else
	    body = CDR (body);

	  decrement_refcount (ret, NULL);
	}
      else
	body = CDR (body);
    }

  return &nil_object;
}


struct object *
evaluate_tagbody (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  struct object *cons = list, *destfind, *car, *dest, *ret;
  int tags = 0;

  while (cons != &nil_object)
    {
      car = CAR (cons);

      if (car->type == TYPE_SYMBOL_NAME || car->type == TYPE_INTEGER)
	{
	  destfind = CDR (cons);

	  while (destfind != &nil_object && (dest = CAR (destfind))
		 && (dest->type == TYPE_SYMBOL_NAME
		     || dest->type == TYPE_INTEGER))
	    destfind = CDR (destfind);

	  if (!tags)
	    env->go_tag_stack = add_go_tag_frame (env->go_tag_stack);

	  env->go_tag_stack = add_go_tag (car, destfind, env->go_tag_stack);
	  tags++;
	}

      cons = CDR (cons);
    }

  ret = execute_body_of_tagbody (list, env, outcome);

  if (!ret)
    return NULL;

  env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);

  return &nil_object;
}


struct object *
evaluate_go (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  struct go_tag *t;

  if (!env->go_tag_stack)
    {
      outcome->type = CANT_GO_OUTSIDE_TAGBODY;

      return NULL;
    }

  t = find_go_tag (CAR (list), env->go_tag_stack);

  if (!t)
    {
      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);

      if (!env->go_tag_stack)
	{
	  outcome->type = CANT_GO_TO_NONEXISTENT_TAG;

	  return NULL;
	}

      outcome->tag_to_find = CAR (list);

      return NULL;
    }

  outcome->cont = t->dest;

  return NULL;
}


struct object *
builtin_al_print_no_warranty (struct object *list, struct environment *env,
			      struct eval_outcome *outcome)
{
  puts ("THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY "
	"APPLICABLE LAW.\nEXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT "
	"HOLDERS AND/OR OTHER\nPARTIES PROVIDE THE PROGRAM “AS IS” WITHOUT "
	"WARRANTY OF ANY KIND, EITHER EXPRESSED\nOR IMPLIED, INCLUDING, BUT NOT "
	"LIMITED TO, THE IMPLIED WARRANTIES OF\nMERCHANTABILITY AND FITNESS FOR "
	"A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE\nQUALITY AND "
	"PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE\n"
	"DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR "
	"CORRECTION.\n");

  return &t_object;
}


struct object *
builtin_al_print_terms_and_conditions (struct object *list,
				       struct environment *env,
				       struct eval_outcome *outcome)
{
  puts ("  0. Definitions.\n"
	"\n"
	"  \"This License\" refers to version 3 of the GNU General Public License.\n"
	"\n"
	"  \"Copyright\" also means copyright-like laws that apply to other kinds of\n"
	"works, such as semiconductor masks.\n"
	"\n"
	"  \"The Program\" refers to any copyrightable work licensed under this\n"
	"License.  Each licensee is addressed as \"you\".  \"Licensees\" and\n"
	"\"recipients\" may be individuals or organizations.\n"
	"\n"
	"  To \"modify\" a work means to copy from or adapt all or part of the work\n"
	"in a fashion requiring copyright permission, other than the making of an\n"
	"exact copy.  The resulting work is called a \"modified version\" of the\n"
	"earlier work or a work \"based on\" the earlier work.\n"
	"\n"
	"  A \"covered work\" means either the unmodified Program or a work based\n"
	"on the Program.\n"
	"\n"
	"  To \"propagate\" a work means to do anything with it that, without\n"
	"permission, would make you directly or secondarily liable for\n"
	"infringement under applicable copyright law, except executing it on a\n"
	"computer or modifying a private copy.  Propagation includes copying,\n"
	"distribution (with or without modification), making available to the\n"
	"public, and in some countries other activities as well.\n"
	"\n"
	"  To \"convey\" a work means any kind of propagation that enables other\n"
	"parties to make or receive copies.  Mere interaction with a user through\n"
	"a computer network, with no transfer of a copy, is not conveying.\n"
	"\n"
	"  An interactive user interface displays \"Appropriate Legal Notices\"\n"
	"to the extent that it includes a convenient and prominently visible\n"
	"feature that (1) displays an appropriate copyright notice, and (2)\n"
	"tells the user that there is no warranty for the work (except to the\n"
	"extent that warranties are provided), that licensees may convey the\n"
	"work under this License, and how to view a copy of this License.  If\n"
	"the interface presents a list of user commands or options, such as a\n"
	"menu, a prominent item in the list meets this criterion.\n"
	"\n"
	"  1. Source Code.\n"
	"\n"
	"  The \"source code\" for a work means the preferred form of the work\n"
	"for making modifications to it.  \"Object code\" means any non-source\n"
	"form of a work.\n"
	"\n"
	"  A \"Standard Interface\" means an interface that either is an official\n"
	"standard defined by a recognized standards body, or, in the case of\n"
	"interfaces specified for a particular programming language, one that\n"
	"is widely used among developers working in that language.\n"
	"\n"
	"  The \"System Libraries\" of an executable work include anything, other\n"
	"than the work as a whole, that (a) is included in the normal form of\n"
	"packaging a Major Component, but which is not part of that Major\n"
	"Component, and (b) serves only to enable use of the work with that\n"
	"Major Component, or to implement a Standard Interface for which an\n"
	"implementation is available to the public in source code form.  A\n"
	"\"Major Component\", in this context, means a major essential component\n"
	"(kernel, window system, and so on) of the specific operating system\n"
	"(if any) on which the executable work runs, or a compiler used to\n"
	"produce the work, or an object code interpreter used to run it.\n"
	"\n"
	"  The \"Corresponding Source\" for a work in object code form means all\n"
	"the source code needed to generate, install, and (for an executable\n"
	"work) run the object code and to modify the work, including scripts to\n"
	"control those activities.  However, it does not include the work's\n"
	"System Libraries, or general-purpose tools or generally available free\n"
	"programs which are used unmodified in performing those activities but\n"
	"which are not part of the work.  For example, Corresponding Source\n"
	"includes interface definition files associated with source files for\n"
	"the work, and the source code for shared libraries and dynamically\n"
	"linked subprograms that the work is specifically designed to require,\n"
	"such as by intimate data communication or control flow between those\n"
	"subprograms and other parts of the work.\n"
	"\n"
	"  The Corresponding Source need not include anything that users\n"
	"can regenerate automatically from other parts of the Corresponding\n"
	"Source.\n"
	"\n"
	"  The Corresponding Source for a work in source code form is that\n"
	"same work.\n"
	"\n"
	"  2. Basic Permissions.\n"
	"\n"
	"  All rights granted under this License are granted for the term of\n"
	"copyright on the Program, and are irrevocable provided the stated\n"
	"conditions are met.  This License explicitly affirms your unlimited\n"
	"permission to run the unmodified Program.  The output from running a\n"
	"covered work is covered by this License only if the output, given its\n"
	"content, constitutes a covered work.  This License acknowledges your\n"
	"rights of fair use or other equivalent, as provided by copyright law.\n"
	"\n"
	"  You may make, run and propagate covered works that you do not\n"
	"convey, without conditions so long as your license otherwise remains\n"
	"in force.  You may convey covered works to others for the sole purpose\n"
	"of having them make modifications exclusively for you, or provide you\n"
	"with facilities for running those works, provided that you comply with\n"
	"the terms of this License in conveying all material for which you do\n"
	"not control copyright.  Those thus making or running the covered works\n"
	"for you must do so exclusively on your behalf, under your direction\n"
	"and control, on terms that prohibit them from making any copies of\n"
	"your copyrighted material outside their relationship with you.\n"
	"\n"
	"  Conveying under any other circumstances is permitted solely under\n"
	"the conditions stated below.  Sublicensing is not allowed; section 10\n"
	"makes it unnecessary.\n"
	"\n"
	"  3. Protecting Users' Legal Rights From Anti-Circumvention Law.\n"
	"\n"
	"  No covered work shall be deemed part of an effective technological\n"
	"measure under any applicable law fulfilling obligations under article\n"
	"11 of the WIPO copyright treaty adopted on 20 December 1996, or\n"
	"similar laws prohibiting or restricting circumvention of such\n"
	"measures.\n"
	"\n"
	"  When you convey a covered work, you waive any legal power to forbid\n"
	"circumvention of technological measures to the extent such circumvention\n"
	"is effected by exercising rights under this License with respect to\n"
	"the covered work, and you disclaim any intention to limit operation or\n"
	"modification of the work as a means of enforcing, against the work's\n"
	"users, your or third parties' legal rights to forbid circumvention of\n"
	"technological measures.\n"
	"\n"
	"  4. Conveying Verbatim Copies.\n"
	"\n"
	"  You may convey verbatim copies of the Program's source code as you\n"
	"receive it, in any medium, provided that you conspicuously and\n"
	"appropriately publish on each copy an appropriate copyright notice;\n"
	"keep intact all notices stating that this License and any\n"
	"non-permissive terms added in accord with section 7 apply to the code;\n"
	"keep intact all notices of the absence of any warranty; and give all\n"
	"recipients a copy of this License along with the Program.\n"
	"\n"
	"  You may charge any price or no price for each copy that you convey,\n"
	"and you may offer support or warranty protection for a fee.\n"
	"\n"
	"  5. Conveying Modified Source Versions.\n"
	"\n"
	"  You may convey a work based on the Program, or the modifications to\n"
	"produce it from the Program, in the form of source code under the\n"
	"terms of section 4, provided that you also meet all of these conditions:\n"
	"\n"
	"    a) The work must carry prominent notices stating that you modified\n"
	"    it, and giving a relevant date.\n"
	"\n"
	"    b) The work must carry prominent notices stating that it is\n"
	"    released under this License and any conditions added under section\n"
	"    7.  This requirement modifies the requirement in section 4 to\n"
	"    \"keep intact all notices\".\n"
	"\n"
	"    c) You must license the entire work, as a whole, under this\n"
	"    License to anyone who comes into possession of a copy.  This\n"
	"    License will therefore apply, along with any applicable section 7\n"
	"    additional terms, to the whole of the work, and all its parts,\n"
	"    regardless of how they are packaged.  This License gives no\n"
	"    permission to license the work in any other way, but it does not\n"
	"    invalidate such permission if you have separately received it.\n"
	"\n"
	"    d) If the work has interactive user interfaces, each must display\n"
	"    Appropriate Legal Notices; however, if the Program has interactive\n"
	"    interfaces that do not display Appropriate Legal Notices, your\n"
	"    work need not make them do so.\n"
	"\n"
	"  A compilation of a covered work with other separate and independent\n"
	"works, which are not by their nature extensions of the covered work,\n"
	"and which are not combined with it such as to form a larger program,\n"
	"in or on a volume of a storage or distribution medium, is called an\n"
	"\"aggregate\" if the compilation and its resulting copyright are not\n"
	"used to limit the access or legal rights of the compilation's users\n"
	"beyond what the individual works permit.  Inclusion of a covered work\n"
	"in an aggregate does not cause this License to apply to the other\n"
	"parts of the aggregate.\n"
	"\n"
	"  6. Conveying Non-Source Forms.\n"
	"\n"
	"  You may convey a covered work in object code form under the terms\n"
	"of sections 4 and 5, provided that you also convey the\n"
	"machine-readable Corresponding Source under the terms of this License,\n"
	"in one of these ways:\n"
	"\n"
	"    a) Convey the object code in, or embodied in, a physical product\n"
	"    (including a physical distribution medium), accompanied by the\n"
	"    Corresponding Source fixed on a durable physical medium\n"
	"    customarily used for software interchange.\n"
	"\n"
	"    b) Convey the object code in, or embodied in, a physical product\n"
	"    (including a physical distribution medium), accompanied by a\n"
	"    written offer, valid for at least three years and valid for as\n"
	"    long as you offer spare parts or customer support for that product\n"
	"    model, to give anyone who possesses the object code either (1) a\n"
	"    copy of the Corresponding Source for all the software in the\n"
	"    product that is covered by this License, on a durable physical\n"
	"    medium customarily used for software interchange, for a price no\n"
	"    more than your reasonable cost of physically performing this\n"
	"    conveying of source, or (2) access to copy the\n"
	"    Corresponding Source from a network server at no charge.\n"
	"\n"
	"    c) Convey individual copies of the object code with a copy of the\n"
	"    written offer to provide the Corresponding Source.  This\n"
	"    alternative is allowed only occasionally and noncommercially, and\n"
	"    only if you received the object code with such an offer, in accord\n"
	"    with subsection 6b.\n"
	"\n"
	"    d) Convey the object code by offering access from a designated\n"
	"    place (gratis or for a charge), and offer equivalent access to the\n"
	"    Corresponding Source in the same way through the same place at no\n"
	"    further charge.  You need not require recipients to copy the\n"
	"    Corresponding Source along with the object code.  If the place to\n"
	"    copy the object code is a network server, the Corresponding Source\n"
	"    may be on a different server (operated by you or a third party)\n"
	"    that supports equivalent copying facilities, provided you maintain\n"
	"    clear directions next to the object code saying where to find the\n"
	"    Corresponding Source.  Regardless of what server hosts the\n"
	"    Corresponding Source, you remain obligated to ensure that it is\n"
	"    available for as long as needed to satisfy these requirements.\n"
	"\n"
	"    e) Convey the object code using peer-to-peer transmission, provided\n"
	"    you inform other peers where the object code and Corresponding\n"
	"    Source of the work are being offered to the general public at no\n"
	"    charge under subsection 6d.\n"
	"\n"
	"  A separable portion of the object code, whose source code is excluded\n"
	"from the Corresponding Source as a System Library, need not be\n"
	"included in conveying the object code work.\n"
	"\n"
	"  A \"User Product\" is either (1) a \"consumer product\", which means any\n"
	"tangible personal property which is normally used for personal, family,\n"
	"or household purposes, or (2) anything designed or sold for incorporation\n"
	"into a dwelling.  In determining whether a product is a consumer product,\n"
	"doubtful cases shall be resolved in favor of coverage.  For a particular\n"
	"product received by a particular user, \"normally used\" refers to a\n"
	"typical or common use of that class of product, regardless of the status\n"
	"of the particular user or of the way in which the particular user\n"
	"actually uses, or expects or is expected to use, the product.  A product\n"
	"is a consumer product regardless of whether the product has substantial\n"
	"commercial, industrial or non-consumer uses, unless such uses represent\n"
	"the only significant mode of use of the product.\n"
	"\n"
	"  \"Installation Information\" for a User Product means any methods,\n"
	"procedures, authorization keys, or other information required to install\n"
	"and execute modified versions of a covered work in that User Product from\n"
	"a modified version of its Corresponding Source.  The information must\n"
	"suffice to ensure that the continued functioning of the modified object\n"
	"code is in no case prevented or interfered with solely because\n"
	"modification has been made.\n"
	"\n"
	"  If you convey an object code work under this section in, or with, or\n"
	"specifically for use in, a User Product, and the conveying occurs as\n"
	"part of a transaction in which the right of possession and use of the\n"
	"User Product is transferred to the recipient in perpetuity or for a\n"
	"fixed term (regardless of how the transaction is characterized), the\n"
	"Corresponding Source conveyed under this section must be accompanied\n"
	"by the Installation Information.  But this requirement does not apply\n"
	"if neither you nor any third party retains the ability to install\n"
	"modified object code on the User Product (for example, the work has\n"
	"been installed in ROM).\n"
	"\n"
	"  The requirement to provide Installation Information does not include a\n"
	"requirement to continue to provide support service, warranty, or updates\n"
	"for a work that has been modified or installed by the recipient, or for\n"
	"the User Product in which it has been modified or installed.  Access to a\n"
	"network may be denied when the modification itself materially and\n"
	"adversely affects the operation of the network or violates the rules and\n"
	"protocols for communication across the network.\n"
	"\n"
	"  Corresponding Source conveyed, and Installation Information provided,\n"
	"in accord with this section must be in a format that is publicly\n"
	"documented (and with an implementation available to the public in\n"
	"source code form), and must require no special password or key for\n"
	"unpacking, reading or copying.\n"
	"\n"
	"  7. Additional Terms.\n"
	"\n"
	"  \"Additional permissions\" are terms that supplement the terms of this\n"
	"License by making exceptions from one or more of its conditions.\n"
	"Additional permissions that are applicable to the entire Program shall\n"
	"be treated as though they were included in this License, to the extent\n"
	"that they are valid under applicable law.  If additional permissions\n"
	"apply only to part of the Program, that part may be used separately\n"
	"under those permissions, but the entire Program remains governed by\n"
	"this License without regard to the additional permissions.\n"
	"\n"
	"  When you convey a copy of a covered work, you may at your option\n"
	"remove any additional permissions from that copy, or from any part of\n"
	"it.  (Additional permissions may be written to require their own\n"
	"removal in certain cases when you modify the work.)  You may place\n"
	"additional permissions on material, added by you to a covered work,\n"
	"for which you have or can give appropriate copyright permission.\n"
	"\n"
	"  Notwithstanding any other provision of this License, for material you\n"
	"add to a covered work, you may (if authorized by the copyright holders of\n"
	"that material) supplement the terms of this License with terms:\n"
	"\n"
	"    a) Disclaiming warranty or limiting liability differently from the\n"
	"    terms of sections 15 and 16 of this License; or\n"
	"\n"
	"    b) Requiring preservation of specified reasonable legal notices or\n"
	"    author attributions in that material or in the Appropriate Legal\n"
	"    Notices displayed by works containing it; or\n"
	"\n"
	"    c) Prohibiting misrepresentation of the origin of that material, or\n"
	"    requiring that modified versions of such material be marked in\n"
	"    reasonable ways as different from the original version; or\n"
	"\n"
	"    d) Limiting the use for publicity purposes of names of licensors or\n"
	"    authors of the material; or\n"
	"\n"
	"    e) Declining to grant rights under trademark law for use of some\n"
	"    trade names, trademarks, or service marks; or\n"
	"\n"
	"    f) Requiring indemnification of licensors and authors of that\n"
	"    material by anyone who conveys the material (or modified versions of\n"
	"    it) with contractual assumptions of liability to the recipient, for\n"
	"    any liability that these contractual assumptions directly impose on\n"
	"    those licensors and authors.\n"
	"\n"
	"  All other non-permissive additional terms are considered \"further\n"
	"restrictions\" within the meaning of section 10.  If the Program as you\n"
	"received it, or any part of it, contains a notice stating that it is\n"
	"governed by this License along with a term that is a further\n"
	"restriction, you may remove that term.  If a license document contains\n"
	"a further restriction but permits relicensing or conveying under this\n"
	"License, you may add to a covered work material governed by the terms\n"
	"of that license document, provided that the further restriction does\n"
	"not survive such relicensing or conveying.\n"
	"\n"
	"  If you add terms to a covered work in accord with this section, you\n"
	"must place, in the relevant source files, a statement of the\n"
	"additional terms that apply to those files, or a notice indicating\n"
	"where to find the applicable terms.\n"
	"\n"
	"  Additional terms, permissive or non-permissive, may be stated in the\n"
	"form of a separately written license, or stated as exceptions;\n"
	"the above requirements apply either way.\n"
	"\n"
	"  8. Termination.\n"
	"\n"
	"  You may not propagate or modify a covered work except as expressly\n"
	"provided under this License.  Any attempt otherwise to propagate or\n"
	"modify it is void, and will automatically terminate your rights under\n"
	"this License (including any patent licenses granted under the third\n"
	"paragraph of section 11).\n"
	"\n"
	"  However, if you cease all violation of this License, then your\n"
	"license from a particular copyright holder is reinstated (a)\n"
	"provisionally, unless and until the copyright holder explicitly and\n"
	"finally terminates your license, and (b) permanently, if the copyright\n"
	"holder fails to notify you of the violation by some reasonable means\n"
	"prior to 60 days after the cessation.\n"
	"\n"
	"  Moreover, your license from a particular copyright holder is\n"
	"reinstated permanently if the copyright holder notifies you of the\n"
	"violation by some reasonable means, this is the first time you have\n"
	"received notice of violation of this License (for any work) from that\n"
	"copyright holder, and you cure the violation prior to 30 days after\n"
	"your receipt of the notice.\n"
	"\n"
	"  Termination of your rights under this section does not terminate the\n"
	"licenses of parties who have received copies or rights from you under\n"
	"this License.  If your rights have been terminated and not permanently\n"
	"reinstated, you do not qualify to receive new licenses for the same\n"
	"material under section 10.\n"
	"\n"
	"  9. Acceptance Not Required for Having Copies.\n"
	"\n"
	"  You are not required to accept this License in order to receive or\n"
	"run a copy of the Program.  Ancillary propagation of a covered work\n"
	"occurring solely as a consequence of using peer-to-peer transmission\n"
	"to receive a copy likewise does not require acceptance.  However,\n"
	"nothing other than this License grants you permission to propagate or\n"
	"modify any covered work.  These actions infringe copyright if you do\n"
	"not accept this License.  Therefore, by modifying or propagating a\n"
	"covered work, you indicate your acceptance of this License to do so.\n"
	"\n"
	"  10. Automatic Licensing of Downstream Recipients.\n"
	"\n"
	"  Each time you convey a covered work, the recipient automatically\n"
	"receives a license from the original licensors, to run, modify and\n"
	"propagate that work, subject to this License.  You are not responsible\n"
	"for enforcing compliance by third parties with this License.\n"
	"\n"
	"  An \"entity transaction\" is a transaction transferring control of an\n"
	"organization, or substantially all assets of one, or subdividing an\n"
	"organization, or merging organizations.  If propagation of a covered\n"
	"work results from an entity transaction, each party to that\n"
	"transaction who receives a copy of the work also receives whatever\n"
	"licenses to the work the party's predecessor in interest had or could\n"
	"give under the previous paragraph, plus a right to possession of the\n"
	"Corresponding Source of the work from the predecessor in interest, if\n"
	"the predecessor has it or can get it with reasonable efforts.\n"
	"\n"
	"  You may not impose any further restrictions on the exercise of the\n"
	"rights granted or affirmed under this License.  For example, you may\n"
	"not impose a license fee, royalty, or other charge for exercise of\n"
	"rights granted under this License, and you may not initiate litigation\n"
	"(including a cross-claim or counterclaim in a lawsuit) alleging that\n"
	"any patent claim is infringed by making, using, selling, offering for\n"
	"sale, or importing the Program or any portion of it.\n"
	"\n"
	"  11. Patents.\n"
	"\n"
	"  A \"contributor\" is a copyright holder who authorizes use under this\n"
	"License of the Program or a work on which the Program is based.  The\n"
	"work thus licensed is called the contributor's \"contributor version\".\n"
	"\n"
	"  A contributor's \"essential patent claims\" are all patent claims\n"
	"owned or controlled by the contributor, whether already acquired or\n"
	"hereafter acquired, that would be infringed by some manner, permitted\n"
	"by this License, of making, using, or selling its contributor version,\n"
	"but do not include claims that would be infringed only as a\n"
	"consequence of further modification of the contributor version.  For\n"
	"purposes of this definition, \"control\" includes the right to grant\n"
	"patent sublicenses in a manner consistent with the requirements of\n"
	"this License.\n"
	"\n"
	"  Each contributor grants you a non-exclusive, worldwide, royalty-free\n"
	"patent license under the contributor's essential patent claims, to\n"
	"make, use, sell, offer for sale, import and otherwise run, modify and\n"
	"propagate the contents of its contributor version.\n"
	"\n"
	"  In the following three paragraphs, a \"patent license\" is any express\n"
	"agreement or commitment, however denominated, not to enforce a patent\n"
	"(such as an express permission to practice a patent or covenant not to\n"
	"sue for patent infringement).  To \"grant\" such a patent license to a\n"
	"party means to make such an agreement or commitment not to enforce a\n"
	"patent against the party.\n"
	"\n"
	"  If you convey a covered work, knowingly relying on a patent license,\n"
	"and the Corresponding Source of the work is not available for anyone\n"
	"to copy, free of charge and under the terms of this License, through a\n"
	"publicly available network server or other readily accessible means,\n"
	"then you must either (1) cause the Corresponding Source to be so\n"
	"available, or (2) arrange to deprive yourself of the benefit of the\n"
	"patent license for this particular work, or (3) arrange, in a manner\n"
	"consistent with the requirements of this License, to extend the patent\n"
	"license to downstream recipients.  \"Knowingly relying\" means you have\n"
	"actual knowledge that, but for the patent license, your conveying the\n"
	"covered work in a country, or your recipient's use of the covered work\n"
	"in a country, would infringe one or more identifiable patents in that\n"
	"country that you have reason to believe are valid.\n"
	"\n"
	"  If, pursuant to or in connection with a single transaction or\n"
	"arrangement, you convey, or propagate by procuring conveyance of, a\n"
	"covered work, and grant a patent license to some of the parties\n"
	"receiving the covered work authorizing them to use, propagate, modify\n"
	"or convey a specific copy of the covered work, then the patent license\n"
	"you grant is automatically extended to all recipients of the covered\n"
	"work and works based on it.\n"
	"\n"
	"  A patent license is \"discriminatory\" if it does not include within\n"
	"the scope of its coverage, prohibits the exercise of, or is\n"
	"conditioned on the non-exercise of one or more of the rights that are\n"
	"specifically granted under this License.  You may not convey a covered\n"
	"work if you are a party to an arrangement with a third party that is\n"
	"in the business of distributing software, under which you make payment\n"
	"to the third party based on the extent of your activity of conveying\n"
	"the work, and under which the third party grants, to any of the\n"
	"parties who would receive the covered work from you, a discriminatory\n"
	"patent license (a) in connection with copies of the covered work\n"
	"conveyed by you (or copies made from those copies), or (b) primarily\n"
	"for and in connection with specific products or compilations that\n"
	"contain the covered work, unless you entered into that arrangement,\n"
	"or that patent license was granted, prior to 28 March 2007.\n"
	"\n"
	"  Nothing in this License shall be construed as excluding or limiting\n"
	"any implied license or other defenses to infringement that may\n"
	"otherwise be available to you under applicable patent law.\n"
	"\n"
	"  12. No Surrender of Others' Freedom.\n"
	"\n"
	"  If conditions are imposed on you (whether by court order, agreement or\n"
	"otherwise) that contradict the conditions of this License, they do not\n"
	"excuse you from the conditions of this License.  If you cannot convey a\n"
	"covered work so as to satisfy simultaneously your obligations under this\n"
	"License and any other pertinent obligations, then as a consequence you may\n"
	"not convey it at all.  For example, if you agree to terms that obligate you\n"
	"to collect a royalty for further conveying from those to whom you convey\n"
	"the Program, the only way you could satisfy both those terms and this\n"
	"License would be to refrain entirely from conveying the Program.\n"
	"\n"
	"  13. Use with the GNU Affero General Public License.\n"
	"\n"
	"  Notwithstanding any other provision of this License, you have\n"
	"permission to link or combine any covered work with a work licensed\n"
	"under version 3 of the GNU Affero General Public License into a single\n"
	"combined work, and to convey the resulting work.  The terms of this\n"
	"License will continue to apply to the part which is the covered work,\n"
	"but the special requirements of the GNU Affero General Public License,\n"
	"section 13, concerning interaction through a network will apply to the\n"
	"combination as such.\n"
	"\n"
	"  14. Revised Versions of this License.\n"
	"\n"
	"  The Free Software Foundation may publish revised and/or new versions of\n"
	"the GNU General Public License from time to time.  Such new versions will\n"
	"be similar in spirit to the present version, but may differ in detail to\n"
	"address new problems or concerns.\n"
	"\n"
	"  Each version is given a distinguishing version number.  If the\n"
	"Program specifies that a certain numbered version of the GNU General\n"
	"Public License \"or any later version\" applies to it, you have the\n"
	"option of following the terms and conditions either of that numbered\n"
	"version or of any later version published by the Free Software\n"
	"Foundation.  If the Program does not specify a version number of the\n"
	"GNU General Public License, you may choose any version ever published\n"
	"by the Free Software Foundation.\n"
	"\n"
	"  If the Program specifies that a proxy can decide which future\n"
	"versions of the GNU General Public License can be used, that proxy's\n"
	"public statement of acceptance of a version permanently authorizes you\n"
	"to choose that version for the Program.\n"
	"\n"
	"  Later license versions may give you additional or different\n"
	"permissions.  However, no additional obligations are imposed on any\n"
	"author or copyright holder as a result of your choosing to follow a\n"
	"later version.\n"
	"\n"
	"  15. Disclaimer of Warranty.\n"
	"\n"
	"  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY\n"
	"APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT\n"
	"HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY\n"
	"OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,\n"
	"THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\n"
	"PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM\n"
	"IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF\n"
	"ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n"
	"\n"
	"  16. Limitation of Liability.\n"
	"\n"
	"  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING\n"
	"WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS\n"
	"THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY\n"
	"GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE\n"
	"USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF\n"
	"DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD\n"
	"PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),\n"
	"EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF\n"
	"SUCH DAMAGES.\n"
	"\n"
	"  17. Interpretation of Sections 15 and 16.\n"
	"\n"
	"  If the disclaimer of warranty and limitation of liability provided\n"
	"above cannot be given local legal effect according to their terms,\n"
	"reviewing courts shall apply local law that most closely approximates\n"
	"an absolute waiver of all civil liability in connection with the\n"
	"Program, unless a warranty or assumption of liability accompanies a\n"
	"copy of the Program in return for a fee.\n");

  return &t_object;
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
symbol_equals (const struct object *sym, const char *str,
	       struct environment *env)
{
  struct symbol *s;

  if (sym->type != TYPE_SYMBOL_NAME && sym->type != TYPE_SYMBOL)
    return 0;

  s = SYMBOL (sym)->value_ptr.symbol;

  if (*str == ':')
    {
      return s->home_package == env->keyword_package
	&& eqmem (s->name, s->name_len, str+1, strlen (str)-1);
    }

  return eqmem (s->name, s->name_len, str, strlen (str));
}


int
symbol_is_among (const struct object *sym, struct environment *env, ...)
{
  va_list valist;
  char *s;

  va_start (valist, env);

  while ((s = va_arg (valist, char *)))
    {
      if (symbol_equals (sym, s, env))
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
print_as_symbol (const char *sym, size_t len)
{
  size_t i;
  char need_escape [] = "().,;'#\"\n\\";
  int do_need_escape = 0;

  for (i = 0; i < len && !do_need_escape; i++)
    {
      if (strchr (need_escape, sym [i]) || !sym [i] || islower (sym [i]))
	do_need_escape = 1;
    }

  if (do_need_escape)
    putchar ('|');

  for (i = 0; i < len; i++)
    {
      if (sym [i] == '|' || sym [i] == '\\')
	putchar ('\\');

      putchar (sym [i]);
    }

  if (do_need_escape)
    putchar ('|');
}


void
print_symbol_name (const struct symbol_name *sym, struct environment *env)
{
  if (sym->sym->value_ptr.symbol->home_package)
    print_symbol (sym->sym->value_ptr.symbol, env);
  else
    {
      printf ("#:");
      print_as_symbol (sym->value, sym->used_size);
    }
}


void
print_symbol (const struct symbol *sym, struct environment *env)
{
  if (!sym->home_package)
    printf ("#:");
  else if (sym->home_package == env->keyword_package)
    printf (":");

  print_as_symbol (sym->name, sym->name_len);
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
  
  while (cdr && cdr != &nil_object)
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
print_function_or_macro (const struct object *obj, struct environment *env)
{
  if (obj->type == TYPE_FUNCTION)
    {
      printf ("#<FUNCTION ");

      if (obj->value_ptr.function->builtin_form)
	printf ("BUILTIN ");

      if (obj->value_ptr.function->name)
	print_symbol (obj->value_ptr.function->name->value_ptr.symbol, env);
      else
	printf ("%p", (void *)obj);

      printf (">");
    }
  else
    {
      if (obj->value_ptr.macro->is_special_operator)
	{
	  printf ("#<SPECIAL OPERATOR ");
	}
      else
	{
	  printf ("#<MACRO ");

	  if (obj->value_ptr.function->builtin_form)
	    printf ("BUILTIN ");
	}

      if (obj->value_ptr.macro->name)
	print_symbol (obj->value_ptr.macro->name->value_ptr.symbol, env);
      else
	printf ("%p", (void *)obj);

      printf (">");
    }
}


void
print_object (const struct object *obj, struct environment *env)
{
  if (obj == &nil_object)
    printf ("()");
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
  else if (obj->type == TYPE_AT)
    {
      printf ("@");
      print_object (obj->value_ptr.next, env);
    }
  else if (obj->type == TYPE_DOT)
    {
      printf (".");
      print_object (obj->value_ptr.next, env);
    }
  else if (obj->type == TYPE_INTEGER)
    mpz_out_str (NULL, 10, obj->value_ptr.integer);
  else if (obj->type == TYPE_RATIO)
    mpq_out_str (NULL, 10, obj->value_ptr.ratio);
  else if (obj->type == TYPE_FLOAT)
    gmp_printf ("%.Ff", obj->value_ptr.floating);
  else if (obj->type == TYPE_STRING)
    print_string (obj->value_ptr.string);
  else if (obj->type == TYPE_CHARACTER)
    print_character (obj->value_ptr.character);
  else if (obj->type == TYPE_FILENAME)
    print_filename (obj->value_ptr.filename);
  else if (obj->type == TYPE_SYMBOL_NAME)
    print_symbol_name (obj->value_ptr.symbol_name, env);
  else if (obj->type == TYPE_SYMBOL)
    print_symbol (obj->value_ptr.symbol, env);
  else if (obj->type == TYPE_CONS_PAIR)
    print_list (obj->value_ptr.cons_pair, env);
  else if (obj->type == TYPE_ARRAY)
    print_array (obj->value_ptr.array, env);
  else if (obj->type == TYPE_FUNCTION || obj->type == TYPE_MACRO)
    print_function_or_macro (obj, env);
  else if (obj->type == TYPE_PACKAGE)
    {
      printf ("#<PACKAGE \"");
      print_symbol (obj->value_ptr.package->name->value_ptr.symbol, env);
      printf ("\">");
    }
  else if (obj->type == TYPE_ENVIRONMENT)
    printf ("#<ENVIRONMENT %p>", (void *)obj);
  else if (obj->type == TYPE_STREAM)
    printf ("#<STREAM %p>", (void *)obj);
  else
    printf ("#<print not implemented>");
}


void
print_read_error (enum read_outcome err, const char *input, size_t size,
		  const char *begin, const char *end,
		  const struct read_outcome_args *args)
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
  else if (err == FUNCTION_NOT_FOUND_IN_READ)
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
  else if (err == MORE_THAN_A_CONSING_DOT_NOT_ALLOWED)
    {
      printf ("read error: more than one consing dot not allowed in list\n");
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
      printf ("read error: package ");
      fwrite (args->obj->value_ptr.symbol_name->value,
	      args->obj->value_ptr.symbol_name->used_size, 1, stdout);
      printf (" not found\n");
    }
  else if (err == PACKAGE_MARKER_IN_SHARP_COLON)
    {
      printf ("read error: a package marker can't appear in a sharp-colon macro\n");
    }
}


void
print_eval_error (struct eval_outcome *err, struct environment *env)
{
  if (err->type == UNBOUND_SYMBOL)
    {
      printf ("eval error: symbol ");
      print_object (err->obj, env);
      printf (" not bound to any object\n");
    }
  else if (err->type == UNKNOWN_FUNCTION)
    {
      printf ("eval error: symbol ");
      print_object (err->obj, env);
      printf (" not bound to any function, macro or special operator\n");
    }
  else if (err->type == INVALID_FUNCTION_CALL)
    {
      printf ("eval error: not a function form, a macro form or a special "
	      "form\n");
    }
  else if (err->type == KEY_NOT_FOUND_IN_FUNCALL)
    {
      printf ("eval error: unknown key in keyword part of function call\n");
    }
  else if (err->type == ODD_NUMBER_OF_ARGUMENTS)
    {
      printf ("eval error: odd number of arguments\n");
    }
  else if (err->type == ODD_NUMBER_OF_KEYWORD_ARGUMENTS)
    {
      printf ("eval error: odd number of arguments in keyword part of function "
	      "call\n");
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
  else if (err->type == CANT_SPLICE_AFTER_CONSING_DOT)
    {
      printf ("eval error: splicing after consing dot is not allowed\n");
    }
  else if (err->type == SPLICING_OF_ATOM_NOT_ALLOWED_HERE)
    {
      printf ("eval error: splicing of an atom is not allowed here\n");
    }
  else if (err->type == NOTHING_EXPANDED_BEFORE_CONSING_DOT)
    {
      printf ("eval error: splicing produced nothing before consing dot\n");
    }
  else if (err->type == WRONG_NUMBER_OF_ARGUMENTS)
    {
      printf ("eval error: wrong number of arguments\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_LET)
    {
      printf ("eval error: incorrect syntax in LET or LET*\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_FLET)
    {
      printf ("eval error: incorrect syntax in FLET, LABELS or MACROLET\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_DEFUN)
    {
      printf ("eval error: incorrect syntax in DEFUN\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_DEFMACRO)
    {
      printf ("eval error: incorrect syntax in DEFMACRO\n");
    }
  else if (err->type == INVALID_LAMBDA_LIST)
    {
      printf ("eval error: lambda list is invalid\n");
    }
  else if (err->type == CANT_REDEFINE_SPECIAL_OPERATOR)
    {
      printf ("eval error: redefining special operators is not allowed\n");
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
  else if (err->type == WRONG_NUMBER_OF_AXIS)
    {
      printf ("eval error: wrong number of axis\n");
    }
  else if (err->type == OUT_OF_BOUND_INDEX)
    {
      printf ("eval error: out-of-bound index\n");
    }
  else if (err->type == COULD_NOT_OPEN_FILE)
    {
      printf ("file error: could not open file\n");
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
  else if (err->type == CANT_GO_OUTSIDE_TAGBODY)
    {
      printf ("eval error: can't perform GO outside of a TAGBODY\n");
    }
  else if (err->type == INVALID_GO_TAG)
    {
      printf ("eval error: not a valid go tag\n");
    }
  else if (err->type == CANT_GO_TO_NONEXISTENT_TAG)
    {
      printf ("eval error: can't GO to a tag that doesn't exist\n");
    }
  else if (err->type == INVALID_ACCESSOR)
    {
      printf ("eval error: not a valid accessor\n");
    }
  else if (err->type == FUNCTION_NOT_FOUND_IN_EVAL)
    {
      printf ("eval error: function not found\n");
    }
  else if (err->type == DECLARE_NOT_ALLOWED_HERE)
    {
      printf ("eval error: DECLARE form only allowed as first in body of "
	      "certain forms\n");
    }
  else if (err->type == CANT_DIVIDE_BY_ZERO)
    {
      printf ("eval error: division by zero is not allowed\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT)
    {
      printf ("eval error: incorrect syntax in loop construct\n");
    }
}


#define ANTILOOP_HASH_T_SIZE 1024


void
increment_refcount_by (struct object *obj, int count, struct object *parent)
{
  struct object_list **antiloop_hash_t = NULL;

  for (; count; count--)
    {
      if (parent)
	{
	  antiloop_hash_t = alloc_empty_hash_table (ANTILOOP_HASH_T_SIZE);
	  prepend_object_to_obj_list
	    (parent, &antiloop_hash_t [hash_object (parent, ANTILOOP_HASH_T_SIZE)]);
	}

      increment_refcount (obj, antiloop_hash_t);

      if (antiloop_hash_t)
	{
	  free_hash_table (antiloop_hash_t, ANTILOOP_HASH_T_SIZE);
	  antiloop_hash_t = NULL;
	}
    }
}


void
increment_refcount (struct object *obj, struct object_list **antiloop_hash_t)
{
  int allocated_now = 0;
  struct object_list **hash_t_clone = NULL;
  struct parameter *par;

  if (!obj || obj == &nil_object  || obj == &t_object)
    return;

  if (HAS_LEAF_TYPE (obj))
    obj->refcount++;
  else
    {
      if (!antiloop_hash_t)
	{
	  antiloop_hash_t = alloc_empty_hash_table (ANTILOOP_HASH_T_SIZE);
	  allocated_now = 1;
	}

      if (!allocated_now && is_object_in_hash_table (obj, antiloop_hash_t,
						     ANTILOOP_HASH_T_SIZE))
	return;

      obj->refcount++;

      prepend_object_to_obj_list
	(obj, &antiloop_hash_t [hash_object (obj, ANTILOOP_HASH_T_SIZE)]);

      if (obj->type & TYPE_PREFIX)
	increment_refcount (obj->value_ptr.next, antiloop_hash_t);
      else if (obj->type == TYPE_CONS_PAIR)
	{
	  if (obj->value_ptr.cons_pair->cdr == &nil_object)
	    increment_refcount (obj->value_ptr.cons_pair->car,
				antiloop_hash_t);
	  else
	    {
	      hash_t_clone = clone_hash_table (antiloop_hash_t,
					       ANTILOOP_HASH_T_SIZE);
	      increment_refcount (obj->value_ptr.cons_pair->car,
				  antiloop_hash_t);
	      increment_refcount (obj->value_ptr.cons_pair->cdr,
				  hash_t_clone);
	    }
	}
      else if (obj->type == TYPE_SYMBOL_NAME)
	increment_refcount (obj->value_ptr.symbol_name->sym, antiloop_hash_t);
      else if (obj->type == TYPE_SYMBOL)
	{
	  if (obj->value_ptr.symbol->value_cell
	      && obj->value_ptr.symbol->function_cell)
	    {
	      hash_t_clone = clone_hash_table (antiloop_hash_t,
					       ANTILOOP_HASH_T_SIZE);
	      increment_refcount (obj->value_ptr.symbol->value_cell,
				  antiloop_hash_t);
	      increment_refcount (obj->value_ptr.symbol->function_cell,
				  hash_t_clone);
	    }
	  else
	    {
	      increment_refcount (obj->value_ptr.symbol->value_cell,
				  antiloop_hash_t);
	      increment_refcount (obj->value_ptr.symbol->function_cell,
				  antiloop_hash_t);
	    }
	}
      else if (obj->type == TYPE_FUNCTION || obj->type == TYPE_MACRO)
	{
	  if (!obj->value_ptr.function->lambda_list)
	    increment_refcount (obj->value_ptr.function->body, antiloop_hash_t);
	  else
	    {
	      hash_t_clone = clone_hash_table (antiloop_hash_t,
					       ANTILOOP_HASH_T_SIZE);

	      increment_refcount (obj->value_ptr.function->body, hash_t_clone);

	      par = obj->value_ptr.function->lambda_list;

	      while (par)
		{
		  free_hash_table (hash_t_clone, ANTILOOP_HASH_T_SIZE);

		  hash_t_clone = clone_hash_table (antiloop_hash_t,
						   ANTILOOP_HASH_T_SIZE);

		  increment_refcount (par->name, hash_t_clone);

		  if (par->init_form)
		    {
		      free_hash_table (hash_t_clone, ANTILOOP_HASH_T_SIZE);

		      hash_t_clone = clone_hash_table (antiloop_hash_t,
						       ANTILOOP_HASH_T_SIZE);

		      increment_refcount (par->init_form, hash_t_clone);
		    }

		  if (par->supplied_p_param)
		    {
		      free_hash_table (hash_t_clone, ANTILOOP_HASH_T_SIZE);

		      hash_t_clone = clone_hash_table (antiloop_hash_t,
						       ANTILOOP_HASH_T_SIZE);

		      increment_refcount (par->supplied_p_param, hash_t_clone);
		    }

		  par = par->next;
		}
	    }
	}

      if (allocated_now)
	free_hash_table (antiloop_hash_t, ANTILOOP_HASH_T_SIZE);

      if (hash_t_clone)
	free_hash_table (hash_t_clone, ANTILOOP_HASH_T_SIZE);
    }
}


int
decrement_refcount_by (struct object *obj, int count, struct object *parent)
{
  struct object_list **antiloop_hash_t = NULL;
  int ret;

  for (; count; count--)
    {
      if (parent)
	{
	  antiloop_hash_t = alloc_empty_hash_table (ANTILOOP_HASH_T_SIZE);
	  prepend_object_to_obj_list
	    (parent, &antiloop_hash_t [hash_object (parent, ANTILOOP_HASH_T_SIZE)]);
	}

      ret = decrement_refcount (obj, antiloop_hash_t);

      if (antiloop_hash_t)
	{
	  free_hash_table (antiloop_hash_t, ANTILOOP_HASH_T_SIZE);
	  antiloop_hash_t = NULL;
	}
    }

  return ret;
}


int
decrement_refcount (struct object *obj, struct object_list **antiloop_hash_t)
{
  int allocated_now = 0;
  struct object_list **hash_t_clone = NULL;
  struct parameter *par;

  if (!obj || obj == &nil_object || obj == &t_object)
    return 0;

  if (HAS_LEAF_TYPE (obj))
    obj->refcount--;
  else
    {
      if (!antiloop_hash_t)
	{
	  antiloop_hash_t = alloc_empty_hash_table (ANTILOOP_HASH_T_SIZE);
	  allocated_now = 1;
	}

      if (!allocated_now && is_object_in_hash_table (obj, antiloop_hash_t,
						     ANTILOOP_HASH_T_SIZE))
	return 0;

      obj->refcount--;

      prepend_object_to_obj_list
	(obj, &antiloop_hash_t [hash_object (obj, ANTILOOP_HASH_T_SIZE)]);

      if (obj->type & TYPE_PREFIX)
	decrement_refcount (obj->value_ptr.next, antiloop_hash_t);
      else if (obj->type == TYPE_SYMBOL_NAME)
	decrement_refcount (obj->value_ptr.symbol_name->sym, antiloop_hash_t);
      else if (obj->type == TYPE_SYMBOL)
	{
	  if (obj->value_ptr.symbol->value_cell
	      && obj->value_ptr.symbol->function_cell)
	    {
	      hash_t_clone = clone_hash_table (antiloop_hash_t,
					       ANTILOOP_HASH_T_SIZE);
	      decrement_refcount (obj->value_ptr.symbol->value_cell,
				  antiloop_hash_t);
	      decrement_refcount (obj->value_ptr.symbol->function_cell,
				  hash_t_clone);
	    }
	  else
	    {
	      decrement_refcount (obj->value_ptr.symbol->value_cell,
				  antiloop_hash_t);
	      decrement_refcount (obj->value_ptr.symbol->function_cell,
				  antiloop_hash_t);
	    }
	}
      else if (obj->type == TYPE_CONS_PAIR)
	{
	  if (obj->value_ptr.cons_pair->cdr == &nil_object)
	    decrement_refcount (obj->value_ptr.cons_pair->car, antiloop_hash_t);
	  else
	    {
	      hash_t_clone = clone_hash_table (antiloop_hash_t,
					       ANTILOOP_HASH_T_SIZE);
	      decrement_refcount (obj->value_ptr.cons_pair->car,
				  antiloop_hash_t);
	      decrement_refcount (obj->value_ptr.cons_pair->cdr,
				  hash_t_clone);
	    }
	}
      else if (obj->type == TYPE_FUNCTION || obj->type == TYPE_MACRO)
	{
	  if (!obj->value_ptr.function->lambda_list)
	    decrement_refcount (obj->value_ptr.function->body, antiloop_hash_t);
	  else
	    {
	      hash_t_clone = clone_hash_table (antiloop_hash_t,
					       ANTILOOP_HASH_T_SIZE);

	      decrement_refcount (obj->value_ptr.function->body,
				  hash_t_clone);

	      par = obj->value_ptr.function->lambda_list;

	      while (par)
		{
		  free_hash_table (hash_t_clone, ANTILOOP_HASH_T_SIZE);

		  hash_t_clone = clone_hash_table (antiloop_hash_t,
						   ANTILOOP_HASH_T_SIZE);

		  decrement_refcount (par->name, hash_t_clone);

		  if (par->init_form)
		    {
		      free_hash_table (hash_t_clone, ANTILOOP_HASH_T_SIZE);

		      hash_t_clone = clone_hash_table (antiloop_hash_t,
						       ANTILOOP_HASH_T_SIZE);

		      decrement_refcount (par->init_form, hash_t_clone);
		    }

		  if (par->supplied_p_param)
		    {
		      free_hash_table (hash_t_clone, ANTILOOP_HASH_T_SIZE);

		      hash_t_clone = clone_hash_table (antiloop_hash_t,
						       ANTILOOP_HASH_T_SIZE);

		      decrement_refcount (par->supplied_p_param, hash_t_clone);
		    }

		  par = par->next;
		}
	    }
	}

      if (allocated_now)
	free_hash_table (antiloop_hash_t, ANTILOOP_HASH_T_SIZE);

      if (hash_t_clone)
	free_hash_table (hash_t_clone, ANTILOOP_HASH_T_SIZE);
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
  if (!obj)
    return;

  if (obj->type == TYPE_STRING)
    free_string (obj);
  else if (obj->type == TYPE_SYMBOL_NAME)
    free_symbol_name (obj);
  else if (obj->type == TYPE_SYMBOL && !obj->value_ptr.symbol->is_const
	   && !obj->value_ptr.symbol->is_parameter
	   && !obj->value_ptr.symbol->is_special)
    free_symbol (obj);
  else if (obj->type & TYPE_PREFIX)
    free (obj);
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
  else if (obj->type == TYPE_INTEGER)
    free_integer (obj);
  else if (obj->type == TYPE_RATIO)
    free_ratio (obj);
  else if (obj->type == TYPE_FLOAT)
    free_float (obj);
  else if (obj->type == TYPE_FUNCTION || obj->type == TYPE_MACRO)
    free_function_or_macro (obj);
  else if (obj->type == TYPE_STREAM)
    {
      if (obj->value_ptr.stream->is_open)
	fclose (obj->value_ptr.stream->file);

      free (obj->value_ptr.stream);
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
free_symbol_name (struct object *obj)
{
  struct symbol_name *s = obj->value_ptr.symbol_name;

  free (s->value);

  if (s->packname_present)
    free (s->actual_symname);

  free (s);
  free (obj);
}


void
free_symbol (struct object *obj)
{
  struct object *p = obj->value_ptr.symbol->home_package;
  struct symbol *s = obj->value_ptr.symbol;

  if (p)
    unintern_symbol (obj);

  free (s->name);
  free (s);
  free (obj);
}


void
free_cons_pair (struct object *obj)
{
  free (obj->value_ptr.cons_pair);
  free (obj);
}


void
free_array_size (struct array_size *size)
{
  struct array_size *next = size->next;

  free (size);

  if (next)
    free_array_size (next);
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
free_integer (struct object *obj)
{
  mpz_clear (obj->value_ptr.integer);
  free (obj);
}


void
free_ratio (struct object *obj)
{
  mpq_clear (obj->value_ptr.ratio);
  free (obj);
}


void
free_float (struct object *obj)
{
  mpf_clear (obj->value_ptr.floating);
  free (obj);
}


void
free_function_or_macro (struct object *obj)
{
  struct parameter *n, *l = obj->value_ptr.function->lambda_list;

  while (l)
    {
      n = l->next;
      free (l);
      l = n;
    }

  free (obj->value_ptr.function);
  free (obj);
}


void
free_list_structure (struct object *list)
{
  if (list->type == TYPE_CONS_PAIR)
    {
      free_list_structure (CDR (list));
      free_cons_pair (list);
    }
}


void
print_welcome_message (void)
{
  puts ("al Copyright (C) 2022 Andrea G. Monaco\n"
	"This program comes with ABSOLUTELY NO WARRANTY; for details type "
	"`(al-print-no-warranty)'.\n"
	"This is free software, and you are welcome to redistribute it\n"
	"under certain conditions; type `(al-print-terms-and-conditions)' for "
	"details.\n");
}


void
print_version (void)
{
  puts ("al " PACKAGE_VERSION "\n"
	"Copyright (C) 2022 Andrea G. Monaco\n"
	"License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/"
	"gpl.html>\n"
	"This is free software: you are free to change and redistribute it.\n"
	"There is NO WARRANTY, to the extent permitted by law.");
}


void
print_help (void)
{
  puts ("Usage: al [OPTIONS] [FILE]\n\n"
	"  If a FILE is provided, load it and then exit\n\n"
	"  -l FILE              load FILE and then start a REPL\n"
	"  -q, --dont-load-cl   don't load cl.lisp at startup\n"
	"  -h, --help           display this help and exit\n"
	"  -v, --version        display version information and exit");
}
