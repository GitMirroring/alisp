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
#include <limits.h>
#include <errno.h>

#include <gmp.h>

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif


#ifndef HAVE_MEMMEM
#define memmem al_memmem
#endif


#define CAR(list) (SYMBOL (list) == &nil_object ? &nil_object :	\
		   (list)->value_ptr.cons_pair->car)

#define CDR(list) (SYMBOL (list) == &nil_object ? &nil_object :		\
		   (list)->value_ptr.cons_pair->cdr ?			\
		   (list)->value_ptr.cons_pair->cdr : &nil_object)

#define IS_SEQUENCE(s) (SYMBOL (s) == &nil_object || (s)->type == TYPE_CONS_PAIR \
			|| (s)->type == TYPE_STRING			\
			|| ((s)->type == TYPE_ARRAY			\
			    && (s)->value_ptr.array->alloc_size		\
			    && !(s)->value_ptr.array->alloc_size->next))

#define IS_LIST(s) ((s)->type == TYPE_CONS_PAIR || SYMBOL (s) == &nil_object)

#define IS_VECTOR(s) ((s)->type == TYPE_STRING				\
		      || ((s)->type == TYPE_ARRAY			\
			  && (s)->value_ptr.array->alloc_size		\
			  && !(s)->value_ptr.array->alloc_size->next))

#define IS_ARRAY(s) ((s)->type == TYPE_STRING || (s)->type == TYPE_ARRAY)

#define HAS_FILL_POINTER(s) (((s)->type == TYPE_STRING			\
			      && (s)->value_ptr.string->fill_pointer >= 0) \
			     || ((s)->type == TYPE_ARRAY		\
				 && (s)->value_ptr.array->fill_pointer >= 0))

#define IS_SYMBOL(s) ((s)->type == TYPE_SYMBOL || (s)->type == TYPE_SYMBOL_NAME)

#define IS_REAL(s) ((s)->type == TYPE_BIGNUM || (s)->type == TYPE_FIXNUM \
		    || (s)->type == TYPE_RATIO || (s)->type == TYPE_FLOAT)

#define IS_RATIONAL(s) ((s)->type == TYPE_BIGNUM || (s)->type == TYPE_RATIO)

#define SYMBOL(s) ((s)->type == TYPE_SYMBOL ? (s) :			\
		   (s)->type == TYPE_SYMBOL_NAME ?			\
		   (s)->value_ptr.symbol_name->sym :			\
		   NULL)

#define IS_STRING_DESIGNATOR(s) ((s)->type == TYPE_CHARACTER || IS_SYMBOL (s) \
				 || (s)->type == TYPE_STRING)

#define IS_PACKAGE_DESIGNATOR(s) (IS_STRING_DESIGNATOR(s) \
				  || (s)->type == TYPE_PACKAGE)

#define HAS_LEAF_TYPE(obj) ((obj)->type & (TYPE_BIGNUM | TYPE_FIXNUM	\
					   | TYPE_RATIO | TYPE_FLOAT \
					   | TYPE_BYTESPEC | TYPE_STRING \
					   | TYPE_CHARACTER | TYPE_FILENAME))


#define CLEAR_MULTIPLE_OR_NO_VALUES(out)	\
  do						\
    {						\
      (out).no_value = 0;			\
      free_object_list ((out).other_values);	\
      (out).other_values = NULL;		\
    } while (0)


#define increment_refcount(obj) offset_refcount_by ((obj), 1, NULL, 0)

#define decrement_refcount(obj) offset_refcount_by ((obj), -1, NULL, 0)


#define ANTILOOP_HASH_T_SIZE 64



#define TERMINATING_MACRO_CHARS "()';\"`,"



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
    PACKAGE_NOT_FOUND_IN_READ = 1 << 30,
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
		    | MORE_THAN_A_PACKAGE_SEPARATOR | PACKAGE_NOT_FOUND_IN_READ \
		    | PACKAGE_MARKER_IN_SHARP_COLON)




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
eval_outcome_type
  {
    EVAL_OK,
    UNBOUND_SYMBOL,
    INVALID_FUNCTION_CALL,
    UNKNOWN_KEYWORD_ARGUMENT,
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
    INVALID_SIZE,
    COULD_NOT_OPEN_FILE,
    COULD_NOT_OPEN_FILE_FOR_READING,
    COULD_NOT_SEEK_FILE,
    COULD_NOT_TELL_FILE,
    ERROR_READING_FILE,
    UNKNOWN_TYPE,
    INVALID_GO_TAG,
    TAG_NOT_FOUND,
    BLOCK_NOT_FOUND,
    INVALID_ACCESSOR,
    FUNCTION_NOT_FOUND_IN_EVAL,
    DECLARE_NOT_ALLOWED_HERE,
    CANT_DIVIDE_BY_ZERO,
    INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT,
    PACKAGE_NOT_FOUND_IN_EVAL,
    PACKAGE_NAME_OR_NICKNAME_ALREADY_IN_USE
  };


struct
eval_outcome
{
  enum eval_outcome_type type;

  int no_value;
  struct object_list *other_values;

  struct object *obj;

  struct object *block_to_leave;
  struct object *return_value;

  struct object *tag_to_jump_to;
};


struct
object_list
{
  struct object *obj;
  struct object_list *next;
};



/* not a C string. not null-terminated and explicit size. null bytes are
   allowed inside */

struct
string
{
  char *value;
  long alloc_size;
  long used_size;

  long fill_pointer;
};


struct
environment
{
  struct binding *vars;
  int lex_env_vars_boundary;

  struct binding *funcs;
  int lex_env_funcs_boundary;

  struct object_list *packages;
  struct object *cl_package, *keyword_package;

  struct object_list *blocks;

  struct go_tag_frame *go_tag_stack;

  struct binding *structs;

  struct object *c_stdout;

  struct object *package_sym, *std_in_sym, *std_out_sym, *print_escape_sym,
    *print_readably_sym;
};


enum
package_record_visibility
  {
    INTERNAL_VISIBILITY,
    EXTERNAL_VISIBILITY
  };


struct
package_record
{
  enum package_record_visibility visibility;

  struct object *sym;

  struct package_record *next;
};


struct
name_list
{
  char *name;
  long name_len;

  struct name_list *next;
};


#define SYMTABLE_SIZE 2048


struct
package
{
  char *name;
  long name_len;

  struct name_list *nicks;

  struct package_record **symtable;

  struct object_list *uses;
  struct object_list *used_by;
};


struct
symbol
{
  char *name;
  long name_len;

  int is_type;
  int is_standard_type;
  int (*builtin_type) (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct eval_outcome *outcome);
  struct object *typespec;
  struct object_list *parent_types;

  struct object *(*builtin_accessor) (struct object *list,
				      struct object *newval,
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
  long size;

  struct array_size *next;
};


struct
array
{
  struct array_size *alloc_size;

  long fill_pointer;

  struct object **value;
};


#define LISP_HASHTABLE_SIZE 1024

struct
hashtable
{
  struct object **table;
};


struct
filename
{
  struct object *value;
};


enum
stream_medium
  {
    FILE_STREAM,
    STRING_STREAM
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
  enum stream_medium medium;

  enum stream_type type;

  enum stream_direction direction;

  FILE *file;

  struct object *string;

  int is_open;

  int dirty_line;
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


struct
complex
{
  struct object *real;
  struct object *imag;
};


struct
bytespec
{
  mpz_t size;
  mpz_t pos;
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
    TYPE_BIGNUM = 1 << 7,
    TYPE_FIXNUM = 1 << 8,
    TYPE_RATIO = 1 << 9,
    TYPE_FLOAT = 1 << 10,
    TYPE_COMPLEX = 1 << 11,
    TYPE_BYTESPEC = 1 << 12,
    TYPE_CONS_PAIR = 1 << 13,
    TYPE_STRING = 1 << 14,
    TYPE_CHARACTER = 1 << 15,
    TYPE_ARRAY = 1 << 16,
    TYPE_HASHTABLE = 1 << 17,
    TYPE_ENVIRONMENT = 1 << 18,
    TYPE_PACKAGE = 1 << 19,
    TYPE_FILENAME = 1 << 20,
    TYPE_STREAM = 1 << 21,
    TYPE_STRUCTURE = 1 << 22,
    TYPE_CONDITION = 1 << 23,
    TYPE_FUNCTION = 1 << 24,
    TYPE_MACRO = 1 << 25,
    TYPE_SHARP_MACRO_CALL = 1 << 26,
  };


#define TYPE_PREFIX (TYPE_QUOTE | TYPE_BACKQUOTE | TYPE_COMMA | TYPE_AT \
		     | TYPE_DOT)
#define TYPE_REAL (TYPE_BIGNUM | TYPE_FIXNUM | TYPE_RATIO | TYPE_FLOAT)
#define TYPE_NUMBER (TYPE_REAL | TYPE_COMPLEX)


union
object_ptr_union
{
  struct symbol *symbol;
  struct symbol_name *symbol_name;
  struct object *next;
  mpz_t integer;
  long *fixnum;
  mpq_t ratio;
  mpf_t floating;
  struct complex *complex;
  struct bytespec *bytespec;
  struct cons_pair *cons_pair;
  struct string *string;
  char *character;
  struct array *array;
  struct hashtable *hashtable;
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
refcounted_object_list
{
  int refc;

  struct object *obj;

  struct refcounted_object_list *next;
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
char *generate_prompt (struct environment *env);

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
void free_object_list_structure (struct object_list *list);

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
 enum readtable_case read_case, int escape_first_char);

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
struct object *alloc_complex (void);
struct object *create_integer_from_long (long num);
struct object *create_floating_from_double (double d);

void print_range (const char *begin, const char *end);

char *append_newline (char *string);
char *append_zero_byte (char *string, size_t size);
char *copy_token_to_buffer (const char *input, size_t size);

size_t utf8len (const char *string);
size_t next_utf8_char (char *str, size_t sz);
size_t char_vector_utf8_length (const char *str, size_t sz);
size_t string_utf8_length (const struct object *str);

void *malloc_and_check (size_t size);
void *realloc_and_check (void *ptr, size_t size);
void *calloc_and_check (size_t nmemb, size_t size);

struct object *alloc_object (void);
struct object *alloc_prefix (enum element type);
struct object *alloc_empty_cons_pair (void);
struct object *alloc_empty_list (size_t sz);
struct object *alloc_function (void);
struct object *alloc_sharp_macro_call (void);
struct object *alloc_bytespec (void);

struct package_record **alloc_empty_symtable (size_t table_size);
struct object *create_package (char *name, int name_len);
struct object *create_package_from_c_strings (char *name, ...);
void free_name_list (struct name_list *list);
struct package_record *inspect_accessible_symbol (char *name, size_t len,
						  struct object *pack,
						  int *is_inherited);
struct object *inspect_package_by_designator (struct object *des,
					      struct environment *env);
struct object *find_package (const char *name, size_t len,
			     struct environment *env);
struct package_record *find_package_entry (struct object *symbol,
					   struct package_record **symtable,
					   struct package_record **prev);
int use_package (struct object *used, struct object *pack,
		 struct object **conflicting);

int hash_object (const struct object *object, size_t table_size);
int hash_char_vector (const char *str, size_t sz, size_t table_size);
int hash_symbol_name (const struct object *symname, size_t table_size);
int hash_symbol (const struct object *sym, size_t table_size);

struct object_list **alloc_empty_hash_table (size_t table_size);
int is_object_in_hash_table (const struct object *object,
			     struct object_list **hash_table,
			     size_t table_size);
void free_hash_table (struct object_list **hash_table, size_t table_size);

struct refcounted_object_list **alloc_empty_tailsharing_hash_table
(size_t table_size);
int is_object_in_refcounted_obj_list (const struct object *obj,
				      const struct refcounted_object_list *list);
void prepend_object_to_refcounted_obj_list (struct object *obj,
					    struct refcounted_object_list **list);
struct refcounted_object_list **clone_tailsharing_hash_table
(struct refcounted_object_list **hash_table, size_t table_size);
void free_tailsharing_hash_table (struct refcounted_object_list **hash_table,
				  size_t table_size);

void clone_lexical_environment (struct binding **lex_vars,
				struct binding **lex_funcs, struct binding *vars,
				int var_num, struct binding *funcs, int func_num);

struct object *create_function (struct object *lambda_list, struct object *body,
				struct environment *env,
				struct eval_outcome *outcome, int is_macro);

const char *find_end_of_string
(const char *input, size_t size, size_t *new_size, size_t *string_length);
void normalize_string (char *output, const char *input, size_t size);

struct object *alloc_string (size_t size);
struct object *create_string_from_char_vector (const char *str, size_t size,
					       int do_copy);
struct object *create_string_from_c_string (const char *str);
void resize_string_allocation (struct object *string, size_t size);
char *copy_string_to_c_string (struct string *str);

struct object *alloc_symbol_name (size_t value_s, size_t actual_symname_s);
void resize_symbol_name (struct object *symname, size_t value_s,
			 size_t actual_symname_s);

const char *find_end_of_symbol_name
(const char *input, size_t size, int found_package_sep, size_t *new_size,
 const char **start_of_package_separator,
 enum package_record_visibility *sym_visibility, size_t *name_length,
 size_t *act_name_length, enum read_outcome *outcome, int escape_first_char);
void copy_symname_with_case_conversion (char *output, const char *input,
					size_t size,
					enum readtable_case read_case,
					int escape_first_char);

struct object *create_symbol (char *name, size_t size, int do_copy);
struct object *create_filename (struct object *string);

struct object *alloc_vector (size_t size, int fill_with_nil,
			     int dont_store_size);
struct object *create_vector (struct object *list);
void resize_vector (struct object *vector, size_t size);

struct object *create_character (char *character, int do_copy);
struct object *create_character_from_utf8 (char *character, size_t size);
struct object *create_character_from_char (char ch);
struct object *get_nth_character (struct object *str, int ind);
struct object *set_nth_character (struct object *str, int ind, char *ch);

struct object *create_file_stream (enum stream_type type,
				   enum stream_direction direction,
				   struct string *filename,
				   struct eval_outcome *outcome);
struct object *create_stream_from_open_file (enum stream_type type,
					     enum stream_direction direction,
					     FILE *file);
struct object *create_string_stream (enum stream_direction direction,
				     struct object *instr);

struct object *load_file (const char *filename, struct environment *env,
			  struct eval_outcome *outcome);

struct object *intern_symbol_from_char_vector (char *name, size_t len,
					       int do_copy,
					       enum package_record_visibility vis,
					       int always_create_if_missing,
					       struct object *package);
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

struct go_tag *collect_go_tags (struct object *body);
struct go_tag_frame *add_go_tag_frame (struct go_tag_frame *stack);
struct go_tag *add_go_tag (struct object *tagname, struct object *tagdest,
			   struct go_tag *tags);
struct go_tag_frame *remove_go_tag_frame (struct go_tag_frame *stack);
struct go_tag *find_go_tag (struct object *tagname, struct go_tag_frame *frame);

struct object_list *add_block (struct object *name, struct object_list *blocks);
struct object_list *remove_block (struct object_list *blocks);

void add_builtin_type (char *name, struct environment *env,
		       int (*builtin_type)
		       (const struct object *obj, const struct object *typespec,
			struct environment *env, struct eval_outcome *outcome),
		       int is_standard, ...);
struct object *add_builtin_form (char *name, struct environment *env,
				 struct object *(*builtin_form)
				 (struct object *list, struct environment *env,
				  struct eval_outcome *outcome),
				 enum object_type type,
				  struct object *(*builtin_accessor)
				 (struct object *list, struct object *newval,
				  struct environment *env,
				  struct eval_outcome *outcome),
				 int is_special_operator);

struct object *define_constant
(struct object *sym, struct object *form, struct environment *env,
 struct eval_outcome *outcome);
struct object *define_parameter
(struct object *sym, struct object *form, struct environment *env,
 struct eval_outcome *outcome);

struct object *define_constant_by_name (char *name, struct object *value,
					struct environment *env);
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
int is_dotted_or_circular_list (struct object *list, int *is_circular);
int is_proper_list (struct object *list);

struct object *copy_prefix (const struct object *begin, const struct object *end,
			    struct object **last_prefix, int refcount);
struct object *copy_list_structure (struct object *list,
				    const struct object *prefix, int cell_num,
				    struct object **last_cell);

int array_rank (const struct array *array);
size_t array_total_size (const struct array *array);

int hash_object_respecting_eq (const struct object *object, size_t table_size);
int hash_table_count (const struct hashtable *hasht);

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
(struct object *body, int is_tagbody, struct object *block_name,
 struct environment *env, struct eval_outcome *outcome);
struct object *call_function
(struct object *func, struct object *arglist, int eval_args, int is_macro,
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
struct object *apply_backquote (struct object *form, int backts_commas_balance,
				struct environment *env,
				struct eval_outcome *outcome,
				int forbid_splicing, int *do_splice,
				struct object **last_pref);
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
int type_atom (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome);  
int type_list (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome);
int type_symbol (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome);
int type_keyword (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct eval_outcome *outcome);
int type_boolean (const struct object *obj, const struct object *typespec,
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
int type_bignum (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome);
int type_fixnum (const struct object *obj, const struct object *typespec,
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
int type_complex (const struct object *obj, const struct object *typespec,
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
int type_hash_table (const struct object *obj, const struct object *typespec,
		     struct environment *env, struct eval_outcome *outcome);
int type_pathname (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct eval_outcome *outcome);
int type_stream (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome);
int type_file_stream (const struct object *obj, const struct object *typespec,
		      struct environment *env, struct eval_outcome *outcome);
int type_string_stream (const struct object *obj, const struct object *typespec,
			struct environment *env, struct eval_outcome *outcome);

struct object *builtin_car
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_cdr
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_rplaca
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_rplacd
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_cons
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_list
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_list_star
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_append
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_nconc
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_nth
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_nthcdr
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_nth_value
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_elt
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_aref
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_row_major_aref
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_list_length
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_length
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_fill_pointer
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_make_array
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_array_has_fill_pointer_p
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_array_dimensions
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_array_row_major_index
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_make_hash_table
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_hash_table_size
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_hash_table_count
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_hash_table_test
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_gethash
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_remhash
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_last
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_read_line
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_eval
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_write
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_write_string
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_write_char
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_write_byte
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_fresh_line
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_load
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_open
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_close
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_open_stream_p
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_input_stream_p
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_output_stream_p
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_interactive_stream_p
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_make_string_input_stream
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_make_string_output_stream
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_get_output_stream_string
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_upper_case_p
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_lower_case_p
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_both_case_p
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_eq
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_eql
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
struct object *builtin_remove_if
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *builtin_reverse
(struct object *list, struct environment *env, struct eval_outcome *outcome);

struct object *accessor_car (struct object *list, struct object *newval,
			     struct environment *env,
			     struct eval_outcome *outcome);
struct object *accessor_cdr (struct object *list, struct object *newval,
			     struct environment *env,
			     struct eval_outcome *outcome);
struct object *accessor_aref (struct object *list, struct object *newval,
			      struct environment *env,
			      struct eval_outcome *outcome);
struct object *accessor_elt (struct object *list, struct object *newval,
			     struct environment *env,
			     struct eval_outcome *outcome);
struct object *accessor_gethash (struct object *list, struct object *newval,
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
 enum object_type quotient_type, struct eval_outcome *outcome);

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
struct object *builtin_ffloor (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);
struct object *builtin_ceiling (struct object *list, struct environment *env,
				struct eval_outcome *outcome);
struct object *builtin_fceiling (struct object *list, struct environment *env,
				struct eval_outcome *outcome);
struct object *builtin_truncate (struct object *list, struct environment *env,
				 struct eval_outcome *outcome);
struct object *builtin_ftruncate (struct object *list, struct environment *env,
				 struct eval_outcome *outcome);
struct object *builtin_round (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);
struct object *builtin_fround (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);
struct object *builtin_numerator (struct object *list, struct environment *env,
				  struct eval_outcome *outcome);
struct object *builtin_denominator (struct object *list, struct environment *env,
				    struct eval_outcome *outcome);
struct object *builtin_sqrt (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);
struct object *builtin_complex (struct object *list, struct environment *env,
				struct eval_outcome *outcome);
struct object *builtin_realpart (struct object *list, struct environment *env,
				 struct eval_outcome *outcome);
struct object *builtin_imagpart (struct object *list, struct environment *env,
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
struct object *builtin_min (struct object *list, struct environment *env,
			    struct eval_outcome *outcome);
struct object *builtin_max (struct object *list, struct environment *env,
			    struct eval_outcome *outcome);

struct object *builtin_byte (struct object *list, struct environment *env,
			     struct eval_outcome *outcome);
struct object *builtin_byte_size (struct object *list, struct environment *env,
				  struct eval_outcome *outcome);
struct object *builtin_byte_position (struct object *list,
				      struct environment *env,
				      struct eval_outcome *outcome);

struct object *builtin_typep (struct object *list, struct environment *env,
			      struct eval_outcome *outcome);

struct object *builtin_make_string (struct object *list, struct environment *env,
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
struct object *builtin_symbol_name (struct object *list, struct environment *env,
				    struct eval_outcome *outcome);
struct object *builtin_symbol_package (struct object *list,
				       struct environment *env,
				       struct eval_outcome *outcome);
struct object *builtin_special_operator_p (struct object *list,
					   struct environment *env,
					   struct eval_outcome *outcome);
struct object *builtin_string (struct object *list, struct environment *env,
			       struct eval_outcome *outcome);
struct object *builtin_string_eq (struct object *list, struct environment *env,
				  struct eval_outcome *outcome);
struct object *builtin_char_eq (struct object *list, struct environment *env,
				struct eval_outcome *outcome);
struct object *builtin_char_upcase (struct object *list, struct environment *env,
				    struct eval_outcome *outcome);
struct object *builtin_char_downcase (struct object *list,
				      struct environment *env,
				      struct eval_outcome *outcome);
struct object *builtin_alpha_char_p (struct object *list,
				     struct environment *env,
				     struct eval_outcome *outcome);
struct object *builtin_alphanumericp (struct object *list,
				      struct environment *env,
				      struct eval_outcome *outcome);
struct object *builtin_char_code (struct object *list,
				  struct environment *env,
				  struct eval_outcome *outcome);
struct object *builtin_find_package (struct object *list,
				     struct environment *env,
				     struct eval_outcome *outcome);
struct object *builtin_package_name (struct object *list,
				     struct environment *env,
				     struct eval_outcome *outcome);
struct object *builtin_package_nicknames (struct object *list,
					  struct environment *env,
					  struct eval_outcome *outcome);
struct object *builtin_rename_package (struct object *list,
				       struct environment *env,
				       struct eval_outcome *outcome);
struct object *builtin_package_use_list (struct object *list,
					 struct environment *env,
					 struct eval_outcome *outcome);
struct object *builtin_package_used_by_list (struct object *list,
					     struct environment *env,
					     struct eval_outcome *outcome);
struct object *builtin_list_all_packages (struct object *list,
					  struct environment *env,
					  struct eval_outcome *outcome);
struct object *builtin_make_package (struct object *list,
				     struct environment *env,
				     struct eval_outcome *outcome);
struct object *builtin_in_package (struct object *list, struct environment *env,
				   struct eval_outcome *outcome);
struct object *builtin_lisp_implementation_type (struct object *list,
						 struct environment *env,
						 struct eval_outcome *outcome);
struct object *builtin_lisp_implementation_version (struct object *list,
						    struct environment *env,
						    struct eval_outcome *outcome);
struct object *builtin_software_type (struct object *list,
				      struct environment *env,
				      struct eval_outcome *outcome);
struct object *builtin_software_version (struct object *list,
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

struct object *inspect_variable_from_c_string (const char *var,
					       struct environment *env);
struct object *inspect_variable (struct object *sym, struct environment *env);

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
struct object *evaluate_values_list
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_multiple_value_list
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
struct object *evaluate_funcall
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_declare
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_prog1
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_prog2
(struct object *list, struct environment *env, struct eval_outcome *outcome);

struct object *evaluate_tagbody
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_go
(struct object *list, struct environment *env, struct eval_outcome *outcome);

struct object *evaluate_block
(struct object *list, struct environment *env, struct eval_outcome *outcome);
struct object *evaluate_return_from
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

struct object *fresh_line (struct stream *str);

int is_printer_escaping_enabled (struct environment *env);

int write_to_stream (struct stream *stream, const char *str, size_t size);
int write_char_to_stream (struct stream *stream, char ch);
int write_long_to_stream (struct stream *stream, long z);

int print_as_symbol (const char *sym, size_t len, int print_escapes,
		     struct stream *str);
int print_symbol_name (const struct symbol_name *sym, struct environment *env,
		       struct stream *str);
int print_symbol (const struct symbol *sym, struct environment *env,
		  struct stream *str);
int print_bignum (const mpz_t z, struct environment *env, struct stream *str);
int print_floating (const mpf_t f, struct environment *env, struct stream *str);
int print_complex (const struct complex *c, struct environment *env,
		   struct stream *str);
int print_bytespec (const struct bytespec *bs, struct environment *env,
		    struct stream *str);
int print_string (const struct string *st, struct environment *env,
		  struct stream *str);
int print_character (const char *character, struct environment *env,
		     struct stream *str);
int print_filename (const struct filename *fn, struct environment *env,
		    struct stream *str);
int print_list (const struct cons_pair *list, struct environment *env,
		struct stream *str);
int print_array (const struct array *array, struct environment *env,
		 struct stream *str);
int print_function_or_macro (const struct object *obj, struct environment *env,
			     struct stream *str);
int print_object (const struct object *obj, struct environment *env,
		  struct stream *str);

void print_read_error (enum read_outcome err, const char *input, size_t size,
		       const char *begin, const char *end,
		       const struct read_outcome_args *args);
void print_eval_error (struct eval_outcome *err, struct environment *env);

void increment_refcount_by (struct object *obj, int count, struct object *parent);
int decrement_refcount_by (struct object *obj, int count, struct object *parent);
int offset_refcount_by (struct object *obj, int delta,
			struct refcounted_object_list **antiloop_hash_t,
			int clone_hash_t);

void free_object (struct object *obj);
void free_string (struct object *obj);
void free_symbol_name (struct object *obj);
void free_symbol (struct object *obj);
void free_cons_pair (struct object *obj);
void free_array_size (struct array_size *size);
void free_array (struct object *obj);
void free_hashtable (struct object *obj);
void free_integer (struct object *obj);
void free_ratio (struct object *obj);
void free_float (struct object *obj);
void free_bytespec (struct object *obj);
void free_function_or_macro (struct object *obj);

void free_sharp_macro_call (struct object *macro);
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

  struct object *result, *obj, *c_stdout;
  struct object_list *vals;
  struct environment env = {NULL};

  struct eval_outcome eval_out = {EVAL_OK};

  const char *input_left = NULL;
  size_t input_left_s = 0;

  struct command_line_options opts = {1};


  parse_command_line (&opts, argc, argv);

  add_standard_definitions (&env);

  c_stdout = env.c_stdout;

  if (!opts.load_and_exit)
    print_welcome_message ();

  if (opts.load_cl)
    {
      if (!opts.load_and_exit)
	printf ("Loading cl.lisp... ");

      result = load_file ("cl.lisp", &env, &eval_out);

      if (result && !opts.load_and_exit)
	print_object (result, &env, c_stdout->value_ptr.stream);
      else if (!opts.load_and_exit)
	print_eval_error (&eval_out, &env);

      eval_out.type = EVAL_OK;

      if (!opts.load_and_exit)
	{
	  printf ("\n");
	  c_stdout->value_ptr.stream->dirty_line = 0;
	}
    }

  if (opts.load_before_repl)
    {
      if (!opts.load_and_exit)
	printf ("Loading %s...\n", opts.load_and_exit);

      result = load_file (opts.load_before_repl, &env, &eval_out);

      if (result && !opts.load_and_exit)
	print_object (result, &env, c_stdout->value_ptr.stream);
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

	  if (!result && eval_out.tag_to_jump_to)
	    {
	      eval_out.type = TAG_NOT_FOUND;
	      eval_out.tag_to_jump_to = NULL;
	    }
	  else if (!result && eval_out.block_to_leave)
	    {
	      eval_out.type = BLOCK_NOT_FOUND;
	      eval_out.block_to_leave = NULL;
	    }

	  if (!result)
	    print_eval_error (&eval_out, &env);
	  else if (eval_out.no_value)
	    eval_out.no_value = 0;
	  else
	    {
	      fresh_line (c_stdout->value_ptr.stream);

	      print_object (result, &env, c_stdout->value_ptr.stream);
	      printf ("\n");
	      c_stdout->value_ptr.stream->dirty_line = 0;

	      vals = eval_out.other_values;

	      while (vals)
		{
		  print_object (vals->obj, &env, c_stdout->value_ptr.stream);
		  printf ("\n");
		  c_stdout->value_ptr.stream->dirty_line = 0;
		  vals = vals->next;
		}

	      free_object_list (eval_out.other_values);

	      eval_out.other_values = NULL;
	    }

	  decrement_refcount (result);
	  decrement_refcount (obj);

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
  struct package_record *rec;

  env->keyword_package = create_package_from_c_strings ("KEYWORD", (char *)NULL);
  prepend_object_to_obj_list (env->keyword_package, &env->packages);

  env->cl_package = create_package_from_c_strings ("COMMON-LISP", "CL",
						   (char *)NULL);
  prepend_object_to_obj_list (env->cl_package, &env->packages);

  cluser_package = create_package_from_c_strings ("COMMON-LISP-USER", "CL-USER",
						  (char *)NULL);
  prepend_object_to_obj_list (cluser_package, &env->packages);

  use_package (env->cl_package, cluser_package, NULL);

  t_symbol.value_cell = &t_object;
  t_symbol.home_package = env->cl_package;

  nil_symbol.value_cell = &nil_object;
  nil_symbol.home_package = env->cl_package;


  rec = malloc_and_check (sizeof (*rec));
  rec->visibility = EXTERNAL_VISIBILITY;
  rec->sym = &t_object;
  rec->next = NULL;
  env->cl_package->value_ptr.package->symtable
    [hash_char_vector ("T", sizeof ("T"), SYMTABLE_SIZE)] = rec;

  rec = malloc_and_check (sizeof (*rec));
  rec->visibility = EXTERNAL_VISIBILITY;
  rec->sym = &nil_object;
  rec->next = NULL;
  env->cl_package->value_ptr.package->symtable
    [hash_char_vector ("NIL", sizeof ("NIL"), SYMTABLE_SIZE)] = rec;


  env->package_sym = intern_symbol_from_char_vector ("*PACKAGE*",
						     strlen ("*PACKAGE*"), 1,
						     EXTERNAL_VISIBILITY, 1,
						     env->cl_package);
  env->package_sym->value_ptr.symbol->is_parameter = 1;
  env->package_sym->value_ptr.symbol->value_cell = env->cl_package;


  add_builtin_form ("CAR", env, builtin_car, TYPE_FUNCTION, accessor_car, 0);
  add_builtin_form ("CDR", env, builtin_cdr, TYPE_FUNCTION, accessor_cdr, 0);
  add_builtin_form ("RPLACA", env, builtin_rplaca, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("RPLACD", env, builtin_rplacd, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CONS", env, builtin_cons, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LIST", env, builtin_list, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LIST*", env, builtin_list_star, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("APPEND", env, builtin_append, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NCONC", env, builtin_nconc, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NTH", env, builtin_nth, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NTHCDR", env, builtin_nthcdr, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NTH-VALUE", env, builtin_nth_value, TYPE_MACRO, NULL, 0);
  add_builtin_form ("ELT", env, builtin_elt, TYPE_FUNCTION, accessor_elt, 0);
  add_builtin_form ("AREF", env, builtin_aref, TYPE_FUNCTION, accessor_aref, 0);
  add_builtin_form ("ROW-MAJOR-AREF", env, builtin_row_major_aref, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("LIST-LENGTH", env, builtin_list_length, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("LENGTH", env, builtin_length, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FILL-POINTER", env, builtin_fill_pointer, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("MAKE-ARRAY", env, builtin_make_array, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("ARRAY-HAS-FILL-POINTER-P", env,
		    builtin_array_has_fill_pointer_p, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ARRAY-DIMENSIONS", env, builtin_array_dimensions,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ARRAY-ROW-MAJOR-INDEX", env, builtin_array_row_major_index,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAKE-HASH-TABLE", env, builtin_make_hash_table,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("HASH-TABLE-SIZE", env, builtin_hash_table_size,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("HASH-TABLE-COUNT", env, builtin_hash_table_count,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("HASH-TABLE-TEST", env, builtin_hash_table_test,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("GETHASH", env, builtin_gethash, TYPE_FUNCTION,
		    accessor_gethash, 0);
  add_builtin_form ("REMHASH", env, builtin_remhash, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LAST", env, builtin_last, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("READ-LINE", env, builtin_read_line, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("EVAL", env, builtin_eval, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("WRITE", env, builtin_write, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("WRITE-STRING", env, builtin_write_string, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("WRITE-CHAR", env, builtin_write_char, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("WRITE-BYTE", env, builtin_write_byte, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("FRESH-LINE", env, builtin_fresh_line, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("LOAD", env, builtin_load, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("OPEN", env, builtin_open, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CLOSE", env, builtin_close, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("OPEN-STREAM-P", env, builtin_open_stream_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("INPUT-STREAM-P", env, builtin_input_stream_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("OUTPUT-STREAM-P", env, builtin_output_stream_p,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("INTERACTIVE-STREAM-P", env, builtin_interactive_stream_p,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAKE-STRING-INPUT-STREAM", env,
		    builtin_make_string_input_stream, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAKE-STRING-OUTPUT-STREAM", env,
		    builtin_make_string_output_stream, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("GET-OUTPUT-STREAM-STRING", env,
		    builtin_get_output_stream_string, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("UPPER-CASE-P", env, builtin_upper_case_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("LOWER-CASE-P", env, builtin_lower_case_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("BOTH-CASE-P", env, builtin_both_case_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("EQ", env, builtin_eq, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("EQL", env, builtin_eql, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NOT", env, builtin_not, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NULL", env, builtin_not, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CONCATENATE", env, builtin_concatenate, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("DOTIMES", env, builtin_dotimes, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DOLIST", env, builtin_dolist, TYPE_MACRO, NULL, 0);
  add_builtin_form ("MAPCAR", env, builtin_mapcar, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("REMOVE-IF", env, builtin_remove_if, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("REVERSE", env, builtin_reverse, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("+", env, builtin_plus, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("-", env, builtin_minus, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("*", env, builtin_multiply, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("/", env, builtin_divide, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FLOOR", env, builtin_floor, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FFLOOR", env, builtin_ffloor, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CEILING", env, builtin_ceiling, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FCEILING", env, builtin_fceiling, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("TRUNCATE", env, builtin_truncate, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FTRUNCATE", env, builtin_ftruncate, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ROUND", env, builtin_round, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FROUND", env, builtin_fround, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NUMERATOR", env, builtin_numerator, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DENOMINATOR", env, builtin_denominator, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("SQRT", env, builtin_sqrt, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("COMPLEX", env, builtin_complex, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("REALPART", env, builtin_realpart, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("IMAGPART", env, builtin_imagpart, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("=", env, builtin_numbers_equal, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("/=", env, builtin_numbers_different, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("<", env, builtin_numbers_less_than, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("<=", env, builtin_numbers_less_than_or_equal,TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form (">", env, builtin_numbers_more_than, TYPE_FUNCTION, NULL, 0);
  add_builtin_form (">=", env, builtin_numbers_more_than_or_equal, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("MIN", env, builtin_min, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAX", env, builtin_max, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("QUOTE", env, evaluate_quote, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LET", env, evaluate_let, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LET*", env, evaluate_let_star, TYPE_MACRO, NULL, 1);
  add_builtin_form ("FLET", env, evaluate_flet, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LABELS", env, evaluate_labels, TYPE_MACRO, NULL, 1);
  add_builtin_form ("MACROLET", env, evaluate_macrolet, TYPE_MACRO, NULL, 1);
  add_builtin_form ("IF", env, evaluate_if, TYPE_MACRO, NULL, 1);
  add_builtin_form ("PROGN", env, evaluate_progn, TYPE_MACRO, NULL, 1);
  add_builtin_form ("VALUES", env, evaluate_values, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("VALUES-LIST", env, evaluate_values_list, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("MULTIPLE-VALUE-LIST", env, evaluate_multiple_value_list,
		    TYPE_MACRO, NULL, 0);
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
  add_builtin_form ("FUNCALL", env, evaluate_funcall, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DECLARE", env, evaluate_declare, TYPE_MACRO, NULL, 0);
  add_builtin_form ("PROG1", env, evaluate_prog1, TYPE_MACRO, NULL, 0);
  add_builtin_form ("PROG2", env, evaluate_prog2, TYPE_MACRO, NULL, 0);
  add_builtin_form ("TAGBODY", env, evaluate_tagbody, TYPE_MACRO, NULL, 1);
  add_builtin_form ("GO", env, evaluate_go, TYPE_MACRO, NULL, 1);
  add_builtin_form ("BLOCK", env, evaluate_block, TYPE_MACRO, NULL, 1);
  add_builtin_form ("RETURN-FROM", env, evaluate_return_from, TYPE_MACRO, NULL,
		    1);
  add_builtin_form ("TYPEP", env, builtin_typep, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("BYTE", env, builtin_byte, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("BYTE-SIZE", env, builtin_byte_size, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("BYTE-POSITION", env, builtin_byte_position, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("MAKE-STRING", env, builtin_make_string, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("MAKE-SYMBOL", env, builtin_make_symbol, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("BOUNDP", env, builtin_boundp, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SYMBOL-VALUE", env, builtin_symbol_value, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("FBOUNDP", env, builtin_fboundp, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SYMBOL-FUNCTION", env, builtin_symbol_function,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SYMBOL-NAME", env, builtin_symbol_name, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("SYMBOL-PACKAGE", env, builtin_symbol_package, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("SPECIAL-OPERATOR-P", env, builtin_special_operator_p,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("STRING", env, builtin_string, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("STRING=", env, builtin_string_eq, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CHAR=", env, builtin_char_eq, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CHAR-UPCASE", env, builtin_char_upcase, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("CHAR-DOWNCASE", env, builtin_char_downcase, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("ALPHA-CHAR-P", env, builtin_alpha_char_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("ALPHANUMERICP", env, builtin_alphanumericp, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("CHAR-CODE", env, builtin_char_code, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FIND-PACKAGE", env, builtin_find_package, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("PACKAGE-NAME", env, builtin_package_name, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("PACKAGE-NICKNAMES", env, builtin_package_nicknames,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("RENAME-PACKAGE", env, builtin_rename_package, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("PACKAGE-USE-LIST", env, builtin_package_use_list,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("PACKAGE-USED-BY-LIST", env, builtin_package_used_by_list,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LIST-ALL-PACKAGES", env, builtin_list_all_packages,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAKE-PACKAGE", env, builtin_make_package, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("IN-PACKAGE", env, builtin_in_package, TYPE_MACRO, NULL,
		    0);
  add_builtin_form ("LISP-IMPLEMENTATION-TYPE", env,
		    builtin_lisp_implementation_type, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LISP-IMPLEMENTATION-VERSION", env,
		    builtin_lisp_implementation_version, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SOFTWARE-TYPE", env, builtin_software_type, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("SOFTWARE-VERSION", env, builtin_software_version,
		    TYPE_FUNCTION, NULL, 0);

  add_builtin_type ("T", env, type_t, 1, (char *)NULL);
  add_builtin_type ("NIL", env, type_nil, 1, (char *)NULL);
  add_builtin_type ("SYMBOL", env, type_symbol, 1, (char *)NULL);
  add_builtin_type ("KEYWORD", env, type_keyword, 1, "SYMBOL", (char *)NULL);
  add_builtin_type ("BOOLEAN", env, type_boolean, 1, "SYMBOL", (char *)NULL);
  add_builtin_type ("FUNCTION", env, type_function, 1, (char *)NULL);
  add_builtin_type ("PACKAGE", env, type_package, 1, (char *)NULL);
  add_builtin_type ("NUMBER", env, type_number, 1, (char *)NULL);
  add_builtin_type ("REAL", env, type_real, 1, "NUMBER", (char *)NULL);
  add_builtin_type ("RATIONAL", env, type_rational, 1, "REAL", (char *)NULL);
  add_builtin_type ("INTEGER", env, type_integer, 1, "RATIONAL", (char *)NULL);
  add_builtin_type ("BIGNUM", env, type_bignum, 1, "INTEGER", (char *)NULL);
  add_builtin_type ("FIXNUM", env, type_fixnum, 1, "INTEGER", (char *)NULL);
  add_builtin_type ("RATIO", env, type_ratio, 1, "RATIONAL", (char *)NULL);
  add_builtin_type ("FLOAT", env, type_float, 1, (char *)NULL);
  add_builtin_type ("SHORT-FLOAT", env, type_short_float, 1, "SINGLE-FLOAT",
		    (char *)NULL);
  add_builtin_type ("SINGLE-FLOAT", env, type_single_float, 1, "FLOAT",
		    "SHORT-FLOAT", "DOUBLE-FLOAT", "LONG-FLOAT", (char *)NULL);
  add_builtin_type ("DOUBLE-FLOAT", env, type_double_float, 1, "SINGLE-FLOAT",
		    (char *)NULL);
  add_builtin_type ("LONG-FLOAT", env, type_long_float, 1, "SINGLE-FLOAT",
		    (char *)NULL);
  add_builtin_type ("COMPLEX", env, type_complex, 1, "NUMBER", (char *)NULL);
  add_builtin_type ("CHARACTER", env, type_character, 1, (char *)NULL);
  add_builtin_type ("SEQUENCE", env, type_sequence, 1, (char *)NULL);
  add_builtin_type ("LIST", env, type_list, 1, "SEQUENCE", (char *)NULL);
  add_builtin_type ("CONS", env, type_cons, 1, "LIST", "SEQUENCE", (char *)NULL);
  add_builtin_type ("ATOM", env, type_atom, 1, (char *)NULL);
  add_builtin_type ("ARRAY", env, type_array, 1, (char *)NULL);
  add_builtin_type ("VECTOR", env, type_vector, 1, "ARRAY", "SEQUENCE",
		    (char *)NULL);
  add_builtin_type ("STRING", env, type_string, 1, "VECTOR", "ARRAY", "SEQUENCE",
		    (char *)NULL);
  add_builtin_type ("HASH-TABLE", env, type_hash_table, 1, (char *)NULL);
  add_builtin_type ("NULL", env, type_null, 1, "SYMBOL", "LIST", "SEQUENCE",
		    (char *)NULL);
  add_builtin_type ("PATHNAME", env, type_pathname, 1, (char *)NULL);
  add_builtin_type ("STREAM", env, type_stream, 1, (char *)NULL);
  add_builtin_type ("FILE-STREAM", env, type_file_stream, 1, "STREAM",
		    (char *)NULL);
  add_builtin_type ("STRING-STREAM", env, type_string_stream, 1, "STREAM",
		    (char *)NULL);


  define_constant_by_name ("MOST-POSITIVE-FIXNUM",
			   create_integer_from_long (LONG_MAX), env);
  define_constant_by_name ("MOST-NEGATIVE-FIXNUM",
			   create_integer_from_long (LONG_MIN), env);

  env->std_in_sym = define_variable ("*STANDARD-INPUT*",
				     create_stream_from_open_file
				     (CHARACTER_STREAM, INPUT_STREAM, stdin),
				     env);

  env->c_stdout = create_stream_from_open_file (CHARACTER_STREAM, OUTPUT_STREAM,
						stdout);

  env->std_out_sym = define_variable ("*STANDARD-OUTPUT*", env->c_stdout, env);
  env->print_escape_sym = define_variable ("*PRINT-ESCAPE*", &t_object, env);
  env->print_readably_sym = define_variable ("*PRINT-READABLY*", &nil_object,
					     env);

  env->package_sym->value_ptr.symbol->value_cell = cluser_package;

  add_builtin_form ("AL-PRINT-NO-WARRANTY", env, builtin_al_print_no_warranty,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-PRINT-TERMS-AND-CONDITIONS", env,
		    builtin_al_print_terms_and_conditions, TYPE_FUNCTION, NULL,
		    0);
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


char *
generate_prompt (struct environment *env)
{
  struct package *pack =
    inspect_variable (env->package_sym, env)->value_ptr.package;
  size_t s = pack->nicks ? pack->nicks->name_len + 5 : pack->name_len + 5;
  char *ret = malloc_and_check (s);

  ret [0] = '[';
  memcpy (ret+1, pack->nicks ? pack->nicks->name : pack->name, s);
  ret [s-4] = ']';
  ret [s-3] = '>';
  ret [s-2] = ' ';
  ret [s-1] = 0;

  return ret;
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
  struct object *last_pref, *ob = skip_prefix (*obj, &bts, &cs, &last_pref, NULL,
					       NULL);
  struct object *l, *call;

  backts_commas_balance += (bts - cs);

  if (args->multiline_comment_depth)
    {
      input = jump_to_end_of_multiline_comment (input, size,
						args->multiline_comment_depth,
						&args->multiline_comment_depth);

      if (!input)
	return NO_OBJECT;

      input++;
      size = args->multiline_comment_depth-1;
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

      if (out == INCOMPLETE_EMPTY_LIST)
	out = INCOMPLETE_NONEMPTY_LIST;
    }
  else if (ob->type == TYPE_STRING)
    {
      out = read_string (&ob, input, size, obj_end);
    }
  else if (ob->type == TYPE_SYMBOL_NAME)
    {
      out = read_symbol_name (&ob, input, size, obj_end, CASE_UPCASE, 0);

      if (out == COMPLETE_OBJECT && !intern_symbol_name (ob, env))
	{
	  args->obj = ob;
	  return PACKAGE_NOT_FOUND_IN_READ;
	}
    }
  else if (ob->type == TYPE_SHARP_MACRO_CALL)
    {
      out = read_sharp_macro_call (&ob, input, size, env, outcome, obj_end,
				   args);

      if (out == COMPLETE_OBJECT)
	{
	  call = ob;
	  ob = call_sharp_macro (call->value_ptr.sharp_macro_call, env, outcome,
				 &out);
	  free_sharp_macro_call (call);

	  if (out & READ_ERROR)
	    {
	      return out;
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

  while (read_out & INCOMPLETE_OBJECT || read_out & READ_ERROR
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
  
  if (read_out == COMPLETE_OBJECT && !args.multiline_comment_depth)
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
  char *pr = generate_prompt (env), *line;
  struct object *ret;

  line = read_line_interactively (pr);

  ret = read_object_interactively_continued (line, strlen (line), env, outcome,
					     input_left, input_left_size);

  free (pr);

  return ret;
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
  if (!size)
    return NULL;

  while (depth && (input = find_multiline_comment_delimiter (input, size,
							     &size)))
    {
      if (*input == '#')
	depth++;
      else
	depth--;

      input += 2, size -= 2;
    }

  if (!depth)
    {
      *depth_or_new_size = size+1;
      return input-1;
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

  while (SYMBOL (list) != &nil_object)
    {
      if (!ret)
	ret = curr = malloc_and_check (sizeof (*ret));
      else
	curr = curr->next = malloc_and_check (sizeof (*curr));

      increment_refcount (CAR (list));
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
      decrement_refcount (list->obj);
      free (list);
      list = next;
    }
}


void
free_object_list_structure (struct object_list *list)
{
  struct object_list *next;

  while (list)
    {
      next = list->next;
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
  struct object *last_pref, *ob = NULL, *call;
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
	  input = jump_to_end_of_line (input, size, &size);
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
	      call = ob;
	      ob = call_sharp_macro (call->value_ptr.sharp_macro_call, env,
				     outcome, &out);
	      free_sharp_macro_call (call);

	      if (out & READ_ERROR)
		{
		  return out;
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
	      out = read_symbol_name (&ob, input, size, obj_end, CASE_UPCASE, 0);

	      if (out == COMPLETE_OBJECT && !intern_symbol_name (ob, env))
		{
		  args->obj = ob;
		  return PACKAGE_NOT_FOUND_IN_READ;
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

	  if (out == COMPLETE_OBJECT)
	    ob->value_ptr.cons_pair->filling_car =
	      ob->value_ptr.cons_pair->filling_cdr = 0;
	  else if (out & INCOMPLETE_OBJECT || out & READ_ERROR)
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

	  if (out == COMPLETE_OBJECT)
	    ob->value_ptr.cons_pair->empty_list_in_car =
	      ob->value_ptr.cons_pair->empty_list_in_cdr = 0;

	  if (out & INCOMPLETE_OBJECT && out != INCOMPLETE_EMPTY_LIST)
	    {
	      ob->value_ptr.cons_pair->empty_list_in_car
		? (ob->value_ptr.cons_pair->filling_car = 1)
		: (ob->value_ptr.cons_pair->filling_cdr = 1);

	      ob->value_ptr.cons_pair->empty_list_in_car =
		ob->value_ptr.cons_pair->empty_list_in_cdr = 0;
	    }

	  if (out & INCOMPLETE_OBJECT || out & READ_ERROR)
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
    resize_string_allocation (ob, ob->value_ptr.string->used_size + length);

  if (!length)
    return COMPLETE_OBJECT;

  str = ob->value_ptr.string;

  normalize_string (str->value + str->used_size, input, size);

  str->used_size += length;

  return out;
}


enum read_outcome
read_symbol_name (struct object **obj, const char *input, size_t size,
		  const char **symname_end, enum readtable_case read_case,
		  int escape_first_char)
{
  struct symbol_name *sym;
  size_t name_l, act_name_l, new_size;
  struct object *ob = *obj;
  enum read_outcome out = NO_OBJECT;
  const char *start_of_pack_sep;
  enum package_record_visibility visib;


  *symname_end = find_end_of_symbol_name
    (input, size, ob && ob->value_ptr.symbol_name->packname_present ? 1 : 0,
     &new_size, &start_of_pack_sep, &visib, &name_l, &act_name_l, &out,
     escape_first_char);

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
				       read_case, escape_first_char);
  else if (start_of_pack_sep)
    {
      copy_symname_with_case_conversion (sym->value + sym->used_size, input,
					 start_of_pack_sep - input, read_case,
					 escape_first_char);
      sym->packname_present = 1;
      copy_symname_with_case_conversion (sym->actual_symname
					 + sym->actual_symname_used_s,
					 visib == EXTERNAL_VISIBILITY ?
					 start_of_pack_sep + 1
					 : start_of_pack_sep + 2, size,
					 read_case, 0);
    }
  else
    copy_symname_with_case_conversion (sym->value + sym->used_size, input, size,
				       read_case, escape_first_char);

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
  enum read_outcome out;

  if (!size)
    return NO_OBJECT;

  if (*obj)
    {
      call = (*obj)->value_ptr.sharp_macro_call;

      out = read_object_continued (&call->obj, 0, call->is_empty_list, input,
				   size, env, e_outcome, &obj_b, macro_end, args);

      if (out == INCOMPLETE_EMPTY_LIST)
	call->is_empty_list = 1;
      else
	call->is_empty_list = 0;

      if (call->dispatch_ch == '\\'
	  && call->obj->value_ptr.symbol_name->packname_present)
	{
	  return WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	}

      if (out & INCOMPLETE_OBJECT)
	return INCOMPLETE_SHARP_MACRO_CALL;

      return out;
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
      call->obj = NULL;
      out = read_symbol_name (&call->obj, input+i+1, size-i-1, macro_end,
			      CASE_UPCASE, 1);

      if (call->obj->value_ptr.symbol_name->packname_present)
	{
	  return WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	}

      if (out & INCOMPLETE_OBJECT)
	return INCOMPLETE_SHARP_MACRO_CALL;

      return out;
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

      return out;
    }

  call->obj = NULL;
  out = read_object (&call->obj, 0, input+i+1, size-i-1, env, e_outcome, &obj_b,
		     macro_end, args);

  if (out == INCOMPLETE_EMPTY_LIST)
    call->is_empty_list = 1;

  if (out & INCOMPLETE_OBJECT)
    return INCOMPLETE_SHARP_MACRO_CALL;

  return out;
}


struct object *
call_sharp_macro (struct sharp_macro_call *macro_call, struct environment *env,
		  struct eval_outcome *e_outcome, enum read_outcome *r_outcome)
{
  struct object *obj = macro_call->obj, *fun, *ret;
  struct symbol_name *s;

  if (macro_call->dispatch_ch == '\'')
    {
      if (!IS_SYMBOL (obj))
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
      if (obj->type != TYPE_SYMBOL_NAME
	  || obj->value_ptr.symbol_name->packname_present)
	{
	  *r_outcome = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

	  return NULL;
	}

      s = obj->value_ptr.symbol_name;

      if (char_vector_utf8_length (s->value, s->used_size) == 1)
	{
	  return create_character_from_utf8 (s->value, s->used_size);
	}

      s->value [0] = toupper ((unsigned char)s->value [0]);

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
  
  *numtype = TYPE_BIGNUM;
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

  if (numtype == TYPE_BIGNUM)
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
  
  if (numtype == TYPE_BIGNUM)
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
alloc_complex (void)
{
  struct object *ret = malloc_and_check (sizeof (*ret));

  ret->refcount = 1;
  ret->type = TYPE_COMPLEX;
  ret->value_ptr.complex = malloc_and_check (sizeof (*ret->value_ptr.complex));

  return ret;
}


struct object *
create_integer_from_long (long num)
{
  struct object *obj = malloc_and_check (sizeof (*obj));

  obj->type = TYPE_BIGNUM;
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
utf8len (const char *string)
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


size_t
char_vector_utf8_length (const char *str, size_t sz)
{
  size_t len = 0, i;

  for (i = 0; i < sz; i++)
    {
      if ((str [i] & 0xc0) >> 6 != 2)
	len++;
    }

  return len;
}


size_t
string_utf8_length (const struct object *str)
{
  return char_vector_utf8_length (str->value_ptr.string->value,
				  str->value_ptr.string->used_size);
}


void *
malloc_and_check (size_t size)
{
  void *mem = malloc (size);

  if (size && !mem)
    {
      fprintf (stderr, "could not allocate %lu bytes. Exiting...\n", size);
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
      fprintf (stderr, "could not allocate %lu bytes. Exiting...\n", size);
      exit (1);
    }

  return mem;
}


void *
calloc_and_check (size_t nmemb, size_t size)
{
  void *mem = calloc (nmemb, size);

  if (nmemb && size && !mem)
    {
      fprintf (stderr, "could not allocate %lu elements of %lu bytes each. "
	       "Exiting...\n", nmemb, size);
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

  fun->lex_vars = NULL;
  fun->lex_funcs = NULL;

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


struct object *
alloc_bytespec (void)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct bytespec *bs = malloc_and_check (sizeof (*bs));

  obj->type = TYPE_BYTESPEC;
  obj->refcount = 1;
  obj->value_ptr.bytespec = bs;

  mpz_init (bs->size);
  mpz_init (bs->pos);

  return obj;
}


struct package_record **
alloc_empty_symtable (size_t table_size)
{
  struct package_record **ret = malloc_and_check (table_size * sizeof (*ret));
  size_t i;

  for (i = 0; i < table_size; i++)
    ret [i] = NULL;

  return ret;
}


struct object *
create_package (char *name, int name_len)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct package *pack = malloc_and_check (sizeof (*pack));

  pack->name = malloc_and_check (name_len);
  memcpy (pack->name, name, name_len);

  pack->name_len = name_len;
  pack->nicks = NULL;
  pack->uses = NULL;
  pack->used_by = NULL;
  pack->symtable = alloc_empty_symtable (SYMTABLE_SIZE);

  obj->type = TYPE_PACKAGE;
  obj->refcount = 1;
  obj->value_ptr.package = pack;

  return obj;
}


struct object *
create_package_from_c_strings (char *name, ...)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct package *pack = malloc_and_check (sizeof (*pack));
  struct name_list *nicks;
  va_list valist;
  char *n;

  pack->name = name;
  pack->name_len = strlen (name);
  pack->nicks = NULL;
  pack->uses = NULL;
  pack->used_by = NULL;
  pack->symtable = alloc_empty_symtable (SYMTABLE_SIZE);

  va_start (valist, name);

  while ((n = va_arg (valist, char *)))
    {
      if (pack->nicks)
	nicks = nicks->next = malloc_and_check (sizeof (*nicks));
      else
	pack->nicks = nicks = malloc_and_check (sizeof (*nicks));

      nicks->name = n;
      nicks->name_len = strlen (n);
    }

  va_end (valist);

  if (pack->nicks)
    nicks->next = NULL;

  obj->type = TYPE_PACKAGE;
  obj->refcount = 1;
  obj->value_ptr.package = pack;

  return obj;
}


void
free_name_list (struct name_list *list)
{
  struct name_list *next;

  while (list)
    {
      next = list->next;
      free (list->name);
      free (list);
      list = next;
    }
}


struct package_record *
inspect_accessible_symbol (char *name, size_t len, struct object *pack,
			   int *is_inherited)
{
  int ind = hash_char_vector (name, len, SYMTABLE_SIZE);
  struct package_record *cur = pack->value_ptr.package->symtable [ind];
  struct object_list *uses;

  while (cur)
    {
      if (eqmem (cur->sym->value_ptr.symbol->name,
		 cur->sym->value_ptr.symbol->name_len, name, len))
	{
	  *is_inherited = 0;
	  return cur;
	}

      cur = cur->next;
    }

  uses = pack->value_ptr.package->uses;

  while (uses)
    {
      cur = uses->obj->value_ptr.package->symtable [ind];

      while (cur)
	{
	  if (cur->visibility == EXTERNAL_VISIBILITY
	      && eqmem (cur->sym->value_ptr.symbol->name,
			cur->sym->value_ptr.symbol->name_len, name, len))
	    {
	      *is_inherited = 1;
	      return cur;
	    }

	  cur = cur->next;
	}

      uses = uses->next;
    }

  return NULL;
}


struct object *
inspect_package_by_designator (struct object *des, struct environment *env)
{
  char *name;
  int len;

  if (des->type == TYPE_PACKAGE)
    return des;

  if (des->type == TYPE_STRING)
    {
      name = des->value_ptr.string->value;
      len = des->value_ptr.string->used_size;
    }
  else if (des->type == TYPE_CHARACTER)
    {
      name = des->value_ptr.character;
      len = strlen (name);
    }
  else
    {
      name = SYMBOL (des)->value_ptr.symbol->name;
      len = SYMBOL (des)->value_ptr.symbol->name_len;
    }

  return find_package (name, len, env);
}


struct object *
find_package (const char *name, size_t len, struct environment *env)
{
  struct object_list *l = env->packages;
  struct package *p;
  struct name_list *n;

  while (l)
    {
      p = l->obj->value_ptr.package;

      if (eqmem (p->name, p->name_len, name, len))
	return l->obj;

      n = p->nicks;

      while (n)
	{
	  if (eqmem (n->name, n->name_len, name, len))
	    return l->obj;

	  n = n->next;
	}

      l = l->next;
    }

  return NULL;
}


struct package_record *
find_package_entry (struct object *symbol, struct package_record **symtable,
		    struct package_record **prev)
{
  struct package_record *c = symtable [hash_symbol (symbol, SYMTABLE_SIZE)];

  *prev = NULL;

  while (c && c->sym != symbol)
    {
      *prev = c;
      c = c->next;
    }

  return c;
}


int
use_package (struct object *used, struct object *pack,
	     struct object **conflicting)
{
  struct object_list *p = pack->value_ptr.package->uses;
  struct package_record *r;
  size_t i;
  int inh;

  while (p)
    {
      if (p->obj == used)
	return 1;

      p = p->next;
    }

  for (i = 0; i < SYMTABLE_SIZE; i++)
    {
      r = used->value_ptr.package->symtable [i];

      while (r)
	{
	  if (r->visibility == EXTERNAL_VISIBILITY
	      && inspect_accessible_symbol (r->sym->value_ptr.symbol->name,
					    r->sym->value_ptr.symbol->name_len,
					    pack, &inh))
	    {
	      *conflicting = r->sym;

	      return 0;
	    }

	  r = r->next;
	}
    }

  p = malloc_and_check (sizeof (*p));
  p->obj = used;
  p->next = pack->value_ptr.package->uses;
  pack->value_ptr.package->uses = p;

  p = malloc_and_check (sizeof (*p));
  p->obj = pack;
  p->next = used->value_ptr.package->used_by;
  used->value_ptr.package->used_by = p;

  return 1;
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
hash_char_vector (const char *str, size_t sz, size_t table_size)
{
  size_t i, tot = 0;

  for (i = 0; i < sz && i < 5; i++)
    {
      tot += str [i];
    }

  return tot % table_size;
}


int
hash_symbol_name (const struct object *symname, size_t table_size)
{
  struct symbol_name *s = symname->value_ptr.symbol_name;

  if (s->packname_present)
    {
      return hash_char_vector (s->actual_symname, s->actual_symname_used_s,
			       table_size);
    }

  return hash_char_vector (s->value, s->used_size, table_size);
}


int
hash_symbol (const struct object *sym, size_t table_size)
{
  return hash_char_vector (sym->value_ptr.symbol->name,
			   sym->value_ptr.symbol->name_len, table_size);
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


int
is_object_in_hash_table (const struct object *object,
			 struct object_list **hash_table, size_t table_size)
{
  return is_object_in_obj_list (object,
				hash_table [hash_object (object, table_size)]);
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


struct refcounted_object_list **
alloc_empty_tailsharing_hash_table (size_t table_size)
{
  struct refcounted_object_list **ret =
    malloc_and_check (table_size * sizeof (*ret));
  size_t i;

  for (i = 0; i < table_size; i++)
    ret [i] = NULL;

  return ret;
}


int
is_object_in_refcounted_obj_list (const struct object *obj,
				  const struct refcounted_object_list *list)
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
prepend_object_to_refcounted_obj_list (struct object *obj,
				       struct refcounted_object_list **list)
{
  struct refcounted_object_list *l = malloc_and_check (sizeof (*l));

  l->obj = obj;
  l->next = *list;
  l->refc = 1;

  if (*list)
    (*list)->refc--;

  *list = l;
}


struct refcounted_object_list **
clone_tailsharing_hash_table (struct refcounted_object_list **hash_table,
			      size_t table_size)
{
  struct refcounted_object_list **ret =
    malloc_and_check (table_size * sizeof (*ret));
  size_t i;

  for (i = 0; i < table_size; i++)
    {
      ret [i] = hash_table [i];

      if (hash_table [i])
	hash_table [i]->refc++;
    }

  return ret;
}


void
free_tailsharing_hash_table (struct refcounted_object_list **hash_table,
			     size_t table_size)
{
  size_t i;
  struct refcounted_object_list *l, *n;

  for (i = 0; i < table_size; i++)
    {
      if (hash_table [i])
	{
	  l = hash_table [i];

	  l->refc--;

	  if (!l->refc)
	    {
	      l = l->next;

	      while (l && !l->refc)
		{
		  n = l->next;
		  free (l);
		  l = n;
		}
	    }
	}
    }

  free (hash_table);
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
		 struct environment *env, struct eval_outcome *outcome,
		 int is_macro)
{
  struct object *fun = alloc_function ();
  struct function *f = fun->value_ptr.function;

  outcome->type = EVAL_OK;
  f->lambda_list = parse_lambda_list (lambda_list, env, outcome);

  if (outcome->type == INVALID_LAMBDA_LIST)
    {
      free_function_or_macro (fun);

      return NULL;
    }

  if (!is_macro)
    clone_lexical_environment (&f->lex_vars, &f->lex_funcs, env->vars,
			       env->lex_env_vars_boundary, env->funcs,
			       env->lex_env_funcs_boundary);

  increment_refcount (body);
  f->body = body;

  return fun;
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

  obj->value_ptr.string->value = malloc_and_check (size);
  obj->value_ptr.string->alloc_size = size;
  obj->value_ptr.string->used_size = 0;
  obj->value_ptr.string->fill_pointer = -1;

  return obj;
}


struct object *
create_string_from_char_vector (const char *str, size_t size, int do_copy)
{
  size_t i;
  struct object *ret = alloc_string (size);

  for (i = 0; i < size; i++)
    ret->value_ptr.string->value [i] = str [i];

  ret->value_ptr.string->used_size = size;

  return ret;
}


struct object *
create_string_from_c_string (const char *str)
{
  return create_string_from_char_vector (str, strlen (str), 1);
}


void
resize_string_allocation (struct object *string, size_t size)
{
  if (size == string->value_ptr.string->alloc_size)
    return;

  string->value_ptr.string->value =
    realloc_and_check (string->value_ptr.string->value, size);

  if (size < string->value_ptr.string->used_size)
    string->value_ptr.string->used_size = size;

  string->value_ptr.string->alloc_size = size;
}


char *
copy_string_to_c_string (struct string *str)
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
			 enum read_outcome *outcome, int escape_first_char)
{
  size_t i = 0;
  int single_escape = escape_first_char, multiple_escape = 0, just_dots = 1,
    colons = 0;
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
				   enum readtable_case read_case,
				   int escape_first_char)
{
  size_t i;
  int j, single_escape = escape_first_char, multiple_escape = 0;
  
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
		output [j++] = toupper ((unsigned char)input [i]);
		break;
	      case CASE_DOWNCASE:
		output [j++] = tolower ((unsigned char)input [i]);
		break;
	      case CASE_PRESERVE:
		output [j++] = input [i];
		break;
	      case CASE_INVERT:
		output [j++] = isupper ((unsigned char)input [i])
		  ? tolower ((unsigned char)input [i])
		  : toupper ((unsigned char)input [i]);
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
alloc_vector (size_t size, int fill_with_nil, int dont_store_size)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct array *vec = malloc_and_check (sizeof (*vec));
  struct array_size *sz;
  size_t i;

  if (!dont_store_size)
    {
      sz = malloc_and_check (sizeof (*sz));
      sz->size = size;
      sz->next = NULL;
      vec->alloc_size = sz;
    }

  vec->fill_pointer = -1;
  vec->value = calloc_and_check (size, sizeof (*vec->value));

  if (fill_with_nil)
    {
      for (i = 0; i < size; i++)
	vec->value [i] = &nil_object;
    }

  obj->type = TYPE_ARRAY;
  obj->refcount = 1;
  obj->value_ptr.array = vec;

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
  vec->value = calloc_and_check (sz->size, sizeof (*vec->value));

  for (i = 0; i < sz->size; i++)
    vec->value [i] = nth (i, list);

  obj->type = TYPE_ARRAY;
  obj->refcount = 1;
  obj->value_ptr.array = vec;

  return obj;
}


void
resize_vector (struct object *vector, size_t size)
{
  vector->value_ptr.array->value =
    realloc_and_check (vector->value_ptr.array->value,
		       size * sizeof (*vector->value_ptr.array->value));

  vector->value_ptr.array->alloc_size->size = size;
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
create_character_from_char (char ch)
{
  char *s = malloc_and_check (2);

  s [0] = ch;
  s [1] = 0;

  return create_character (s, 0);
}


struct object *
get_nth_character (struct object *str, int ind)
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
set_nth_character (struct object *str, int ind, char *ch)
{
  char *c = str->value_ptr.string->value;
  size_t s = str->value_ptr.string->used_size, off;

  for (off = 0; ind; ind--)
    {
      off = next_utf8_char (ch, s);

      if (!off)
	return NULL;

      c += off;
      s -= off;
    }

  memcpy (c, ch, strlen (ch));

  return create_character_from_utf8 (ch, strlen (ch));
}


struct object *
create_file_stream (enum stream_type type, enum stream_direction direction,
		    struct string *filename, struct eval_outcome *outcome)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct stream *str = malloc_and_check (sizeof (*str));
  char *fn = copy_string_to_c_string (filename);

  str->medium = FILE_STREAM;

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
  str->dirty_line = 0;

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

  str->medium = FILE_STREAM;
  str->type = type;
  str->direction = direction;
  str->is_open = 1;
  str->dirty_line = 0;
  str->file = file;

  obj->type = TYPE_STREAM;
  obj->refcount = 1;
  obj->value_ptr.stream = str;

  return obj;
}


struct object *
create_string_stream (enum stream_direction direction, struct object *instr)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct stream *str = malloc_and_check (sizeof (*str));

  str->medium = STRING_STREAM;
  str->direction = direction;
  str->is_open = 1;
  str->dirty_line = 0;

  if (direction == INPUT_STREAM)
    {
      increment_refcount (instr);
      str->string = instr;
    }
  else
    str->string = alloc_string (0);

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
intern_symbol_from_char_vector (char *name, size_t len, int do_copy,
				enum package_record_visibility vis,
				int always_create_if_missing,
				struct object *package)
{
  struct object *sym;
  int ind = hash_char_vector (name, len, SYMTABLE_SIZE);
  struct package_record *cell = package->value_ptr.package->symtable [ind],
    *cur = cell, *new_sym;
  struct object_list *uses;

  while (cur)
    {
      if (eqmem (cur->sym->value_ptr.symbol->name,
		 cur->sym->value_ptr.symbol->name_len, name, len))
	{
	  if (vis == EXTERNAL_VISIBILITY
	      && cur->visibility == INTERNAL_VISIBILITY)
	    return NULL;

	  increment_refcount (cur->sym);
	  return cur->sym;
	}

      cur = cur->next;
    }

  uses = package->value_ptr.package->uses;

  while (uses)
    {
      cur = uses->obj->value_ptr.package->symtable [ind];

      while (cur)
	{
	  if (cur->visibility == EXTERNAL_VISIBILITY
	      && eqmem (cur->sym->value_ptr.symbol->name,
			cur->sym->value_ptr.symbol->name_len, name, len))
	    {
	      increment_refcount (cur->sym);
	      return cur->sym;
	    }

	  cur = cur->next;
	}

      uses = uses->next;
    }

  if (vis == EXTERNAL_VISIBILITY && !always_create_if_missing)
    return NULL;

  sym = create_symbol (name, len, do_copy);
  sym->value_ptr.symbol->home_package = package;

  new_sym = malloc_and_check (sizeof (*new_sym));
  new_sym->visibility = vis;
  new_sym->sym = sym;
  new_sym->next = cell;

  package->value_ptr.package->symtable [ind] = new_sym;

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
						   EXTERNAL_VISIBILITY, 1,
						   env->keyword_package);

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
						   s->visibility, 0, pack);
	  return s->sym;
	}
    }

  pack = inspect_variable (env->package_sym, env);
  s->sym = intern_symbol_from_char_vector (s->value, s->used_size, 1,
					   INTERNAL_VISIBILITY, 0, pack);

  return s->sym;
}


void
unintern_symbol (struct object *sym)
{
  struct object *s = SYMBOL (sym);
  struct object *p = s->value_ptr.symbol->home_package;
  struct package_record *prev, *entry;

  entry = find_package_entry (s, p->value_ptr.package->symtable, &prev);

  if (prev)
    prev->next = entry->next;
  else
    p->value_ptr.package->symtable [hash_symbol (sym, SYMTABLE_SIZE)] =
      entry->next;

  free (entry);

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
      increment_refcount (sym);
      increment_refcount (obj);
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

  decrement_refcount (env->sym);
  decrement_refcount (env->obj);

  free (env);

  if (num == 1)
    return b;
  else
    return remove_bindings (env->next, num-1);
}


struct binding *
find_binding (struct symbol *sym, struct binding *bins, enum binding_type type,
	      int bin_num)
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

      increment_refcount (sym);
      return add_binding (create_binding (sym, val, DYNAMIC_BINDING, 0),
			  bins);
    }
  else
    {
      increment_refcount (sym);
      return add_binding (create_binding (sym, val, LEXICAL_BINDING, 0), bins);
    }
}


struct go_tag *
collect_go_tags (struct object *body)
{
  struct object *car, *destfind, *dest;
  struct go_tag *ret = NULL;

  while (SYMBOL (body) != &nil_object)
    {
      car = CAR (body);

      if (IS_SYMBOL (car) || car->type == TYPE_BIGNUM)
	{
	  destfind = CDR (body);

	  while (SYMBOL (destfind) != &nil_object && (dest = CAR (destfind))
		 && (IS_SYMBOL (dest) || dest->type == TYPE_BIGNUM))
	    destfind = CDR (destfind);

	  ret = add_go_tag (car, destfind, ret);
	}

      body = CDR (body);
    }

  return ret;
}


struct go_tag_frame *
add_go_tag_frame (struct go_tag_frame *stack)
{
  struct go_tag_frame *f = malloc_and_check (sizeof (*f));

  f->frame = NULL;
  f->next = stack;

  return f;
}


struct go_tag *
add_go_tag (struct object *tagname, struct object *tagdest, struct go_tag *tags)
{
  struct go_tag *new = malloc_and_check (sizeof (*new));

  new->name = tagname;
  new->dest = tagdest;
  new->next = tags;

  return new;
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
	  if ((IS_SYMBOL (tagname)
	       && SYMBOL (tagname) == SYMBOL (t->name))
	      || (tagname->type == TYPE_BIGNUM
		  && !mpz_cmp (tagname->value_ptr.integer,
			       t->name->value_ptr.integer)))
	    return t;
	}

      t = t->next;
    }

  return NULL;
}


struct object_list *
add_block (struct object *name, struct object_list *blocks)
{
  struct object_list *new = malloc_and_check (sizeof (*new));

  new->obj = name;
  new->next = blocks;

  return new;
}


struct object_list *
remove_block (struct object_list *blocks)
{
  struct object_list *next = blocks->next;

  free (blocks);

  return next;
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
  struct object *pack = inspect_variable (env->package_sym, env);
  struct object *sym = intern_symbol_from_char_vector (name, strlen (name), 1,
						       EXTERNAL_VISIBILITY, 1,
						       pack);
  struct object *par;

  va_start (valist, is_standard);

  sym->value_ptr.symbol->is_type = 1;
  sym->value_ptr.symbol->is_standard_type = is_standard;
  sym->value_ptr.symbol->builtin_type = builtin_type;

  increment_refcount (sym);

  while ((s = va_arg (valist, char *)))
    {
      par = intern_symbol_from_char_vector (s, strlen (s), 1,
					    EXTERNAL_VISIBILITY, 0, pack);

      prepend_object_to_obj_list (par, &sym->value_ptr.symbol->parent_types);
    }

  va_end (valist);
}


struct object *
add_builtin_form (char *name, struct environment *env,
		  struct object *(*builtin_form)
		  (struct object *list, struct environment *env,
		   struct eval_outcome *outcome), enum object_type type,
		  struct object *(*builtin_accessor)
		  (struct object *list, struct object *newval,
		   struct environment *env, struct eval_outcome *outcome),
		  int is_special_operator)
{
  struct object *pack = inspect_variable (env->package_sym, env);
  struct object *sym = intern_symbol_from_char_vector (name, strlen (name), 1,
						       EXTERNAL_VISIBILITY, 1,
						       pack);
  struct object *fun = alloc_function ();
  struct function *f = fun->value_ptr.function;

  fun->type = type;

  f->name = sym;
  f->is_special_operator = is_special_operator;
  f->builtin_form = builtin_form;

  sym->value_ptr.symbol->function_cell = fun;
  sym->value_ptr.symbol->builtin_accessor = builtin_accessor;

  return sym;
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

  sym = SYMBOL (sym);

  if (sym->value_ptr.symbol->is_const)
    {
      outcome->type = CANT_REDEFINE_CONSTANT;
      return NULL;
    }

  sym->value_ptr.symbol->value_cell = val;

  if (!sym->value_ptr.symbol->is_const && !sym->value_ptr.symbol->is_parameter)
    {
      increment_refcount (sym);
      sym->value_ptr.symbol->is_const = 1;
    }

  increment_refcount (sym);

  return sym;
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

  if (!s->value_ptr.symbol->is_parameter)
    {
      increment_refcount (s);
      s->value_ptr.symbol->is_parameter = 1;
    }

  increment_refcount_by (val, s->refcount - 1, NULL);
  s->value_ptr.symbol->value_cell = val;

  increment_refcount (s);
  return s;
}


struct object *
define_constant_by_name (char *name, struct object *value,
			 struct environment *env)
{
  struct object *pack = inspect_variable (env->package_sym, env);
  struct object *sym = intern_symbol_from_char_vector (name, strlen (name), 1,
						       EXTERNAL_VISIBILITY, 1,
						       pack);

  sym->value_ptr.symbol->is_const = 1;
  sym->value_ptr.symbol->value_cell = value;

  return sym;
}


struct object *
define_variable (char *name, struct object *value, struct environment *env)
{
  struct object *pack = inspect_variable (env->package_sym, env);
  struct object *sym = intern_symbol_from_char_vector (name, strlen (name), 1,
						       EXTERNAL_VISIBILITY, 1,
						       pack);

  sym->value_ptr.symbol->is_parameter = 1;
  sym->value_ptr.symbol->value_cell = value;

  return sym;
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

  while (list && SYMBOL (list) != &nil_object)
    {
      prev = list;
      list = CDR (list);

      if (list->type != TYPE_CONS_PAIR)
	break;
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

  if (list && SYMBOL (list) != &nil_object)
    return 1;

  return 0;
}


int
is_circular_list (struct object *list)
{
  int circ;

  is_dotted_or_circular_list (list, &circ);

  return circ;
}


int
is_dotted_or_circular_list (struct object *list, int *is_circular)
{
  struct object_list **hash_t;
  int ind;

  if (SYMBOL (list) == &nil_object)
    {
      *is_circular = 0;
      return 0;
    }

  hash_t = alloc_empty_hash_table (ANTILOOP_HASH_T_SIZE);

  while (SYMBOL (list) != &nil_object)
    {
      if (list->type != TYPE_CONS_PAIR)
	{
	  free_hash_table (hash_t, ANTILOOP_HASH_T_SIZE);
	  *is_circular = 0;
	  return 1;
	}

      ind = hash_object (list, ANTILOOP_HASH_T_SIZE);

      if (is_object_in_obj_list (list, hash_t [ind]))
	{
	  free_hash_table (hash_t, ANTILOOP_HASH_T_SIZE);
	  *is_circular = 1;
	  return 0;
	}

      prepend_object_to_obj_list (list, &hash_t [ind]);

      list = CDR (list);
    }

  free_hash_table (hash_t, ANTILOOP_HASH_T_SIZE);

  *is_circular = 0;
  return 0;
}


int
is_proper_list (struct object *list)
{
  int circ, dot;

  dot = is_dotted_or_circular_list (list, &circ);

  if (!circ && !dot)
    return 1;

  return 0;
}


struct object *
copy_prefix (const struct object *begin, const struct object *end,
	     struct object **last_prefix, int refcount)
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

      tmp->refcount = refcount;

      if (pr)
	pr->value_ptr.next = tmp;
      else
	out = pr = tmp;

      begin = begin->value_ptr.next;
    }

  if (begin)
    {
      tmp = alloc_prefix (begin->type == TYPE_QUOTE ? QUOTE :
			  begin->type == TYPE_BACKQUOTE ? BACKQUOTE :
			  begin->type == TYPE_COMMA ? COMMA :
			  begin->type == TYPE_AT ? AT :
			  begin->type == TYPE_DOT ? DOT : NONE);

      tmp->refcount = refcount;
    }

  if (pr)
    pr->value_ptr.next = tmp;
  else
    out = tmp;

  if (last_prefix)
    *last_prefix = pr ? pr->value_ptr.next : out;

  return out;
}


struct object *
copy_list_structure (struct object *list, const struct object *prefix,
		     int cell_num, struct object **last_cell)
{
  struct object *cons, *out, *lastpref;
  int i = 1;

  out = cons = alloc_empty_cons_pair ();

  increment_refcount (CAR (list));
  out->value_ptr.cons_pair->car = CAR (list);

  list = CDR (list);

  while (list->type == TYPE_CONS_PAIR && (i < cell_num || cell_num < 0))
    {
      cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

      increment_refcount (CAR (list));

      if (prefix)
	{
	  cons->value_ptr.cons_pair->car = copy_prefix (prefix, NULL, &lastpref,
							cons->refcount);
	  lastpref->value_ptr.next = CAR (list);
	}
      else
	cons->value_ptr.cons_pair->car = CAR (list);

      list = CDR (list);
      i++;
    }

  if (SYMBOL (list) != &nil_object && cell_num < 0)
    increment_refcount (list);

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


size_t
array_total_size (const struct array *array)
{
  size_t ret = 1;
  struct array_size *s = array->alloc_size;

  while (s)
    {
      ret *= s->size;

      s = s->next;
    }

  return ret;
}


int
hash_object_respecting_eq (const struct object *object, size_t table_size)
{
  return (long int)(IS_SYMBOL (object) ? SYMBOL (object) : object)
    % table_size;  /* FIXME the cast works on common
		      platforms, but is not allowed by ansi.
		      we should probably change to a hash
		      function on object fields */
}


int
hash_table_count (const struct hashtable *hasht)
{
  int i, cnt = 0;

  for (i = 0; i < LISP_HASHTABLE_SIZE; i++)
    {
      if (hasht->table [i])
	cnt++;
    }

  return cnt;
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

  while (obj && SYMBOL (obj) != &nil_object)
    {
      car = CAR (obj);

      if (!IS_SYMBOL (car))
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
	}

      if (symbol_is_among (car, NULL, "&OPTIONAL", "&REST", "&BODY", "&KEY",
			   "&AUX", "&ALLOW_OTHER_KEYS", (char *)NULL))
	{
	  break;
	}

      increment_refcount (SYMBOL (car));

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
  int l;

  *last = NULL;

  while (obj && SYMBOL (obj) != &nil_object)
    {
      car = CAR (obj);

      if (IS_SYMBOL (car)
	  && symbol_is_among (car, NULL, "&OPTIONAL", "&REST", "&BODY", "&KEY",
			      "&AUX", "&ALLOW_OTHER_KEYS", (char *)NULL))
	{
	  break;
	}
      else if (IS_SYMBOL (car))
	{
	  increment_refcount (SYMBOL (car));

	  if (!first)
	    *last = first = alloc_parameter (OPTIONAL_PARAM, SYMBOL (car));
	  else
	    *last = (*last)->next =
	      alloc_parameter (OPTIONAL_PARAM, SYMBOL (car));
	}
      else if (car->type == TYPE_CONS_PAIR)
	{
	  l = list_length (car);

	  if (!l || l > 3 || !IS_SYMBOL (CAR (car)))
	    {
	      outcome->type = INVALID_LAMBDA_LIST;
	      return NULL;
	    }

	  increment_refcount (SYMBOL (CAR (car)));

	  if (!first)
	    *last = first = alloc_parameter (OPTIONAL_PARAM, SYMBOL (CAR (car)));
	  else
	    *last = (*last)->next =
	      alloc_parameter (OPTIONAL_PARAM, SYMBOL (CAR (car)));

	  if (l >= 2)
	    {
	      increment_refcount (nth (1, car));
	      (*last)->init_form = nth (1, car);
	    }

	  if (l == 3)
	    {
	      if (!IS_SYMBOL (CAR (CDR (CDR (car)))))
		{
		  outcome->type = INVALID_LAMBDA_LIST;
		  return NULL;
		}

	      increment_refcount (SYMBOL (CAR (CDR (CDR (car)))));
	      (*last)->supplied_p_param = SYMBOL (CAR (CDR (CDR (car))));
	    }
	}
      else
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
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
  int l;

  *last = NULL;

  while (obj && SYMBOL (obj) != &nil_object)
    {
      car = CAR (obj);

      if (IS_SYMBOL (car)
	  && symbol_is_among (car, NULL, "&OPTIONAL", "&REST", "&BODY", "&KEY",
			      "&AUX", "&ALLOW_OTHER_KEYS", (char *)NULL))
	{
	  break;
	}
      else if (IS_SYMBOL (car))
	{
	  var = SYMBOL (car);
	  increment_refcount (var);

	  key = intern_symbol_from_char_vector (var->value_ptr.symbol->name,
						var->value_ptr.symbol->name_len,
						1, EXTERNAL_VISIBILITY, 1,
						env->keyword_package);

	  if (!first)
	    *last = first = alloc_parameter (KEYWORD_PARAM, var);
	  else
	    *last = (*last)->next =
	      alloc_parameter (KEYWORD_PARAM, var);

	  (*last)->key = key;
	}
      else if (car->type == TYPE_CONS_PAIR)
	{
	  l = list_length (car);

	  if (!l || l > 3)
	    {
	      outcome->type = INVALID_LAMBDA_LIST;
	      return NULL;
	    }

	  caar = CAR (car);

	  if (IS_SYMBOL (caar))
	    {
	      var = SYMBOL (caar);
	      increment_refcount (var);

	      key = intern_symbol_from_char_vector
		(var->value_ptr.symbol->name, var->value_ptr.symbol->name_len,
		 1, EXTERNAL_VISIBILITY, 1, env->keyword_package);
	    }
	  else if (caar->type == TYPE_CONS_PAIR)
	    {
	      if (list_length (caar) != 2 || !IS_SYMBOL (CAR (caar))
		  || !IS_SYMBOL (CAR (CDR (caar))))
		{
		  outcome->type = INVALID_LAMBDA_LIST;
		  return NULL;
		}

	      key = SYMBOL (CAR (caar));
	      increment_refcount (key);

	      var = SYMBOL (CAR (CDR (caar)));
	      increment_refcount (var);
	    }
	  else
	    {
	      outcome->type = INVALID_LAMBDA_LIST;
	      return NULL;
	    }

	  if (!first)
	    *last = first = alloc_parameter (KEYWORD_PARAM, var);
	  else
	    *last = (*last)->next =
	      alloc_parameter (KEYWORD_PARAM, var);

	  (*last)->key = key;

	  if (l >= 2)
	    {
	      increment_refcount (nth (1, car));
	      (*last)->init_form = nth (1, car);
	    }

	  if (l == 3)
	    {
	      if (!IS_SYMBOL (CAR (CDR (CDR (car)))))
		{
		  outcome->type = INVALID_LAMBDA_LIST;
		  return NULL;
		}

	      increment_refcount (SYMBOL (nth (2, car)));
	      (*last)->supplied_p_param = SYMBOL (nth (2, car));
	    }
	}
      else
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
	}

      obj = CDR (obj);
    }

  *next = obj;

  return first;
}


struct parameter *
parse_lambda_list (struct object *obj, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct parameter *first = NULL, *last = NULL;
  struct object *car;

  if (SYMBOL (obj) == &nil_object)
    {
      return NULL;
    }

  if (obj->type != TYPE_CONS_PAIR)
    {
      outcome->type = INVALID_LAMBDA_LIST;
      return NULL;
    }

  first = parse_required_parameters (obj, &last, &obj, outcome);

  if (outcome->type != EVAL_OK)
    return NULL;

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && symbol_equals (car, "&OPTIONAL", NULL))
    {
      if (first)
	last->next =
	  parse_optional_parameters (CDR (obj), &last, &obj, outcome);
      else
	first = parse_optional_parameters (CDR (obj), &last, &obj, outcome);

      if (outcome->type != EVAL_OK)
	return NULL;
    }

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && symbol_is_among (car, NULL, "&REST", "&BODY", (char *)NULL))
    {
      if (SYMBOL (CDR (obj)) == &nil_object || !IS_SYMBOL (CAR (CDR (obj))))
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
	}

      increment_refcount (SYMBOL (CAR (CDR (obj))));

      if (first)
	last->next = alloc_parameter (REST_PARAM, SYMBOL (CAR (CDR (obj))));
      else
	first = last = alloc_parameter (REST_PARAM, SYMBOL (CAR (CDR (obj))));

      obj = CDR (CDR (obj));
    }

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && symbol_equals (car, "&KEY", NULL))
    {
      if (first)
	last->next =
	  parse_keyword_parameters (CDR (obj), &last, &obj, env, outcome);
      else
	first = parse_keyword_parameters (CDR (obj), &last, &obj, env, outcome);

      if (outcome->type != EVAL_OK)
	return NULL;
    }

  if (SYMBOL (obj) != &nil_object)
    {
      outcome->type = INVALID_LAMBDA_LIST;
      return NULL;
    }

  return first;
}


struct object *
evaluate_body (struct object *body, int is_tagbody, struct object *block_name,
	       struct environment *env, struct eval_outcome *outcome)
{
  struct object *res = &nil_object;
  struct go_tag *tags = NULL, *t;

  if (is_tagbody)
    {
      tags = collect_go_tags (body);

      if (tags)
	{
	  env->go_tag_stack = add_go_tag_frame (env->go_tag_stack);
	  env->go_tag_stack->frame = tags;
	}
    }

  if (block_name)
    env->blocks = add_block (block_name, env->blocks);

  do
    {
      decrement_refcount (res);

      if (!is_tagbody
	  || (CAR (body)->type != TYPE_BIGNUM && !IS_SYMBOL (CAR (body))))
	{
	  res = evaluate_object (CAR (body), env, outcome);

	  if (!res)
	    {
	      if (!outcome->tag_to_jump_to && !outcome->block_to_leave)
		goto cleanup_and_leave;
	      else if (outcome->tag_to_jump_to)
		{
		  if (is_tagbody)
		    {
		      t = find_go_tag (outcome->tag_to_jump_to, env->go_tag_stack);

		      if (!t)
			goto cleanup_and_leave;

		      outcome->tag_to_jump_to = NULL;
		      body = t->dest;
		    }
		  else
		    goto cleanup_and_leave;
		}
	      else if (outcome->block_to_leave)
		{
		  if (block_name && outcome->block_to_leave == env->blocks->obj)
		    {
		      outcome->block_to_leave = NULL;

		      res = outcome->return_value;
		    }

		  goto cleanup_and_leave;
		}
	    }
	  else
	    {
	      if (SYMBOL (CDR (body)) != &nil_object)
		CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      body = CDR (body);
	    }
	}
      else
	body = CDR (body);

    } while (SYMBOL (body) != &nil_object);

 cleanup_and_leave:
  if (tags)
    {
      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);
    }

  if (block_name)
    env->blocks = remove_block (env->blocks);

  return res;
}


struct object *
call_function (struct object *func, struct object *arglist, int eval_args,
	       int is_macro, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct parameter *par = func->value_ptr.function->lambda_list, *findk;
  struct binding *bins = NULL, *b;
  struct object *val, *ret, *ret2, *args = NULL;
  int argsnum = 0, closnum = 0, prev_lex_bin_num = env->lex_env_vars_boundary,
    rest_found = 0;

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
	decrement_refcount (args);

      return ret;
    }

  while (SYMBOL (arglist) != &nil_object && par
	 && (par->type == REQUIRED_PARAM || par->type == OPTIONAL_PARAM))
    {
      if (eval_args)
	{
	  val = evaluate_object (CAR (arglist), env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!val)
	    {
	      ret = NULL;
	      goto clean_lex_env;
	    }
	}
      else
	{
	  increment_refcount (CAR (arglist));
	  val = CAR (arglist);
	}

      bins = bind_variable (par->name, val, bins);

      argsnum++;

      if (par->type == OPTIONAL_PARAM && par->supplied_p_param)
	{
	  bins = bind_variable (par->supplied_p_param, &t_object, bins);

	  argsnum++;
	}

      par = par->next;

      arglist = CDR (arglist);
    }

  if (par && par->type == REQUIRED_PARAM)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      ret = NULL;
      goto clean_lex_env;
    }

  if (SYMBOL (arglist) != &nil_object
      && (!par || (par && par->type != REST_PARAM && par->type != KEYWORD_PARAM)))
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      ret = NULL;
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
	      ret = NULL;
	      goto clean_lex_env;
	    }

	  bins = bind_variable (par->name, val, bins);

	  argsnum++;
	}
      else
	{
	  bins = bind_variable (par->name, &nil_object, bins);

	  argsnum++;
	}

      if (par->supplied_p_param)
	{
	  bins = bind_variable (par->supplied_p_param, &nil_object, bins);

	  argsnum++;
	}

      par = par->next;
    }

  if (par && (par->type == REST_PARAM || par->type == KEYWORD_PARAM))
    {
      if (eval_args)
	{
	  args = evaluate_through_list (arglist, env, outcome);

	  if (!args)
	    {
	      ret = NULL;
	      goto clean_lex_env;
	    }
	}
      else
	{
	  args = arglist;
	  increment_refcount (args);
	}
    }

  if (par && par->type == REST_PARAM)
    {
      bins = bind_variable (par->name, args, bins);

      argsnum++;

      par = par->next;

      rest_found = 1;
    }

  if (par && par->type == KEYWORD_PARAM)
    {
      findk = par;

      while (findk && findk->type == KEYWORD_PARAM)
	{
	  findk->key_passed = 0;

	  findk = findk->next;
	}

      while (SYMBOL (args) != &nil_object)
	{
	  findk = par;

	  while (findk && findk->type == KEYWORD_PARAM)
	    {
	      if (findk->key == SYMBOL (CAR (args)))
		break;

	      findk = findk->next;
	    }

	  if (!findk || findk->type != KEYWORD_PARAM)
	    {
	      outcome->type = UNKNOWN_KEYWORD_ARGUMENT;
	      ret = NULL;
	      goto clean_lex_env;
	    }

	  args = CDR (args);

	  if (SYMBOL (args) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      ret = NULL;
	      goto clean_lex_env;
	    }

	  increment_refcount (CAR (args));

	  bins = bind_variable (findk->name, CAR (args), bins);
	  findk->key_passed = 1;
	  argsnum++;

	  args = CDR (args);
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
		      ret = NULL;
		      goto clean_lex_env;
		    }
		}
	      else
		val = &nil_object;

	      bins = bind_variable (findk->name, val, bins);
	      argsnum++;

	      if (findk->supplied_p_param)
		{
		  bins = bind_variable (findk->supplied_p_param, &nil_object,
					bins);
		  argsnum++;
		}
	    }
	  else if (findk->supplied_p_param)
	    {
	      bins = bind_variable (findk->supplied_p_param, &t_object, bins);
	      argsnum++;
	    }

	  findk = findk->next;
	}
    }

  if (args && !rest_found)
    decrement_refcount (args);

  env->vars = chain_bindings (bins, env->vars, NULL);
  bins = NULL;

  if (is_macro)
    env->lex_env_vars_boundary += argsnum;
  else
    env->lex_env_vars_boundary = argsnum;

  env->vars = chain_bindings (func->value_ptr.function->lex_vars, env->vars,
			      &closnum);
  env->lex_env_vars_boundary += closnum;

  ret = evaluate_body (func->value_ptr.function->body, 0,
		       func->value_ptr.function->name, env, outcome);

  if (ret && is_macro)
    {
      ret2 = evaluate_object (ret, env, outcome);

      decrement_refcount (ret);

      ret = ret2;
    }

 clean_lex_env:
  for (; closnum; closnum--)
    {
      b = env->vars;

      env->vars = env->vars->next;

      if (closnum == 1)
	b->next = NULL;
    }

  if (!bins)
    env->vars = remove_bindings (env->vars, argsnum);
  else
    remove_bindings (bins, argsnum);

  env->lex_env_vars_boundary = prev_lex_bin_num;

  return ret;
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
						     EXTERNAL_VISIBILITY, 0,
						     env->cl_package),
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
      && IS_SYMBOL (CAR (typespec))
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
						     EXTERNAL_VISIBILITY, 0,
						     env->cl_package),
		     env, outcome);
}


int
is_subtype (const struct object *first, const struct object *second,
	    struct environment *env, struct eval_outcome *outcome)
{
  struct object_list *p;
  int ret;

  if (SYMBOL (first) == &nil_object || SYMBOL (second) == &t_object)
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
      increment_refcount (obj->value_ptr.next);
      return obj->value_ptr.next;
    }
  else if (obj->type == TYPE_BACKQUOTE)
    {
      return apply_backquote (obj->value_ptr.next, 1, env, outcome, 1, NULL,
			      NULL);
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
			       LEXICAL_BINDING, env->lex_env_vars_boundary);

	  if (bind)
	    {
	      increment_refcount (bind->obj);
	      return bind->obj;
	    }
	  else if (sym->value_ptr.symbol->value_dyn_bins_num)
	    {
	      bind = find_binding (sym->value_ptr.symbol, env->vars,
				   DYNAMIC_BINDING, -1);
	      increment_refcount (bind->obj);
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
      increment_refcount (obj);
      return obj;
    }
}


struct object *
apply_backquote (struct object *form, int backts_commas_balance,
		 struct environment *env, struct eval_outcome *outcome,
		 int forbid_splicing, int *do_splice, struct object **last_pref)
{
  struct object *obj, *ret, *retform, *retcons, *reading_cons, *cons, *lastpr,
    *tmp, *lp;
  int do_spl, must_copy, refc = 1;

  if (!backts_commas_balance)
    {
      ret = evaluate_object (form, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
      return ret;
    }
  else if (form->type == TYPE_BACKQUOTE)
    {
      ret = apply_backquote (form->value_ptr.next, backts_commas_balance + 1,
			     env, outcome, forbid_splicing, do_splice, last_pref);

      if (!ret)
	return NULL;

      if (ret == form->value_ptr.next)
	{
	  form->refcount++;
	  !forbid_splicing ? *last_pref = ret : NULL;
	  return form;
	}

      retform = alloc_prefix (BACKQUOTE);
      retform->value_ptr.next = ret;
      !forbid_splicing ? *last_pref = retform : NULL;

      return retform;
    }
  else if (form->type == TYPE_COMMA)
    {
      if (backts_commas_balance == 1)
	{
	  if (form->value_ptr.next->type == TYPE_AT
	      || form->value_ptr.next->type == TYPE_DOT)
	    {
	      if (forbid_splicing == 1)
		{
		  outcome->type = COMMA_AT_OR_DOT_NOT_ALLOWED_AT_TOP_LEVEL;

		  return NULL;
		}

	      if (forbid_splicing == 2)
		{
		  outcome->type = CANT_SPLICE_AFTER_CONSING_DOT;

		  return NULL;
		}

	      ret = evaluate_object (form->value_ptr.next->value_ptr.next, env,
				     outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!ret)
		return NULL;

	      if (form->value_ptr.next->type == TYPE_AT)
		*do_splice = 1;
	      else
		*do_splice = 2;

	      return ret;
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
	  ret = apply_backquote (form->value_ptr.next, backts_commas_balance - 1,
				 env, outcome, forbid_splicing, do_splice,
				 last_pref);

	  if (!ret)
	    return NULL;

	  if (ret == form->value_ptr.next)
	    {
	      form->refcount++;
	      !forbid_splicing ? *last_pref = ret : NULL;
	      return form;
	    }

	  retform = alloc_prefix (COMMA);
	  retform->value_ptr.next = ret;
	  !forbid_splicing ? *last_pref = retform : NULL;

	  return retform;
	}
    }
  else if (form->type == TYPE_CONS_PAIR)
    {
      reading_cons = form;
      must_copy = 0;

      while (SYMBOL (reading_cons) != &nil_object)
	{
	  do_spl = 0;
	  lastpr = NULL;
	  obj = reading_cons->type == TYPE_CONS_PAIR ? CAR (reading_cons)
	    : reading_cons;

	  ret = apply_backquote (obj, backts_commas_balance, env,
				 outcome,
				 reading_cons->type == TYPE_CONS_PAIR ? 0 : 2,
				 &do_spl, &lastpr);

	  if (!ret)
	    return NULL;

	  if (ret != obj)
	    {
	      must_copy = 1;
	      break;
	    }

	  if (reading_cons->type != TYPE_CONS_PAIR)
	    break;
	  else
	    reading_cons = CDR (reading_cons);
	}

      if (must_copy)
	{
	  cons = form;
	  retform = NULL;

	  while (cons != reading_cons)
	    {
	      if (!retform)
		retform = retcons = alloc_empty_cons_pair ();
	      else
		retcons = retcons->value_ptr.cons_pair->cdr =
		  alloc_empty_cons_pair ();

	      retcons->value_ptr.cons_pair->car = CAR (cons);

	      cons = CDR (cons);
	    }

	  while (SYMBOL (reading_cons) != &nil_object)
	    {
	      if (!ret)
		{
		  do_spl = 0;
		  lastpr = NULL;
		  obj = reading_cons->type == TYPE_CONS_PAIR ? CAR (reading_cons)
		    : reading_cons;

		  ret = apply_backquote (obj, backts_commas_balance, env,
					 outcome, reading_cons->type
					 == TYPE_CONS_PAIR ? 0 : 2, &do_spl,
					 &lastpr);

		  if (!ret)
		    return NULL;
		}

	      if (!do_spl)
		{
		  if (reading_cons->type == TYPE_CONS_PAIR)
		    {
		      if (!retform)
			retform = retcons = alloc_empty_cons_pair ();
		      else
			retcons = retcons->value_ptr.cons_pair->cdr =
			  alloc_empty_cons_pair ();

		      retcons->value_ptr.cons_pair->car = ret;
		      increment_refcount_by (retcons, refc - 1, NULL);
		    }
		  else
		    retcons->value_ptr.cons_pair->cdr = ret;
		}
	      else if (SYMBOL (ret) != &nil_object
		       && (!lastpr
			   || SYMBOL (lastpr->value_ptr.next) != &nil_object))
		{
		  if (do_spl == 2
		      || (SYMBOL (CDR (reading_cons)) == &nil_object && !lastpr))
		    {
		      if (retform)
			retcons = retcons->value_ptr.cons_pair->cdr =
			  lastpr ? lastpr->value_ptr.next : ret;
		      else
			retform = retcons =
			  lastpr ? lastpr->value_ptr.next : ret;

		      if (lastpr)
			{
			  while (SYMBOL (retcons) != &nil_object)
			    {
			      tmp = CAR (retcons);
			      retcons->value_ptr.cons_pair->car =
				copy_prefix (ret, lastpr, &lp,
					     retcons->refcount);
			      lp->value_ptr.next = tmp;

			      if (SYMBOL (CDR (retcons)) == &nil_object)
				break;
			      else
				retcons = CDR (retcons);
			    }
			}
		      else if (ret->type == TYPE_CONS_PAIR)
			retcons = last_cons_pair (ret);

		      refc = retcons->refcount;
		    }
		  else
		    {
		      cons = lastpr ? lastpr->value_ptr.next : ret;

		      while (SYMBOL (cons) != &nil_object)
			{
			  if (!retform)
			    retform = retcons = alloc_empty_cons_pair ();
			  else
			    retcons = retcons->value_ptr.cons_pair->cdr =
			      alloc_empty_cons_pair ();

			  if (lastpr)
			    {
			      retcons->value_ptr.cons_pair->car =
				copy_prefix (ret, lastpr, &lp,
					     retcons->refcount);
			      lp->value_ptr.next = CAR (cons);
			    }
			  else
			    retcons->value_ptr.cons_pair->car = CAR (cons);

			  cons = CDR (cons);
			}
		    }
		}

	      ret = NULL;

	      if (reading_cons->type != TYPE_CONS_PAIR)
		break;
	      else
		reading_cons = CDR (reading_cons);
	    }

	  if (!retform)
	    retform = &nil_object;
	  else if (retcons->type == TYPE_CONS_PAIR
		   && !retcons->value_ptr.cons_pair->cdr)
	    retcons->value_ptr.cons_pair->cdr = &nil_object;
	}
      else
	{
	  retform = form;

	  while (form->type == TYPE_CONS_PAIR)
	    {
	      form->refcount++;

	      form = CDR (form);

	      if (form->type != TYPE_CONS_PAIR)
		break;
	    }
	}

      return retform;
    }

  increment_refcount (form);
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

  if (!IS_SYMBOL (CAR (list)))
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
			   env->lex_env_funcs_boundary);

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

  while (SYMBOL (list) != &nil_object)
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
  return SYMBOL (obj) == &nil_object;
}


int
type_cons (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_CONS_PAIR;
}


int
type_atom (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct eval_outcome *outcome)
{
  return obj->type != TYPE_CONS_PAIR;
}


int
type_list (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_CONS_PAIR || SYMBOL (obj) == &nil_object;
}


int
type_symbol (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_SYMBOL || obj->type == TYPE_SYMBOL_NAME;
}


int
type_keyword (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct eval_outcome *outcome)
{
  return IS_SYMBOL (obj)
    && SYMBOL (obj)->value_ptr.symbol->home_package == env->keyword_package;
}


int
type_boolean (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct eval_outcome *outcome)
{
  return SYMBOL (obj) == &nil_object || SYMBOL (obj) == &t_object;
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
  return obj->type == TYPE_BIGNUM || obj->type == TYPE_RATIO
    || obj->type == TYPE_FLOAT;
}


int
type_real (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_BIGNUM || obj->type == TYPE_RATIO
    || obj->type == TYPE_FLOAT;
}


int
type_rational (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_BIGNUM || obj->type == TYPE_RATIO;
}


int
type_integer (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_BIGNUM;
}


int
type_bignum (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_BIGNUM;
}


int
type_fixnum (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_FIXNUM;
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
type_complex (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_COMPLEX;
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
    || obj->type == TYPE_CONS_PAIR || SYMBOL (obj) == &nil_object;
}


int
type_string (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_STRING;
}


int
type_hash_table (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_HASHTABLE;
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


int
type_file_stream (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_STREAM
    && obj->value_ptr.stream->medium == FILE_STREAM;
}


int
type_string_stream (const struct object *obj, const struct object *typespec,
		    struct environment *env, struct eval_outcome *outcome)
{
  return obj->type == TYPE_STREAM
    && obj->value_ptr.stream->medium == STRING_STREAM;
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

  if (SYMBOL (CAR (list)) == &nil_object)
    return &nil_object;

  if (CAR (list)->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  increment_refcount (CAR (CAR (list)));

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

  if (SYMBOL (CAR (list)) == &nil_object)
    return &nil_object;

  if (CAR (list)->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  increment_refcount (CDR (CAR (list)));

  return CDR (CAR (list));
}


struct object *
builtin_rplaca (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  decrement_refcount_by (CAR (CAR (list)), CAR (list)->refcount, CAR (list));

  CAR (list)->value_ptr.cons_pair->car = CAR (CDR (list));
  increment_refcount_by (CAR (CAR (list)), CAR (list)->refcount, CAR (list));

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_rplacd (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  decrement_refcount_by (CDR (CAR (list)), CAR (list)->refcount, CAR (list));

  CAR (list)->value_ptr.cons_pair->cdr = CAR (CDR (list));
  increment_refcount_by (CDR (CAR (list)), CAR (list)->refcount, CAR (list));

  increment_refcount (CAR (list));
  return CAR (list);
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

  increment_refcount (CAR (list));
  cons->value_ptr.cons_pair->car = CAR (list);

  increment_refcount (CAR (CDR (list)));
  cons->value_ptr.cons_pair->cdr = CAR (CDR (list));

  return cons;
}


struct object *
builtin_list (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  struct object *l = NULL, *cons, *last_cons;

  while (SYMBOL (list) != &nil_object)
    {
      cons = alloc_empty_cons_pair ();

      increment_refcount (CAR (list));
      cons->value_ptr.cons_pair->car = CAR (list);

      if (!l)
	l = last_cons = cons;
      else
	last_cons = last_cons->value_ptr.cons_pair->cdr = cons;

      list = CDR (list);
    }

  if (l)
    {
      last_cons->value_ptr.cons_pair->cdr = &nil_object;
      return l;
    }

  return &nil_object;
}


struct object *
builtin_list_star (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *l = NULL, *cons, *last_cons;

  if (SYMBOL (list) == &nil_object)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (SYMBOL (CDR (list)) == &nil_object)
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }

  while (SYMBOL (list) != &nil_object)
    {
      cons = alloc_empty_cons_pair ();

      increment_refcount (CAR (list));
      cons->value_ptr.cons_pair->car = CAR (list);

      if (!l)
	l = last_cons = cons;
      else
	last_cons = last_cons->value_ptr.cons_pair->cdr = cons;

      list = CDR (list);

      if (CDR (list) == &nil_object)
	break;
    }

  increment_refcount (CAR (list));
  cons->value_ptr.cons_pair->cdr = CAR (list);

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

      if (obj->type != TYPE_CONS_PAIR && SYMBOL (obj) != &nil_object)
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

  increment_refcount (obj);

  return ret;
}


struct object *
builtin_nconc (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  int i, l = list_length (list);
  struct object *argcons = list, *lastcons = NULL;

  if (!l)
    return &nil_object;

  for (i = 0; i < l-1; i++)
    {
      if (!IS_LIST (CAR (argcons)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      argcons = CDR (argcons);
    }

  increment_refcount (CAR (list));

  argcons = list;

  for (i = 0; i < l; i++)
    {
      if (lastcons)
	lastcons->value_ptr.cons_pair->cdr = CAR (argcons);

      if (CAR (argcons)->type == TYPE_CONS_PAIR)
	lastcons = last_cons_pair (CAR (argcons));

      argcons = CDR (argcons);

      if (lastcons)
	increment_refcount_by (CAR (argcons), lastcons->refcount, lastcons);
    }

  return CAR (list);
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

  if (CAR (list)->type != TYPE_BIGNUM
      || (CAR (CDR (list))->type != TYPE_CONS_PAIR
	  && SYMBOL (CAR (CDR (list))) != &nil_object))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = nth (mpz_get_ui (CAR (list)->value_ptr.integer), CAR (CDR (list)));

  increment_refcount (ret);

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

  if (CAR (list)->type != TYPE_BIGNUM
      || (CAR (CDR (list))->type != TYPE_CONS_PAIR
	  && SYMBOL (CAR (CDR (list))) != &nil_object))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = nthcdr (mpz_get_ui (CAR (list)->value_ptr.integer), CAR (CDR (list)));

  increment_refcount (ret);

  return ret;
}


struct object *
builtin_nth_value (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  int ind;
  struct object *res, *ret;
  struct object_list *l;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_BIGNUM)
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

  res = evaluate_object (CAR (CDR (list)), env, outcome);

  if (!res)
    return NULL;

  if (!ind)
    {
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
      return res;
    }

  decrement_refcount (res);

  for (l = outcome->other_values, ind--; l && ind; ind--)
    l = l->next;

  if (!l)
    {
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
      return &nil_object;
    }

  ret = l->obj;
  increment_refcount (ret);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
  return ret;
}


struct object *
builtin_elt (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  size_t ind;
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (CDR (list))->type != TYPE_BIGNUM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (mpz_cmp_si (CAR (CDR (list))->value_ptr.integer, 0) < 0)
    {
      outcome->type = OUT_OF_BOUND_INDEX;
      return NULL;
    }

  ind = mpz_get_ui (CAR (CDR (list))->value_ptr.integer);

  if (CAR (list)->type == TYPE_STRING)
    {
      ret = get_nth_character (CAR (list), ind);

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

      increment_refcount (ret);
      return ret;
    }
  else if (CAR (list)->type == TYPE_CONS_PAIR
	   || SYMBOL (CAR (list)) == &nil_object)
    {
      if (!is_proper_list (CAR (list)))
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

      increment_refcount (ret);
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
  struct object *arr, *ret, *lin_ind;
  int ind, l = list_length (list);

  if (!l)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  arr = CAR (list);

  if (arr->type == TYPE_STRING)
    {
      list = CDR (list);

      if (l != 2)
	{
	  outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
	  return NULL;
	}

      if (CAR (list)->type != TYPE_BIGNUM)
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

      ret = get_nth_character (arr, ind);

      if (!ret)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      return ret;
    }
  else if (arr->type == TYPE_ARRAY)
    {
      lin_ind = builtin_array_row_major_index (list, env, outcome);

      if (!lin_ind)
	return NULL;

      ind = mpz_get_si (lin_ind->value_ptr.integer);

      increment_refcount (arr->value_ptr.array->value [ind]);
      return arr->value_ptr.array->value [ind];
    }

  outcome->type = WRONG_TYPE_OF_ARGUMENT;
  return NULL;
}


struct object *
builtin_row_major_aref (struct object *list, struct environment *env,
			struct eval_outcome *outcome)
{
  int ind;
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_ARRAY (CAR (list)) || CAR (CDR (list))->type != TYPE_BIGNUM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ind = mpz_get_si (CAR (CDR (list))->value_ptr.integer);

  if (CAR (list)->type == TYPE_STRING)
    {
      if (ind < 0)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      ret = get_nth_character (CAR (list), ind);

      if (!ret)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      return ret;
    }

  if (ind < 0 || ind >= array_total_size (CAR (list)->value_ptr.array))
    {
      outcome->type = OUT_OF_BOUND_INDEX;
      return NULL;
    }

  increment_refcount (CAR (list)->value_ptr.array->value [ind]);
  return CAR (list)->value_ptr.array->value [ind];
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

  if (CAR (list)->type != TYPE_CONS_PAIR && SYMBOL (CAR (list)) != &nil_object)
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

  return create_integer_from_long (list_length (CAR (list)));
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
      && seq->type != TYPE_ARRAY && SYMBOL (seq) != &nil_object)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (seq->type == TYPE_STRING)
    {
      return create_integer_from_long (string_utf8_length (seq));
    }
  else if (seq->type == TYPE_CONS_PAIR || SYMBOL (seq) == &nil_object)
    {
      return create_integer_from_long (list_length (seq));
    }
  else
    {
      if (array_rank (seq->value_ptr.array) != 1)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (seq->value_ptr.array->fill_pointer >= 0)
	return create_integer_from_long (seq->value_ptr.array->fill_pointer);

      return create_integer_from_long (seq->value_ptr.array->alloc_size->size);
    }
}


struct object *
builtin_fill_pointer (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_VECTOR (CAR (list)) || !HAS_FILL_POINTER (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    return create_integer_from_long (CAR (list)->value_ptr.string->fill_pointer);
  else
    return create_integer_from_long (CAR (list)->value_ptr.array->fill_pointer);
}


struct object *
builtin_make_array (struct object *list, struct environment *env,
		    struct eval_outcome *outcome)
{
  int indx, tot = 1;
  struct object *ret, *cons;
  struct array_size *size = NULL, *sz;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_BIGNUM)
    {
      indx = mpz_get_si (CAR (list)->value_ptr.integer);

      if (indx < 0)
	{
	  outcome->type = INVALID_SIZE;
	  return NULL;
	}

      return alloc_vector (indx, 1, 0);
    }
  else if (CAR (list)->type == TYPE_CONS_PAIR)
    {
      cons = CAR (list);

      while (SYMBOL (cons) != &nil_object)
	{
	  if (CAR (cons)->type != TYPE_BIGNUM)
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  indx = mpz_get_si (CAR (cons)->value_ptr.integer);

	  if (indx < 0)
	    {
	      outcome->type = INVALID_SIZE;
	      return NULL;
	    }

	  if (size)
	    sz = sz->next = malloc_and_check (sizeof (*sz->next));
	  else
	    size = sz = malloc_and_check (sizeof (*sz->next));

	  sz->size = indx;
	  tot *= indx;

	  cons = CDR (cons);
	}

      ret = alloc_vector (tot, 1, 1);
      sz->next = NULL;
      ret->value_ptr.array->alloc_size = size;
    }
  else if (SYMBOL (CAR (list)) == &nil_object)
    {
      ret = alloc_vector (1, 1, 1);
      ret->value_ptr.array->alloc_size = 0;
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return ret;
}


struct object *
builtin_array_has_fill_pointer_p (struct object *list, struct environment *env,
				  struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_ARRAY (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (HAS_FILL_POINTER (CAR (list)))
    return &t_object;

  return &nil_object;
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

      num = alloc_number (TYPE_BIGNUM);

      mpz_set_ui (num->value_ptr.integer, arr->value_ptr.string->used_size);
      ret->value_ptr.cons_pair->car = num;
      ret->value_ptr.cons_pair->cdr = &nil_object;
    }
  else if (arr->type == TYPE_ARRAY)
    {
      sz = arr->value_ptr.array->alloc_size;

      if (!sz)
	return &nil_object;

      while (sz)
	{
	  if (!ret)
	    ret = cons = alloc_empty_cons_pair ();
	  else
	    cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

	  num = alloc_number (TYPE_BIGNUM);

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
builtin_array_row_major_index (struct object *list, struct environment *env,
			       struct eval_outcome *outcome)
{
  struct array_size *sz, *sz2;
  int l = list_length (list);
  struct object *arr;
  int ind, tot, rest;

  if (!l)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_ARRAY (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  arr = CAR (list);
  list = CDR (list);

  if (arr->type == TYPE_STRING)
    {
      if (l != 2)
	{
	  outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
	  return NULL;
	}

      if (CAR (list)->type != TYPE_BIGNUM)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      tot = mpz_get_si (CAR (list)->value_ptr.integer);

      if (tot < 0 || tot >= arr->value_ptr.string->used_size)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      increment_refcount (CAR (list));
      return CAR (list);
    }

  sz = arr->value_ptr.array->alloc_size;
  tot = 0;

  while (SYMBOL (list) != &nil_object)
    {
      if (!sz)
	{
	  outcome->type = WRONG_NUMBER_OF_AXIS;
	  return NULL;
	}

      if (CAR (list)->type != TYPE_BIGNUM)
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

      if (ind)
	{
	  sz2 = sz->next;
	  rest = 1;

	  while (sz2)
	    {
	      rest *= sz2->size;
	      sz2 = sz2->next;
	    }

	  tot += ind * rest;
	}

      list = CDR (list);
      sz = sz->next;
    }

  if (sz)
    {
      outcome->type = WRONG_NUMBER_OF_AXIS;
      return NULL;
    }

  return create_integer_from_long (tot);
}


struct object *
builtin_make_hash_table (struct object *list, struct environment *env,
			 struct eval_outcome *outcome)
{
  struct object *ret;
  struct hashtable *ht;

  if (SYMBOL (list) != &nil_object)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  ret = malloc_and_check (sizeof (*ret));
  ht = malloc_and_check (sizeof (*ht));

  ht->table = calloc_and_check (LISP_HASHTABLE_SIZE, sizeof (*ht->table));

  ret->refcount = 1;
  ret->type = TYPE_HASHTABLE;
  ret->value_ptr.hashtable = ht;

  return ret;
}


struct object *
builtin_hash_table_size (struct object *list, struct environment *env,
			 struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_HASHTABLE)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return create_integer_from_long (LISP_HASHTABLE_SIZE);
}


struct object *
builtin_hash_table_count (struct object *list, struct environment *env,
			  struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_HASHTABLE)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return create_integer_from_long
    (hash_table_count (CAR (list)->value_ptr.hashtable));
}


struct object *
builtin_hash_table_test (struct object *list, struct environment *env,
			 struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_HASHTABLE)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return intern_symbol_from_char_vector ("EQ", strlen ("EQ"), 1,
					 EXTERNAL_VISIBILITY, 1, env->cl_package);
}


struct object *
builtin_gethash (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct object *ret, *pres = &t_object;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (CDR (list))->type != TYPE_HASHTABLE)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = CAR (CDR (list))->value_ptr.hashtable->table
    [hash_object_respecting_eq (CAR (list), LISP_HASHTABLE_SIZE)];

  if (!ret)
    {
      ret = &nil_object;
      pres = &nil_object;
    }

  increment_refcount (ret);
  prepend_object_to_obj_list (pres, &outcome->other_values);
  return ret;
}


struct object *
builtin_remhash (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct object *cell;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (CDR (list))->type != TYPE_HASHTABLE)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  cell = CAR (CDR (list))->value_ptr.hashtable->table
    [hash_object_respecting_eq (CAR (list), LISP_HASHTABLE_SIZE)];

  if (cell)
    {
      decrement_refcount_by (cell, CAR (CDR (list))->refcount, CAR (CDR (list)));
      CAR (CDR (list))->value_ptr.hashtable->table
	[hash_object_respecting_eq (CAR (list), LISP_HASHTABLE_SIZE)] = NULL;

      return &t_object;
    }

  return &nil_object;
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

  if ((CAR (list)->type != TYPE_CONS_PAIR && SYMBOL (CAR (list)) != &nil_object)
      || (length == 2 && CAR (CDR (list))->type != TYPE_BIGNUM))
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

  increment_refcount (ret);
  return ret;
}


struct object *
builtin_read_line (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  int l = list_length (list), ch, sz = 32, i, eof;
  struct stream *s;
  struct object *ret, *str;
  char *in;

  if (l > 1)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (l && CAR (list)->type != TYPE_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  str = l ? CAR (list) : inspect_variable (env->std_in_sym, env);
  s = str->value_ptr.stream;

  if (s->medium == FILE_STREAM)
    {
      i = 0, eof = 0;

      in = malloc_and_check (sz);

      ch = getc (s->file);

      while (ch != '\n')
	{
	  if (ch == EOF)
	    {
	      eof = 1;
	      break;
	    }

	  if (i == sz)
	    {
	      sz *= 2;
	      in = realloc_and_check (in, sz);
	    }

	  in [i++] = ch;

	  ch = getc (s->file);
	}

      ret = create_string_from_char_vector (in, i, 0);
      ret->value_ptr.string->alloc_size = sz;
    }
  else if (s->medium == STRING_STREAM)
    {
      eof = 1;

      for (i = 0; i < s->string->value_ptr.string->used_size; i++)
	{
	  if (s->string->value_ptr.string->value [i] == '\n')
	    {
	      eof = 0;
	      break;
	    }
	}

      if (eof)
	{
	  ret = s->string;
	  decrement_refcount_by (ret, str->refcount-1, NULL);

	  s->string = alloc_string (0);
	  increment_refcount_by (s->string, str->refcount-1, NULL);
	}
      else
	{
	  ret = s->string;

	  s->string = create_string_from_char_vector
	    (ret->value_ptr.string->value+i+1,
	     ret->value_ptr.string->used_size-i-1, 1);
	  increment_refcount_by (s->string, str->refcount-1, str);

	  resize_string_allocation (ret, i);
	  decrement_refcount_by (ret, str->refcount-1, NULL);
	}
    }

  if (eof)
    prepend_object_to_obj_list (&t_object, &outcome->other_values);
  else
    prepend_object_to_obj_list (&nil_object, &outcome->other_values);

  return ret;
}


struct object *
builtin_eval (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  int lex_vars = env->lex_env_vars_boundary;
  int lex_funcs = env->lex_env_funcs_boundary;
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  env->lex_env_vars_boundary = env->lex_env_funcs_boundary = 0;

  ret = evaluate_object (CAR (list), env, outcome);

  env->lex_env_vars_boundary = lex_vars;
  env->lex_env_funcs_boundary = lex_funcs;

  return ret;
}


struct object *
builtin_write (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  struct object *obj, *str = NULL;

  if (list_length (list) < 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  obj = CAR (list);
  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":STREAM", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (CAR (CDR (list))->type == TYPE_STREAM)
	    str = CAR (CDR (list));
	  else
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else
	{
	  outcome->type = UNKNOWN_KEYWORD_ARGUMENT;
	  return NULL;
	}

	list = CDR (list);
    }

  if (!str)
    str = inspect_variable (env->std_out_sym, env);

  print_object (obj, env, str->value_ptr.stream);

  increment_refcount (obj);
  return obj;
}


struct object *
builtin_write_string (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  struct string *s;
  struct stream *str;
  int l;

  if (!(l = list_length (list)) || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STRING
      || (l == 2 && CAR (CDR (list))->type != TYPE_STREAM))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  s = CAR (list)->value_ptr.string;

  str = l == 2 ? CAR (CDR (list))->value_ptr.stream
    : inspect_variable (env->std_out_sym, env)->value_ptr.stream;

  write_to_stream (str, s->value, s->used_size);

  if (s->value [s->used_size - 1] == '\n')
    str->dirty_line = 0;
  else
    str->dirty_line = 1;

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_write_char (struct object *list, struct environment *env,
		    struct eval_outcome *outcome)
{
  struct stream *str;
  int l;

  if (!(l = list_length (list)) || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CHARACTER
      || (l == 2 && CAR (CDR (list))->type != TYPE_STREAM))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 2)
    str = CAR (CDR (list))->value_ptr.stream;
  else
    str = inspect_variable (env->std_out_sym, env)->value_ptr.stream;

  write_to_stream (str, CAR (list)->value_ptr.character,
		   strlen (CAR (list)->value_ptr.character));

  if (!strcmp (CAR (list)->value_ptr.character, "\n"))
    str->dirty_line = 0;
  else
    str->dirty_line = 1;

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_write_byte (struct object *list, struct environment *env,
		    struct eval_outcome *outcome)
{
  char b;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_BIGNUM || CAR (CDR (list))->type != TYPE_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  b = mpz_get_ui (CAR (list)->value_ptr.integer);

  write_to_stream (CAR (CDR (list))->value_ptr.stream, &b, 1);

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_fresh_line (struct object *list, struct environment *env,
		    struct eval_outcome *outcome)
{
  struct object *std_out;

  if (list_length (list))
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  std_out = inspect_variable (env->std_out_sym, env);

  return fresh_line (std_out->value_ptr.stream);
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

  fn = copy_string_to_c_string (CAR (list)->value_ptr.string);

  ret = load_file (fn, env, outcome);

  free (fn);

  return ret;
}


struct object *
builtin_open (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  enum stream_direction dir = INPUT_STREAM;
  struct string *ns;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_FILENAME && CAR (list)->type != TYPE_STRING)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ns = CAR (list)->type == TYPE_FILENAME
    ? CAR (list)->value_ptr.filename->value->value_ptr.string
    : CAR (list)->value_ptr.string;
  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":DIRECTION", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

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
	  outcome->type = UNKNOWN_KEYWORD_ARGUMENT;
	  return NULL;
	}

      list = CDR (list);
    }

  return create_file_stream (BINARY_STREAM, dir, ns, outcome);
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

  if (s->medium == FILE_STREAM)
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
builtin_input_stream_p (struct object *list, struct environment *env,
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

  if (CAR (list)->value_ptr.stream->direction == INPUT_STREAM)
    return &t_object;
  else
    return &nil_object;
}


struct object *
builtin_output_stream_p (struct object *list, struct environment *env,
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

  if (CAR (list)->value_ptr.stream->direction == OUTPUT_STREAM)
    return &t_object;
  else
    return &nil_object;
}


struct object *
builtin_interactive_stream_p (struct object *list, struct environment *env,
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

  return &nil_object;
}


struct object *
builtin_make_string_input_stream (struct object *list, struct environment *env,
				  struct eval_outcome *outcome)
{
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

  return create_string_stream (INPUT_STREAM, CAR (list));
}


struct object *
builtin_make_string_output_stream (struct object *list, struct environment *env,
				   struct eval_outcome *outcome)
{
  if (SYMBOL (list) != &nil_object)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  return create_string_stream (OUTPUT_STREAM, NULL);
}


struct object *
builtin_get_output_stream_string (struct object *list, struct environment *env,
				  struct eval_outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STREAM
      || CAR (list)->value_ptr.stream->medium != STRING_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = CAR (list)->value_ptr.stream->string;
  decrement_refcount_by (ret, CAR (list)->refcount-1, NULL);

  CAR (list)->value_ptr.stream->string = alloc_string (0);
  increment_refcount_by (CAR (list)->value_ptr.stream->string,
			 CAR (list)->refcount-1, NULL);

  return ret;
}


struct object *
builtin_upper_case_p (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  char *ch;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ch = CAR (list)->value_ptr.character;

  if (strlen (ch) > 1)
    return &nil_object;
  else if (isupper ((unsigned char) *ch))
    return &t_object;

  return &nil_object;
}


struct object *
builtin_lower_case_p (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  char *ch;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ch = CAR (list)->value_ptr.character;

  if (strlen (ch) > 1)
    return &nil_object;
  else if (islower ((unsigned char) *ch))
    return &t_object;

  return &nil_object;
}


struct object *
builtin_both_case_p (struct object *list, struct environment *env,
		     struct eval_outcome *outcome)
{
  char *ch;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ch = CAR (list)->value_ptr.character;

  if (strlen (ch) > 1)
    return &nil_object;
  else if (isupper ((unsigned char) *ch) || islower ((unsigned char) *ch))
    return &t_object;

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

  if (IS_SYMBOL (CAR (list)))
    arg1 = SYMBOL (CAR (list));
  else
    arg1 = CAR (list);

  if (IS_SYMBOL (CAR (CDR (list))))
    arg2 = SYMBOL (CAR (CDR (list)));
  else
    arg2 = CAR (CDR (list));

  if (arg1 == arg2)
    return &t_object;

  return &nil_object;
}


struct object *
builtin_eql (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  struct object *arg1, *arg2;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (IS_SYMBOL (CAR (list)))
    arg1 = SYMBOL (CAR (list));
  else
    arg1 = CAR (list);

  if (IS_SYMBOL (CAR (CDR (list))))
    arg2 = SYMBOL (CAR (CDR (list)));
  else
    arg2 = CAR (CDR (list));

  if (arg1 == arg2)
    return &t_object;

  if ((IS_RATIONAL (arg1) && IS_RATIONAL (arg2))
      || (arg1->type == TYPE_FLOAT && arg2->type == TYPE_FLOAT))
    {
      if (!compare_two_numbers (arg1, arg2))
	return &t_object;
      else
	return &nil_object;
    }

  if (arg1->type == TYPE_CHARACTER && arg2->type == TYPE_CHARACTER)
    {
      if (!strcmp (arg1->value_ptr.character, arg2->value_ptr.character))
	return &t_object;
    }

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

  if (SYMBOL (CAR (list)) == &nil_object)
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
	  resize_string_allocation (ret, ret->value_ptr.string->alloc_size
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

  env->blocks = add_block (&nil_object, env->blocks);

  count = evaluate_object (CAR (CDR (CAR (list))), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!count)
    {
      env->blocks = remove_block (env->blocks);

      if (outcome->block_to_leave == &nil_object)
	{
	  outcome->block_to_leave = NULL;
	  return outcome->return_value;
	}
      else
	return NULL;
    }

  if (count->type != TYPE_BIGNUM)
    {
      env->blocks = remove_block (env->blocks);
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      decrement_refcount (count);
      return NULL;
    }

  cnt = mpz_get_si (count->value_ptr.integer);
  decrement_refcount (count);
  var = SYMBOL (CAR (CAR (list)));

  for (i = 0; i < cnt; i++)
    {
      env->vars = bind_variable (var, create_integer_from_long (i), env->vars);

      env->lex_env_vars_boundary++;

      ret = evaluate_body (CDR (list), 1, NULL, env, outcome);

      env->lex_env_vars_boundary--;

      env->vars = remove_bindings (env->vars, 1);

      if (!ret)
	{
	  env->blocks = remove_block (env->blocks);

	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      return outcome->return_value;
	    }
	  else
	    return NULL;
	}

      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
      decrement_refcount (ret);
    }

  if (l == 3)
    {
      env->vars = bind_variable (var, create_integer_from_long (i), env->vars);

      env->lex_env_vars_boundary++;

      ret = evaluate_object (nth (2, CAR (list)), env, outcome);

      env->lex_env_vars_boundary--;

      env->vars = remove_bindings (env->vars, 1);

      if (!ret)
	{
	  env->blocks = remove_block (env->blocks);

	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      return outcome->return_value;
	    }
	  else
	    return NULL;
	}

      env->blocks = remove_block (env->blocks);

      return ret;
    }

  env->blocks = remove_block (env->blocks);

  return &nil_object;
}


struct object *
builtin_dolist (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *var, *lst, *cons, *ret;
  int l;

  if (list_length (list) < 1 || CAR (list)->type != TYPE_CONS_PAIR
      || (l = list_length (CAR (list))) < 2 || l > 3
      || !IS_SYMBOL (CAR (CAR (list))))
    {
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      return NULL;
    }

  env->blocks = add_block (&nil_object, env->blocks);

  lst = evaluate_object (CAR (CDR (CAR (list))), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!lst)
    {
      env->blocks = remove_block (env->blocks);

      if (outcome->block_to_leave == &nil_object)
	{
	  outcome->block_to_leave = NULL;
	  return outcome->return_value;
	}
      else
	return NULL;
    }

  if (lst->type != TYPE_CONS_PAIR && SYMBOL (lst) != &nil_object)
    {
      env->blocks = remove_block (env->blocks);
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      decrement_refcount (lst);
      return NULL;
    }

  var = SYMBOL (CAR (CAR (list)));
  cons = lst;

  while (SYMBOL (cons) != &nil_object)
    {
      increment_refcount (CAR (cons));
      env->vars = bind_variable (var, CAR (cons), env->vars);

      env->lex_env_vars_boundary++;

      ret = evaluate_body (CDR (list), 1, NULL, env, outcome);

      env->lex_env_vars_boundary--;

      env->vars = remove_bindings (env->vars, 1);

      if (!ret)
	{
	  env->blocks = remove_block (env->blocks);

	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      return outcome->return_value;
	    }
	  else
	    return NULL;
	}

      decrement_refcount (ret);

      cons = CDR (cons);
    }

  if (l == 3)
    {
      env->vars = bind_variable (var, &nil_object, env->vars);

      env->lex_env_vars_boundary++;

      ret = evaluate_object (nth (2, CAR (list)), env, outcome);

      env->lex_env_vars_boundary--;

      env->vars = remove_bindings (env->vars, 1);

      decrement_refcount (lst);

      if (!ret)
	{
	  env->blocks = remove_block (env->blocks);

	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      return outcome->return_value;
	    }
	  else
	    return NULL;
	}

      env->blocks = remove_block (env->blocks);

      return ret;
    }

  env->blocks = remove_block (env->blocks);
  decrement_refcount (lst);
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

      if (SYMBOL (nth (i, list)) == &nil_object)
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
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

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

	  if (SYMBOL (CAR (cdrlistcons)) == &nil_object)
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
builtin_remove_if (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *fun, *seq, *ret, *cons, *arg, *res;
  size_t sz, off, i, j;
  char *s, *out;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_FUNCTION || !IS_SEQUENCE (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  fun = CAR (list);
  seq = CAR (CDR (list));

  if (SYMBOL (seq) == &nil_object)
    return &nil_object;

  arg = alloc_empty_cons_pair ();
  arg->value_ptr.cons_pair->cdr = &nil_object;

  if (seq->type == TYPE_CONS_PAIR)
    {
      ret = &nil_object;

      while (SYMBOL (seq) != &nil_object)
	{
	  arg->value_ptr.cons_pair->car = CAR (seq);

	  res = call_function (fun, arg, 1, 0, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!res)
	    return NULL;

	  if (SYMBOL (res) == &nil_object)
	    {
	      if (SYMBOL (ret) == &nil_object)
		ret = cons = alloc_empty_cons_pair ();
	      else
		cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

	      cons->value_ptr.cons_pair->car = CAR (seq);
	      increment_refcount (CAR (seq));
	    }

	  decrement_refcount (res);

	  seq = CDR (seq);
	}

      cons->value_ptr.cons_pair->cdr = &nil_object;
    }
  else if (seq->type == TYPE_STRING)
    {
      sz = seq->value_ptr.string->used_size;

      ret = alloc_string (sz);

      out = ret->value_ptr.string->value;

      s = seq->value_ptr.string->value;
      off = 0;

      do
	{
	  s += off;
	  sz -= off;

	  arg->value_ptr.cons_pair->car = create_character_from_utf8 (s, sz);

	  res = call_function (fun, arg, 1, 0, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!res)
	    return NULL;

	  if (SYMBOL (res) == &nil_object)
	    {
	      strcpy (out, CAR (arg)->value_ptr.character);

	      out += strlen (CAR (arg)->value_ptr.character);

	      ret->value_ptr.string->used_size +=
		strlen (CAR (arg)->value_ptr.character);
	    }
	  else
	    decrement_refcount (CAR (arg));

	  decrement_refcount (res);

	} while ((off = next_utf8_char (s, sz)));
    }
  else if (seq->type == TYPE_ARRAY)
    {
      ret = alloc_vector (seq->value_ptr.array->alloc_size->size, 0, 0);

      j = 0;

      for (i = 0; i < seq->value_ptr.array->alloc_size->size; i++)
	{
	  arg->value_ptr.cons_pair->car = seq->value_ptr.array->value [i];
	  increment_refcount (CAR (arg));

	  res = call_function (fun, arg, 1, 0, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!res)
	    return NULL;

	  if (SYMBOL (res) == &nil_object)
	    ret->value_ptr.array->value [j++] = CAR (arg);
	  else
	    decrement_refcount (CAR (arg));

	  decrement_refcount (res);
	}

      resize_vector (ret, j);
    }

  free_cons_pair (arg);
  return ret;
}


struct object *
builtin_reverse (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct object *seq, *ret, *cons;
  size_t sz, i;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_SEQUENCE (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  seq = CAR (list);

  if (SYMBOL (seq) == &nil_object)
    return &nil_object;

  if (seq->type == TYPE_CONS_PAIR)
    {
      ret = &nil_object;

      while (SYMBOL (seq) != &nil_object)
	{
	  cons = alloc_empty_cons_pair ();

	  increment_refcount (CAR (seq));
	  cons->value_ptr.cons_pair->car = CAR (seq);

	  cons->value_ptr.cons_pair->cdr = ret;

	  ret = cons;
	  seq = CDR (seq);
	}
    }
  else if (seq->type == TYPE_STRING)
    {
      sz = seq->value_ptr.string->used_size;

      ret = alloc_string (sz);
      ret->value_ptr.string->used_size = sz;

      for (i = sz; i > 0; i--)
	{
	  ret->value_ptr.string->value [sz-i] =
	    seq->value_ptr.string->value [i-1];
	}
    }
  else if (seq->type == TYPE_ARRAY)
    {
      sz = seq->value_ptr.array->alloc_size->size;

      ret = alloc_vector (sz, 0, 0);

      for (i = 0; i < sz; i++)
	{
	  increment_refcount (seq->value_ptr.array->value [i]);

	  ret->value_ptr.array->value [i] = seq->value_ptr.array->value [sz-i-1];
	}
    }

  return ret;
}


struct object *
accessor_car (struct object *list, struct object *newval,
	      struct environment *env, struct eval_outcome *outcome)
{
  struct object *obj;

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

  increment_refcount_by (newval, obj->refcount - 1, obj);
  decrement_refcount_by (obj->value_ptr.cons_pair->car, obj->refcount, obj);

  obj->value_ptr.cons_pair->car = newval;

  return newval;
}


struct object *
accessor_cdr (struct object *list, struct object *newval,
	      struct environment *env, struct eval_outcome *outcome)
{
  struct object *obj;

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

  increment_refcount_by (newval, obj->refcount - 1, obj);
  decrement_refcount_by (obj->value_ptr.cons_pair->cdr, obj->refcount, obj);

  obj->value_ptr.cons_pair->cdr = newval;

  return newval;
}


struct object *
accessor_aref (struct object *list, struct object *newval,
	       struct environment *env, struct eval_outcome *outcome)
{
  struct object *ret, *lin_ind;
  int l = list_length (list), ind;

  if (l < 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  list = evaluate_through_list (CDR (list), env, outcome);

  if (!list)
    return NULL;

  if (!IS_ARRAY (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      if (l != 3)
	{
	  outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
	  return NULL;
	}

      if (CAR (CDR (list))->type != TYPE_BIGNUM)
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

      if (newval->type != TYPE_CHARACTER)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = set_nth_character (CAR (list), ind, newval->value_ptr.character);

      if (!ret)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}
    }
  else
    {
      lin_ind = builtin_array_row_major_index (list, env, outcome);

      if (!lin_ind)
	return NULL;

      ind = mpz_get_si (lin_ind->value_ptr.integer);

      increment_refcount_by (newval, CAR (list)->refcount-1, CAR (list));
      decrement_refcount_by (CAR (list)->value_ptr.array->value [ind],
			     CAR (list)->refcount, CAR (list));

      CAR (list)->value_ptr.array->value [ind] = newval;

      ret = newval;
    }

  decrement_refcount (list);

  return ret;
}


struct object *
accessor_elt (struct object *list, struct object *newval,
	      struct environment *env, struct eval_outcome *outcome)
{
  struct object *ret, *cons;
  int ind;

  if (list_length (list) != 3)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  list = evaluate_through_list (CDR (list), env, outcome);

  if (!list)
    return NULL;

  if (!IS_SEQUENCE (CAR (list)) || CAR (CDR (list))->type != TYPE_BIGNUM)
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
      if (newval->type != TYPE_CHARACTER)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = set_nth_character (CAR (list), ind, newval->value_ptr.character);

      if (!ret)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}
    }
  else if (CAR (list)->type == TYPE_ARRAY)
    {
      increment_refcount_by (newval, CAR (list)->refcount-1, CAR (list));
      decrement_refcount_by (CAR (list)->value_ptr.array->value [ind],
			     CAR (list)->refcount, CAR (list));

      CAR (list)->value_ptr.array->value [ind] = newval;

      ret = newval;
    }
  else
    {
      if (ind >= list_length (CAR (list)))
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      cons = nthcdr (ind, CAR (list));

      increment_refcount_by (newval, cons->refcount-1, cons);
      decrement_refcount_by (CAR (cons), cons->refcount, cons);

      cons->value_ptr.cons_pair->car = newval;

      ret = newval;
    }

  decrement_refcount (list);

  return ret;
}


struct object *
accessor_gethash (struct object *list, struct object *newval,
		  struct environment *env, struct eval_outcome *outcome)
{
  struct object *key, *table;
  int ind;

  if (list_length (list) != 3)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  key = evaluate_object (CAR (CDR (list)), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!key)
    return NULL;

  table = evaluate_object (CAR (CDR (CDR (list))), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!table)
    return NULL;

  if (table->type != TYPE_HASHTABLE)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ind = hash_object_respecting_eq (key, LISP_HASHTABLE_SIZE);

  increment_refcount_by (newval, table->refcount, table);

  decrement_refcount_by (table->value_ptr.hashtable->table [ind],
			 table->refcount, table);

  table->value_ptr.hashtable->table [ind] = newval;

  return newval;
}


int
compare_two_numbers (struct object *num1, struct object *num2)
{
  enum object_type tp = highest_num_type (num1->type, num2->type);
  struct object *first_p = promote_number (num1, tp);
  struct object *second_p = promote_number (num2, tp);
  int eq;

  if (tp == TYPE_BIGNUM)
    eq = mpz_cmp (first_p->value_ptr.integer, second_p->value_ptr.integer);
  else if (tp == TYPE_RATIO)
    eq = mpq_cmp (first_p->value_ptr.ratio, second_p->value_ptr.ratio);
  else
    eq = mpf_cmp (first_p->value_ptr.floating, second_p->value_ptr.floating);

  decrement_refcount (first_p);
  decrement_refcount (second_p);

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
  return (num->type == TYPE_BIGNUM && !mpz_sgn (num->value_ptr.integer))
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

  if (t == TYPE_BIGNUM
      && mpz_divisible_p (n1->value_ptr.integer, n2->value_ptr.integer))
    {
      ret = alloc_number (TYPE_BIGNUM);
      mpz_divexact (ret->value_ptr.integer, n1->value_ptr.integer,
		    n2->value_ptr.integer);
    }
  else if (t == TYPE_BIGNUM || t == TYPE_RATIO)
    {
      pn1 = promote_number (n1, TYPE_RATIO);
      pn2 = promote_number (n2, TYPE_RATIO);

      ret = alloc_number (TYPE_RATIO);

      mpq_div (ret->value_ptr.ratio, pn1->value_ptr.ratio, pn2->value_ptr.ratio);

      decrement_refcount (pn1);
      decrement_refcount (pn2);
    }
  else
    {
      pn1 = promote_number (n1, TYPE_FLOAT);
      pn2 = promote_number (n2, TYPE_FLOAT);

      ret = alloc_number (TYPE_FLOAT);

      mpf_div (ret->value_ptr.floating, pn1->value_ptr.floating,
	       pn2->value_ptr.floating);

      decrement_refcount (pn1);
      decrement_refcount (pn2);
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

  return TYPE_BIGNUM;
}


struct object *
copy_number (const struct object *num)
{
  struct object *ret = malloc_and_check (sizeof (*ret));

  ret->refcount = 1;
  ret->type = num->type;

  if (num->type == TYPE_BIGNUM)
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
      increment_refcount (num);
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

      if (num->type == TYPE_BIGNUM)
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

      if (ret->type == TYPE_BIGNUM)
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

      decrement_refcount (op);

      list = CDR (list);

    } while (SYMBOL (list) != &nil_object);

  return ret;
}


struct object *
perform_division_with_remainder (struct object *args,
				 enum rounding_behavior round_behavior,
				 enum object_type quotient_type,
				 struct eval_outcome *outcome)
{
  int l = list_length (args);
  enum object_type rem_type, op_type;
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

  if (!IS_REAL (CAR (args)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 2)
    {
      if (!IS_REAL (CAR (CDR (args))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      div_ = CAR (CDR (args));
    }
  else
    div_ = create_integer_from_long (1);

  rem_type = highest_num_type (CAR (args)->type, div_->type);

  if (rem_type == TYPE_BIGNUM && round_behavior != ROUND_TO_NEAREST)
    op_type = TYPE_BIGNUM;
  else
    op_type = TYPE_FLOAT;

  num = promote_number (CAR (args), op_type);
  div = promote_number (div_, op_type);

  if (op_type == TYPE_BIGNUM)
    {
      ret = alloc_number (quotient_type);

      ret2 = alloc_number (TYPE_BIGNUM);
      mpz_init (ret2->value_ptr.integer);

      mpz_init (tmp);

      if (round_behavior == FLOOR)
	mpz_fdiv_qr (tmp, ret2->value_ptr.integer, num->value_ptr.integer,
		     div->value_ptr.integer);
      else if (round_behavior == CEILING)
	mpz_cdiv_qr (tmp, ret2->value_ptr.integer, num->value_ptr.integer,
		     div->value_ptr.integer);
      else if (round_behavior == TRUNCATE)
	mpz_tdiv_qr (tmp, ret2->value_ptr.integer, num->value_ptr.integer,
		     div->value_ptr.integer);

      if (quotient_type == TYPE_BIGNUM)
	{
	  mpz_init (ret->value_ptr.integer);
	  mpz_set (ret->value_ptr.integer, tmp);
	}
      else
	{
	  mpf_init (ret->value_ptr.floating);
	  mpf_set_z (ret->value_ptr.floating, tmp);
	}

      mpz_clear (tmp);
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

      ret = alloc_number (quotient_type);

      if (quotient_type == TYPE_BIGNUM)
	mpz_set_f (ret->value_ptr.integer, q);
      else
	mpf_set (ret->value_ptr.floating, q);

      ret2 = alloc_number (rem_type);

      if (rem_type == TYPE_BIGNUM)
	mpz_set_f (ret2->value_ptr.integer, r);
      else if (rem_type == TYPE_RATIO)
	mpq_set_f (ret2->value_ptr.ratio, r);
      else if (rem_type == TYPE_FLOAT)
	mpf_set (ret2->value_ptr.floating, r);

      mpf_clear (q);
      mpf_clear (r);
    }

  decrement_refcount (num);
  decrement_refcount (div);

  prepend_object_to_obj_list (ret2, &outcome->other_values);

  return ret;
}


struct object *
builtin_plus (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  if (!list_length (list))
    {
      return create_integer_from_long (0);
    }
  else if (list_length (list) == 1)
    {
      if (!(CAR (list)->type & TYPE_NUMBER))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      increment_refcount (CAR (list));
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

      if (ret->type == TYPE_BIGNUM)
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
  if (!list_length (list))
    {
      return create_integer_from_long (1);
    }
  else if (list_length (list) == 1)
    {
      if (!(CAR (list)->type & TYPE_NUMBER))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      increment_refcount (CAR (list));
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
      ret = alloc_number (TYPE_BIGNUM);
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
    } while (SYMBOL (list) != &nil_object);

  return ret;
}


struct object *
builtin_floor (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, FLOOR, TYPE_BIGNUM, outcome);
}


struct object *
builtin_ffloor (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, FLOOR, TYPE_FLOAT, outcome);
}


struct object *
builtin_ceiling (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, CEILING, TYPE_BIGNUM, outcome);
}


struct object *
builtin_fceiling (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, CEILING, TYPE_FLOAT, outcome);
}


struct object *
builtin_truncate (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, TRUNCATE, TYPE_BIGNUM, outcome);
}


struct object *
builtin_ftruncate (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, TRUNCATE, TYPE_FLOAT, outcome);
}


struct object *
builtin_round (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, ROUND_TO_NEAREST, TYPE_BIGNUM,
					  outcome);
}


struct object *
builtin_fround (struct object *list, struct environment *env,
	       struct eval_outcome *outcome)
{
  return perform_division_with_remainder (list, ROUND_TO_NEAREST, TYPE_FLOAT,
					  outcome);
}


struct object *
builtin_numerator (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_RATIONAL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (list)->type == TYPE_BIGNUM)
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }

  mpq_canonicalize (CAR (list)->value_ptr.ratio);

  ret = alloc_number (TYPE_BIGNUM);
  mpq_get_num (ret->value_ptr.integer, CAR (list)->value_ptr.ratio);

  return ret;
}


struct object *
builtin_denominator (struct object *list, struct environment *env,
		     struct eval_outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_RATIONAL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (list)->type == TYPE_BIGNUM)
    {
      return create_integer_from_long (1);
    }

  mpq_canonicalize (CAR (list)->value_ptr.ratio);

  ret = alloc_number (TYPE_BIGNUM);
  mpq_get_den (ret->value_ptr.integer, CAR (list)->value_ptr.ratio);

  return ret;
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

  if (!IS_REAL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
    }

  num = promote_number (CAR (list), TYPE_FLOAT);

  mpf_init (rt);
  mpf_sqrt (rt, num->value_ptr.floating);

  ret = alloc_number (TYPE_FLOAT);
  mpf_set (ret->value_ptr.floating, rt);

  decrement_refcount (num);

  return ret;
}


struct object *
builtin_complex (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  int l = list_length (list);
  struct object *ret, *r, *i;
  enum object_type t;

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_REAL (CAR (list)) || (l == 2 && !IS_REAL (CAR (CDR (list)))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 1 && IS_RATIONAL (CAR (list)))
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }

  if (l == 1)
    {
      ret = alloc_complex ();

      increment_refcount (CAR (list));
      ret->value_ptr.complex->real = CAR (list);
      ret->value_ptr.complex->imag = create_floating_from_double (0.0);

      return ret;
    }

  if (IS_RATIONAL (CAR (list)))
    {
      if ((CAR (CDR (list))->type == TYPE_BIGNUM
	   && !mpz_sgn (CAR (CDR (list))->value_ptr.integer))
	  || (CAR (CDR (list))->type == TYPE_RATIO
	      && !mpq_sgn (CAR (CDR (list))->value_ptr.ratio)))
	{
	  increment_refcount (CAR (list));
	  return CAR (list);
	}
    }

  t = highest_num_type (CAR (list)->type, CAR (CDR (list))->type);
  r = promote_number (CAR (list), t);
  i = promote_number (CAR (CDR (list)), t);

  ret = alloc_complex ();
  ret->value_ptr.complex->real = r;
  ret->value_ptr.complex->imag = i;

  return ret;
}


struct object *
builtin_realpart (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  struct object *num;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  num = CAR (list);

  if (num->type == TYPE_COMPLEX)
    {
      increment_refcount (num->value_ptr.complex->real);
      return num->value_ptr.complex->real;
    }
  else if (num->type & TYPE_REAL)
    {
      increment_refcount (num);
      return num;
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }
}


struct object *
builtin_imagpart (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  struct object *num;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  num = CAR (list);

  if (num->type == TYPE_COMPLEX)
    {
      increment_refcount (num->value_ptr.complex->imag);
      return num->value_ptr.complex->imag;
    }
  else if (IS_RATIONAL (num))
    {
      return create_integer_from_long (0);
    }
  else if (num->type == TYPE_FLOAT)
    {
      return create_floating_from_double (0.0);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }
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
builtin_min (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  int i, l = list_length (list);
  struct object *cur, *ret = CAR (list);

  if (!l)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  cur = list;

  if (!IS_REAL (CAR (cur)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  for (i = 1; i < l; i++)
    {
      cur = CDR (cur);

      if (!IS_REAL (CAR (cur)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (compare_two_numbers (CAR (cur), ret) < 0)
	ret = CAR (cur);
    }

  increment_refcount (ret);
  return ret;
}


struct object *
builtin_max (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  int i, l = list_length (list);
  struct object *cur, *ret = CAR (list);

  if (!l)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  cur = list;

  if (!IS_REAL (CAR (cur)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  for (i = 1; i < l; i++)
    {
      cur = CDR (cur);

      if (!IS_REAL (CAR (cur)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (compare_two_numbers (CAR (cur), ret) > 0)
	ret = CAR (cur);
    }

  increment_refcount (ret);
  return ret;
}


struct object *
builtin_byte (struct object *list, struct environment *env,
	      struct eval_outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_BIGNUM
      || mpz_sgn (CAR (list)->value_ptr.integer) < 0
      || CAR (CDR (list))->type != TYPE_BIGNUM
      || mpz_sgn (CAR (CDR (list))->value_ptr.integer) < 0)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = alloc_bytespec ();

  mpz_set (ret->value_ptr.bytespec->size, CAR (list)->value_ptr.integer);
  mpz_set (ret->value_ptr.bytespec->pos, CAR (CDR (list))->value_ptr.integer);

  return ret;
}


struct object *
builtin_byte_size (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_BYTESPEC)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = alloc_number (TYPE_BIGNUM);

  mpz_set (ret->value_ptr.integer, CAR (list)->value_ptr.bytespec->size);

  return ret;
}


struct object *
builtin_byte_position (struct object *list, struct environment *env,
		       struct eval_outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_BYTESPEC)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = alloc_number (TYPE_BIGNUM);

  mpz_set (ret->value_ptr.integer, CAR (list)->value_ptr.bytespec->pos);

  return ret;
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

  ret = check_type (nth (0, list), nth (1, list), env, outcome);

  if (ret == -1)
    return NULL;
  else if (ret)
    return &t_object;
  else
    return &nil_object;
}


struct object *
builtin_make_string (struct object *list, struct environment *env,
		     struct eval_outcome *outcome)
{
  int sz;
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (CAR (list)->type != TYPE_BIGNUM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;

      return NULL;
    }

  sz = mpz_get_si (CAR (list)->value_ptr.integer);

  if (sz < 0)
    {
      outcome->type = INVALID_SIZE;

      return NULL;
    }

  ret = alloc_object ();

  ret->type = TYPE_STRING;
  ret->value_ptr.string = malloc_and_check (sizeof (*ret->value_ptr.string));
  ret->value_ptr.string->value = calloc_and_check (sz, 1);
  ret->value_ptr.string->alloc_size = ret->value_ptr.string->used_size = sz;
  ret->value_ptr.string->fill_pointer = -1;

  return ret;
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
builtin_symbol_name (struct object *list, struct environment *env,
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

  return create_string_from_char_vector (s->name, s->name_len, 1);
}


struct object *
builtin_symbol_package (struct object *list, struct environment *env,
			struct eval_outcome *outcome)
{
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

  return SYMBOL (CAR (list))->value_ptr.symbol->home_package;
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


struct object *
builtin_string (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct symbol *s;
  struct object *ret;
  size_t l;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      s = SYMBOL (CAR (list))->value_ptr.symbol;

      return create_string_from_char_vector (s->name, s->name_len, 1);
    }
  else if (CAR (list)->type == TYPE_CHARACTER)
    {
      l = strlen (CAR (list)->value_ptr.character);

      ret = alloc_string (l);
      strcpy (ret->value_ptr.string->value, CAR (list)->value_ptr.character);
      ret->value_ptr.string->used_size = l;

      return ret;
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;

      return NULL;
    }
}


struct object *
builtin_string_eq (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (CAR (list)->type != TYPE_STRING || CAR (CDR (list))->type != TYPE_STRING)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;

      return NULL;
    }

  if (equal_strings (CAR (list)->value_ptr.string,
		     CAR (CDR (list))->value_ptr.string))
    return &t_object;

  return &nil_object;
}


struct object *
builtin_char_eq (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  int l = list_length (list);
  struct object *ch;

  if (!l)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  ch = CAR (list);

  if (ch->type != TYPE_CHARACTER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (CAR (list)->type != TYPE_CHARACTER)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (strcmp (ch->value_ptr.character, CAR (list)->value_ptr.character))
	return &nil_object;

      list = CDR (list);
    }

  return &t_object;
}


struct object *
builtin_char_upcase (struct object *list, struct environment *env,
		     struct eval_outcome *outcome)
{
  char *ch;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ch = CAR (list)->value_ptr.character;

  if (strlen (ch) > 1)
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }

  return create_character_from_char (toupper ((unsigned char)*ch));
}


struct object *
builtin_char_downcase (struct object *list, struct environment *env,
		       struct eval_outcome *outcome)
{
  char *ch;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ch = CAR (list)->value_ptr.character;

  if (strlen (ch) > 1)
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }

  return create_character_from_char (tolower ((unsigned char)*ch));
}


struct object *
builtin_alpha_char_p (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  char *ch;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ch = CAR (list)->value_ptr.character;

  if (strlen (ch) > 1)
    return &nil_object;

  if (isalpha ((unsigned char) *ch))
    return &t_object;

  return &nil_object;
}


struct object *
builtin_alphanumericp (struct object *list, struct environment *env,
		       struct eval_outcome *outcome)
{
  char *ch;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ch = CAR (list)->value_ptr.character;

  if (strlen (ch) > 1)
    return &nil_object;

  if (isalnum ((unsigned char) *ch))
    return &t_object;

  return &nil_object;
}


struct object *
builtin_char_code (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  char *ch;
  long ret = 0;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ch = CAR (list)->value_ptr.character;

  while (*ch)
    {
      ret <<= 8;
      ret += *ch;

      ch++;
    }

  return create_integer_from_long (ret);
}


struct object *
builtin_find_package (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_PACKAGE)
    {
      return CAR (list);
    }
  else if (CAR (list)->type == TYPE_STRING)
    {
      ret = find_package (CAR (list)->value_ptr.string->value,
			  CAR (list)->value_ptr.string->used_size, env);
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      ret = find_package (SYMBOL (CAR (list))->value_ptr.symbol->name,
			  SYMBOL (CAR (list))->value_ptr.symbol->name_len, env);
    }
  else if (CAR (list)->type == TYPE_CHARACTER)
    {
      ret = find_package (CAR (list)->value_ptr.character,
			  strlen (CAR (list)->value_ptr.character), env);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (!ret)
    return &nil_object;

  return ret;
}


struct object *
builtin_package_name (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  struct object *pack;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  pack = inspect_package_by_designator (CAR (list), env);

  if (!pack)
    {
      outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
      return NULL;
    }

  return create_string_from_char_vector (pack->value_ptr.package->name,
					 pack->value_ptr.package->name_len, 1);
}


struct object *
builtin_package_nicknames (struct object *list, struct environment *env,
			   struct eval_outcome *outcome)
{
  struct object *pack, *ret = &nil_object, *cons;
  struct name_list *n;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  pack = inspect_package_by_designator (CAR (list), env);

  if (!pack)
    {
      outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
      return NULL;
    }

  n = pack->value_ptr.package->nicks;

  while (n)
    {
      if (ret == &nil_object)
	ret = cons = alloc_empty_cons_pair ();
      else
	cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

      cons->value_ptr.cons_pair->car =
	create_string_from_char_vector (n->name, n->name_len, 1);

      n = n->next;
    }

  if (ret != &nil_object)
    cons->value_ptr.cons_pair->cdr = &nil_object;

  return ret;
}


struct object *
builtin_rename_package (struct object *list, struct environment *env,
			struct eval_outcome *outcome)
{
  int l = list_length (list), len;
  struct object *cons, *pack;
  struct package *p;
  struct name_list *nicks;
  char *name;

  if (l < 2 || l > 3)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list))
      || !IS_PACKAGE_DESIGNATOR (CAR (CDR (list)))
      || !IS_LIST (CAR (CDR (CDR (list)))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  cons = CAR (CDR (CDR (list)));

  while (SYMBOL (cons) != &nil_object)
    {
      if (!IS_STRING_DESIGNATOR (CAR (cons)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      cons = CDR (cons);
    }

  pack = inspect_package_by_designator (CAR (list), env);

  if (!pack)
    {
      outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
      return NULL;
    }

  if (CAR (CDR (list))->type == TYPE_PACKAGE)
    {
      name = CAR (CDR (list))->value_ptr.package->name;
      len = CAR (CDR (list))->value_ptr.package->name_len;
    }
  else if (CAR (CDR (list))->type == TYPE_STRING)
    {
      name = CAR (CDR (list))->value_ptr.string->value;
      len = CAR (CDR (list))->value_ptr.string->used_size;
    }
  else if (CAR (CDR (list))->type == TYPE_CHARACTER)
    {
      name = CAR (CDR (list))->value_ptr.character;
      len = strlen (name);
    }
  else
    {
      name = SYMBOL (CAR (CDR (list)))->value_ptr.symbol->name;
      len = SYMBOL (CAR (CDR (list)))->value_ptr.symbol->name_len;
    }

  if (find_package (name, len, env))
    {
      outcome->type = PACKAGE_NAME_OR_NICKNAME_ALREADY_IN_USE;
      return NULL;
    }

  p = pack->value_ptr.package;

  free (p->name);
  free_name_list (p->nicks);

  p->name = malloc_and_check (len);
  memcpy (p->name, name, len);
  p->name_len = len;

  cons = CAR (CDR (CDR (list)));
  p->nicks = NULL;

  while (SYMBOL (cons) != &nil_object)
    {
      if (CAR (cons)->type == TYPE_STRING)
	{
	  name = CAR (cons)->value_ptr.string->value;
	  len = CAR (cons)->value_ptr.string->used_size;
	}
      else if (CAR (cons)->type == TYPE_CHARACTER)
	{
	  name = CAR (cons)->value_ptr.character;
	  len = strlen (name);
	}
      else
	{
	  name = SYMBOL (CAR (cons))->value_ptr.symbol->name;
	  len = SYMBOL (CAR (cons))->value_ptr.symbol->name_len;
	}

      if (find_package (name, len, env))
	{
	  if (p->nicks)
	    nicks->next = NULL;

	  outcome->type = PACKAGE_NAME_OR_NICKNAME_ALREADY_IN_USE;
	  return NULL;
	}

      if (p->nicks)
	nicks = nicks->next = malloc_and_check (sizeof (*nicks));
      else
	p->nicks = nicks = malloc_and_check (sizeof (*nicks));

      nicks->name = malloc_and_check (len);
      memcpy (nicks->name, name, len);
      nicks->name_len = len;
      nicks->next = NULL;

      cons = CDR (cons);
    }

  if (p->nicks)
    nicks->next = NULL;

  return pack;
}


struct object *
builtin_package_use_list (struct object *list, struct environment *env,
			  struct eval_outcome *outcome)
{
  struct object *pack, *ret = &nil_object, *cons;
  struct object_list *n;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  pack = inspect_package_by_designator (CAR (list), env);

  if (!pack)
    {
      outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
      return NULL;
    }

  n = pack->value_ptr.package->uses;

  while (n)
    {
      if (ret == &nil_object)
	ret = cons = alloc_empty_cons_pair ();
      else
	cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

      cons->value_ptr.cons_pair->car = n->obj;

      n = n->next;
    }

  if (ret != &nil_object)
    cons->value_ptr.cons_pair->cdr = &nil_object;

  return ret;
}


struct object *
builtin_package_used_by_list (struct object *list, struct environment *env,
			      struct eval_outcome *outcome)
{
  struct object *pack, *ret = &nil_object, *cons;
  struct object_list *n;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  pack = inspect_package_by_designator (CAR (list), env);

  if (!pack)
    {
      outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
      return NULL;
    }

  n = pack->value_ptr.package->used_by;

  while (n)
    {
      if (ret == &nil_object)
	ret = cons = alloc_empty_cons_pair ();
      else
	cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

      cons->value_ptr.cons_pair->car = n->obj;

      n = n->next;
    }

  if (ret != &nil_object)
    cons->value_ptr.cons_pair->cdr = &nil_object;

  return ret;
}


struct object *
builtin_list_all_packages (struct object *list, struct environment *env,
			   struct eval_outcome *outcome)
{
  struct object *ret = &nil_object, *cons;
  struct object_list *p;

  if (SYMBOL (list) != &nil_object)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  p = env->packages;

  while (p)
    {
      if (ret == &nil_object)
	ret = cons = alloc_empty_cons_pair ();
      else
	cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

      cons->value_ptr.cons_pair->car = p->obj;

      p = p->next;
    }

  if (ret != &nil_object)
    cons->value_ptr.cons_pair->cdr = &nil_object;

  return ret;
}


struct object *
builtin_make_package (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  char *name;
  int len;
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STRING && CAR (list)->type != TYPE_CHARACTER
      && !IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      name = CAR (list)->value_ptr.string->value;
      len = CAR (list)->value_ptr.string->used_size;
    }
  else if (CAR (list)->type == TYPE_CHARACTER)
    {
      name = CAR (list)->value_ptr.character;
      len = strlen (name);
    }
  else
    {
      name = SYMBOL (CAR (list))->value_ptr.symbol->name;
      len = SYMBOL (CAR (list))->value_ptr.symbol->name_len;
    }

  if (find_package (name, len, env))
    {
      outcome->type = PACKAGE_NAME_OR_NICKNAME_ALREADY_IN_USE;
      return NULL;
    }

  ret = create_package (name, len);

  prepend_object_to_obj_list (ret, &env->packages);

  return ret;
}


struct object *
builtin_in_package (struct object *list, struct environment *env,
		    struct eval_outcome *outcome)
{
  char *name;
  int len;
  struct object *pack;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STRING && CAR (list)->type != TYPE_CHARACTER
      && !IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      name = CAR (list)->value_ptr.string->value;
      len = CAR (list)->value_ptr.string->used_size;
    }
  else if (CAR (list)->type == TYPE_CHARACTER)
    {
      name = CAR (list)->value_ptr.character;
      len = strlen (name);
    }
  else
    {
      name = SYMBOL (CAR (list))->value_ptr.symbol->name;
      len = SYMBOL (CAR (list))->value_ptr.symbol->name_len;
    }

  pack = find_package (name, len, env);

  if (!pack)
    {
      outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
      return NULL;
    }

  return set_value (env->package_sym, pack, env, outcome);
}


struct object *
builtin_lisp_implementation_type (struct object *list, struct environment *env,
				  struct eval_outcome *outcome)
{
  if (list_length (list))
    {
      outcome->type = TOO_MANY_ARGUMENTS;

      return NULL;
    }

  return create_string_from_c_string ("alisp");
}


struct object *
builtin_lisp_implementation_version (struct object *list,
				     struct environment *env,
				     struct eval_outcome *outcome)
{
  if (list_length (list))
    {
      outcome->type = TOO_MANY_ARGUMENTS;

      return NULL;
    }

  return create_string_from_c_string (PACKAGE_VERSION);
}


struct object *
builtin_software_type (struct object *list, struct environment *env,
		       struct eval_outcome *outcome)
{
  if (SYMBOL (list) != &nil_object)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  return &nil_object;
}


struct object *
builtin_software_version (struct object *list, struct environment *env,
			  struct eval_outcome *outcome)
{
  if (SYMBOL (list) != &nil_object)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

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
  int bin_num = 0;
  struct binding *bins = NULL, *bin;

  if (!list_length (list) || (CAR (list)->type != TYPE_CONS_PAIR
			      && SYMBOL (CAR (list)) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_LET;
      return NULL;
    }

  bind_forms = CAR (list);
  body = CDR (list);

  while (SYMBOL (bind_forms) != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome);

      if (!bin)
	return NULL;

      bins = add_binding (bin, bins);
      bin_num++;

      bind_forms = CDR (bind_forms);
    }

  env->vars = chain_bindings (bins, env->vars, NULL);

  env->lex_env_vars_boundary += bin_num;

  res = evaluate_body (body, 0, NULL, env, outcome);

  env->vars = remove_bindings (env->vars, bin_num);

  env->lex_env_vars_boundary -= bin_num;

  return res;
}


struct object *
evaluate_let_star (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *res, *bind_forms, *body;
  int bin_num = 0;
  struct binding *bin;

  if (!list_length (list) || (CAR (list)->type != TYPE_CONS_PAIR
			      && SYMBOL (CAR (list)) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_LET;
      return NULL;
    }

  bind_forms = CAR (list);
  body = CDR (list);

  while (SYMBOL (bind_forms) != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome);

      if (!bin)
	return NULL;

      env->vars = add_binding (bin, env->vars);
      env->lex_env_vars_boundary++, bin_num++;

      bind_forms = CDR (bind_forms);
    }

  res = evaluate_body (body, 0, NULL, env, outcome);

  env->vars = remove_bindings (env->vars, bin_num);

  env->lex_env_vars_boundary -= bin_num;

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

      fun = create_function (CAR (CDR (form)), CDR (CDR (form)), env, outcome,
			     type == TYPE_MACRO);

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
  int bin_num = 0;
  struct binding *bins = NULL, *bin;

  if (!list_length (list) || (CAR (list)->type != TYPE_CONS_PAIR
			      && SYMBOL (CAR (list)) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_FLET;
      return NULL;
    }

  bind_forms = CAR (list);
  body = CDR (list);

  while (SYMBOL (bind_forms) != &nil_object)
    {
      bin = create_binding_from_flet_form (CAR (bind_forms), env, outcome,
					   TYPE_FUNCTION);

      if (!bin)
	return NULL;

      bins = add_binding (bin, bins);
      bin_num++;

      bind_forms = CDR (bind_forms);
    }

  env->funcs = chain_bindings (bins, env->funcs, NULL);

  env->lex_env_funcs_boundary += bin_num;

  res = evaluate_body (body, 0, NULL, env, outcome);

  env->funcs = remove_bindings (env->funcs, bin_num);

  env->lex_env_funcs_boundary -= bin_num;

  return res;
}


struct object *
evaluate_labels (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  struct object *res, *bind_forms, *body;
  int bin_num = 0;
  struct binding *bin;

  if (!list_length (list) || (CAR (list)->type != TYPE_CONS_PAIR
			      && SYMBOL (CAR (list)) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_FLET;
      return NULL;
    }

  bind_forms = CAR (list);
  body = CDR (list);

  while (SYMBOL (bind_forms) != &nil_object)
    {
      bin = create_binding_from_flet_form (CAR (bind_forms), env, outcome,
					   TYPE_FUNCTION);

      if (!bin)
	return NULL;

      env->funcs = add_binding (bin, env->funcs);
      env->lex_env_funcs_boundary++, bin_num++;

      bind_forms = CDR (bind_forms);
    }

  res = evaluate_body (body, 0, NULL, env, outcome);

  env->funcs = remove_bindings (env->funcs, bin_num);

  env->lex_env_funcs_boundary -= bin_num;

  return res;
}


struct object *
evaluate_macrolet (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *res, *bind_forms, *body;
  int bin_num = 0;
  struct binding *bins = NULL, *bin;

  if (!list_length (list) || (CAR (list)->type != TYPE_CONS_PAIR
			      && SYMBOL (CAR (list)) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_FLET;
      return NULL;
    }

  bind_forms = CAR (list);
  body = CDR (list);

  while (SYMBOL (bind_forms) != &nil_object)
    {
      bin = create_binding_from_flet_form (CAR (bind_forms), env, outcome,
					   TYPE_MACRO);

      if (!bin)
	return NULL;

      bins = add_binding (bin, bins);
      bin_num++;

      bind_forms = CDR (bind_forms);
    }

  env->funcs = chain_bindings (bins, env->funcs, NULL);

  env->lex_env_funcs_boundary += bin_num;

  res = evaluate_body (body, 0, NULL, env, outcome);

  env->funcs = remove_bindings (env->funcs, bin_num);

  env->lex_env_funcs_boundary -= bin_num;

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
      increment_refcount (s->value_cell);

      return s->value_cell;
    }

  b = find_binding (s, env->vars, DYNAMIC_BINDING, -1);

  if (b)
    {
      increment_refcount (b->obj);

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

  increment_refcount (f);
  return f;
}


struct object *
inspect_variable_from_c_string (const char *var, struct environment *env)
{
  size_t l = strlen (var);
  struct object *pack = inspect_variable (env->package_sym, env);
  struct package_record *cur = pack->value_ptr.package->symtable
    [hash_char_vector (var, strlen (var), SYMTABLE_SIZE)];

  while (cur)
    {
      if (eqmem (cur->sym->value_ptr.symbol->name,
		 cur->sym->value_ptr.symbol->name_len, var, l))
	{
	  return inspect_variable (cur->sym, env);
	}

      cur = cur->next;
    }

  return NULL;
}


struct object *
inspect_variable (struct object *sym, struct environment *env)
{
  struct symbol *s = sym->value_ptr.symbol;
  struct binding *b;

  if (s->is_const || !s->value_dyn_bins_num)
    {
      return s->value_cell;
    }

  b = find_binding (s, env->vars, DYNAMIC_BINDING, -1);

  if (b)
    {
      return b->obj;
    }

  return NULL;
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
      b = find_binding (s, env->vars, LEXICAL_BINDING,
			env->lex_env_vars_boundary);

      if (b)
	{
	  b->obj = val;
	}
      else
	{
	  s->value_dyn_bins_num++;
	  s->is_special = 1;
	  increment_refcount (sym);
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

  increment_refcount (CAR (list));
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

  if (SYMBOL (if_clause) != &nil_object)
    {
      if (SYMBOL (CDR (list)) == &nil_object)
	{
	  outcome->type = MALFORMED_IF;
	  return NULL;
	}

      ret = evaluate_object (CAR (CDR (list)), env, outcome);
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
	  return ret;
	}
    }
}


struct object *
evaluate_progn (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  return evaluate_body (list, 0, NULL, env, outcome);
}


struct object *
evaluate_values (struct object *list, struct environment *env,
		 struct eval_outcome *outcome)
{
  if (SYMBOL (list) == &nil_object)
    {
      outcome->no_value = 1;
      return &nil_object;
    }

  outcome->other_values = copy_list_to_obj_list (CDR (list));

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
evaluate_values_list (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_LIST (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return evaluate_values (CAR (list), env, outcome);
}


struct object *
evaluate_multiple_value_list (struct object *list, struct environment *env,
			      struct eval_outcome *outcome)
{
  struct object *res, *ret, *cons;
  struct object_list *vals;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  res = evaluate_object (CAR (list), env, outcome);

  if (outcome->no_value)
    {
      outcome->no_value = 0;
      return &nil_object;
    }

  ret = cons = alloc_empty_cons_pair ();
  ret->value_ptr.cons_pair->car = res;

  vals = outcome->other_values;

  while (vals)
    {
      cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

      cons->value_ptr.cons_pair->car = vals->obj;

      vals = vals->next;
    }

  cons->value_ptr.cons_pair->cdr = &nil_object;

  free_object_list_structure (outcome->other_values);

  outcome->other_values = NULL;

  return ret;
}


struct object *
evaluate_defconstant (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

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

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

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
      if (!s->value_ptr.symbol->is_parameter)
	{
	  increment_refcount (s);
	  s->value_ptr.symbol->is_parameter = 1;
	}
    }
  else if (l == 2)
    {
      if (!s->value_ptr.symbol->value_cell)
	return define_parameter (CAR (list), CAR (CDR (list)), env, outcome);
    }

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
evaluate_defun (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *fun, *sym;

  if (list_length (list) < 2 || !IS_SYMBOL (CAR (list))
      || (!IS_LIST (CAR (CDR (list)))))
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

  fun = create_function (CAR (CDR (list)), CDR (CDR (list)), env, outcome, 0);

  if (!fun)
    return NULL;

  if (sym->value_ptr.symbol->function_cell)
    {
      decrement_refcount_by (sym->value_ptr.symbol->function_cell, sym->refcount,
			     sym);
    }
  else
    increment_refcount (sym);

  fun->value_ptr.function->name = sym;

  sym->value_ptr.symbol->function_cell = fun;
  increment_refcount_by (fun, sym->refcount - 1, sym);

  increment_refcount (sym);
  return sym;
}


struct object *
evaluate_defmacro (struct object *list, struct environment *env,
		   struct eval_outcome *outcome)
{
  struct object *mac, *sym;

  if (list_length (list) < 2 || !IS_SYMBOL (CAR (list))
      || (!IS_LIST (CAR (CDR (list)))))
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

  mac = create_function (CAR (CDR (list)), CDR (CDR (list)), env, outcome, 1);

  if (!mac)
    return NULL;

  mac->type = TYPE_MACRO;

  if (sym->value_ptr.symbol->function_cell)
    {
      decrement_refcount_by (sym->value_ptr.symbol->function_cell, sym->refcount,
			     sym);
    }
  else
    increment_refcount (sym);

  mac->value_ptr.function->name = sym;

  sym->value_ptr.symbol->function_cell = mac;
  increment_refcount_by (mac, sym->refcount - 1, sym);

  increment_refcount (sym);
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

  while (SYMBOL (list) != &nil_object)
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

  increment_refcount (ret);
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

  while (SYMBOL (list) != &nil_object)
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

	  val = evaluate_object (CAR (CDR (list)), env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!val)
	    return NULL;

	  val = SYMBOL (CAR (CAR (list)))->value_ptr.symbol->builtin_accessor
	    (CAR (list), val, env, outcome);

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

  increment_refcount (val);
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
				 && SYMBOL (CAR (list)) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_DEFUN;
      return NULL;
    }

  fun = create_function (CAR (list), CDR (list), env, outcome, 0);

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

  if (last->type != TYPE_CONS_PAIR && SYMBOL (last) != &nil_object)
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
evaluate_funcall (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  struct object *fun;

  if (CAR (list)->type == TYPE_FUNCTION)
    {
      fun = CAR (list);
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      fun = get_function (SYMBOL (CAR (list)), env, 1);

      if (!fun)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return call_function (fun, CDR (list), 0, 0, env, outcome);
}


struct object *
evaluate_declare (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  outcome->type = DECLARE_NOT_ALLOWED_HERE;

  return NULL;
}


struct object *
evaluate_prog1 (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *tmp, *ret;

  if (!list_length (list))
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  ret = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!ret)
    return NULL;

  tmp = evaluate_body (CDR (list), 0, NULL, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  decrement_refcount (tmp);

  return ret;
}


struct object *
evaluate_prog2 (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  struct object *tmp, *ret;

  if (list_length (list) < 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  tmp = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!tmp)
    return NULL;

  decrement_refcount (tmp);

  ret = evaluate_object (CAR (CDR (list)), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!ret)
    return NULL;

  tmp = evaluate_body (CDR (CDR (list)), 0, NULL, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  decrement_refcount (tmp);

  return ret;
}


struct object *
evaluate_tagbody (struct object *list, struct environment *env,
		  struct eval_outcome *outcome)
{
  struct object *ret = evaluate_body (list, 1, NULL, env, outcome);

  if (!ret)
    return NULL;

  decrement_refcount (ret);

  return &nil_object;
}


struct object *
evaluate_go (struct object *list, struct environment *env,
	     struct eval_outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_BIGNUM && !IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  outcome->tag_to_jump_to = CAR (list);

  return NULL;
}


struct object *
evaluate_block (struct object *list, struct environment *env,
		struct eval_outcome *outcome)
{
  if (!list_length (list))
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return evaluate_body (CDR (list), 0, SYMBOL (CAR (list)), env, outcome);
}


struct object *
evaluate_return_from (struct object *list, struct environment *env,
		      struct eval_outcome *outcome)
{
  int l = list_length (list);
  struct object *ret;

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 2)
    {
      ret = evaluate_object (CAR (CDR (list)), env, outcome);

      if (!ret)
	return NULL;
    }
  else
    ret = &nil_object;

  outcome->block_to_leave = SYMBOL (CAR (list));
  outcome->return_value = ret;

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


struct object *
fresh_line (struct stream *str)
{
  if (str->dirty_line)
    {
      write_to_stream (str, "\n", 1);
      str->dirty_line = 0;

      return &t_object;
    }

  return &nil_object;
}


int
write_to_stream (struct stream *stream, const char *str, size_t size)
{
  struct string *s;
  size_t i;

  if (stream->medium == FILE_STREAM)
    {
      if (fwrite (str, 1, size, stream->file) < size)
	return -1;

      return 0;
    }
  else
    {
      s = stream->string->value_ptr.string;
      resize_string_allocation (stream->string, s->used_size + size);

      for (i = 0; i < size; i++)
	s->value [s->used_size + i] = str [i];

      s->used_size = s->alloc_size;

      return 0;
    }
}


int
write_char_to_stream (struct stream *stream, char ch)
{
  return write_to_stream (stream, &ch, 1);
}


int
write_long_to_stream (struct stream *stream, long z)
{
  int exp = 1, is_negative = 0, size = 1;

  if (!z)
    return write_to_stream (stream, "0", 1);

  if (z < 0)
    {
      z = -z;
      is_negative = 1;
      size++;
    }

  while (z / exp >= 10)
    {
      exp *= 10;
      size++;
    }

  if (stream->medium == STRING_STREAM)
    resize_string_allocation (stream->string,
			      stream->string->value_ptr.string->used_size + size);

  if (is_negative && write_to_stream (stream, "-", 1) < 0)
    return -1;

  while (exp > 0)
    {
      if (write_char_to_stream (stream, z / exp + '0') < 0)
	return -1;

      z %= exp;
      exp /= 10;
    }

  return 0;
}


int
is_printer_escaping_enabled (struct environment *env)
{
  struct object *p_escape = inspect_variable (env->print_escape_sym, env),
    *p_readably = inspect_variable (env->print_readably_sym, env);

  return SYMBOL (p_escape) != &nil_object || SYMBOL (p_readably) != &nil_object;
}


int
print_as_symbol (const char *sym, size_t len, int print_escapes,
		 struct stream *str)
{
  size_t i, sz;
  char need_escape [] = "().,;'#\"\n\\";
  int do_need_escape = 0;

  for (i = 0, sz = 0; print_escapes && i < len; i++)
    {
      if (strchr (need_escape, sym [i]) || !sym [i]
	  || islower ((unsigned char)sym [i]))
	{
	  do_need_escape = 1;

	  if (str->medium == FILE_STREAM)
	    break;

	  sz += 2;
	}
      else
	sz++;
    }

  if (do_need_escape)
    sz += 2;

  if (str->medium == STRING_STREAM)
    resize_string_allocation (str->string,
			      str->string->value_ptr.string->used_size + sz);

  if (do_need_escape && write_to_stream (str, "|", 1) < 0)
    return -1;

  for (i = 0; i < len; i++)
    {
      if (print_escapes && (sym [i] == '|' || sym [i] == '\\')
	  && write_to_stream (str, "\\", 1) < 0)
	return -1;

      if (write_to_stream (str, &sym [i], 1) < 0)
	return -1;
    }

  if (do_need_escape && write_to_stream (str, "|", 1) < 0)
    return -1;

  return 0;
}


int
print_symbol_name (const struct symbol_name *sym, struct environment *env,
		   struct stream *str)
{
  int pesc = is_printer_escaping_enabled (env);

  if (sym->sym->value_ptr.symbol->home_package)
    return print_symbol (sym->sym->value_ptr.symbol, env, str);
  else
    {
      if (pesc && write_to_stream (str, "#:", 2) < 0)
	return -1;

      return print_as_symbol (sym->value, sym->used_size, 1, str);
    }
}


int
print_symbol (const struct symbol *sym, struct environment *env,
	      struct stream *str)
{
  int pesc = is_printer_escaping_enabled (env);

  if (!sym->home_package)
    {
      if (pesc && write_to_stream (str, "#:", 2) < 0)
	return -1;
    }
  else if (sym->home_package == env->keyword_package
	   && write_to_stream (str, ":", 1) < 0)
    return -1;

  return print_as_symbol (sym->name, sym->name_len, pesc, str);
}


int
print_bignum (const mpz_t z, struct environment *env, struct stream *str)
{
  char *out;
  int ret;

  gmp_asprintf (&out, "%Zd", z);

  ret = write_to_stream (str, out, strlen (out));
  free (out);
  return ret;
}


int
print_floating (const mpf_t f, struct environment *env, struct stream *str)
{
  char *out;
  int l;

  l = gmp_asprintf (&out, "%.Ff", f);

  if (!strchr (out, '.'))
    {
      out = realloc (out, l+3);
      out [l] = '.';
      out [l+1] = '0';
      out [l+2] = 0;
    }

  if (write_to_stream (str, out, strlen (out)) < 0)
    {
      free (out);
      return -1;
    }

  free (out);
  return 0;
}


int
print_complex (const struct complex *c, struct environment *env,
	       struct stream *str)
{
  if (write_to_stream (str, "#C(", 3) < 0)
    return -1;

  print_object (c->real, env, str);

  if (write_to_stream (str, " ", 1) < 0)
    return -1;

  print_object (c->imag, env, str);

  return write_to_stream (str, ")", 1);
}


int
print_bytespec (const struct bytespec *bs, struct environment *env,
		struct stream *str)
{
  if (write_to_stream (str, "#<BYTE-SPECIFIER size ",
		       strlen ("#<BYTE-SPECIFIER size ")) < 0
      || print_bignum (bs->size, env, str) < 0
      || write_to_stream (str, " position ", strlen (" position ")) < 0
      || print_bignum (bs->pos, env, str) < 0
      || write_to_stream (str, ">", 1) < 0)
    return -1;

  return 0;
}


int
print_string (const struct string *st, struct environment *env,
	      struct stream *str)
{
  size_t i;
  int pesc = is_printer_escaping_enabled (env);

  if (pesc && write_to_stream (str, "\"", 1) < 0)
    return -1;

  for (i = 0; i < st->used_size; i++)
    {
      if (pesc && (st->value [i] == '"' || st->value [i] == '\\')
	  && write_to_stream (str, "\\", 1) < 0)
	return -1;

      if (write_to_stream (str, &st->value [i], 1) < 0)
	return -1;
    }

  if (pesc)
    return write_to_stream (str, "\"", 1);

  return 0;
}


int
print_character (const char *character, struct environment *env,
		 struct stream *str)
{
  if (!is_printer_escaping_enabled (env))
    return write_to_stream (str, character, strlen (character));

  if (write_to_stream (str, "#\\", 2) < 0)
    return -1;

  if (strlen (character) == 1)
    {
      switch (character [0])
	{
	case '\n':
	  return write_to_stream (str, "Newline", strlen ("Newline"));
	  break;
	case ' ':
	  return write_to_stream (str, "Space", strlen ("Space"));
	  break;
	case '\t':
	  return write_to_stream (str, "Tab", strlen ("Tab"));
	  break;
	case '\b':
	  return write_to_stream (str, "Backspace", strlen ("Backspace"));
	  break;
	case '\f':
	  return write_to_stream (str, "Page", strlen ("Page"));
	  break;
	case '\r':
	  return write_to_stream (str, "Return", strlen ("Return"));
	  break;
	default:
	  return write_to_stream (str, character, strlen (character));
	  break;
	}
    }
  else
    return write_to_stream (str, character, strlen (character));
}


int
print_filename (const struct filename *fn, struct environment *env,
		struct stream *str)
{
  if (write_to_stream (str, "#P", 2) < 0)
    return -1;

  return print_string (fn->value->value_ptr.string, env, str);
}


int
print_list (const struct cons_pair *list, struct environment *env,
	    struct stream *str)
{
  struct object *cdr;

  if (write_to_stream (str, "(", 1) < 0)
    return -1;

  if (print_object (list->car, env, str) < 0)
    return -1;

  cdr = list->cdr;

  while (cdr && SYMBOL (cdr) != &nil_object)
    {
      if (cdr->type == TYPE_CONS_PAIR)
	{
	  if (write_to_stream (str, " ", 1) < 0
	      || print_object (cdr->value_ptr.cons_pair->car, env, str) < 0)
	    return -1;

	  cdr = cdr->value_ptr.cons_pair->cdr;
	}
      else
	{
	  if (write_to_stream (str, " . ", 3) < 0
	      || print_object (cdr, env, str) < 0)
	    return -1;

	  break;
	}
    }

  return write_to_stream (str, ")", 1);
}


int
print_array (const struct array *array, struct environment *env,
	     struct stream *str)
{
  int rk = array_rank (array);
  size_t i;

  if (rk == 1)
    {
      if (write_to_stream (str, "#(", 2) < 0)
	return -1;

      for (i = 0; i < (array->fill_pointer >= 0 ? array->fill_pointer :
		       array->alloc_size->size); i++)
	{
	  if (i && write_to_stream (str, " ", 1) < 0)
	    return -1;

	  if (print_object (array->value [i], env, str) < 0)
	    return -1;
	}

      if (write_to_stream (str, ")", 1) < 0)
	return -1;
      else
	return 0;
    }
  else if (write_to_stream (str, "#<ARRAY, RANK ", strlen ("#<ARRAY, RANK ")) < 0
	   || write_long_to_stream (str, rk) < 0
	   || write_to_stream (str, ">", 1) < 0)
    return -1;

  return 0;
}


int
print_function_or_macro (const struct object *obj, struct environment *env,
			 struct stream *str)
{
  if (obj->type == TYPE_FUNCTION)
    {
      if (write_to_stream (str, "#<FUNCTION ", strlen ("#<FUNCTION ")) < 0)
	return -1;

      if (obj->value_ptr.function->builtin_form
	  && write_to_stream (str, "BUILTIN ", strlen ("BUILTIN ")) < 0)
	return -1;

      if (obj->value_ptr.function->name)
	{
	  if (print_symbol (obj->value_ptr.function->name->value_ptr.symbol, env,
			    str) < 0)
	    return -1;
	}
      else if (write_to_stream (str, "?", 1) < 0)
	return -1;

      return write_to_stream (str, ">", 1);
    }
  else
    {
      if (obj->value_ptr.macro->is_special_operator)
	{
	  if (write_to_stream (str, "#<SPECIAL OPERATOR ",
			       strlen ("#<SPECIAL OPERATOR ")) < 0)
	    return -1;
	}
      else
	{
	  if (write_to_stream (str, "#<MACRO ", strlen ("#<MACRO ")) < 0)
	    return -1;

	  if (obj->value_ptr.function->builtin_form
	      && write_to_stream (str, "BUILTIN ", strlen ("BUILTIN ")) < 0)
	    return -1;
	}

      if (obj->value_ptr.macro->name)
	{
	  if (print_symbol (obj->value_ptr.macro->name->value_ptr.symbol, env,
			    str) < 0)
	    return -1;
	}
      else if (write_to_stream (str, "?", 1) < 0)
	return -1;

      return write_to_stream (str, ">", 1);
    }
}


int
print_object (const struct object *obj, struct environment *env,
	      struct stream *str)
{
  char *out;
  int ret;

  if (obj->type == TYPE_QUOTE)
    {
      if (write_to_stream (str, "'", 1) < 0)
	return -1;

      return print_object (obj->value_ptr.next, env, str);
    }
  else if (obj->type == TYPE_BACKQUOTE)
    {
      if (write_to_stream (str, "`", 1) < 0)
	return -1;

      return print_object (obj->value_ptr.next, env, str);
    }
  else if (obj->type == TYPE_COMMA)
    {
      if (write_to_stream (str, ",", 1) < 0)
	return -1;

      return print_object (obj->value_ptr.next, env, str);
    }
  else if (obj->type == TYPE_AT)
    {
      if (write_to_stream (str, "@", 1) < 0)
	return -1;

      return print_object (obj->value_ptr.next, env, str);
    }
  else if (obj->type == TYPE_DOT)
    {
      if (write_to_stream (str, ".", 1) < 0)
	return -1;

      return print_object (obj->value_ptr.next, env, str);
    }
  else
    {
      str->dirty_line = 1;

      if (SYMBOL (obj) == &nil_object)
	return write_to_stream (str, "NIL", strlen ("NIL"));
      else if (obj->type == TYPE_BIGNUM)
	return print_bignum (obj->value_ptr.integer, env, str);
      else if (obj->type == TYPE_FIXNUM)
	return write_long_to_stream (str, *obj->value_ptr.fixnum);
      else if (obj->type == TYPE_RATIO)
	{
	  gmp_asprintf (&out, "%Qd", obj->value_ptr.integer);

	  ret = write_to_stream (str, out, strlen (out));
	  free (out);
	  return ret;
	}
      else if (obj->type == TYPE_FLOAT)
	return print_floating (obj->value_ptr.floating, env, str);
      else if (obj->type == TYPE_COMPLEX)
	return print_complex (obj->value_ptr.complex, env, str);
      else if (obj->type == TYPE_BYTESPEC)
	return print_bytespec (obj->value_ptr.bytespec, env, str);
      else if (obj->type == TYPE_STRING)
	return print_string (obj->value_ptr.string, env, str);
      else if (obj->type == TYPE_CHARACTER)
	return print_character (obj->value_ptr.character, env, str);
      else if (obj->type == TYPE_FILENAME)
	return print_filename (obj->value_ptr.filename, env, str);
      else if (obj->type == TYPE_SYMBOL_NAME)
	return print_symbol_name (obj->value_ptr.symbol_name, env, str);
      else if (obj->type == TYPE_SYMBOL)
	return print_symbol (obj->value_ptr.symbol, env, str);
      else if (obj->type == TYPE_CONS_PAIR)
	return print_list (obj->value_ptr.cons_pair, env, str);
      else if (obj->type == TYPE_ARRAY)
	return print_array (obj->value_ptr.array, env, str);
      else if (obj->type == TYPE_HASHTABLE)
	{
	  if (write_to_stream (str, "#<HASH-TABLE EQ ",
			       strlen ("#<HASH-TABLE EQ ")) < 0
	      || write_long_to_stream (str,
				       hash_table_count
				       (obj->value_ptr.hashtable)) < 0
	      || write_to_stream (str, "/", 1) < 0
	      || write_long_to_stream (str, LISP_HASHTABLE_SIZE) < 0
	      || write_to_stream (str, ">", 1) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_FUNCTION || obj->type == TYPE_MACRO)
	return print_function_or_macro (obj, env, str);
      else if (obj->type == TYPE_PACKAGE)
	{
	  if (write_to_stream (str, "#<PACKAGE \"", strlen ("#<PACKAGE \"")) < 0
	      || write_to_stream (str, obj->value_ptr.package->name,
				  obj->value_ptr.package->name_len) < 0
	      || write_to_stream (str, "\">", 2) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_ENVIRONMENT)
	return write_to_stream (str, "#<ENVIRONMENT ?>",
				strlen ("#<ENVIRONMENT ?>"));
      else if (obj->type == TYPE_STREAM)
	return write_to_stream (str, "#<STREAM ?>",
				strlen ("#<STREAM ?>"));
      else
	return write_to_stream (str, "#<print not implemented>",
				strlen ("#<print not implemented>"));
    }
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
      printf ("read error: more than two consecutive colons cannot appear in a "
	      "token\n");
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
  else if (err == PACKAGE_NOT_FOUND_IN_READ)
    {
      printf ("read error: package ");
      fwrite (args->obj->value_ptr.symbol_name->value,
	      args->obj->value_ptr.symbol_name->used_size, 1, stdout);
      printf (" not found\n");
    }
  else if (err == PACKAGE_MARKER_IN_SHARP_COLON)
    {
      printf ("read error: a package marker can't appear in a sharp-colon "
	      "macro\n");
    }
}


void
print_eval_error (struct eval_outcome *err, struct environment *env)
{
  struct object *std_out = inspect_variable (env->std_out_sym, env);

  fresh_line (std_out->value_ptr.stream);

  if (err->type == UNBOUND_SYMBOL)
    {
      printf ("eval error: symbol ");
      print_object (err->obj, env, env->c_stdout->value_ptr.stream);
      printf (" not bound to any object\n");
    }
  else if (err->type == UNKNOWN_FUNCTION)
    {
      printf ("eval error: symbol ");
      print_object (err->obj, env, env->c_stdout->value_ptr.stream);
      printf (" not bound to any function, macro or special operator\n");
    }
  else if (err->type == INVALID_FUNCTION_CALL)
    {
      printf ("eval error: not a function form, a macro form or a special "
	      "form\n");
    }
  else if (err->type == UNKNOWN_KEYWORD_ARGUMENT)
    {
      printf ("eval error: unknown keyword argument in function call\n");
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
  else if (err->type == INVALID_SIZE)
    {
      printf ("eval error: not a valid size\n");
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
  else if (err->type == INVALID_GO_TAG)
    {
      printf ("eval error: not a valid go tag\n");
    }
  else if (err->type == TAG_NOT_FOUND)
    {
      printf ("eval error: tag not found\n");
    }
  else if (err->type == BLOCK_NOT_FOUND)
    {
      printf ("eval error: block not found\n");
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
  else if (err->type == PACKAGE_NOT_FOUND_IN_EVAL)
    {
      printf ("eval error: package not found\n");
    }
  else if (err->type == PACKAGE_NAME_OR_NICKNAME_ALREADY_IN_USE)
    {
      printf ("eval error: package name or nickname is already in use\n");
    }

  std_out->value_ptr.stream->dirty_line = 0;
}


void
increment_refcount_by (struct object *obj, int count, struct object *parent)
{
  struct refcounted_object_list **antiloop_hash_t = NULL;

  if (parent)
    {
      antiloop_hash_t =
	alloc_empty_tailsharing_hash_table (ANTILOOP_HASH_T_SIZE);

      prepend_object_to_refcounted_obj_list
	(parent, &antiloop_hash_t [hash_object (parent, ANTILOOP_HASH_T_SIZE)]);
    }

  offset_refcount_by (obj, count, antiloop_hash_t, 0);

  if (antiloop_hash_t)
    free_tailsharing_hash_table (antiloop_hash_t, ANTILOOP_HASH_T_SIZE);
}


int
decrement_refcount_by (struct object *obj, int count, struct object *parent)
{
  struct refcounted_object_list **antiloop_hash_t = NULL;
  int ret;

  if (parent)
    {
      antiloop_hash_t =
	alloc_empty_tailsharing_hash_table (ANTILOOP_HASH_T_SIZE);

      prepend_object_to_refcounted_obj_list
	(parent, &antiloop_hash_t [hash_object (parent, ANTILOOP_HASH_T_SIZE)]);
    }

  ret = offset_refcount_by (obj, -count, antiloop_hash_t, 0);

  if (antiloop_hash_t)
    free_tailsharing_hash_table (antiloop_hash_t, ANTILOOP_HASH_T_SIZE);

  return ret;
}


int
offset_refcount_by (struct object *obj, int delta,
		    struct refcounted_object_list **antiloop_hash_t,
		    int clone_hash_t)
{
  int allocated_now = 0;
  struct parameter *par;
  size_t i, sz, ind;

  if (!obj || obj == &nil_object || obj == &t_object
      || obj->type == TYPE_PACKAGE)
    return 0;

  if (HAS_LEAF_TYPE (obj))
    obj->refcount += delta;
  else
    {
      if (!antiloop_hash_t)
	{
	  antiloop_hash_t =
	    alloc_empty_tailsharing_hash_table (ANTILOOP_HASH_T_SIZE);

	  allocated_now = 1;
	}

      ind = hash_object (obj, ANTILOOP_HASH_T_SIZE);

      if (!allocated_now
	  && is_object_in_refcounted_obj_list (obj, antiloop_hash_t [ind]))
	return 0;

      obj->refcount += delta;

      if (!allocated_now && clone_hash_t)
	{
	  antiloop_hash_t = clone_tailsharing_hash_table (antiloop_hash_t,
							  ANTILOOP_HASH_T_SIZE);
	  allocated_now = 1;
	}

      prepend_object_to_refcounted_obj_list (obj, &antiloop_hash_t [ind]);

      if (obj->type & TYPE_PREFIX)
	offset_refcount_by (obj->value_ptr.next, delta, antiloop_hash_t, 0);
      else if (obj->type == TYPE_SYMBOL_NAME)
	offset_refcount_by (obj->value_ptr.symbol_name->sym, delta,
			    antiloop_hash_t, 0);
      else if (obj->type == TYPE_SYMBOL)
	{
	  if (obj->value_ptr.symbol->value_cell
	      && obj->value_ptr.symbol->function_cell)
	    {
	      offset_refcount_by (obj->value_ptr.symbol->value_cell, delta,
				  antiloop_hash_t, 1);
	      offset_refcount_by (obj->value_ptr.symbol->function_cell, delta,
				  antiloop_hash_t, 0);
	    }
	  else
	    {
	      offset_refcount_by (obj->value_ptr.symbol->value_cell, delta,
				  antiloop_hash_t, 1);
	      offset_refcount_by (obj->value_ptr.symbol->function_cell, delta,
				  antiloop_hash_t, 0);
	    }
	}
      else if (obj->type == TYPE_CONS_PAIR)
	{
	  offset_refcount_by (obj->value_ptr.cons_pair->cdr, delta,
			      antiloop_hash_t, 1);
	  offset_refcount_by (obj->value_ptr.cons_pair->car, delta,
			      antiloop_hash_t, 0);
	}
      else if (obj->type == TYPE_COMPLEX)
	{
	  offset_refcount_by (obj->value_ptr.complex->real, delta,
			      antiloop_hash_t, 1);
	  offset_refcount_by (obj->value_ptr.complex->imag, delta,
			      antiloop_hash_t, 0);
	}
      else if (obj->type == TYPE_ARRAY)
	{
	  sz = array_total_size (obj->value_ptr.array);

	  for (i = 0; sz && i < sz-1; i++)
	    {
	      offset_refcount_by (obj->value_ptr.array->value [i], delta,
				  antiloop_hash_t, 1);
	    }

	  if (sz)
	    offset_refcount_by (obj->value_ptr.array->value [sz-1], delta,
				antiloop_hash_t, 0);
	}
      else if (obj->type == TYPE_HASHTABLE)
	{
	  for (i = 0; i < LISP_HASHTABLE_SIZE - 1; i++)
	    {
	      offset_refcount_by (obj->value_ptr.hashtable->table [i], delta,
				  antiloop_hash_t, 1);
	    }

	  offset_refcount_by (obj->value_ptr.hashtable->table
			      [LISP_HASHTABLE_SIZE-1], delta, antiloop_hash_t,
			      0);
	}
      else if (obj->type == TYPE_STREAM)
	{
	  if (obj->value_ptr.stream->medium == STRING_STREAM)
	    {
	      offset_refcount_by (obj->value_ptr.stream->string, delta,
				  antiloop_hash_t, 0);
	    }
	}
      else if (obj->type == TYPE_FUNCTION || obj->type == TYPE_MACRO)
	{
	  offset_refcount_by (obj->value_ptr.function->body, delta,
			      antiloop_hash_t, 1);

	  par = obj->value_ptr.function->lambda_list;

	  while (par)
	    {
	      offset_refcount_by (par->name, delta, antiloop_hash_t, 1);

	      if (par->init_form)
		{
		  offset_refcount_by (par->init_form, delta, antiloop_hash_t, 1);
		}

	      if (par->supplied_p_param)
		{
		  offset_refcount_by (par->supplied_p_param, delta,
				      antiloop_hash_t, 1);
		}

	      par = par->next;
	    }

	  offset_refcount_by (obj->value_ptr.function->name, delta,
			      antiloop_hash_t, 0);
	}

      if (allocated_now)
	free_tailsharing_hash_table (antiloop_hash_t, ANTILOOP_HASH_T_SIZE);
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
  else if (obj->type == TYPE_HASHTABLE)
    free_hashtable (obj);
  else if (obj->type == TYPE_CHARACTER)
    {
      free (obj->value_ptr.character);
      free (obj);
    }
  else if (obj->type == TYPE_BIGNUM)
    free_integer (obj);
  else if (obj->type == TYPE_FIXNUM)
    {
      free (obj->value_ptr.fixnum);
      free (obj);
    }
  else if (obj->type == TYPE_RATIO)
    free_ratio (obj);
  else if (obj->type == TYPE_FLOAT)
    free_float (obj);
  else if (obj->type == TYPE_COMPLEX)
    {
      free (obj->value_ptr.complex);
      free (obj);
    }
  else if (obj->type == TYPE_BYTESPEC)
    free_bytespec (obj);
  else if (obj->type == TYPE_FUNCTION || obj->type == TYPE_MACRO)
    free_function_or_macro (obj);
  else if (obj->type == TYPE_STREAM)
    {
      if (obj->value_ptr.stream->is_open
	  && obj->value_ptr.stream->medium == FILE_STREAM)
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
  struct array_size *next;

  if (size)
    {
      next = size->next;
      free (size);
      free_array_size (next);
    }
}


void
free_array (struct object *obj)
{
  free_array_size (obj->value_ptr.array->alloc_size);
  free (obj->value_ptr.array);
  free (obj);
}


void
free_hashtable (struct object *obj)
{
  free (obj->value_ptr.hashtable->table);
  free (obj->value_ptr.hashtable);
  free (obj);
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
free_bytespec (struct object *obj)
{
  mpz_clear (obj->value_ptr.bytespec->size);
  mpz_clear (obj->value_ptr.bytespec->pos);

  free (obj->value_ptr.bytespec);
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
free_sharp_macro_call (struct object *macro)
{
  free (macro->value_ptr.sharp_macro_call);
  free (macro);
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
