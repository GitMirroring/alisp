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

#include <getopt.h>

#include <gmp.h>
#include <readline/readline.h>
#include <readline/history.h>


#define CAR(list) ((list) == &nil_object ? &nil_object : (list)->value_ptr.cons_pair->car)

#define CDR(list) ((list) == &nil_object ? &nil_object :		\
		   (list)->value_ptr.cons_pair->cdr ? (list)->value_ptr.cons_pair->cdr : &nil_object)

#define SYMBOL(s) ((s)->type == TYPE_SYMBOL ? (s) : (s)->value_ptr.symbol_name->sym) 



/* not a C string. not null-terminated and explicit size. null bytes are allowed inside */

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
  struct binding *macros;
  struct binding *spec_ops;

  struct binding *packages;

  struct global_environment *glob_env;
  struct dynamic_environment *dyn_env;
  struct lexical_environment *lex_env;
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
    EMPTY_LIST = 1 << 6,
    INCOMPLETE_LIST = 1 << 7,
    INCOMPLETE_STRING = 1 << 8,
    INCOMPLETE_SYMBOL_NAME = 1 << 9,
    INCOMPLETE_SHARP_MACRO_CALL = 1 << 10,

    INVALID_SHARP_DISPATCH = 1 << 11,
    UNKNOWN_SHARP_DISPATCH = 1 << 12,

    UNFINISHED_SINGLELINE_COMMENT = 1 << 13,
    UNFINISHED_MULTILINE_COMMENT = 1 << 14,

    COMMA_WITHOUT_BACKQUOTE = 1 << 15,

    SINGLE_DOT = 1 << 16,

    MULTIPLE_DOTS = 1 << 17,

    NO_OBJ_BEFORE_DOT_IN_LIST = 1 << 18,
    NO_OBJ_AFTER_DOT_IN_LIST = 1 << 19,
    MULTIPLE_OBJS_AFTER_DOT_IN_LIST = 1 << 20
  };


#define INCOMPLETE_OBJECT (JUST_PREFIX | INCOMPLETE_LIST | INCOMPLETE_STRING | INCOMPLETE_SYMBOL_NAME |	INCOMPLETE_SHARP_MACRO_CALL)

#define READ_ERROR (CLOSING_PARENTHESIS_AFTER_PREFIX | CLOSING_PARENTHESIS | INVALID_SHARP_DISPATCH | UNKNOWN_SHARP_DISPATCH |\
		    COMMA_WITHOUT_BACKQUOTE | SINGLE_DOT | MULTIPLE_DOTS | NO_OBJ_BEFORE_DOT_IN_LIST | NO_OBJ_AFTER_DOT_IN_LIST |\
		    MULTIPLE_OBJS_AFTER_DOT_IN_LIST)


enum
eval_outcome
  {
    EVAL_OK,
    UNBOUND_SYMBOL,
    ILLEGAL_FUNCTION_CALL,
    WRONG_NUMBER_OF_ARGUMENTS,
    CANT_EVALUATE_LISTS_YET,
    UNKNOWN_FUNCTION,
    EVAL_NOT_IMPLEMENTED,
    MALFORMED_IF,
    INCORRECT_SYNTAX_IN_LET,
    INCORRECT_SYNTAX_IN_PROGN,
    INCORRECT_SYNTAX_IN_DEFUN,
    CANT_REDEFINE_CONSTANT,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS
  };


struct
symbol
{
  char *name;
  size_t name_len;

  int is_builtin_form;
  struct parameter *lambda_list;
  int evaluate_args;
  struct object *(*builtin_form) (struct object *list, struct environment *env, enum eval_outcome *outcome,
				  struct object **cursor);

  int is_const;
  int is_parameter;
  int is_special;

  struct object *value_cell;
  struct object *function_cell;

  struct package *package;
};


struct
symbol_name
{
  char *value;
  size_t alloc_size;
  size_t used_size;
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

  struct object *body;
};


struct
cons_pair
{
  int filling_car;  /* when car is incomplete but already partly allocated */
  int empty_list_in_car;  /* when car is a still empty list so nothing allocated yet */
  struct object *car;
  struct object *cdr;
};


/* a slight abuse of terminology: commas, quotes, backquotes and ats
 * are not Lisp objects.  But treating them as objects of type prefix,
 * we can implement them as a linked list before the proper object */

enum
object_type
  {
    TYPE_QUOTE = 1,
    TYPE_BACKQUOTE = 1 << 1,
    TYPE_COMMA = 1 << 2,
    TYPE_AT = 1 << 3,
    TYPE_SYMBOL_NAME = 1 << 4,
    TYPE_SYMBOL = 1 << 5,
    TYPE_INTEGER = 1 << 6,
    TYPE_RATIO = 1 << 7,
    TYPE_FLOAT = 1 << 8,
    TYPE_CONS_PAIR = 1 << 9,
    TYPE_CHARACTER = 1 << 10,
    TYPE_STRING = 1 << 11,
    TYPE_ARRAY = 1 << 12,
    TYPE_HASHTABLE = 1 << 13,
    TYPE_ENVIRONMENT = 1 << 14,
    TYPE_PACKAGE = 1 << 15,
    TYPE_PATHNAME = 1 << 16,
    TYPE_STREAM = 1 << 17,
    TYPE_STRUCTURE = 1 << 18,
    TYPE_CONDITION = 1 << 19,
    TYPE_FUNCTION = 1 << 20,
    TYPE_T = 1 << 21,
    TYPE_NIL = 1 << 22
  };


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
  struct environment *environment;
  struct package *package;
  struct function *function;
};


struct
object
{
  int refcount;
  const char *begin;
  const char *end;
  enum object_type type;
  union object_ptr_union value_ptr;  /* only when type is TYPE_NIL, this is allowed to be NULL */
};  


enum
package_record_visibility
  {
    INTERNAL_VISIBLITY,
    EXTERNAL_VISIBILITY
  };


struct
package_record
{
  enum package_record_visibility visibility;

  char *symname;
  size_t symname_len;

  struct symbol *symbol;

  struct package_record *next;
};


struct
package
{
  struct symbol *name;

  struct package_record *recs;
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


struct
line_list
{
  int refcount;
  char *line;
  size_t size;
  struct line_list *next;
};


struct
object_list
{
  struct object *obj;
  struct object_list *next;
};



int input_needs_continuation (const char *input, size_t size);

char *read_line_interactively (const char prompt []);

enum read_outcome read_object_continued (struct object **obj, int is_empty_list, const char *input, size_t size, const char **obj_begin,
					 const char **obj_end, struct object_list **symbol_list, size_t *mult_comm_depth);
struct object *complete_object_interactively (struct object *obj, int is_empty_list, struct object_list **symbol_list,
					      size_t multiline_comm_depth, const char **input_left, size_t *input_left_size);
struct object *read_object_interactively_continued (const char *input, size_t input_size, struct object_list **symbol_list,
						    const char **input_left, size_t *input_left_size);
struct object *read_object_interactively (struct object_list **symbol_list, const char **input_left, size_t *input_left_size);

const char *skip_space_block (const char *input, size_t size, size_t *new_size);
const char *jump_to_end_of_line (const char *input, size_t size, size_t *new_size);
const char *find_multiline_comment_delimiter (const char *input, size_t size, size_t *new_size);
const char *jump_to_end_of_multiline_comment (const char *input, size_t size, size_t depth, size_t *depth_or_new_size);

struct line_list *append_line_to_list (char *line, size_t size, struct line_list *list, int do_copy);
struct object_list *append_object_to_list (struct object *obj, struct object_list *list);

enum read_outcome read_object (struct object **obj, const char *input, size_t size, const char **obj_begin,
			       const char **obj_end, struct object_list **symbol_list, size_t *out_arg);

enum read_outcome read_list (struct object **obj, const char *input, size_t size, const char **list_end,
			     struct object_list **symbol_list, size_t *out_arg);

enum read_outcome read_string (struct object **obj, const char *input, size_t size, const char **string_end);

enum read_outcome read_symbol_name (struct object **obj, const char *input, size_t size, const char **symbol_end);

enum read_outcome read_prefix (struct object **obj, const char *input, size_t size, struct object **last, const char **prefix_end);

struct sharp_macro_call *read_sharp_macro_call (const char *input, size_t size, const char **macro_end, struct object_list **symbol_list,
						size_t *out_arg, enum read_outcome *outcome);
struct object *call_sharp_macro (struct sharp_macro_call *macro_call, enum read_outcome *outcome);

enum element find_next_element (const char *input, size_t size, const char **elem_begin);

int is_number (const char *token, size_t size, int radix, enum object_type *numtype, const char **number_end, const char **token_end);
struct object *alloc_number (const char *token, size_t size, int radix, enum object_type numtype);

void print_range (const char *begin, const char *end);
char *append_newline (char *string);

void *malloc_and_check (size_t size);
void *realloc_and_check (void *ptr, size_t size);

struct object *alloc_object (void);
struct object *alloc_prefix (enum element type);
struct object *alloc_empty_cons_pair (void);
struct object *alloc_function (void);

const char *find_end_of_string (const char *input, size_t size, size_t *new_size, size_t *string_length);
void normalize_string (char *output, const char *input, size_t size);

struct object *alloc_string (size_t size);
void resize_string (struct object *string, size_t size);

struct object *alloc_symbol_name (size_t size);
void resize_symbol_name (struct object *symname, size_t size);

const char *find_end_of_symbol_name (const char *input, size_t size, size_t *new_size, size_t *name_length, enum read_outcome *outcome);
void normalize_symbol_name (char *output, const char *input, size_t size);

struct object *create_symbol (char *name, size_t size);
struct object *intern_symbol (char *name, size_t len, struct object_list **symbol_list);
struct object *intern_symbol_name (struct object *symname, struct object_list **symbol_list);

struct binding *create_binding (struct object *sym, struct object *obj, enum binding_type type);
struct binding *add_binding (struct binding *bin, struct binding *env);
struct binding *chain_bindings (struct binding *bin, struct binding *env);
struct binding *remove_bindings (struct binding *env, int num);
struct binding *find_binding (struct symbol *sym, struct binding *env, enum binding_type type);

void add_builtin_form (char *name, struct object_list **symbol_list,
		       struct object *(*builtin_form) (struct object *list, struct environment *env, enum eval_outcome *outcome,
						       struct object **cursor));

struct object *define_constant (struct object *sym, struct object *form, struct environment *env,
				enum eval_outcome *outcome, struct object **cursor);
struct object *define_constant_by_name (char *name, size_t size, struct object_list **symbol_list,struct object *form,
					struct environment *env, enum eval_outcome *outcome, struct object **cursor);
struct object *define_parameter (struct object *sym, struct object *form, struct environment *env,
				 enum eval_outcome *outcome, struct object **cursor);
struct object *define_parameter_by_name (char *name, size_t size, struct object_list **symbol_list, struct object *form,
					 struct environment *env, enum eval_outcome *outcome, struct object **cursor);

struct object *skip_prefix (struct object *prefix, struct object **last_prefix);
struct object *append_prefix (struct object *obj, enum element type);

struct object *nth (unsigned int ind, struct object *list);
struct object *nthcdr (unsigned int ind, struct object *list);
unsigned int list_length (const struct object *list);

void copy_symbol_name (char *out, const struct symbol_name *name);
struct parameter *alloc_parameter (enum parameter_type type, struct object *sym);
struct parameter *parse_required_parameters (struct object *obj, struct parameter **last, struct object **next,
					     enum parse_lambda_list_outcome *out);
struct parameter *parse_optional_parameters (struct object *obj, struct parameter **last, struct object **next,
					     enum parse_lambda_list_outcome *out);
struct parameter *parse_lambda_list (struct object *obj, enum parse_lambda_list_outcome *out);
struct object *call_function (struct object *func, struct object *arglist, struct environment *env,
			      enum eval_outcome *outcome, struct object **cursor);

int check_type (const struct object *obj, const struct typespec *type);

struct object *evaluate_object (struct object *obj, struct environment *env, enum eval_outcome *outcome, struct object **cursor);
struct object *evaluate_list (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor);

struct object *builtin_car (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor);

struct binding *create_binding_from_let_form (struct object *form, struct environment *env, enum eval_outcome *outcome,
					   struct object **cursor);
struct object *evaluate_let (struct object *bind_forms, struct object *body, struct environment *env, enum eval_outcome *outcome,
			     struct object **cursor);
struct object *evaluate_let_star (struct object *bind_forms, struct object *body, struct environment *env, enum eval_outcome *outcome,
				  struct object **cursor);

struct object *evaluate_if (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor);
struct object *evaluate_progn (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor);
struct object *evaluate_defconstant (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor);
struct object *evaluate_defparameter (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor);
struct object *evaluate_defvar (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor);
struct object *evaluate_defun (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor);

int eqmem (const char *s1, size_t n1, const char *s2, size_t n2);
int symname_equals (const struct symbol_name *sym, const char *s);
int symname_is_among (const struct symbol_name *sym, ...);
int equal_strings (const struct string *s1, const struct string *s2);

void print_symbol (const struct symbol *sym);
void print_string (const struct string *str);
void print_list (const struct cons_pair *list);
void print_object (const struct object *obj);

void print_read_error (enum read_outcome err, const char *input, size_t size, const char *begin, const char *end);
void print_eval_error (enum eval_outcome err, struct object *arg);

int decrement_refcount (struct object *obj);

void free_object (struct object *obj);
void free_string (struct object *obj);
void free_symbol (struct object *obj);
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

struct object nil_symbol = {1, NULL, NULL, TYPE_SYMBOL, .value_ptr.symbol = &_nil_symbol};


struct object t_object = {1, NULL, NULL, TYPE_T};



int
main (int argc, char *argv [])
{
  int end_repl = 0;

  struct object *result, *cursor, *obj;
  struct object_list *sym_list = NULL;
  struct environment env = {NULL};
  
  enum eval_outcome eval_out;

  const char *input_left = NULL;
  size_t input_left_s = 0;
  
  int c, option_index = 0;

  
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

  define_constant_by_name ("NIL", strlen ("NIL"), &sym_list, &nil_object, &env, &eval_out, &cursor);
  define_constant_by_name ("T", strlen ("T"), &sym_list, &t_object, &env, &eval_out, &cursor);

  add_builtin_form ("CAR", &sym_list, builtin_car);
  add_builtin_form ("IF", &sym_list, evaluate_if);
  add_builtin_form ("PROGN", &sym_list, evaluate_progn);
  add_builtin_form ("DEFCONSTANT", &sym_list, evaluate_defconstant);
  add_builtin_form ("DEFPARAMETER", &sym_list, evaluate_defparameter);
  add_builtin_form ("DEFVAR", &sym_list, evaluate_defvar);
  add_builtin_form ("DEFUN", &sym_list, evaluate_defun);


  print_welcome_message ();

  while (!end_repl)
    {
      obj = read_object_interactively (&sym_list, &input_left, &input_left_s);
      
      while (obj && input_left && input_left_s > 0)
	{
	  result = evaluate_object (obj, &env, &eval_out, &cursor);

	  if (result)
	    {
	      print_object (result);
	      printf ("\n");
	    }
	  else
	    print_eval_error (eval_out, cursor);

	  decrement_refcount (result);
	  decrement_refcount (obj);

	  obj = read_object_interactively_continued (input_left, input_left_s, &sym_list, &input_left, &input_left_s);
	}

      if (obj)
	{
	  result = evaluate_object (obj, &env, &eval_out, &cursor);

	  if (result)
	    {
	      print_object (result);
	      printf ("\n");
	    }
	  else
	    print_eval_error (eval_out, cursor);

	  decrement_refcount (result);
	  decrement_refcount (obj);
	}
    }
  
  return 0;
}


int
input_needs_continuation (const char *input, size_t size)
{
  return 0;
}


char *
read_line_interactively (const char prompt [])
{
  char *line = readline (prompt);
      
  if (line && *line)
    add_history (line);
  
  line = append_newline (line);

  return line;
}


enum read_outcome
read_object_continued (struct object **obj, int is_empty_list, const char *input, size_t size, const char **obj_begin,
		       const char **obj_end, struct object_list **symbol_list, size_t *mult_comm_depth)
{
  enum read_outcome out;
  struct object *last_pref, *ob = skip_prefix (*obj, &last_pref);
  struct object *l;

  if (*mult_comm_depth)
    {
      input = jump_to_end_of_multiline_comment (input, size, *mult_comm_depth, mult_comm_depth);

      if (!input)
	return NO_OBJECT;

      input++;
      size = --(*mult_comm_depth);
      *mult_comm_depth = 0;
    }

  if (is_empty_list)
    {
      l = NULL;

      out = read_list (&l, input, size, obj_end, symbol_list, mult_comm_depth);

      ob = l;
    }
  else if (!ob)
    {
      out = read_object (&ob, input, size, obj_begin, obj_end, symbol_list, mult_comm_depth);

      if (out == NO_OBJECT && last_pref)
	out = JUST_PREFIX;
    }
  else if (ob->type == TYPE_CONS_PAIR)
    {
      out = read_list (&ob, input, size, obj_end, symbol_list, mult_comm_depth);
    }
  else if (ob->type == TYPE_STRING)
    {
      out = read_string (&ob, input, size, obj_end);
    }
  else if (ob->type == TYPE_SYMBOL_NAME)
    {
      out = read_symbol_name (&ob, input, size, obj_end);

      if (out == COMPLETE_OBJECT)
	intern_symbol_name (ob, symbol_list);
    }

  if (last_pref)
    last_pref->value_ptr.next = ob;
  else
    *obj = ob;

  return out;
}


struct object *
complete_object_interactively (struct object *obj, int is_empty_list, struct object_list **symbol_list,
			       size_t multiline_comm_depth, const char **input_left, size_t *input_left_size)  
{
  char *line;
  enum read_outcome read_out;
  const char *begin, *end;
  size_t len;

  
  line = read_line_interactively ("> ");
  len = strlen (line);
  
  read_out = read_object_continued (&obj, is_empty_list, line, len, &begin, &end, symbol_list, &multiline_comm_depth);

  while (read_out & (INCOMPLETE_LIST | INCOMPLETE_STRING | INCOMPLETE_SYMBOL_NAME | JUST_PREFIX
		     | INCOMPLETE_SHARP_MACRO_CALL | UNFINISHED_MULTILINE_COMMENT | EMPTY_LIST)
	 || multiline_comm_depth)
    {
      line = read_line_interactively ("> ");
      len = strlen (line);

      if (read_out & EMPTY_LIST)
	read_out = read_object_continued (&obj, 1, line, len, &begin, &end, symbol_list, &multiline_comm_depth);
      else
	read_out = read_object_continued (&obj, 0, line, len, &begin, &end, symbol_list, &multiline_comm_depth);
    }

  *input_left = end + 1;
  *input_left_size = (line + len) - end - 1;
  
  return obj;
}


struct object *
read_object_interactively_continued (const char *input, size_t input_size, struct object_list **symbol_list,
				     const char **input_left, size_t *input_left_size)
{
  enum read_outcome read_out;
  struct object *obj = NULL;
  const char *begin, *end;
  size_t mult_comm_depth = 0;

  
  read_out = read_object (&obj, input, input_size, &begin, &end, symbol_list, &mult_comm_depth);
  
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
  else if (read_out == EMPTY_LIST)
    {
      return complete_object_interactively (obj, 1, symbol_list, mult_comm_depth, input_left, input_left_size);
    }
  else
    return complete_object_interactively (obj, 0, symbol_list, mult_comm_depth, input_left, input_left_size);
}


struct object *
read_object_interactively (struct object_list **symbol_list, const char **input_left, size_t *input_left_size)
{
  char *line = read_line_interactively ("al> ");

  return read_object_interactively_continued (line, strlen (line), symbol_list, input_left, input_left_size);
}


const char *
skip_space_block (const char *input, size_t size, size_t *new_size)
{
  int i;

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
find_multiline_comment_delimiter (const char *input, size_t size, size_t *new_size)
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
jump_to_end_of_multiline_comment (const char *input, size_t size, size_t depth, size_t *depth_or_new_size)
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
append_line_to_list (char *line, size_t size, struct line_list *list, int do_copy)
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
read_object (struct object **obj, const char *input, size_t size, const char **obj_begin,
	     const char **obj_end, struct object_list **symbol_list, size_t *out_arg)
{
  int found_prefix = 0;
  struct object *last_pref, *ob = NULL;
  enum object_type numtype;
  enum read_outcome out = NO_OBJECT;
  const char *num_end;
  struct sharp_macro_call *sharp_m;
  
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
	  if (!(input = jump_to_end_of_multiline_comment (input+2, size-2, 1, out_arg)))
	    return NO_OBJECT;
	  else
	    {
	      size = *out_arg;
	      *out_arg = 0;
	    }
	}
      else if (*input == '\'' || *input == '`' || *input == ',' || *input == '@')
 	{
	  read_prefix (obj, input, size, &last_pref, obj_end);
	  size = size - (*obj_end - input);
	  input = *obj_end;
	  found_prefix = 1;
	}
      else if (*input == ')')
	{
	  *obj_end = input;
	  return found_prefix ? CLOSING_PARENTHESIS_AFTER_PREFIX : CLOSING_PARENTHESIS;
	}
      else if (*input == '(')
	{
	  *obj_begin = input;
	  out = read_list (&ob, input+1, size-1, obj_end, symbol_list, out_arg);
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
	  sharp_m = read_sharp_macro_call (input+1, size-1, obj_end, symbol_list, out_arg, &out);

	  if (out == COMPLETE_OBJECT)
	    ob = call_sharp_macro (sharp_m, &out);

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
	      out = read_symbol_name (&ob, input, size, obj_end);

	      if (out == COMPLETE_OBJECT)
		intern_symbol_name (ob, symbol_list);
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
read_list (struct object **obj, const char *input, size_t size, const char **list_end,
	   struct object_list **symbol_list, size_t *out_arg)
{
  struct object *last_cons = NULL, *car = NULL, *ob, *last_pref, *cons;
  const char *obj_beg, *obj_end = NULL;
  enum read_outcome out;
  int found_dot = 0, dotted_list_full = 0;


  if (!size)
    return EMPTY_LIST;

  ob = skip_prefix (*obj, &last_pref);

  while (ob && ob != &nil_object)
    {
      if (ob->value_ptr.cons_pair->filling_car)
	{
	  out = read_object_continued (&ob->value_ptr.cons_pair->car, 0, input, size, &obj_beg, &obj_end, symbol_list, out_arg);

	  if (out == COMPLETE_OBJECT || out == CLOSING_PARENTHESIS)
	      ob->value_ptr.cons_pair->filling_car = 0;
	  else if (out == NO_OBJECT || out == EMPTY_LIST || out == UNFINISHED_MULTILINE_COMMENT || out & READ_ERROR
		   || out & INCOMPLETE_OBJECT)
	    return out;
	}
      else if (ob->value_ptr.cons_pair->empty_list_in_car)
	{
	  out = read_object_continued (&ob->value_ptr.cons_pair->car, 1, input, size, &obj_beg, &obj_end, symbol_list, out_arg);

	  if (out != EMPTY_LIST)
	    ob->value_ptr.cons_pair->empty_list_in_car = 0;

	  if (out & INCOMPLETE_OBJECT)
	    {
	      ob->value_ptr.cons_pair->filling_car = 1;
	      return out;
	    }

	  if (out == NO_OBJECT || out == EMPTY_LIST || out == UNFINISHED_MULTILINE_COMMENT || out & READ_ERROR)
	    return out;
	}

      last_cons = ob;
      ob = ob->value_ptr.cons_pair->cdr;      
    }

  if (obj_end)
    {
      out = read_object (&car, obj_end + 1, size - (obj_end + 1 - input), &obj_beg, &obj_end, symbol_list, out_arg);
    }
  else
    {
      out = read_object (&car, input, size, &obj_beg, &obj_end, symbol_list, out_arg);
    }

  if (out == NO_OBJECT && !last_cons)
    return EMPTY_LIST;

  if (out == CLOSING_PARENTHESIS && !last_cons)
    {
      *list_end = obj_end;
      *obj = &nil_object;
      return COMPLETE_OBJECT;
    }

  while (out != NO_OBJECT && out != EMPTY_LIST)
    {
      if (out == CLOSING_PARENTHESIS)
	{
	  if (found_dot && !dotted_list_full)
	    return NO_OBJ_AFTER_DOT_IN_LIST;

	  *list_end = obj_end;
	  return COMPLETE_OBJECT;
	}
      else if (out == UNFINISHED_MULTILINE_COMMENT)
	{
	  return out;
	}
      else if (out == SINGLE_DOT)
	{
	  found_dot = 1;
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

		  return INCOMPLETE_LIST;
		}
	    }
	}

      if (obj_end == input + size)
	break;

      car = NULL;
      out = read_object (&car, obj_end + 1, size - (obj_end + 1 - input), &obj_beg, &obj_end, symbol_list, out_arg);
    }

  if (out == EMPTY_LIST)
    {
      cons = alloc_empty_cons_pair ();
      cons->value_ptr.cons_pair->empty_list_in_car = 1;

      if (last_cons)
	last_cons->value_ptr.cons_pair->cdr = cons;
      else
	*obj = cons;
    }

  return INCOMPLETE_LIST;
}


enum read_outcome 
read_string (struct object **obj, const char *input, size_t size, const char **string_end)
{
  size_t length, new_size;
  struct string *str;
  enum read_outcome out = COMPLETE_OBJECT;
  struct object *last_pref, *ob;


  *string_end = find_end_of_string (input, size, &new_size, &length);

  if (!*string_end)
    out = INCOMPLETE_STRING;
    
  ob = skip_prefix (*obj, &last_pref);
  
  if (!ob)
    {
      ob = alloc_string (length);
      if (last_pref)
	last_pref->value_ptr.next = ob;
      else
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
read_symbol_name (struct object **obj, const char *input, size_t size, const char **symbol_end)
{
  struct symbol_name *sym;
  size_t length, new_size;
  struct object *last_pref, *ob;
  enum read_outcome out;
  
  
  *symbol_end = find_end_of_symbol_name (input, size, &new_size, &length, &out);

  if (out == SINGLE_DOT || out == MULTIPLE_DOTS)
    return out;
  
  ob = skip_prefix (*obj, &last_pref);

  if (!ob)
    {
      ob = alloc_symbol_name (length);
      if (last_pref)
	last_pref->value_ptr.next = ob;
      else
	*obj = ob;
    }
  else
    resize_symbol_name (ob, ob->value_ptr.symbol_name->used_size + length);  

  if (!length)
    return COMPLETE_OBJECT;
    
  sym = ob->value_ptr.symbol_name;

  normalize_symbol_name (sym->value + sym->used_size, input, size);

  sym->used_size += length;

  if (!*symbol_end)
    return INCOMPLETE_SYMBOL_NAME;
  else
    return COMPLETE_OBJECT;
}


enum read_outcome
read_prefix (struct object **obj, const char *input, size_t size, struct object **last, const char **prefix_end)
{
  const char *n = input;
  enum element el;
  
  if (!size)
    return NO_OBJECT;

  skip_prefix (*obj, last);
  
  el = find_next_element (input, size, &n);
  
  while (el == QUOTE || el == BACKQUOTE || el == COMMA || el == AT)
    {
      *prefix_end = n;
      
      if (!*last)
	*obj = *last = alloc_prefix (el);
      else
	*last = (*last)->value_ptr.next = alloc_prefix (el);
      
      el = find_next_element (n+1, size - (n + 1 - input), &n);
    }

  return JUST_PREFIX;
}


struct sharp_macro_call *
read_sharp_macro_call (const char *input, size_t size, const char **macro_end, struct object_list **symbol_list,
		       size_t *out_arg, enum read_outcome *outcome)
{
  int arg, i = 0;
  const char *obj_b, *obj_e;
  struct sharp_macro_call *call;

  if (!size)
    return NULL;

  call = malloc_and_check (sizeof (*call));
  
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
    return NULL;

  if (strchr ("\b\t\n\r\f ", call->dispatch_ch))
    {
      *outcome = INVALID_SHARP_DISPATCH;
      return NULL;
    }

  if (!strchr ("'\\", call->dispatch_ch))
    {
      *outcome = UNKNOWN_SHARP_DISPATCH;
      return NULL;
    }

  call->obj = NULL;
  *outcome = read_object (&call->obj, input+i+1, size-i-1, &obj_b, &obj_e, symbol_list, out_arg);
  
  return call;
}


struct object *
call_sharp_macro (struct sharp_macro_call *macro_call, enum read_outcome *outcome)
{
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
      break;
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
is_number (const char *token, size_t size, int radix, enum object_type *numtype, const char **number_end, const char **token_end)
{
  int i = 0;
  
  int found_dec_point = 0, found_exp_marker = 0, exp_marker_pos, found_slash = 0, found_dec_digit = 0,
    found_digit = 0, found_digit_after_slash = 0, found_digit_after_exp_marker = 0, need_decimal_digit = 0;
  
  char decimal_digits [] = "0123456789";
  char digits [] = "00112233445566778899aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ";
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
	  if (i > 0 && found_exp_marker && i - exp_marker_pos > 1)
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
      else if (i == 0)
	return 0;
      else
	break;
      
      i++;
    }

  if (found_slash && !found_digit_after_slash)
    return 0;
  if (found_exp_marker && !found_digit_after_exp_marker)
    return 0;
  if (need_decimal_digit)
    return 0;

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
  char *newstring = realloc (string, len + 2);
  
  newstring [len] = '\n';
  newstring [len+1] = 0;

  return newstring;
}


void *
malloc_and_check (size_t size)
{
  void *mem = malloc (size);

  if (!mem)
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

  if (!mem)
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
  cons->cdr = NULL;

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
  fun->body = NULL;

  obj->type = TYPE_FUNCTION;
  obj->refcount = 1;
  obj->value_ptr.function = fun;

  return obj;
}


const char *
find_end_of_string (const char *input, size_t size, size_t *new_size, size_t *string_length)
{
  int i = 0, escape = 0;

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
  int escape = 0, i = 0, j = 0;

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
  string->value_ptr.string->value = realloc_and_check (string->value_ptr.string->value,
						      sizeof (char) * size);

  if (size < string->value_ptr.string->used_size)
    string->value_ptr.string->used_size = size;

  string->value_ptr.string->alloc_size = sizeof (char) * size;
}


struct object *
alloc_symbol_name (size_t size)
{
  struct object *obj = malloc_and_check (sizeof (*obj));

  obj->type = TYPE_SYMBOL_NAME;
  obj->refcount = 1;

  obj->value_ptr.symbol_name = malloc_and_check (sizeof (*obj->value_ptr.symbol_name));

  obj->value_ptr.symbol_name->value = malloc_and_check (sizeof (char) * size);
  obj->value_ptr.symbol_name->alloc_size = sizeof (char) * size;
  obj->value_ptr.symbol_name->used_size = 0;
  obj->value_ptr.symbol_name->sym = NULL;

  return obj;
}


void
resize_symbol_name (struct object *symname, size_t size)
{
  symname->value_ptr.symbol_name->value = realloc_and_check (symname->value_ptr.symbol_name->value,
							     sizeof (char) * size);
  
  if (size < symname->value_ptr.symbol_name->used_size)
    symname->value_ptr.symbol_name->used_size = size;

  symname->value_ptr.symbol_name->alloc_size = sizeof (char) * size;
}


const char *
find_end_of_symbol_name (const char *input, size_t size, size_t *new_size, size_t *name_length, enum read_outcome *outcome)
{
  int i = 0, single_escape = 0, multiple_escape = 0, just_dots = 1;
  const char term_macro_chars [] = "()';\"`,";
  
  *name_length = 0;
  
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
	      (*name_length)++;
	    }
	}
      else if (input [i] == '|' && !single_escape)
	{
	  just_dots = 0;
	  
	  multiple_escape = (multiple_escape ? 0 : 1);
	}
      else
	{
	  if ((isspace (input [i]) || strchr (term_macro_chars, input [i]))
	      && !single_escape && !multiple_escape)
	    {
	      if (just_dots && *name_length == 1)
		*outcome = SINGLE_DOT;
	      else if (just_dots)
		*outcome = MULTIPLE_DOTS;
	      
	      *new_size = size-i+1;
	      return input+i-1;
	    }

	  if (input [i] != '.')
	    just_dots = 0;
	  
	  (*name_length)++;
	  single_escape = 0;
	}
      i++;
    }

  return NULL;
}


void
normalize_symbol_name (char *output, const char *input, size_t size)
{
  int i, j, single_escape = 0, multiple_escape = 0;
  const char term_macro_chars [] = "()';\"`,";
  
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
      else if ((isspace (input [i]) || strchr (term_macro_chars, input [i]))
	       && !single_escape && !multiple_escape)
	{
	  break;
	}
      else
	{
	  if (single_escape || multiple_escape)
	    output [j++] = input [i];
	  else
	    output [j++] = toupper (input [i]);
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
intern_symbol (char *name, size_t len, struct object_list **symbol_list)
{
  struct object *sym;
  struct object_list *cur, *new_sym;
  
  cur = *symbol_list;

  while (cur)
    {
      if (eqmem (cur->obj->value_ptr.symbol->name, cur->obj->value_ptr.symbol->name_len, name, len))
	{
	  cur->obj->refcount++;
	  return cur->obj;
	}
      
      cur = cur->next;
    }
  
  sym = create_symbol (name, len);

  new_sym = malloc_and_check (sizeof (*new_sym));
  new_sym->obj = sym;
  new_sym->next = *symbol_list;

  *symbol_list = new_sym;
  
  return sym;  
}


struct object *
intern_symbol_name (struct object *symname, struct object_list **symbol_list)
{
  return (symname->value_ptr.symbol_name->sym = intern_symbol (symname->value_ptr.symbol_name->value,
							       symname->value_ptr.symbol_name->used_size,
							       symbol_list));
}


struct binding *
create_binding (struct object *sym, struct object *obj, enum binding_type type)
{
  struct binding *bin = malloc_and_check (sizeof (*bin));

  bin->type = type;
  bin->sym = sym;
  bin->obj = obj;
  bin->next = NULL;

  sym->refcount++;
  obj->refcount++;

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
add_builtin_form (char *name, struct object_list **symbol_list,
		  struct object *(*builtin_form) (struct object *list, struct environment *env, enum eval_outcome *outcome,
						  struct object **cursor))
{
  struct object *sym = intern_symbol (name, strlen (name), symbol_list);

  sym->value_ptr.symbol->is_builtin_form = 1;
  sym->value_ptr.symbol->builtin_form = builtin_form;
  sym->refcount++;
}


struct object *
define_constant (struct object *sym, struct object *form, struct environment *env,
		 enum eval_outcome *outcome, struct object **cursor)
{
  struct object *val = evaluate_object (form, env, outcome, cursor);
  
  if (!val)
    return NULL;

  if (SYMBOL (sym)->value_ptr.symbol->is_const)
    {
      *outcome = CANT_REDEFINE_CONSTANT;
      return NULL;
    }

  SYMBOL (sym)->value_ptr.symbol->is_const = 1;
  SYMBOL (sym)->value_ptr.symbol->value_cell = val;
  SYMBOL (sym)->refcount++;

  return sym;
}


struct object *
define_constant_by_name (char *name, size_t size, struct object_list **symbol_list, struct object *form,
			 struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  struct object *sym = intern_symbol (name, size, symbol_list);

  return define_constant (sym, form, env, outcome, cursor);
}


struct object *
define_parameter (struct object *sym, struct object *form, struct environment *env,
		  enum eval_outcome *outcome, struct object **cursor)
{
  struct object *s;
  struct object *val = evaluate_object (form, env, outcome, cursor);

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
define_parameter_by_name (char *name, size_t size, struct object_list **symbol_list, struct object *form,
			  struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  struct object *sym = intern_symbol (name, size, symbol_list);

  return define_parameter (sym, form, env, outcome, cursor);
}


struct object *
skip_prefix (struct object *prefix, struct object **last_prefix)
{
  if (last_prefix)
    *last_prefix = NULL;
  
  while (prefix &&
	 (prefix->type == TYPE_QUOTE
	  || prefix->type == TYPE_BACKQUOTE
	  || prefix->type == TYPE_COMMA))
    {
      if (last_prefix)
	*last_prefix = prefix;
      prefix = prefix->value_ptr.next;
    }

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
  int i;

  for (i = 0; i < ind; i++)
    if (!list->value_ptr.cons_pair->cdr)
      return &nil_object;
    else
      list = list->value_ptr.cons_pair->cdr;

  return list->value_ptr.cons_pair->car;
}


struct object *
nthcdr (unsigned int ind, struct object *list)
{
  int i;

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

  if (list)
    l++;

  return l;
}


void
copy_symbol_name (char *out, const struct symbol_name *name)
{
  
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
parse_required_parameters (struct object *obj, struct parameter **last, struct object **rest, enum parse_lambda_list_outcome *out)
{
  struct object *car;
  struct parameter *first = NULL;

  *last = NULL;
  
  while (obj && (car = obj->value_ptr.cons_pair->car)
	 && car->type == TYPE_SYMBOL_NAME 
	 && !symname_is_among (car->value_ptr.symbol_name, "&OPTIONAL", "&REST", "&KEYWORD", "&AUX",
			       "&ALLOW_OTHER_KEYS", NULL))
    {
      if (!first)
	*last = first = alloc_parameter (REQUIRED_PARAM, SYMBOL (car));
      else
	*last = (*last)->next = alloc_parameter (REQUIRED_PARAM, SYMBOL (car));
      
      obj = obj->value_ptr.cons_pair->cdr;
    }

  *rest = obj;
  
  return first;
}


struct parameter *
parse_optional_parameters (struct object *obj, struct parameter **last, struct object **next, enum parse_lambda_list_outcome *out)
{
  struct object *car;
  struct parameter *first = NULL;

  *last = NULL;
  
  while (obj && (car = obj->value_ptr.cons_pair->car))
    {
      if (car->type == TYPE_SYMBOL_NAME 
	  && symname_is_among (car->value_ptr.symbol_name, "&OPTIONAL", "&REST", "&KEYWORD", "&AUX",
			       "&ALLOW_OTHER_KEYS", NULL))
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
	last->next = parse_optional_parameters (obj->value_ptr.cons_pair->cdr, &newlast, &obj, out);
      else
	first = parse_optional_parameters (obj->value_ptr.cons_pair->cdr, &last, &obj, out);
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
call_function (struct object *func, struct object *arglist, struct environment *env,
	       enum eval_outcome *outcome, struct object **cursor)
{
  struct parameter *par = func->value_ptr.function->lambda_list;
  struct binding *bins = NULL;
  struct object *val, *res;
  int args = 0;

  while (arglist != &nil_object && par
	 && (par->type == REQUIRED_PARAM || par->type == OPTIONAL_PARAM))
    {
      val = evaluate_object (CAR (arglist), env, outcome, cursor);

      if (!val)
	return NULL;

      bins = add_binding (create_binding (par->name, val, LEXICAL_BINDING), bins);
      args++;

      if (par->type == OPTIONAL_PARAM && par->supplied_p_param)
	{
	  bins = add_binding (create_binding (par->supplied_p_param, &t_object, LEXICAL_BINDING), bins);
	  args++;
	}

      par = par->next;

      arglist = CDR (arglist);
    }

  if (par && par->type == REQUIRED_PARAM)
    {
      *outcome = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (arglist != &nil_object && (!par || (par && par->type != REST_PARAM)))
    {
      *outcome = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  while (par && par->type == OPTIONAL_PARAM)
    {
      if (par->init_form)
	{
	  val = evaluate_object (par->init_form, env, outcome, cursor);

	  if (!val)
	    return NULL;

	  bins = add_binding (create_binding (par->name, val, LEXICAL_BINDING), bins);
	  args++;
	}
      else
	{
	  bins = add_binding (create_binding (par->name, &nil_object, LEXICAL_BINDING), bins);
	  args++;
	}

      if (par->supplied_p_param)
	{
	  bins = add_binding (create_binding (par->supplied_p_param, &nil_object, LEXICAL_BINDING), bins);
	  args++;
	}

      par = par->next;
    }

  if (par && par->type == REST_PARAM)
    bins = add_binding (create_binding (par->name, arglist, LEXICAL_BINDING), bins);

  env->vars = chain_bindings (bins, env->vars);

  res = evaluate_progn (func->value_ptr.function->body, env, outcome, cursor);

  env->vars = remove_bindings (env->vars, args);

  return res;
}


int
check_type (const struct object *obj, const struct typespec *type)
{
  if (type->type == TYPESPEC_INTERNAL_OR)
    return obj->type & type->int_value;
  else if (type->type == TYPESPEC_INTERNAL_AND)
    {

    }
  else if (type->type == TYPESPEC_OR)
    return check_type (obj, type->first) || check_type (obj, type->second);
  else if (type->type == TYPESPEC_AND)
    return check_type (obj, type->first) && check_type (obj, type->second);

  return 0;
}


struct object *
evaluate_object (struct object *obj, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  struct binding *bind;
  struct object *sym = obj;
  
  if (obj->type == TYPE_T || obj->type == TYPE_NIL || obj->type == TYPE_INTEGER
      || obj->type == TYPE_RATIO || obj->type == TYPE_FLOAT || obj->type == TYPE_CHARACTER
      || obj->type == TYPE_STRING)
    {
      obj->refcount++;
      return obj;
    }
  else if (obj->type == TYPE_QUOTE || obj->type == TYPE_BACKQUOTE)
    {
      obj->value_ptr.next->refcount++;
      return obj->value_ptr.next;
    }
  else if (obj->type == TYPE_SYMBOL || obj->type == TYPE_SYMBOL_NAME)
    {
      if (obj->type == TYPE_SYMBOL_NAME)
	sym = obj->value_ptr.symbol_name->sym;

      if (sym->value_ptr.symbol->is_const)
	{
	  sym->refcount++;
	  return sym->value_ptr.symbol->value_cell;
	}
      else if (sym->value_ptr.symbol->is_parameter || sym->value_ptr.symbol->is_special)
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
	      *outcome = UNBOUND_SYMBOL;
	      *cursor = sym;
	      return NULL;
	    }
	}
    }
  else if (obj->type == TYPE_CONS_PAIR)
    {
      return evaluate_list (obj, env, outcome, cursor); 
    }

  *outcome = EVAL_NOT_IMPLEMENTED;
  return NULL;
}


struct object *
evaluate_list (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  struct symbol_name *symname;
  struct binding *bind;

  if (CAR (list)->type != TYPE_SYMBOL_NAME)
    {
      *outcome = ILLEGAL_FUNCTION_CALL;
      *cursor = CAR (list);
      return NULL;
    }

  symname = CAR (list)->value_ptr.symbol_name;

  if (symname->sym->value_ptr.symbol->is_builtin_form)
    return symname->sym->value_ptr.symbol->builtin_form (CDR (list), env, outcome, cursor);

  if (symname_equals (symname, "LET"))
    {
      if (!(CDR (list)->type & TYPE_LIST))
	{
	  *outcome = INCORRECT_SYNTAX_IN_LET;
	  return NULL;
	}

      return evaluate_let (CAR (CDR (list)), CDR (CDR (list)), env, outcome, cursor);
    }
  else if (symname_equals (symname, "LET*"))
    {
      if (!(CDR (list)->type & TYPE_LIST))
	{
	  *outcome = INCORRECT_SYNTAX_IN_LET;
	  return NULL;
	}

      return evaluate_let_star (CAR (CDR (list)), CDR (CDR (list)), env, outcome, cursor);
    }
  else if (symname_equals (symname, "QUOTE"))
    {
      CAR (CDR (list))->refcount++;
      return CAR (CDR (list));
    }

  bind = find_binding (symname->sym->value_ptr.symbol, env->funcs, DYNAMIC_BINDING);

  if (bind)
    return call_function (bind->obj, CDR (list), env, outcome, cursor);

  *outcome = UNKNOWN_FUNCTION;
  *cursor = CAR (list);
  return NULL;
}


struct object *
builtin_car (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  return CAR (CAR (list));
}


struct binding *
create_binding_from_let_form (struct object *form, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  struct object *sym, *val;

  if (form->type == TYPE_SYMBOL_NAME || form->type == TYPE_SYMBOL)
    {
      sym = SYMBOL (form);

      return create_binding (sym, &nil_object, LEXICAL_BINDING);
    }
  else if (form->type == TYPE_CONS_PAIR)
    {
      if (list_length (form) != 2
	  || (CAR (form)->type != TYPE_SYMBOL_NAME && CAR (form)->type != TYPE_SYMBOL)) 
	{
	  *outcome = INCORRECT_SYNTAX_IN_LET;
	  return NULL;
	}

      sym = SYMBOL (CAR (form));

      val = evaluate_object (CAR (CDR (form)), env, outcome, cursor);

      if (!val)
	return NULL;

      return create_binding (sym, val, LEXICAL_BINDING);
    }
  else
    {
      *outcome = INCORRECT_SYNTAX_IN_LET;
      return NULL;
    }
}


struct object *
evaluate_let (struct object *bind_forms, struct object *body, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  struct object *res;
  int binding_num = 0;
  struct binding *bins = NULL, *bin;

  while (bind_forms != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome, cursor);

      if (!bin)
	return NULL;

      bins = add_binding (bin, bins);
      binding_num++;

      bind_forms = CDR (bind_forms);
    }

  env->vars = chain_bindings (bins, env->vars);

  res = evaluate_progn (body, env, outcome, cursor);

  env->vars = remove_bindings (env->vars, binding_num);

  return res;
}


struct object *
evaluate_let_star (struct object *bind_forms, struct object *body, struct environment *env, enum eval_outcome *outcome,
		   struct object **cursor)
{
  struct object *res;
  int binding_num = 0;
  struct binding *bin;

  while (bind_forms != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome, cursor);

      if (!bin)
	return NULL;

      env->vars = add_binding (bin, env->vars);
      binding_num++;

      bind_forms = CDR (bind_forms);
    }

  res = evaluate_progn (body, env, outcome, cursor);

  env->vars = remove_bindings (env->vars, binding_num);

  return res;
}


struct object *
evaluate_if (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  struct object *if_clause;

  if (!list)
    {
      *outcome = MALFORMED_IF;
      return NULL;
    }

  if_clause = evaluate_object (CAR (list), env, outcome, cursor);

  if (!if_clause)
    return NULL;

  if (if_clause != &nil_object)
    {
      if (CDR (list) == &nil_object)
	{
	  *outcome = MALFORMED_IF;
	  return NULL;
	}

      return evaluate_object (CAR (CDR (list)), env, outcome, cursor);
    }
  else
    {
      if (!nth (2, list))
	{
	  return &nil_object;
	}
      else
	return evaluate_object (nth (2, list), env, outcome, cursor);
    }
}


struct object *
evaluate_progn (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  struct object *res;

  if (!list || list == &nil_object)
    return &nil_object;

  if (list->type != TYPE_CONS_PAIR)
    {
      *outcome = INCORRECT_SYNTAX_IN_PROGN;

      return NULL;
    }

  res = evaluate_object (list->value_ptr.cons_pair->car, env, outcome, cursor);

  while (res && (list = list->value_ptr.cons_pair->cdr))
    {
      if (list->type != TYPE_CONS_PAIR)
	return evaluate_object (list, env, outcome, cursor);
      else
	res = evaluate_object (list->value_ptr.cons_pair->car, env, outcome, cursor);
    }

  return res;
}


struct object *
evaluate_defconstant (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  return define_constant (CAR (list)->value_ptr.symbol_name->sym, CAR (CDR (list)), env, outcome, cursor);
}


struct object *
evaluate_defparameter (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  if (list_length (list) != 2)
    {
      *outcome = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  return define_parameter (CAR (list), CAR (CDR (list)), env, outcome, cursor);
}


struct object *
evaluate_defvar (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
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
	return define_parameter (CAR (list), CAR (CDR (list)), env, outcome, cursor);
    }
  else
    {
      *outcome = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  return CAR (list);
}


struct object *
evaluate_defun (struct object *list, struct environment *env, enum eval_outcome *outcome, struct object **cursor)
{
  struct object *fun;
  enum parse_lambda_list_outcome out;

  if (CAR (list)->type != TYPE_SYMBOL_NAME || !(CAR (CDR (list))->type & TYPE_LIST))
    {
      *outcome = INCORRECT_SYNTAX_IN_DEFUN;
      return NULL;
    }

  fun = alloc_function ();

  fun->value_ptr.function->lambda_list = parse_lambda_list (CAR (CDR (list)), &out);

  fun->value_ptr.function->body = CDR (CDR (list));

  env->funcs = add_binding (create_binding (SYMBOL (CAR (list)), fun, DYNAMIC_BINDING), env->funcs);

  return CAR (list);
}


int
eqmem (const char *s1, size_t n1, const char *s2, size_t n2)
{
  int i;
  
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
  int i;
  
  if (s1->used_size != s2->used_size)
    return 0;

  for (i = 0; i < s1->used_size; i++)
    if (s1->value [i] != s2->value [i])
      return 0;

  return 1;
}


void
print_symbol (const struct symbol *sym)
{  
  int i;
  char *nm = sym->name;
  char need_escape [] = "().,;'#\"\n\\";
  int do_need_escape = 0;

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
  int i;
  
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
print_list (const struct cons_pair *list)
{
  struct object *cdr;
  
  printf ("(");

  print_object (list->car);

  cdr = list->cdr;
  
  while (cdr && cdr->type != TYPE_NIL)
    {
      if (cdr->type == TYPE_CONS_PAIR)
	{
	  printf (" ");
	  print_object (cdr->value_ptr.cons_pair->car);
	  cdr = cdr->value_ptr.cons_pair->cdr;
	}
      else
	{
	  printf (" . ");
	  print_object (cdr);
	  break;
	}
    }
  
  printf (")");
}


void
print_object (const struct object *obj)
{
  if (obj->type == TYPE_NIL)
    printf ("()");
  else if (obj->type == TYPE_T)
    printf ("T");
  else if (obj->type == TYPE_QUOTE)
    {
      printf ("'");
      print_object (obj->value_ptr.next);
    }
  else if (obj->type == TYPE_BACKQUOTE)
    {
      printf ("`");
      print_object (obj->value_ptr.next);
    }
  else if (obj->type == TYPE_COMMA)
    {
      printf (",");
      print_object (obj->value_ptr.next);
    }
  else if (obj->type == TYPE_INTEGER)
    mpz_out_str (NULL, 10, obj->value_ptr.integer);
  else if (obj->type == TYPE_RATIO)
    mpq_out_str (NULL, 10, obj->value_ptr.ratio);
  else if (obj->type == TYPE_FLOAT)
    mpf_out_str (NULL, 10, 0, obj->value_ptr.floating);
  else if (obj->type == TYPE_STRING)
    print_string (obj->value_ptr.string);
  else if (obj->type == TYPE_SYMBOL_NAME)
    print_symbol (obj->value_ptr.symbol_name->sym->value_ptr.symbol);
  else if (obj->type == TYPE_SYMBOL)
    print_symbol (obj->value_ptr.symbol);
  else if (obj->type == TYPE_CONS_PAIR)
    print_list (obj->value_ptr.cons_pair);
  else if (obj->type == TYPE_FUNCTION)
    printf ("#<FUNCTION %p>", obj);
  else
    printf ("#<print not implemented>");
}


void
print_read_error (enum read_outcome err, const char *input, size_t size, const char *begin, const char *end)
{
  if (err == CLOSING_PARENTHESIS)
    {
      printf ("read error: mismatched closing parenthesis\n");
    }
  else if (err == CLOSING_PARENTHESIS_AFTER_PREFIX)
    {
      printf ("read error: closing parenthesis can't follows commas, ticks, backticks\n");
    }
  else if (err == SINGLE_DOT)
    {
      printf ("read error: single dot is only allowed inside a list and must be followed by exactly one object\n");
    }
  else if (err == MULTIPLE_DOTS)
    {
      printf ("read error: symbol names made of non-escaped dots only are not allowed\n");
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
}


void
print_eval_error (enum eval_outcome err, struct object *arg)
{
  if (err == UNBOUND_SYMBOL)
    {
      printf ("eval error: symbol ");
      print_symbol (arg->value_ptr.symbol);
      printf (" not bound to any object\n");
    }
  else if (err == UNKNOWN_FUNCTION)
    {
      printf ("eval error: symbol ");
      print_symbol (arg->value_ptr.symbol);
      printf (" not bound to any function\n");
    }
  else if (err == ILLEGAL_FUNCTION_CALL)
    {
      printf ("eval error: illegal function call\n");
    }
  else if (err == WRONG_NUMBER_OF_ARGUMENTS)
    {
      printf ("eval error: wrong number of arguments\n");
    }
  else if (err == CANT_EVALUATE_LISTS_YET)
    {
      printf ("eval error: can't evaluate lists yet!\n");
    }
  else if (err == INCORRECT_SYNTAX_IN_LET)
    {
      printf ("eval error: incorrect syntax in let\n");
    }
  else if (err == INCORRECT_SYNTAX_IN_PROGN)
    {
      printf ("eval error: incorrect syntax in progn\n");
    }
  else if (err == INCORRECT_SYNTAX_IN_DEFUN)
    {
      printf ("eval error: incorrect syntax in defun\n");
    }
  else if (err == CANT_REDEFINE_CONSTANT)
    {
      printf ("eval error: redefining constants is not allowed\n");
    }
  else if (err == EVAL_NOT_IMPLEMENTED)
    {
      printf ("eval error: not implemente\n");
    }
  else if (err == TOO_FEW_ARGUMENTS)
    {
      printf ("eval error: too few arguments to function call\n");
    }
  else if (err == TOO_MANY_ARGUMENTS)
    {
      printf ("eval error: too many arguments to function call\n");
    }
}


int
decrement_refcount (struct object *obj)
{
  if (!obj)
    return 0;

  if (obj == &nil_object
      || obj == &nil_symbol)
    return 0;
  
  obj->refcount--;

  if (obj->refcount <= 0)
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
  else if (obj->type == TYPE_SYMBOL && !obj->value_ptr.symbol->is_special)
    free_symbol (obj);
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
