/*  Copyright (C) 2022-2024 Andrea G. Monaco
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
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <errno.h>
#include <time.h>

#include <gmp.h>

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif


#ifndef HAVE_MEMMEM
#define memmem al_memmem
#endif



#ifdef FIXNUM_IS_INT

typedef int fixnum;
#define FIXNUM_MIN INT_MIN
#define FIXNUM_MAX INT_MAX

#else

typedef long fixnum;
#define FIXNUM_MIN LONG_MIN
#define FIXNUM_MAX LONG_MAX

#endif



#define CAR(list) ((list)->value_ptr.cons_pair->car)

#define CDR(list) ((list)->value_ptr.cons_pair->cdr)


#define IS_SEQUENCE(s) (SYMBOL (s) == &nil_object || (s)->type == TYPE_CONS_PAIR \
			|| (s)->type == TYPE_STRING			\
			|| ((s)->type == TYPE_ARRAY			\
			    && (s)->value_ptr.array->alloc_size		\
			    && !(s)->value_ptr.array->alloc_size->next)	\
			|| ((s)->type == TYPE_BITARRAY			\
			    && (s)->value_ptr.bitarray->alloc_size	\
			    && !(s)->value_ptr.bitarray->alloc_size->next))

#define IS_LIST(s) ((s)->type == TYPE_CONS_PAIR || SYMBOL (s) == &nil_object)

#define IS_VECTOR(s) ((s)->type == TYPE_STRING				\
		      || ((s)->type == TYPE_ARRAY			\
			  && (s)->value_ptr.array->alloc_size		\
			  && !(s)->value_ptr.array->alloc_size->next)	\
		      || ((s)->type == TYPE_BITARRAY			\
			  && (s)->value_ptr.bitarray->alloc_size	\
			  && !(s)->value_ptr.bitarray->alloc_size->next))

#define IS_ARRAY(s) ((s)->type == TYPE_STRING || (s)->type == TYPE_ARRAY \
		     || (s)->type == TYPE_BITARRAY)

#define HAS_FILL_POINTER(s) (((s)->type == TYPE_STRING			\
			      && (s)->value_ptr.string->fill_pointer >= 0) \
			     || ((s)->type == TYPE_ARRAY		\
				 && (s)->value_ptr.array->fill_pointer >= 0) \
			     || ((s)->type == TYPE_BITARRAY		\
				 && (s)->value_ptr.bitarray->fill_pointer >= 0))

#define IS_SYMBOL(s) ((s)->type == TYPE_SYMBOL || (s)->type == TYPE_SYMBOL_NAME)

#define IS_REAL(s) ((s)->type == TYPE_INTEGER || (s)->type == TYPE_FIXNUM \
		    || (s)->type == TYPE_RATIO || (s)->type == TYPE_FLOAT)

#define IS_RATIONAL(s) ((s)->type == TYPE_INTEGER || (s)->type == TYPE_RATIO)

#define IS_NUMBER(s) (IS_REAL (s) || (s)->type == TYPE_COMPLEX)

#define SYMBOL(s) ((s)->type == TYPE_SYMBOL ? (s) :			\
		   (s)->type == TYPE_SYMBOL_NAME ?			\
		   (s)->value_ptr.symbol_name->sym :			\
		   NULL)

#define IS_STRING_DESIGNATOR(s) ((s)->type == TYPE_CHARACTER || IS_SYMBOL (s) \
				 || (s)->type == TYPE_STRING)

#define IS_PACKAGE_DESIGNATOR(s) (IS_STRING_DESIGNATOR(s) \
				  || (s)->type == TYPE_PACKAGE)

/*#define HAS_LEAF_TYPE(obj) ((obj)->type & (TYPE_INTEGER | TYPE_FIXNUM	\
					   | TYPE_RATIO | TYPE_FLOAT \
					   | TYPE_BYTESPEC | TYPE_STRING \
					   | TYPE_CHARACTER | TYPE_BITARRAY)*/



#define CLEAR_READER_STATUS(out)		\
  do						\
    {						\
      (out).multiline_comment_depth = 0;	\
      (out).single_escape = 0;			\
      (out).multiple_escape = 0;		\
    } while (0)					\


#define CLEAR_MULTIPLE_OR_NO_VALUES(out)	\
  do						\
    {						\
      (out).no_value = 0;			\
      free_object_list ((out).other_values);	\
      (out).other_values = NULL;		\
    } while (0)



#define increment_refcount(obj) INC_STRONG_REFCOUNT(obj)

#define decrement_refcount(obj) delete_reference (NULL, obj, 0)



#define IS_LOWEST_BYTE_IN_UTF8(ch) (((ch) & 0xc0) != 0x80)



#define BUILTIN_SYMBOL(sym) \
  (intern_symbol_by_char_vector ((sym), strlen (sym), 0, EXTERNAL_VISIBILITY, \
				 0, env->cl_package))

#define CREATE_BUILTIN_SYMBOL(sym) \
  (intern_symbol_by_char_vector ((sym), strlen (sym), 1, EXTERNAL_VISIBILITY, \
				 1, env->cl_package))

#define KEYWORD(name) \
  (intern_symbol_by_char_vector ((name)+1, strlen (name)-1, 1, \
				 EXTERNAL_VISIBILITY, 1, env->keyword_package))


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
handler_binding
{
  struct object *condition;

  struct object *handler;

  struct handler_binding *next;
};


struct
handler_binding_frame
{
  struct handler_binding *frame;

  struct handler_binding_frame *next;
};


struct
restart_binding
{
  struct object *name;

  struct object *restart;

  struct restart_binding *next;
};


enum
binding_type
  {
    ANY_BINDING,
    LEXICAL_BINDING,
    DYNAMIC_BINDING,

    DELETED_BINDING = 4
  };


struct
binding
{
  enum binding_type type;

  int refcount;

  struct object *sym;
  struct object *obj;

  struct binding *closure_bin;

  struct binding *next;
};


enum
outcome_type
  {
    NO_OBJECT,

    SKIPPED_OBJECT,

    COMPLETE_OBJECT,

    CLOSING_PARENTHESIS,
    CLOSING_PARENTHESIS_AFTER_PREFIX,

    JUST_PREFIX,
    UNCLOSED_EMPTY_LIST,
    UNCLOSED_NONEMPTY_LIST,
    INCOMPLETE_STRING,
    INCOMPLETE_SYMBOL_NAME,
    INCOMPLETE_SHARP_MACRO_CALL,

    INVALID_SHARP_DISPATCH,
    UNKNOWN_SHARP_DISPATCH,
    WRONG_OBJECT_TYPE_TO_SHARP_MACRO,
    UNKNOWN_CHARACTER_NAME,
    FUNCTION_NOT_FOUND_IN_READ,
    WRONG_SYNTAX_IN_SHARP_MACRO_FOR_COMPLEX,
    INVALID_FEATURE_TEST,

    COMMA_WITHOUT_BACKQUOTE,
    TOO_MANY_COMMAS,

    SINGLE_DOT,

    MULTIPLE_DOTS,

    NO_OBJ_BEFORE_DOT_IN_LIST,
    NO_OBJ_AFTER_DOT_IN_LIST,
    MULTIPLE_OBJS_AFTER_DOT_IN_LIST,
    MORE_THAN_A_CONSING_DOT,

    TOO_MANY_COLONS,
    CANT_BEGIN_WITH_TWO_COLONS_OR_MORE,
    CANT_END_WITH_PACKAGE_SEPARATOR,
    MORE_THAN_A_PACKAGE_SEPARATOR,
    PACKAGE_NOT_FOUND_IN_READ,
    SYMBOL_IS_NOT_EXTERNAL_IN_PACKAGE,
    PACKAGE_MARKER_IN_SHARP_COLON,

    GOT_EOF_IN_MIDDLE_OF_OBJECT,
    GOT_EOF,


    EVAL_OK,
    UNBOUND_SYMBOL,
    INVALID_FUNCTION_CALL,
    UNKNOWN_KEYWORD_ARGUMENT,
    ODD_NUMBER_OF_ARGUMENTS,
    ODD_NUMBER_OF_KEYWORD_ARGUMENTS,
    DOTTED_LIST_NOT_ALLOWED_HERE,
    COMMA_AT_OR_DOT_NOT_ALLOWED_AT_TOP_LEVEL,
    CANT_SPLICE_AN_ATOM_HERE,
    CANT_SPLICE_AFTER_CONSING_DOT,
    SPLICING_OF_ATOM_NOT_ALLOWED_HERE,
    NOTHING_EXPANDED_BEFORE_CONSING_DOT,
    UNKNOWN_FUNCTION,
    MALFORMED_IF,
    INCORRECT_SYNTAX_IN_LET,
    INCORRECT_SYNTAX_IN_FLET,
    NOT_A_FUNCTION_NAME_OR_LAMBDA_IN_FUNCTION,
    INCORRECT_SYNTAX_IN_DEFUN,
    INCORRECT_SYNTAX_IN_DEFMACRO,
    INCORRECT_SYNTAX_IN_DEFTYPE,
    INVALID_LAMBDA_LIST,
    CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST,
    LAMBDA_LISTS_NOT_CONGRUENT,
    CANT_REDEFINE_SPECIAL_OPERATOR,
    CANT_REDEFINE_AS_GENERIC_FUNCTION,
    CANT_REDEFINE_CONSTANT_TO_NONEQL_VALUE,
    CANT_REDEFINE_CONSTANT_BY_DIFFERENT_OPERATOR,
    CANT_MODIFY_CONSTANT,
    CANT_REBIND_CONSTANT,
    CANT_REDEFINE_STANDARD_TYPE,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS,
    WRONG_NUMBER_OF_ARGUMENTS,
    MISMATCH_IN_DESTRUCTURING_CALL,
    WRONG_TYPE_OF_ARGUMENT,
    WRONG_NUMBER_OF_AXIS,
    OUT_OF_BOUND_INDEX,
    DECREASING_INTERVAL_NOT_MEANINGFUL,
    INVALID_SIZE,
    NO_APPLICABLE_METHOD,
    NO_NEXT_METHOD,
    COULD_NOT_OPEN_FILE,
    COULD_NOT_OPEN_FILE_FOR_READING,
    COULD_NOT_SEEK_FILE,
    COULD_NOT_TELL_FILE,
    ERROR_READING_FILE,
    INVALID_TYPE_SPECIFIER,
    UNKNOWN_TYPE,
    CLASS_NOT_FOUND,
    CLASS_DEFINITION_NOT_COMPLETE,
    INVALID_GO_TAG,
    TAG_NOT_FOUND,
    BLOCK_NOT_FOUND,
    CATCH_NOT_FOUND,
    INVALID_ACCESSOR,
    NO_SETF_EXPANDER,
    SETF_EXPANDER_PRODUCED_NOT_ENOUGH_VALUES,
    INVALID_SETF_EXPANSION,
    FUNCTION_NOT_FOUND_IN_EVAL,
    DECLARE_NOT_ALLOWED_HERE,
    WRONG_SYNTAX_IN_DECLARE,
    WRONG_SYNTAX_IN_DECLARATION,
    UNKNOWN_DECLARATION,
    CANT_DIVIDE_BY_ZERO,
    INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT,
    PACKAGE_NOT_FOUND_IN_EVAL,
    PACKAGE_NAME_OR_NICKNAME_ALREADY_IN_USE,
    SYMBOL_NOT_ACCESSIBLE_IN_PACKAGE,
    SYMBOL_NOT_PRESENT_IN_PACKAGE,
    IMPORTING_SYMBOL_WOULD_CAUSE_CONFLICT,
    EXPORTING_SYMBOL_WOULD_CAUSE_CONFLICT,
    KEYWORD_PACKAGE_CANT_USE_ANY_PACKAGE,
    CANT_USE_KEYWORD_PACKAGE,
    USING_PACKAGE_WOULD_CAUSE_CONFLICT_ON_SYMBOL,
    PACKAGE_IS_NOT_IN_USE,
    SLOT_NOT_FOUND,
    SLOT_NOT_BOUND,
    RESTART_NOT_FOUND
  };



#define INCOMPLETE_OBJECT(o,emptylist)					\
  (!(o) ? JUST_PREFIX :							\
   emptylist ? UNCLOSED_EMPTY_LIST :					\
   (o)->type == TYPE_CONS_PAIR ? UNCLOSED_NONEMPTY_LIST :		\
   (o)->type == TYPE_STRING ? INCOMPLETE_STRING :			\
   (o)->type == TYPE_SYMBOL_NAME ? INCOMPLETE_SYMBOL_NAME :		\
   (o)->type == TYPE_SHARP_MACRO_CALL ? INCOMPLETE_SHARP_MACRO_CALL : 0)


#define IS_INCOMPLETE_OBJECT(t) ((t) == JUST_PREFIX			\
				 || (t) == UNCLOSED_EMPTY_LIST		\
				 || (t) == UNCLOSED_NONEMPTY_LIST	\
				 || (t) == INCOMPLETE_STRING		\
				 || (t) == INCOMPLETE_SYMBOL_NAME	\
				 || (t) == INCOMPLETE_SHARP_MACRO_CALL)


#define IS_READ_OR_EVAL_ERROR(t) ((t) == CLOSING_PARENTHESIS		\
				  || (t) == CLOSING_PARENTHESIS_AFTER_PREFIX \
				  || (t) == INVALID_SHARP_DISPATCH	\
				  || (t) == UNKNOWN_SHARP_DISPATCH	\
				  || (t) == WRONG_OBJECT_TYPE_TO_SHARP_MACRO \
				  || (t) == UNKNOWN_CHARACTER_NAME	\
				  || (t) == FUNCTION_NOT_FOUND_IN_READ	\
				  || (t) == WRONG_SYNTAX_IN_SHARP_MACRO_FOR_COMPLEX \
				  || (t) == INVALID_FEATURE_TEST	\
				  || (t) == COMMA_WITHOUT_BACKQUOTE	\
				  || (t) == TOO_MANY_COMMAS		\
				  || (t) == SINGLE_DOT			\
				  || (t) == MULTIPLE_DOTS		\
				  || (t) == NO_OBJ_BEFORE_DOT_IN_LIST	\
				  || (t) == NO_OBJ_AFTER_DOT_IN_LIST	\
				  || (t) == MULTIPLE_OBJS_AFTER_DOT_IN_LIST \
				  || (t) == MORE_THAN_A_CONSING_DOT	\
				  || (t) == TOO_MANY_COLONS		\
				  || (t) == CANT_BEGIN_WITH_TWO_COLONS_OR_MORE \
				  || (t) == CANT_END_WITH_PACKAGE_SEPARATOR \
				  || (t) == MORE_THAN_A_PACKAGE_SEPARATOR \
				  || (t) == PACKAGE_NOT_FOUND_IN_READ	\
				  || (t) == SYMBOL_IS_NOT_EXTERNAL_IN_PACKAGE \
				  || (t) == PACKAGE_MARKER_IN_SHARP_COLON \
				  || (t) == GOT_EOF_IN_MIDDLE_OF_OBJECT	\
				  || (t) == GOT_EOF			\
				  || (t) > EVAL_OK)



struct
outcome
{
  enum outcome_type type;

  size_t multiline_comment_depth;
  int single_escape;
  int multiple_escape;

  int skipped_list_depth;

  int no_value;
  struct object_list *other_values;

  struct object *obj;
  struct object *pack;

  struct object *tag_to_jump_to;

  struct object *block_to_leave;

  struct object *object_to_catch;

  struct object *condition;

  struct object *return_value;
  int return_no_value;
  struct object_list *return_other_values;
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
  fixnum alloc_size;
  fixnum used_size;

  fixnum fill_pointer;
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

  struct go_tag_frame *go_tag_stack;

  struct object_list *blocks;

  struct object_list *catches;

  struct handler_binding_frame *handlers;

  struct restart_binding *restarts;

  struct binding *structs;

  struct object *c_stdout;

  struct object *method_args;
  struct method_list *next_methods;


  struct object *quote_sym, *function_sym, *lambda_sym, *setf_sym;

  struct object *declare_sym, *ignorable_sym, *ignore_sym, *inline_sym,
    *notinline_sym, *optimize_sym, *compilation_speed_sym, *debug_sym,
    *safety_sym, *space_sym, *speed_sym, *special_sym, *type_sym, *ftype_sym;

  struct object *amp_optional_sym, *amp_rest_sym, *amp_body_sym, *amp_key_sym,
    *amp_allow_other_keys_sym, *amp_aux_sym, *amp_whole_sym,
    *key_allow_other_keys_sym;

  struct object *not_sym, *and_sym, *or_sym, *eql_sym, *member_sym,
    *satisfies_sym, *star_sym;

  struct object *package_sym, *random_state_sym, *std_in_sym, *std_out_sym,
    *print_escape_sym, *print_readably_sym, *print_base_sym, *print_array_sym,
    *read_base_sym, *read_suppress_sym;
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
		       struct environment *env, struct outcome *outcome);
  struct object *typespec;
  struct object_list *parent_types;

  int is_const;
  int is_parameter;
  int is_special;

  int value_dyn_bins_num;
  struct object *value_cell;

  int function_dyn_bins_num;
  struct object *function_cell;

  struct object *plist;

  struct object *setf_expander;

  struct object *home_package;

  int setf_func_dyn_bins_num;
  struct object *setf_func_cell;
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
    AUXILIARY_VAR,
    WHOLE_PARAM
  };


struct
parameter
{
  enum parameter_type type;

  struct object *name;

  struct parameter *sub_lambda_list;
  int sub_allow_other_keys;

  struct object *key;
  struct object *typespec;

  struct object *init_form;
  struct object *supplied_p_param;

  int reference_strength_factor;

  int key_passed;

  struct parameter *next;
};


struct
function
{
  struct object *name;
  int is_setf_func;
  int is_special_operator;

  struct parameter *lambda_list;
  int allow_other_keys;
  /*int min_args;
    int max_args;*/

  struct binding *lex_vars;
  struct binding *lex_funcs;

  struct object *body;

  int is_generic;
  struct method_list *methods;

  struct object *(*builtin_form)
    (struct object *list, struct environment *env, struct outcome *outcome);


  struct object *struct_constructor_class;


  struct object *struct_accessor_class;
  struct object *struct_accessor_field;


  struct object *struct_predicate_class;


  struct object *struct_copyier_class;


  struct object *condition_reader_class;
  struct object *condition_reader_field;


  struct object *function_macro;


  struct object *macro_function;
};


struct
method
{
  struct object *generic_func;

  struct parameter *lambda_list;
  int allow_other_keys;

  struct object *body;
};


struct
method_list
{
  int reference_strength_factor;

  struct object *meth;

  struct method_list *next;
};


struct
cons_pair
{
  int filling_car;
  int empty_list_in_car;

  int found_dot;

  int filling_cdr;
  int empty_list_in_cdr;

  struct object *car;
  struct object *cdr;
};


struct
array_size
{
  fixnum size;

  struct array_size *next;
};


struct
array
{
  struct array_size *alloc_size;

  fixnum fill_pointer;

  struct object **value;

  int *reference_strength_factor;
};


struct
bitarray
{
  struct array_size *alloc_size;

  fixnum fill_pointer;

  mpz_t value;
};


#define LISP_HASHTABLE_SIZE 1024


enum
hashtable_type
  {
    HT_NONE, HT_EQ, HT_EQL, HT_EQUAL, HT_EQUALP
  };


struct
hashtable_record
{
  struct object *key;
  struct object *value;

  int reference_strength_factor;

  struct hashtable_record *next;
};


struct
hashtable
{
  enum hashtable_type type;

  struct hashtable_record **table;
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
structure_field_decl
{
  struct object *name;
  struct object *initform;
  struct object *type;
  int read_only;

  struct structure_field_decl *next;
};


struct
structure_class
{
  struct object *name;

  char *conc_name;
  size_t initial_offset;
  int named;

  struct structure_field_decl *fields;
};


struct
structure_field
{
  struct object *name;

  struct object *value;

  struct structure_field *next;
};


struct
structure
{
  struct object *class_name;

  struct structure_field *fields;
};


struct
class_field_decl
{
  struct object *name;

  struct class_field_decl *next;
};


struct
standard_class
{
  struct object *name;

  struct object_list *parents;

  struct class_field_decl *fields;
};


struct
class_field
{
  struct object *name;

  struct object *value;

  struct class_field *next;
};


struct
standard_object
{
  struct object *class_name;

  struct class_field *fields;
};


struct
condition_field_decl
{
  struct object *name;

  struct condition_field_decl *next;
};


struct
condition_class
{
  struct object *name;

  struct object_list *parents;

  struct condition_field_decl *fields;
};


struct
condition_field
{
  struct object *name;

  struct object *value;

  struct condition_field *next;
};


struct
condition
{
  struct object *class_name;

  struct condition_field *fields;
};


struct
sharp_macro_call
{
  int arg;
  int dispatch_ch;

  int feat_test_incomplete;
  int feat_test_is_empty_list;
  int feat_test_result;
  struct object *feature_test;

  enum outcome_type obj_type;
  int list_depth;

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
    TYPE_BACKQUOTE,
    TYPE_COMMA,
    TYPE_AT,
    TYPE_DOT,
    TYPE_SYMBOL_NAME,
    TYPE_SYMBOL,
    TYPE_INTEGER,
    TYPE_FIXNUM,
    TYPE_RATIO,
    TYPE_FLOAT,
    TYPE_COMPLEX,
    TYPE_RANDOM_STATE,
    TYPE_BYTESPEC,
    TYPE_CONS_PAIR,
    TYPE_STRING,
    TYPE_CHARACTER,
    TYPE_ARRAY,
    TYPE_BITARRAY,
    TYPE_HASHTABLE,
    TYPE_ENVIRONMENT,
    TYPE_PACKAGE,
    TYPE_FILENAME,
    TYPE_STREAM,
    TYPE_STRUCTURE_CLASS,
    TYPE_STRUCTURE,
    TYPE_STANDARD_CLASS,
    TYPE_STANDARD_OBJECT,
    TYPE_CONDITION_CLASS,
    TYPE_CONDITION,
    TYPE_FUNCTION,
    TYPE_MACRO,
    TYPE_METHOD,
    TYPE_SHARP_MACRO_CALL
  };


#define IS_PREFIX(t) ((t) == TYPE_BACKQUOTE || (t) == TYPE_COMMA \
		      || (t) == TYPE_AT || (t) == TYPE_DOT)


union
object_ptr_union
{
  struct symbol *symbol;
  struct symbol_name *symbol_name;
  struct object *next;
  mpz_t integer;
  fixnum *fixnum;
  mpq_t ratio;
  double *floating;
  struct complex *complex;
  gmp_randstate_t random_state;
  struct bytespec *bytespec;
  struct cons_pair *cons_pair;
  struct string *string;
  char *character;
  struct array *array;
  struct bitarray *bitarray;
  struct hashtable *hashtable;
  struct environment *environment;
  struct package *package;
  struct filename *filename;
  struct stream *stream;
  struct structure_class *structure_class;
  struct structure *structure;
  struct standard_class *standard_class;
  struct standard_object *standard_object;
  struct condition_class *condition_class;
  struct condition *condition;
  struct function *function;
  struct function *macro;
  struct method *method;
  struct sharp_macro_call *sharp_macro_call;
};


struct
object
{
  int refcount1;
  int refcount2;
  int mark;
  int flags;

  /*const char *begin;
    const char *end;*/
  enum object_type type;
  union object_ptr_union value_ptr;
};  


#define DONT_REFCOUNT(obj) (!(obj) || (obj) == &nil_object	\
			    || (obj) == &t_object || (obj)->type == TYPE_PACKAGE \
			    || IS_CONSTANT (obj))

#define STRENGTH_FACTOR_OF_OBJECT(x) ((x)->flags & 0x100)
#define FLIP_STRENGTH_FACTOR_OF_OBJECT(x) ((x)->flags ^= 0x100)

#define STRONG_REFCOUNT(x) (STRENGTH_FACTOR_OF_OBJECT(x) ? (x)->refcount1 \
			    : (x)->refcount2)
#define INC_STRONG_REFCOUNT(x) (STRENGTH_FACTOR_OF_OBJECT(x) ? (x)->refcount1++ \
				: (x)->refcount2++)
#define DEC_STRONG_REFCOUNT(x) (STRENGTH_FACTOR_OF_OBJECT(x) ? (x)->refcount1-- \
				: (x)->refcount2--)

#define WEAK_REFCOUNT(x) (STRENGTH_FACTOR_OF_OBJECT(x) ? (x)->refcount2 \
			  : (x)->refcount1)
#define INC_WEAK_REFCOUNT(x) (STRENGTH_FACTOR_OF_OBJECT(x) ? (x)->refcount2++ :\
			      (x)->refcount1++)
#define DEC_WEAK_REFCOUNT(x) (STRENGTH_FACTOR_OF_OBJECT(x) ? (x)->refcount2-- :\
			      (x)->refcount1--)

#define WITH_CHANGED_BIT(x,ind,new)		\
  (((x) & ~(1 << (ind))) | ((new) << (ind)))


#define WAS_A_READER_MACRO(obj) ((obj)->flags & 0x200)

#define SET_READER_MACRO_FLAG(obj) ((obj)->flags =	\
				    WITH_CHANGED_BIT ((obj)->flags, 9, 1))


#define IS_CONSTANT(obj) ((obj)->flags & 0x400)

#define SET_CONSTANT_FLAG(obj) ((obj)->flags =				\
				WITH_CHANGED_BIT ((obj)->flags, 10, 1))



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

enum outcome_type read_object_continued
(struct object **obj, int backts_commas_balance, int is_empty_list,
 const char *input, size_t size, FILE *stream, int preserve_whitespace,
 int ends_with_eof, struct environment *env, struct outcome *outcome,
 const char **obj_begin, const char **obj_end);
struct object *complete_object_interactively
(struct object *obj, int is_empty_list, struct environment *env,
 struct outcome *outcome, const char **input_left, size_t *input_left_size,
 char **wholeline);
struct object *read_object_interactively_continued
(const char *input, size_t input_size, struct environment *env,
 struct outcome *outcome, const char **input_left, size_t *input_left_size,
 char **wholeline);
struct object *read_object_interactively
(struct environment *env, struct outcome *outcome, const char **input_left,
 size_t *input_left_size, char **wholeline);

int next_char (unsigned char *ch, const char **input, size_t *size,
	       FILE *stream);
int next_nonspace_char (unsigned char *ch, const char **input, size_t *size,
			FILE *stream);
int unget_char (unsigned char ch, const char **input, size_t *size,
		FILE *stream);
int jump_to_end_of_line (const char **input, size_t *size, FILE *stream);
int jump_to_end_of_multiline_comment (const char **input, size_t *size,
				      FILE *stream, size_t *depth);

struct line_list *append_line_to_list
(char *line, size_t size, struct line_list *list, int do_copy);

fixnum object_list_length (struct object_list *list);
void prepend_object_to_obj_list (struct object *obj, struct object_list **list);
struct object_list *copy_list_to_obj_list (struct object *list);
struct object *pop_object_from_obj_list (struct object_list **list);
int is_object_in_obj_list (const struct object *obj,
			   const struct object_list *list);
void free_object_list (struct object_list *list);
void free_object_list_structure (struct object_list *list);

enum outcome_type read_object
(struct object **obj, int backts_commas_balance, const char *input, size_t size,
 FILE *stream, int preserve_whitespace, int ends_with_eof,
 struct environment *env, struct outcome *outcome, const char **obj_begin,
 const char **obj_end);

enum outcome_type read_list
(struct object **obj, int backts_commas_balance, const char *input, size_t size,
 FILE *stream, int preserve_whitespace, int ends_with_eof,
 struct environment *env, struct outcome *outcome, const char **list_end);

enum outcome_type read_string (struct object **obj, const char *input,
			       size_t size, FILE *stream,
			       const char **string_end);

enum outcome_type read_symbol_name
(struct object **obj, const char *input, size_t size, int got_eof,
 int preserve_whitespace, const char **symname_end,
 enum readtable_case read_case, struct outcome *out);

enum outcome_type read_prefix
(struct object **obj, const char *input, size_t size, FILE *stream,
 int *backts_commas_balance, struct object **last, const char **prefix_end);

enum outcome_type read_sharp_macro_call
(struct object **obj, const char *input, size_t size, FILE *stream,
 int preserve_whitespace, int ends_with_eof, struct environment *env,
 struct outcome *outcome, const char **macro_end);
struct object *call_sharp_macro
(struct sharp_macro_call *macro_call, struct environment *env,
 struct outcome *outcome);

enum outcome_type skip_without_reading
(enum outcome_type type, int backts_commas_balance, const char *input,
 size_t size, FILE *stream, int preserve_whitespace, int ends_with_eof,
 struct environment *env, struct outcome *outc, int *list_depth,
 const char **obj_begin, const char **obj_end);

int does_token_begin (const char *input, size_t size, FILE *stream);
char *accumulate_token (FILE *stream, int preserve_whitespace, int *token_size,
			int *token_length, struct outcome *out);

int get_read_base (struct environment *env);
int get_print_base (struct environment *env);
int is_number (const char *token, size_t size, int radix,
	       enum object_type *numtype, const char **number_end,
	       size_t *exp_marker_pos, const char **token_end);
struct object *alloc_number (enum object_type numtype);
struct object *create_number (const char *token, size_t size,
			      size_t exp_marker_pos, int radix,
			      enum object_type numtype);
struct object *alloc_complex (void);
struct object *create_complex (struct object *real, struct object *imag,
			       int steal_refs, struct environment *env,
			       struct outcome *outcome);
struct object *create_integer_from_long (long num);
struct object *create_integer_from_unsigned_long (unsigned long num);
struct object *convert_to_integer_if_possible (struct object *rat);
struct object *create_ratio_from_longs (long num, long den);
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
struct object *alloc_prefix (unsigned char pr);
struct object *alloc_empty_cons_pair (void);
struct object *alloc_empty_list (size_t sz);
struct object *alloc_function (void);
struct object *alloc_sharp_macro_call (void);
struct object *alloc_bytespec (void);
struct object_list *alloc_empty_object_list (size_t sz);

struct package_record **alloc_empty_symtable (size_t table_size);
struct object *create_package (char *name, int name_len);
struct object *create_package_from_c_strings (char *name, ...);
void free_name_list (struct name_list *list);
struct package_record *inspect_accessible_symbol (const struct object *sym,
						  const struct object *pack,
						  int *is_present);
struct package_record *inspect_accessible_symbol_by_name (char *name, size_t len,
							  struct object *pack,
							  int *is_present);
struct object *inspect_package_by_designator (struct object *des,
					      struct environment *env);
struct object *find_package (const char *name, size_t len,
			     struct environment *env);
struct package_record *find_package_entry (const struct object *symbol,
					   struct package_record **symtable,
					   struct package_record **prev);
int is_external_in_home_package (const struct object *sym);
int import_symbol (struct object *sym, struct object *pack,
		   struct package_record **rec);
int use_package (struct object *used, struct object *pack,
		 struct object **conflicting);
int unuse_package (struct object *used, struct object *pack);

int hash_object (const struct object *object, size_t table_size);
int hash_char_vector (const char *str, size_t sz, size_t table_size);
int hash_number (const struct object *num, size_t table_size);
int hash_symbol_name (const struct object *symname, size_t table_size);
int hash_symbol (const struct object *sym, size_t table_size);
struct hashtable_record *find_hashtable_record (struct object *obj,
						const struct object *tbl,
						int *ind, int *j,
						struct hashtable_record **prev);

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

void capture_lexical_environment (struct binding **lex_vars,
				  struct binding **lex_funcs,
				  struct binding *vars, int var_num,
				  struct binding *funcs, int func_num);

struct object *create_function (struct object *lambda_list, struct object *body,
				struct environment *env,
				struct outcome *outcome, int is_macro,
				int allow_destructuring);

const char *find_end_of_string
(const char *input, size_t size, size_t *new_size, size_t *string_length);
void normalize_string (char *output, const char *input, size_t size);

struct object *alloc_string (fixnum size);
struct object *create_string_copying_char_vector (const char *str, fixnum size);
struct object *create_string_with_char_vector (char *str, fixnum size);
struct object *create_string_copying_c_string (char *str);
void resize_string_allocation (struct object *string, fixnum size);
char *copy_string_to_c_string (struct string *str);

char *concatenate_char_vectors (size_t totsize, ...);

struct object *alloc_symbol_name (size_t value_s, size_t actual_symname_s);
void resize_symbol_name (struct object *symname, size_t value_s,
			 size_t actual_symname_s);

const char *find_end_of_symbol_name (const char *input, size_t size,
				     int ends_with_eof, int preserve_whitespace,
				     int already_begun, int found_package_sep,
				     const char **start_of_package_separator,
				     enum package_record_visibility *sym_visibility,
				     size_t *name_length,
				     size_t *act_name_length,
				     struct outcome *out);
void copy_symname_with_case_conversion (char *output, const char *input,
					size_t size,
					enum readtable_case read_case,
					int single_escape, int multiple_escape);

struct object *create_symbol (char *name, size_t size, int do_copy);
struct object *create_filename (struct object *string);

struct object *alloc_vector (fixnum size, int fill_with_nil,
			     int dont_store_size);
struct object *create_vector_from_list (struct object *list);
struct object *create_bitvector_from_char_vector (const char *in, size_t sz,
						  size_t req_size);
void resize_vector (struct object *vector, fixnum size);

struct object *create_character (char *character, int do_copy);
struct object *create_character_from_utf8 (char *character, size_t size);
struct object *create_character_from_char (char ch);
struct object *get_nth_character (struct object *str, int ind);
fixnum get_nth_character_offset (struct object *str, int ind);
int set_nth_character (struct object *str, int ind, char *ch);

struct object *create_file_stream (enum stream_type type,
				   enum stream_direction direction,
				   struct string *filename,
				   struct outcome *outcome);
struct object *create_stream_from_open_file (enum stream_type type,
					     enum stream_direction direction,
					     FILE *file);
struct object *create_string_stream (enum stream_direction direction,
				     struct object *instr);

struct structure_field_decl *create_structure_field_decl
(struct object *fieldform, struct environment *env, struct outcome *outcome);

struct class_field_decl *create_class_field_decl
(struct object *fieldform, struct environment *env, struct outcome *outcome);

struct condition_field_decl *create_condition_field_decl
(struct object *fieldform, struct environment *env, struct outcome *outcome);

void create_object_fields (struct object *stdobj, struct object *class);
void create_condition_fields (struct object *stdobj, struct object *class);

struct object *load_file (const char *filename, struct environment *env,
			  struct outcome *outcome);

struct object *intern_symbol_by_char_vector (char *name, size_t len,
					     int do_copy,
					     enum package_record_visibility vis,
					     int always_create_if_missing,
					     struct object *package);
struct object *intern_symbol_name (struct object *symname,
				   struct environment *env,
				   enum outcome_type *out);
int unintern_symbol (struct object *sym, struct object *pack);

struct binding *create_binding (struct object *sym, struct object *obj,
				enum binding_type type, int inc_refcs);
struct binding *add_binding (struct binding *bin, struct binding *env);
struct binding *chain_bindings (struct binding *bin, struct binding *env,
				int *num, struct binding **last_bin);
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

struct object *create_condition (struct object *type, va_list valist);
struct object *create_condition_by_class (struct object *type, ...);
struct object *create_condition_by_c_string (char *type,
					     struct environment *env, ...);
int does_condition_include_outcome_type (struct object *cond,
					 enum outcome_type type,
					 struct environment *env);
void add_condition_class (char *name, struct environment *env, int is_standard,
			  ...);
struct object *handle_condition (struct object *cond, struct environment *env,
				 struct outcome *outcome);

void add_builtin_type (char *name, struct environment *env,
		       int (*builtin_type)
		       (const struct object *obj, const struct object *typespec,
			struct environment *env, struct outcome *outcome),
		       int is_standard, ...);
struct object *add_builtin_form (char *name, struct environment *env,
				 struct object *(*builtin_form)
				 (struct object *list, struct environment *env,
				  struct outcome *outcome),
				 enum object_type type,
				  struct object *(*builtin_setf_func)
				 (struct object *list, struct environment *env,
				  struct outcome *outcome),
				 int is_special_operator);

struct object *define_constant
(struct object *sym, struct object *form, struct environment *env,
 struct outcome *outcome);
struct object *define_parameter
(struct object *sym, struct object *form, struct environment *env,
 struct outcome *outcome);

struct object *define_constant_by_name (char *name, struct object *value,
					struct environment *env);
struct object *define_variable (char *name, struct object *value,
				struct environment *env);

struct object *skip_prefix
(struct object *prefix, int *num_backticks_before_last_comma, int *num_commas,
 struct object **last_prefix);

struct object *elt (struct object *seq, unsigned int ind);
void set_elt (struct object *seq, unsigned int ind, struct object *val);
fixnum sequence_length (const struct object *seq);

struct object *nth (unsigned int ind, struct object *list);
struct object *nthcdr (unsigned int ind, struct object *list);
fixnum list_length (const struct object *list);
struct object *last_cons_pair (struct object *list);
int is_dotted_list (const struct object *list);
int is_circular_list (struct object *list);
int is_dotted_or_circular_list (struct object *list, int *is_circular);
int is_proper_list (struct object *list);

struct object *copy_prefix (const struct object *begin, const struct object *end,
			    struct object **last_prefix);
struct object *copy_list_structure (struct object *list,
				    const struct object *prefix, int num_conses,
				    struct object **last_cell);

fixnum array_rank (const struct array_size *sz);
fixnum array_total_size (const struct array_size *sz);

int hash_object_respecting_eq (const struct object *object, size_t table_size);
int hash_object_respecting_eql (const struct object *object, size_t table_size);
int hash_object_respecting_equal (const struct object *object, size_t table_size);
int hash_object_respecting_equalp (const struct object *object,
				   size_t table_size);
int hash_table_count (const struct hashtable *hasht);
void clear_hash_table (struct object *hasht);

struct parameter *alloc_parameter (enum parameter_type type,
				   struct object *sym);
struct parameter *parse_required_parameters (struct object *obj,
					     struct parameter **last,
					     struct object **rest,
					     int allow_destructuring,
					     int is_specialized,
					     struct environment *env,
					     struct outcome *outcome);
struct parameter *parse_optional_parameters
(struct object *obj, struct parameter **last, struct object **next,
 struct environment *env, struct outcome *outcome);
struct parameter *parse_keyword_parameters
(struct object *obj, struct parameter **last, struct object **next,
 struct environment *env, struct outcome *outcome);
struct parameter *parse_lambda_list
(struct object *obj, int allow_destructuring, int is_specialized,
 struct environment *env, struct outcome *outcome, int *allow_other_keys);

int are_lambda_lists_congruent (struct parameter *meth_list,
				struct parameter *gen_list);

int parse_declaration_specifier (struct object *spec, int is_local,
				 struct environment *env, int bin_num,
				 struct outcome *outcome);
int parse_declarations (struct object *body, struct environment *env,
			int bin_num, struct outcome *outcome,
			struct object **next);
void undo_special_declarations (struct object *decl, struct environment *env);

struct object *evaluate_body
(struct object *body, int is_tagbody, struct object *block_name,
 struct environment *env, struct outcome *outcome);
int parse_argument_list (struct object *arglist, struct parameter *par,
			 int eval_args, int also_pass_name, int is_typespec,
			 int allow_other_keys, struct environment *env,
			 struct outcome *outcome, struct binding **bins,
			 int *argsnum);
struct object *call_function (struct object *func, struct object *arglist,
			      int eval_args, int also_pass_name,
			      int create_new_lex_env, int expand_and_eval,
			      int is_typespec, struct environment *env,
			      struct outcome *outcome);
struct object *call_structure_constructor (struct object *class_name,
					   struct object *args,
					   struct environment *env,
					   struct outcome *outcome);
struct object *call_structure_accessor (struct object *class_name,
					struct object *field,
					struct object *args,
					struct object *newval,
					struct environment *env,
					struct outcome *outcome);
struct object *call_condition_reader (struct object *class_name,
				      struct object *field, struct object *args,
				      struct environment *env,
				      struct outcome *outcome);

int is_class_completely_defined (struct object *class);

int is_method_applicable (struct object *meth, struct object *args,
			  struct environment *env, struct outcome *outcome);
int compare_method_specificity (struct object *first, struct object *second);
struct object *dispatch_generic_function_call (struct object *func,
					       struct object *arglist,
					       struct environment *env,
					       struct outcome *outcome);

int check_type (struct object *obj, struct object *typespec,
		struct environment *env, struct outcome *outcome);
int check_type_by_char_vector (struct object *obj, char *type,
			       struct environment *env, struct outcome *outcome);
int type_starts_with (const struct object *typespec, const struct object *sym);
int is_subtype_by_char_vector (const struct object *first, char *second,
			       struct environment *env);
int is_descendant (const struct object *first, const struct object_list *parents,
		   const struct object *second, const struct object *prev);
int is_subtype (const struct object *first, const struct object *second,
		const struct object *prev);

int evaluate_feature_test (const struct object *feat_test,
			   struct environment *env, struct outcome *outcome);

struct object *evaluate_object
(struct object *obj, struct environment *env, struct outcome *outcome);
struct object *apply_backquote (struct object *form, int backts_commas_balance,
				struct environment *env, struct outcome *outcome,
				int forbid_splicing, int *do_splice,
				struct object **last_pref);
struct object *evaluate_list
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_through_list
(struct object *list, struct environment *env, struct outcome *outcome);

int type_t (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct outcome *outcome);
int type_nil (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct outcome *outcome);
int type_null (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome);
int type_cons (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome);
int type_atom (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome);
int type_list (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome);
int type_symbol (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome);
int type_keyword (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct outcome *outcome);
int type_boolean (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct outcome *outcome);
int type_function (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct outcome *outcome);
int type_package (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct outcome *outcome);
int type_number (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome);
int type_real (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome);
int type_rational (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct outcome *outcome);
int type_integer (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct outcome *outcome);
int type_bignum (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome);
int type_fixnum (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome);
int type_bit (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct outcome *outcome);
int type_ratio (const struct object *obj, const struct object *typespec,
		struct environment *env, struct outcome *outcome);
int type_float (const struct object *obj, const struct object *typespec,
		struct environment *env, struct outcome *outcome);
int type_short_float (const struct object *obj, const struct object *typespec,
		      struct environment *env, struct outcome *outcome);
int type_single_float (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct outcome *outcome);
int type_double_float (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct outcome *outcome);
int type_long_float (const struct object *obj, const struct object *typespec,
		     struct environment *env, struct outcome *outcome);
int type_complex (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct outcome *outcome);
int type_random_state (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct outcome *outcome);
int type_character (const struct object *obj, const struct object *typespec,
		    struct environment *env, struct outcome *outcome);
int type_standard_char (const struct object *obj, const struct object *typespec,
			struct environment *env, struct outcome *outcome);
int type_vector (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome);
int type_simple_vector (const struct object *obj, const struct object *typespec,
			struct environment *env, struct outcome *outcome);
int type_array (const struct object *obj, const struct object *typespec,
		struct environment *env, struct outcome *outcome);
int type_simple_array (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct outcome *outcome);
int type_sequence (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct outcome *outcome);
int type_string (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome);
int type_simple_string (const struct object *obj, const struct object *typespec,
			struct environment *env, struct outcome *outcome);
int type_bit_vector (const struct object *obj, const struct object *typespec,
		     struct environment *env, struct outcome *outcome);
int type_simple_bit_vector (const struct object *obj,
			    const struct object *typespec,
			    struct environment *env, struct outcome *outcome);
int type_hash_table (const struct object *obj, const struct object *typespec,
		     struct environment *env, struct outcome *outcome);
int type_pathname (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct outcome *outcome);
int type_stream (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome);
int type_file_stream (const struct object *obj, const struct object *typespec,
		      struct environment *env, struct outcome *outcome);
int type_string_stream (const struct object *obj, const struct object *typespec,
			struct environment *env, struct outcome *outcome);
int type_standard_object (const struct object *obj, const struct object *typespec,
			  struct environment *env, struct outcome *outcome);
int type_class (const struct object *obj, const struct object *typespec,
		struct environment *env, struct outcome *outcome);
int type_structure_class (const struct object *obj, const struct object *typespec,
			  struct environment *env, struct outcome *outcome);
int type_standard_class (const struct object *obj, const struct object *typespec,
			 struct environment *env, struct outcome *outcome);

int type_type_error (const struct object *obj, const struct object *typespec,
		     struct environment *env, struct outcome *outcome);
int type_file_error (const struct object *obj, const struct object *typespec,
		     struct environment *env, struct outcome *outcome);
int type_division_by_zero (const struct object *obj,
			   const struct object *typespec,
			   struct environment *env, struct outcome *outcome);
int type_arithmetic_error (const struct object *obj,
			   const struct object *typespec,
			   struct environment *env, struct outcome *outcome);
int type_error (const struct object *obj, const struct object *typespec,
		struct environment *env, struct outcome *outcome);
int type_serious_condition (const struct object *obj,
			    const struct object *typespec,
			    struct environment *env, struct outcome *outcome);
int type_condition (const struct object *obj, const struct object *typespec,
		    struct environment *env, struct outcome *outcome);
int type_restart (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct outcome *outcome);

struct object *builtin_car
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_cdr
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_rplaca
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_rplacd
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_cons
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_list
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_list_star
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_append
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_nconc
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_nth
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_nthcdr
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_nth_value
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_elt
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_aref
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_row_major_aref
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_copy_list
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_copy_seq
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_subseq
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_list_length
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_length
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_fill_pointer
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_make_array
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_vector
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_array_has_fill_pointer_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_array_dimensions
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_array_row_major_index
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_adjust_array
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_make_hash_table
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_hash_table_size
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_hash_table_count
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_hash_table_test
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_gethash
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_remhash
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_clrhash
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_maphash
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_last
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_read_line
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_read
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_read_preserving_whitespace
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_eval
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_write
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_write_string
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_write_char
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_write_byte
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_fresh_line
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_load
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_open
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_close
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_open_stream_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_input_stream_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_output_stream_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_interactive_stream_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_make_string_input_stream
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_make_string_output_stream
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_get_output_stream_string
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_upper_case_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_lower_case_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_both_case_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_eq
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_eql
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_not
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_concatenate
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_do
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_do_star
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_dotimes
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_dolist
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_mapcar
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_map
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_remove_if
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_reverse
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_setf_car (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_setf_cdr (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_setf_nth (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_setf_aref (struct object *list, struct environment *env,
				  struct outcome *outcome);
struct object *builtin_setf_elt (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_setf_fill_pointer (struct object *list,
					  struct environment *env,
					  struct outcome *outcome);
struct object *builtin_setf_gethash (struct object *list,
				     struct environment *env,
				     struct outcome *outcome);
struct object *builtin_setf_symbol_plist (struct object *list,
					  struct environment *env,
					  struct outcome *outcome);
struct object *builtin_setf_slot_value (struct object *list,
					struct environment *env,
					struct outcome *outcome);
struct object *builtin_setf_macro_function (struct object *list,
					    struct environment *env,
					    struct outcome *outcome);

int compare_two_numbers (struct object *num1, struct object *num2);
struct object *compare_any_numbers (struct object *list, struct environment *env,
				    struct outcome *outcome,
				    enum number_comparison comp);
int is_zero (const struct object *num);
int is_bit (const struct object *num);
struct object *negate_number (struct object *num, int in_place);
struct object *reciprocate_number (struct object *num);
double convert_number_to_double (struct object *num);
struct object *add_two_numbers (struct object *n1, struct object *n2);
struct object *subtract_two_numbers (struct object *n1, struct object *n2);
struct object *multiply_two_numbers (struct object *n1, struct object *n2);
struct object *divide_two_numbers (struct object *n1, struct object *n2,
				   struct environment *env,
				   struct outcome *outcome);

enum object_type highest_num_type (enum object_type t1, enum object_type t2);
struct object *copy_number (const struct object *num);
struct object *promote_number (struct object *num, enum object_type type);
struct object *perform_division_with_remainder
(struct object *args, enum rounding_behavior round_behavior,
 enum object_type quotient_type, struct outcome *outcome);

struct object *builtin_plus (struct object *list, struct environment *env,
			     struct outcome *outcome);
struct object *builtin_minus (struct object *list, struct environment *env,
			      struct outcome *outcome);
struct object *builtin_multiply (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_divide (struct object *list, struct environment *env,
			       struct outcome *outcome);
struct object *builtin_floor (struct object *list, struct environment *env,
			      struct outcome *outcome);
struct object *builtin_ffloor (struct object *list, struct environment *env,
			      struct outcome *outcome);
struct object *builtin_ceiling (struct object *list, struct environment *env,
				struct outcome *outcome);
struct object *builtin_fceiling (struct object *list, struct environment *env,
				struct outcome *outcome);
struct object *builtin_truncate (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_ftruncate (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_round (struct object *list, struct environment *env,
			      struct outcome *outcome);
struct object *builtin_fround (struct object *list, struct environment *env,
			      struct outcome *outcome);
struct object *builtin_numerator (struct object *list, struct environment *env,
				  struct outcome *outcome);
struct object *builtin_denominator (struct object *list, struct environment *env,
				    struct outcome *outcome);
struct object *builtin_sqrt (struct object *list, struct environment *env,
			      struct outcome *outcome);
struct object *builtin_complex (struct object *list, struct environment *env,
				struct outcome *outcome);
struct object *builtin_realpart (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_imagpart (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_numbers_equal (struct object *list,
				      struct environment *env,
				      struct outcome *outcome);
struct object *builtin_numbers_different (struct object *list,
					  struct environment *env,
					  struct outcome *outcome);
struct object *builtin_numbers_less_than (struct object *list,
					  struct environment *env,
					  struct outcome *outcome);
struct object *builtin_numbers_less_than_or_equal (struct object *list,
						   struct environment *env,
						   struct outcome *outcome);
struct object *builtin_numbers_more_than (struct object *list,
					  struct environment *env,
					  struct outcome *outcome);
struct object *builtin_numbers_more_than_or_equal (struct object *list,
						   struct environment *env,
						   struct outcome *outcome);
struct object *builtin_min (struct object *list, struct environment *env,
			    struct outcome *outcome);
struct object *builtin_max (struct object *list, struct environment *env,
			    struct outcome *outcome);
struct object *builtin_sin (struct object *list, struct environment *env,
			    struct outcome *outcome);
struct object *builtin_cos (struct object *list, struct environment *env,
			    struct outcome *outcome);
struct object *builtin_tan (struct object *list, struct environment *env,
			    struct outcome *outcome);
struct object *builtin_sinh (struct object *list, struct environment *env,
			     struct outcome *outcome);
struct object *builtin_cosh (struct object *list, struct environment *env,
			     struct outcome *outcome);
struct object *builtin_tanh (struct object *list, struct environment *env,
			     struct outcome *outcome);
struct object *builtin_exp (struct object *list, struct environment *env,
			    struct outcome *outcome);
struct object *builtin_expt (struct object *list, struct environment *env,
			     struct outcome *outcome);
struct object *builtin_log (struct object *list, struct environment *env,
			    struct outcome *outcome);
struct object *builtin_lognot (struct object *list, struct environment *env,
			       struct outcome *outcome);
struct object *builtin_logior (struct object *list, struct environment *env,
			       struct outcome *outcome);
struct object *builtin_make_random_state (struct object *list,
					  struct environment *env,
					  struct outcome *outcome);
struct object *builtin_random (struct object *list, struct environment *env,
			       struct outcome *outcome);

struct object *builtin_byte (struct object *list, struct environment *env,
			     struct outcome *outcome);
struct object *builtin_byte_size (struct object *list, struct environment *env,
				  struct outcome *outcome);
struct object *builtin_byte_position (struct object *list,
				      struct environment *env,
				      struct outcome *outcome);

struct object *builtin_typep (struct object *list, struct environment *env,
			      struct outcome *outcome);
struct object *builtin_type_of (struct object *list, struct environment *env,
				struct outcome *outcome);
struct object *builtin_subtypep (struct object *list, struct environment *env,
				 struct outcome *outcome);

struct object *builtin_make_string (struct object *list, struct environment *env,
				    struct outcome *outcome);

struct object *builtin_intern (struct object *list, struct environment *env,
			       struct outcome *outcome);
struct object *builtin_find_symbol (struct object *list, struct environment *env,
				    struct outcome *outcome);
struct object *builtin_unintern (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_make_symbol (struct object *list, struct environment *env,
				    struct outcome *outcome);
struct object *builtin_copy_symbol (struct object *list, struct environment *env,
				    struct outcome *outcome);
struct object *builtin_boundp (struct object *list, struct environment *env,
			       struct outcome *outcome);
struct object *builtin_symbol_value (struct object *list,
				     struct environment *env,
				     struct outcome *outcome);
struct object *builtin_set (struct object *list, struct environment *env,
			    struct outcome *outcome);
struct object *builtin_fboundp (struct object *list, struct environment *env,
				struct outcome *outcome);
struct object *builtin_symbol_function (struct object *list,
					struct environment *env,
					struct outcome *outcome);
struct object *builtin_symbol_name (struct object *list, struct environment *env,
				    struct outcome *outcome);
struct object *builtin_symbol_package (struct object *list,
				       struct environment *env,
				       struct outcome *outcome);
struct object *builtin_symbol_plist (struct object *list,
				     struct environment *env,
				     struct outcome *outcome);
struct object *builtin_special_operator_p (struct object *list,
					   struct environment *env,
					   struct outcome *outcome);
struct object *builtin_makunbound (struct object *list, struct environment *env,
				   struct outcome *outcome);
struct object *builtin_fmakunbound (struct object *list, struct environment *env,
				    struct outcome *outcome);
struct object *builtin_macroexpand_1 (struct object *list,
				      struct environment *env,
				      struct outcome *outcome);
struct object *builtin_macro_function (struct object *list,
				       struct environment *env,
				       struct outcome *outcome);
struct object *builtin_string (struct object *list, struct environment *env,
			       struct outcome *outcome);
/*struct object *builtin_string_eq (struct object *list, struct environment *env,
  struct outcome *outcome);*/
struct object *builtin_char_eq (struct object *list, struct environment *env,
				struct outcome *outcome);
struct object *builtin_char_upcase (struct object *list, struct environment *env,
				    struct outcome *outcome);
struct object *builtin_char_downcase (struct object *list,
				      struct environment *env,
				      struct outcome *outcome);
struct object *builtin_alpha_char_p (struct object *list,
				     struct environment *env,
				     struct outcome *outcome);
struct object *builtin_alphanumericp (struct object *list,
				      struct environment *env,
				      struct outcome *outcome);
struct object *builtin_char_code (struct object *list,
				  struct environment *env,
				  struct outcome *outcome);
struct object *builtin_code_char (struct object *list,
				  struct environment *env,
				  struct outcome *outcome);
struct object *builtin_find_package (struct object *list,
				     struct environment *env,
				     struct outcome *outcome);
struct object *builtin_package_name (struct object *list,
				     struct environment *env,
				     struct outcome *outcome);
struct object *builtin_package_nicknames (struct object *list,
					  struct environment *env,
					  struct outcome *outcome);
struct object *builtin_rename_package (struct object *list,
				       struct environment *env,
				       struct outcome *outcome);
struct object *builtin_package_use_list (struct object *list,
					 struct environment *env,
					 struct outcome *outcome);
struct object *builtin_package_used_by_list (struct object *list,
					     struct environment *env,
					     struct outcome *outcome);
struct object *builtin_list_all_packages (struct object *list,
					  struct environment *env,
					  struct outcome *outcome);
struct object *builtin_make_package (struct object *list,
				     struct environment *env,
				     struct outcome *outcome);
struct object *builtin_in_package (struct object *list, struct environment *env,
				   struct outcome *outcome);
struct object *builtin_import (struct object *list, struct environment *env,
			       struct outcome *outcome);
struct object *builtin_export (struct object *list, struct environment *env,
			       struct outcome *outcome);
struct object *builtin_unexport (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_use_package (struct object *list, struct environment *env,
				    struct outcome *outcome);
struct object *builtin_unuse_package (struct object *list,
				      struct environment *env,
				      struct outcome *outcome);
struct object *builtin_do_symbols (struct object *list, struct environment *env,
				   struct outcome *outcome);
struct object *builtin_do_external_symbols (struct object *list,
					    struct environment *env,
					    struct outcome *outcome);

struct object *builtin_get_internal_run_time (struct object *list,
					      struct environment *env,
					      struct outcome *outcome);
struct object *builtin_get_decoded_time (struct object *list,
					 struct environment *env,
					 struct outcome *outcome);
struct object *builtin_lisp_implementation_type (struct object *list,
						 struct environment *env,
						 struct outcome *outcome);
struct object *builtin_lisp_implementation_version (struct object *list,
						    struct environment *env,
						    struct outcome *outcome);
struct object *builtin_software_type (struct object *list,
				      struct environment *env,
				      struct outcome *outcome);
struct object *builtin_software_version (struct object *list,
					 struct environment *env,
					 struct outcome *outcome);

struct binding *create_binding_from_let_form
(struct object *form, struct environment *env, struct outcome *outcome,
 int allow_three_elements);
struct object *evaluate_let
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_let_star
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_progv
(struct object *list, struct environment *env, struct outcome *outcome);

struct binding *create_binding_from_flet_form
(struct object *form, struct environment *env, struct outcome *outcome,
 enum object_type type);
struct object *evaluate_flet
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_labels
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_macrolet
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *get_dynamic_value (struct object *sym, struct environment *env);
struct object *get_function (struct object *sym, struct environment *env,
			     int only_functions, int setf_func, int only_globals,
			     int increment_refc);

struct object *inspect_variable_by_c_string (char *var,
					     struct environment *env);
struct object *inspect_variable (struct object *sym, struct environment *env);

struct object *set_value (struct object *sym, struct object *value,
			  int eval_value, struct environment *env,
			  struct outcome *outcome);

struct object *evaluate_quote
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_if
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_progn
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_values
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_values_list
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_multiple_value_list
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_multiple_value_call
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_eval_when
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defconstant
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defparameter
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defvar
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defun
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defmacro
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_setq
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_psetq
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_setf
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_function
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_lambda
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_apply
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_funcall
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_declare
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_the
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_prog1
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_prog2
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_destructuring_bind
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_deftype
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_define_setf_expander
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_get_setf_expansion
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defstruct
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defclass
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_find_class
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_make_instance
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_class_of
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_class_name
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_slot_exists_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_slot_boundp
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_slot_value
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_slot_makunbound
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defgeneric
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defmethod
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_next_method_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_call_next_method
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_no_next_method
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *evaluate_declaim
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_proclaim
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *evaluate_tagbody
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_go
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *evaluate_block
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_return_from
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *evaluate_catch
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_throw
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *evaluate_handler_bind
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_restart_bind
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_invoke_restart
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_unwind_protect
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_signal
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_error
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_define_condition
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_make_condition
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_print_no_warranty
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_print_terms_and_conditions
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_exit
(struct object *list, struct environment *env, struct outcome *outcome);

int eqmem (const char *s1, size_t n1, const char *s2, size_t n2);
int symname_equals (const struct symbol_name *sym, const char *s);
int symname_is_among (const struct symbol_name *sym, ...);
int symbol_equals (const struct object *sym, const char *str,
		   struct environment *env);
int symbol_is_among (const struct object *sym, struct environment *env, ...);
int equal_strings (const struct string *s1, const struct string *s2);
int equalp_strings (const struct string *s1, const struct string *s2);
struct object *eq_objects (const struct object *obj1, const struct object *obj2);
struct object *eql_objects (struct object *obj1, struct object *obj2);
struct object *equal_objects (struct object *obj1, struct object *obj2);
struct object *equalp_objects (struct object *obj1, struct object *obj2);
int arrays_have_equal_size (struct array *a1, struct array *a2);

struct object *fresh_line (struct stream *str);

int is_printer_escaping_enabled (struct environment *env);

int write_to_stream (struct stream *stream, const char *str, size_t size);
int write_char_to_stream (struct stream *stream, char ch);
int write_long_to_stream (struct stream *stream, long z);

int print_as_symbol (const char *sym, size_t len, int print_escapes,
		     struct stream *str);
int print_symbol_name (const struct symbol_name *sym, struct environment *env,
		       struct stream *str);
int print_symbol (const struct object *sym, struct environment *env,
		  struct stream *str);
int print_bignum (const mpz_t z, struct environment *env, struct stream *str);
int print_floating (const double f, struct environment *env, struct stream *str);
int print_complex (const struct complex *c, struct environment *env,
		   struct stream *str);
int print_bytespec (const struct bytespec *bs, struct environment *env,
		    struct stream *str);
int print_as_string (const char *value, size_t sz, struct environment *env,
		     struct stream *str);
int print_character (const char *character, struct environment *env,
		     struct stream *str);
int print_filename (const struct filename *fn, struct environment *env,
		    struct stream *str);
int print_list (const struct cons_pair *list, struct environment *env,
		struct stream *str);
int print_array (const struct array *array, struct environment *env,
		 struct stream *str);
int print_bitarray (const struct bitarray *array, struct environment *env,
		    struct stream *str);
int print_function_or_macro (const struct object *obj, struct environment *env,
			     struct stream *str);
int print_method (const struct object *obj, struct environment *env,
		  struct stream *str);
int print_object (const struct object *obj, struct environment *env,
		  struct stream *str);

void print_error (struct outcome *err, struct environment *env);

void mark_as_constant (struct object *obj);

struct parameter *parameter_by_index (struct parameter *par, int *ind);
int is_reference_weak (struct object *src, int ind, struct object *dest);
void set_reference_strength_factor (struct object *src, int ind,
				    struct object *dest, int new_weakness,
				    int increase_refcount,
				    int decrease_other_refcount);
void add_strong_reference (struct object *src, struct object *dest, int ind);
void add_reference (struct object *src, struct object *dest, int ind);
void delete_reference (struct object *src, struct object *dest, int ind);
void weakify_loops (struct object *root, int *depth);
void restore_invariants_at_edge (struct object *src, struct object *dest,
				 int ind, struct object *root, int *depth);
void restore_invariants_at_lambda_list (struct parameter *par,
					struct object *node,
					size_t *i, struct object *root,
					int *depth);
void restore_invariants_at_node (struct object *node, struct object *root,
				 int *depth);

void free_object (struct object *obj);
void free_string (struct object *obj);
void free_symbol_name (struct object *obj);
void free_symbol (struct object *obj);
void free_cons_pair (struct object *obj);
void free_array_size (struct array_size *size);
void free_array (struct object *obj);
void free_bitarray (struct object *obj);
void free_hashtable (struct object *obj);
void free_integer (struct object *obj);
void free_ratio (struct object *obj);
void free_float (struct object *obj);
void free_bytespec (struct object *obj);
void free_structure_class (struct object *obj);
void free_structure (struct object *obj);
void free_standard_class (struct object *obj);
void free_standard_object (struct object *obj);
void free_condition_class (struct object *obj);
void free_condition (struct object *obj);
void free_lambda_list_content (struct object *obj, struct parameter *par, int *i);
void free_lambda_list_structure (struct parameter *par);
void free_function_or_macro (struct object *obj);
void free_method (struct object *obj);

void free_sharp_macro_call (struct object *macro);
void free_list_structure (struct object *list);

void print_welcome_message (void);
void print_version (void);
void print_help (void);



struct symbol nil_symbol = {"NIL", 3, 1, 1, type_nil, NULL, NULL, 1};

struct object nil_object = {0, 0, 0, 0, TYPE_SYMBOL, {&nil_symbol}};


struct symbol t_symbol = {"T", 1, 1, 1, type_t, NULL, NULL, 1};

struct object t_object = {0, 0, 0, 0, TYPE_SYMBOL, {&t_symbol}};


mpz_t integer_one;



int
main (int argc, char *argv [])
{
  int end_repl = 0, i;

#ifdef HAVE_LIBREADLINE
  int c;
#endif

  struct object *result, *obj, *c_stdout, *al_argv;
  struct object_list *vals;
  struct environment env = {NULL};

  struct outcome eval_out = {EVAL_OK};

  const char *input_left = NULL;
  size_t input_left_s = 0;
  char *wholel = NULL;

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
	print_error (&eval_out, &env);

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
	print_error (&eval_out, &env);

      eval_out.type = EVAL_OK;

      if (!opts.load_and_exit)
	printf ("\n");
    }

  if (opts.load_and_exit)
    {
      result = load_file (opts.load_and_exit, &env, &eval_out);

      exit (0);
    }


  define_variable ("AL-ARGC", create_integer_from_long (argc), &env);

  al_argv = alloc_vector (argc, 0, 0);

  for (i = 0; i < argc; i++)
    {
      al_argv->value_ptr.array->value [i] =
	create_string_copying_c_string (argv [i]);
    }

  define_variable ("AL-ARGV", al_argv, &env);


#ifdef HAVE_LIBREADLINE
  c = read_history ("al_history");

  if (c && c != ENOENT)
    printf ("could not read line history from al_history: %s\n", strerror (c));
#endif


  while (!end_repl)
    {
      free (wholel);
      obj = read_object_interactively (&env, &eval_out, &input_left,
				       &input_left_s, &wholel);

      while (obj)
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
	    {
	      if (eval_out.condition)
		{
		  if (is_subtype_by_char_vector
		      (eval_out.condition->value_ptr.condition->class_name,
		       "SIMPLE-CONDITION", &env)
		      || is_subtype_by_char_vector
		      (eval_out.condition->value_ptr.condition->class_name,
		       "TYPE-ERROR", &env))
		    {
		      print_object (eval_out.condition->value_ptr.condition->fields->
				    value, &env, c_stdout->value_ptr.stream);
		      fresh_line (c_stdout->value_ptr.stream);
		    }

		  decrement_refcount (eval_out.condition);
		  eval_out.condition = NULL;
		}
	      else
		print_error (&eval_out, &env);
	    }
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

	  if (input_left && input_left_s > 0)
	    obj = read_object_interactively_continued (input_left, input_left_s,
						       &env, &eval_out,
						       &input_left,
						       &input_left_s, &wholel);
	  else
	    obj = NULL;
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
  struct object *cluser_package, *rs, *stdobjsym, *stdobjcl;
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

  mpz_init (integer_one);
  mpz_set_si (integer_one, 1);


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


  env->package_sym = intern_symbol_by_char_vector ("*PACKAGE*",
						   strlen ("*PACKAGE*"), 1,
						   EXTERNAL_VISIBILITY, 1,
						   env->cl_package);
  env->package_sym->value_ptr.symbol->is_parameter = 1;
  env->package_sym->value_ptr.symbol->value_cell = env->cl_package;


  env->random_state_sym =
    intern_symbol_by_char_vector ("*RANDOM-STATE*", strlen ("*RANDOM-STATE*"), 1,
				  EXTERNAL_VISIBILITY, 1, env->cl_package);
  env->random_state_sym->value_ptr.symbol->is_parameter = 1;
  rs = alloc_object ();
  rs->type = TYPE_RANDOM_STATE;
  gmp_randinit_default (rs->value_ptr.random_state);
  gmp_randseed_ui (rs->value_ptr.random_state, time (NULL));
  env->random_state_sym->value_ptr.symbol->value_cell = rs;


  add_builtin_form ("CAR", env, builtin_car, TYPE_FUNCTION, builtin_setf_car, 0);
  add_builtin_form ("CDR", env, builtin_cdr, TYPE_FUNCTION, builtin_setf_cdr, 0);
  add_builtin_form ("RPLACA", env, builtin_rplaca, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("RPLACD", env, builtin_rplacd, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CONS", env, builtin_cons, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LIST", env, builtin_list, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LIST*", env, builtin_list_star, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("APPEND", env, builtin_append, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NCONC", env, builtin_nconc, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NTH", env, builtin_nth, TYPE_FUNCTION, builtin_setf_nth, 0);
  add_builtin_form ("NTHCDR", env, builtin_nthcdr, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NTH-VALUE", env, builtin_nth_value, TYPE_MACRO, NULL, 0);
  add_builtin_form ("ELT", env, builtin_elt, TYPE_FUNCTION, builtin_setf_elt, 0);
  add_builtin_form ("AREF", env, builtin_aref, TYPE_FUNCTION, builtin_setf_aref,
		    0);
  add_builtin_form ("ROW-MAJOR-AREF", env, builtin_row_major_aref, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("COPY-LIST", env, builtin_copy_list, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("COPY-SEQ", env, builtin_copy_seq, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SUBSEQ", env, builtin_subseq, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LIST-LENGTH", env, builtin_list_length, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("LENGTH", env, builtin_length, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FILL-POINTER", env, builtin_fill_pointer, TYPE_FUNCTION,
		    builtin_setf_fill_pointer, 0);
  add_builtin_form ("MAKE-ARRAY", env, builtin_make_array, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("VECTOR", env, builtin_vector, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ARRAY-HAS-FILL-POINTER-P", env,
		    builtin_array_has_fill_pointer_p, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ARRAY-DIMENSIONS", env, builtin_array_dimensions,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ARRAY-ROW-MAJOR-INDEX", env, builtin_array_row_major_index,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ADJUST-ARRAY", env, builtin_adjust_array, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("MAKE-HASH-TABLE", env, builtin_make_hash_table,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("HASH-TABLE-SIZE", env, builtin_hash_table_size,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("HASH-TABLE-COUNT", env, builtin_hash_table_count,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("HASH-TABLE-TEST", env, builtin_hash_table_test,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("GETHASH", env, builtin_gethash, TYPE_FUNCTION,
		    builtin_setf_gethash, 0);
  add_builtin_form ("REMHASH", env, builtin_remhash, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CLRHASH", env, builtin_clrhash, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAPHASH", env, builtin_maphash, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LAST", env, builtin_last, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("READ-LINE", env, builtin_read_line, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("READ", env, builtin_read, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("READ-PRESERVING-WHITESPACE", env,
		    builtin_read_preserving_whitespace, TYPE_FUNCTION, NULL, 0);
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
  add_builtin_form ("DO", env, builtin_do, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DO*", env, builtin_do_star, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DOTIMES", env, builtin_dotimes, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DOLIST", env, builtin_dolist, TYPE_MACRO, NULL, 0);
  add_builtin_form ("MAPCAR", env, builtin_mapcar, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAP", env, builtin_map, TYPE_FUNCTION, NULL, 0);
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
  add_builtin_form ("SIN", env, builtin_sin, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("COS", env, builtin_cos, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("TAN", env, builtin_tan, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SINH", env, builtin_sinh, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("COSH", env, builtin_cosh, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("TANH", env, builtin_tanh, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("EXP", env, builtin_exp, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("EXPT", env, builtin_expt, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LOG", env, builtin_log, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LOGNOT", env, builtin_lognot, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LOGIOR", env, builtin_logior, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("QUOTE", env, evaluate_quote, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LET", env, evaluate_let, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LET*", env, evaluate_let_star, TYPE_MACRO, NULL, 1);
  add_builtin_form ("PROGV", env, evaluate_progv, TYPE_MACRO, NULL, 1);
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
  add_builtin_form ("MULTIPLE-VALUE-CALL", env, evaluate_multiple_value_call,
		    TYPE_MACRO, NULL, 1);
  add_builtin_form ("EVAL-WHEN", env, evaluate_eval_when, TYPE_MACRO, NULL,
		    1);
  add_builtin_form ("DEFCONSTANT", env, evaluate_defconstant, TYPE_MACRO, NULL,
		    0);
  add_builtin_form ("DEFPARAMETER", env, evaluate_defparameter, TYPE_MACRO, NULL,
		    0);
  add_builtin_form ("DEFVAR", env, evaluate_defvar, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFUN", env, evaluate_defun, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFMACRO", env, evaluate_defmacro, TYPE_MACRO, NULL, 0);
  add_builtin_form ("SETQ", env, evaluate_setq, TYPE_MACRO, NULL, 1);
  add_builtin_form ("PSETQ", env, evaluate_psetq, TYPE_MACRO, NULL, 0);
  add_builtin_form ("SETF", env, evaluate_setf, TYPE_MACRO, NULL, 0);
  add_builtin_form ("FUNCTION", env, evaluate_function, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LAMBDA", env, evaluate_lambda, TYPE_MACRO, NULL, 0);
  add_builtin_form ("APPLY", env, evaluate_apply, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FUNCALL", env, evaluate_funcall, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DECLARE", env, evaluate_declare, TYPE_MACRO, NULL, 0);
  add_builtin_form ("THE", env, evaluate_the, TYPE_MACRO, NULL, 1);
  add_builtin_form ("PROG1", env, evaluate_prog1, TYPE_MACRO, NULL, 0);
  add_builtin_form ("PROG2", env, evaluate_prog2, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DESTRUCTURING-BIND", env, evaluate_destructuring_bind,
		    TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFTYPE", env, evaluate_deftype, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFINE-SETF-EXPANDER", env, evaluate_define_setf_expander,
		    TYPE_MACRO, NULL, 0);
  add_builtin_form ("GET-SETF-EXPANSION", env, builtin_get_setf_expansion,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DEFSTRUCT", env, evaluate_defstruct, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFCLASS", env, evaluate_defclass, TYPE_MACRO, NULL, 0);
  add_builtin_form ("FIND-CLASS", env, builtin_find_class, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("MAKE-INSTANCE", env, builtin_make_instance, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("CLASS-OF", env, builtin_class_of, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("CLASS-NAME", env, builtin_class_name, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("SLOT-EXISTS-P", env, builtin_slot_exists_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("SLOT-BOUNDP", env, builtin_slot_boundp, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("SLOT-VALUE", env, builtin_slot_value, TYPE_FUNCTION,
		    builtin_setf_slot_value, 0);
  add_builtin_form ("SLOT-MAKUNBOUND", env, builtin_slot_makunbound,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DEFGENERIC", env, evaluate_defgeneric, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFMETHOD", env, evaluate_defmethod, TYPE_MACRO, NULL, 0);
  add_builtin_form ("NEXT-METHOD-P", env, builtin_next_method_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("CALL-NEXT-METHOD", env, evaluate_call_next_method,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NO-NEXT-METHOD", env, evaluate_no_next_method,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DECLAIM", env, evaluate_declaim, TYPE_MACRO, NULL, 0);
  add_builtin_form ("PROCLAIM", env, builtin_proclaim, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("TAGBODY", env, evaluate_tagbody, TYPE_MACRO, NULL, 1);
  add_builtin_form ("GO", env, evaluate_go, TYPE_MACRO, NULL, 1);
  add_builtin_form ("BLOCK", env, evaluate_block, TYPE_MACRO, NULL, 1);
  add_builtin_form ("RETURN-FROM", env, evaluate_return_from, TYPE_MACRO, NULL,
		    1);
  add_builtin_form ("CATCH", env, evaluate_catch, TYPE_MACRO, NULL, 1);
  add_builtin_form ("THROW", env, evaluate_throw, TYPE_MACRO, NULL, 1);
  add_builtin_form ("HANDLER-BIND", env, evaluate_handler_bind, TYPE_MACRO, NULL,
		    0);
  add_builtin_form ("RESTART-BIND", env, evaluate_restart_bind, TYPE_MACRO, NULL,
		    0);
  add_builtin_form ("INVOKE-RESTART", env, builtin_invoke_restart, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("UNWIND-PROTECT", env, evaluate_unwind_protect, TYPE_MACRO,
		    NULL, 1);
  add_builtin_form ("SIGNAL", env, builtin_signal, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ERROR", env, builtin_error, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DEFINE-CONDITION", env, evaluate_define_condition,
		    TYPE_MACRO, NULL, 0);
  add_builtin_form ("MAKE-CONDITION", env, builtin_make_condition, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("TYPEP", env, builtin_typep, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("TYPE-OF", env, builtin_type_of, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SUBTYPEP", env, builtin_subtypep, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAKE-RANDOM-STATE", env, builtin_make_random_state,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("RANDOM", env, builtin_random, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("BYTE", env, builtin_byte, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("BYTE-SIZE", env, builtin_byte_size, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("BYTE-POSITION", env, builtin_byte_position, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("MAKE-STRING", env, builtin_make_string, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("INTERN", env, builtin_intern, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FIND-SYMBOL", env, builtin_find_symbol, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("UNINTERN", env, builtin_unintern, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAKE-SYMBOL", env, builtin_make_symbol, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("COPY-SYMBOL", env, builtin_copy_symbol, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("BOUNDP", env, builtin_boundp, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SYMBOL-VALUE", env, builtin_symbol_value, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("SET", env, builtin_set, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FBOUNDP", env, builtin_fboundp, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SYMBOL-FUNCTION", env, builtin_symbol_function,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SYMBOL-NAME", env, builtin_symbol_name, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("SYMBOL-PACKAGE", env, builtin_symbol_package, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("SYMBOL-PLIST", env, builtin_symbol_plist, TYPE_FUNCTION,
		    builtin_setf_symbol_plist, 0);
  add_builtin_form ("SPECIAL-OPERATOR-P", env, builtin_special_operator_p,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAKUNBOUND", env, builtin_makunbound, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("FMAKUNBOUND", env, builtin_fmakunbound, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("MACROEXPAND-1", env, builtin_macroexpand_1, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("MACRO-FUNCTION", env, builtin_macro_function, TYPE_FUNCTION,
		    builtin_setf_macro_function, 0);
  add_builtin_form ("STRING", env, builtin_string, TYPE_FUNCTION, NULL, 0);
  /*add_builtin_form ("STRING=", env, builtin_string_eq, TYPE_FUNCTION, NULL, 0);*/
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
  add_builtin_form ("CODE-CHAR", env, builtin_code_char, TYPE_FUNCTION, NULL, 0);
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
  add_builtin_form ("IMPORT", env, builtin_import, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("EXPORT", env, builtin_export, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("UNEXPORT", env, builtin_unexport, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("USE-PACKAGE", env, builtin_use_package, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("UNUSE-PACKAGE", env, builtin_unuse_package, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("DO-SYMBOLS", env, builtin_do_symbols, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DO-EXTERNAL-SYMBOLS", env, builtin_do_external_symbols,
		    TYPE_MACRO, NULL, 0);
  add_builtin_form ("GET-INTERNAL-RUN-TIME", env, builtin_get_internal_run_time,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("GET-DECODED-TIME", env, builtin_get_decoded_time,
		    TYPE_FUNCTION, NULL, 0);
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
  add_builtin_type ("BIT", env, type_bit, 1, "INTEGER", (char *)NULL);
  add_builtin_type ("RATIO", env, type_ratio, 1, "RATIONAL", (char *)NULL);
  add_builtin_type ("FLOAT", env, type_float, 1, "REAL", (char *)NULL);
  add_builtin_type ("SHORT-FLOAT", env, type_short_float, 1, "SINGLE-FLOAT",
		    (char *)NULL);
  add_builtin_type ("SINGLE-FLOAT", env, type_single_float, 1, "FLOAT",
		    "SHORT-FLOAT", "DOUBLE-FLOAT", "LONG-FLOAT", (char *)NULL);
  add_builtin_type ("DOUBLE-FLOAT", env, type_double_float, 1, "SINGLE-FLOAT",
		    (char *)NULL);
  add_builtin_type ("LONG-FLOAT", env, type_long_float, 1, "SINGLE-FLOAT",
		    (char *)NULL);
  add_builtin_type ("COMPLEX", env, type_complex, 1, "NUMBER", (char *)NULL);
  add_builtin_type ("RANDOM-STATE", env, type_random_state, 1, (char *)NULL);
  add_builtin_type ("CHARACTER", env, type_character, 1, "BASE-CHAR",
		    (char *)NULL);
  add_builtin_type ("BASE-CHAR", env, type_character, 1, "CHARACTER",
		    (char *)NULL);
  add_builtin_type ("STANDARD-CHAR", env, type_standard_char, 1, "BASE-CHAR",
		    (char *)NULL);
  add_builtin_type ("SEQUENCE", env, type_sequence, 1, (char *)NULL);
  add_builtin_type ("LIST", env, type_list, 1, "SEQUENCE", (char *)NULL);
  add_builtin_type ("CONS", env, type_cons, 1, "LIST", "SEQUENCE", (char *)NULL);
  add_builtin_type ("ATOM", env, type_atom, 1, (char *)NULL);
  add_builtin_type ("ARRAY", env, type_array, 1, (char *)NULL);
  add_builtin_type ("SIMPLE-ARRAY", env, type_simple_array, 1, "ARRAY",
		    (char *)NULL);
  add_builtin_type ("VECTOR", env, type_vector, 1, "ARRAY", "SEQUENCE",
		    (char *)NULL);
  add_builtin_type ("SIMPLE-VECTOR", env, type_simple_vector, 1, "VECTOR",
		    "SIMPLE-ARRAY", "ARRAY", "SEQUENCE", (char *)NULL);
  add_builtin_type ("STRING", env, type_string, 1, "VECTOR", "ARRAY", "SEQUENCE",
		    "BASE-STRING", (char *)NULL);
  add_builtin_type ("SIMPLE-STRING", env, type_string, 1, "STRING", "VECTOR",
		    "SIMPLE-ARRAY", "ARRAY", "SEQUENCE", "BASE-STRING",
		    "SIMPLE-BASE-STRING", (char *)NULL);
  add_builtin_type ("BASE-STRING", env, type_string, 1, "VECTOR", "ARRAY",
		    "SEQUENCE", "STRING", (char *)NULL);
  add_builtin_type ("SIMPLE-BASE-STRING", env, type_simple_string, 1, "STRING",
		    "VECTOR", "SIMPLE-ARRAY", "ARRAY", "SEQUENCE", "BASE-STRING",
		    "SIMPLE-STRING", (char *)NULL);
  add_builtin_type ("BIT-VECTOR", env, type_bit_vector, 1, "VECTOR", "ARRAY",
		    "SEQUENCE", (char *)NULL);
  add_builtin_type ("SIMPLE-BIT-VECTOR", env, type_simple_bit_vector, 1,
		    "BIT-VECTOR", "SIMPLE-ARRAY", (char *)NULL);
  add_builtin_type ("HASH-TABLE", env, type_hash_table, 1, (char *)NULL);
  add_builtin_type ("NULL", env, type_null, 1, "SYMBOL", "LIST", "SEQUENCE",
		    (char *)NULL);
  add_builtin_type ("PATHNAME", env, type_pathname, 1, (char *)NULL);
  add_builtin_type ("STREAM", env, type_stream, 1, (char *)NULL);
  add_builtin_type ("FILE-STREAM", env, type_file_stream, 1, "STREAM",
		    (char *)NULL);
  add_builtin_type ("STRING-STREAM", env, type_string_stream, 1, "STREAM",
		    (char *)NULL);
  add_builtin_type ("CLASS", env, type_class, 1, (char *)NULL);
  add_builtin_type ("STRUCTURE-CLASS", env, type_structure_class, 1,
		    (char *)NULL);
  add_builtin_type ("STANDARD-CLASS", env, type_standard_class, 1,
		    (char *)NULL);


  stdobjcl = alloc_object ();
  stdobjcl->type = TYPE_STANDARD_CLASS;
  stdobjcl->value_ptr.standard_class =
    malloc_and_check (sizeof (*stdobjcl->value_ptr.standard_class));
  stdobjcl->value_ptr.standard_class->parents = NULL;
  stdobjcl->value_ptr.standard_class->fields = NULL;

  stdobjsym = CREATE_BUILTIN_SYMBOL ("STANDARD-OBJECT");
  stdobjsym->value_ptr.symbol->is_type = 1;
  stdobjsym->value_ptr.symbol->typespec = stdobjcl;


  add_condition_class ("TYPE-ERROR", env, 1, "ERROR", (char *)NULL,
		       "EXPECTED-TYPE", "DATUM", (char *)NULL);
  add_condition_class ("FILE-ERROR", env, 1, "ERROR", (char *)NULL, "PATHNAME",
		       (char *)NULL);
  add_condition_class ("READER-ERROR", env, 1, "PARSE-ERROR", "STREAM-ERROR",
		       (char *)NULL, (char *)NULL);
  add_condition_class ("STREAM-ERROR", env, 1, "ERROR", (char *)NULL, "STREAM",
		       (char *)NULL);
  add_condition_class ("PARSE-ERROR", env, 1, "ERROR", (char *)NULL,
		       (char *)NULL);
  add_condition_class ("DIVISION-BY-ZERO", env, 1, "ARITHMETIC-ERROR",
		       (char *)NULL, (char *)NULL);
  add_condition_class ("ARITHMETIC-ERROR", env, 1, "ERROR", (char *)NULL,
		       "OPERATION", "OPERANDS", (char *)NULL);
  add_condition_class ("ERROR", env, 1, "SERIOUS-CONDITION", (char *)NULL,
		       (char *)NULL);
  add_condition_class ("SERIOUS-CONDITION", env, 1, "CONDITION", (char *)NULL,
		       (char *)NULL);
  add_condition_class ("SIMPLE-CONDITION", env, 1, "CONDITION", (char *)NULL,
		       "FORMAT-ARGUMENTS", "FORMAT-CONTROL", (char *)NULL);
  add_condition_class ("SIMPLE-ERROR", env, 1, "SIMPLE-CONDITION", (char *)NULL,
		       (char *)NULL);
  add_condition_class ("STYLE-WARNING", env, 1, "WARNING", (char *)NULL,
		       (char *)NULL);
  add_condition_class ("WARNING", env, 1, "CONDITION", (char *)NULL,
		       (char *)NULL);
  add_condition_class ("CONDITION", env, 1, (char *)NULL, (char *)NULL);


  add_builtin_type ("RESTART", env, type_restart, 1, (char *)NULL);


  env->amp_optional_sym = intern_symbol_by_char_vector ("&OPTIONAL",
							strlen ("&OPTIONAL"),
							1, EXTERNAL_VISIBILITY,
							1, env->cl_package);
  env->amp_rest_sym = intern_symbol_by_char_vector ("&REST", strlen ("&REST"),
						    1, EXTERNAL_VISIBILITY, 1,
						    env->cl_package);
  env->amp_body_sym = intern_symbol_by_char_vector ("&BODY", strlen ("&BODY"),
						    1, EXTERNAL_VISIBILITY, 1,
						    env->cl_package);
  env->amp_key_sym = intern_symbol_by_char_vector ("&KEY", strlen ("&KEY"), 1,
						   EXTERNAL_VISIBILITY, 1,
						   env->cl_package);
  env->amp_allow_other_keys_sym =
    intern_symbol_by_char_vector ("&ALLOW-OTHER-KEYS",
				  strlen ("&ALLOW-OTHER-KEYS"), 1,
				  EXTERNAL_VISIBILITY, 1, env->cl_package);
  env->amp_aux_sym = intern_symbol_by_char_vector ("&AUX",
						   strlen ("&AUX"), 1,
						   EXTERNAL_VISIBILITY, 1,
						   env->cl_package);
  env->amp_whole_sym = intern_symbol_by_char_vector ("&WHOLE",
						     strlen ("&WHOLE"), 1,
						     EXTERNAL_VISIBILITY, 1,
						     env->cl_package);
  env->key_allow_other_keys_sym =
    intern_symbol_by_char_vector ("ALLOW-OTHER-KEYS",
				  strlen ("ALLOW-OTHER-KEYS"), 1,
				  EXTERNAL_VISIBILITY, 1, env->keyword_package);

  env->not_sym = CREATE_BUILTIN_SYMBOL ("NOT");
  env->and_sym = CREATE_BUILTIN_SYMBOL ("AND");
  env->or_sym = CREATE_BUILTIN_SYMBOL ("OR");
  env->eql_sym = CREATE_BUILTIN_SYMBOL ("EQL");
  env->member_sym = CREATE_BUILTIN_SYMBOL ("MEMBER");
  env->satisfies_sym = CREATE_BUILTIN_SYMBOL ("SATISFIES");
  env->star_sym = CREATE_BUILTIN_SYMBOL ("*");


  define_constant_by_name ("MOST-POSITIVE-FIXNUM",
			   create_integer_from_long (FIXNUM_MAX), env);
  define_constant_by_name ("MOST-NEGATIVE-FIXNUM",
			   create_integer_from_long (FIXNUM_MIN), env);

  define_constant_by_name ("MOST-POSITIVE-SHORT-FLOAT",
			   create_floating_from_double (DBL_MAX), env);
  define_constant_by_name ("MOST-POSITIVE-SINGLE-FLOAT",
			   create_floating_from_double (DBL_MAX), env);
  define_constant_by_name ("MOST-POSITIVE-DOUBLE-FLOAT",
			   create_floating_from_double (DBL_MAX), env);
  define_constant_by_name ("MOST-POSITIVE-LONG-FLOAT",
			   create_floating_from_double (DBL_MAX), env);

  define_constant_by_name ("LEAST-POSITIVE-SHORT-FLOAT",
			   create_floating_from_double (DBL_MIN), env);
  define_constant_by_name ("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT",
			   create_floating_from_double (DBL_MIN), env);
  define_constant_by_name ("LEAST-POSITIVE-SINGLE-FLOAT",
			   create_floating_from_double (DBL_MIN), env);
  define_constant_by_name ("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT",
			   create_floating_from_double (DBL_MIN), env);
  define_constant_by_name ("LEAST-POSITIVE-DOUBLE-FLOAT",
			   create_floating_from_double (DBL_MIN), env);
  define_constant_by_name ("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT",
			   create_floating_from_double (DBL_MIN), env);
  define_constant_by_name ("LEAST-POSITIVE-LONG-FLOAT",
			   create_floating_from_double (DBL_MIN), env);
  define_constant_by_name ("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT",
			   create_floating_from_double (DBL_MIN), env);

  /* we assume that the set of floating-point numbers is symmetrical around 0,
     even though C89 doesn't mandate that */
  define_constant_by_name ("LEAST-NEGATIVE-SHORT-FLOAT",
			   create_floating_from_double (-DBL_MIN), env);
  define_constant_by_name ("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT",
			   create_floating_from_double (-DBL_MIN), env);
  define_constant_by_name ("LEAST-NEGATIVE-SINGLE-FLOAT",
			   create_floating_from_double (-DBL_MIN), env);
  define_constant_by_name ("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT",
			   create_floating_from_double (-DBL_MIN), env);
  define_constant_by_name ("LEAST-NEGATIVE-DOUBLE-FLOAT",
			   create_floating_from_double (-DBL_MIN), env);
  define_constant_by_name ("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT",
			   create_floating_from_double (-DBL_MIN), env);
  define_constant_by_name ("LEAST-NEGATIVE-LONG-FLOAT",
			   create_floating_from_double (-DBL_MIN), env);
  define_constant_by_name ("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT",
			   create_floating_from_double (-DBL_MIN), env);

  define_constant_by_name ("MOST-NEGATIVE-SHORT-FLOAT",
			   create_floating_from_double (-DBL_MAX), env);
  define_constant_by_name ("MOST-NEGATIVE-SINGLE-FLOAT",
			   create_floating_from_double (-DBL_MAX), env);
  define_constant_by_name ("MOST-NEGATIVE-DOUBLE-FLOAT",
			   create_floating_from_double (-DBL_MAX), env);
  define_constant_by_name ("MOST-NEGATIVE-LONG-FLOAT",
			   create_floating_from_double (-DBL_MAX), env);

  define_constant_by_name ("INTERNAL-TIME-UNITS-PER-SECOND",
			   create_integer_from_long (CLOCKS_PER_SEC), env);

  env->std_in_sym = define_variable ("*STANDARD-INPUT*",
				     create_stream_from_open_file
				     (CHARACTER_STREAM, INPUT_STREAM, stdin),
				     env);

  env->c_stdout = create_stream_from_open_file (CHARACTER_STREAM, OUTPUT_STREAM,
						stdout);

  env->std_out_sym = define_variable ("*STANDARD-OUTPUT*", env->c_stdout, env);

  env->quote_sym = CREATE_BUILTIN_SYMBOL ("QUOTE");
  env->function_sym = CREATE_BUILTIN_SYMBOL ("FUNCTION");
  env->lambda_sym = CREATE_BUILTIN_SYMBOL ("LAMBDA");
  env->setf_sym = CREATE_BUILTIN_SYMBOL ("SETF");

  env->declare_sym = CREATE_BUILTIN_SYMBOL ("DECLARE");
  env->ignorable_sym = CREATE_BUILTIN_SYMBOL ("IGNORABLE");
  env->ignore_sym = CREATE_BUILTIN_SYMBOL ("IGNORE");
  env->inline_sym = CREATE_BUILTIN_SYMBOL ("INLINE");
  env->notinline_sym = CREATE_BUILTIN_SYMBOL ("NOTINLINE");
  env->optimize_sym = CREATE_BUILTIN_SYMBOL ("OPTIMIZE");
  env->compilation_speed_sym = CREATE_BUILTIN_SYMBOL ("COMPILATION-SPEED");
  env->debug_sym = CREATE_BUILTIN_SYMBOL ("DEBUG");
  env->safety_sym = CREATE_BUILTIN_SYMBOL ("SAFETY");
  env->space_sym = CREATE_BUILTIN_SYMBOL ("SPACE");
  env->speed_sym = CREATE_BUILTIN_SYMBOL ("SPEED");
  env->special_sym = CREATE_BUILTIN_SYMBOL ("SPECIAL");
  env->type_sym = CREATE_BUILTIN_SYMBOL ("TYPE");
  env->ftype_sym = CREATE_BUILTIN_SYMBOL ("FTYPE");

  env->print_escape_sym = define_variable ("*PRINT-ESCAPE*", &t_object, env);
  env->print_readably_sym = define_variable ("*PRINT-READABLY*", &nil_object,
					     env);
  env->print_base_sym = define_variable ("*PRINT-BASE*",
					 create_integer_from_long (10), env);
  env->print_array_sym = define_variable ("*PRINT-ARRAY*", &t_object, env);

  env->read_base_sym = define_variable ("*READ-BASE*",
					create_integer_from_long (10), env);
  env->read_suppress_sym = define_variable ("*READ-SUPPRESS*", &nil_object, env);

  env->package_sym->value_ptr.symbol->value_cell = cluser_package;

  add_builtin_form ("AL-PRINT-NO-WARRANTY", env, builtin_al_print_no_warranty,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-PRINT-TERMS-AND-CONDITIONS", env,
		    builtin_al_print_terms_and_conditions, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("AL-EXIT", env, builtin_al_exit, TYPE_FUNCTION, NULL, 0);
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

  printf ("%s", prompt);

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


enum outcome_type
read_object_continued (struct object **obj, int backts_commas_balance,
		       int is_empty_list, const char *input, size_t size,
		       FILE *stream, int preserve_whitespace,
		       int ends_with_eof, struct environment *env,
		       struct outcome *outcome,  const char **obj_begin,
		       const char **obj_end)
{
  enum outcome_type out;
  int bts, cs, tokensize, tokenlength;
  struct object *last_pref, *ob = skip_prefix (*obj, &bts, &cs, &last_pref),
    *skip = inspect_variable (env->read_suppress_sym, env);
  struct object *l, *call;
  char *token;

  backts_commas_balance += (bts - cs);

  if (outcome->multiline_comment_depth)
    {
      if (!jump_to_end_of_multiline_comment (&input, &size, stream,
					     &outcome->multiline_comment_depth))
	return INCOMPLETE_OBJECT (ob, is_empty_list);
    }

  if (SYMBOL (skip) != &nil_object)
    {
      if (outcome->type == UNCLOSED_NONEMPTY_LIST)
	outcome->type = NO_OBJECT;

      out = skip_without_reading (outcome->type, 0, input, size, stream,
				  preserve_whitespace, ends_with_eof, env,
				  outcome, &outcome->skipped_list_depth,
				  obj_begin, obj_end);

      if (outcome->skipped_list_depth)
	out = UNCLOSED_NONEMPTY_LIST;
      else if (out == COMPLETE_OBJECT)
	out = SKIPPED_OBJECT;

      return out;
    }

  if (is_empty_list)
    {
      l = NULL;

      out = read_list (&l, backts_commas_balance, input, size, stream,
		       preserve_whitespace, ends_with_eof, env, outcome,
		       obj_end);

      ob = l;
    }
  else if (!ob)
    {
      out = read_object (&ob, backts_commas_balance, input, size, stream,
			 preserve_whitespace, ends_with_eof, env, outcome,
			 obj_begin, obj_end);

      if (out == NO_OBJECT && last_pref)
	out = JUST_PREFIX;
      else if (out == CLOSING_PARENTHESIS && last_pref)
	out = CLOSING_PARENTHESIS_AFTER_PREFIX;
    }
  else if (ob->type == TYPE_CONS_PAIR)
    {
      out = read_list (&ob, backts_commas_balance, input, size, stream,
		       preserve_whitespace, ends_with_eof, env, outcome,
		       obj_end);

      if (out == UNCLOSED_EMPTY_LIST)
	out = UNCLOSED_NONEMPTY_LIST;
    }
  else if (ob->type == TYPE_STRING)
    {
      out = read_string (&ob, input, size, stream, obj_end);
    }
  else if (ob->type == TYPE_SYMBOL_NAME)
    {
      if (!input)
	token = accumulate_token (stream, preserve_whitespace, &tokensize,
				  &tokenlength, outcome);
      else
	tokenlength = size;

      if ((!input || ends_with_eof)
	  && (outcome->single_escape || outcome->multiple_escape))
	{
	  free (token);
	  return GOT_EOF_IN_MIDDLE_OF_OBJECT;
	}

      out = read_symbol_name (&ob, input ? input : token, tokenlength,
			      input == NULL, preserve_whitespace, obj_end,
			      CASE_UPCASE, outcome);

      if (out == COMPLETE_OBJECT && !intern_symbol_name (ob, env, &out))
	{
	  outcome->obj = ob;
	  return out;
	}
    }
  else if (ob->type == TYPE_SHARP_MACRO_CALL)
    {
      out = read_sharp_macro_call (&ob, input, size, stream, preserve_whitespace,
				   ends_with_eof, env, outcome, obj_end);

      if (out == COMPLETE_OBJECT)
	{
	  call = ob;
	  ob = call_sharp_macro (call->value_ptr.sharp_macro_call, env, outcome);
	  free_sharp_macro_call (call);

	  if (!ob)
	    {
	      return outcome->type;
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
			       struct outcome *outcome,
			       const char **input_left, size_t *input_left_size,
			       char **wholeline)
{
  char *line;
  enum outcome_type read_out;
  const char *begin, *end;
  size_t len;

  line = read_line_interactively ("> ");
  len = strlen (line);

  read_out = read_object_continued (&obj, 0, is_empty_list, line, len, NULL, 0,
				    0, env, outcome, &begin, &end);

  while (IS_INCOMPLETE_OBJECT (read_out) || IS_READ_OR_EVAL_ERROR (read_out)
	 || outcome->multiline_comment_depth)
    {
      if (IS_READ_OR_EVAL_ERROR (read_out))
	{
	  outcome->type = read_out;
	  print_error (outcome, env);
	  return NULL;
	}

      free (line);
      line = read_line_interactively ("> ");
      len = strlen (line);

      read_out = read_object_continued (&obj, 0,
					read_out == UNCLOSED_EMPTY_LIST, line,
					len, NULL, 0, 0, env, outcome, &begin,
					&end);
    }

  *input_left = end + 1;
  *input_left_size = (line + len) - end - 1;
  *wholeline = line;

  if (read_out == SKIPPED_OBJECT)
    {
      outcome->type = SKIPPED_OBJECT;
      return NULL;
    }

  return obj;
}


struct object *
read_object_interactively_continued (const char *input, size_t input_size,
				     struct environment *env,
				     struct outcome *outcome,
				     const char **input_left,
				     size_t *input_left_size, char **wholeline)
{
  enum outcome_type read_out;
  struct object *obj = NULL, *ret;
  const char *begin, *end;


 read_further:
  read_out = read_object (&obj, 0, input, input_size, NULL, 0, 0, env, outcome,
			  &begin, &end);

  if (read_out == COMPLETE_OBJECT && !outcome->multiline_comment_depth)
    {
      *input_left = end + 1;
      *input_left_size = (input + input_size) - end - 1;

      return obj;
    }
  else if (read_out == SKIPPED_OBJECT)
    {
      input_size = (input + input_size) - end - 1;
      input = end + 1;
      obj = NULL;

      goto read_further;
    }
  else if (read_out == NO_OBJECT && !outcome->multiline_comment_depth)
    {
      *input_left = NULL;
      *input_left_size = 0;
      
      return NULL;
    }
  else if (IS_READ_OR_EVAL_ERROR (read_out))
    {
      outcome->type = read_out;
      print_error (outcome, env);

      return NULL;
    }
  else
    {
      outcome->type = read_out;

      ret = complete_object_interactively (obj,
					   read_out == UNCLOSED_EMPTY_LIST,
					   env, outcome, input_left,
					   input_left_size, wholeline);

      if (!ret && outcome->type == SKIPPED_OBJECT)
	{
	  input = *input_left;
	  input_size = *input_left_size;
	  obj = NULL;

	  goto read_further;
	}

      return ret;
    }
}


struct object *
read_object_interactively (struct environment *env, struct outcome *outcome,
			   const char **input_left, size_t *input_left_size,
			   char **wholeline)
{
  char *pr = generate_prompt (env), *line;
  struct object *ret;

  line = read_line_interactively (pr);
  *wholeline = line;

  ret = read_object_interactively_continued (line, strlen (line), env, outcome,
					     input_left, input_left_size,
					     wholeline);

  free (pr);

  return ret;
}


int
next_char (unsigned char *ch, const char **input, size_t *size, FILE *stream)
{
  int c;

  if (*input)
    {
      if (!*size)
	return 0;

      (*input)++;
      (*size)--;

      *ch = *(*input-1);
      return 1;
    }
  else
    {
      c = fgetc (stream);

      if (c == EOF)
	return 0;

      *ch = c;
      return 1;
    }
}


int
next_nonspace_char (unsigned char *ch, const char **input, size_t *size,
		    FILE *stream)
{
  while (next_char (ch, input, size, stream))
    {
      if (!isspace ((unsigned char)*ch))
	{
	  return 1;
	}
    }

  return 0;
}


int
unget_char (unsigned char ch, const char **input, size_t *size, FILE *stream)
{
  if (*input)
    {
      (*input)--;
      (*size)++;
      return 1;
    }
  else
    {
      ungetc (ch, stream);
      return 1;
    }
}


int
jump_to_end_of_line (const char **input, size_t *size, FILE *stream)
{
  unsigned char ch;

  while (next_char (&ch, input, size, stream))
    {
      if (ch == '\n')
	{
	  return 1;
	}
    }

  return 0;
}


int
jump_to_end_of_multiline_comment (const char **input, size_t *size, FILE *stream,
				  size_t *depth)
{
  unsigned char ch, ch2;

  while (next_char (&ch, input, size, stream))
    {
      if (ch == '#' || ch == '|')
	{
	  if (!next_char (&ch2, input, size, stream))
	    return 0;

	  if (ch == '#' && ch2 == '|')
	    (*depth)++;
	  else if (ch == '|' && ch2 == '#')
	    (*depth)--;
	  else
	    unget_char (ch2, input, size, stream);

	  if (!*depth)
	    return 1;
	}
    }

  return 0;
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


fixnum
object_list_length (struct object_list *list)
{
  fixnum i = 0;

  while (list)
    {
      i++;
      list = list->next;
    }

  return i;
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


enum outcome_type
read_object (struct object **obj, int backts_commas_balance, const char *input,
	     size_t size, FILE *stream, int preserve_whitespace,
	     int ends_with_eof, struct environment *env, struct outcome *outcome,
	     const char **obj_begin, const char **obj_end)
{
  int found_prefix = 0, tokensize, tokenlength, numbase;
  struct object *last_pref, *ob = NULL, *call,
    *skip = inspect_variable (env->read_suppress_sym, env);
  enum object_type numtype;
  enum outcome_type out = NO_OBJECT;
  const char *num_end;
  char *token;
  size_t exp_mark_pos;
  unsigned char ch;


  if (SYMBOL (skip) != &nil_object)
    {
      out = skip_without_reading (NO_OBJECT, 0, input, size, stream,
				  preserve_whitespace, ends_with_eof, env,
				  outcome, &outcome->skipped_list_depth,
				  obj_begin, obj_end);

      if (outcome->skipped_list_depth)
	out = UNCLOSED_NONEMPTY_LIST;
      else if (out == COMPLETE_OBJECT)
	out = SKIPPED_OBJECT;

      return out;
    }

  if (!next_nonspace_char (&ch, &input, &size, stream))
    return NO_OBJECT;

  while (1)
    {
      if (ch == ';')
	{
	  if (!jump_to_end_of_line (&input, &size, stream))
	    break;
	}
      else if (ch == '#')
	{
	  if (!next_char (&ch, &input, &size, stream))
	    break;

	  if (ch == '|')
	    {
	      outcome->multiline_comment_depth = 1;

	      if (!jump_to_end_of_multiline_comment (&input, &size, stream,
						     &outcome->
						     multiline_comment_depth))
		break;
	    }
	  else
	    {
	      unget_char (ch, &input, &size, stream);

	      if (input)
		*obj_begin = input;

	      out = read_sharp_macro_call (&ob, input, size, stream,
					   preserve_whitespace,
					   ends_with_eof, env, outcome, obj_end);

	      if (out == COMPLETE_OBJECT)
		{
		  call = ob;
		  ob = call_sharp_macro (call->value_ptr.sharp_macro_call, env,
					 outcome);
		  free_sharp_macro_call (call);

		  if (!ob && outcome->type == NO_OBJECT)
		    {
		      if (input)
			{
			  size = size - (*obj_end - input) - 1;
			  input = *obj_end + 1;
			}

		      if (!next_nonspace_char (&ch, &input, &size, stream))
			return found_prefix ? JUST_PREFIX : NO_OBJECT;

		      continue;
		    }
		  else if (!ob)
		    {
		      return outcome->type;
		    }
		}

	      break;
	    }
	}
      else if (ch == '\'')
	{
	  ob = alloc_empty_cons_pair ();

	  SET_READER_MACRO_FLAG (ob);

	  ob->value_ptr.cons_pair->car = env->quote_sym;
	  add_reference (ob, env->quote_sym, 0);

	  ob->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

	  ob->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr = &nil_object;

	  out = read_object (&ob->value_ptr.cons_pair->cdr->
			     value_ptr.cons_pair->car, backts_commas_balance,
			     input, size, stream, preserve_whitespace,
			     ends_with_eof, env, outcome, obj_begin, obj_end);

	  if (input)
	    {
	      size = size - (*obj_end - input);
	      input = *obj_end;
	    }

	  if (out == UNCLOSED_EMPTY_LIST)
	    {
	      ob->value_ptr.cons_pair->cdr->value_ptr.cons_pair->
		empty_list_in_car = 1;
	      out = UNCLOSED_NONEMPTY_LIST;
	    }
	  else if (out != COMPLETE_OBJECT)
	    {
	      ob->value_ptr.cons_pair->cdr->value_ptr.cons_pair->filling_car = 1;
	      out = UNCLOSED_NONEMPTY_LIST;
	    }

	  break;
	}
      else if (ch == '`' || ch == ',')
 	{
	  unget_char (ch, &input, &size, stream);

	  out = read_prefix (obj, input, size, stream, &backts_commas_balance,
			     &last_pref, obj_end);

	  if (out == TOO_MANY_COMMAS)
	    return out;

	  if (input)
	    {
	      size = size - (*obj_end - input);
	      input = *obj_end;
	    }

	  found_prefix = 1;
	}
      else if (ch == ')')
	{
	  if (input)
	    *obj_end = input-1;

	  return found_prefix ? CLOSING_PARENTHESIS_AFTER_PREFIX :
	    CLOSING_PARENTHESIS;
	}
      else if (ch == '(')
	{
	  if (input)
	    *obj_begin = input;

	  out = read_list (&ob, backts_commas_balance, input, size, stream,
			   preserve_whitespace, ends_with_eof, env, outcome,
			   obj_end);
	  break;
	}
      else if (ch == '"')
	{
	  if (input)
	    *obj_begin = input;

	  out = read_string (&ob, input, size, stream, obj_end);
	  break;
	}
      else
	{
	  unget_char (ch, &input, &size, stream);

	  if (input)
	    *obj_begin = input;

	  if (!input)
	    token = accumulate_token (stream, preserve_whitespace, &tokensize,
				      &tokenlength, outcome);
	  else
	    tokenlength = size;

	  numbase = get_read_base (env);

	  if (is_number (input ? input : token, tokenlength, numbase, &numtype,
			 &num_end, &exp_mark_pos, obj_end))
	    {
	      ob = create_number (input ? input : token, num_end
				  - (input ? input : token) + 1,
				  exp_mark_pos, numbase, numtype);
	      out = COMPLETE_OBJECT;
	    }
	  else
	    {
	      if ((!input || ends_with_eof)
		  && (outcome->single_escape || outcome->multiple_escape))
		{
		  free (token);
		  return GOT_EOF_IN_MIDDLE_OF_OBJECT;
		}

	      out = read_symbol_name (&ob, input ? input : token, tokenlength,
				      input == NULL || ends_with_eof,
				      preserve_whitespace, obj_end, CASE_UPCASE,
				      outcome);

	      if (out == COMPLETE_OBJECT && !intern_symbol_name (ob, env, &out))
		{
		  outcome->obj = ob;
		  return out;
		}
	    }

	  break;
	}

      if (!next_nonspace_char (&ch, &input, &size, stream))
	return found_prefix ? JUST_PREFIX : NO_OBJECT;
    }

  if (found_prefix)
    last_pref->value_ptr.next = ob;
  else
    *obj = ob;

  return out;
}


enum outcome_type
read_list (struct object **obj, int backts_commas_balance, const char *input,
	   size_t size, FILE *stream, int preserve_whitespace,
	   int ends_with_eof, struct environment *env,
	   struct outcome *outcome, const char **list_end)
{
  struct object *last_cons = *obj, *car = NULL, *ob = *obj, *cons;
  const char *obj_beg, *obj_end = NULL;
  enum outcome_type out;
  int found_dot = 0;


  while (ob && ob->type == TYPE_CONS_PAIR)
    {
      if (ob->value_ptr.cons_pair->found_dot)
	found_dot = 1;

      if (ob->value_ptr.cons_pair->filling_car
	  || ob->value_ptr.cons_pair->filling_cdr)
	{
	  out = read_object_continued (ob->value_ptr.cons_pair->filling_car
				       ? &ob->value_ptr.cons_pair->car
				       : &ob->value_ptr.cons_pair->cdr,
				       backts_commas_balance, 0, input, size,
				       stream, preserve_whitespace,
				       ends_with_eof, env, outcome, &obj_beg,
				       &obj_end);

	  if (out == COMPLETE_OBJECT)
	    {
	      ob->value_ptr.cons_pair->filling_car =
		ob->value_ptr.cons_pair->filling_cdr = 0;
	    }
	  else if (out == UNCLOSED_EMPTY_LIST)
	    {
	      ob->value_ptr.cons_pair->filling_car
		? (ob->value_ptr.cons_pair->empty_list_in_car = 1)
		: (ob->value_ptr.cons_pair->empty_list_in_cdr = 1);

	      ob->value_ptr.cons_pair->filling_car =
		ob->value_ptr.cons_pair->filling_cdr = 0;
	    }

	  if (IS_INCOMPLETE_OBJECT (out) || IS_READ_OR_EVAL_ERROR (out))
	    return out;
	}
      else if (ob->value_ptr.cons_pair->empty_list_in_car
	       || ob->value_ptr.cons_pair->empty_list_in_cdr)
	{
	  out = read_object_continued (ob->value_ptr.cons_pair->empty_list_in_car
				       ? &ob->value_ptr.cons_pair->car
				       : &ob->value_ptr.cons_pair->cdr,
				       backts_commas_balance, 1, input, size,
				       stream, preserve_whitespace,
				       ends_with_eof, env, outcome, &obj_beg,
				       &obj_end);

	  if (out == COMPLETE_OBJECT)
	    ob->value_ptr.cons_pair->empty_list_in_car =
	      ob->value_ptr.cons_pair->empty_list_in_cdr = 0;

	  if (IS_INCOMPLETE_OBJECT (out) && out != UNCLOSED_EMPTY_LIST)
	    {
	      ob->value_ptr.cons_pair->empty_list_in_car
		? (ob->value_ptr.cons_pair->filling_car = 1)
		: (ob->value_ptr.cons_pair->filling_cdr = 1);

	      ob->value_ptr.cons_pair->empty_list_in_car =
		ob->value_ptr.cons_pair->empty_list_in_cdr = 0;
	    }

	  if (IS_INCOMPLETE_OBJECT (out) || IS_READ_OR_EVAL_ERROR (out))
	    return out;
	}

      last_cons = ob;
      ob = ob->value_ptr.cons_pair->cdr;
    }

  if (input && obj_end)
    {
      size -= obj_end - input + 1;
      input = obj_end + 1;
    }

  if (last_cons && last_cons->value_ptr.cons_pair->cdr == &nil_object
      && !found_dot)
    {
      *list_end = obj_end;
      return COMPLETE_OBJECT;
    }

  out = read_object (&car, backts_commas_balance, input, size, stream,
		     preserve_whitespace, ends_with_eof, env, outcome,
		     &obj_beg, &obj_end);

  if (out == NO_OBJECT && !last_cons)
    return UNCLOSED_EMPTY_LIST;

  if (out == CLOSING_PARENTHESIS && !last_cons)
    {
      *list_end = obj_end;
      *obj = &nil_object;
      return COMPLETE_OBJECT;
    }

  while (out != NO_OBJECT && out != UNCLOSED_EMPTY_LIST)
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
	    return MORE_THAN_A_CONSING_DOT;

	  last_cons->value_ptr.cons_pair->found_dot = 1;
	}
      else if (IS_READ_OR_EVAL_ERROR (out))
	{
	  return out;
	}
      else if (out == COMPLETE_OBJECT || IS_INCOMPLETE_OBJECT (out))
	{
	  if (last_cons && last_cons->value_ptr.cons_pair->found_dot
	      && last_cons->value_ptr.cons_pair->cdr
	      && !last_cons->value_ptr.cons_pair->filling_cdr)
	    return MULTIPLE_OBJS_AFTER_DOT_IN_LIST;
	  else if (last_cons && last_cons->value_ptr.cons_pair->found_dot)
	    {
	      last_cons->value_ptr.cons_pair->cdr = car;

	      if (IS_INCOMPLETE_OBJECT (out))
		{
		  last_cons->value_ptr.cons_pair->filling_cdr = 1;

		  return UNCLOSED_NONEMPTY_LIST;
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

	      if (IS_INCOMPLETE_OBJECT (out))
		{
		  cons->value_ptr.cons_pair->filling_car = 1;

		  return UNCLOSED_NONEMPTY_LIST;
		}
	    }
	}

      car = NULL;

      out = read_object (&car, backts_commas_balance, input ? obj_end + 1 : NULL,
			 size - (obj_end + 1 - input), stream,
			 preserve_whitespace, ends_with_eof, env, outcome,
			 &obj_beg, &obj_end);
    }

  if (out == UNCLOSED_EMPTY_LIST)
    {
      if (last_cons && last_cons->value_ptr.cons_pair->found_dot)
	{
	  last_cons->value_ptr.cons_pair->cdr = car;
	  last_cons->value_ptr.cons_pair->empty_list_in_cdr = 1;
	}
      else
	{
	  cons = alloc_empty_cons_pair ();
	  cons->value_ptr.cons_pair->car = car;
	  cons->value_ptr.cons_pair->empty_list_in_car = 1;

	  if (last_cons)
	    last_cons->value_ptr.cons_pair->cdr = cons;
	  else
	    *obj = cons;
	}
    }

  return UNCLOSED_NONEMPTY_LIST;
}


enum outcome_type
read_string (struct object **obj, const char *input, size_t size, FILE *stream,
	     const char **string_end)
{
  size_t length, new_size, incr = 16;
  struct string *str;
  enum outcome_type out = INCOMPLETE_STRING;
  int ch, quote = 0;

  if (input)
    {
      *string_end = find_end_of_string (input, size, &new_size, &length);

      if (*string_end)
	out = COMPLETE_OBJECT;

      if (!*obj)
	{
	  *obj = alloc_string (length);
	}
      else
	resize_string_allocation (*obj, (*obj)->value_ptr.string->used_size
				  + length);

      if (!length)
	return COMPLETE_OBJECT;

      str = (*obj)->value_ptr.string;

      normalize_string (str->value + str->used_size, input, size);

      str->used_size += length;
    }
  else
    {
      if (!*obj)
	{
	  *obj = alloc_string (incr);
	}

      str = (*obj)->value_ptr.string;

      ch = fgetc (stream);

      while (1)
	{
	  if (ch == EOF)
	    return INCOMPLETE_STRING;

	  if (ch == '\\' && !quote)
	    {
	      quote = 1;
	    }
	  else if (ch == '"' && !quote)
	    {
	      return COMPLETE_OBJECT;
	    }
	  else
	    {
	      if (str->used_size == str->alloc_size)
		{
		  incr <<= 1;
		  resize_string_allocation (*obj,
					    (*obj)->value_ptr.string->used_size
					    + incr);
		}

	      str->value [str->used_size++] = ch;

	      quote = 0;
	    }

	  ch = fgetc (stream);
	}
    }

  return out;
}


enum outcome_type
read_symbol_name (struct object **obj, const char *input, size_t size,
		  int got_eof, int preserve_whitespace, const char **symname_end,
		  enum readtable_case read_case,
		  struct outcome *out)
{
  struct symbol_name *sym;
  size_t name_l, act_name_l;
  struct object *ob = *obj;
  const char *start_of_pack_sep;
  enum package_record_visibility visib;
  int escape_first_ch = out->single_escape, mult_esc = out->multiple_escape;

  out->type = NO_OBJECT;

  *symname_end = find_end_of_symbol_name
    (input, size, got_eof, preserve_whitespace, ob != NULL,
     ob && ob->value_ptr.symbol_name->packname_present ? 1 : 0,
     &start_of_pack_sep, &visib, &name_l, &act_name_l, out);

  if (IS_READ_OR_EVAL_ERROR (out->type))
    return out->type;

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
				       read_case, escape_first_ch, mult_esc);
  else if (start_of_pack_sep)
    {
      copy_symname_with_case_conversion (sym->value + sym->used_size, input,
					 start_of_pack_sep - input, read_case,
					 escape_first_ch, mult_esc);
      sym->packname_present = 1;
      sym->visibility = visib;
      copy_symname_with_case_conversion (sym->actual_symname
					 + sym->actual_symname_used_s,
					 visib == EXTERNAL_VISIBILITY ?
					 start_of_pack_sep + 1
					 : start_of_pack_sep + 2, size,
					 read_case, 0, 0);
    }
  else
    copy_symname_with_case_conversion (sym->value + sym->used_size, input, size,
				       read_case, escape_first_ch, mult_esc);

  sym->used_size += name_l;
  sym->actual_symname_used_s += act_name_l;

  if (!*symname_end)
    return INCOMPLETE_SYMBOL_NAME;
  else
    return COMPLETE_OBJECT;
}


enum outcome_type
read_prefix (struct object **obj, const char *input, size_t size, FILE *stream,
	     int *backts_commas_balance, struct object **last,
	     const char **prefix_end)
{
  int num_backts, num_commas;
  int found_comma = 0;
  unsigned char ch;

  skip_prefix (*obj, &num_backts, &num_commas, last);

  *backts_commas_balance += (num_backts - num_commas);

  if (*backts_commas_balance < 0)
    return TOO_MANY_COMMAS;

  if (!next_nonspace_char (&ch, &input, &size, stream))
    return JUST_PREFIX;

  while (ch == '`' || ch == ',' || ch == '@' || ch == '.')
    {
      if ((ch == '@' || ch == '.') && !found_comma)
	{
	  unget_char (ch, &input, &size, stream);
	  return JUST_PREFIX;
	}

      if (input)
	*prefix_end = input;

      if (!*last)
	*obj = *last = alloc_prefix (ch);
      else
	*last = (*last)->value_ptr.next = alloc_prefix (ch);

      if (ch == '`')
	(*backts_commas_balance)++;
      else if (ch == ',')
	(*backts_commas_balance)--;

      if (*backts_commas_balance < 0)
	return TOO_MANY_COMMAS;

      if (ch == ',')
	found_comma = 1;
      else
	found_comma = 0;

      if (!next_nonspace_char (&ch, &input, &size, stream))
	return JUST_PREFIX;
    }

  unget_char (ch, &input, &size, stream);
  return JUST_PREFIX;
}


enum outcome_type
read_sharp_macro_call (struct object **obj, const char *input, size_t size,
		       FILE *stream, int preserve_whitespace, int ends_with_eof,
		       struct environment *env, struct outcome *outcome,
		       const char **macro_end)
{
  int arg, tokenlength, tokensize, base;
  const char *obj_b, *num_e;
  char *token;
  struct object *prevpack;
  struct sharp_macro_call *call;
  enum outcome_type out;
  enum object_type ot;
  unsigned char ch;
  size_t ep;

  if (*obj)
    {
      call = (*obj)->value_ptr.sharp_macro_call;

      if ((call->dispatch_ch == '+' || call->dispatch_ch == '-')
	  && (call->feat_test_incomplete || call->feat_test_is_empty_list))
	{
	  prevpack = inspect_variable (env->package_sym, env);
	  set_value (env->package_sym, env->keyword_package, 0, env, outcome);
	  out = read_object_continued (&call->feature_test, 0,
				       call->feat_test_is_empty_list, input,
				       size, stream, preserve_whitespace,
				       ends_with_eof, env, outcome, &obj_b,
				       macro_end);
	  set_value (env->package_sym, prevpack, 0, env, outcome);

	  if (IS_READ_OR_EVAL_ERROR (out))
	    return out;

	  if (input)
	    {
	      size = size - (*macro_end - input) - 1;
	      input = *macro_end + 1;
	    }

	  if (out == UNCLOSED_EMPTY_LIST)
	    {
	      call->feat_test_is_empty_list = 1;
	      return INCOMPLETE_SHARP_MACRO_CALL;
	    }

	  if (IS_INCOMPLETE_OBJECT (out) || out == NO_OBJECT)
	    {
	      call->feat_test_incomplete = 1;
	      return INCOMPLETE_SHARP_MACRO_CALL;
	    }

	  call->feat_test_incomplete = call->feat_test_is_empty_list = 0;

	  call->feat_test_result = evaluate_feature_test (call->feature_test,
							  env, outcome);

	  call->obj = NULL;
	  call->list_depth = 0;
	  call->obj_type = NO_OBJECT;
	}

      if ((call->dispatch_ch == '+' || call->dispatch_ch == '-')
	  && !call->feat_test_incomplete && !call->feat_test_is_empty_list
	  && (call->feat_test_result != (call->dispatch_ch == '+')))
	{
	  out = skip_without_reading (call->obj_type, 0, input, size, stream,
				      preserve_whitespace, ends_with_eof, env,
				      outcome, &call->list_depth, &obj_b,
				      macro_end);

	  if (out == NO_OBJECT || IS_INCOMPLETE_OBJECT (out) || call->list_depth)
	    out = INCOMPLETE_SHARP_MACRO_CALL;

	  return out;
	}

      if (call->dispatch_ch == ':')
	{
	  prevpack = inspect_variable (env->package_sym, env);

	  set_value (env->package_sym, NULL, 0, env, outcome);

	  call->obj = NULL;
	  out = read_object_continued (&call->obj, 0, call->is_empty_list, input,
				       size, stream, preserve_whitespace,
				       ends_with_eof, env, outcome, &obj_b,
				       macro_end);

	  set_value (env->package_sym, prevpack, 0, env, outcome);

	  if (out == UNCLOSED_EMPTY_LIST)
	    call->is_empty_list = 1;
	  else
	    call->is_empty_list = 0;

	  if (IS_INCOMPLETE_OBJECT (out))
	    return INCOMPLETE_SHARP_MACRO_CALL;

	  return out;
	}


      out = read_object_continued (&call->obj, 0, call->is_empty_list, input,
				   size, stream, preserve_whitespace,
				   ends_with_eof, env, outcome, &obj_b,
				   macro_end);

      if (out == UNCLOSED_EMPTY_LIST)
	call->is_empty_list = 1;
      else
	call->is_empty_list = 0;

      if (call->dispatch_ch == '\\'
	  && call->obj->value_ptr.symbol_name->packname_present)
	{
	  return WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	}

      if (IS_INCOMPLETE_OBJECT (out))
	return INCOMPLETE_SHARP_MACRO_CALL;

      return out;
    }

  *obj = alloc_sharp_macro_call ();

  call = (*obj)->value_ptr.sharp_macro_call;

  next_char (&ch, &input, &size, stream);

  if (isdigit ((unsigned char)ch))
    {
      arg = ch - '0';
      next_char (&ch, &input, &size, stream);
    }
  else
    arg = -1;

  while (isdigit ((unsigned char)ch))
    {
      arg *= 10;
      arg += ch - '0';
      next_char (&ch, &input, &size, stream);
    }

  call->arg = arg;

  call->dispatch_ch = ch;

  if (strchr ("\b\t\n\r\f <)", call->dispatch_ch))
    {
      return INVALID_SHARP_DISPATCH;
    }

  if (!strchr ("'\\.pP(:cC+-*bBoOxXrR", call->dispatch_ch))
    {
      return UNKNOWN_SHARP_DISPATCH;
    }

  if (call->dispatch_ch == '\\')
    {
      call->obj = NULL;

      if (!input)
	{
	  outcome->single_escape = 1;
	  token = accumulate_token (stream, preserve_whitespace, &tokensize,
				    &tokenlength, outcome);
	}
      else
	{
	  tokenlength = size;
	}

      if ((!input || ends_with_eof)
	  && (outcome->single_escape || outcome->multiple_escape))
	{
	  free (token);
	  return GOT_EOF_IN_MIDDLE_OF_OBJECT;
	}

      outcome->single_escape = 1;
      out = read_symbol_name (&call->obj, input ? input : token, tokenlength,
			      input == NULL, preserve_whitespace, macro_end,
			      CASE_UPCASE, outcome);

      if (call->obj->value_ptr.symbol_name->packname_present)
	{
	  return WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	}

      if (IS_INCOMPLETE_OBJECT (out))
	return INCOMPLETE_SHARP_MACRO_CALL;

      return out;
    }
  else if (call->dispatch_ch == '(')
    {
      call->obj = NULL;
      out = read_list (&call->obj, 0, input, size, stream, preserve_whitespace,
		       ends_with_eof, env, outcome, macro_end);

      if (out == UNCLOSED_EMPTY_LIST)
	call->is_empty_list = 1;

      if (IS_INCOMPLETE_OBJECT (out))
	return INCOMPLETE_SHARP_MACRO_CALL;

      return out;
    }
  else if (call->dispatch_ch == '+' || call->dispatch_ch == '-')
    {
      prevpack = inspect_variable (env->package_sym, env);

      set_value (env->package_sym, env->keyword_package, 0, env, outcome);

      call->feature_test = NULL;
      out = read_object (&call->feature_test, 0, input, size, stream,
			 preserve_whitespace, ends_with_eof, env, outcome,
			 &obj_b, macro_end);

      set_value (env->package_sym, prevpack, 0, env, outcome);

      if (IS_READ_OR_EVAL_ERROR (out))
	return out;

      if (input)
	{
	  size = size - (*macro_end - input) - 1;
	  input = *macro_end + 1;
	}

      if (out == UNCLOSED_EMPTY_LIST)
	{
	  call->feat_test_is_empty_list = 1;
	  return INCOMPLETE_SHARP_MACRO_CALL;
	}

      if (IS_INCOMPLETE_OBJECT (out) || out == NO_OBJECT)
	{
	  call->feat_test_incomplete = 1;
	  return INCOMPLETE_SHARP_MACRO_CALL;
	}

      call->feat_test_result = evaluate_feature_test (call->feature_test, env,
						      outcome);

      if (call->feat_test_result < 0)
	return outcome->type;

      if (call->feat_test_result == (call->dispatch_ch == '+'))
	{
	  call->obj = NULL;
	  out = read_object (&call->obj, 0, input, size, stream,
			     preserve_whitespace, ends_with_eof, env, outcome,
			     &obj_b, macro_end);

	  if (out == UNCLOSED_EMPTY_LIST)
	    call->is_empty_list = 1;
	}
      else
	{
	  call->list_depth = 0;
	  call->obj_type = NO_OBJECT;

	  out = skip_without_reading (NO_OBJECT, 0, input, size, stream,
				      preserve_whitespace, ends_with_eof, env,
				      outcome, &call->list_depth, &obj_b,
				      macro_end);

	  if (IS_INCOMPLETE_OBJECT (out))
	    call->obj_type = out;

	  if (call->list_depth)
	    out = INCOMPLETE_SHARP_MACRO_CALL;
	}

      if (out == NO_OBJECT || IS_INCOMPLETE_OBJECT (out))
	out = INCOMPLETE_SHARP_MACRO_CALL;

      return out;
    }
  else if (call->dispatch_ch == '*')
    {
      if (!input)
	{
	  token = accumulate_token (stream, 1, &tokensize, &tokenlength, outcome);
	}
      else
	{
	  tokenlength = size;
	}

      if (!does_token_begin (input ? input : token, tokenlength, NULL))
	{
	  if (input)
	    *macro_end = input;
	}
      else if (!is_number (input ? input : token, tokenlength, 2, &ot, &num_e, &ep,
			   macro_end)
	       || ot != TYPE_INTEGER || num_e != *macro_end
	       || (arg >= 0 && num_e - (input ? input : token) > arg-1))
	{
	  if (!input)
	    free (token);

	  return WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	}

      call->obj = create_bitvector_from_char_vector (input ? input : token,
						     tokenlength,
						     arg > 0 ? arg : 0);

      if (!input)
	free (token);

      return COMPLETE_OBJECT;
    }
  else if (strchr ("bBoOxXrR", call->dispatch_ch))
    {
      switch (call->dispatch_ch)
	{
	case 'b':
	case 'B':
	  base = 2;
	  break;
	case 'o':
	case 'O':
	  base = 8;
	  break;
	case 'x':
	case 'X':
	  base = 16;
	  break;
	default:
	  if (arg < 2 || arg > 32)
	    {
	      return WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	    }
	  base = arg;
	  break;
	}

      if (!does_token_begin (input, size, stream))
	{
	  return WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	}

      if (!input)
	{
	  token = accumulate_token (stream, 1, &tokensize, &tokenlength, outcome);
	}
      else
	{
	  tokenlength = size;
	}

      if (!is_number (input ? input : token, tokenlength, base, &ot, &num_e, &ep,
		      macro_end) || ot == TYPE_FLOAT)
	{
	  if (!input)
	    free (token);

	  return WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	}

      call->obj = create_number (input ? input : token, tokenlength, 0, base, ot);

      if (!input)
	free (token);

      return COMPLETE_OBJECT;
    }
  else if (call->dispatch_ch == ':')
    {
      prevpack = inspect_variable (env->package_sym, env);

      set_value (env->package_sym, NULL, 0, env, outcome);

      call->obj = NULL;
      out = read_object (&call->obj, 0, input, size, stream, preserve_whitespace,
			 ends_with_eof, env, outcome, &obj_b, macro_end);

      set_value (env->package_sym, prevpack, 0, env, outcome);

      if (out == UNCLOSED_EMPTY_LIST)
	call->is_empty_list = 1;

      if (IS_INCOMPLETE_OBJECT (out))
	return INCOMPLETE_SHARP_MACRO_CALL;

      return out;
    }

  call->obj = NULL;
  out = read_object (&call->obj, 0, input, size, stream, preserve_whitespace,
		     ends_with_eof, env, outcome, &obj_b, macro_end);

  if (out == UNCLOSED_EMPTY_LIST)
    call->is_empty_list = 1;

  if (IS_INCOMPLETE_OBJECT (out))
    return INCOMPLETE_SHARP_MACRO_CALL;

  return out;
}


struct object *
call_sharp_macro (struct sharp_macro_call *macro_call, struct environment *env,
		  struct outcome *outcome)
{
  struct object *obj = macro_call->obj, *ret;
  struct symbol_name *s;
  int l;

  if (macro_call->dispatch_ch == '\'')
    {
      ret = alloc_empty_cons_pair ();

      ret->value_ptr.cons_pair->car = env->function_sym;
      add_reference (ret, env->function_sym, 0);

      ret->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
      ret->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr = &nil_object;

      ret->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = obj;
      add_reference (ret->value_ptr.cons_pair->cdr, obj, 0);

      return ret;
    }
  else if (macro_call->dispatch_ch == '\\')
    {
      if (obj->type != TYPE_SYMBOL_NAME
	  || obj->value_ptr.symbol_name->packname_present)
	{
	  outcome->type = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

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
	  outcome->type = UNKNOWN_CHARACTER_NAME;

	  return NULL;
	}
    }
  else if (macro_call->dispatch_ch == '.')
    {
      ret = evaluate_object (obj, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      return ret;
    }
  else if (macro_call->dispatch_ch == 'p' || macro_call->dispatch_ch == 'P')
    {
      if (obj->type != TYPE_STRING)
	{
	  outcome->type = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

	  return NULL;
	}

      return create_filename (obj);
    }
  else if (macro_call->dispatch_ch == '(')
    return create_vector_from_list (obj);
  else if (macro_call->dispatch_ch == ':')
    {
      if (!obj)
	{
	  outcome->type = PACKAGE_MARKER_IN_SHARP_COLON;

	  return NULL;
	}

      if (obj->type != TYPE_SYMBOL_NAME)
	{
	  outcome->type = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

	  return NULL;
	}

      if (obj->value_ptr.symbol_name->packname_present)
	{
	  outcome->type = PACKAGE_MARKER_IN_SHARP_COLON;

	  return NULL;
	}

      increment_refcount (obj);
      return obj;
    }
  else if (macro_call->dispatch_ch == 'c' || macro_call->dispatch_ch == 'C')
    {
      if (obj->type != TYPE_CONS_PAIR)
	{
	  outcome->type = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	  return NULL;
	}

      l = list_length (obj);

      if (l != 2)
	{
	  outcome->type = WRONG_SYNTAX_IN_SHARP_MACRO_FOR_COMPLEX;
	  return NULL;
	}

      return create_complex (CAR (obj), CAR (CDR (obj)), 0, env, outcome);
    }
  else if (macro_call->dispatch_ch == '+' || macro_call->dispatch_ch == '-')
    {
      if (macro_call->feat_test_result == (macro_call->dispatch_ch == '+'))
	{
	  increment_refcount (obj);
	  return obj;
	}
      else
	{
	  outcome->type = SKIPPED_OBJECT;
	  return NULL;
	}
    }
  else if (strchr ("*bBoOxXrR", macro_call->dispatch_ch))
    {
      increment_refcount (obj);
      return obj;
    }

  return NULL;
}


enum outcome_type
skip_without_reading (enum outcome_type type, int backts_commas_balance,
		      const char *input, size_t size, FILE *stream,
		      int preserve_whitespace, int ends_with_eof,
		      struct environment *env, struct outcome *outc,
		      int *list_depth, const char **obj_begin,
		      const char **obj_end)
{
  int found_prefix = 0;
  enum outcome_type out = NO_OBJECT;
  unsigned char ch;


  if (type == NO_OBJECT)
    {
      do
	{
	  if (!next_nonspace_char (&ch, &input, &size, stream))
	    return out;

	  if (ch == ';')
	    {
	      if (!jump_to_end_of_line (&input, &size, stream))
		break;
	    }
	  else if (ch == '#')
	    {
	      if (!next_char (&ch, &input, &size, stream))
		break;

	      if (ch == '|')
		{
		  outc->multiline_comment_depth = 1;

		  if (!jump_to_end_of_multiline_comment (&input, &size, stream,
							 &outc->
							 multiline_comment_depth))
		    break;
		}
	      else
		{
		  if (input)
		    *obj_begin = input;

		  if (ch == '\\')
		    {
		      outc->single_escape = 1;

		      out = skip_without_reading (INCOMPLETE_SYMBOL_NAME,
						  backts_commas_balance,
						  input, size, stream,
						  preserve_whitespace,
						  ends_with_eof, env, outc,
						  list_depth, obj_begin,
						  obj_end);
		    }
		  else if (ch == '(')
		    {
		      (*list_depth)++;
		    }
		}
	    }
	  else if (ch == '\'' || ch == '`' || ch == ',')
	    {
	      found_prefix = 1;

	      while (next_char (&ch, &input, &size, stream))
		{
		  if (ch != '\'' && ch != '`' && ch != ',' && ch != '@'
		      && ch != '.')
		    {
		      unget_char (ch, &input, &size, stream);

		      out = skip_without_reading (NO_OBJECT,
						  backts_commas_balance, input,
						  size, stream,
						  preserve_whitespace,
						  ends_with_eof, env, outc,
						  list_depth, obj_begin,
						  obj_end);

		      break;
		    }
		}
	    }
	  else if (ch == ')')
	    {
	      if (input)
		*obj_end = input-1;

	      if (*list_depth)
		{
		  (*list_depth)--;

		  if (!*list_depth)
		    {
		      out = COMPLETE_OBJECT;
		    }
		}
	      else
		{
		  out = found_prefix ? CLOSING_PARENTHESIS_AFTER_PREFIX :
		    CLOSING_PARENTHESIS;

		  break;
		}
	    }
	  else if (ch == '(')
	    {
	      if (input)
		*obj_begin = input;

	      (*list_depth)++;
	    }
	  else if (ch == '"')
	    {
	      if (input)
		*obj_begin = input;

	      out = skip_without_reading (INCOMPLETE_STRING,
					  backts_commas_balance, input, size,
					  stream, preserve_whitespace,
					  ends_with_eof, env, outc, list_depth,
					  obj_begin, obj_end);
	    }
	  else
	    {
	      if (input)
		*obj_begin = input;

	      out = skip_without_reading (INCOMPLETE_SYMBOL_NAME,
					  backts_commas_balance, input, size,
					  stream, preserve_whitespace,
					  ends_with_eof, env, outc, list_depth,
					  obj_begin, obj_end);
	    }

	} while (!IS_READ_OR_EVAL_ERROR (out) && !IS_INCOMPLETE_OBJECT (out)
		 && *list_depth);

      return out;
    }
  else if (type == INCOMPLETE_STRING)
    {
      while (next_char (&ch, &input, &size, stream))
	{
	  if (ch == '"' && !outc->single_escape)
	    {
	      if (input)
		*obj_end = input - 1;

	      return COMPLETE_OBJECT;
	    }

	  if (ch == '\\' && !outc->single_escape)
	    outc->single_escape = 1;
	  else
	    outc->single_escape = 0;
	}

      return INCOMPLETE_STRING;
    }
  else if (type == INCOMPLETE_SYMBOL_NAME)
    {
      while (next_char (&ch, &input, &size, stream))
	{
	  if (ch == '\\')
	    {
	      outc->single_escape = !outc->single_escape;
	    }
	  else if (ch == '|' && !outc->single_escape)
	    {
	      outc->multiple_escape = !outc->multiple_escape;
	    }
	  else if ((isspace ((unsigned char)ch)
		    || strchr (TERMINATING_MACRO_CHARS, (unsigned char)ch))
		   && !outc->single_escape && !outc->multiple_escape)
	    {
	      if (input)
		*obj_end = input - 2;

	      return COMPLETE_OBJECT;
	    }
	}

      return INCOMPLETE_SYMBOL_NAME;
    }

  return COMPLETE_OBJECT;
}


int
does_token_begin (const char *input, size_t size, FILE *stream)
{
  unsigned char ch;

  next_char (&ch, &input, &size, stream);

  if (!input)
    ungetc (ch, stream);

  if (isspace (ch) || strchr (TERMINATING_MACRO_CHARS, ch))
    return 0;

  return 1;
}


char *
accumulate_token (FILE *stream, int preserve_whitespace, int *token_size,
		  int *token_length, struct outcome *out)
{
  unsigned char ch;
  char *outbuf;

  *token_size = 16;
  outbuf = malloc_and_check (*token_size);
  *token_length = 0;

  while ((ch = fgetc (stream)) != EOF)
    {
      if (ch == '\\')
	{
	  out->single_escape = !out->single_escape;
	}
      else if (ch == '|' && !out->single_escape)
	{
	  out->multiple_escape = !out->multiple_escape;
	}
      else
	{
	  if ((isspace ((unsigned char)ch)
	       || strchr (TERMINATING_MACRO_CHARS, ch))
	      && !out->single_escape && !out->multiple_escape)
	    {
	      if (preserve_whitespace || strchr (TERMINATING_MACRO_CHARS, ch))
		ungetc (ch, stream);

	      return outbuf;
	    }

	  out->single_escape = 0;
	}

      if (*token_length == *token_size)
	{
	  *token_size <<= 1;
	  outbuf = realloc_and_check (outbuf, *token_size);
	}

      outbuf [(*token_length)++] = ch;
    }

  return outbuf;
}


int
get_read_base (struct environment *env)
{
  struct object *b = inspect_variable (env->read_base_sym, env);

  return mpz_get_si (b->value_ptr.integer);
}


int
get_print_base (struct environment *env)
{
  struct object *b = inspect_variable (env->print_base_sym, env);

  return mpz_get_si (b->value_ptr.integer);
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
      else if (isspace ((unsigned char)token [i])
	       || strchr (TERMINATING_MACRO_CHARS, token [i]))
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
  struct object *obj = alloc_object ();

  obj->type = numtype;

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
      obj->value_ptr.floating = malloc_and_check
	(sizeof (*obj->value_ptr.floating));
      *obj->value_ptr.floating = 0;
    }

  return obj;
}


struct object *
create_number (const char *token, size_t size, size_t exp_marker_pos, int radix,
	       enum object_type numtype)
{
  struct object *obj = alloc_object ();
  char *buf = malloc_and_check (size + 1);
  mpf_t t;

  obj->type = numtype;

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

      obj = convert_to_integer_if_possible (obj);
    }
  else if (numtype == TYPE_FLOAT)
    {
      if (exp_marker_pos > 0)
	buf [exp_marker_pos] = 'e';

      mpf_init (t);
      mpf_set_str (t, buf, radix);

      obj->value_ptr.floating = malloc_and_check
	(sizeof (*obj->value_ptr.floating));
      *obj->value_ptr.floating = mpf_get_d (t);

      mpf_clear (t);
    }

  free (buf);

  return obj;
}


struct object *
alloc_complex (void)
{
  struct object *ret = alloc_object ();

  ret->type = TYPE_COMPLEX;
  ret->value_ptr.complex = malloc_and_check (sizeof (*ret->value_ptr.complex));

  return ret;
}


struct object *
create_complex (struct object *real, struct object *imag, int steal_refs,
		struct environment *env, struct outcome *outcome)
{
  struct object *ret, *r, *i;
  enum object_type t;

  if (!IS_REAL (real) || (imag && !IS_REAL (imag)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (IS_RATIONAL (real) && (!imag || ((imag->type == TYPE_INTEGER
					&& !mpz_sgn (imag->value_ptr.integer))
				       || (imag->type == TYPE_RATIO
					   && !mpq_sgn (imag->value_ptr.ratio)))))
    {
      if (!steal_refs)
	increment_refcount (real);

      return real;
    }

  if (!imag)
    {
      ret = alloc_complex ();

      if (!steal_refs)
	increment_refcount (real);

      ret->value_ptr.complex->real = real;
      ret->value_ptr.complex->imag = create_floating_from_double (0.0);

      return ret;
    }

  t = highest_num_type (real->type, imag->type);
  r = promote_number (real, t);
  i = promote_number (imag, t);

  if (steal_refs)
    {
      decrement_refcount (real);
      decrement_refcount (imag);
    }

  ret = alloc_complex ();
  ret->value_ptr.complex->real = convert_to_integer_if_possible (r);
  ret->value_ptr.complex->imag = convert_to_integer_if_possible (i);

  return ret;
}


struct object *
create_integer_from_long (long num)
{
  struct object *obj = alloc_object ();

  obj->type = TYPE_INTEGER;

  mpz_init (obj->value_ptr.integer);
  mpz_set_si (obj->value_ptr.integer, num);

  return obj;
}


struct object *
create_integer_from_unsigned_long (unsigned long num)
{
  struct object *obj = alloc_object ();

  obj->type = TYPE_INTEGER;

  mpz_init (obj->value_ptr.integer);
  mpz_set_ui (obj->value_ptr.integer, num);

  return obj;
}


struct object *
convert_to_integer_if_possible (struct object *rat)
{
  mpz_t den;
  struct object *obj;

  if (rat->type != TYPE_RATIO)
    return rat;

  mpz_init (den);
  mpq_get_den (den, rat->value_ptr.ratio);

  if (!mpz_cmp (den, integer_one))
    {
      obj = alloc_object ();
      obj->type = TYPE_INTEGER;
      mpz_init (obj->value_ptr.integer);
      mpz_set (obj->value_ptr.integer, mpq_numref (rat->value_ptr.ratio));

      mpz_clear (den);
      decrement_refcount (rat);
      return obj;
    }

  mpz_clear (den);

  return rat;
}


struct object *
create_ratio_from_longs (long num, long den)
{
  struct object *obj = alloc_object ();

  obj->type = TYPE_RATIO;

  mpq_init (obj->value_ptr.ratio);
  mpq_set_si (obj->value_ptr.ratio, num, den);
  mpq_canonicalize (obj->value_ptr.ratio);

  return obj;
}


struct object *
create_floating_from_double (double d)
{
  struct object *obj = alloc_object ();

  obj->type = TYPE_FLOAT;

  obj->value_ptr.floating = malloc_and_check (sizeof (*obj->value_ptr.floating));
  *obj->value_ptr.floating = d;

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
	       && (isspace ((unsigned char)input [i])
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
      if (IS_LOWEST_BYTE_IN_UTF8 (*string))
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
      if (IS_LOWEST_BYTE_IN_UTF8 (str [off]))
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
      if (IS_LOWEST_BYTE_IN_UTF8 (str [i]))
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
      fprintf (stderr, "could not allocate %lu bytes.  Exiting...\n", size);
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
      fprintf (stderr, "could not reallocate to %lu bytes.  Exiting...\n", size);
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
      fprintf (stderr, "could not allocate %lu elements of %lu bytes each.  "
	       "Exiting...\n", nmemb, size);
      exit (1);
    }

  return mem;
}


struct object *
alloc_object (void)
{
  struct object *obj = malloc_and_check (sizeof (*obj));

  obj->refcount1 = 0;
  obj->refcount2 = 1;
  obj->flags = 0;

  return obj;
}


struct object *
alloc_prefix (unsigned char pr)
{
  struct object *obj = alloc_object ();

  switch (pr)
    {
    case '`':
      obj->type = TYPE_BACKQUOTE;
      break;
    case ',':
      obj->type = TYPE_COMMA;
      break;
    case '@':
      obj->type = TYPE_AT;
      break;
    case '.':
      obj->type = TYPE_DOT;
      break;
    default:
      break;
    }

  obj->value_ptr.next = NULL;

  return obj;
}


struct object *
alloc_empty_cons_pair (void)
{
  struct object *obj = alloc_object ();
  struct cons_pair *cons = malloc_and_check (sizeof (*cons));

  cons->filling_car = 0;
  cons->empty_list_in_car = 0;
  cons->found_dot = 0;
  cons->filling_cdr = 0;
  cons->empty_list_in_cdr = 0;
  cons->car = NULL;
  cons->cdr = NULL;

  obj->type = TYPE_CONS_PAIR;
  obj->value_ptr.cons_pair = cons;

  return obj;
}


struct object *
alloc_empty_list (size_t sz)
{
  struct object *ret, *cons;

  ret = cons = alloc_empty_cons_pair ();

  for (sz--; sz; sz--)
    {
      cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
      set_reference_strength_factor (cons, 1, CDR (cons), 0, 0, 0);
      cons = CDR (cons);
    }

  cons->value_ptr.cons_pair->cdr = &nil_object;

  return ret;
}


struct object *
alloc_function (void)
{
  struct object *obj = alloc_object ();
  struct function *fun = malloc_and_check (sizeof (*fun));

  fun->name = NULL;
  fun->is_setf_func = 0;
  fun->is_special_operator = 0;

  fun->lambda_list = NULL;
  fun->allow_other_keys = 0;
  fun->lex_vars = NULL;
  fun->lex_funcs = NULL;
  fun->body = NULL;
  fun->is_generic = 0;
  fun->methods = NULL;
  fun->builtin_form = NULL;
  fun->struct_constructor_class = NULL;
  fun->struct_accessor_class = NULL;
  fun->struct_predicate_class = NULL;
  fun->struct_copyier_class = NULL;
  fun->condition_reader_class = NULL;
  fun->function_macro = NULL;
  fun->macro_function = NULL;

  obj->type = TYPE_FUNCTION;
  obj->value_ptr.function = fun;

  return obj;
}


struct object *
alloc_sharp_macro_call (void)
{
  struct object *obj = alloc_object ();
  struct sharp_macro_call *call = malloc_and_check (sizeof (*call));

  obj->type = TYPE_SHARP_MACRO_CALL;
  obj->value_ptr.sharp_macro_call = call;

  call->feat_test_incomplete = 0;
  call->feat_test_is_empty_list = 0;

  call->is_empty_list = 0;

  call->feature_test = NULL;
  call->obj = NULL;

  return obj;
}


struct object *
alloc_bytespec (void)
{
  struct object *obj = alloc_object ();
  struct bytespec *bs = malloc_and_check (sizeof (*bs));

  obj->type = TYPE_BYTESPEC;
  obj->value_ptr.bytespec = bs;

  mpz_init (bs->size);
  mpz_init (bs->pos);

  return obj;
}


struct object_list *
alloc_empty_object_list (size_t sz)
{
  struct object_list *ret = NULL, *last;

  while (sz)
    {
      if (ret)
	last = last->next = malloc_and_check (sizeof (*ret));
      else
	last = ret = malloc_and_check (sizeof (*ret));

      sz--;
    }

  if (ret)
    last->next = NULL;

  return ret;
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
  struct object *obj = alloc_object ();
  struct package *pack = malloc_and_check (sizeof (*pack));

  pack->name = malloc_and_check (name_len);
  memcpy (pack->name, name, name_len);

  pack->name_len = name_len;
  pack->nicks = NULL;
  pack->uses = NULL;
  pack->used_by = NULL;
  pack->symtable = alloc_empty_symtable (SYMTABLE_SIZE);

  obj->type = TYPE_PACKAGE;
  obj->value_ptr.package = pack;

  return obj;
}


struct object *
create_package_from_c_strings (char *name, ...)
{
  struct object *obj = alloc_object ();
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
inspect_accessible_symbol (const struct object *sym, const struct object *pack,
			   int *is_present)
{
  int ind = hash_char_vector (sym->value_ptr.symbol->name,
			      sym->value_ptr.symbol->name_len, SYMTABLE_SIZE);
  struct package_record *cur = pack->value_ptr.package->symtable [ind];
  struct object_list *uses;

  while (cur)
    {
      if (sym == cur->sym)
	{
	  *is_present = 1;
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
	  if (cur->visibility == EXTERNAL_VISIBILITY && sym == cur->sym)
	    {
	      *is_present = 0;
	      return cur;
	    }

	  cur = cur->next;
	}

      uses = uses->next;
    }

  return NULL;
}


struct package_record *
inspect_accessible_symbol_by_name (char *name, size_t len, struct object *pack,
				   int *is_present)
{
  int ind = hash_char_vector (name, len, SYMTABLE_SIZE);
  struct package_record *cur = pack->value_ptr.package->symtable [ind];
  struct object_list *uses;

  while (cur)
    {
      if (eqmem (cur->sym->value_ptr.symbol->name,
		 cur->sym->value_ptr.symbol->name_len, name, len))
	{
	  *is_present = 1;
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
	      *is_present = 0;
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
find_package_entry (const struct object *symbol,
		    struct package_record **symtable,
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
is_external_in_home_package (const struct object *sym)
{
  struct package_record *prev,
    *entry = find_package_entry (sym,
				 sym->value_ptr.symbol->home_package->
				 value_ptr.package->symtable, &prev);

  return entry->visibility == EXTERNAL_VISIBILITY;
}


int
import_symbol (struct object *sym, struct object *pack,
	       struct package_record **rec)
{
  int pres, ind;
  struct package_record *r, *new_cell;

  if ((r = inspect_accessible_symbol_by_name (sym->value_ptr.symbol->name,
					      sym->value_ptr.symbol->name_len,
					      pack, &pres))
      && pres && r->sym == sym)
    {
      if (rec)
	*rec = r;

      return 1;
    }

  if (!r || r->sym == sym)
    {
      ind = hash_char_vector (sym->value_ptr.symbol->name,
			      sym->value_ptr.symbol->name_len, SYMTABLE_SIZE);

      new_cell = malloc_and_check (sizeof (*new_cell));
      new_cell->visibility = INTERNAL_VISIBILITY;
      new_cell->sym = sym;
      new_cell->next = pack->value_ptr.package->symtable [ind];

      increment_refcount (sym);

      if (!sym->value_ptr.symbol->home_package)
	sym->value_ptr.symbol->home_package = pack;

      pack->value_ptr.package->symtable [ind] = new_cell;

      if (rec)
	*rec = new_cell;

      return 1;
    }

  return 0;
}


int
use_package (struct object *used, struct object *pack,
	     struct object **conflicting)
{
  struct object_list *p = pack->value_ptr.package->uses;
  struct package_record *r;
  size_t i;
  int pres;

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
	      && inspect_accessible_symbol_by_name (r->sym->value_ptr.symbol->
						    name, r->sym->
						    value_ptr.symbol->name_len,
						    pack, &pres))
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
unuse_package (struct object *used, struct object *pack)
{
  struct object_list *p = pack->value_ptr.package->uses, *prev = NULL;

  while (p)
    {
      if (p->obj == used)
	{
	  if (prev)
	    {
	      prev->next = p->next;
	    }
	  else
	    {
	      pack->value_ptr.package->uses = p->next;
	    }

	  return 1;
	}

      prev = p;
      p = p->next;
    }

  return 0;
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
hash_number (const struct object *num, size_t table_size)
{
  mpz_t numer, denom;

  if (num->type == TYPE_INTEGER)
    return mpz_get_si (num->value_ptr.integer) % table_size;
  else if (num->type == TYPE_RATIO)
    {
      mpq_get_num (numer, num->value_ptr.ratio);
      mpq_get_den (denom, num->value_ptr.ratio);

      return (mpz_get_si (numer) + mpz_get_si (denom)) % table_size;
    }
  else if (num->type == TYPE_FLOAT)
    return (long) num->value_ptr.floating % table_size;  /* FIXME */
  else
    return (hash_number (num->value_ptr.complex->real, table_size)
	    + hash_number (num->value_ptr.complex->imag, table_size))
      % table_size;
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


struct hashtable_record *
find_hashtable_record (struct object *obj, const struct object *tbl,
		       int *ind, int *j, struct hashtable_record **prev)
{
  enum hashtable_type t = tbl->value_ptr.hashtable->type;
  struct hashtable_record *r;

  switch (t)
    {
    case HT_EQ:
      *ind = hash_object_respecting_eq (obj, LISP_HASHTABLE_SIZE);
      break;
    case HT_EQL:
      *ind = hash_object_respecting_eql (obj, LISP_HASHTABLE_SIZE);
      break;
    case HT_EQUAL:
      *ind = hash_object_respecting_equal (obj, LISP_HASHTABLE_SIZE);
      break;
    case HT_EQUALP:
      *ind = hash_object_respecting_equalp (obj, LISP_HASHTABLE_SIZE);
      break;
    default:
      break;
    }

  r = tbl->value_ptr.hashtable->table [*ind];

  if (j)
    *j = 0;

  if (prev)
    *prev = NULL;

  while (r)
    {
      switch (t)
	{
	case HT_EQ:
	  if (eq_objects (obj, r->key) == &t_object)
	    return r;
	  break;
	case HT_EQL:
	  if (eql_objects (obj, r->key) == &t_object)
	    return r;
	  break;
	case HT_EQUAL:
	  if (equal_objects (obj, r->key) == &t_object)
	    return r;
	  break;
	case HT_EQUALP:
	  if (equalp_objects (obj, r->key) == &t_object)
	    return r;
	  break;
	default:
	  break;
	}

      if (prev)
	*prev = r;

      if (j)
	(*j)++;

      r = r->next;
    }

  return NULL;
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
capture_lexical_environment (struct binding **lex_vars,
			     struct binding **lex_funcs, struct binding *vars,
			     int var_num, struct binding *funcs, int func_num)
{
  struct binding *bin, *b;

  *lex_vars = NULL;
  *lex_funcs = NULL;

  while (vars && var_num)
    {
      if (vars->type == LEXICAL_BINDING)
	{
	  b = vars;

	  while (!b->sym)
	    {
	      b = b->closure_bin;
	    }

	  if (!*lex_vars)
	    *lex_vars = bin = malloc_and_check (sizeof (*bin));
	  else
	    bin = bin->next = malloc_and_check (sizeof (*bin));


	  bin->type = LEXICAL_BINDING;
	  bin->refcount = 0;
	  bin->sym = NULL;
	  bin->obj = NULL;
	  bin->closure_bin = b;
	  bin->next = NULL;

	  b->refcount++;
	}

      vars = vars->next;

      if (var_num > 0)
	var_num--;
    }


  while (funcs && func_num)
    {
      if (funcs->type == LEXICAL_BINDING)
	{
	  b = funcs;

	  while (!b->sym)
	    {
	      b = b->closure_bin;
	    }

	  if (!*lex_funcs)
	    *lex_funcs = bin = malloc_and_check (sizeof (*bin));
	  else
	    bin = bin->next = malloc_and_check (sizeof (*bin));


	  bin->type = LEXICAL_BINDING;
	  bin->refcount = 0;
	  bin->sym = NULL;
	  bin->obj = NULL;
	  bin->closure_bin = b;
	  bin->next = NULL;

	  increment_refcount (b->sym);
	  increment_refcount (b->obj);
	  b->refcount++;
	}

      funcs = funcs->next;

      if (func_num > 0)
	func_num--;
    }
}


struct object *
create_function (struct object *lambda_list, struct object *body,
		 struct environment *env, struct outcome *outcome, int is_macro,
		 int allow_destructuring)
{
  struct object *fun = alloc_function ();
  struct function *f = fun->value_ptr.function;

  outcome->type = EVAL_OK;
  f->lambda_list = parse_lambda_list (lambda_list, allow_destructuring, 0, env,
				      outcome, &f->allow_other_keys);

  if (outcome->type != EVAL_OK)
    {
      free_function_or_macro (fun);

      return NULL;
    }

  if (!is_macro)
    {
      capture_lexical_environment (&f->lex_vars, &f->lex_funcs, env->vars,
				   env->lex_env_vars_boundary, env->funcs,
				   env->lex_env_funcs_boundary);
    }

  f->body = body;
  add_reference (fun, body, 1);

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
alloc_string (fixnum size)
{
  struct object *obj = alloc_object ();

  obj->type = TYPE_STRING;

  obj->value_ptr.string = malloc_and_check (sizeof (*obj->value_ptr.string));

  obj->value_ptr.string->value = malloc_and_check (size);
  obj->value_ptr.string->alloc_size = size;
  obj->value_ptr.string->used_size = 0;
  obj->value_ptr.string->fill_pointer = -1;

  return obj;
}


struct object *
create_string_copying_char_vector (const char *str, fixnum size)
{
  fixnum i;
  struct object *ret;

  ret = alloc_string (size);

  for (i = 0; i < size; i++)
    ret->value_ptr.string->value [i] = str [i];

  ret->value_ptr.string->used_size = size;

  return ret;
}


struct object *
create_string_with_char_vector (char *str, fixnum size)
{
  struct object *ret;

  ret = alloc_object ();
  ret->type = TYPE_STRING;
  ret->value_ptr.string = malloc_and_check (sizeof (*ret->value_ptr.string));

  ret->value_ptr.string->value = str;
  ret->value_ptr.string->alloc_size = size;

  ret->value_ptr.string->used_size = size;

  return ret;
}


struct object *
create_string_copying_c_string (char *str)
{
  return create_string_copying_char_vector (str, strlen (str));
}


void
resize_string_allocation (struct object *string, fixnum size)
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


char *
concatenate_char_vectors (size_t totsize, ...)
{
  va_list valist;
  char *s, *ret = malloc_and_check (totsize);
  size_t sz, i = 0;

  va_start (valist, totsize);

  while ((s = va_arg (valist, char *)))
    {
      sz = va_arg (valist, size_t);

      memcpy (ret+i, s, sz);

      i += sz;
    }

  va_end (valist);

  return ret;
}


struct object *
alloc_symbol_name (size_t value_s, size_t actual_symname_s)
{
  struct object *obj = alloc_object ();
  struct symbol_name *s;

  obj->type = TYPE_SYMBOL_NAME;

  obj->value_ptr.symbol_name =
    malloc_and_check (sizeof (*obj->value_ptr.symbol_name));
  s = obj->value_ptr.symbol_name;

  s->value = malloc_and_check (value_s);
  s->alloc_size = value_s;
  s->used_size = 0;

  s->packname_present = 0;

  if (actual_symname_s)
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

  if (actual_symname_s)
    s->actual_symname = realloc_and_check (s->actual_symname, actual_symname_s);

  if (actual_symname_s < s->actual_symname_used_s)
    s->actual_symname_used_s = actual_symname_s;

  s->actual_symname_alloc_s = actual_symname_s;
}


const char *
find_end_of_symbol_name (const char *input, size_t size, int ends_with_eof,
			 int preserve_whitespace, int already_begun,
			 int found_package_sep,
			 const char **start_of_package_separator,
			 enum package_record_visibility *sym_visibility,
			 size_t *name_length, size_t *act_name_length,
			 struct outcome *out)
{
  size_t i = 0;
  int just_dots = 1, colons = 0;
  size_t **length;

  *start_of_package_separator = NULL;

  *name_length = 0, *act_name_length = 0;

  length = found_package_sep ? &act_name_length : &name_length;

  while (i < size)
    {
      if (input [i] == '\\')
	{
	  just_dots = 0;

	  if (!out->single_escape)
	    {
	      out->single_escape = 1;
	    }
	  else
	    {
	      out->single_escape = 0;
	      (**length)++;
	    }
	}
      else if (input [i] == '|' && !out->single_escape)
	{
	  just_dots = 0;

	  out->multiple_escape = !out->multiple_escape;
	}
      else if (input [i] == ':' && !out->single_escape && !out->multiple_escape)
	{
	  if (found_package_sep
	      || (*start_of_package_separator
		  && (input + i > *start_of_package_separator + colons)))
	    {
	      out->type = MORE_THAN_A_PACKAGE_SEPARATOR;

	      return NULL;
	    }

	  if (colons == 1 && (i == 1))
	    {
	      out->type = CANT_BEGIN_WITH_TWO_COLONS_OR_MORE;

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
	      out->type = TOO_MANY_COLONS;

	      return NULL;
	    }
	  else if (colons == 2)
	    {
	      *sym_visibility = INTERNAL_VISIBILITY;
	    }
	}
      else
	{
	  if ((isspace ((unsigned char)input [i])
	       || strchr (TERMINATING_MACRO_CHARS, (unsigned char)input [i]))
	      && !out->single_escape && !out->multiple_escape)
	    {
	      if (!already_begun && just_dots && **length == 1)
		out->type = SINGLE_DOT;
	      else if (!already_begun && just_dots && **length)
		out->type = MULTIPLE_DOTS;
	      else if (*start_of_package_separator
		       && (input + i == *start_of_package_separator + colons))
		out->type = CANT_END_WITH_PACKAGE_SEPARATOR;

	      return input + i - (!isspace ((unsigned char)input [i])
				  || !!preserve_whitespace);
	    }

	  if (input [i] != '.')
	    just_dots = 0;

	  (**length)++;
	  out->single_escape = 0;
	}
      i++;
    }

  if (ends_with_eof)
    return input+size-1;

  return NULL;
}


void
copy_symname_with_case_conversion (char *output, const char *input, size_t size,
				   enum readtable_case read_case,
				   int single_escape, int multiple_escape)
{
  size_t i;
  int j;

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
      else if ((isspace ((unsigned char)input [i])
		|| strchr (TERMINATING_MACRO_CHARS, input [i]))
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

  obj = alloc_object ();
  obj->type = TYPE_SYMBOL;

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
  sym->is_const = 0;
  sym->is_parameter = 0;
  sym->is_special = 0;
  sym->value_dyn_bins_num = 0;
  sym->value_cell = NULL;
  sym->function_dyn_bins_num = 0;
  sym->function_cell = NULL;
  sym->plist = &nil_object;
  sym->setf_expander = NULL;
  sym->home_package = NULL;
  sym->setf_func_dyn_bins_num = 0;
  sym->setf_func_cell = NULL;

  obj->value_ptr.symbol = sym;

  return obj;
}


struct object *
create_filename (struct object *string)
{
  struct object *obj = alloc_object ();
  struct filename *fn = malloc_and_check (sizeof (*fn));

  obj->type = TYPE_FILENAME;

  obj->value_ptr.filename = fn;

  fn->value = string;
  add_reference (obj, string, 0);

  return obj;
}


struct object *
alloc_vector (fixnum size, int fill_with_nil, int dont_store_size)
{
  struct object *obj = alloc_object ();
  struct array *vec = malloc_and_check (sizeof (*vec));
  struct array_size *sz;
  fixnum i;

  if (!dont_store_size)
    {
      sz = malloc_and_check (sizeof (*sz));
      sz->size = size;
      sz->next = NULL;
      vec->alloc_size = sz;
    }

  vec->fill_pointer = -1;
  vec->value = calloc_and_check (size, sizeof (*vec->value));
  vec->reference_strength_factor = malloc_and_check (size * sizeof (int));

  if (fill_with_nil)
    {
      for (i = 0; i < size; i++)
	{
	  vec->value [i] = &nil_object;
	  vec->reference_strength_factor [i] = 0;
	}
    }

  obj->type = TYPE_ARRAY;
  obj->value_ptr.array = vec;

  return obj;
}


struct object *
create_vector_from_list (struct object *list)
{
  struct object *obj = alloc_object ();
  struct array *vec = malloc_and_check (sizeof (*vec));
  struct array_size *sz = malloc_and_check (sizeof (*sz));
  fixnum i;

  obj->type = TYPE_ARRAY;

  sz->size = list_length (list);
  sz->next = NULL;

  vec->alloc_size = sz;
  vec->fill_pointer = -1;
  vec->value = calloc_and_check (sz->size, sizeof (*vec->value));
  vec->reference_strength_factor = calloc_and_check (sz->size, sizeof (int));

  obj->value_ptr.array = vec;

  for (i = 0; i < sz->size; i++)
    {
      vec->value [i] = nth (i, list);
      add_reference (obj, vec->value [i], i);
    }

  return obj;
}


struct object *
create_bitvector_from_char_vector (const char *in, size_t sz, size_t req_size)
{
  struct object *ret = alloc_object ();
  size_t i;
  char l;

  ret->type = TYPE_BITARRAY;
  ret->value_ptr.bitarray = malloc_and_check (sizeof (*ret->value_ptr.bitarray));

  ret->value_ptr.bitarray->alloc_size =
    malloc_and_check (sizeof (*ret->value_ptr.bitarray->alloc_size));
  ret->value_ptr.bitarray->alloc_size->next = NULL;
  ret->value_ptr.bitarray->fill_pointer = -1;

  mpz_init (ret->value_ptr.bitarray->value);

  for (i = 0; i < sz; i++)
    {
      if (in [i] == '1')
	mpz_setbit (ret->value_ptr.bitarray->value, i);
      else if (in [i] == '0')
	mpz_clrbit (ret->value_ptr.bitarray->value, i);
      else
	break;
    }

  if (req_size)
    {
      if (in [i-1] == '0' || in [i-1] == '1')
	l = in [i-1];
      else
	l = '0';

      for (; i < req_size; i++)
	{
	  if (l == '1')
	    mpz_setbit (ret->value_ptr.bitarray->value, i);
	  else if (l == '0')
	    mpz_clrbit (ret->value_ptr.bitarray->value, i);
	}
    }

  ret->value_ptr.bitarray->alloc_size->size = req_size ? req_size : i;

  return ret;
}


void
resize_vector (struct object *vector, fixnum size)
{
  vector->value_ptr.array->value =
    realloc_and_check (vector->value_ptr.array->value,
		       size * sizeof (*vector->value_ptr.array->value));

  vector->value_ptr.array->alloc_size->size = size;
}


struct object *
create_character (char *character, int do_copy)
{
  struct object *obj = alloc_object ();

  obj->type = TYPE_CHARACTER;

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
      if (IS_LOWEST_BYTE_IN_UTF8 (character [sz]))
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


fixnum
get_nth_character_offset (struct object *str, int ind)
{
  fixnum i;

  for (i = 0; i < str->value_ptr.string->used_size; i++)
    {
      if (!ind)
	return i;

      if (IS_LOWEST_BYTE_IN_UTF8 (str->value_ptr.string->value [i]))
	ind--;
    }

  return -1;
}


int
set_nth_character (struct object *str, int ind, char *ch)
{
  char *c = str->value_ptr.string->value;
  size_t s = str->value_ptr.string->used_size, off, csz, newsz;

  for (off = 0; ind; ind--)
    {
      off = next_utf8_char (c, s);

      if (!off)
	return 0;

      c += off;
      s -= off;
    }

  if (!(csz = next_utf8_char (c, s)))
    csz = str->value_ptr.string->value + str->value_ptr.string->used_size - c;

  if (csz == strlen (ch))
    memcpy (c, ch, strlen (ch));
  else
    {
      newsz = str->value_ptr.string->used_size + strlen (ch) - csz;

      if (newsz > str->value_ptr.string->alloc_size)
	{
	  str->value_ptr.string->value =
	    realloc_and_check (str->value_ptr.string->value, newsz);
	  str->value_ptr.string->alloc_size = newsz;
	}

      if (csz < strlen (ch))
	{
	  for (off = newsz-1;
	       off >= c - str->value_ptr.string->value + strlen (ch); off--)
	    {
	      str->value_ptr.string->value [off] =
		str->value_ptr.string->value [off-strlen(ch)+csz];
	    }
	}
      else
	{
	  for (off = c - str->value_ptr.string->value + strlen (ch);
	       off < str->value_ptr.string->used_size; off++)
	    {
	      str->value_ptr.string->value [off] =
		str->value_ptr.string->value [off-strlen(ch)+csz];
	    }
	}

      memcpy (c, ch, strlen (ch));
      str->value_ptr.string->used_size = newsz;
    }

  return 1;
}


struct object *
create_file_stream (enum stream_type type, enum stream_direction direction,
		    struct string *filename, struct outcome *outcome)
{
  struct object *obj = alloc_object ();
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
  obj->value_ptr.stream = str;

  return obj;
}


struct object *
create_stream_from_open_file (enum stream_type type,
			      enum stream_direction direction, FILE *file)
{
  struct object *obj = alloc_object ();
  struct stream *str = malloc_and_check (sizeof (*str));

  str->medium = FILE_STREAM;
  str->type = type;
  str->direction = direction;
  str->is_open = 1;
  str->dirty_line = 0;
  str->file = file;

  obj->type = TYPE_STREAM;
  obj->value_ptr.stream = str;

  return obj;
}


struct object *
create_string_stream (enum stream_direction direction, struct object *instr)
{
  struct object *obj = alloc_object ();
  struct stream *str = malloc_and_check (sizeof (*str));

  str->medium = STRING_STREAM;
  str->direction = direction;
  str->is_open = 1;
  str->dirty_line = 0;

  obj->type = TYPE_STREAM;
  obj->value_ptr.stream = str;

  if (direction == INPUT_STREAM)
    {
      add_reference (obj, instr, 0);
      str->string = instr;
    }
  else
    str->string = alloc_string (0);

  return obj;
}


struct structure_field_decl *
create_structure_field_decl (struct object *fieldform, struct environment *env,
			     struct outcome *outcome)
{
  struct object *name;
  struct structure_field_decl *ret;

  if (IS_SYMBOL (fieldform))
    name = SYMBOL (fieldform);
  else if (IS_LIST (fieldform) && IS_SYMBOL (CAR (fieldform)))
    name = SYMBOL (CAR (fieldform));
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = malloc_and_check (sizeof (*ret));

  ret->name = name;
  ret->initform = NULL;
  ret->next = NULL;

  return ret;
}


struct class_field_decl *
create_class_field_decl (struct object *fieldform, struct environment *env,
			 struct outcome *outcome)
{
  struct object *name;
  struct class_field_decl *ret;

  if (IS_SYMBOL (fieldform))
    name = SYMBOL (fieldform);
  else if (IS_LIST (fieldform) && IS_SYMBOL (CAR (fieldform)))
    name = SYMBOL (CAR (fieldform));
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = malloc_and_check (sizeof (*ret));

  ret->name = name;
  ret->next = NULL;

  return ret;
}


struct condition_field_decl *
create_condition_field_decl (struct object *fieldform, struct environment *env,
			     struct outcome *outcome)
{
  struct object *name;
  struct condition_field_decl *ret;

  if (IS_SYMBOL (fieldform))
    name = SYMBOL (fieldform);
  else if (IS_LIST (fieldform) && IS_SYMBOL (CAR (fieldform)))
    name = SYMBOL (CAR (fieldform));
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = malloc_and_check (sizeof (*ret));

  ret->name = name;
  ret->next = NULL;

  return ret;
}


void
create_object_fields (struct object *stdobj, struct object *class)
{
  struct class_field_decl *fd = class->value_ptr.standard_class->fields;
  struct object_list *p = class->value_ptr.standard_class->parents;
  struct class_field *f;

  while (fd)
    {
      f = malloc_and_check (sizeof (*f));

      f->name = fd->name;
      f->value = NULL;
      f->next = stdobj->value_ptr.standard_object->fields;
      stdobj->value_ptr.standard_object->fields = f;

      fd = fd->next;
    }

  while (p)
    {
      create_object_fields (stdobj, p->obj->value_ptr.symbol->typespec);

      p = p->next;
    }
}


void
create_condition_fields (struct object *stdobj, struct object *class)
{
  struct condition_field_decl *fd = class->value_ptr.condition_class->fields;
  struct object_list *p = class->value_ptr.condition_class->parents;
  struct condition_field *f;

  while (fd)
    {
      f = malloc_and_check (sizeof (*f));

      f->name = fd->name;
      f->value = &nil_object;
      f->next = stdobj->value_ptr.condition->fields;
      stdobj->value_ptr.condition->fields = f;

      fd = fd->next;
    }

  while (p)
    {
      create_condition_fields (stdobj, p->obj->value_ptr.symbol->typespec);

      p = p->next;
    }
}


struct object *
load_file (const char *filename, struct environment *env,
	   struct outcome *outcome)
{
  FILE *f;
  long l;
  char *buf;
  enum outcome_type out;
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
      fclose (f);
      outcome->type = COULD_NOT_SEEK_FILE;
      return NULL;
    }

  if ((l = ftell (f)) == -1)
    {
      fclose (f);
      outcome->type = COULD_NOT_TELL_FILE;
      return NULL;
    }

  if (fseek (f, 0l, SEEK_SET))
    {
      fclose (f);
      outcome->type = COULD_NOT_SEEK_FILE;
      return NULL;
    }

  buf = malloc_and_check (l);

  if (!fread (buf, l, 1, f))
    {
      free (buf);
      fclose (f);
      outcome->type = ERROR_READING_FILE;
      return NULL;
    }

  out = read_object (&obj, 0, buf, l, NULL, 0, 1, env, outcome, &obj_b, &obj_e);
  sz = l - (obj_e + 1 - buf);
  in = obj_e + 1;

  while (1)
    {
      if (out == COMPLETE_OBJECT || out == SKIPPED_OBJECT)
	{
	  if (out == COMPLETE_OBJECT)
	    {
	      res = evaluate_object (obj, env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!res)
		{
		  free (buf);
		  fclose (f);

		  return NULL;
		}

	      decrement_refcount (res);
	      decrement_refcount (obj);
	    }

	  obj = NULL;
	  out = read_object (&obj, 0, in, sz, NULL, 0, 1, env, outcome, &obj_b,
			     &obj_e);
	  sz = sz - (obj_e + 1 - in);
	  in = obj_e + 1;
	}
      else if (out == NO_OBJECT)
	{
	  free (buf);
	  fclose (f);
	  CLEAR_READER_STATUS (*outcome);

	  return &t_object;
	}
      else if (IS_READ_OR_EVAL_ERROR (out) || IS_INCOMPLETE_OBJECT (out))
	{
	  free (buf);
	  fclose (f);
	  CLEAR_READER_STATUS (*outcome);

	  if (IS_READ_OR_EVAL_ERROR (out))
	    outcome->type = out;
	  else
	    outcome->type = GOT_EOF_IN_MIDDLE_OF_OBJECT;

	  return NULL;
	}
    }
}


struct object *
intern_symbol_by_char_vector (char *name, size_t len, int do_copy,
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

	  return cur->sym;
	}

      cur = cur->next;
    }

  if (vis == INTERNAL_VISIBILITY)
    {
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
		  return cur->sym;
		}

	      cur = cur->next;
	    }

	  uses = uses->next;
	}
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
intern_symbol_name (struct object *symname, struct environment *env,
		    enum outcome_type *out)
{
  struct symbol_name *s = symname->value_ptr.symbol_name;
  struct object *pack = inspect_variable (env->package_sym, env);

  if (!pack)
    {
      s->sym = create_symbol (s->value, s->used_size, 1);
      s->sym->value_ptr.symbol->home_package = &nil_object;
      return s->sym;
    }


  if (s->packname_present || pack == env->keyword_package)
    {
      if (!s->used_size || pack == env->keyword_package)
	{
	  s->sym = intern_symbol_by_char_vector (!s->used_size
						 ? s->actual_symname
						 : s->value,
						 !s->used_size
						 ? s->actual_symname_used_s
						 : s->used_size, 1,
						 EXTERNAL_VISIBILITY, 1,
						 env->keyword_package);

	  s->sym->value_ptr.symbol->is_const = 1;
	  s->sym->value_ptr.symbol->value_cell = s->sym;
	  add_reference (symname, s->sym, 0);

	  return s->sym;
	}

      pack = find_package (s->value, s->used_size, env);

      if (!pack)
	{
	  *out = PACKAGE_NOT_FOUND_IN_READ;
	  return NULL;
	}
      else
	{
	  s->sym = intern_symbol_by_char_vector (s->actual_symname,
						 s->actual_symname_used_s, 1,
						 s->visibility, 0, pack);
	  if (!s->sym)
	    {
	      *out = SYMBOL_IS_NOT_EXTERNAL_IN_PACKAGE;
	      return NULL;
	    }

	  add_reference (symname, s->sym, 0);

	  return s->sym;
	}
    }

  s->sym = intern_symbol_by_char_vector (s->value, s->used_size, 1,
					 INTERNAL_VISIBILITY, 0, pack);
  add_reference (symname, s->sym, 0);

  return s->sym;
}


int
unintern_symbol (struct object *sym, struct object *pack)
{
  struct object *s = SYMBOL (sym);
  struct package_record *prev, *entry;

  entry = find_package_entry (s, pack->value_ptr.package->symtable, &prev);

  if (!entry)
    return 0;

  if (prev)
    prev->next = entry->next;
  else
    pack->value_ptr.package->symtable [hash_symbol (sym, SYMTABLE_SIZE)] =
      entry->next;

  free (entry);

  if (pack == s->value_ptr.symbol->home_package)
    s->value_ptr.symbol->home_package = &nil_object;

  return 1;
}


struct binding *
create_binding (struct object *sym, struct object *obj, enum binding_type type,
		int inc_refcs)
{
  struct binding *bin = malloc_and_check (sizeof (*bin));

  bin->type = type;
  bin->refcount = 1;
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
chain_bindings (struct binding *bin, struct binding *env, int *num,
		struct binding **last_bin)
{
  struct binding *last = bin, *b = bin;

  if (num)
    *num = 0;

  if (last_bin)
    *last_bin = NULL;

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

  if (last_bin)
    *last_bin = last;

  return bin;
}


struct binding *
remove_bindings (struct binding *env, int num)
{
  struct binding *b;

  if (!num)
    return env;  

  b = env->next;

  if (env->type == DYNAMIC_BINDING && !(env->type & DELETED_BINDING))
    env->sym->value_ptr.symbol->value_dyn_bins_num--;

  env->refcount--;

  if (!env->refcount)
    {
      decrement_refcount (env->sym);
      decrement_refcount (env->obj);
      free (env);
    }

  if (num == 1)
    return b;
  else
    return remove_bindings (b, num-1);
}


struct binding *
find_binding (struct symbol *sym, struct binding *bins, enum binding_type type,
	      int bin_num)
{
  while (bins && (type != LEXICAL_BINDING || bin_num))
    {
      if ((type == ANY_BINDING || bins->type & type)
	  && (bins->type == DYNAMIC_BINDING || bin_num)
	  && !(bins->type & DELETED_BINDING))
	{
	  if (!bins->sym && bins->closure_bin->sym->value_ptr.symbol == sym)
	    return bins->closure_bin;
	  else if (bins->sym && bins->sym->value_ptr.symbol == sym)
	    return bins;
	}

      bins = bins->next;

      if (bin_num >= 0)
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

      if (IS_SYMBOL (car) || car->type == TYPE_INTEGER)
	{
	  destfind = CDR (body);

	  while (SYMBOL (destfind) != &nil_object && (dest = CAR (destfind))
		 && (IS_SYMBOL (dest) || dest->type == TYPE_INTEGER))
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
	      || (tagname->type == TYPE_INTEGER
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


struct object *
create_condition (struct object *type, va_list valist)
{
  struct object *ret = alloc_object (), *n;
  struct condition *c;
  struct condition_field *f;

  ret->type = TYPE_CONDITION;

  c = malloc_and_check (sizeof (*c));
  c->class_name = type->value_ptr.condition_class->name;
  c->fields = NULL;
  ret->value_ptr.condition = c;

  create_condition_fields (ret, type);

  f = ret->value_ptr.condition->fields;

  while ((n = va_arg (valist, struct object *)))
    {
      f->value = n;
      increment_refcount (n);
      f = f->next;
    }

  return ret;
}


struct object *
create_condition_by_class (struct object *type, ...)
{
  struct object *ret;
  va_list valist;

  va_start (valist, type);

  ret = create_condition (type, valist);

  va_end (valist);

  return ret;
}


struct object *
create_condition_by_c_string (char *type, struct environment *env, ...)
{
  struct object *sym = intern_symbol_by_char_vector (type, strlen (type), 1,
						     EXTERNAL_VISIBILITY, 0,
						     env->cl_package), *ret;
  va_list valist;

  va_start (valist, env);

  ret = create_condition (sym->value_ptr.symbol->typespec, valist);

  va_end (valist);

  return ret;
}


int
does_condition_include_outcome_type (struct object *cond, enum outcome_type type,
				     struct environment *env)
{
  if ((cond == BUILTIN_SYMBOL ("TYPE-ERROR")
      && type == WRONG_TYPE_OF_ARGUMENT)
      || (cond == BUILTIN_SYMBOL ("DIVISION-BY-ZERO")
       && type == CANT_DIVIDE_BY_ZERO)
      || (cond == BUILTIN_SYMBOL ("ARITHMETIC-ERROR")
	  && type == CANT_DIVIDE_BY_ZERO)
      || (cond == BUILTIN_SYMBOL ("ERROR") && type > EVAL_OK))
    {
      return 1;
    }

  return 0;
}


void
add_condition_class (char *name, struct environment *env, int is_standard, ...)
{
  va_list valist;
  char *s, *rn;
  struct object *condcl, *pack = inspect_variable (env->package_sym, env),
    *sym = intern_symbol_by_char_vector (name, strlen (name), 1,
					 EXTERNAL_VISIBILITY, 1, pack), *par, *rs;
  struct condition_class *cc;
  struct condition_field_decl *f, *prev;

  va_start (valist, is_standard);

  sym->value_ptr.symbol->is_type = 1;
  sym->value_ptr.symbol->is_standard_type = is_standard;

  condcl = alloc_object ();
  condcl->type = TYPE_CONDITION_CLASS;

  cc = malloc_and_check (sizeof (*cc));
  condcl->value_ptr.condition_class = cc;

  increment_refcount (sym);
  cc->name = sym;

  cc->parents = NULL;

  while ((s = va_arg (valist, char *)))
    {
      par = intern_symbol_by_char_vector (s, strlen (s), 1, EXTERNAL_VISIBILITY,
					  1, pack);

      prepend_object_to_obj_list (par, &cc->parents);
    }

  cc->fields = NULL;

  while ((s = va_arg (valist, char *)))
    {
      par = intern_symbol_by_char_vector (s, strlen (s), 1, INTERNAL_VISIBILITY,
					  1, pack);

      f = create_condition_field_decl (par, env, NULL);

      if (cc->fields)
	prev = prev->next = f;
      else
	cc->fields = prev = f;

      rn = concatenate_char_vectors (sym->value_ptr.symbol->name_len + 1
				     + par->value_ptr.symbol->name_len,
				     sym->value_ptr.symbol->name,
				     sym->value_ptr.symbol->name_len, "-", 1,
				     par->value_ptr.symbol->name,
				     par->value_ptr.symbol->name_len,
				     (char *)NULL);

      rs = intern_symbol_by_char_vector (rn, sym->value_ptr.symbol->name_len+1+
					 par->value_ptr.symbol->name_len, 0,
					 EXTERNAL_VISIBILITY, 1, pack);
      increment_refcount (rs);
      rs->value_ptr.symbol->function_cell = alloc_function ();
      rs->value_ptr.symbol->function_cell->value_ptr.function->
	condition_reader_class = sym;
      rs->value_ptr.symbol->function_cell->value_ptr.function->
	condition_reader_field = par;
      rs->value_ptr.symbol->function_cell->value_ptr.function->name = rs;
    }

  sym->value_ptr.symbol->typespec = condcl;

  va_end (valist);
}


struct object *
handle_condition (struct object *cond, struct environment *env,
		  struct outcome *outcome)
{
  struct handler_binding *b;
  struct handler_binding_frame *f;
  struct object *hret;
  struct object arg;
  struct cons_pair c;

  if (!env->handlers)
    return &nil_object;

  b = env->handlers->frame;

  arg.type = TYPE_CONS_PAIR;
  arg.value_ptr.cons_pair = &c;
  arg.refcount1 = 0;
  arg.refcount2 = 1;
  c.car = cond;
  c.cdr = &nil_object;

  while (b)
    {
      if (is_subtype (cond->value_ptr.condition->class_name, b->condition, NULL))
	{
	  f = env->handlers;
	  env->handlers = env->handlers->next;

	  hret = call_function (b->handler, &arg, 1, 0, 1, 0, 0, env, outcome);

	  env->handlers = f;

	  if (!hret)
	    return NULL;

	  decrement_refcount (hret);
	}

      b = b->next;
    }

  return &t_object;
}


void
add_builtin_type (char *name, struct environment *env,
		  int (*builtin_type) (const struct object *obj,
				       const struct object *typespec,
				       struct environment *env,
				       struct outcome *outcome),
		  int is_standard, ...)
{
  va_list valist;
  char *s;
  struct object *pack = inspect_variable (env->package_sym, env);
  struct object *sym = intern_symbol_by_char_vector (name, strlen (name), 1,
						     EXTERNAL_VISIBILITY, 1,
						     pack);
  struct object *par;

  va_start (valist, is_standard);

  sym->value_ptr.symbol->is_type = 1;
  sym->value_ptr.symbol->is_standard_type = is_standard;
  sym->value_ptr.symbol->builtin_type = builtin_type;

  while ((s = va_arg (valist, char *)))
    {
      par = intern_symbol_by_char_vector (s, strlen (s), 1,
					  EXTERNAL_VISIBILITY, 1, pack);

      prepend_object_to_obj_list (par, &sym->value_ptr.symbol->parent_types);
    }

  va_end (valist);
}


struct object *
add_builtin_form (char *name, struct environment *env,
		  struct object *(*builtin_form)
		  (struct object *list, struct environment *env,
		   struct outcome *outcome), enum object_type type,
		  struct object *(*builtin_setf_func)
		  (struct object *list, struct environment *env,
		   struct outcome *outcome), int is_special_operator)
{
  struct object *pack = inspect_variable (env->package_sym, env);
  struct object *sym = intern_symbol_by_char_vector (name, strlen (name), 1,
						     EXTERNAL_VISIBILITY, 1,
						     pack);
  struct object *fun = alloc_function ();
  struct function *f = fun->value_ptr.function;

  fun->type = type;

  sym->value_ptr.symbol->function_cell = fun;

  f->name = sym;
  add_reference (fun, sym, 0);

  f->is_special_operator = is_special_operator;
  f->builtin_form = builtin_form;

  if (builtin_setf_func)
    {
      fun = alloc_function ();
      sym->value_ptr.symbol->setf_func_cell = fun;

      fun->value_ptr.function->name = sym;
      add_reference (fun, sym, 0);

      fun->value_ptr.function->is_setf_func = 1;
      fun->value_ptr.function->builtin_form = builtin_setf_func;
    }

  return sym;
}


struct object *
define_constant (struct object *sym, struct object *form,
		 struct environment *env, struct outcome *outcome)
{
  struct object *val;

  val = evaluate_object (form, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!val)
    return NULL;

  sym = SYMBOL (sym);

  if (sym->value_ptr.symbol->is_const)
    {
      if (eql_objects (val, sym->value_ptr.symbol->value_cell) == &t_object)
	{
	  decrement_refcount (val);
	  increment_refcount (sym);
	  return sym;
	}
      else
	{
	  outcome->type = CANT_REDEFINE_CONSTANT_TO_NONEQL_VALUE;
	  return NULL;
	}
    }

  sym->value_ptr.symbol->is_const = 1;
  sym->value_ptr.symbol->value_cell = val;
  add_reference (sym, val, 0);
  decrement_refcount (val);
  mark_as_constant (val);

  increment_refcount (sym);
  return sym;
}


struct object *
define_parameter (struct object *sym, struct object *form,
		  struct environment *env, struct outcome *outcome)
{
  struct object *val = evaluate_object (form, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!val)
    return NULL;

  if (sym->value_ptr.symbol->value_cell)
    {
      delete_reference (sym, sym->value_ptr.symbol->value_cell, 0);
    }

  sym->value_ptr.symbol->is_parameter = 1;
  sym->value_ptr.symbol->value_cell = val;
  add_reference (sym, val, 0);
  decrement_refcount (val);

  increment_refcount (sym);
  return sym;
}


struct object *
define_constant_by_name (char *name, struct object *value,
			 struct environment *env)
{
  struct object *pack = inspect_variable (env->package_sym, env);
  struct object *sym = intern_symbol_by_char_vector (name, strlen (name), 1,
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
  struct object *sym = intern_symbol_by_char_vector (name, strlen (name), 1,
						     EXTERNAL_VISIBILITY, 1,
						     pack);

  sym->value_ptr.symbol->is_parameter = 1;
  sym->value_ptr.symbol->value_cell = value;

  return sym;
}


struct object *
skip_prefix (struct object *prefix, int *num_backticks_before_last_comma,
	     int *num_commas, struct object **last_prefix)
{
  int num_backticks = 0;

  if (last_prefix)
    *last_prefix = NULL;
  if (num_backticks_before_last_comma)
    *num_backticks_before_last_comma = 0;
  if (num_commas)
    *num_commas = 0;

  while (prefix && (prefix->type == TYPE_BACKQUOTE
		    || prefix->type == TYPE_COMMA
		    || prefix->type == TYPE_AT || prefix->type == TYPE_DOT))
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

      prefix = prefix->value_ptr.next;
    }

  if (num_backticks_before_last_comma && (!num_commas || !*num_commas))
    *num_backticks_before_last_comma = num_backticks;

  return prefix;
}


struct object *
elt (struct object *seq, unsigned int ind)
{
  if (IS_LIST (seq))
    return nth (ind, seq);
  else if (seq->type == TYPE_STRING)
    return get_nth_character (seq, ind);
  else if (seq->type == TYPE_ARRAY)
    return seq->value_ptr.array->value [ind];
  else
    return NULL;
}


void
set_elt (struct object *seq, unsigned int ind, struct object *val)
{
  struct object *cons;

  if (IS_LIST (seq))
    {
      cons = nthcdr (ind, seq);
      cons->value_ptr.cons_pair->car = val;
      add_reference (cons, val, 0);
    }
  else if (seq->type == TYPE_STRING)
    seq->value_ptr.string->value [ind] = val->value_ptr.character [0];
  else if (seq->type == TYPE_ARRAY)
    {
      seq->value_ptr.array->value [ind] = val;
      add_reference (seq, val, ind);
    }
}


fixnum
sequence_length (const struct object *seq)
{
  if (IS_LIST (seq))
    return list_length (seq);
  else if (seq->type == TYPE_STRING)
    return seq->value_ptr.string->used_size;
  else if (seq->type == TYPE_ARRAY)
    return seq->value_ptr.array->alloc_size->size;
  else
    return -1;
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


fixnum
list_length (const struct object *list)
{
  fixnum l = 0;
  
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
	     struct object **last_prefix)
{
  struct object *out, *pr = NULL, *tmp;

  while (begin)
    {
      tmp = alloc_prefix (begin->type == TYPE_BACKQUOTE ? '`' :
			  begin->type == TYPE_COMMA ? ',' :
			  begin->type == TYPE_AT ? '@' :
			  begin->type == TYPE_DOT ? '.'
			  : NONE);

      if (pr)
	pr = pr->value_ptr.next = tmp;
      else
	out = pr = tmp;

      if (begin == end)
	break;

      begin = begin->value_ptr.next;
    }

  if (last_prefix)
    *last_prefix = pr;

  return out;
}


struct object *
copy_list_structure (struct object *list, const struct object *prefix,
		     int num_conses, struct object **last_cell)
{
  struct object *cons, *out, *lastpref, *cdr;
  int i = 1;

  out = cons = alloc_empty_cons_pair ();

  out->value_ptr.cons_pair->car = CAR (list);
  add_reference (out, CAR (list), 0);

  list = CDR (list);

  while (list->type == TYPE_CONS_PAIR && (i < num_conses || num_conses < 0))
    {
      cdr = alloc_empty_cons_pair ();

      cons->value_ptr.cons_pair->cdr = cdr;
      add_reference (cons, cdr, 1);
      decrement_refcount (cdr);

      cons = CDR (cons);

      if (prefix)
	{
	  cons->value_ptr.cons_pair->car = copy_prefix (prefix, NULL, &lastpref);
	  add_reference (cons, CAR (cons), 0);
	  decrement_refcount (CAR (cons));

	  lastpref->value_ptr.next = CAR (list);
	  add_reference (lastpref, CAR (list), 0);
	}
      else
	{
	  cons->value_ptr.cons_pair->car = CAR (list);
	  add_reference (cons, CAR (list), 0);
	}

      list = CDR (list);
      i++;
    }

  if (SYMBOL (list) != &nil_object && (i < num_conses || num_conses < 0))
    {
      cons->value_ptr.cons_pair->cdr = list;
      add_reference (cons, list, 1);
    }

  if (last_cell)
    *last_cell = cons;

  return out;
}


fixnum
array_rank (const struct array_size *sz)
{
  fixnum rank = 0;

  while (sz)
    {
      rank++;

      sz = sz->next;
    }

  return rank;
}


fixnum
array_total_size (const struct array_size *sz)
{
  fixnum ret = 1;

  while (sz)
    {
      ret *= sz->size;

      sz = sz->next;
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
hash_object_respecting_eql (const struct object *object, size_t table_size)
{
  if (object->type == TYPE_CHARACTER)
    {
      return hash_char_vector (object->value_ptr.character,
			       strlen (object->value_ptr.character), table_size);
    }
  else if (object->type == TYPE_INTEGER || object->type == TYPE_RATIO
	   || object->type == TYPE_FLOAT || object->type == TYPE_COMPLEX)
    {
      return hash_number (object, table_size);
    }
  else
    {
      return hash_object_respecting_eq (object, table_size);
    }
}


int
hash_object_respecting_equal (const struct object *object, size_t table_size)
{
  if (object->type == TYPE_CONS_PAIR)
    {
      return (hash_object_respecting_equal (object->value_ptr.cons_pair->car,
					    table_size)
	      + hash_object_respecting_equal (object->value_ptr.cons_pair->cdr,
					      table_size)) % table_size;
    }
  else if (object->type == TYPE_STRING)
    {
      return hash_char_vector (object->value_ptr.string->value,
			       object->value_ptr.string->used_size, table_size);
    }
  else
    {
      return hash_object_respecting_eql (object, table_size);
    }
}


int
hash_object_respecting_equalp (const struct object *object, size_t table_size)
{
  return 0;
}


int
hash_table_count (const struct hashtable *hasht)
{
  int i, cnt = 0;
  struct hashtable_record *r;

  for (i = 0; i < LISP_HASHTABLE_SIZE; i++)
    {
      r = hasht->table [i];

      while (r)
	{
	  cnt++;
	  r = r->next;
	}
    }

  return cnt;
}


void
clear_hash_table (struct object *hasht)
{
  size_t i, j;
  struct hashtable_record *r, *n;

  for (i = 0; i < LISP_HASHTABLE_SIZE; i++)
    {
      r = hasht->value_ptr.hashtable->table [i];
      j = 0;

      while (r)
	{
	  n = r->next;

	  delete_reference (hasht, r->key, i+j*2*LISP_HASHTABLE_SIZE);
	  delete_reference (hasht, r->value, i+(j*2+1)*LISP_HASHTABLE_SIZE);

	  free (r);

	  r = n;
	  j++;
	}

      hasht->value_ptr.hashtable->table [i] = NULL;
    }
}


struct parameter *
alloc_parameter (enum parameter_type type, struct object *sym)
{
  struct parameter *par = malloc_and_check (sizeof (*par));
  par->type = type;
  par->name = sym;
  par->sub_lambda_list = NULL;
  par->init_form = NULL;
  par->supplied_p_param = NULL;
  par->key = NULL;
  par->typespec = NULL;
  par->next = NULL;

  return par;
}


struct parameter *
parse_required_parameters (struct object *obj, struct parameter **last,
			   struct object **rest, int allow_destructuring,
			   int is_specialized, struct environment *env,
			   struct outcome *outcome)
{
  struct object *car;
  struct parameter *first = NULL;

  *last = NULL;

  while (obj && obj->type == TYPE_CONS_PAIR)
    {
      car = CAR (obj);

      if (car->type == TYPE_CONS_PAIR && allow_destructuring)
	{
	  if (!first)
	    *last = first = alloc_parameter (REQUIRED_PARAM, NULL);
	  else
	    *last = (*last)->next = alloc_parameter (REQUIRED_PARAM, NULL);

	  outcome->type = EVAL_OK;
	  (*last)->sub_lambda_list = parse_lambda_list (car, 1, 0, env, outcome,
							&(*last)->
							sub_allow_other_keys);

	  if (outcome->type != EVAL_OK)
	    return NULL;
	}
      else if (car->type == TYPE_CONS_PAIR && is_specialized)
	{
	  if (list_length (car) != 2 || !IS_SYMBOL (CAR (car))
	      || !IS_SYMBOL (CAR (CDR (car))))
	    {
	      outcome->type = INVALID_LAMBDA_LIST;
	      return NULL;
	    }

	  if (SYMBOL (CAR (car))->value_ptr.symbol->is_const)
	    {
	      outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	      return NULL;
	    }

	  if (!first)
	    *last = first = alloc_parameter (REQUIRED_PARAM, SYMBOL (CAR (car)));
	  else
	    *last = (*last)->next = alloc_parameter (REQUIRED_PARAM,
						     SYMBOL (CAR (car)));

	  (*last)->reference_strength_factor =
	    !STRENGTH_FACTOR_OF_OBJECT (SYMBOL (CAR (car)));
	  INC_WEAK_REFCOUNT (SYMBOL (CAR (car)));

	  (*last)->typespec = SYMBOL (CAR (CDR (car)));
	}
      else
	{
	  if (!IS_SYMBOL (car))
	    {
	      outcome->type = INVALID_LAMBDA_LIST;
	      return NULL;
	    }

	  if (SYMBOL (car)->value_ptr.symbol->is_const)
	    {
	      outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	      return NULL;
	    }

	  car = SYMBOL (car);

	  if (car == env->amp_optional_sym || car == env->amp_rest_sym
	      || car == env->amp_body_sym || car == env->amp_key_sym
	      || car == env->amp_allow_other_keys_sym || car == env->amp_aux_sym)
	    {
	      break;
	    }

	  if (!first)
	    *last = first = alloc_parameter (REQUIRED_PARAM, car);
	  else
	    *last = (*last)->next = alloc_parameter (REQUIRED_PARAM, car);

	  (*last)->reference_strength_factor = !STRENGTH_FACTOR_OF_OBJECT (car);
	  INC_WEAK_REFCOUNT (car);

	  if (is_specialized)
	    (*last)->typespec = &t_object;
	}

      obj = CDR (obj);
    }

  *rest = obj;

  return first;
}


struct parameter *
parse_optional_parameters (struct object *obj, struct parameter **last,
			   struct object **next, struct environment *env,
			   struct outcome *outcome)
{
  struct object *car, *supplp;
  struct parameter *first = NULL;
  int l;

  *last = NULL;

  while (obj && SYMBOL (obj) != &nil_object)
    {
      car = CAR (obj);

      if (IS_SYMBOL (car) && (car = SYMBOL (car))
	  && (car == env->amp_optional_sym || car == env->amp_rest_sym
	      || car == env->amp_body_sym || car == env->amp_key_sym
	      || car == env->amp_allow_other_keys_sym
	      || car == env->amp_aux_sym))
	{
	  break;
	}
      else if (IS_SYMBOL (car))
	{
	  if (SYMBOL (car)->value_ptr.symbol->is_const)
	    {
	      outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	      return NULL;
	    }

	  increment_refcount (SYMBOL (car));

	  if (!first)
	    *last = first = alloc_parameter (OPTIONAL_PARAM, SYMBOL (car));
	  else
	    *last = (*last)->next =
	      alloc_parameter (OPTIONAL_PARAM, SYMBOL (car));

	  (*last)->reference_strength_factor =
	    !STRENGTH_FACTOR_OF_OBJECT (SYMBOL (car));
	  INC_WEAK_REFCOUNT (SYMBOL (car));
	}
      else if (car->type == TYPE_CONS_PAIR)
	{
	  l = list_length (car);

	  if (!l || l > 3 || !IS_SYMBOL (CAR (car)))
	    {
	      outcome->type = INVALID_LAMBDA_LIST;
	      return NULL;
	    }

	  if (SYMBOL (CAR (car))->value_ptr.symbol->is_const)
	    {
	      outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	      return NULL;
	    }

	  if (!first)
	    *last = first = alloc_parameter (OPTIONAL_PARAM, SYMBOL (CAR (car)));
	  else
	    *last = (*last)->next =
	      alloc_parameter (OPTIONAL_PARAM, SYMBOL (CAR (car)));

	  (*last)->reference_strength_factor =
	    !STRENGTH_FACTOR_OF_OBJECT (SYMBOL (CAR (car)));
	  INC_WEAK_REFCOUNT (SYMBOL (CAR (car)));

	  if (l >= 2)
	    {
	      (*last)->init_form = nth (1, car);
	      (*last)->reference_strength_factor =
		WITH_CHANGED_BIT ((*last)->reference_strength_factor, 1,
				  !STRENGTH_FACTOR_OF_OBJECT ((*last)->init_form));
	      INC_WEAK_REFCOUNT ((*last)->init_form);
	    }

	  if (l == 3)
	    {
	      if (!IS_SYMBOL (CAR (CDR (CDR (car)))))
		{
		  outcome->type = INVALID_LAMBDA_LIST;
		  return NULL;
		}

	      if (SYMBOL (CAR (CDR (CDR (car))))->value_ptr.symbol->is_const)
		{
		  outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
		  return NULL;
		}

	      supplp = SYMBOL (CAR (CDR (CDR (car))));

	      (*last)->supplied_p_param = supplp;
	      (*last)->reference_strength_factor =
		WITH_CHANGED_BIT ((*last)->reference_strength_factor, 2,
				  !STRENGTH_FACTOR_OF_OBJECT (supplp));
	      INC_WEAK_REFCOUNT (supplp);
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
			  struct outcome *outcome)
{
  struct object *car, *caar, *key, *var, *supplp;
  struct parameter *first = NULL;
  int l;

  *last = NULL;

  while (obj && SYMBOL (obj) != &nil_object)
    {
      car = CAR (obj);

      if (IS_SYMBOL (car) && (car = SYMBOL (car))
	  && (car == env->amp_optional_sym || car == env->amp_rest_sym
	      || car == env->amp_body_sym || car == env->amp_key_sym))
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
	}
      else if (IS_SYMBOL (car) && (car == env->amp_allow_other_keys_sym
				   || car == env->amp_aux_sym))
	{
	  break;
	}
      else if (IS_SYMBOL (car))
	{
	  if (SYMBOL (car)->value_ptr.symbol->is_const)
	    {
	      outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	      return NULL;
	    }

	  var = SYMBOL (car);

	  key = intern_symbol_by_char_vector (var->value_ptr.symbol->name,
					      var->value_ptr.symbol->name_len,
					      1, EXTERNAL_VISIBILITY, 1,
					      env->keyword_package);

	  if (!first)
	    *last = first = alloc_parameter (KEYWORD_PARAM, var);
	  else
	    *last = (*last)->next =
	      alloc_parameter (KEYWORD_PARAM, var);

	  (*last)->key = key;

	  (*last)->reference_strength_factor = !STRENGTH_FACTOR_OF_OBJECT (var);
	  INC_WEAK_REFCOUNT (var);

	  (*last)->reference_strength_factor =
	    WITH_CHANGED_BIT ((*last)->reference_strength_factor, 3,
			      !STRENGTH_FACTOR_OF_OBJECT (key));
	  INC_WEAK_REFCOUNT (key);
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
	      if (SYMBOL (caar)->value_ptr.symbol->is_const)
		{
		  outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
		  return NULL;
		}

	      var = SYMBOL (caar);

	      key = intern_symbol_by_char_vector
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

	      if (SYMBOL (CAR (CDR (caar)))->value_ptr.symbol->is_const)
		{
		  outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
		  return NULL;
		}

	      key = SYMBOL (CAR (caar));
	      var = SYMBOL (CAR (CDR (caar)));
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

	  (*last)->reference_strength_factor = !STRENGTH_FACTOR_OF_OBJECT (var);
	  INC_WEAK_REFCOUNT (var);

	  (*last)->reference_strength_factor =
	    WITH_CHANGED_BIT ((*last)->reference_strength_factor, 3,
			      !STRENGTH_FACTOR_OF_OBJECT (key));
	  INC_WEAK_REFCOUNT (key);

	  if (l >= 2)
	    {
	      (*last)->init_form = nth (1, car);
	      (*last)->reference_strength_factor =
		WITH_CHANGED_BIT ((*last)->reference_strength_factor, 1,
				  !STRENGTH_FACTOR_OF_OBJECT (nth (1, car)));
	      INC_WEAK_REFCOUNT (nth (1, car));
	    }

	  if (l == 3)
	    {
	      if (!IS_SYMBOL (CAR (CDR (CDR (car)))))
		{
		  outcome->type = INVALID_LAMBDA_LIST;
		  return NULL;
		}

	      if (SYMBOL (CAR (CDR (CDR (car))))->value_ptr.symbol->is_const)
		{
		  outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
		  return NULL;
		}

	      supplp = SYMBOL (nth (2, car));

	      (*last)->supplied_p_param = supplp;
	      (*last)->reference_strength_factor =
		WITH_CHANGED_BIT ((*last)->reference_strength_factor, 2,
				  !STRENGTH_FACTOR_OF_OBJECT (supplp));
	      INC_WEAK_REFCOUNT (supplp);
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
parse_lambda_list (struct object *obj, int allow_destructuring,
		   int is_specialized, struct environment *env,
		   struct outcome *outcome, int *allow_other_keys)
{
  struct parameter *first = NULL, *last = NULL, *p, *ls;
  struct object *car, *restsym, *wholesym;
  int found_amp_key = 0, dotted_ok = 0, l;

  *allow_other_keys = 0;

  if (SYMBOL (obj) == &nil_object)
    {
      return NULL;
    }

  if (obj->type != TYPE_CONS_PAIR)
    {
      outcome->type = INVALID_LAMBDA_LIST;
      return NULL;
    }

  if (allow_destructuring && (car = CAR (obj))
      && SYMBOL (car) == env->amp_whole_sym)
    {
      if ((SYMBOL (CDR (obj)) == &nil_object || !IS_SYMBOL (CAR (CDR (obj)))))
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
	}

      wholesym = SYMBOL (CAR (CDR (obj)));

      if (wholesym->value_ptr.symbol->is_const)
	{
	  outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	  return NULL;
	}

      last = first = alloc_parameter (WHOLE_PARAM, wholesym);

      last->reference_strength_factor = !STRENGTH_FACTOR_OF_OBJECT (wholesym);
      INC_WEAK_REFCOUNT (wholesym);

      obj = CDR (CDR (obj));
    }

  p = parse_required_parameters (obj, &ls, &obj, allow_destructuring,
				 is_specialized, env, outcome);

  if (first)
    last->next = p;
  else
    first = last = p;

  last = ls;

  if (outcome->type != EVAL_OK)
    return NULL;

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && SYMBOL (car) == env->amp_optional_sym)
    {
      p = parse_optional_parameters (CDR (obj), &ls, &obj, env, outcome);

      if (first)
	last->next = p;
      else
	first = last = p;

      last = ls;

      if (outcome->type != EVAL_OK)
	return NULL;
    }

  if (obj && ((obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
	       && (SYMBOL (car) == env->amp_rest_sym
		   || SYMBOL (car) == env->amp_body_sym))
	      || (obj->type != TYPE_CONS_PAIR && SYMBOL (obj) != &nil_object
		  && allow_destructuring)))
    {
      if (obj->type == TYPE_CONS_PAIR
	  && (SYMBOL (CDR (obj)) == &nil_object || !IS_SYMBOL (CAR (CDR (obj)))))
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
	}

      if (obj->type != TYPE_CONS_PAIR && !IS_SYMBOL (obj))
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
	}

      restsym = obj->type == TYPE_CONS_PAIR ? SYMBOL (CAR (CDR (obj)))
	: SYMBOL (obj);

      if (restsym->value_ptr.symbol->is_const)
	{
	  outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	  return NULL;
	}

      if (first)
	last = last->next = alloc_parameter (REST_PARAM, restsym);
      else
	last = first = alloc_parameter (REST_PARAM, restsym);

      last->reference_strength_factor = !STRENGTH_FACTOR_OF_OBJECT (restsym);
      INC_WEAK_REFCOUNT (restsym);

      if (obj->type == TYPE_CONS_PAIR)
	obj = CDR (CDR (obj));
      else
	dotted_ok = 1;
    }

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && SYMBOL (car) == env->amp_key_sym)
    {
      found_amp_key = 1;

      p = parse_keyword_parameters (CDR (obj), &ls, &obj, env, outcome);

      if (first)
	last->next = p;
      else
	first = last = p;

      last = ls;

      if (outcome->type != EVAL_OK)
	return NULL;
    }

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && SYMBOL (car) == env->amp_allow_other_keys_sym)
    {
      if (!found_amp_key)
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
	}

      *allow_other_keys = 1;

      obj = CDR (obj);
    }

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && SYMBOL (car) == env->amp_aux_sym)
    {
      obj = CDR (obj);

      while (obj && SYMBOL (obj) != &nil_object)
	{
	  car = CAR (obj);

	  if (IS_SYMBOL (car) && (car = SYMBOL (car))
	      && (car == env->amp_optional_sym || car == env->amp_rest_sym
		  || car == env->amp_body_sym || car == env->amp_key_sym
		  || car == env->amp_allow_other_keys_sym
		  || car == env->amp_aux_sym))
	    {
	      break;
	    }

	  if (IS_SYMBOL (car))
	    {
	      if (SYMBOL (car)->value_ptr.symbol->is_const)
		{
		  outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
		  return NULL;
		}

	      if (!first)
		last = first = alloc_parameter (AUXILIARY_VAR, SYMBOL (car));
	      else
		last = last->next =
		  alloc_parameter (AUXILIARY_VAR, SYMBOL (car));

	      last->reference_strength_factor =
		!STRENGTH_FACTOR_OF_OBJECT (SYMBOL (car));
	      INC_WEAK_REFCOUNT (SYMBOL (car));
	    }
	  else if (car->type == TYPE_CONS_PAIR)
	    {
	      l = list_length (car);

	      if (!l || l > 2 || !IS_SYMBOL (CAR (car)))
		{
		  outcome->type = INVALID_LAMBDA_LIST;
		  return NULL;
		}

	      if (SYMBOL (CAR (car))->value_ptr.symbol->is_const)
		{
		  outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
		  return NULL;
		}

	      if (!first)
		last = first = alloc_parameter (AUXILIARY_VAR,
						SYMBOL (CAR (car)));
	      else
		last = last->next =
		  alloc_parameter (AUXILIARY_VAR, SYMBOL (CAR (car)));

	      last->reference_strength_factor =
		!STRENGTH_FACTOR_OF_OBJECT (SYMBOL (CAR (car)));
	      INC_WEAK_REFCOUNT (SYMBOL (CAR (car)));

	      if (l == 2)
		{
		  last->init_form = nth (1, car);
		  last->reference_strength_factor =
		    WITH_CHANGED_BIT (last->reference_strength_factor, 1,
				      !STRENGTH_FACTOR_OF_OBJECT (nth (1, car)));
		  INC_WEAK_REFCOUNT (nth (1, car));
		}
	    }

	  obj = CDR (obj);
	}
    }

  if (SYMBOL (obj) != &nil_object && !dotted_ok)
    {
      outcome->type = INVALID_LAMBDA_LIST;
      return NULL;
    }

  return first;
}


int
are_lambda_lists_congruent (struct parameter *meth_list,
			    struct parameter *gen_list)
{
  return 1;
}


int
parse_declaration_specifier (struct object *spec, int is_local,
			     struct environment *env, int bin_num,
			     struct outcome *outcome)
{
  struct object *form, *declid, *sym;
  struct binding *vars;

  if (spec->type != TYPE_CONS_PAIR || !IS_SYMBOL (CAR (spec)))
    {
      outcome->type = WRONG_SYNTAX_IN_DECLARATION;
      return 0;
    }

  declid = SYMBOL (CAR (spec));
  form = CDR (spec);

  if (declid == env->ignorable_sym || declid == env->ignore_sym)
    {
      sym = BUILTIN_SYMBOL ("FUNCTION");

      while (SYMBOL (form) != &nil_object)
	{
	  if (!IS_SYMBOL (CAR (form))
	      && (CAR (form)->type != TYPE_CONS_PAIR
		  || list_length (CAR (form)) != 2
		  || SYMBOL (CAR (CAR (form))) != sym
		  || !IS_SYMBOL (CAR (CDR (CAR (form))))))
	    {
	      outcome->type = WRONG_SYNTAX_IN_DECLARATION;
	      return 0;
	    }

	  form = CDR (form);
	}
    }
  else if (declid == env->inline_sym || declid == env->notinline_sym)
    {
      sym = BUILTIN_SYMBOL ("SETF");

      while (SYMBOL (form) != &nil_object)
	{
	  if (!IS_SYMBOL (CAR (form))
	      && (CAR (form)->type != TYPE_CONS_PAIR
		  || list_length (CAR (form)) != 2
		  || SYMBOL (CAR (CAR (form))) != sym
		  || !IS_SYMBOL (CAR (CDR (CAR (form))))))
	    {
	      outcome->type = WRONG_SYNTAX_IN_DECLARATION;
	      return 0;
	    }

	  form = CDR (form);
	}
    }
  else if (declid == env->optimize_sym)
    {
      while (SYMBOL (form) != &nil_object)
	{
	  if (IS_SYMBOL (CAR (form)))
	    {
	      sym = SYMBOL (CAR (form));

	      if (sym != env->compilation_speed_sym
		  && sym != env->debug_sym && sym != env->safety_sym
		  && sym != env->space_sym && sym != env->speed_sym)
		{
		  outcome->type = WRONG_SYNTAX_IN_DECLARATION;
		  return 0;
		}
	    }
	  else if (CAR (form)->type == TYPE_CONS_PAIR
		   && list_length (CAR (form)) == 2
		   && IS_SYMBOL (CAR (CAR (form))))
	    {
	      sym = SYMBOL (CAR (CAR (form)));

	      if ((sym != env->compilation_speed_sym
		   && sym != env->debug_sym && sym != env->safety_sym
		   && sym != env->space_sym && sym != env->speed_sym)
		  || CAR (CDR (CAR (form)))->type != TYPE_INTEGER
		  || mpz_cmp_si (CAR (CDR (CAR (form)))->value_ptr.integer,
				 0) < 0
		  || mpz_cmp_si (CAR (CDR (CAR (form)))->value_ptr.integer,
				 3) > 0)
		{
		  outcome->type = WRONG_SYNTAX_IN_DECLARATION;
		  return 0;
		}
	    }
	  else
	    {
	      outcome->type = WRONG_SYNTAX_IN_DECLARATION;
	      return 0;
	    }

	  form = CDR (form);
	}
    }
  else if (declid == env->special_sym)
    {
      while (SYMBOL (form) != &nil_object)
	{
	  if (!IS_SYMBOL (CAR (form)))
	    {
	      outcome->type = WRONG_SYNTAX_IN_DECLARATION;
	      return 0;
	    }

	  if (is_local)
	    {
	      vars = env->vars;

	      for (; bin_num; bin_num--)
		{
		  if (vars->sym == SYMBOL (CAR (form)))
		    {
		      vars->type = DYNAMIC_BINDING;
		      SYMBOL (CAR (form))->value_ptr.symbol->
			value_dyn_bins_num++;
		      break;
		    }

		  vars = vars->next;
		}

	      SYMBOL (CAR (form))->value_ptr.symbol->is_special++;
	    }
	  else
	    {
	      SYMBOL (CAR (form))->value_ptr.symbol->is_parameter = 1;
	    }

	  form = CDR (form);
	}
    }
  else if (declid == env->type_sym)
    {
      form = CDR (form);

      while (SYMBOL (form) != &nil_object)
	{
	  if (!IS_SYMBOL (CAR (form)))
	    {
	      outcome->type = WRONG_SYNTAX_IN_DECLARATION;
	      return 0;
	    }

	  form = CDR (form);
	}
    }
  else if (declid == env->ftype_sym)
    {
      form = CDR (form);

      while (SYMBOL (form) != &nil_object)
	{
	  if (!IS_SYMBOL (CAR (form)))
	    {
	      outcome->type = WRONG_SYNTAX_IN_DECLARATION;
	      return 0;
	    }

	  form = CDR (form);
	}
    }
  else
    {
      outcome->type = UNKNOWN_DECLARATION;
      return 0;
    }

  return 1;
}


int
parse_declarations (struct object *body, struct environment *env, int bin_num,
		    struct outcome *outcome, struct object **next)
{
  struct object *forms;

  *next = body;

  while (SYMBOL (*next) != &nil_object)
    {
      if (CAR (*next)->type != TYPE_CONS_PAIR
	  || SYMBOL (CAR (CAR (*next))) != env->declare_sym)
	return 1;

      forms = CDR (CAR (*next));

      while (forms->type == TYPE_CONS_PAIR)
	{
	  if (!parse_declaration_specifier (CAR (forms), 1, env, bin_num,
					    outcome))
	    {
	      return 0;
	    }

	  forms = CDR (forms);
	}

      *next = CDR (*next);
    }

  return 1;
}


void
undo_special_declarations (struct object *decl, struct environment *env)
{
  struct object *forms, *vars;

  while (SYMBOL (decl) != &nil_object && CAR (decl)->type == TYPE_CONS_PAIR
	 && SYMBOL (CAR (CAR (decl))) == env->declare_sym)
    {
      forms = CDR (CAR (decl));

      while (forms->type == TYPE_CONS_PAIR)
	{
	  if (SYMBOL (CAR (CAR (forms))) == env->special_sym)
	    {
	      vars = CDR (CAR (forms));

	      while (SYMBOL (vars) != &nil_object)
		{
		  SYMBOL (CAR (vars))->value_ptr.symbol->is_special--;

		  vars = CDR (vars);
		}
	    }

	  forms = CDR (forms);
	}

      decl = CDR (decl);
    }
}


struct object *
evaluate_body (struct object *body, int is_tagbody, struct object *block_name,
	       struct environment *env, struct outcome *outcome)
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

  while (SYMBOL (body) != &nil_object)
    {
      decrement_refcount (res);
      res = &nil_object;

      if (!is_tagbody
	  || (CAR (body)->type != TYPE_INTEGER && !IS_SYMBOL (CAR (body))))
	{
	  res = evaluate_object (CAR (body), env, outcome);

	  if (!res)
	    {
	      if (!outcome->tag_to_jump_to && !outcome->block_to_leave)
		goto cleanup_and_leave;
	      else if (outcome->tag_to_jump_to)
		{
		  if (is_tagbody && tags)
		    {
		      t = find_go_tag (outcome->tag_to_jump_to, env->go_tag_stack);

		      if (!t)
			goto cleanup_and_leave;

		      outcome->tag_to_jump_to = NULL;
		      body = t->dest;
		      res = &nil_object;
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

		      outcome->no_value = outcome->return_no_value;
		      outcome->other_values = outcome->return_other_values;
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

    }

 cleanup_and_leave:
  if (tags)
    {
      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);
    }

  if (block_name)
    env->blocks = remove_block (env->blocks);

  return res;
}


int
parse_argument_list (struct object *arglist, struct parameter *par,
		     int eval_args, int also_pass_name, int is_typespec,
		     int allow_other_keys, struct environment *env,
		     struct outcome *outcome, struct binding **bins,
		     int *argsnum)
{
  struct parameter *findk;
  struct object *val, *args = NULL, *as, *key_allow_other_k = NULL;
  struct binding *subbins, *lastbin = NULL;
  int rest_found = 0, subargs, found_unknown_key = 0;

  *bins = NULL, *argsnum = 0;


  if (par && par->type == WHOLE_PARAM)
    {
      increment_refcount (arglist);
      *bins = bind_variable (par->name, arglist, *bins);

      (*argsnum)++;

      par = par->next;
    }

  if (also_pass_name)
    arglist = CDR (arglist);

  while (SYMBOL (arglist) != &nil_object && par
	 && (par->type == REQUIRED_PARAM || par->type == OPTIONAL_PARAM))
    {
      if (par->sub_lambda_list)
	{
	  if (CAR (arglist)->type != TYPE_CONS_PAIR)
	    {
	      outcome->type = MISMATCH_IN_DESTRUCTURING_CALL;
	      return 0;
	    }

	  if (!parse_argument_list (CAR (arglist), par->sub_lambda_list,
				    eval_args, 0, is_typespec,
				    par->sub_allow_other_keys, env, outcome,
				    &subbins, &subargs))
	    {
	      return 0;
	    }

	  *argsnum += subargs;
	  *bins = chain_bindings (subbins, *bins, NULL, NULL);
	}
      else
	{
	  if (eval_args)
	    {
	      val = evaluate_object (CAR (arglist), env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!val)
		{
		  return 0;
		}
	    }
	  else
	    {
	      increment_refcount (CAR (arglist));
	      val = CAR (arglist);
	    }

	  *bins = bind_variable (par->name, val, *bins);

	  (*argsnum)++;

	  if (par->type == OPTIONAL_PARAM && par->supplied_p_param)
	    {
	      *bins = bind_variable (par->supplied_p_param, &t_object, *bins);

	      (*argsnum)++;
	    }
	}

      par = par->next;

      arglist = CDR (arglist);
    }

  if (par && par->type == REQUIRED_PARAM)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return 0;
    }

  if (SYMBOL (arglist) != &nil_object
      && (!par || (par && par->type != REST_PARAM && par->type != KEYWORD_PARAM))
      && !allow_other_keys)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return 0;
    }

  while (par && par->type == OPTIONAL_PARAM)
    {
      if (par->init_form)
	{
	  val = evaluate_object (par->init_form, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!val)
	    {
	      return 0;
	    }

	  *bins = bind_variable (par->name, val, *bins);

	  (*argsnum)++;
	}
      else
	{
	  if (is_typespec)
	    increment_refcount (env->star_sym);

	  *bins = bind_variable (par->name, is_typespec ? env->star_sym
				 : &nil_object, *bins);

	  (*argsnum)++;
	}

      if (par->supplied_p_param)
	{
	  *bins = bind_variable (par->supplied_p_param, &nil_object, *bins);

	  (*argsnum)++;
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
	      return 0;
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
      *bins = bind_variable (par->name, args, *bins);

      (*argsnum)++;

      par = par->next;

      rest_found = 1;
    }

  as = args;

  if (par && par->type == KEYWORD_PARAM)
    {
      findk = par;

      while (findk && findk->type == KEYWORD_PARAM)
	{
	  findk->key_passed = 0;

	  findk = findk->next;
	}

      while (SYMBOL (as) != &nil_object)
	{
	  findk = par;

	  while (findk && findk->type == KEYWORD_PARAM)
	    {
	      if (findk->key == SYMBOL (CAR (as)))
		break;

	      findk = findk->next;
	    }

	  if (!findk || findk->type != KEYWORD_PARAM)
	    {
	      if (SYMBOL (CDR (as)) == &nil_object)
		{
		  outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
		  return 0;
		}

	      found_unknown_key = 1;

	      if (SYMBOL (CAR (as)) == env->key_allow_other_keys_sym
		  && !key_allow_other_k)
		{
		  key_allow_other_k = CAR (CDR (as));
		}

	      as = CDR (CDR (as));
	      continue;
	    }

	  as = CDR (as);

	  if (SYMBOL (as) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return 0;
	    }

	  if (!findk->key_passed)
	    {
	      increment_refcount (CAR (as));

	      *bins = bind_variable (findk->name, CAR (as), *bins);
	      findk->key_passed = 1;
	      (*argsnum)++;

	      if (findk->key == env->key_allow_other_keys_sym
		  && !key_allow_other_k)
		{
		  key_allow_other_k = CAR (as);
		}
	    }

	  as = CDR (as);
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
		      return 0;
		    }
		}
	      else
		val = &nil_object;

	      *bins = bind_variable (findk->name, val, *bins);
	      (*argsnum)++;

	      if (findk->supplied_p_param)
		{
		  *bins = bind_variable (findk->supplied_p_param, &nil_object,
					 *bins);
		  (*argsnum)++;
		}
	    }
	  else if (findk->supplied_p_param)
	    {
	      *bins = bind_variable (findk->supplied_p_param, &t_object, *bins);
	      (*argsnum)++;
	    }

	  findk = findk->next;
	}
    }

  if (args && !rest_found)
    decrement_refcount (args);

  if (found_unknown_key && !allow_other_keys
      && (!key_allow_other_k || SYMBOL (key_allow_other_k) == &nil_object))
    {
      outcome->type = UNKNOWN_KEYWORD_ARGUMENT;
      return 0;
    }


  if (par && par->type == AUXILIARY_VAR)
    {
      env->vars = chain_bindings (*bins, env->vars, NULL, &lastbin);
      env->lex_env_vars_boundary += *argsnum;
    }

  while (par && par->type == AUXILIARY_VAR)
    {
      if (par->init_form)
	{
	  val = evaluate_object (par->init_form, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!val)
	    {
	      *bins = env->vars;
	      env->vars = lastbin->next;
	      lastbin->next = NULL;
	      env->lex_env_vars_boundary -= *argsnum;

	      return 0;
	    }

	  env->vars = bind_variable (par->name, val, env->vars);

	  (*argsnum)++;
	}
      else
	{
	  env->vars = bind_variable (par->name, &nil_object, env->vars);

	  (*argsnum)++;
	}

      par = par->next;
    }

  if (lastbin)
    {
      *bins = env->vars;
      env->vars = lastbin->next;
      lastbin->next = NULL;
      env->lex_env_vars_boundary -= *argsnum;
    }

  return 1;
}


struct object *
call_function (struct object *func, struct object *arglist, int eval_args,
	       int also_pass_name, int create_new_lex_env, int expand_and_eval,
	       int is_typespec, struct environment *env, struct outcome *outcome)
{
  struct binding *bins, *b;
  struct object *ret, *ret2, *args = NULL;
  int argsnum, closnum, prev_lex_bin_num = env->lex_env_vars_boundary;


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
  else if (func->value_ptr.function->struct_constructor_class)
    {
      args = evaluate_through_list (arglist, env, outcome);

      if (!args)
	return NULL;

      ret = call_structure_constructor (func->value_ptr.function->
					struct_constructor_class, args,
					env, outcome);

      decrement_refcount (args);

      return ret;
    }
  else if (func->value_ptr.function->struct_accessor_class)
    {
      args = evaluate_through_list (arglist, env, outcome);

      if (!args)
	return NULL;

      ret = call_structure_accessor (func->value_ptr.function->
				     struct_accessor_class,
				     func->value_ptr.function->
				     struct_accessor_field, args, NULL,
				     env, outcome);

      decrement_refcount (args);

      return ret;
    }
  else if (func->value_ptr.function->condition_reader_class)
    {
      args = evaluate_through_list (arglist, env, outcome);

      if (!args)
	return NULL;

      ret = call_condition_reader (func->value_ptr.function->
				   condition_reader_class,
				   func->value_ptr.function->
				   condition_reader_field, args, env, outcome);

      decrement_refcount (args);

      return ret;
    }
  else if (func->value_ptr.function->function_macro)
    {
      return call_function (func->value_ptr.function->function_macro,
			    CAR (arglist), 0, 1, 1, 0, 0, env, outcome);
    }
  else if (func->value_ptr.function->macro_function)
    {
      args = alloc_empty_list (2);
      args->value_ptr.cons_pair->car = arglist;
      args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = &nil_object;

      ret = call_function (func->value_ptr.macro->macro_function, args, 0, 0, 0,
			   1, 0, env, outcome);

      free_list_structure (args);

      return ret;
    }


  if (func->value_ptr.function->is_generic)
    {
      return dispatch_generic_function_call (func, arglist, env, outcome);
    }


  if (parse_argument_list (arglist, func->value_ptr.function->lambda_list,
			   eval_args, also_pass_name, is_typespec,
			   func->value_ptr.function->allow_other_keys, env,
			   outcome, &bins, &argsnum))
    {
      env->vars = chain_bindings (func->value_ptr.function->lex_vars, env->vars,
				  &closnum, NULL);

      if (!create_new_lex_env)
	env->lex_env_vars_boundary += closnum;
      else
	env->lex_env_vars_boundary = closnum;

      env->vars = chain_bindings (bins, env->vars, NULL, NULL);
      env->lex_env_vars_boundary += argsnum;

      ret = evaluate_body (func->value_ptr.function->body, 0,
			   func->value_ptr.function->name, env, outcome);

      env->vars = remove_bindings (env->vars, argsnum);

      for (; closnum; closnum--)
	{
	  b = env->vars;

	  env->vars = env->vars->next;

	  if (closnum == 1)
	    b->next = NULL;
	}

      env->lex_env_vars_boundary = prev_lex_bin_num;
    }
  else
    {
      ret = NULL;
    }


  if (ret && expand_and_eval)
    {
      ret2 = evaluate_object (ret, env, outcome);

      decrement_refcount (ret);

      ret = ret2;
    }

  return ret;
}


struct object *
call_structure_constructor (struct object *class_name, struct object *args,
			    struct environment *env, struct outcome *outcome)
{
  struct object *ret;
  struct structure *s;
  struct structure_field *f;
  struct structure_field_decl *fd;

  ret = alloc_object ();
  ret->type = TYPE_STRUCTURE;

  s = malloc_and_check (sizeof (*s));
  s->class_name = class_name;
  s->fields = NULL;
  ret->value_ptr.structure = s;

  fd = class_name->value_ptr.symbol->typespec->value_ptr.structure_class->fields;

  while (fd)
    {
      if (s->fields)
	f = f->next = malloc_and_check (sizeof (*f));
      else
	s->fields = f = malloc_and_check (sizeof (*f));

      f->name = fd->name;
      f->value = &nil_object;

      fd = fd->next;
    }

  if (s->fields)
    f->next = NULL;

  return ret;
}


struct object *
call_structure_accessor (struct object *class_name, struct object *field,
			 struct object *args, struct object *newval,
			 struct environment *env, struct outcome *outcome)
{
  struct structure_field *f;

  if (list_length (args) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (args)->type != TYPE_STRUCTURE
      || CAR (args)->value_ptr.structure->class_name != class_name)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  f = CAR (args)->value_ptr.structure->fields;

  while (f)
    {
      if (f->name == field)
	{
	  if (newval)
	    f->value = newval;

	  increment_refcount (f->value);
	  return f->value;
	}

      f = f->next;
    }

  return NULL;
}


struct object *
call_condition_reader (struct object *class_name, struct object *field,
		       struct object *args, struct environment *env,
		       struct outcome *outcome)
{
  struct condition_field *f;

  if (list_length (args) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (args)->type != TYPE_CONDITION
      || !is_subtype (CAR (args)->value_ptr.condition->class_name, class_name,
		      NULL))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  f = CAR (args)->value_ptr.condition->fields;

  while (f)
    {
      if (f->name == field)
	{
	  increment_refcount (f->value);
	  return f->value;
	}

      f = f->next;
    }

  return NULL;
}


int
is_class_completely_defined (struct object *class)
{
  struct object_list *p;

  if (class->type == TYPE_STANDARD_CLASS)
    {
      p = class->value_ptr.standard_class->parents;
    }
  else if (IS_SYMBOL (class) && SYMBOL (class)->value_ptr.symbol->typespec
	   && SYMBOL (class)->value_ptr.symbol->typespec->type
	   == TYPE_STANDARD_CLASS)
    {
      p = SYMBOL (class)->value_ptr.symbol->typespec->value_ptr.standard_class->
	parents;
    }
  else
    {
      return 0;
    }

  while (p)
    {
      if (!is_class_completely_defined (p->obj))
	return 0;

      p = p->next;
    }

  return 1;
}


int
is_method_applicable (struct object *meth, struct object *args,
		      struct environment *env, struct outcome *outcome)
{
  struct parameter *par = meth->value_ptr.method->lambda_list;
  int ret;

  while (par && par->type == REQUIRED_PARAM)
    {
      if (args->type != TYPE_CONS_PAIR)
	return 0;

      ret = check_type (CAR (args), par->typespec, env, outcome);

      if (ret <= 0)
	return ret;

      par = par->next;
      args = CDR (args);
    }

  return 1;
}


int
compare_method_specificity (struct object *first, struct object *second)
{
  struct parameter *par = first->value_ptr.method->lambda_list,
    *par2 = second->value_ptr.method->lambda_list;
  int ret1, ret2;

  while (par && par->type == REQUIRED_PARAM)
    {
      ret1 = is_subtype (par->typespec, par2->typespec, NULL);
      ret2 = is_subtype (par2->typespec, par->typespec, NULL);

      if (ret1 && !ret2)
	{
	  return -1;
	}

      if (!ret1 && ret2)
	{
	  return 1;
	}

      par = par->next;
      par2 = par2->next;
    }

  return 0;
}


struct object *
dispatch_generic_function_call (struct object *func, struct object *arglist,
				struct environment *env,
				struct outcome *outcome)
{
  struct object *args = evaluate_through_list (arglist, env, outcome), *ret,
    *tmp;
  struct method_list *applm = NULL, *lapplm,
    *ml = func->value_ptr.function->methods;
  struct binding *bins;
  int argsnum, prev_lex_bin_num = env->lex_env_vars_boundary, applnum = 0, i;

  if (!args)
    return NULL;

  while (ml)
    {
      if (is_method_applicable (ml->meth, args, env, outcome))
	{
	  if (applm)
	    lapplm = lapplm->next = malloc_and_check (sizeof (*lapplm));
	  else
	    applm = lapplm = malloc_and_check (sizeof (*lapplm));

	  lapplm->meth = ml->meth;
	  lapplm->next = NULL;

	  applnum++;
	}

      ml = ml->next;
    }

  if (!applm)
    {
      outcome->type = NO_APPLICABLE_METHOD;
      return NULL;
    }


  while (applnum > 1)
    {
      lapplm = applm;

      for (i = 0; i < applnum-1; i++)
	{
	  if (compare_method_specificity (lapplm->meth, lapplm->next->meth) == 1)
	    {
	      tmp = lapplm->meth;
	      lapplm->meth = lapplm->next->meth;
	      lapplm->next->meth = tmp;
	    }

	  lapplm = lapplm->next;
	}

      applnum--;
    }


  if (parse_argument_list (args, applm->meth->value_ptr.method->lambda_list, 0, 0,
			   0, 0, env, outcome, &bins, &argsnum))
    {
      env->method_args = args;
      env->next_methods = applm->next;

      env->vars = chain_bindings (bins, env->vars, NULL, NULL);
      env->lex_env_vars_boundary += argsnum;

      ret = evaluate_body (applm->meth->value_ptr.method->body, 0, NULL, env,
			   outcome);

      env->vars = remove_bindings (env->vars, argsnum);
      env->lex_env_vars_boundary = prev_lex_bin_num;

      env->method_args = NULL;
      env->next_methods = NULL;
    }
  else
    {
      ret = NULL;
    }

  return ret;
}


int
check_type (struct object *obj, struct object *typespec, struct environment *env,
	    struct outcome *outcome)
{
  struct object *args, *sym, *res, *fun, *cons;
  int ret;

  if ((typespec->type != TYPE_CONS_PAIR || !IS_SYMBOL (CAR (typespec)))
      && !IS_SYMBOL (typespec))
    {
      outcome->type = INVALID_TYPE_SPECIFIER;
      return -1;
    }

  if (typespec->type == TYPE_CONS_PAIR)
    sym = SYMBOL (CAR (typespec));
  else
    sym = SYMBOL (typespec);


  if (sym->value_ptr.symbol->builtin_type)
    {
      return sym->value_ptr.symbol->builtin_type (obj,
						  typespec->type == TYPE_CONS_PAIR
						  ? CDR (typespec) : &nil_object,
						  env, outcome);
    }
  else
    {
      args = (typespec->type == TYPE_CONS_PAIR) ? CDR (typespec) : &nil_object;

      if (sym == env->not_sym)
	{
	  if (list_length (args) != 1)
	    {
	      outcome->type = INVALID_TYPE_SPECIFIER;
	      return -1;
	    }

	  ret = check_type (obj, CAR (args), env, outcome);

	  return (ret == -1 ? ret : !ret);
	}
      else if (sym == env->and_sym)
	{
	  while (SYMBOL (args) != &nil_object)
	    {
	      ret = check_type (obj, CAR (args), env, outcome);

	      if (ret <= 0)
		return ret;

	      args = CDR (args);
	    }

	  return 1;
	}
      else if (sym == env->or_sym)
	{
	  while (SYMBOL (args) != &nil_object)
	    {
	      ret = check_type (obj, CAR (args), env, outcome);

	      if (ret)
		return ret;

	      args = CDR (args);
	    }

	  return 0;
	}
      else if (sym == env->eql_sym)
	{
	  if (list_length (args) != 1)
	    {
	      outcome->type = INVALID_TYPE_SPECIFIER;
	      return -1;
	    }

	  if (eql_objects (CAR (args), obj) == &t_object)
	    return 1;
	  else
	    return 0;
	}
      else if (sym == env->member_sym)
	{
	  while (SYMBOL (args) != &nil_object)
	    {
	      if (eql_objects (CAR (args), obj) == &t_object)
		return 1;

	      args = CDR (args);
	    }

	  return 0;
	}
      else if (sym == env->satisfies_sym)
	{
	  if (list_length (args) != 1)
	    {
	      outcome->type = INVALID_TYPE_SPECIFIER;
	      return -1;
	    }

	  if (!IS_SYMBOL (CAR (args)) ||
	      !(fun = get_function (SYMBOL (CAR (args)), env, 1, 0, 1, 0)))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return -1;
	    }

	  cons = alloc_empty_cons_pair ();
	  cons->value_ptr.cons_pair->car = obj;
	  increment_refcount (obj);
	  cons->value_ptr.cons_pair->cdr = &nil_object;

	  res = call_function (fun, cons, 0, 0, 1, 0, 0, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  free_cons_pair (cons);

	  if (!res)
	    return -1;

	  if (SYMBOL (res) != &nil_object)
	    ret = 1;
	  else
	    ret = 0;

	  decrement_refcount (res);

	  return ret;
	}
      else if (sym->value_ptr.symbol->is_type
	       && sym->value_ptr.symbol->typespec->type == TYPE_STRUCTURE_CLASS)
	{
	  return obj->type == TYPE_STRUCTURE
	    && obj->value_ptr.structure->class_name == sym;
	}
      else if (sym->value_ptr.symbol->is_type
	       && sym->value_ptr.symbol->typespec->type == TYPE_STANDARD_CLASS)
	{
	  return obj->type == TYPE_STANDARD_OBJECT
	    && (obj->value_ptr.standard_object->class_name == sym
		|| is_descendant (NULL, obj->value_ptr.standard_object->
				  class_name->value_ptr.symbol->typespec->
				  value_ptr.standard_class->parents,
				  sym, NULL));
	}
      else if (sym->value_ptr.symbol->is_type
	       && sym->value_ptr.symbol->typespec->type == TYPE_CONDITION_CLASS)
	{
	  return obj->type == TYPE_CONDITION
	    && (obj->value_ptr.condition->class_name == sym
		|| is_descendant (NULL, obj->value_ptr.condition->class_name->
				  value_ptr.symbol->typespec->
				  value_ptr.condition_class->parents,
				  sym, NULL));
	}
      else if (sym->value_ptr.symbol->is_type)
	{
	  res = call_function (sym->value_ptr.symbol->typespec, args, 0, 0, 0, 0,
			       1, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!res)
	    return -1;

	  ret = check_type (obj, res, env, outcome);

	  decrement_refcount (res);

	  return ret;
	}
      else
	{
	  outcome->type = UNKNOWN_TYPE;
	  return -1;
	}
    }
}


int
check_type_by_char_vector (struct object *obj, char *type,
			   struct environment *env, struct outcome *outcome)
{
  return check_type (obj,
		     intern_symbol_by_char_vector (type, strlen (type), 1,
						   EXTERNAL_VISIBILITY, 0,
						   env->cl_package),
		     env, outcome);
}


int
type_starts_with (const struct object *typespec, const struct object *sym)
{
  if (SYMBOL (typespec) == sym)
    return 1;

  if (typespec->type == TYPE_CONS_PAIR && SYMBOL (CAR (typespec)) == sym)
    return 1;

  return 0;
}


int
is_subtype_by_char_vector (const struct object *first, char *second,
			   struct environment *env)
{
  return is_subtype (first,
		     intern_symbol_by_char_vector (second, strlen (second), 1,
						   EXTERNAL_VISIBILITY, 0,
						   env->cl_package), NULL);
}


int
is_descendant (const struct object *first, const struct object_list *parents,
	       const struct object *second, const struct object *prev)
{
  const struct object_list *p = parents;
  int ret;

  while (p)
    {
      if (p->obj == SYMBOL (second))
	return 1;

      p = p->next;
    }

  p = parents;

  while (p)
    {
      if (p->obj != prev)
	{
	  ret = is_subtype (p->obj, SYMBOL (second), first ? SYMBOL (first)
			    : NULL);

	  if (ret)
	    return 1;
	}

      p = p->next;
    }

  return 0;
}


int
is_subtype (const struct object *first, const struct object *second,
	    const struct object *prev)
{
  struct object_list *p;
  int ret;

  if (SYMBOL (first) == &nil_object || SYMBOL (second) == &t_object)
    return 1;

  if (IS_SYMBOL (second) && type_starts_with (first, SYMBOL (second)))
    return 1;

  if (IS_SYMBOL (first) && IS_SYMBOL (second))
    {
      p = SYMBOL (first)->value_ptr.symbol->builtin_type
	? SYMBOL (first)->value_ptr.symbol->parent_types
	: SYMBOL (first)->value_ptr.symbol->typespec->type == TYPE_CONDITION_CLASS
	? SYMBOL (first)->value_ptr.symbol->typespec->value_ptr.condition_class->
	parents : NULL;

      ret = is_descendant (SYMBOL (first), p, SYMBOL (second), prev);

      if (ret)
	return 1;
    }

  return 0;
}


int
evaluate_feature_test (const struct object *feat_test,
		       struct environment *env, struct outcome *outcome)
{
  struct object *feat_list;
  int ret;

  if (IS_SYMBOL (feat_test))
    {
      feat_list = inspect_variable_by_c_string ("*FEATURES*", env);

      while (SYMBOL (feat_list) != &nil_object)
	{
	  if (SYMBOL (CAR (feat_list)) == SYMBOL (feat_test))
	    return 1;

	  feat_list = CDR (feat_list);
	}

      return 0;
    }
  else if (feat_test->type == TYPE_CONS_PAIR)
    {
      if (symbol_equals (CAR (feat_test), "NOT", env))
	{
	  if (list_length (feat_test) != 2)
	    {
	      outcome->type = INVALID_FEATURE_TEST;
	      return -1;
	    }

	  ret = evaluate_feature_test (CAR (CDR (feat_test)), env, outcome);

	  return (ret < 0) ? ret : !ret;
	}
      else if (symbol_equals (CAR (feat_test), "AND", env))
	{
	  feat_test = CDR (feat_test);

	  while (SYMBOL (feat_test) != &nil_object)
	    {
	      ret = evaluate_feature_test (CAR (feat_test), env, outcome);

	      if (ret < 0)
		return -1;

	      if (!ret)
		return 0;

	      feat_test = CDR (feat_test);
	    }

	  return 1;
	}
      else if (symbol_equals (CAR (feat_test), "OR", env))
	{
	  feat_test = CDR (feat_test);

	  while (SYMBOL (feat_test) != &nil_object)
	    {
	      ret = evaluate_feature_test (CAR (feat_test), env, outcome);

	      if (ret < 0)
		return -1;

	      if (ret)
		return 1;

	      feat_test = CDR (feat_test);
	    }

	  return 0;
	}
      else
	{
	  outcome->type = INVALID_FEATURE_TEST;
	  return -1;
	}
    }
  else
    {
      outcome->type = INVALID_FEATURE_TEST;
      return -1;
    }
}


struct object *
evaluate_object (struct object *obj, struct environment *env,
		 struct outcome *outcome)
{
  struct binding *bind;
  struct object *sym, *ret;

  if (obj->type == TYPE_BACKQUOTE)
    {
      return apply_backquote (obj->value_ptr.next, 1, env, outcome, 1, NULL,
			      NULL);
    }
  else if (obj->type == TYPE_SYMBOL || obj->type == TYPE_SYMBOL_NAME)
    {
      sym = SYMBOL (obj);

      if (sym->value_ptr.symbol->is_const || sym->value_ptr.symbol->is_parameter
	  || sym->value_ptr.symbol->is_special)
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
		 struct environment *env, struct outcome *outcome,
		 int forbid_splicing, int *do_splice, struct object **last_pref)
{
  struct object *obj, *ret, *retform, *retcons, *reading_cons, *cons, *lastpr,
    *tmp, *lp;
  int do_spl, must_copy;

  if (form->type == TYPE_BACKQUOTE)
    {
      ret = apply_backquote (form->value_ptr.next, backts_commas_balance+1, env,
			     outcome, forbid_splicing, do_splice, last_pref);

      if (!ret)
	return NULL;

      if (do_splice && *do_splice)
	return ret;

      if (ret == form->value_ptr.next)
	{
	  increment_refcount (form);
	  decrement_refcount (form->value_ptr.next);
	  return form;
	}

      retform = alloc_prefix ('`');
      retform->value_ptr.next = ret;
      add_reference (retform, ret, 0);
      decrement_refcount (ret);

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
	  if (backts_commas_balance == 2
	      && form->value_ptr.next->type == TYPE_COMMA)
	    {
	      *last_pref = form;
	    }

	  ret = apply_backquote (form->value_ptr.next, backts_commas_balance - 1,
				 env, outcome, forbid_splicing, do_splice,
				 last_pref);

	  if (!ret)
	    return NULL;

	  if (do_splice && *do_splice)
	    return ret;

	  if (ret == form->value_ptr.next)
	    {
	      increment_refcount (form);
	      decrement_refcount (form->value_ptr.next);
	      return form;
	    }

	  retform = alloc_prefix (',');
	  retform->value_ptr.next = ret;
	  add_reference (retform, ret, 0);
	  decrement_refcount (ret);

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

	  ret = apply_backquote (obj, backts_commas_balance, env, outcome,
				 reading_cons->type == TYPE_CONS_PAIR ? 0 : 2,
				 &do_spl, &lastpr);

	  if (!ret)
	    return NULL;

	  if (do_spl && !IS_LIST (ret) &&
	      SYMBOL (CDR (reading_cons)) != &nil_object)
	    {
	      outcome->type = CANT_SPLICE_AN_ATOM_HERE;
	      return NULL;
	    }

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
	      add_reference (retcons, CAR (cons), 0);
	      decrement_refcount (CAR (cons));

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

		  ret = apply_backquote (obj, backts_commas_balance, env, outcome,
					 reading_cons->type == TYPE_CONS_PAIR
					 ? 0 : 2, &do_spl, &lastpr);

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
		      add_reference (retcons, ret, 0);
		      decrement_refcount (ret);
		    }
		  else
		    {
		      retcons->value_ptr.cons_pair->cdr = ret;
		      add_reference (retcons, ret, 1);
		      decrement_refcount (ret);
		    }
		}
	      else if (!IS_LIST (ret))
		{
		  if (lastpr)
		    {
		      tmp = copy_prefix (CAR (reading_cons), lastpr, &lp);
		      lp->value_ptr.next = ret;
		    }
		  else
		    tmp = ret;

		  if (retform)
		    retcons->value_ptr.cons_pair->cdr = tmp;
		  else
		    retform = tmp;
		}
	      else if (SYMBOL (ret) != &nil_object)
		{
		  if (do_spl == 2
		      || (SYMBOL (CDR (reading_cons)) == &nil_object && !lastpr))
		    {
		      if (retform)
			{
			  retcons->value_ptr.cons_pair->cdr = ret;
			  add_reference (retcons, ret, 1);
			  decrement_refcount (ret);
			  retcons = CDR (retcons);
			}
		      else
			retform = retcons = ret;

		      if (lastpr)
			{
			  while (SYMBOL (retcons) != &nil_object)
			    {
			      increment_refcount (CAR (retcons));
			      delete_reference (retcons, CAR (retcons), 0);
			      tmp = CAR (retcons);

			      retcons->value_ptr.cons_pair->car =
				copy_prefix (CAR (reading_cons), lastpr, &lp);
			      add_reference (retcons, CAR (retcons), 0);
			      decrement_refcount (CAR (retcons));

			      lp->value_ptr.next = tmp;
			      add_reference (lp, tmp, 0);
			      decrement_refcount (tmp);

			      if (SYMBOL (CDR (retcons)) == &nil_object)
				break;
			      else
				retcons = CDR (retcons);
			    }
			}
		      else
			retcons = last_cons_pair (ret);
		    }
		  else
		    {
		      cons = ret;

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
				copy_prefix (CAR (reading_cons), lastpr, &lp);
			      add_reference (retcons, CAR (retcons), 0);
			      decrement_refcount (CAR (retcons));
			      lp->value_ptr.next = CAR (cons);
			      add_reference (lp, CAR (cons), 0);
			    }
			  else
			    {
			      retcons->value_ptr.cons_pair->car = CAR (cons);
			      add_reference (retcons, CAR (cons), 0);
			    }

			  cons = CDR (cons);
			}

		      decrement_refcount (ret);
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
	  else if (retform->type == TYPE_CONS_PAIR
		   && retcons->type == TYPE_CONS_PAIR
		   && !retcons->value_ptr.cons_pair->cdr)
	    retcons->value_ptr.cons_pair->cdr = &nil_object;
	}
      else
	{
	  retform = form;

	  while (form->type == TYPE_CONS_PAIR)
	    {
	      decrement_refcount (CAR (form));

	      form = CDR (form);

	      if (form->type != TYPE_CONS_PAIR)
		decrement_refcount (form);
	    }

	  increment_refcount (retform);
	}

      return retform;
    }

  increment_refcount (form);
  return form;
}


struct object *
evaluate_list (struct object *list, struct environment *env,
	       struct outcome *outcome)
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
  else if ((bind = find_binding (sym->value_ptr.symbol, env->funcs,
				 LEXICAL_BINDING, env->lex_env_funcs_boundary)))
    {
      fun = bind->obj;
    }
  else
    {
      fun = sym->value_ptr.symbol->function_cell;
    }

  if (fun && fun->type == TYPE_FUNCTION)
    return call_function (fun, CDR (list), 1, 0, 1, 0, 0, env, outcome);
  else if (fun && fun->value_ptr.macro->builtin_form)
    return call_function (fun, CDR (list), 0, 0, 0, 0, 0, env, outcome);
  else if (fun)
    return call_function (fun, list, 0, 1, 0, 1, 0, env, outcome);

  outcome->type = UNKNOWN_FUNCTION;
  outcome->obj = CAR (list);
  return NULL;
}


struct object *
evaluate_through_list (struct object *list, struct environment *env,
		       struct outcome *outcome)
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
      add_reference (cons, obj, 0);
      decrement_refcount (obj);

      if (!args)
	args = last_cons = cons;
      else
	{
	  last_cons->value_ptr.cons_pair->cdr = cons;
	  add_reference (last_cons, cons, 1);
	  decrement_refcount (cons);

	  last_cons = cons;
	}

      list = CDR (list);
    }

  if (!args)
    return &nil_object;

  last_cons->value_ptr.cons_pair->cdr = &nil_object;

  return args;
}


int
type_t (const struct object *obj, const struct object *typespec,
	struct environment *env, struct outcome *outcome)
{
  return 1;
}


int
type_nil (const struct object *obj, const struct object *typespec,
	  struct environment *env, struct outcome *outcome)
{
  return 0;
}


int type_null (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome)
{
  return SYMBOL (obj) == &nil_object;
}


int
type_cons (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_CONS_PAIR;
}


int
type_atom (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct outcome *outcome)
{
  return obj->type != TYPE_CONS_PAIR;
}


int
type_list (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_CONS_PAIR || SYMBOL (obj) == &nil_object;
}


int
type_symbol (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_SYMBOL || obj->type == TYPE_SYMBOL_NAME;
}


int
type_keyword (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct outcome *outcome)
{
  return IS_SYMBOL (obj)
    && SYMBOL (obj)->value_ptr.symbol->home_package == env->keyword_package;
}


int
type_boolean (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct outcome *outcome)
{
  return SYMBOL (obj) == &nil_object || SYMBOL (obj) == &t_object;
}


int
type_function (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FUNCTION;
}


int
type_package (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_PACKAGE;
}


int
type_number (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_INTEGER || obj->type == TYPE_FIXNUM
    || obj->type == TYPE_RATIO || obj->type == TYPE_FLOAT
    || obj->type == TYPE_COMPLEX;
}


int
type_real (const struct object *obj, const struct object *typespec,
	   struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_INTEGER || obj->type == TYPE_FIXNUM
    || obj->type == TYPE_RATIO || obj->type == TYPE_FLOAT;
}


int
type_rational (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_INTEGER || obj->type == TYPE_FIXNUM
    || obj->type == TYPE_RATIO;
}


int
type_integer (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct outcome *outcome)
{
  int l = list_length (typespec);

  if (l > 2)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return -1;
    }

  if ((l > 0 && !symbol_equals (CAR (typespec), "*", NULL)
       && CAR (typespec)->type != TYPE_INTEGER)
      || (l > 1 && !symbol_equals (CAR (CDR (typespec)), "*", NULL)
	  && CAR (CDR (typespec))->type != TYPE_INTEGER))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return -1;
    }

  if (obj->type != TYPE_INTEGER && obj->type != TYPE_FIXNUM)
    return 0;

  if (l > 0 && CAR (typespec)->type == TYPE_INTEGER
      && mpz_cmp (CAR (typespec)->value_ptr.integer, obj->value_ptr.integer) > 0)
    return 0;

  if (l == 2 && CAR (CDR (typespec))->type == TYPE_INTEGER
      && mpz_cmp (obj->value_ptr.integer,
		  CAR (CDR (typespec))->value_ptr.integer) > 0)
    return 0;

  return 1;
}


int
type_bignum (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_INTEGER;
}


int
type_fixnum (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FIXNUM;
}


int
type_bit (const struct object *obj, const struct object *typespec,
	  struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_INTEGER && is_bit (obj);
}


int
type_ratio (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_RATIO;
}


int
type_float (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FLOAT;
}


int
type_short_float (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FLOAT;
}


int
type_single_float (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FLOAT;
}


int
type_double_float (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FLOAT;
}


int
type_long_float (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FLOAT;
}


int
type_complex (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_COMPLEX;
}


int
type_random_state (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_RANDOM_STATE;
}


int
type_character (const struct object *obj, const struct object *typespec,
		struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_CHARACTER;
}


int
type_standard_char (const struct object *obj, const struct object *typespec,
		    struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_CHARACTER && strlen (obj->value_ptr.character) == 1
    && strchr ("\n aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ12345678"
	       "90!$\"'(),_-./:;?+<=>#%&*@[\\]{|}`^~", *obj->value_ptr.character);
}


int
type_vector (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct outcome *outcome)
{
  return (obj->type == TYPE_ARRAY
	  && array_rank (obj->value_ptr.array->alloc_size) == 1)
    || obj->type == TYPE_STRING;
}


int
type_simple_vector (const struct object *obj, const struct object *typespec,
		    struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_ARRAY && obj->value_ptr.array->fill_pointer < 0;
}


int
type_array (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_ARRAY || obj->type == TYPE_STRING
    || obj->type == TYPE_BITARRAY;
}


int
type_simple_array (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct outcome *outcome)
{
  return (obj->type == TYPE_ARRAY && obj->value_ptr.array->fill_pointer < 0)
    || (obj->type == TYPE_STRING && obj->value_ptr.string->fill_pointer < 0)
    || (obj->type == TYPE_BITARRAY && obj->value_ptr.bitarray->fill_pointer < 0);
}


int
type_sequence (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_ARRAY || obj->type == TYPE_STRING
    || obj->type == TYPE_CONS_PAIR || SYMBOL (obj) == &nil_object;
}


int
type_string (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STRING;
}


int
type_simple_string (const struct object *obj, const struct object *typespec,
		    struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STRING && obj->value_ptr.string->fill_pointer < 0;
}


int
type_bit_vector (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome)
{
  return IS_VECTOR (obj) && obj->type == TYPE_BITARRAY;
}


int
type_simple_bit_vector (const struct object *obj, const struct object *typespec,
			struct environment *env, struct outcome *outcome)
{
  return IS_VECTOR (obj) && obj->type == TYPE_BITARRAY && !HAS_FILL_POINTER (obj);
}


int
type_hash_table (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_HASHTABLE;
}


int
type_pathname (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FILENAME;
}


int
type_stream (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STREAM;
}


int
type_file_stream (const struct object *obj, const struct object *typespec,
		  struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STREAM
    && obj->value_ptr.stream->medium == FILE_STREAM;
}


int
type_string_stream (const struct object *obj, const struct object *typespec,
		    struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STREAM
    && obj->value_ptr.stream->medium == STRING_STREAM;
}


int
type_standard_object (const struct object *obj, const struct object *typespec,
		      struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STANDARD_OBJECT || obj->type == TYPE_STANDARD_CLASS
    || obj->type == TYPE_STRUCTURE_CLASS;
}


int
type_class (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STANDARD_CLASS || obj->type == TYPE_STRUCTURE_CLASS;
}


int
type_structure_class (const struct object *obj, const struct object *typespec,
		      struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STRUCTURE_CLASS;
}


int
type_standard_class (const struct object *obj, const struct object *typespec,
		     struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STANDARD_CLASS;
}


int
type_type_error (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome)
{
  return 0;
}


int
type_file_error (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome)
{
  return 0;
}


int
type_division_by_zero (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct outcome *outcome)
{
  return 0;
}


int
type_arithmetic_error (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct outcome *outcome)
{
  return 0;
}


int
type_error (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct outcome *outcome)
{
  return 0;
}


int
type_serious_condition (const struct object *obj, const struct object *typespec,
			struct environment *env, struct outcome *outcome)
{
  return 0;
}


int
type_condition (const struct object *obj, const struct object *typespec,
		struct environment *env, struct outcome *outcome)
{
  return 0;
}


int
type_restart (const struct object *obj, const struct object *typespec,
	      struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FUNCTION;
}


struct object *
builtin_car (struct object *list, struct environment *env,
	     struct outcome *outcome)
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
	     struct outcome *outcome)
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
		struct outcome *outcome)
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

  delete_reference (CAR (list), CAR (CAR (list)), 0);
  CAR (list)->value_ptr.cons_pair->car = CAR (CDR (list));
  add_reference (CAR (list), CAR (CDR (list)), 0);

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_rplacd (struct object *list, struct environment *env,
		struct outcome *outcome)
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

  delete_reference (CAR (list), CDR (CAR (list)), 1);
  CAR (list)->value_ptr.cons_pair->cdr = CAR (CDR (list));
  add_reference (CAR (list), CAR (CDR (list)), 1);

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_cons (struct object *list, struct environment *env,
	      struct outcome *outcome)
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
  add_reference (cons, CAR (list), 0);

  cons->value_ptr.cons_pair->cdr = CAR (CDR (list));
  add_reference (cons, CAR (CDR (list)), 1);

  return cons;
}


struct object *
builtin_list (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  struct object *l = NULL, *cons, *last_cons;

  while (SYMBOL (list) != &nil_object)
    {
      cons = alloc_empty_cons_pair ();

      cons->value_ptr.cons_pair->car = CAR (list);
      add_reference (cons, CAR (cons), 0);

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
		   struct outcome *outcome)
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

      cons->value_ptr.cons_pair->car = CAR (list);
      add_reference (cons, CAR (cons), 0);

      if (!l)
	l = last_cons = cons;
      else
	last_cons = last_cons->value_ptr.cons_pair->cdr = cons;

      list = CDR (list);

      if (CDR (list) == &nil_object)
	break;
    }

  cons->value_ptr.cons_pair->cdr = CAR (list);
  add_reference (cons, CDR (cons), 1);

  return l;
}


struct object *
builtin_append (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  int length = list_length (list), i;
  struct object *obj;
  struct object *ret = &nil_object, *last = NULL, *l;

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

      if (SYMBOL (obj) == &nil_object)
	continue;

      if (last)
	{
	  last->value_ptr.cons_pair->cdr = copy_list_structure (obj, NULL, -1,
								&l);
	  last = l;
	}
      else
	ret = copy_list_structure (obj, NULL, -1, &last);
    }

  obj = nth (length - 1, list);

  if (last)
    {
      last->value_ptr.cons_pair->cdr = obj;
      add_reference (last, CDR (last), 1);
    }
  else
    {
      ret = obj;
      increment_refcount (obj);
    }

  return ret;
}


struct object *
builtin_nconc (struct object *list, struct environment *env,
	       struct outcome *outcome)
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
	{
	  lastcons->value_ptr.cons_pair->cdr = CAR (argcons);
	  add_reference (lastcons, CAR (argcons), 1);
	}

      if (CAR (argcons)->type == TYPE_CONS_PAIR)
	lastcons = last_cons_pair (CAR (argcons));

      argcons = CDR (argcons);
    }

  return CAR (list);
}


struct object *
builtin_nth (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_INTEGER
      || mpz_cmp_si (CAR (list)->value_ptr.integer, 0) < 0
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
		struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_INTEGER
      || mpz_cmp_si (CAR (list)->value_ptr.integer, 0) < 0
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
		   struct outcome *outcome)
{
  int ind;
  struct object *res, *ret;
  struct object_list *l;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

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
	     struct outcome *outcome)
{
  fixnum ind;
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

  if (mpz_cmp_si (CAR (CDR (list))->value_ptr.integer, 0) < 0)
    {
      outcome->type = OUT_OF_BOUND_INDEX;
      return NULL;
    }

  ind = mpz_get_si (CAR (CDR (list))->value_ptr.integer);

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
      if (array_rank (CAR (list)->value_ptr.array->alloc_size) != 1)
	{
	  outcome->type = WRONG_NUMBER_OF_AXIS;
	  return NULL;
	}

      if (ind >= (CAR (list)->value_ptr.array->fill_pointer >= 0
		  ? CAR (list)->value_ptr.array->fill_pointer
		  : CAR (list)->value_ptr.array->alloc_size->size))
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      ret = CAR (list)->value_ptr.array->value [ind];

      increment_refcount (ret);
      return ret;
    }
  else if (CAR (list)->type == TYPE_BITARRAY)
    {
      if (array_rank (CAR (list)->value_ptr.bitarray->alloc_size) != 1)
	{
	  outcome->type = WRONG_NUMBER_OF_AXIS;
	  return NULL;
	}

      if (ind >= (CAR (list)->value_ptr.bitarray->fill_pointer >= 0
		  ? CAR (list)->value_ptr.bitarray->fill_pointer
		  : CAR (list)->value_ptr.bitarray->alloc_size->size))
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      return create_integer_from_long
	(mpz_tstbit (CAR (list)->value_ptr.bitarray->value, ind));
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
	      struct outcome *outcome)
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

      ret = get_nth_character (arr, ind);

      if (!ret)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      return ret;
    }
  else if (arr->type == TYPE_ARRAY || arr->type == TYPE_BITARRAY)
    {
      lin_ind = builtin_array_row_major_index (list, env, outcome);

      if (!lin_ind)
	return NULL;

      ind = mpz_get_si (lin_ind->value_ptr.integer);

      decrement_refcount (lin_ind);

      if (arr->type == TYPE_ARRAY)
	{
	  increment_refcount (arr->value_ptr.array->value [ind]);
	  return arr->value_ptr.array->value [ind];
	}
      else
	return create_integer_from_long
	  (mpz_tstbit (arr->value_ptr.bitarray->value, ind));
    }

  outcome->type = WRONG_TYPE_OF_ARGUMENT;
  return NULL;
}


struct object *
builtin_row_major_aref (struct object *list, struct environment *env,
			struct outcome *outcome)
{
  int ind;
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_ARRAY (CAR (list)) || CAR (CDR (list))->type != TYPE_INTEGER)
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

  if (ind < 0
      || ind >= array_total_size (CAR (list)->type == TYPE_ARRAY
				  ? CAR (list)->value_ptr.array->alloc_size
				  : CAR (list)->value_ptr.bitarray->alloc_size))
    {
      outcome->type = OUT_OF_BOUND_INDEX;
      return NULL;
    }

  if (CAR (list)->type == TYPE_ARRAY)
    {
      increment_refcount (CAR (list)->value_ptr.array->value [ind]);
      return CAR (list)->value_ptr.array->value [ind];
    }

  return create_integer_from_long
    (mpz_tstbit (CAR (list)->value_ptr.bitarray->value, ind));
}


struct object *
builtin_copy_list (struct object *list, struct environment *env,
		   struct outcome *outcome)
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

  if (SYMBOL (CAR (list)) == &nil_object)
    return &nil_object;

  return copy_list_structure (CAR (list), NULL, -1, NULL);
}


struct object *
builtin_copy_seq (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *ret, *cons, *retcons;
  int i;

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

  if (CAR (list)->type == TYPE_STRING)
    {
      ret = alloc_string (CAR (list)->value_ptr.string->used_size);
      ret->value_ptr.string->used_size = CAR (list)->value_ptr.string->used_size;

      for (i = 0; i < ret->value_ptr.string->used_size; i++)
	{
	  ret->value_ptr.string->value [i] =
	    CAR (list)->value_ptr.string->value [i];
	}
    }
  else if (CAR (list)->type == TYPE_ARRAY)
    {
      ret = alloc_vector (CAR (list)->value_ptr.array->alloc_size->size, 0, 0);

      for (i = 0; i < ret->value_ptr.array->alloc_size->size; i++)
	{
	  ret->value_ptr.array->value [i] =
	    CAR (list)->value_ptr.array->value [i];

	  add_reference (ret, ret->value_ptr.array->value [i], i);
	}
    }
  else
    {
      if (SYMBOL (CAR (list)) == &nil_object)
	return &nil_object;

      cons = CAR (list);
      retcons = ret = alloc_empty_list (list_length (CAR (list)));

      while (SYMBOL (cons) != &nil_object)
	{
	  retcons->value_ptr.cons_pair->car = CAR (cons);
	  add_reference (retcons, CAR (retcons), 0);

	  cons = CDR (cons);
	  retcons = CDR (retcons);
	}
    }

  return ret;
}


struct object *
builtin_subseq (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  int l = list_length (list);
  fixnum i, beg, end, len;
  struct object *ret, *cons;

  if (l != 2 && l != 3)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_SEQUENCE (CAR (list)) || CAR (CDR (list))->type != TYPE_INTEGER
      || (l == 3 && SYMBOL (CAR (CDR (CDR (list)))) != &nil_object
	  && CAR (CDR (CDR (list)))->type != TYPE_INTEGER))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      if (mpz_cmp_si (CAR (CDR (list))->value_ptr.integer, 0) < 0
	  || mpz_cmp_si (CAR (CDR (list))->value_ptr.integer,
			 CAR (list)->value_ptr.string->used_size) > 0)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      if (l == 3 && SYMBOL (CAR (CDR (CDR (list)))) != &nil_object
	  && mpz_cmp (CAR (CDR (list))->value_ptr.integer,
		      CAR (CDR (CDR (list)))->value_ptr.integer) > 0)
	{
	  outcome->type = DECREASING_INTERVAL_NOT_MEANINGFUL;
	  return NULL;
	}

      if (l == 3 && SYMBOL (CAR (CDR (CDR (list)))) != &nil_object
	  && mpz_cmp_si (CAR (CDR (CDR (list)))->value_ptr.integer,
			 CAR (list)->value_ptr.string->used_size) > 0)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      beg = mpz_get_si (CAR (CDR (list))->value_ptr.integer);
      end = (l == 2 || SYMBOL (CAR (CDR (CDR (list)))) == &nil_object)
	? CAR (list)->value_ptr.string->used_size
	: mpz_get_si (CAR (CDR (CDR (list)))->value_ptr.integer);

      ret = alloc_string (end-beg);
      ret->value_ptr.string->used_size = end-beg;

      for (i = 0; i<end-beg; i++)
	{
	  ret->value_ptr.string->value [i] =
	    CAR (list)->value_ptr.string->value [beg+i];
	}
    }
  else if (CAR (list)->type == TYPE_ARRAY)
    {
      if (mpz_cmp_si (CAR (CDR (list))->value_ptr.integer, 0) < 0
	  || mpz_cmp_si (CAR (CDR (list))->value_ptr.integer,
			 CAR (list)->value_ptr.array->alloc_size->size) > 0)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      if (l == 3 && SYMBOL (CAR (CDR (CDR (list)))) != &nil_object
	  && mpz_cmp (CAR (CDR (list))->value_ptr.integer,
		      CAR (CDR (CDR (list)))->value_ptr.integer) > 0)
	{
	  outcome->type = DECREASING_INTERVAL_NOT_MEANINGFUL;
	  return NULL;
	}

      if (l == 3 && SYMBOL (CAR (CDR (CDR (list)))) != &nil_object
	  && mpz_cmp_si (CAR (CDR (CDR (list)))->value_ptr.integer,
			 CAR (list)->value_ptr.array->alloc_size->size) > 0)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      beg = mpz_get_si (CAR (CDR (list))->value_ptr.integer);
      end = (l == 2 || SYMBOL (CAR (CDR (CDR (list)))) == &nil_object)
	? CAR (list)->value_ptr.array->alloc_size->size
	: mpz_get_si (CAR (CDR (CDR (list)))->value_ptr.integer);

      ret = alloc_vector (end-beg, 0, 0);

      for (i = 0; i<end-beg; i++)
	{
	  ret->value_ptr.array->value [i] =
	    CAR (list)->value_ptr.array->value [beg+i];

	  add_reference (ret, ret->value_ptr.array->value [i], i);
	}
    }
  else
    {
      if (SYMBOL (CAR (list)) == &nil_object)
	len = 0;
      else
	len = list_length (CAR (list));

      if (mpz_cmp_si (CAR (CDR (list))->value_ptr.integer, 0) < 0
	  || mpz_cmp_si (CAR (CDR (list))->value_ptr.integer, len) > 0)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      if (l == 3 && SYMBOL (CAR (CDR (CDR (list)))) != &nil_object
	  && mpz_cmp (CAR (CDR (list))->value_ptr.integer,
		      CAR (CDR (CDR (list)))->value_ptr.integer) > 0)
	{
	  outcome->type = DECREASING_INTERVAL_NOT_MEANINGFUL;
	  return NULL;
	}

      if (l == 3 && SYMBOL (CAR (CDR (CDR (list)))) != &nil_object
	  && mpz_cmp_si (CAR (CDR (CDR (list)))->value_ptr.integer, len) > 0)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      beg = mpz_get_si (CAR (CDR (list))->value_ptr.integer);
      end = (l == 2 || SYMBOL (CAR (CDR (CDR (list)))) == &nil_object) ? len
	: mpz_get_si (CAR (CDR (CDR (list)))->value_ptr.integer);

      if (end == beg)
	ret = &nil_object;
      else
	cons = ret = alloc_empty_list (end-beg);

      for (; beg<end; beg++)
	{
	  cons->value_ptr.cons_pair->car = nth (beg, CAR (list));
	  add_reference (cons, cons->value_ptr.cons_pair->car, 0);

	  cons = CDR (cons);
	}
    }

  return ret;
}


struct object *
builtin_list_length (struct object *list, struct environment *env,
		     struct outcome *outcome)
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
		struct outcome *outcome)
{
  struct object *seq;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  seq = CAR (list);

  if (seq->type == TYPE_STRING)
    {
      return create_integer_from_long (string_utf8_length (seq));
    }
  else if (seq->type == TYPE_CONS_PAIR || SYMBOL (seq) == &nil_object)
    {
      return create_integer_from_long (list_length (seq));
    }
  else if (seq->type == TYPE_ARRAY &&
	   array_rank (seq->value_ptr.array->alloc_size) == 1)
    {
      if (seq->value_ptr.array->fill_pointer >= 0)
	return create_integer_from_long (seq->value_ptr.array->fill_pointer);

      return create_integer_from_long (seq->value_ptr.array->alloc_size->size);
    }
  else if (seq->type == TYPE_BITARRAY &&
	   array_rank (seq->value_ptr.bitarray->alloc_size) == 1)
    {
      if (seq->value_ptr.bitarray->fill_pointer >= 0)
	return create_integer_from_long (seq->value_ptr.bitarray->fill_pointer);

      return create_integer_from_long (seq->value_ptr.bitarray->alloc_size->size);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }
}


struct object *
builtin_fill_pointer (struct object *list, struct environment *env,
		      struct outcome *outcome)
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
		    struct outcome *outcome)
{
  int indx, tot = 1, fillp = -1;
  struct object *ret, *cons, *dims, *fp = NULL;
  struct array_size *size = NULL, *sz;

  if (!list_length (list))
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  dims = CAR (list);
  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":FILL-POINTER", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (CAR (CDR (list))->type == TYPE_INTEGER
	      || SYMBOL (CAR (CDR (list))) == &t_object
	      || SYMBOL (CAR (CDR (list))) == &nil_object)
	    {
	      if (!fp)
		fp = CAR (CDR (list));
	    }
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

  if (fp && fp->type == TYPE_INTEGER)
    {
      fillp = mpz_get_si (fp->value_ptr.integer);

      if (fillp < 0)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}
    }

  if (dims->type == TYPE_INTEGER)
    {
      indx = mpz_get_si (dims->value_ptr.integer);

      if (indx < 0)
	{
	  outcome->type = INVALID_SIZE;
	  return NULL;
	}

      if (fp && fillp > indx)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = alloc_vector (indx, 1, 0);

      if (fp && SYMBOL (fp) == &t_object)
	ret->value_ptr.array->fill_pointer = indx;
      else
	ret->value_ptr.array->fill_pointer = fillp;

      return ret;
    }
  else if (dims->type == TYPE_CONS_PAIR)
    {
      cons = dims;

      if (fp && list_length (dims) != 1)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      while (SYMBOL (cons) != &nil_object)
	{
	  if (CAR (cons)->type != TYPE_INTEGER)
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

      if (fp && fillp > sz->size)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = alloc_vector (tot, 1, 1);

      if (fp && SYMBOL (fp) == &t_object)
	ret->value_ptr.array->fill_pointer = indx;
      else
	ret->value_ptr.array->fill_pointer = fillp;

      sz->next = NULL;
      ret->value_ptr.array->alloc_size = size;
    }
  else if (SYMBOL (dims) == &nil_object)
    {
      if (fp)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

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
builtin_vector (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  return create_vector_from_list (list);
}


struct object *
builtin_array_has_fill_pointer_p (struct object *list, struct environment *env,
				  struct outcome *outcome)
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
			  struct outcome *outcome)
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

      if (!sz)
	return &nil_object;

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
builtin_array_row_major_index (struct object *list, struct environment *env,
			       struct outcome *outcome)
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

      if (CAR (list)->type != TYPE_INTEGER)
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

  if (arr->type == TYPE_ARRAY)
    sz = arr->value_ptr.array->alloc_size;
  else
    sz = arr->value_ptr.bitarray->alloc_size;

  tot = 0;

  while (SYMBOL (list) != &nil_object)
    {
      if (!sz)
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
builtin_adjust_array (struct object *list, struct environment *env,
		      struct outcome *outcome)
{
  fixnum newsz, oldsz, i, newchsz;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if ((CAR (CDR (list))->type != TYPE_CONS_PAIR
       || CAR (CAR (CDR (list)))->type != TYPE_INTEGER)
      && CAR (CDR (list))->type != TYPE_INTEGER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  newsz = CAR (CDR (list))->type == TYPE_CONS_PAIR
    ? mpz_get_si (CAR (CAR (CDR (list)))->value_ptr.integer)
    : mpz_get_si (CAR (CDR (list))->value_ptr.integer);

  if (CAR (list)->type == TYPE_STRING)
    {
      oldsz = string_utf8_length (CAR (list));

      if (oldsz > newsz)
	{
	  if (!newsz)
	    {
	      newchsz = 0;
	    }
	  else
	    {
	      newchsz = get_nth_character_offset (CAR (list), newsz);
	    }
	}

      if (newsz > oldsz)
	{
	  newchsz = CAR (list)->value_ptr.string->used_size + (newsz-oldsz);
	}

      if (newchsz > CAR (list)->value_ptr.string->alloc_size)
	{
	  CAR (list)->value_ptr.string->value =
	    realloc_and_check (CAR (list)->value_ptr.string->value, newchsz);
	  CAR (list)->value_ptr.string->alloc_size = newchsz;
	}

      for (i = CAR (list)->value_ptr.string->used_size; i < newchsz; i++)
	{
	  CAR (list)->value_ptr.string->value [i] = 0;
	}

      CAR (list)->value_ptr.string->used_size = newchsz;
    }
  else if (CAR (list)->type == TYPE_ARRAY)
    {
      if (!CAR (list)->value_ptr.array->alloc_size)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (CAR (list)->value_ptr.array->alloc_size->size > newsz)
	{
	  for (i = newsz; i < CAR (list)->value_ptr.array->alloc_size->size; i++)
	    {
	      delete_reference (CAR (list), CAR (list)->value_ptr.array->value [i],
				i);
	    }
	}

      CAR (list)->value_ptr.array->value =
	realloc_and_check (CAR (list)->value_ptr.array->value, newsz
			   * sizeof (*CAR (list)->value_ptr.array->value));

      if (CAR (list)->value_ptr.array->alloc_size->size < newsz)
	{
	  for (i = CAR (list)->value_ptr.array->alloc_size->size; i < newsz; i++)
	    {
	      CAR (list)->value_ptr.array->value [i] = &nil_object;
	    }
	}

      CAR (list)->value_ptr.array->alloc_size->size = newsz;

      CAR (list)->value_ptr.array->reference_strength_factor =
	realloc_and_check (CAR (list)->value_ptr.array->reference_strength_factor,
			   newsz * sizeof (int));
    }
  else if (CAR (list)->type == TYPE_BITARRAY)
    {
      if (!CAR (list)->value_ptr.array->alloc_size)
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

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_make_hash_table (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  struct object *ret;
  struct hashtable *ht;
  enum hashtable_type type = HT_NONE;

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":TEST", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (symbol_equals (CAR (CDR (list)), "EQ", env))
	    {
	      if (!type)
		type = HT_EQ;
	    }
	  else if (symbol_equals (CAR (CDR (list)), "EQL", env))
	    {
	      if (!type)
		type = HT_EQL;
	    }
	  else if (symbol_equals (CAR (CDR (list)), "EQUAL", env))
	    {
	      if (!type)
		type = HT_EQUAL;
	    }
	  else if (symbol_equals (CAR (CDR (list)), "EQUALP", env))
	    {
	      if (!type)
		type = HT_EQUALP;
	    }
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

  if (type == HT_NONE)
    type = HT_EQL;

  ret = alloc_object ();
  ht = malloc_and_check (sizeof (*ht));

  ht->type = type;
  ht->table = calloc_and_check (LISP_HASHTABLE_SIZE, sizeof (*ht->table));

  ret->type = TYPE_HASHTABLE;
  ret->value_ptr.hashtable = ht;

  return ret;
}


struct object *
builtin_hash_table_size (struct object *list, struct environment *env,
			 struct outcome *outcome)
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
			  struct outcome *outcome)
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
			 struct outcome *outcome)
{
  struct object *ret;
  enum hashtable_type t;

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

  t = CAR (list)->value_ptr.hashtable->type;

  switch (t)
    {
    case HT_EQ:
      ret = intern_symbol_by_char_vector ("EQ", strlen ("EQ"), 1,
					  EXTERNAL_VISIBILITY, 1, env->cl_package);
      break;
    case HT_EQL:
      ret = intern_symbol_by_char_vector ("EQL", strlen ("EQL"), 1,
					  EXTERNAL_VISIBILITY, 1, env->cl_package);
      break;
    case HT_EQUAL:
      ret = intern_symbol_by_char_vector ("EQUAL", strlen ("EQUAL"), 1,
					  EXTERNAL_VISIBILITY, 1, env->cl_package);
      break;
    case HT_EQUALP:
      ret = intern_symbol_by_char_vector ("EQUALP", strlen ("EQUALP"), 1,
					  EXTERNAL_VISIBILITY, 1, env->cl_package);
      break;
    default:
      break;
    }

  increment_refcount (ret);

  return ret;
}


struct object *
builtin_gethash (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  struct object *ret, *pres = &t_object;
  struct hashtable_record *r;
  int ind;

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

  r = find_hashtable_record (CAR (list), CAR (CDR (list)), &ind, NULL, NULL);

  if (!r)
    {
      ret = &nil_object;
      pres = &nil_object;
    }
  else
    ret = r->value;

  increment_refcount (ret);
  prepend_object_to_obj_list (pres, &outcome->other_values);
  return ret;
}


struct object *
builtin_remhash (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  struct hashtable_record *r, *prev;
  int ind, j;

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

  r = find_hashtable_record (CAR (list), CAR (CDR (list)), &ind, &j, &prev);

  if (r)
    {
      delete_reference (CAR (CDR (list)), r->key, ind+j*2*LISP_HASHTABLE_SIZE);
      delete_reference (CAR (CDR (list)), r->value, ind+(j*2+1)*LISP_HASHTABLE_SIZE);

      if (prev)
	prev->next = r->next;
      else
	CAR (CDR (list))->value_ptr.hashtable->table [ind] = NULL;

      free (r);

      return &t_object;
    }

  return &nil_object;
}


struct object *
builtin_clrhash (struct object *list, struct environment *env,
		 struct outcome *outcome)
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

  clear_hash_table (CAR (list));

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_maphash (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  struct object *fun, *args, *ret;
  size_t i;
  struct hashtable_record *r;

  if (list_length (list) != 2)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_SYMBOL_NAME || CAR (list)->type == TYPE_SYMBOL)
    {
      fun = get_function (SYMBOL (CAR (list)), env, 1, 0, 0, 0);

      if (!fun)
	{
	  outcome->type = UNKNOWN_FUNCTION;
	  outcome->obj = SYMBOL (CAR (list));
	  return NULL;
	}
    }
  else if (CAR (list)->type == TYPE_FUNCTION)
    {
      fun = CAR (list);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (CDR (list))->type != TYPE_HASHTABLE)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  args = alloc_empty_list (2);

  for (i = 0; i < LISP_HASHTABLE_SIZE; i++)
    {
      r = CAR (CDR (list))->value_ptr.hashtable->table [i];

      while (r)
	{
	  args->value_ptr.cons_pair->car = r->key;
	  args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = r->value;

	  ret = call_function (fun, args, 0, 0, 1, 0, 0, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!ret)
	    return NULL;

	  decrement_refcount (ret);

	  r = r->next;
	}
    }

  args->value_ptr.cons_pair->car = NULL;
  args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = NULL;
  decrement_refcount (args);

  return &nil_object;
}


struct object *
builtin_last (struct object *list, struct environment *env,
	      struct outcome *outcome)
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

  increment_refcount (ret);
  return ret;
}


struct object *
builtin_read_line (struct object *list, struct environment *env,
		   struct outcome *outcome)
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

      ret = create_string_with_char_vector (in, i);
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
	  increment_refcount (ret);
	  delete_reference (str, s->string, 0);

	  s->string = alloc_string (0);
	  add_reference (str, s->string, 0);
	  decrement_refcount (s->string);
	}
      else
	{
	  ret = s->string;
	  increment_refcount (ret);
	  delete_reference (str, s->string, 0);

	  s->string = create_string_copying_char_vector
	    (ret->value_ptr.string->value+i+1,
	     ret->value_ptr.string->used_size-i-1);
	  resize_string_allocation (ret, i);

	  add_reference (str, s->string, 0);
	  decrement_refcount (s->string);
	}
    }

  if (eof)
    prepend_object_to_obj_list (&t_object, &outcome->other_values);
  else
    prepend_object_to_obj_list (&nil_object, &outcome->other_values);

  return ret;
}


struct object *
builtin_read (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  int l = list_length (list);
  struct stream *s;
  struct object *str, *ret = NULL, *newstr;
  enum outcome_type out;
  const char *objbeg, *objend;

  if (l > 1)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (l == 1 && CAR (list)->type != TYPE_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 1)
    str = CAR (list);
  else
    str = inspect_variable (env->std_in_sym, env);

  s = str->value_ptr.stream;

  out = read_object (&ret, 0, s->medium == STRING_STREAM
		     ? s->string->value_ptr.string->value : NULL,
		     s->medium == STRING_STREAM
		     ? s->string->value_ptr.string->used_size : 0,
		     s->medium == FILE_STREAM ? s->file : NULL, 0, 1, env,
		     outcome, &objbeg, &objend);

  if (IS_READ_OR_EVAL_ERROR (out))
    {
      outcome->type = out;
      return NULL;
    }

  if (IS_INCOMPLETE_OBJECT (out))
    {
      outcome->type = GOT_EOF_IN_MIDDLE_OF_OBJECT;
      return NULL;
    }

  if (out == NO_OBJECT)
    {
      outcome->type = GOT_EOF;
      return NULL;
    }

  if (s->medium == STRING_STREAM)
    {
      newstr =
	create_string_copying_char_vector (objend + 1,
					   s->string->value_ptr.string->used_size
					   - (objend
					      - s->string->value_ptr.string->value
					      + 1));

      delete_reference (str, s->string, 0);

      s->string = newstr;
      add_reference (str, s->string, 0);
      decrement_refcount (newstr);
    }

  return ret;
}


struct object *
builtin_read_preserving_whitespace (struct object *list, struct environment *env,
				    struct outcome *outcome)
{
  int l = list_length (list);
  struct stream *s;
  struct object *str, *ret = NULL, *newstr;
  enum outcome_type out;
  const char *objbeg, *objend;

  if (l > 1)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (l == 1 && CAR (list)->type != TYPE_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 1)
    str = CAR (list);
  else
    str = inspect_variable (env->std_in_sym, env);

  s = str->value_ptr.stream;

  out = read_object (&ret, 0, s->medium == STRING_STREAM
		     ? s->string->value_ptr.string->value : NULL,
		     s->medium == STRING_STREAM
		     ? s->string->value_ptr.string->used_size : 0,
		     s->medium == FILE_STREAM ? s->file : NULL, 1, 1, env,
		     outcome, &objbeg, &objend);

  if (IS_READ_OR_EVAL_ERROR (out))
    {
      outcome->type = out;
      return NULL;
    }

  if (IS_INCOMPLETE_OBJECT (out))
    {
      outcome->type = GOT_EOF_IN_MIDDLE_OF_OBJECT;
      return NULL;
    }

  if (out == NO_OBJECT)
    {
      outcome->type = GOT_EOF;
      return NULL;
    }

  if (s->medium == STRING_STREAM)
    {
      newstr =
	create_string_copying_char_vector (objend + 1,
					   s->string->value_ptr.string->used_size
					   - (objend
					      - s->string->value_ptr.string->value
					      + 1));

      delete_reference (str, s->string, 0);

      s->string = newstr;
      add_reference (str, s->string, 0);
      decrement_refcount (newstr);
    }

  return ret;
}


struct object *
builtin_eval (struct object *list, struct environment *env,
	      struct outcome *outcome)
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
	       struct outcome *outcome)
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
	    {
	      if (!str)
		str = CAR (CDR (list));
	    }
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
		      struct outcome *outcome)
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
		    struct outcome *outcome)
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
		    struct outcome *outcome)
{
  char b;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_INTEGER || CAR (CDR (list))->type != TYPE_STREAM)
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
		    struct outcome *outcome)
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
	      struct outcome *outcome)
{
  int l = list_length (list);
  char *fn;
  struct object *ret, *pack = inspect_variable (env->package_sym, env);

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

  set_value (env->package_sym, pack, 0, env, outcome);

  free (fn);

  return ret;
}


struct object *
builtin_open (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  enum stream_direction dir = NO_DIRECTION;
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
	    {
	      if (!dir)
		dir = INPUT_STREAM;
	    }
	  else if (symbol_equals (CAR (CDR (list)), ":OUTPUT", env))
	    {
	      if (!dir)
		dir = OUTPUT_STREAM;
	    }
	  else if (symbol_equals (CAR (CDR (list)), ":IO", env))
	    {
	      if (!dir)
		dir = BIDIRECTIONAL_STREAM;
	    }
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

  if (!dir)
    dir = INPUT_STREAM;

  return create_file_stream (BINARY_STREAM, dir, ns, outcome);
}


struct object *
builtin_close (struct object *list, struct environment *env,
	       struct outcome *outcome)
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
		       struct outcome *outcome)
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
			struct outcome *outcome)
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
			 struct outcome *outcome)
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
			      struct outcome *outcome)
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
				  struct outcome *outcome)
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
				   struct outcome *outcome)
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
				  struct outcome *outcome)
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
  increment_refcount (ret);
  delete_reference (CAR (list), ret, 0);

  CAR (list)->value_ptr.stream->string = alloc_string (0);
  add_reference (CAR (list), CAR (list)->value_ptr.stream->string, 0);
  decrement_refcount (CAR (list)->value_ptr.stream->string);

  return ret;
}


struct object *
builtin_upper_case_p (struct object *list, struct environment *env,
		      struct outcome *outcome)
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
		      struct outcome *outcome)
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
		     struct outcome *outcome)
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
	    struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  return eq_objects (CAR (list), CAR (CDR (list)));
}


struct object *
builtin_eql (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  return eql_objects (CAR (list), CAR (CDR (list)));
}


struct object *
builtin_not (struct object *list, struct environment *env,
	     struct outcome *outcome)
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
		     struct outcome *outcome)
{
  int l = list_length (list), i, j, k;
  struct object *ret, *retcons, *cons;
  fixnum len = 0;

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

  if (!SYMBOL (CAR (list))->value_ptr.symbol->is_type
      || !is_subtype_by_char_vector (CAR (list), "SEQUENCE", env)
      || SYMBOL (CAR (list)) == &nil_object)
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

  if (is_subtype_by_char_vector (CAR (list), "STRING", env))
    {
      for (i = 1; i < l; i++)
	{
	  if (nth (i, list)->type != TYPE_STRING)
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  len += nth (i, list)->value_ptr.string->used_size;
	}

      ret = alloc_string (len);
      list = CDR (list);

      for (i = 1; i < l; i++)
	{
	  memcpy (ret->value_ptr.string->value + ret->value_ptr.string->used_size,
		  CAR (list)->value_ptr.string->value,
		  CAR (list)->value_ptr.string->used_size);
	  ret->value_ptr.string->used_size +=
	    CAR (list)->value_ptr.string->used_size;

	  list = CDR (list);
	}

      return ret;
    }
  else if (is_subtype_by_char_vector (CAR (list), "VECTOR", env))
    {
      for (i = 1; i < l; i++)
	{
	  if (nth (i, list)->type != TYPE_ARRAY || !IS_VECTOR (nth (i, list)))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  len += nth (i, list)->value_ptr.array->alloc_size->size;
	}

      ret = alloc_vector (len, 0, 0);
      list = CDR (list);
      k = 0;

      for (i = 1; i < l; i++)
	{
	  for (j = 0; j < CAR (list)->value_ptr.array->alloc_size->size; j++)
	    {
	      ret->value_ptr.array->value [k++]
		= CAR (list)->value_ptr.array->value [j];
	      add_reference (ret, CAR (list)->value_ptr.array->value [j], k-1);
	    }

	  list = CDR (list);
	}

      return ret;
    }
  else
    {
      for (i = 1; i < l; i++)
	{
	  if (!IS_LIST (nth (i, list)))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }
	}

      ret = NULL;
      list = CDR (list);

      for (i = 1; i < l; i++)
	{
	  cons = CAR (list);

	  while (SYMBOL (cons) != &nil_object)
	    {
	      if (ret)
		retcons = retcons->value_ptr.cons_pair->cdr =
		  alloc_empty_cons_pair ();
	      else
		ret = retcons = alloc_empty_cons_pair ();

	      retcons->value_ptr.cons_pair->car = CAR (cons);
	      add_reference (retcons, CAR (retcons), 0);

	      cons = CDR (cons);
	    }

	  list = CDR (list);
	}

      if (ret)
	retcons->value_ptr.cons_pair->cdr = &nil_object;
      else
	ret = &nil_object;

      return ret;
    }
}


struct object *
builtin_do (struct object *list, struct environment *env,
	    struct outcome *outcome)
{
  int l = list_length (list), bin_num = 0;
  struct object *bind_forms, *test_form, *testres, *body, *bodyres, *ret, *res;
  struct object_list *incr = NULL, *lastincr;
  struct binding *bins = NULL, *bin;

  if (l < 2 || !IS_LIST (CAR (list))
      || CAR (CDR (list))->type != TYPE_CONS_PAIR)
    {
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      return NULL;
    }

  env->blocks = add_block (&nil_object, env->blocks);

  bind_forms = CAR (list);

  while (SYMBOL (bind_forms) != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome, 1);

      if (!bin)
	{
	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      ret = outcome->return_value;

	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	    }
	  else
	    ret = NULL;

	  env->vars = chain_bindings (bins, env->vars, NULL, NULL);
	  goto cleanup_and_leave;
	}

      bins = add_binding (bin, bins);
      bin_num++;

      bind_forms = CDR (bind_forms);
    }

  env->vars = chain_bindings (bins, env->vars, NULL, NULL);

  env->lex_env_vars_boundary += bin_num;

  test_form = CAR (CAR (CDR (list)));

  testres = evaluate_object (test_form, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!testres)
    {
      if (outcome->block_to_leave == &nil_object)
	{
	  outcome->block_to_leave = NULL;
	  ret = outcome->return_value;

	  outcome->no_value = outcome->return_no_value;
	  outcome->other_values = outcome->return_other_values;
	}
      else
	ret = NULL;

      goto cleanup_and_leave;
    }

  body = CDR (CDR (list));

  incr = alloc_empty_object_list (bin_num);

  while (SYMBOL (testres) == &nil_object)
    {
      decrement_refcount (testres);

      bodyres = evaluate_body (body, 1, NULL, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!bodyres)
	{
	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      ret = outcome->return_value;

	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	    }
	  else
	    ret = NULL;

	  goto cleanup_and_leave;
	}

      decrement_refcount (bodyres);

      bind_forms = CAR (list);
      lastincr = incr;

      while (SYMBOL (bind_forms) != &nil_object)
	{
	  if (list_length (CAR (bind_forms)) == 3)
	    {
	      res = evaluate_object (CAR (CDR (CDR (CAR (bind_forms)))), env,
				     outcome);

	      if (!res)
		{
		  if (outcome->block_to_leave == &nil_object)
		    {
		      outcome->block_to_leave = NULL;
		      ret = outcome->return_value;

		      outcome->no_value = outcome->return_no_value;
		      outcome->other_values = outcome->return_other_values;
		    }
		  else
		    ret = NULL;

		  goto cleanup_and_leave;
		}
	    }
	  else
	    res = NULL;

	  lastincr->obj = res;

	  bind_forms = CDR (bind_forms);
	  lastincr = lastincr->next;
	}

      bind_forms = CAR (list);
      lastincr = incr;

      while (SYMBOL (bind_forms) != &nil_object)
	{
	  if (lastincr->obj)
	    set_value (SYMBOL (CAR (CAR (bind_forms))), lastincr->obj, 0, env,
		       outcome);

	  decrement_refcount (lastincr->obj);
	  bind_forms = CDR (bind_forms);
	  lastincr = lastincr->next;
	}

      testres = evaluate_object (test_form, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!testres)
	{
	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      ret = outcome->return_value;

	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	    }
	  else
	    ret = NULL;

	  goto cleanup_and_leave;
	}
    }

  ret = evaluate_body (CDR (CAR (CDR (list))), 0, NULL, env, outcome);

  if (!ret && outcome->block_to_leave == &nil_object)
    {
      outcome->block_to_leave = NULL;
      ret = outcome->return_value;

      outcome->no_value = outcome->return_no_value;
      outcome->other_values = outcome->return_other_values;
    }

 cleanup_and_leave:
  env->vars = remove_bindings (env->vars, bin_num);

  env->lex_env_vars_boundary -= bin_num;

  env->blocks = remove_block (env->blocks);

  free_object_list_structure (incr);

  return ret;
}


struct object *
builtin_do_star (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  int l = list_length (list), bin_num = 0;
  struct object *bind_forms, *test_form, *testres, *body, *bodyres, *ret, *res;
  struct binding *bin;

  if (l < 2 || !IS_LIST (CAR (list))
      || CAR (CDR (list))->type != TYPE_CONS_PAIR)
    {
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      return NULL;
    }

  env->blocks = add_block (&nil_object, env->blocks);

  bind_forms = CAR (list);

  while (SYMBOL (bind_forms) != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome, 1);

      if (!bin)
	{
	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      ret = outcome->return_value;

	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	    }
	  else
	    ret = NULL;

	  goto cleanup_and_leave;
	}

      env->vars = add_binding (bin, env->vars);
      env->lex_env_vars_boundary++, bin_num++;

      bind_forms = CDR (bind_forms);
    }

  test_form = CAR (CAR (CDR (list)));

  testres = evaluate_object (test_form, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!testres)
    {
      if (outcome->block_to_leave == &nil_object)
	{
	  outcome->block_to_leave = NULL;
	  ret = outcome->return_value;

	  outcome->no_value = outcome->return_no_value;
	  outcome->other_values = outcome->return_other_values;
	}
      else
	ret = NULL;

      goto cleanup_and_leave;
    }

  body = CDR (CDR (list));

  while (SYMBOL (testres) == &nil_object)
    {
      decrement_refcount (testres);

      bodyres = evaluate_body (body, 1, NULL, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!bodyres)
	{
	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      ret = outcome->return_value;

	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	    }
	  else
	    ret = NULL;

	  goto cleanup_and_leave;
	}

      decrement_refcount (bodyres);

      bind_forms = CAR (list);

      while (SYMBOL (bind_forms) != &nil_object)
	{
	  if (list_length (CAR (bind_forms)) == 3)
	    {
	      res = set_value (SYMBOL (CAR (CAR (bind_forms))),
			       CAR (CDR (CDR (CAR (bind_forms)))), 1, env,
			       outcome);

	      if (!res)
		{
		  if (outcome->block_to_leave == &nil_object)
		    {
		      outcome->block_to_leave = NULL;
		      ret = outcome->return_value;

		      outcome->no_value = outcome->return_no_value;
		      outcome->other_values = outcome->return_other_values;
		    }
		  else
		    ret = NULL;

		  goto cleanup_and_leave;
		}

	      decrement_refcount (res);
	    }

	  bind_forms = CDR (bind_forms);
	}

      testres = evaluate_object (test_form, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!testres)
	{
	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      ret = outcome->return_value;

	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	    }
	  else
	    ret = NULL;

	  goto cleanup_and_leave;
	}
    }

  ret = evaluate_body (CDR (CAR (CDR (list))), 0, NULL, env, outcome);

  if (!ret && outcome->block_to_leave == &nil_object)
    {
      outcome->block_to_leave = NULL;
      ret = outcome->return_value;

      outcome->no_value = outcome->return_no_value;
      outcome->other_values = outcome->return_other_values;
    }

 cleanup_and_leave:
  env->vars = remove_bindings (env->vars, bin_num);

  env->lex_env_vars_boundary -= bin_num;

  env->blocks = remove_block (env->blocks);

  return ret;
}


struct object *
builtin_dotimes (struct object *list, struct environment *env,
		 struct outcome *outcome)
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
	  outcome->no_value = outcome->return_no_value;
	  outcome->other_values = outcome->return_other_values;
	  return outcome->return_value;
	}
      else
	return NULL;
    }

  if (count->type != TYPE_INTEGER)
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
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
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
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
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
		struct outcome *outcome)
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
	  outcome->no_value = outcome->return_no_value;
	  outcome->other_values = outcome->return_other_values;
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
	  decrement_refcount (lst);

	  if (outcome->block_to_leave == &nil_object)
	    {
	      outcome->block_to_leave = NULL;
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
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
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
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
		struct outcome *outcome)
{
  int i, l = list_length (list), finished = 0;
  struct object *fun, *cdrlist, *cdrlistcons, *args, *argscons, *ret, *retcons,
    *val;

  if (l < 2)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_SYMBOL_NAME || CAR (list)->type == TYPE_SYMBOL)
    {
      fun = get_function (SYMBOL (CAR (list)), env, 1, 0, 0, 0);

      if (!fun)
	{
	  outcome->type = UNKNOWN_FUNCTION;
	  outcome->obj = SYMBOL (CAR (list));
	  return NULL;
	}
    }
  else if (CAR (list)->type == TYPE_FUNCTION)
    {
      fun = CAR (list);
    }
  else
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

      val = call_function (fun, args, 0, 0, 1, 0, 0, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!val)
	{
	  free_list_structure (cdrlist);
	  free_list_structure (args);
	  return NULL;
	}

      retcons->value_ptr.cons_pair->car = val;
      add_reference (retcons, val, 0);
      decrement_refcount (val);

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
	{
	  retcons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	  retcons = CDR (retcons);
	}
    }

  retcons->value_ptr.cons_pair->cdr = &nil_object;

  free_list_structure (cdrlist);
  free_list_structure (args);

  return ret;
}


struct object *
builtin_map (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  int i, l = list_length (list), j, min = -1;
  struct object *fun, *args, *argscons, *ret, *val;

  if (l < 3)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)) || !SYMBOL (CAR (list))->value_ptr.symbol->is_type
      || !is_subtype_by_char_vector (CAR (list), "SEQUENCE", env))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (IS_SYMBOL (CAR (CDR (list))))
    {
      fun = get_function (SYMBOL (CAR (CDR (list))), env, 1, 0, 0, 0);

      if (!fun)
	{
	  outcome->type = UNKNOWN_FUNCTION;
	  outcome->obj = SYMBOL (CAR (CDR (list)));
	  return NULL;
	}
    }
  else if (CAR (CDR (list))->type == TYPE_FUNCTION)
    {
      fun = CAR (CDR (list));
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }


  for (i = 2; i < l; i++)
    {
      if (!IS_SEQUENCE (nth (i, list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (min == -1 || (sequence_length (nth (i, list)) < min))
	min = sequence_length (nth (i, list));

      if (!min && is_subtype_by_char_vector (CAR (list), "LIST", env))
	return &nil_object;
    }

  if (SYMBOL (CAR (list)) == &nil_object)
    ret = &nil_object;
  else if (is_subtype_by_char_vector (CAR (list), "LIST", env))
    ret = alloc_empty_list (min);
  else if (is_subtype_by_char_vector (CAR (list), "STRING", env))
    ret = alloc_string (min);
  else
    ret = alloc_vector (min, 0, 0);

  if (!min)
    return ret;

  args = alloc_empty_list (l-2);

  for (i = 0; i < min; i++)
    {
      argscons = args;

      for (j = 2; j < l; j++)
	{
	  argscons->value_ptr.cons_pair->car = elt (nth (j, list), i);
	  argscons = CDR (argscons);
	}

      val = call_function (fun, args, 0, 0, 1, 0, 0, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);


      argscons = args;

      for (j = 2; j < l; j++)
	{
	  if (nth (j, list)->type == TYPE_STRING)
	    decrement_refcount (CAR (argscons));

	  argscons = CDR (argscons);
	}


      if (!val)
	{
	  free_list_structure (args);
	  return NULL;
	}

      if (ret->type == TYPE_STRING && val->type != TYPE_CHARACTER)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (SYMBOL (CAR (list)) != &nil_object)
	set_elt (ret, i, val);

      decrement_refcount (val);
    }

  if (ret->type == TYPE_CONS_PAIR)
    last_cons_pair (ret)->value_ptr.cons_pair->cdr = &nil_object;

  if (ret->type == TYPE_STRING)
    ret->value_ptr.string->used_size = min;

  free_list_structure (args);

  return ret;
}


struct object *
builtin_remove_if (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  struct object *fun, *seq, *ret, *cons, *arg, *res;
  fixnum sz, off, i, j;
  char *s, *out;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_SYMBOL_NAME || CAR (list)->type == TYPE_SYMBOL)
    {
      fun = get_function (SYMBOL (CAR (list)), env, 1, 0, 0, 0);

      if (!fun)
	{
	  outcome->type = UNKNOWN_FUNCTION;
	  outcome->obj = SYMBOL (CAR (list));
	  return NULL;
	}
    }
  else if (CAR (list)->type == TYPE_FUNCTION)
    {
      fun = CAR (list);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (!IS_SEQUENCE (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

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

	  res = call_function (fun, arg, 0, 0, 1, 0, 0, env, outcome);
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
	      add_reference (cons, CAR (cons), 0);
	    }

	  decrement_refcount (res);

	  seq = CDR (seq);
	}

      if (SYMBOL (ret) != &nil_object)
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

	  res = call_function (fun, arg, 0, 0, 1, 0, 0, env, outcome);
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

	  res = call_function (fun, arg, 0, 0, 1, 0, 0, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!res)
	    return NULL;

	  if (SYMBOL (res) == &nil_object)
	    {
	      ret->value_ptr.array->value [j++] = CAR (arg);
	      add_reference (ret, ret->value_ptr.array->value [j-1], j-1);
	    }
	  else
	    decrement_refcount (CAR (arg));

	  decrement_refcount (res);
	}

      resize_vector (ret, j);
    }

  free (arg->value_ptr.cons_pair);
  free (arg);

  return ret;
}


struct object *
builtin_reverse (struct object *list, struct environment *env,
		 struct outcome *outcome)
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

	  add_reference (cons, CAR (seq), 0);
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
	  ret->value_ptr.array->value [i] = seq->value_ptr.array->value [sz-i-1];
	  add_reference (ret, ret->value_ptr.array->value [i], i);
	}
    }

  return ret;
}


struct object *
builtin_setf_car (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *newval;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  newval = CAR (list);
  list = CDR (list);

  /*if (IS_SYMBOL (CAR (CDR (list)))
      && SYMBOL (CAR (CDR (list)))->value_ptr.symbol->is_const)
    {
      outcome->type = CANT_MODIFY_CONSTANT;
      return NULL;
      }*/

  if (CAR (list)->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  delete_reference (CAR (list), CAR (CAR (list)), 0);
  CAR (list)->value_ptr.cons_pair->car = newval;
  add_reference (CAR (list), newval, 0);

  increment_refcount (newval);
  return newval;
}


struct object *
builtin_setf_cdr (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *newval;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  newval = CAR (list);
  list = CDR (list);

  /*if (IS_SYMBOL (CAR (CDR (list)))
      && SYMBOL (CAR (CDR (list)))->value_ptr.symbol->is_const)
    {
      outcome->type = CANT_MODIFY_CONSTANT;
      return NULL;
      }*/

  if (CAR (list)->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  delete_reference (CAR (list), CDR (CAR (list)), 1);
  CAR (list)->value_ptr.cons_pair->cdr = newval;
  add_reference (CAR (list), newval, 1);

  increment_refcount (newval);
  return newval;
}


struct object *
builtin_setf_nth (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *cons, *newval;
  int i;

  if (list_length (list) != 3)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  newval = CAR (list);
  list = CDR (list);

  if (CAR (list)->type != TYPE_INTEGER
      || (i = mpz_cmp_si (CAR (list)->value_ptr.integer, 0)) < 0
      || (CAR (CDR (list))->type != TYPE_CONS_PAIR))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (i >= list_length (CAR (CDR (list))))
    {
      outcome->type = OUT_OF_BOUND_INDEX;
      return NULL;
    }

  cons = nthcdr (i, CAR (CDR (list)));

  delete_reference (cons, CAR (cons), 0);
  add_reference (cons, newval, 0);
  cons->value_ptr.cons_pair->car = newval;

  increment_refcount (newval);
  return newval;
}


struct object *
builtin_setf_aref (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  struct object *lin_ind, *newval;
  int l = list_length (list), ind;

  if (l < 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  newval = CAR (list);
  list = CDR (list);

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

      if (newval->type != TYPE_CHARACTER)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (!set_nth_character (CAR (list), ind, newval->value_ptr.character))
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

      decrement_refcount (lin_ind);

      if (CAR (list)->type == TYPE_ARRAY)
	{
	  delete_reference (CAR (list), CAR (list)->value_ptr.array->value [ind],
			    ind);
	  add_reference (CAR (list), newval, ind);
	  CAR (list)->value_ptr.array->value [ind] = newval;
	}
      else
	{
	  if (newval->type != TYPE_INTEGER || !is_bit (newval))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  if (is_zero (newval))
	    mpz_clrbit (CAR (list)->value_ptr.bitarray->value, ind);
	  else
	    mpz_setbit (CAR (list)->value_ptr.bitarray->value, ind);
	}
    }

  increment_refcount (newval);
  return newval;
}


struct object *
builtin_setf_elt (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *cons, *newval;
  int ind;

  if (list_length (list) != 3)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  newval = CAR (list);
  list = CDR (list);

  if (!IS_SEQUENCE (CAR (list)) || CAR (CDR (list))->type != TYPE_INTEGER)
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

      if (!set_nth_character (CAR (list), ind, newval->value_ptr.character))
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}
    }
  else if (CAR (list)->type == TYPE_ARRAY)
    {
      if (ind >= (CAR (list)->value_ptr.array->fill_pointer >= 0
		  ? CAR (list)->value_ptr.array->fill_pointer
		  : CAR (list)->value_ptr.array->alloc_size->size))
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      delete_reference (CAR (list), CAR (list)->value_ptr.array->value [ind],
			ind);
      add_reference (CAR (list), newval, ind);
      CAR (list)->value_ptr.array->value [ind] = newval;
    }
  else if (CAR (list)->type == TYPE_BITARRAY)
    {
      if (newval->type != TYPE_INTEGER || !is_bit (newval))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (ind >= (CAR (list)->value_ptr.bitarray->fill_pointer >= 0
		  ? CAR (list)->value_ptr.bitarray->fill_pointer
		  : CAR (list)->value_ptr.bitarray->alloc_size->size))
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      if (is_zero (newval))
	mpz_clrbit (CAR (list)->value_ptr.bitarray->value, ind);
      else
	mpz_setbit (CAR (list)->value_ptr.bitarray->value, ind);
    }
  else
    {
      if (ind >= list_length (CAR (list)))
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      cons = nthcdr (ind, CAR (list));

      delete_reference (cons, CAR (cons), 0);
      add_reference (cons, newval, 0);
      cons->value_ptr.cons_pair->car = newval;
    }

  increment_refcount (newval);
  return newval;
}


struct object *
builtin_setf_fill_pointer (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  int fp;
  struct object *newval;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  newval = CAR (list);
  list = CDR (list);

  if (!IS_VECTOR (CAR (list)) || !HAS_FILL_POINTER (CAR (list))
      || newval->type != TYPE_INTEGER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  fp = mpz_get_si (newval->value_ptr.integer);

  if (fp < 0)
    {
      outcome->type = OUT_OF_BOUND_INDEX;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      if (fp > CAR (list)->value_ptr.string->used_size)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      CAR (list)->value_ptr.string->fill_pointer = fp;
    }
  else if (CAR (list)->type == TYPE_ARRAY)
    {
      if (fp > CAR (list)->value_ptr.array->alloc_size->size)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      CAR (list)->value_ptr.array->fill_pointer = fp;
    }

  increment_refcount (newval);
  return newval;
}


struct object *
builtin_setf_gethash (struct object *list, struct environment *env,
		      struct outcome *outcome)
{
  int ind, j;
  struct hashtable_record *r;
  struct object *newval;

  if (list_length (list) != 3)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  newval = CAR (list);
  list = CDR (list);

  if (CAR (CDR (list))->type != TYPE_HASHTABLE)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  r = find_hashtable_record (CAR (list), CAR (CDR (list)), &ind, &j, NULL);

  if (r)
    {
      delete_reference (CAR (CDR (list)), r->value, ind+(j*2+1)*LISP_HASHTABLE_SIZE);
      r->value = newval;
      r->reference_strength_factor = (r->reference_strength_factor & 1) |
	(!STRENGTH_FACTOR_OF_OBJECT (newval) << 1);
      INC_WEAK_REFCOUNT (newval);
    }
  else
    {
      r = malloc_and_check (sizeof (*r));

      r->key = CAR (list);
      INC_WEAK_REFCOUNT (CAR (list));
      r->value = newval;
      INC_WEAK_REFCOUNT (newval);
      r->reference_strength_factor = (!STRENGTH_FACTOR_OF_OBJECT (CAR (list)))
	| (!STRENGTH_FACTOR_OF_OBJECT (newval) << 1);

      r->next = CAR (CDR (list))->value_ptr.hashtable->table [ind];
      CAR (CDR (list))->value_ptr.hashtable->table [ind] = r;
    }

  increment_refcount (newval);
  return newval;
}


struct object *
builtin_setf_symbol_plist (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  struct object *newval;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  newval = CAR (list);
  list = CDR (list);

  if (!IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  delete_reference (SYMBOL (CAR (list)),
		    SYMBOL (CAR (list))->value_ptr.symbol->plist, 5);
  SYMBOL (CAR (list))->value_ptr.symbol->plist = newval;
  add_reference (SYMBOL (CAR (list)), newval, 5);

  increment_refcount (newval);
  return newval;
}


struct object *
builtin_setf_slot_value (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  struct class_field *f;
  struct object *req, *newval;

  if (list_length (list) != 3)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  newval = CAR (list);
  list = CDR (list);

  if (CAR (list)->type != TYPE_STANDARD_OBJECT || !IS_SYMBOL (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  f = CAR (list)->value_ptr.standard_object->fields;
  req = SYMBOL (CAR (CDR (list)));

  while (f)
    {
      if (f->name == req)
	{
	  f->value = newval;
	  increment_refcount (f->value);
	  increment_refcount (f->value);
	  return f->value;
	}

      f = f->next;
    }

  outcome->type = SLOT_NOT_FOUND;
  return NULL;
}


struct object *
builtin_setf_macro_function (struct object *list, struct environment *env,
			     struct outcome *outcome)
{
  struct object *sym, *newval;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  newval = CAR (list);
  list = CDR (list);

  if (!IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (newval->type != TYPE_FUNCTION)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  sym = SYMBOL (CAR (list));

  if (sym->value_ptr.symbol->function_cell)
    {
      delete_reference (sym, sym->value_ptr.symbol->function_cell, 1);
    }

  sym->value_ptr.symbol->function_cell = alloc_function ();
  sym->value_ptr.symbol->function_cell->type = TYPE_MACRO;
  add_reference (sym, sym->value_ptr.symbol->function_cell, 1);
  decrement_refcount (sym->value_ptr.symbol->function_cell);

  sym->value_ptr.symbol->function_cell->value_ptr.function->macro_function =
    newval;
  increment_refcount (newval);

  increment_refcount (newval);
  return newval;
}


int
compare_two_numbers (struct object *num1, struct object *num2)
{
  enum object_type tp = highest_num_type (num1->type, num2->type);
  struct object *first_p, *second_p;
  int eq;
  double d;

  if (tp != TYPE_COMPLEX)
    {
      first_p = promote_number (num1, tp);
      second_p = promote_number (num2, tp);

      if (tp == TYPE_INTEGER)
	eq = mpz_cmp (first_p->value_ptr.integer, second_p->value_ptr.integer);
      else if (tp == TYPE_RATIO)
	eq = mpq_cmp (first_p->value_ptr.ratio, second_p->value_ptr.ratio);
      else
	{
	  d = *first_p->value_ptr.floating - *second_p->value_ptr.floating;
	  eq = (d < 0) ? -1 : !d ? 0 : 1;
	}

      decrement_refcount (first_p);
      decrement_refcount (second_p);
    }
  else
    {
      if (num1->type == TYPE_COMPLEX)
	{
	  first_p = num1;
	  second_p = num2;
	}
      else
	{
	  first_p = num2;
	  second_p = num1;
	}

      if (second_p->type == TYPE_COMPLEX)
	{
	  return compare_two_numbers (first_p->value_ptr.complex->real,
				      second_p->value_ptr.complex->real)
	    || compare_two_numbers (first_p->value_ptr.complex->imag,
				    second_p->value_ptr.complex->imag);
	}
      else
	{
	  return compare_two_numbers (first_p->value_ptr.complex->real, second_p)
	    || !is_zero (first_p->value_ptr.complex->imag);
	}
    }

  return eq;
}


struct object *
compare_any_numbers (struct object *list, struct environment *env,
		     struct outcome *outcome, enum number_comparison comp)
{
  int l = list_length (list), eq;
  struct object *first, *second;

  if (!l)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (l == 1)
    {
      if (IS_NUMBER (CAR (list))
	  && (comp == EQUAL || CAR (list)->type != TYPE_COMPLEX))
	{
	  return &t_object;
	}
      else
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}
    }


  while (SYMBOL (CDR (list)) != &nil_object)
    {
      first = CAR (list);
      second = CAR (CDR (list));

      if (!IS_NUMBER (first) || !IS_NUMBER (second)
	  || (comp != EQUAL && (first->type == TYPE_COMPLEX
				|| second->type == TYPE_COMPLEX)))
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

      list = CDR (list);
    }

  return &t_object;
}


int
is_zero (const struct object *num)
{
  return (num->type == TYPE_INTEGER && !mpz_sgn (num->value_ptr.integer))
    || (num->type == TYPE_RATIO && !mpq_sgn (num->value_ptr.ratio))
    || (num->type == TYPE_FLOAT && *num->value_ptr.floating == 0)
    || (num->type == TYPE_COMPLEX && is_zero (num->value_ptr.complex->real)
	&& is_zero (num->value_ptr.complex->imag));
}


int
is_bit (const struct object *num)
{
  return (!mpz_cmp_si (num->value_ptr.integer, 0)
	  || !mpz_cmp_si (num->value_ptr.integer, 1));
}


struct object *
negate_number (struct object *num, int in_place)
{
  struct object *ret;

  if (!in_place)
    {
      ret = alloc_object ();
      ret->type = num->type;
    }
  else
    ret = num;

  if (num->type == TYPE_INTEGER)
    {
      mpz_neg (ret->value_ptr.integer, num->value_ptr.integer);
    }
  else if (num->type == TYPE_RATIO)
    {
      mpq_neg (ret->value_ptr.ratio, num->value_ptr.ratio);
    }
  else
    {
      if (!in_place)
	ret->value_ptr.floating =
	  malloc_and_check (sizeof (*ret->value_ptr.floating));

      *ret->value_ptr.floating = -*num->value_ptr.floating;
    }

  return ret;
}


struct object *
reciprocate_number (struct object *num)
{
  struct object *ret, *x, *x2, *y, *y2, *x2py2, *r, *i;

  if (num->type == TYPE_INTEGER)
    {
      ret = alloc_object ();
      ret->type = TYPE_RATIO;
      mpq_init (ret->value_ptr.ratio);
      mpq_set_z (ret->value_ptr.ratio, num->value_ptr.integer);
      mpq_inv (ret->value_ptr.ratio, ret->value_ptr.ratio);

      return convert_to_integer_if_possible (ret);
    }
  else if (num->type == TYPE_RATIO)
    {
      ret = alloc_object ();
      ret->type = TYPE_RATIO;
      mpq_init (ret->value_ptr.ratio);
      mpq_inv (ret->value_ptr.ratio, num->value_ptr.ratio);

      return ret;
    }
  else if (num->type == TYPE_FLOAT)
    {
      ret = alloc_object ();
      ret->type = TYPE_FLOAT;
      ret->value_ptr.floating =
	malloc_and_check (sizeof (*ret->value_ptr.floating));
      *ret->value_ptr.floating = 1 / *num->value_ptr.floating;

      return ret;
    }
  else
    {
      x = copy_number (num->value_ptr.complex->real);
      x2 = multiply_two_numbers (x, x);
      decrement_refcount (x);

      y = copy_number (num->value_ptr.complex->imag);
      y2 = multiply_two_numbers (y, y);
      decrement_refcount (y);

      x2py2 = add_two_numbers (x2, y2);
      decrement_refcount (x2);
      decrement_refcount (y2);

      x = copy_number (num->value_ptr.complex->real);
      r = divide_two_numbers (x, x2py2, NULL, NULL);
      decrement_refcount (x);

      y = copy_number (num->value_ptr.complex->imag);
      i = divide_two_numbers (y, x2py2, NULL, NULL);
      i = negate_number (i, 1);

      return create_complex (r, i, 1, NULL, NULL);
    }
}


double
convert_number_to_double (struct object *num)
{
  if (num->type == TYPE_INTEGER)
    {
      return mpz_get_d (num->value_ptr.integer);
    }
  else if (num->type == TYPE_RATIO)
    {
      return mpq_get_d (num->value_ptr.ratio);
    }
  else
    {
      return *num->value_ptr.floating;
    }
}


struct object *
add_two_numbers (struct object *n1, struct object *n2)
{
  enum object_type t = highest_num_type (n1->type, n2->type);
  struct object *ret, *op, *c, *r, *i;

  if (t == TYPE_COMPLEX)
    {
      if (n1->type == TYPE_COMPLEX)
	{
	  c = n1;
	}
      else
	{
	  c = n2;
	  n2 = n1;
	}

      r = add_two_numbers (c->value_ptr.complex->real,
			   (n2->type == TYPE_COMPLEX)
			   ? n2->value_ptr.complex->real : n2);

      if (n2->type == TYPE_COMPLEX)
	{
	  i = add_two_numbers (c->value_ptr.complex->imag,
			       n2->value_ptr.complex->imag);
	}
      else
	{
	  increment_refcount (c->value_ptr.complex->imag);
	  i = c->value_ptr.complex->imag;
	}

      return create_complex (r, i, 1, NULL, NULL);
    }
  else
    {
      ret = promote_number (n1, t);
      op = promote_number (n2, t);

      if (t == TYPE_INTEGER)
	{
	  mpz_add (ret->value_ptr.integer, ret->value_ptr.integer,
		   op->value_ptr.integer);
	}
      else if (t == TYPE_RATIO)
	{
	  mpq_add (ret->value_ptr.ratio, ret->value_ptr.ratio,
		   op->value_ptr.ratio);

	  ret = convert_to_integer_if_possible (ret);
	}
      else if (t == TYPE_FLOAT)
	{
	  *ret->value_ptr.floating = *ret->value_ptr.floating +
	    *op->value_ptr.floating;
	}

      decrement_refcount (op);

      return ret;
    }
}


struct object *
subtract_two_numbers (struct object *n1, struct object *n2)
{
  enum object_type t = highest_num_type (n1->type, n2->type);
  struct object *ret, *op, *r, *i;

  if (t == TYPE_COMPLEX)
    {
      r = subtract_two_numbers (n1->type == TYPE_COMPLEX
				? n1->value_ptr.complex->real : n1,
				n2->type == TYPE_COMPLEX
				? n2->value_ptr.complex->real : n2);

      if (n1->type == TYPE_COMPLEX && n2->type == TYPE_COMPLEX)
	{
	  i = subtract_two_numbers (n1->value_ptr.complex->imag,
				    n2->value_ptr.complex->imag);
	}
      else if (n1->type == TYPE_COMPLEX)
	{
	  increment_refcount (n1->value_ptr.complex->imag);
	  i = n1->value_ptr.complex->imag;
	}
      else
	{
	  i = negate_number (n2->value_ptr.complex->imag, 0);
	}

      return create_complex (r, i, 1, NULL, NULL);
    }
  else
    {
      ret = promote_number (n1, t);
      op = promote_number (n2, t);

      if (t == TYPE_INTEGER)
	{
	  mpz_sub (ret->value_ptr.integer, ret->value_ptr.integer,
		   op->value_ptr.integer);
	}
      else if (t == TYPE_RATIO)
	{
	  mpq_sub (ret->value_ptr.ratio, ret->value_ptr.ratio,
		   op->value_ptr.ratio);

	  ret = convert_to_integer_if_possible (ret);
	}
      else if (t == TYPE_FLOAT)
	{
	  *ret->value_ptr.floating = *ret->value_ptr.floating -
	    *op->value_ptr.floating;
	}

      decrement_refcount (op);

      return ret;
    }
}


struct object *
multiply_two_numbers (struct object *n1, struct object *n2)
{
  enum object_type t = highest_num_type (n1->type, n2->type);
  struct object *ret, *op, *c, *cr, *ci, *xu, *yv, *xv, *yu, *r, *i;

  if (t == TYPE_COMPLEX)
    {
      if (n1->type == TYPE_COMPLEX)
	{
	  c = n1;
	}
      else
	{
	  c = n2;
	  n2 = n1;
	}

      if (n2->type == TYPE_COMPLEX)
	{
	  cr = copy_number (c->value_ptr.complex->real);
	  ci = copy_number (c->value_ptr.complex->imag);

	  xu = multiply_two_numbers (c->value_ptr.complex->real,
				     n2->value_ptr.complex->real);
	  yv = multiply_two_numbers (c->value_ptr.complex->imag,
				     n2->value_ptr.complex->imag);
	  xv = multiply_two_numbers (cr, n2->value_ptr.complex->imag);
	  yu = multiply_two_numbers (ci, n2->value_ptr.complex->real);

	  r = subtract_two_numbers (xu, yv);
	  i = add_two_numbers (xv, yu);

	  decrement_refcount (cr);
	  decrement_refcount (ci);
	  decrement_refcount (xu);
	  decrement_refcount (yv);
	  decrement_refcount (xv);
	  decrement_refcount (yu);
	}
      else
	{
	  r = multiply_two_numbers (c->value_ptr.complex->real, n2);
	  i = multiply_two_numbers (c->value_ptr.complex->imag, n2);
	}

      return create_complex (r, i, 1, NULL, NULL);
    }
  else
    {
      ret = promote_number (n1, t);
      op = promote_number (n2, t);

      if (t == TYPE_INTEGER)
	{
	  mpz_mul (ret->value_ptr.integer, ret->value_ptr.integer,
		   op->value_ptr.integer);
	}
      else if (t == TYPE_RATIO)
	{
	  mpq_mul (ret->value_ptr.ratio, ret->value_ptr.ratio,
		   op->value_ptr.ratio);

	  ret = convert_to_integer_if_possible (ret);
	}
      else if (t == TYPE_FLOAT)
	{
	  *ret->value_ptr.floating = *ret->value_ptr.floating *
	    *op->value_ptr.floating;
	}

      decrement_refcount (op);

      return ret;
    }
}


struct object *
divide_two_numbers (struct object *n1, struct object *n2, struct environment *env,
		    struct outcome *outcome)
{
  enum object_type t = highest_num_type (n1->type, n2->type);
  struct object *ret, *pn1, *pn2, *div;

  if (is_zero (n2))
    {
      outcome->type = CANT_DIVIDE_BY_ZERO;
      return NULL;
    }

  if (t == TYPE_COMPLEX)
    {
      div = reciprocate_number (n2);

      ret = multiply_two_numbers (n1, div);

      decrement_refcount (div);

      return ret;
    }
  else if (t == TYPE_INTEGER || t == TYPE_RATIO)
    {
      pn1 = promote_number (n1, TYPE_RATIO);
      pn2 = promote_number (n2, TYPE_RATIO);

      ret = alloc_number (TYPE_RATIO);

      mpq_div (ret->value_ptr.ratio, pn1->value_ptr.ratio, pn2->value_ptr.ratio);

      ret = convert_to_integer_if_possible (ret);
    }
  else
    {
      pn1 = promote_number (n1, TYPE_FLOAT);
      pn2 = promote_number (n2, TYPE_FLOAT);

      ret = alloc_number (TYPE_FLOAT);

      *ret->value_ptr.floating = *pn1->value_ptr.floating
	/ *pn2->value_ptr.floating;
    }

  decrement_refcount (pn1);
  decrement_refcount (pn2);

  return ret;
}


enum object_type
highest_num_type (enum object_type t1, enum object_type t2)
{
  if (t1 == TYPE_COMPLEX || t2 == TYPE_COMPLEX)
    return TYPE_COMPLEX;

  if (t1 == TYPE_FLOAT || t2 == TYPE_FLOAT)
    return TYPE_FLOAT;

  if (t1 == TYPE_RATIO || t2 == TYPE_RATIO)
    return TYPE_RATIO;

  return TYPE_INTEGER;
}


struct object *
copy_number (const struct object *num)
{
  struct object *ret = alloc_object ();

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
  else if (num->type == TYPE_FLOAT)
    {
      ret->value_ptr.floating = malloc_and_check
	(sizeof (*ret->value_ptr.floating));
      *ret->value_ptr.floating = *num->value_ptr.floating;
    }
  else
    {
      ret->value_ptr.complex = malloc_and_check (sizeof (*ret->value_ptr.complex));
      ret->value_ptr.complex->real = copy_number (num->value_ptr.complex->real);
      ret->value_ptr.complex->imag = copy_number (num->value_ptr.complex->imag);
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

  ret = alloc_object ();
  ret->type = type;

  if (type == TYPE_RATIO)
    {
      mpq_init (ret->value_ptr.ratio);
      mpq_set_z (ret->value_ptr.ratio, num->value_ptr.integer);
    }
  else if (type == TYPE_FLOAT)
    {
      ret->value_ptr.floating = malloc_and_check
	(sizeof (*ret->value_ptr.floating));

      if (num->type == TYPE_INTEGER)
	*ret->value_ptr.floating = mpz_get_d (num->value_ptr.integer);
      else if (num->type == TYPE_RATIO)
	*ret->value_ptr.floating = mpq_get_d (num->value_ptr.ratio);
    }
  else if (type == TYPE_COMPLEX)
    {
      return create_complex (num, NULL, 0, NULL, NULL);
    }

  return ret;
}


struct object *
perform_division_with_remainder (struct object *args,
				 enum rounding_behavior round_behavior,
				 enum object_type quotient_type,
				 struct outcome *outcome)
{
  int l = list_length (args);
  enum object_type rem_type, op_type;
  struct object *div_, *div, *num, *ret, *ret2;
  mpz_t tmp;
  mpf_t q, r, half, divf, numf;

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

  if (rem_type == TYPE_INTEGER && round_behavior != ROUND_TO_NEAREST)
    op_type = TYPE_INTEGER;
  else
    op_type = TYPE_FLOAT;

  num = promote_number (CAR (args), op_type);
  div = promote_number (div_, op_type);

  if (l != 2)
    decrement_refcount (div_);

  if (op_type == TYPE_INTEGER)
    {
      ret2 = alloc_number (TYPE_INTEGER);

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

      ret = alloc_number (quotient_type);

      if (quotient_type == TYPE_INTEGER)
	{
	  mpz_set (ret->value_ptr.integer, tmp);
	}
      else
	{
	  *ret->value_ptr.floating = mpz_get_si (tmp);
	}

      mpz_clear (tmp);
    }
  else
    {
      mpf_init (q);
      mpf_set_d (q, *num->value_ptr.floating / *div->value_ptr.floating);

      if (round_behavior == FLOOR)
	mpf_floor (q, q);
      else if (round_behavior == CEILING)
	mpf_ceil (q, q);
      else if (round_behavior == TRUNCATE)
	mpf_trunc (q, q);
      else if (round_behavior == ROUND_TO_NEAREST)
	{
	  mpf_init (half);
	  mpf_set_d (half, .5);

	  mpf_add (q, q, half);

	  mpf_clear (half);

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
	}

      mpf_init (r);

      mpf_init (divf);
      mpf_set_d (divf, *div->value_ptr.floating);

      mpf_init (numf);
      mpf_set_d (numf, *num->value_ptr.floating);

      mpf_mul (r, divf, q);
      mpf_sub (r, numf, r);

      mpf_clear (divf);
      mpf_clear (numf);

      ret = alloc_number (quotient_type);

      if (quotient_type == TYPE_INTEGER)
	mpz_set_f (ret->value_ptr.integer, q);
      else
	*ret->value_ptr.floating = mpf_get_d (q);

      ret2 = alloc_number (rem_type);

      if (rem_type == TYPE_INTEGER)
	mpz_set_f (ret2->value_ptr.integer, r);
      else if (rem_type == TYPE_RATIO)
	mpq_set_f (ret2->value_ptr.ratio, r);
      else if (rem_type == TYPE_FLOAT)
	*ret2->value_ptr.floating = mpf_get_d (r);

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
	      struct outcome *outcome)
{
  struct object *ret, *ret2;
  int l;

  if (!(l = list_length (list)))
    {
      return create_integer_from_long (0);
    }

  if (!IS_NUMBER (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 1)
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }

  ret = copy_number (CAR (list));
  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (!IS_NUMBER (CAR (list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret2 = add_two_numbers (ret, CAR (list));
      decrement_refcount (ret);
      ret = ret2;

      list = CDR (list);
    }

  return ret;
}


struct object *
builtin_minus (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  struct object *ret, *ret2;
  int l;

  if (!(l = list_length (list)))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }
  else if (l == 1)
    {
      ret = create_integer_from_long (0);
    }
  else
    {
      if (!IS_NUMBER (CAR (list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = copy_number (CAR (list));
      list = CDR (list);
    }


  while (SYMBOL (list) != &nil_object)
    {
      if (!IS_NUMBER (CAR (list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret2 = subtract_two_numbers (ret, CAR (list));
      decrement_refcount (ret);
      ret = ret2;

      list = CDR (list);
    }

  return ret;
}


struct object *
builtin_multiply (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *ret, *ret2;
  int l;

  if (!(l = list_length (list)))
    {
      return create_integer_from_long (1);
    }

  if (!IS_NUMBER (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 1)
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }

  ret = copy_number (CAR (list));
  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (!IS_NUMBER (CAR (list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret2 = multiply_two_numbers (ret, CAR (list));
      decrement_refcount (ret);
      ret = ret2;

      list = CDR (list);
    }

  return ret;
}


struct object *
builtin_divide (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *ret, *ret2;
  int l;

  if (!(l = list_length (list)))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }
  else if (l == 1)
    {
      ret = create_integer_from_long (1);
    }
  else
    {
      if (!IS_NUMBER (CAR (list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = copy_number (CAR (list));
      list = CDR (list);
    }


  while (SYMBOL (list) != &nil_object)
    {
      if (!IS_NUMBER (CAR (list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret2 = divide_two_numbers (ret, CAR (list), env, outcome);

      if (!ret2)
	return NULL;

      decrement_refcount (ret);
      ret = ret2;

      list = CDR (list);
    }

  return ret;
}


struct object *
builtin_floor (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  return perform_division_with_remainder (list, FLOOR, TYPE_INTEGER, outcome);
}


struct object *
builtin_ffloor (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  return perform_division_with_remainder (list, FLOOR, TYPE_FLOAT, outcome);
}


struct object *
builtin_ceiling (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  return perform_division_with_remainder (list, CEILING, TYPE_INTEGER, outcome);
}


struct object *
builtin_fceiling (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  return perform_division_with_remainder (list, CEILING, TYPE_FLOAT, outcome);
}


struct object *
builtin_truncate (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  return perform_division_with_remainder (list, TRUNCATE, TYPE_INTEGER, outcome);
}


struct object *
builtin_ftruncate (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  return perform_division_with_remainder (list, TRUNCATE, TYPE_FLOAT, outcome);
}


struct object *
builtin_round (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  return perform_division_with_remainder (list, ROUND_TO_NEAREST, TYPE_INTEGER,
					  outcome);
}


struct object *
builtin_fround (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  return perform_division_with_remainder (list, ROUND_TO_NEAREST, TYPE_FLOAT,
					  outcome);
}


struct object *
builtin_numerator (struct object *list, struct environment *env,
		   struct outcome *outcome)
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

  if (CAR (list)->type == TYPE_INTEGER)
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }

  mpq_canonicalize (CAR (list)->value_ptr.ratio);

  ret = alloc_number (TYPE_INTEGER);
  mpq_get_num (ret->value_ptr.integer, CAR (list)->value_ptr.ratio);

  return ret;
}


struct object *
builtin_denominator (struct object *list, struct environment *env,
		     struct outcome *outcome)
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

  if (CAR (list)->type == TYPE_INTEGER)
    {
      return create_integer_from_long (1);
    }

  mpq_canonicalize (CAR (list)->value_ptr.ratio);

  ret = alloc_number (TYPE_INTEGER);
  mpq_get_den (ret->value_ptr.integer, CAR (list)->value_ptr.ratio);

  return ret;
}


struct object *
builtin_sqrt (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  struct object *num, *ret;

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

  ret = alloc_number (TYPE_FLOAT);
  *ret->value_ptr.floating = sqrt (*num->value_ptr.floating);

  decrement_refcount (num);

  return ret;
}


struct object *
builtin_complex (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  int l = list_length (list);

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  return create_complex (CAR (list), l == 2 ? CAR (CDR (list)) : NULL, 0, env,
			 outcome);
}


struct object *
builtin_realpart (struct object *list, struct environment *env,
		  struct outcome *outcome)
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
  else if (IS_REAL (num))
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
		  struct outcome *outcome)
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
		       struct outcome *outcome)
{
  return compare_any_numbers (list, env, outcome, EQUAL);
}


struct object *
builtin_numbers_different (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  int l = list_length (list), i, j;
  struct object *first, *second, *cons;

  if (l == 1 && IS_NUMBER (CAR (list)))
    return &t_object;
  else if (l == 1)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;

      return NULL;
    }

  for (i = 0; i + 1 < l; i++)
    {
      first = CAR (list);

      if (!IS_NUMBER (first))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;

	  return NULL;
	}

      cons = CDR (list);

      for (j = i + 1; j < l; j++)
	{
	  second = CAR (cons);

	  if (!IS_NUMBER (second))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;

	      return NULL;
	    }

	  if (!compare_two_numbers (first, second))
	    return &nil_object;

	  cons = CDR (cons);
	}

      list = CDR (list);
    }

  return &t_object;
}


struct object *
builtin_numbers_less_than (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  return compare_any_numbers (list, env, outcome, LESS_THAN);
}


struct object *
builtin_numbers_less_than_or_equal (struct object *list, struct environment *env,
				    struct outcome *outcome)
{
  return compare_any_numbers (list, env, outcome, LESS_THAN_OR_EQUAL);
}


struct object *
builtin_numbers_more_than (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  return compare_any_numbers (list, env, outcome, MORE_THAN);
}


struct object *
builtin_numbers_more_than_or_equal (struct object *list, struct environment *env,
				    struct outcome *outcome)
{
  return compare_any_numbers (list, env, outcome, MORE_THAN_OR_EQUAL);
}


struct object *
builtin_min (struct object *list, struct environment *env,
	     struct outcome *outcome)
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
	     struct outcome *outcome)
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
builtin_sin (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_REAL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return create_floating_from_double (sin (convert_number_to_double (CAR (list))));
}


struct object *
builtin_cos (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_REAL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return create_floating_from_double (cos (convert_number_to_double (CAR (list))));
}


struct object *
builtin_tan (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_REAL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return create_floating_from_double (tan (convert_number_to_double (CAR (list))));
}


struct object *
builtin_sinh (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_REAL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return create_floating_from_double (sinh (convert_number_to_double (CAR (list))));
}


struct object *
builtin_cosh (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_REAL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return create_floating_from_double (cosh (convert_number_to_double (CAR (list))));
}


struct object *
builtin_tanh (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_REAL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return create_floating_from_double (tanh (convert_number_to_double (CAR (list))));
}


struct object *
builtin_exp (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  double arg;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_REAL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (list)->type == TYPE_INTEGER)
    {
      arg = mpz_get_d (CAR (list)->value_ptr.integer);
    }
  else if (CAR (list)->type == TYPE_RATIO)
    {
      arg = mpq_get_d (CAR (list)->value_ptr.ratio);
    }
  else
    {
      arg = *CAR (list)->value_ptr.floating;
    }

  return create_floating_from_double (exp (arg));
}


struct object *
builtin_expt (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  double base, exp;
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_REAL (CAR (list)) || !IS_REAL (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  exp = convert_number_to_double (CAR (CDR (list)));

  if (CAR (list)->type == TYPE_FLOAT || CAR (CDR (list))->type != TYPE_INTEGER)
    {
      base = convert_number_to_double (CAR (list));

      if (!exp && base <= 0)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      return create_floating_from_double (pow (base, exp));
    }

  if (exp < 0)
    {
      ret = alloc_number (TYPE_RATIO);

      if (CAR (list)->type == TYPE_INTEGER)
	{
	  mpz_set (mpq_denref (ret->value_ptr.ratio),
		   CAR (list)->value_ptr.integer);
	}
      else
	{
	  mpq_inv (ret->value_ptr.ratio, CAR (list)->value_ptr.ratio);
	}

      mpq_canonicalize (ret->value_ptr.ratio);
      exp = -exp;
    }
  else
    {
      ret = alloc_number (CAR (list)->type);
    }

  if (ret->type == TYPE_INTEGER)
    {
      mpz_set_si (ret->value_ptr.integer,
		  pow (mpz_get_si (CAR (list)->value_ptr.integer), exp));
    }
  else
    {
      mpz_set_si (mpq_numref (ret->value_ptr.ratio),
		  pow (mpz_get_si (mpq_numref (ret->value_ptr.ratio)), exp));
      mpq_canonicalize (ret->value_ptr.ratio);
      mpz_set_si (mpq_denref (ret->value_ptr.ratio),
		  pow (mpz_get_si (mpq_denref (ret->value_ptr.ratio)), exp));
      mpq_canonicalize (ret->value_ptr.ratio);
    }

  return ret;
}


struct object *
builtin_log (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  int l = list_length (list);

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_REAL (CAR (list)) || convert_number_to_double (CAR (list)) <= 0
      || (l == 2 && !IS_REAL (CAR (CDR (list)))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 1)
    {
      return create_floating_from_double (log (convert_number_to_double (CAR (list))));
    }

  if (is_zero (CAR (CDR (list))))
    {
      return create_floating_from_double (0);
    }

  return
    create_floating_from_double (log (convert_number_to_double (CAR (list)))
				 / log (convert_number_to_double (CAR (CDR (list)))));
}


struct object *
builtin_lognot (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_INTEGER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = alloc_number (TYPE_INTEGER);

  mpz_com (ret->value_ptr.integer, CAR (list)->value_ptr.integer);

  return ret;
}


struct object *
builtin_logior (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  struct object *ret;

  if (SYMBOL (list) == &nil_object)
    {
      return create_integer_from_long (0);
    }

  if (CAR (list)->type != TYPE_INTEGER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = alloc_number (TYPE_INTEGER);
  mpz_set (ret->value_ptr.integer, CAR (list)->value_ptr.integer);

  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (CAR (list)->type != TYPE_INTEGER)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      mpz_ior (ret->value_ptr.integer, ret->value_ptr.integer,
	       CAR (list)->value_ptr.integer);

      list = CDR (list);
    }

  return ret;
}


struct object *
builtin_make_random_state (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  int l = list_length (list);
  struct object *ret;

  if (l > 1)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (l == 1 && CAR (list)->type != TYPE_RANDOM_STATE
      && SYMBOL (CAR (list)) != &nil_object
      && SYMBOL (CAR (list)) != &t_object)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = alloc_object ();
  ret->type = TYPE_RANDOM_STATE;

  if (!l || SYMBOL (CAR (list)) == &t_object)
    {
      gmp_randinit_default (ret->value_ptr.random_state);
      gmp_randseed_ui (ret->value_ptr.random_state, time (NULL));
    }
  else if (SYMBOL (CAR (list)) == &nil_object)
    {
      gmp_randinit_set
	(ret->value_ptr.random_state,
	 inspect_variable (env->random_state_sym, env)->value_ptr.random_state);
    }
  else
    {
      gmp_randinit_set (ret->value_ptr.random_state,
			CAR (list)->value_ptr.random_state);
    }

  return ret;
}


struct object *
builtin_random (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  int l = list_length (list);
  struct object *rs, *ret;
  mpf_t r;

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (((CAR (list)->type != TYPE_INTEGER
	|| mpz_cmp_si (CAR (list)->value_ptr.integer, 0) <= 0)
       && (CAR (list)->type != TYPE_FLOAT
	   || *CAR (list)->value_ptr.floating <= 0))
      || (l == 2 && CAR (CDR (list))->type != TYPE_RANDOM_STATE))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 1)
    rs = inspect_variable (env->random_state_sym, env);
  else
    rs = CAR (CDR (list));

  if (CAR (list)->type == TYPE_INTEGER)
    {
      ret = alloc_number (TYPE_INTEGER);
      mpz_urandomm (ret->value_ptr.integer, rs->value_ptr.random_state,
		    CAR (list)->value_ptr.integer);
    }
  else
    {
      ret = alloc_number (TYPE_FLOAT);

      mpf_init (r);
      mpf_urandomb (r, rs->value_ptr.random_state, mpf_get_default_prec ());

      *ret->value_ptr.floating = mpf_get_d (r) * *CAR (list)->value_ptr.floating;

      mpf_clear (r);
    }

  return ret;
}


struct object *
builtin_byte (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_INTEGER
      || mpz_sgn (CAR (list)->value_ptr.integer) < 0
      || CAR (CDR (list))->type != TYPE_INTEGER
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
		   struct outcome *outcome)
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

  ret = alloc_number (TYPE_INTEGER);

  mpz_set (ret->value_ptr.integer, CAR (list)->value_ptr.bytespec->size);

  return ret;
}


struct object *
builtin_byte_position (struct object *list, struct environment *env,
		       struct outcome *outcome)
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

  ret = alloc_number (TYPE_INTEGER);

  mpz_set (ret->value_ptr.integer, CAR (list)->value_ptr.bytespec->pos);

  return ret;
}


struct object *
builtin_typep (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  int ret;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  ret = check_type (CAR (list), CAR (CDR (list)), env, outcome);

  if (ret == -1)
    return NULL;
  else if (ret)
    return &t_object;
  else
    return &nil_object;
}


struct object *
builtin_type_of (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (IS_SYMBOL (CAR (list)))
    {
      ret = BUILTIN_SYMBOL ("SYMBOL");
    }
  else if (CAR (list)->type == TYPE_STRING)
    {
      ret = BUILTIN_SYMBOL ("STRING");
    }
  else if (CAR (list)->type == TYPE_CHARACTER)
    {
      ret = BUILTIN_SYMBOL ("CHARACTER");
    }
  else if (CAR (list)->type == TYPE_CONS_PAIR)
    {
      ret = BUILTIN_SYMBOL ("CONS");
    }
  else if (CAR (list)->type == TYPE_INTEGER)
    {
      ret = BUILTIN_SYMBOL ("INTEGER");
    }
  else if (CAR (list)->type == TYPE_RATIO)
    {
      ret = BUILTIN_SYMBOL ("RATIO");
    }
  else if (CAR (list)->type == TYPE_FLOAT)
    {
      ret = BUILTIN_SYMBOL ("FLOAT");
    }
  else if (CAR (list)->type == TYPE_COMPLEX)
    {
      ret = BUILTIN_SYMBOL ("COMPLEX");
    }
  else if (CAR (list)->type == TYPE_RANDOM_STATE)
    {
      ret = BUILTIN_SYMBOL ("RANDOM-STATE");
    }
  else if (CAR (list)->type == TYPE_FUNCTION)
    {
      ret = BUILTIN_SYMBOL ("FUNCTION");
    }
  else if (CAR (list)->type == TYPE_PACKAGE)
    {
      ret = BUILTIN_SYMBOL ("PACKAGE");
    }
  else if (CAR (list)->type == TYPE_ARRAY || CAR (list)->type == TYPE_BITARRAY)
    {
      ret = BUILTIN_SYMBOL ("ARRAY");
    }
  else if (CAR (list)->type == TYPE_HASHTABLE)
    {
      ret = BUILTIN_SYMBOL ("HASH-TABLE");
    }
  else if (CAR (list)->type == TYPE_FILENAME)
    {
      ret = BUILTIN_SYMBOL ("PATHNAME");
    }
  else if (CAR (list)->type == TYPE_STREAM)
    {
      ret = BUILTIN_SYMBOL ("STREAM");
    }
  else if (CAR (list)->type == TYPE_STRUCTURE_CLASS)
    {
      ret = BUILTIN_SYMBOL ("STRUCTURE-CLASS");
    }
  else if (CAR (list)->type == TYPE_STRUCTURE)
    {
      ret = CAR (list)->value_ptr.structure->class_name;
    }
  else if (CAR (list)->type == TYPE_STANDARD_CLASS)
    {
      ret = BUILTIN_SYMBOL ("STANDARD-CLASS");
    }
  else if (CAR (list)->type == TYPE_STANDARD_OBJECT)
    {
      ret = CAR (list)->value_ptr.standard_object->class_name;
    }
  else if (CAR (list)->type == TYPE_CONDITION)
    {
      ret = CAR (list)->value_ptr.condition->class_name;
    }

  increment_refcount (ret);
  return ret;
}


struct object *
builtin_subtypep (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)) || !IS_SYMBOL (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (!SYMBOL (CAR (list))->value_ptr.symbol->is_type
      || !SYMBOL (CAR (CDR (list)))->value_ptr.symbol->is_type)
    {
      outcome->type = UNKNOWN_TYPE;
      return NULL;
    }

  prepend_object_to_obj_list (&t_object, &outcome->other_values);

  if (is_subtype (SYMBOL (CAR (list)), SYMBOL (CAR (CDR (list))), NULL))
    {
      return &t_object;
    }

  return &nil_object;
}


struct object *
builtin_make_string (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  int sz;
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (CAR (list)->type != TYPE_INTEGER)
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
builtin_intern (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  int l = list_length (list), ispr;
  struct object *pack, *ret, *ret2;
  struct package_record *ent;

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      pack = inspect_package_by_designator (CAR (CDR (list)), env);

      if (!pack)
	{
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}
    }
  else
    pack = inspect_variable (env->package_sym, env);


  ent = inspect_accessible_symbol_by_name (CAR (list)->value_ptr.string->value,
					   CAR (list)->value_ptr.string->used_size,
					   pack, &ispr);

  if (ent)
    {
      ret = ent->sym;
      ret2 = KEYWORD (ispr ?
		      (ent->visibility == INTERNAL_VISIBILITY ?
		       ":INTERNAL" : ":EXTERNAL") : ":INHERITED");

      increment_refcount (ret2);
    }
  else
    {
      ret = intern_symbol_by_char_vector (CAR (list)->value_ptr.string->value,
					  CAR (list)->value_ptr.string->used_size,
					  1, pack == env->keyword_package
					  ? EXTERNAL_VISIBILITY
					  : INTERNAL_VISIBILITY, 1, pack);
      ret2 = &nil_object;
    }

  increment_refcount (ret);
  prepend_object_to_obj_list (ret2, &outcome->other_values);
  return ret;
}


struct object *
builtin_find_symbol (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  int l = list_length (list), ispr;
  struct object *pack, *ret, *ret2;
  struct package_record *ent;

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      pack = inspect_package_by_designator (CAR (CDR (list)), env);

      if (!pack)
	{
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}
    }
  else
    pack = inspect_variable (env->package_sym, env);


  ent = inspect_accessible_symbol_by_name (CAR (list)->value_ptr.string->value,
					   CAR (list)->value_ptr.string->used_size,
					   pack, &ispr);

  if (ent)
    {
      ret = ent->sym;
      increment_refcount (ret);

      ret2 = KEYWORD (ispr ?
		      (ent->visibility == INTERNAL_VISIBILITY ?
		       ":INTERNAL" : ":EXTERNAL") : ":INHERITED");
      increment_refcount (ret2);
    }
  else
    {
      ret = ret2 = &nil_object;
    }

  prepend_object_to_obj_list (ret2, &outcome->other_values);
  return ret;
}


struct object *
builtin_unintern (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  int l = list_length (list), ret;
  struct object *pack;

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
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      pack = inspect_package_by_designator (CAR (CDR (list)), env);

      if (!pack)
	{
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}
    }
  else
    pack = inspect_variable (env->package_sym, env);


  ret = unintern_symbol (SYMBOL (CAR (list)), pack);

  decrement_refcount (SYMBOL (CAR (list)));

  return ret ? &t_object : &nil_object;
}


struct object *
builtin_make_symbol (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  struct string *s;
  struct object *ret;

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

  ret = create_symbol (s->value, s->used_size, 1);

  ret->value_ptr.symbol->home_package = &nil_object;

  return ret;
}


struct object *
builtin_copy_symbol (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  int l = list_length (list);
  struct object *sym, *ret;

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

  sym = SYMBOL (CAR (list));

  ret = create_symbol (sym->value_ptr.symbol->name,
		       sym->value_ptr.symbol->name_len, 1);
  ret->value_ptr.symbol->home_package = &nil_object;

  if (l == 2 && SYMBOL (CAR (CDR (list))) != &nil_object)
    {
      ret->value_ptr.symbol->value_cell = sym->value_ptr.symbol->value_cell;
      add_reference (ret, ret->value_ptr.symbol->value_cell, 0);

      ret->value_ptr.symbol->function_cell = sym->value_ptr.symbol->function_cell;
      add_reference (ret, ret->value_ptr.symbol->function_cell, 1);

      if (SYMBOL (sym->value_ptr.symbol->plist) != &nil_object)
	{
	  ret->value_ptr.symbol->plist =
	    copy_list_structure (sym->value_ptr.symbol->plist, NULL, -1, NULL);
	  add_reference (ret, ret->value_ptr.symbol->plist, 5);
	  decrement_refcount (ret->value_ptr.symbol->plist);
	}
    }

  return ret;
}


struct object *
builtin_boundp (struct object *list, struct environment *env,
		struct outcome *outcome)
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
		      struct outcome *outcome)
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
builtin_set (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  struct binding *b;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  b = find_binding (SYMBOL (CAR (list))->value_ptr.symbol, env->vars,
		    DYNAMIC_BINDING, -1);

  if (b)
    {
      decrement_refcount (b->obj);
      b->obj = CAR (CDR (list));
      increment_refcount (b->obj);
    }
  else
    {
      delete_reference (SYMBOL (CAR (list)),
			SYMBOL (CAR (list))->value_ptr.symbol->value_cell, 0);
      add_reference (SYMBOL (CAR (list)), CAR (CDR (list)), 0);
      SYMBOL (CAR (list))->value_ptr.symbol->value_cell = CAR (CDR (list));

      SYMBOL (CAR (list))->value_ptr.symbol->is_parameter = 1;
    }

  increment_refcount (CAR (CDR (list)));
  return CAR (CDR (list));
}


struct object *
builtin_fboundp (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  struct object *s, *sym;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL
      && !(CAR (list)->type == TYPE_CONS_PAIR && list_length (CAR (list)) == 2
	   && SYMBOL (CAR (CAR (list))) == env->setf_sym
	   && IS_SYMBOL (CAR (CDR (CAR (list))))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  sym = SYMBOL (s);

  if ((CAR (list)->type == TYPE_CONS_PAIR
       && SYMBOL (CAR (CDR (CAR (list))))->value_ptr.symbol->setf_func_cell)
      || (CAR (list)->type != TYPE_CONS_PAIR &&
	  (sym->value_ptr.symbol->function_cell
	   || sym->value_ptr.symbol->function_dyn_bins_num)))
    {
      return &t_object;
    }

  return &nil_object;
}


struct object *
builtin_symbol_function (struct object *list, struct environment *env,
			 struct outcome *outcome)
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

  ret = get_function (SYMBOL (s), env, 0, 0, 1, 1);

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
		     struct outcome *outcome)
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

  return create_string_copying_char_vector (s->name, s->name_len);
}


struct object *
builtin_symbol_package (struct object *list, struct environment *env,
			struct outcome *outcome)
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
builtin_symbol_plist (struct object *list, struct environment *env,
		      struct outcome *outcome)
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

  increment_refcount (SYMBOL (CAR (list))->value_ptr.symbol->plist);
  return SYMBOL (CAR (list))->value_ptr.symbol->plist;
}


struct object *
builtin_special_operator_p (struct object *list, struct environment *env,
			    struct outcome *outcome)
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
builtin_makunbound (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct binding *b = env->vars;

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

  delete_reference (SYMBOL (CAR (list)),
		    SYMBOL (CAR (list))->value_ptr.symbol->value_cell, 0);
  SYMBOL (CAR (list))->value_ptr.symbol->value_cell = NULL;

  while (SYMBOL (CAR (list))->value_ptr.symbol->value_dyn_bins_num)
    {
      while (1)
	{
	  if (b->sym == SYMBOL (CAR (list)) && b->type & DYNAMIC_BINDING)
	    {
	      b->type |= DELETED_BINDING;
	      b = b->next;
	      break;
	    }
	  else
	    b = b->next;
	}

      SYMBOL (CAR (list))->value_ptr.symbol->value_dyn_bins_num--;
    }

  increment_refcount (SYMBOL (CAR (list)));
  return SYMBOL (CAR (list));
}


struct object *
builtin_fmakunbound (struct object *list, struct environment *env,
		     struct outcome *outcome)
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

  delete_reference (SYMBOL (CAR (list)),
		    SYMBOL (CAR (list))->value_ptr.symbol->function_cell, 1);
  SYMBOL (CAR (list))->value_ptr.symbol->function_cell = NULL;

  increment_refcount (SYMBOL (CAR (list)));
  return SYMBOL (CAR (list));
}


struct object *
builtin_macroexpand_1 (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct object *ret, *ret2, *mac, *args;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_CONS_PAIR && IS_SYMBOL (CAR (CAR (list)))
      && (mac = get_function (CAR (CAR (list)), env, 0, 0, 0, 0))
      && mac->type == TYPE_MACRO && !mac->value_ptr.function->builtin_form)
    {
      if (mac->value_ptr.macro->macro_function)
	{
	  args = alloc_empty_list (2);
	  args->value_ptr.cons_pair->car = CAR (list);
	  args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = &nil_object;

	  ret = call_function (mac->value_ptr.macro->macro_function, args,
			       0, 0, 0, 0, 0, env, outcome);

	  free_list_structure (args);
	}
      else
	{
	  ret = call_function (mac, CAR (list), 0, 1, 1, 0, 0, env, outcome);
	}

      if (!ret)
	return NULL;

      ret2 = &t_object;
    }
  else
    {
      increment_refcount (CAR (list));
      ret = CAR (list);
      ret2 = &nil_object;
    }

  prepend_object_to_obj_list (ret2, &outcome->other_values);
  return ret;
}


struct object *
builtin_macro_function (struct object *list, struct environment *env,
			struct outcome *outcome)
{
  struct object *sym, *ret;

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

  sym = SYMBOL (CAR (list));

  if (!sym->value_ptr.symbol->function_cell
      || sym->value_ptr.symbol->function_cell->type != TYPE_MACRO
      || sym->value_ptr.symbol->function_cell->value_ptr.function->builtin_form)
    {
      return &nil_object;
    }

  if (sym->value_ptr.symbol->function_cell->value_ptr.function->macro_function)
    {
      ret = sym->value_ptr.symbol->function_cell->value_ptr.function->
	macro_function;

      increment_refcount (ret);
    }
  else
    {
      ret = alloc_function ();

      ret->value_ptr.function->function_macro =
	sym->value_ptr.symbol->function_cell;
    }

  return ret;
}


struct object *
builtin_string (struct object *list, struct environment *env,
		struct outcome *outcome)
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

      return create_string_copying_char_vector (s->name, s->name_len);
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


/*struct object *
builtin_string_eq (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  char *s1, *s2;
  int l1, l2;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      s1 = CAR (list)->value_ptr.string->value;
      l1 = CAR (list)->value_ptr.string->used_size;
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      s1 = SYMBOL (CAR (list))->value_ptr.symbol->name;
      l1 = SYMBOL (CAR (list))->value_ptr.symbol->name_len;
    }
  else if (CAR (list)->type == TYPE_CHARACTER)
    {
      s1 = CAR (list)->value_ptr.character;
      l1 = strlen (s1);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (CDR (list))->type == TYPE_STRING)
    {
      s2 = CAR (CDR (list))->value_ptr.string->value;
      l2 = CAR (CDR (list))->value_ptr.string->used_size;
    }
  else if (IS_SYMBOL (CAR (CDR (list))))
    {
      s2 = SYMBOL (CAR (CDR (list)))->value_ptr.symbol->name;
      l2 = SYMBOL (CAR (CDR (list)))->value_ptr.symbol->name_len;
    }
  else if (CAR (CDR (list))->type == TYPE_CHARACTER)
    {
      s2 = CAR (CDR (list))->value_ptr.character;
      l2 = strlen (s2);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (eqmem (s1, l1, s2, l2))
    return &t_object;

  return &nil_object;
  }*/


struct object *
builtin_char_eq (struct object *list, struct environment *env,
		 struct outcome *outcome)
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
		     struct outcome *outcome)
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
		       struct outcome *outcome)
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
		      struct outcome *outcome)
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
		       struct outcome *outcome)
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
		   struct outcome *outcome)
{
  unsigned char *ch;
  unsigned long ret = 0;

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

  ch = (unsigned char *)CAR (list)->value_ptr.character;

  while (*ch)
    {
      ret <<= 8;
      ret = ret + *ch;

      ch++;
    }

  return create_integer_from_unsigned_long (ret);
}


struct object *
builtin_code_char (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  unsigned long code;
  unsigned char *ch, *c, tmp;
  struct object *ret;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_INTEGER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (mpz_cmp_ui (CAR (list)->value_ptr.integer, 0) < 0
      || mpz_cmp_ui (CAR (list)->value_ptr.integer, 4294967295) > 0)
    {
      return &nil_object;
    }

  code = mpz_get_ui (CAR (list)->value_ptr.integer);

  ret = alloc_object ();
  ret->type = TYPE_CHARACTER;
  ret->value_ptr.character = malloc_and_check (5);

  ch = (unsigned char *)ret->value_ptr.character;

  while (code)
    {
      *ch = code & 0xff;
      code >>= 8;
      ch++;
    }

  *ch = 0;

  ch = (unsigned char *)ret->value_ptr.character;
  c = ch + strlen ((char *)ch)-1;

  while (ch < c)
    {
      tmp = *ch;
      *ch = *c;
      *c = tmp;

      ch++, c--;
    }

  return ret;
}


struct object *
builtin_find_package (struct object *list, struct environment *env,
		      struct outcome *outcome)
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
		      struct outcome *outcome)
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

  return create_string_copying_char_vector (pack->value_ptr.package->name,
					    pack->value_ptr.package->name_len);
}


struct object *
builtin_package_nicknames (struct object *list, struct environment *env,
			   struct outcome *outcome)
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
	create_string_copying_char_vector (n->name, n->name_len);

      n = n->next;
    }

  if (ret != &nil_object)
    cons->value_ptr.cons_pair->cdr = &nil_object;

  return ret;
}


struct object *
builtin_rename_package (struct object *list, struct environment *env,
			struct outcome *outcome)
{
  int l = list_length (list), len;
  struct object *cons, *pack, *dbl;
  struct package *p;
  struct name_list *nicks;
  char *name, *newname;

  if (l < 2 || l > 3)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list))
      || !IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l == 3)
    {
      if (!IS_LIST (CAR (CDR (CDR (list)))))
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

  if ((dbl = find_package (name, len, env)) && dbl != pack)
    {
      outcome->type = PACKAGE_NAME_OR_NICKNAME_ALREADY_IN_USE;
      return NULL;
    }

  p = pack->value_ptr.package;

  free_name_list (p->nicks);
  p->nicks = NULL;

  newname = malloc_and_check (len);
  memcpy (newname, name, len);
  free (p->name);
  p->name = newname;
  p->name_len = len;

  if (l == 3)
    {
      cons = CAR (CDR (CDR (list)));

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

	  if ((dbl = find_package (name, len, env)) && dbl != pack)
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
    }

  return pack;
}


struct object *
builtin_package_use_list (struct object *list, struct environment *env,
			  struct outcome *outcome)
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
			      struct outcome *outcome)
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
			   struct outcome *outcome)
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
		      struct outcome *outcome)
{
  char *name;
  int len;
  struct object *ret, *nicks = NULL, *args;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
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

  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":NICKNAMES", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!nicks)
	    nicks = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  outcome->type = UNKNOWN_KEYWORD_ARGUMENT;
	  return NULL;
	}

      list = CDR (list);
    }

  ret = create_package (name, len);

  prepend_object_to_obj_list (ret, &env->packages);

  if (nicks)
    {
      args = alloc_empty_cons_pair ();
      args->value_ptr.cons_pair->car = ret;
      args->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
      args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = ret;
      args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr =
	alloc_empty_cons_pair ();
      args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr->
	value_ptr.cons_pair->car = nicks;
      increment_refcount (nicks);
      args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr->
	value_ptr.cons_pair->cdr = &nil_object;

      if (!builtin_rename_package (args, env, outcome))
	return NULL;

      decrement_refcount (args);
    }

  return ret;
}


struct object *
builtin_in_package (struct object *list, struct environment *env,
		    struct outcome *outcome)
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

  return set_value (env->package_sym, pack, 1, env, outcome);
}


struct object *
builtin_import (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  int l = list_length (list), ret;
  struct object *pack, *cons;

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      pack = inspect_package_by_designator (CAR (CDR (list)), env);

      if (!pack)
	{
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}
    }
  else
    pack = inspect_variable (env->package_sym, env);

  if (SYMBOL (CAR (list)) == &nil_object)
    {
      return &t_object;
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      ret = import_symbol (SYMBOL (CAR (list)), pack, NULL);

      if (!ret)
	{
	  outcome->type = IMPORTING_SYMBOL_WOULD_CAUSE_CONFLICT;
	  return NULL;
	}
    }
  else if (CAR (list)->type == TYPE_CONS_PAIR)
    {
      cons = CAR (list);

      while (SYMBOL (cons) != &nil_object)
	{
	  if (!IS_SYMBOL (CAR (cons)))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  ret = import_symbol (SYMBOL (CAR (cons)), pack, NULL);

	  if (!ret)
	    {
	      outcome->type = IMPORTING_SYMBOL_WOULD_CAUSE_CONFLICT;
	      return NULL;
	    }

	  cons = CDR (cons);
	}
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return &t_object;
}


struct object *
builtin_export (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  int l = list_length (list), ret, pres, pres2;
  struct object *pack, *cons = NULL, *sym;
  struct package_record *rec;
  struct object_list *used;

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      pack = inspect_package_by_designator (CAR (CDR (list)), env);

      if (!pack)
	{
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}
    }
  else
    pack = inspect_variable (env->package_sym, env);

  if (CAR (list)->type == TYPE_CONS_PAIR)
    {
      cons = CAR (list);
      sym = SYMBOL (CAR (cons));
    }
  else if (SYMBOL (CAR (list)) == &nil_object)
    {
      return &t_object;
    }
  else
    sym = SYMBOL (CAR (list));

  do
    {
      if (!sym)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      rec = inspect_accessible_symbol (sym, pack, &pres);

      if (!rec)
	{
	  outcome->type = SYMBOL_NOT_ACCESSIBLE_IN_PACKAGE;
	  return NULL;
	}

      if (!pres || rec->visibility == INTERNAL_VISIBILITY)
	{
	  used = pack->value_ptr.package->used_by;

	  while (used)
	    {
	      if (inspect_accessible_symbol_by_name (sym->value_ptr.symbol->name,
						     sym->value_ptr.symbol->name_len,
						     used->obj, &pres2))
		{
		  outcome->type = EXPORTING_SYMBOL_WOULD_CAUSE_CONFLICT;
		  return NULL;
		}

	      used = used->next;
	    }

	  if (!pres)
	    {
	      ret = import_symbol (sym, pack, &rec);

	      if (!ret)
		{
		  outcome->type = IMPORTING_SYMBOL_WOULD_CAUSE_CONFLICT;
		  return NULL;
		}
	    }

	  rec->visibility = EXTERNAL_VISIBILITY;
	}

      if (cons)
	{
	  cons = CDR (cons);

	  if (SYMBOL (cons) != &nil_object)
	    sym = SYMBOL (CAR (cons));
	}
    } while (cons && SYMBOL (cons) != &nil_object);

  return &t_object;
}


struct object *
builtin_unexport (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  int l = list_length (list), pres;
  struct object *pack, *cons = NULL, *sym;
  struct package_record *rec;

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      pack = inspect_package_by_designator (CAR (CDR (list)), env);

      if (!pack)
	{
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}
    }
  else
    pack = inspect_variable (env->package_sym, env);

  if (CAR (list)->type == TYPE_CONS_PAIR)
    {
      cons = CAR (list);
      sym = SYMBOL (CAR (cons));
    }
  else if (SYMBOL (CAR (list)) == &nil_object)
    {
      return &t_object;
    }
  else
    sym = SYMBOL (CAR (list));

  do
    {
      if (!sym)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      rec = inspect_accessible_symbol (sym, pack, &pres);

      if (!rec)
	{
	  outcome->type = SYMBOL_NOT_ACCESSIBLE_IN_PACKAGE;
	  return NULL;
	}

      if (rec->visibility == INTERNAL_VISIBILITY)
	return &t_object;

      if (!pres)
	{
	  outcome->type = SYMBOL_NOT_PRESENT_IN_PACKAGE;
	  return NULL;
	}

      rec->visibility = INTERNAL_VISIBILITY;

      if (cons)
	{
	  cons = CDR (cons);

	  if (SYMBOL (cons) != &nil_object)
	    sym = SYMBOL (CAR (cons));
	}
    } while (cons && SYMBOL (cons) != &nil_object);

  return &t_object;
}


struct object *
builtin_use_package (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  int l = list_length (list);
  struct object *pack, *cons = NULL, *des, *use, *conf;

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      pack = inspect_package_by_designator (CAR (CDR (list)), env);

      if (!pack)
	{
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}
    }
  else
    pack = inspect_variable (env->package_sym, env);

  if (pack == env->keyword_package)
    {
      outcome->type = KEYWORD_PACKAGE_CANT_USE_ANY_PACKAGE;
      return NULL;
    }

  if (CAR (list)->type == TYPE_CONS_PAIR)
    {
      cons = CAR (list);
      des = CAR (cons);
    }
  else if (SYMBOL (CAR (list)) == &nil_object)
    {
      return &t_object;
    }
  else
    des = CAR (list);

  do
    {
      if (!IS_PACKAGE_DESIGNATOR (des))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      use = inspect_package_by_designator (des, env);

      if (!des)
	{
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}

      if (use == env->keyword_package)
	{
	  outcome->type = CANT_USE_KEYWORD_PACKAGE;
	  return NULL;
	}

      if (!use_package (use, pack, &conf))
	{
	  outcome->type = USING_PACKAGE_WOULD_CAUSE_CONFLICT_ON_SYMBOL;
	  outcome->obj = conf;
	  outcome->pack = des;
	  return NULL;
	}

      if (cons)
	{
	  cons = CDR (cons);
	  des = CAR (cons);
	}
    } while (cons && SYMBOL (cons) != &nil_object);

  return &t_object;
}


struct object *
builtin_unuse_package (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  int l = list_length (list);
  struct object *pack, *cons = NULL, *des, *use;

  if (!l || l > 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      pack = inspect_package_by_designator (CAR (CDR (list)), env);

      if (!pack)
	{
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}
    }
  else
    pack = inspect_variable (env->package_sym, env);

  if (pack == env->keyword_package)
    {
      outcome->type = KEYWORD_PACKAGE_CANT_USE_ANY_PACKAGE;
      return NULL;
    }

  if (CAR (list)->type == TYPE_CONS_PAIR)
    {
      cons = CAR (list);
      des = CAR (cons);
    }
  else if (SYMBOL (CAR (list)) == &nil_object)
    {
      return &t_object;
    }
  else
    des = CAR (list);

  do
    {
      if (!IS_PACKAGE_DESIGNATOR (des))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      use = inspect_package_by_designator (des, env);

      if (!des)
	{
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}

      if (use == env->keyword_package)
	{
	  outcome->type = CANT_USE_KEYWORD_PACKAGE;
	  return NULL;
	}

      if (!unuse_package (use, pack))
	{
	  outcome->type = PACKAGE_IS_NOT_IN_USE;
	  return NULL;
	}

      if (cons)
	{
	  cons = CDR (cons);
	  des = CAR (cons);
	}
    } while (cons && SYMBOL (cons) != &nil_object);

  return &t_object;
}


struct object *
builtin_do_symbols (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct object *des, *pack, *var, *ret, *p;
  struct object_list *u = NULL;
  struct package_record *rec;
  int l, i;

  if (list_length (list) < 1 || CAR (list)->type != TYPE_CONS_PAIR
      || (l = list_length (CAR (list))) > 3 || !IS_SYMBOL (CAR (CAR (list))))
    {
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      return NULL;
    }

  env->blocks = add_block (&nil_object, env->blocks);

  if (l > 1)
    {
      des = evaluate_object (CAR (CDR (CAR (list))), env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!des)
	return NULL;

      if (!IS_PACKAGE_DESIGNATOR (des))
	{
	  env->blocks = remove_block (env->blocks);
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      pack = inspect_package_by_designator (des, env);

      if (!pack)
	{
	  env->blocks = remove_block (env->blocks);
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}
    }
  else
    pack = inspect_variable (env->package_sym, env);


  var = SYMBOL (CAR (CAR (list)));
  p = pack;

  while (1)
    {
      for (i = 0; i < SYMTABLE_SIZE; i++)
	{
	  rec = p->value_ptr.package->symtable [i];

	  while (rec)
	    {
	      if (u && rec->visibility != EXTERNAL_VISIBILITY)
		{
		  rec = rec->next;
		  continue;
		}

	      increment_refcount (rec->sym);
	      env->vars = bind_variable (var, rec->sym, env->vars);

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
		      outcome->no_value = outcome->return_no_value;
		      outcome->other_values = outcome->return_other_values;
		      return outcome->return_value;
		    }
		  else
		    return NULL;
		}

	      rec = rec->next;
	    }
	}

      if (!u)
	u = pack->value_ptr.package->uses;
      else
	u = u->next;

      if (!u)
	break;

      p = u->obj;
    }

  if (l == 3)
    {
      env->vars = bind_variable (var, &nil_object, env->vars);

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
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
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
builtin_do_external_symbols (struct object *list, struct environment *env,
			     struct outcome *outcome)
{
  struct object *des, *pack, *var, *ret;
  struct package_record *rec;
  int l, i;

  if (list_length (list) < 1 || CAR (list)->type != TYPE_CONS_PAIR
      || (l = list_length (CAR (list))) > 3 || !IS_SYMBOL (CAR (CAR (list))))
    {
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      return NULL;
    }

  env->blocks = add_block (&nil_object, env->blocks);

  if (l > 1)
    {
      des = evaluate_object (CAR (CDR (CAR (list))), env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!des)
	return NULL;

      if (!IS_PACKAGE_DESIGNATOR (des))
	{
	  env->blocks = remove_block (env->blocks);
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      pack = inspect_package_by_designator (des, env);

      if (!pack)
	{
	  env->blocks = remove_block (env->blocks);
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}
    }
  else
    pack = inspect_variable (env->package_sym, env);


  var = SYMBOL (CAR (CAR (list)));

  for (i = 0; i < SYMTABLE_SIZE; i++)
    {
      rec = pack->value_ptr.package->symtable [i];

      while (rec)
	{
	  if (rec->visibility != EXTERNAL_VISIBILITY)
	    {
	      rec = rec->next;
	      continue;
	    }

	  increment_refcount (rec->sym);
	  env->vars = bind_variable (var, rec->sym, env->vars);

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
		  outcome->no_value = outcome->return_no_value;
		  outcome->other_values = outcome->return_other_values;
		  return outcome->return_value;
		}
	      else
		return NULL;
	    }

	  rec = rec->next;
	}
    }

  if (l == 3)
    {
      env->vars = bind_variable (var, &nil_object, env->vars);

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
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
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
builtin_get_internal_run_time (struct object *list, struct environment *env,
			       struct outcome *outcome)
{
  if (list_length (list))
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  return create_integer_from_long (clock ());
}


struct object *
builtin_get_decoded_time (struct object *list, struct environment *env,
			  struct outcome *outcome)
{
  time_t t, t2;
  struct tm *lt, *gmt;
  double tz;

  if (list_length (list))
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  t = time (NULL);

  gmt = gmtime (&t);
  t2 = mktime (gmt);

  lt = localtime (&t);
  tz = difftime (t2, t);

  prepend_object_to_obj_list (create_ratio_from_longs (tz, 3600),
  &outcome->other_values);
  prepend_object_to_obj_list (lt->tm_wday > 0 ? &t_object : &nil_object,
			      &outcome->other_values);
  prepend_object_to_obj_list (create_integer_from_long (!lt->tm_wday ? 6
							: lt->tm_wday-1),
			      &outcome->other_values);
  prepend_object_to_obj_list (create_integer_from_long (lt->tm_year+1900),
			      &outcome->other_values);
  prepend_object_to_obj_list (create_integer_from_long (lt->tm_mon+1),
			      &outcome->other_values);
  prepend_object_to_obj_list (create_integer_from_long (lt->tm_mday),
			      &outcome->other_values);
  prepend_object_to_obj_list (create_integer_from_long (lt->tm_hour),
			      &outcome->other_values);
  prepend_object_to_obj_list (create_integer_from_long (lt->tm_min),
			      &outcome->other_values);

  return create_integer_from_long (lt->tm_sec == 60 ? 0 : lt->tm_sec);
}


struct object *
builtin_lisp_implementation_type (struct object *list, struct environment *env,
				  struct outcome *outcome)
{
  if (list_length (list))
    {
      outcome->type = TOO_MANY_ARGUMENTS;

      return NULL;
    }

  return create_string_copying_c_string ("alisp");
}


struct object *
builtin_lisp_implementation_version (struct object *list,
				     struct environment *env,
				     struct outcome *outcome)
{
  if (list_length (list))
    {
      outcome->type = TOO_MANY_ARGUMENTS;

      return NULL;
    }

  return create_string_copying_c_string (PACKAGE_VERSION);
}


struct object *
builtin_software_type (struct object *list, struct environment *env,
		       struct outcome *outcome)
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
			  struct outcome *outcome)
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
			      struct outcome *outcome, int allow_three_elements)
{
  struct object *sym, *val;
  int l;

  if (form->type == TYPE_SYMBOL_NAME || form->type == TYPE_SYMBOL)
    {
      sym = SYMBOL (form);
      val = &nil_object;
    }
  else if (form->type == TYPE_CONS_PAIR)
    {
      l = list_length (form);

      if (l > (allow_three_elements ? 3 : 2) || !IS_SYMBOL (CAR (form)))
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

      if (l == 1)
	{
	  val = &nil_object;
	}
      else
	{
	  val = evaluate_object (CAR (CDR (form)), env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!val)
	    return NULL;
	}
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

  increment_refcount (sym);

  if (sym->value_ptr.symbol->is_parameter)
    {
      sym->value_ptr.symbol->value_dyn_bins_num++;

      return create_binding (sym, val, DYNAMIC_BINDING, 0);
    }
  else
    return create_binding (sym, val, LEXICAL_BINDING, 0);
}


struct object *
evaluate_let (struct object *list, struct environment *env,
	      struct outcome *outcome)
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

  while (SYMBOL (bind_forms) != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome, 0);

      if (!bin)
	return NULL;

      bins = add_binding (bin, bins);
      bin_num++;

      bind_forms = CDR (bind_forms);
    }

  env->vars = chain_bindings (bins, env->vars, NULL, NULL);

  env->lex_env_vars_boundary += bin_num;

  if (!parse_declarations (CDR (list), env, bin_num, outcome, &body))
    {
      res = NULL;
      goto cleanup_and_leave;
    }

  res = evaluate_body (body, 0, NULL, env, outcome);

  undo_special_declarations (CDR (list), env);

 cleanup_and_leave:
  env->vars = remove_bindings (env->vars, bin_num);

  env->lex_env_vars_boundary -= bin_num;

  return res;
}


struct object *
evaluate_let_star (struct object *list, struct environment *env,
		   struct outcome *outcome)
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

  while (SYMBOL (bind_forms) != &nil_object)
    {
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome, 0);

      if (!bin)
	{
	  env->vars = remove_bindings (env->vars, bin_num);
	  env->lex_env_vars_boundary -= bin_num;
	  return NULL;
	}

      env->vars = add_binding (bin, env->vars);
      env->lex_env_vars_boundary++, bin_num++;

      bind_forms = CDR (bind_forms);
    }

  if (!parse_declarations (CDR (list), env, bin_num, outcome, &body))
    {
      env->vars = remove_bindings (env->vars, bin_num);
      env->lex_env_vars_boundary -= bin_num;
      return NULL;
    }

  res = evaluate_body (body, 0, NULL, env, outcome);

  undo_special_declarations (CDR (list), env);

  env->vars = remove_bindings (env->vars, bin_num);

  env->lex_env_vars_boundary -= bin_num;

  return res;
}


struct object *
evaluate_progv (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *syms, *vals, *cons1, *cons2, *ret;
  struct binding *b, *bins = NULL;
  int binnum = 0;

  if (list_length (list) < 2)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  syms = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!syms)
    return NULL;

  vals = evaluate_object (CAR (CDR (list)), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!vals)
    return NULL;

  if (!IS_LIST (syms) || !IS_LIST (vals))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  cons1 = syms, cons2 = vals;

  while (SYMBOL (cons1) != &nil_object && SYMBOL (cons2) != &nil_object)
    {
      if (!IS_SYMBOL (CAR (cons1)))
	{
	  env->vars = chain_bindings (bins, env->vars, NULL, NULL);
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  ret = NULL;
	  goto cleanup_and_leave;
	}

      b = create_binding (SYMBOL (CAR (cons1)), CAR (cons2), DYNAMIC_BINDING, 0);
      increment_refcount (b->sym);

      if (!bins)
	bins = b;
      else
	{
	  b->next = bins;
	  bins = b;
	}

      b->sym->value_ptr.symbol->value_dyn_bins_num++;
      binnum++;

      cons1 = CDR (cons1);
      cons2 = CDR (cons2);
    }

  env->vars = chain_bindings (bins, env->vars, NULL, NULL);
  env->lex_env_vars_boundary += binnum;
  bins = NULL;

  ret = evaluate_body (CDR (CDR (list)), 0, NULL, env, outcome);

 cleanup_and_leave:
  env->lex_env_vars_boundary -= binnum;

  for (; binnum; binnum--)
    {
      b = env->vars;
      env->vars = env->vars->next;
      b->sym->value_ptr.symbol->value_dyn_bins_num--;
      decrement_refcount (b->sym);
      free (b);
    }

  decrement_refcount (syms);
  decrement_refcount (vals);

  return ret;
}


struct binding *
create_binding_from_flet_form (struct object *form, struct environment *env,
			       struct outcome *outcome,
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
			     type == TYPE_MACRO, 0);

      if (!fun)
	return NULL;

      fun->type = type;
    }
  else
    {
      outcome->type = INCORRECT_SYNTAX_IN_FLET;
      return NULL;
    }

  increment_refcount (sym);
  return create_binding (sym, fun, LEXICAL_BINDING, 0);
}


struct object *
evaluate_flet (struct object *list, struct environment *env,
	       struct outcome *outcome)
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

  env->funcs = chain_bindings (bins, env->funcs, NULL, NULL);

  env->lex_env_funcs_boundary += bin_num;

  res = evaluate_body (body, 0, NULL, env, outcome);

  env->funcs = remove_bindings (env->funcs, bin_num);

  env->lex_env_funcs_boundary -= bin_num;

  return res;
}


struct object *
evaluate_labels (struct object *list, struct environment *env,
		 struct outcome *outcome)
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
		   struct outcome *outcome)
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

  env->funcs = chain_bindings (bins, env->funcs, NULL, NULL);

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

  if (s->is_const)
    {
      increment_refcount (s->value_cell);

      return s->value_cell;
    }

  b = find_binding (s, env->vars, ANY_BINDING, env->lex_env_vars_boundary);

  if (b)
    {
      increment_refcount (b->obj);

      return b->obj;
    }

  if (s->value_cell)
    {
      increment_refcount (s->value_cell);

      return s->value_cell;
    }

  return NULL;
}


struct object *
get_function (struct object *sym, struct environment *env, int only_functions,
	      int setf_func, int only_globals, int increment_refc)
{
  struct object *f;
  struct binding *b = NULL, *n = NULL;

  if (!only_globals)
    {
      do
	{
	  if (n)
	    n = b->next;
	  else
	    n = env->funcs;

	  b = find_binding (SYMBOL (sym)->value_ptr.symbol, n, ANY_BINDING,
			    env->lex_env_funcs_boundary);

	} while (b && setf_func && (b->obj->type != TYPE_FUNCTION
				    || !b->obj->value_ptr.function->is_setf_func));
    }

  if ((only_globals || !b)
      && ((!setf_func && !SYMBOL (sym)->value_ptr.symbol->function_cell)
	  || (setf_func && !SYMBOL (sym)->value_ptr.symbol->setf_func_cell)))
    {
      return NULL;
    }

  if (b)
    f = b->obj;
  else if (setf_func)
    f = SYMBOL (sym)->value_ptr.symbol->setf_func_cell;
  else
    f = SYMBOL (sym)->value_ptr.symbol->function_cell;

  if (f->type != TYPE_FUNCTION && only_functions)
    return NULL;

  if (increment_refc)
    increment_refcount (f);

  return f;
}


struct object *
inspect_variable_by_c_string (char *var, struct environment *env)
{
  struct object *pack = inspect_variable (env->package_sym, env);
  struct object *sym = intern_symbol_by_char_vector (var, strlen (var), 0,
						     INTERNAL_VISIBILITY, 0,
						     pack);

  if (sym)
    return inspect_variable (sym, env);

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
set_value (struct object *sym, struct object *value, int eval_value,
	   struct environment *env, struct outcome *outcome)
{
  struct symbol *s = sym->value_ptr.symbol;
  struct object *val;
  struct binding *b;

  if (s->is_const)
    {
      outcome->type = CANT_REDEFINE_CONSTANT_BY_DIFFERENT_OPERATOR;
      return NULL;
    }

  if (eval_value)
    {
      val = evaluate_object (value, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!val)
	return NULL;
    }
  else
    val = value;

  if (s->is_parameter || s->is_special)
    {
      if (!s->value_dyn_bins_num)
	{
	  delete_reference (sym, s->value_cell, 0);
	  add_reference (sym, val, 0);
	  s->value_cell = val;
	}
      else
	{
	  b = find_binding (s, env->vars, DYNAMIC_BINDING, -1);
	  decrement_refcount (b->obj);
	  b->obj = val;
	  increment_refcount (val);
	}
    }
  else
    {
      b = find_binding (s, env->vars, LEXICAL_BINDING,
			env->lex_env_vars_boundary);

      if (b)
	{
	  decrement_refcount (b->obj);
	  b->obj = val;
	  increment_refcount (val);
	}
      else
	{
	  sym->value_ptr.symbol->is_parameter = 1;
	  sym->value_ptr.symbol->value_cell = val;
	  add_reference (sym, val, 0);
	}
    }

  return val;
}


struct object *
evaluate_quote (struct object *list, struct environment *env,
		struct outcome *outcome)
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
	     struct outcome *outcome)
{
  struct object *if_clause, *ret;
  int l = list_length (list);

  if (l < 2 || l > 3)
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
      decrement_refcount (if_clause);

      ret = evaluate_object (CAR (CDR (list)), env, outcome);
      return ret;
    }
  else
    {
      decrement_refcount (if_clause);

      if (l == 2)
	{
	  return &nil_object;
	}
      else
	{
	  ret = evaluate_object (CAR (CDR (CDR (list))), env, outcome);
	  return ret;
	}
    }
}


struct object *
evaluate_progn (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  return evaluate_body (list, 0, NULL, env, outcome);
}


struct object *
evaluate_values (struct object *list, struct environment *env,
		 struct outcome *outcome)
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
		      struct outcome *outcome)
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
			      struct outcome *outcome)
{
  struct object *res, *ret, *cons;
  struct object_list *vals;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  res = evaluate_object (CAR (list), env, outcome);

  if (!res)
    return NULL;

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
evaluate_multiple_value_call (struct object *list, struct environment *env,
			      struct outcome *outcome)
{
  struct object *fun, *args = &nil_object, *cons, *res, *ret;
  struct object_list *l;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  fun = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!fun)
    return NULL;

  if (fun->type != TYPE_FUNCTION)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      res = evaluate_object (CAR (list), env, outcome);

      if (!res)
	return NULL;

      if (!outcome->no_value)
	{
	  if (args == &nil_object)
	    {
	      args = cons = alloc_empty_cons_pair ();
	    }
	  else
	    {
	      cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	    }

	  cons->value_ptr.cons_pair->car = res;

	  l = outcome->other_values;

	  while (l)
	    {
	      cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

	      cons->value_ptr.cons_pair->car = l->obj;

	      l = l->next;
	    }

	  free_object_list_structure (outcome->other_values);
	  outcome->other_values = NULL;
	}
      else
	{
	  outcome->no_value = 0;
	}

      list = CDR (list);
    }

  if (args != &nil_object)
    cons->value_ptr.cons_pair->cdr = &nil_object;

  ret = call_function (fun, args, 0, 0, 1, 0, 0, env, outcome);

  decrement_refcount (args);
  decrement_refcount (fun);

  return ret;
}


struct object *
evaluate_eval_when (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct object *cons;
  int run = 0;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  cons = CAR (list);

  while (SYMBOL (cons) != &nil_object)
    {
      if (!IS_SYMBOL (CAR (cons)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (symbol_equals (CAR (cons), ":EXECUTE", env)
	  || symbol_equals (CAR (cons), "EVAL", env))
	{
	  run = 1;
	}
      else if (!symbol_equals (CAR (cons), ":COMPILE-TOPLEVEL", env)
	       && !symbol_equals (CAR (cons), "COMPILE", env)
	       && !symbol_equals (CAR (cons), ":LOAD-TOPLEVEL", env)
	       && !symbol_equals (CAR (cons), "LOAD", env))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      cons = CDR (cons);
    }

  if (run)
    {
      return evaluate_body (CDR (list), 0, NULL, env, outcome);
    }
  else
    {
      return &nil_object;
    }
}


struct object *
evaluate_defconstant (struct object *list, struct environment *env,
		      struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return define_constant (CAR (list)->value_ptr.symbol_name->sym,
			  CAR (CDR (list)), env, outcome);
}


struct object *
evaluate_defparameter (struct object *list, struct environment *env,
		       struct outcome *outcome)
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

  return define_parameter (s, CAR (CDR (list)), env, outcome);
}


struct object *
evaluate_defvar (struct object *list, struct environment *env,
		 struct outcome *outcome)
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
      s->value_ptr.symbol->is_parameter = 1;
    }
  else if (l == 2)
    {
      if (!s->value_ptr.symbol->value_cell)
	return define_parameter (s, CAR (CDR (list)), env, outcome);
    }

  increment_refcount (s);
  return s;
}


struct object *
evaluate_defun (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *fun, *sym, *ret;

  if (list_length (list) < 2
      || (!IS_SYMBOL (CAR (list)) && !(CAR (list)->type == TYPE_CONS_PAIR
				       && list_length (CAR (list)) == 2
				       && SYMBOL (CAR (CAR (list))) == env->setf_sym
				       && IS_SYMBOL (CAR (CDR (CAR (list))))))
      || (!IS_LIST (CAR (CDR (list)))))
    {
      outcome->type = INCORRECT_SYNTAX_IN_DEFUN;
      return NULL;
    }

  sym = IS_SYMBOL (CAR (list)) ? SYMBOL (CAR (list))
    : SYMBOL (CAR (CDR (CAR (list))));

  if (sym->value_ptr.symbol->function_cell
      && sym->value_ptr.symbol->function_cell->type == TYPE_MACRO
      && sym->value_ptr.symbol->function_cell->value_ptr.macro->is_special_operator)
    {
      outcome->type = CANT_REDEFINE_SPECIAL_OPERATOR;
      return NULL;
    }

  fun = create_function (CAR (CDR (list)), CDR (CDR (list)), env, outcome, 0, 0);

  if (!fun)
    return NULL;

  if (IS_SYMBOL (CAR (list)))
    {
      if (sym->value_ptr.symbol->function_cell)
	{
	  delete_reference (sym, sym->value_ptr.symbol->function_cell, 1);
	}

      sym->value_ptr.symbol->function_cell = fun;
      add_reference (sym, fun, 1);
      decrement_refcount (fun);
    }
  else
    {
      if (sym->value_ptr.symbol->setf_func_cell)
	{
	  delete_reference (sym, sym->value_ptr.symbol->setf_func_cell, 2);
	}

      sym->value_ptr.symbol->setf_func_cell = fun;
      add_reference (sym, fun, 2);
      decrement_refcount (fun);
    }

  fun->value_ptr.function->name = sym;
  add_reference (fun, sym, 0);

  if (!IS_SYMBOL (CAR (list)))
    {
      fun->value_ptr.function->is_setf_func = 1;

      ret = alloc_empty_cons_pair ();

      increment_refcount (env->setf_sym);
      ret->value_ptr.cons_pair->car = env->setf_sym;

      ret->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
      ret->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr = &nil_object;

      increment_refcount (sym);
      ret->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = sym;

      return ret;
    }

  increment_refcount (sym);
  return sym;
}


struct object *
evaluate_defmacro (struct object *list, struct environment *env,
		   struct outcome *outcome)
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

  mac = create_function (CAR (CDR (list)), CDR (CDR (list)), env, outcome, 1, 1);

  if (!mac)
    return NULL;

  mac->type = TYPE_MACRO;

  if (sym->value_ptr.symbol->function_cell)
    {
      delete_reference (sym, sym->value_ptr.symbol->function_cell, 1);
    }

  sym->value_ptr.symbol->function_cell = mac;
  add_reference (sym, mac, 1);
  decrement_refcount (mac);

  mac->value_ptr.function->name = sym;
  add_reference (mac, sym, 0);

  increment_refcount (sym);
  return sym;
}


struct object *
evaluate_setq (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  struct object *ret = &nil_object;

  if (list_length (list) % 2)
    {
      outcome->type = ODD_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  while (SYMBOL (list) != &nil_object)
    {
      decrement_refcount (ret);

      if (!IS_SYMBOL (CAR (list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = set_value (SYMBOL (CAR (list)), CAR (CDR (list)), 1, env, outcome);

      if (!ret)
	return NULL;

      list = CDR (CDR (list));
    }

  return ret;
}


struct object *
evaluate_psetq (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *cons = list;
  int l = list_length (list);
  struct object_list *ls, *last;

  if (l % 2)
    {
      outcome->type = ODD_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  last = ls = alloc_empty_object_list (l / 2);

  while (SYMBOL (cons) != &nil_object)
    {
      if (!IS_SYMBOL (CAR (cons)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      last->obj = evaluate_object (CAR (CDR (cons)), env, outcome);

      if (!last->obj)
	return NULL;

      cons = CDR (CDR (cons));
      last = last->next;
    }

  cons = list;
  last = ls;

  while (SYMBOL (cons) != &nil_object)
    {
      set_value (SYMBOL (CAR (cons)), last->obj, 0, env, outcome);

      cons = CDR (CDR (cons));
      last = last->next;
    }

  free_object_list (ls);

  return &nil_object;
}


struct object *
evaluate_setf (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  struct object *val = &nil_object, *exp, *cons1, *cons2, *res, *fun, *args;
  struct object_list *l, *expvals;
  int binsnum = 0;

  if (list_length (list) % 2)
    {
      outcome->type = ODD_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  while (SYMBOL (list) != &nil_object)
    {
      decrement_refcount (val);

      if (IS_SYMBOL (CAR (list)))
	{
	  val = set_value (SYMBOL (CAR (list)), nth (1, list), 1, env, outcome);

	  if (!val)
	    return NULL;
	}
      else if (CAR (list)->type == TYPE_CONS_PAIR)
	{
	  if (!IS_SYMBOL (CAR (CAR (list))))
	    {
	      outcome->type = INVALID_ACCESSOR;
	      return NULL;
	    }

	  if (SYMBOL (CAR (CAR (list)))->value_ptr.symbol->setf_expander)
	    {
	      exp = call_function (SYMBOL (CAR (CAR (list)))->value_ptr.symbol->
				   setf_expander, CDR (CAR (list)), 0, 0, 0, 0,
				   0, env, outcome);

	      if (!exp)
		return NULL;

	      if (object_list_length (outcome->other_values) < 4)
		{
		  outcome->type = SETF_EXPANDER_PRODUCED_NOT_ENOUGH_VALUES;
		  return NULL;
		}

	      expvals = outcome->other_values;
	      outcome->other_values = NULL;

	      cons1 = exp;
	      cons2 = expvals->obj;

	      while (SYMBOL (cons1) != &nil_object)
		{
		  if (!IS_SYMBOL (CAR (cons1)) || cons2->type != TYPE_CONS_PAIR)
		    {
		      outcome->type = INVALID_SETF_EXPANSION;
		      return NULL;
		    }

		  res = evaluate_object (CAR (cons2), env, outcome);
		  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

		  if (!res)
		    return NULL;

		  env->vars = bind_variable (SYMBOL (CAR (cons1)), res,
					     env->vars);
		  binsnum++;

		  cons1 = CDR (cons1);
		  cons2 = CDR (cons2);
		}

	      decrement_refcount (exp);
	      decrement_refcount (expvals->obj);

	      val = evaluate_object (CAR (CDR (list)), env, outcome);

	      if (!val)
		return NULL;

	      cons1 = expvals->next->obj;
	      l = NULL;

	      while (SYMBOL (cons1) != &nil_object)
		{
		  if (!IS_SYMBOL (CAR (cons1)))
		    {
		      outcome->type = INVALID_SETF_EXPANSION;
		      return NULL;
		    }

		  if (!l)
		    {
		      env->vars = bind_variable (SYMBOL (CAR (cons1)), val,
						 env->vars);
		      l = outcome->other_values;
		    }
		  else
		    {
		      env->vars = bind_variable (SYMBOL (CAR (cons1)), l->obj,
						 env->vars);
		      l = l->next;
		    }

		  binsnum++;

		  cons1 = CDR (cons1);
		}

	      decrement_refcount (expvals->next->obj);

	      free_object_list_structure (outcome->other_values);
	      outcome->other_values = NULL;

	      env->lex_env_vars_boundary += binsnum;

	      val = evaluate_object (expvals->next->next->obj, env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      free_object_list (expvals->next->next);
	      expvals->next->next = NULL;
	      free_object_list_structure (expvals);

	      env->vars = remove_bindings (env->vars, binsnum);
	      env->lex_env_vars_boundary -= binsnum;
	    }
	  else if ((fun = SYMBOL (CAR (CAR (list)))->value_ptr.symbol->
		    function_cell)
		   && fun->value_ptr.function->struct_accessor_class)
	    {
	      args = evaluate_through_list (CDR (CAR (list)), env, outcome);

	      if (!args)
		return NULL;

	      val = evaluate_object (CAR (CDR (list)), env, outcome);

	      if (!val)
		return NULL;

	      val = call_structure_accessor (fun->value_ptr.function->
					     struct_accessor_class,
					     fun->value_ptr.function->
					     struct_accessor_field, args, val,
					     env, outcome);

	      decrement_refcount (args);
	    }
	  else
	    {
	      args = evaluate_through_list (CDR (CAR (list)), env, outcome);

	      if (!args)
		return NULL;

	      val = evaluate_object (CAR (CDR (list)), env, outcome);

	      if (!val)
		return NULL;

	      fun = get_function (SYMBOL (CAR (CAR (list))), env, 1, 1, 0, 0);

	      if (!fun)
		{
		  outcome->type = INVALID_ACCESSOR;
		  return NULL;
		}

	      cons1 = alloc_empty_cons_pair ();
	      cons1->value_ptr.cons_pair->car = val;
	      add_reference (cons1, val, 0);
	      decrement_refcount (val);
	      cons1->value_ptr.cons_pair->cdr = args;
	      add_reference (cons1, args, 1);
	      decrement_refcount (args);

	      val = call_function (fun, cons1, 0, 0, 0, 0, 0, env, outcome);

	      if (!val)
		return NULL;

	      decrement_refcount (cons1);
	    }
	}
      else
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;

	  return NULL;
	}

      list = CDR (CDR (list));
    }

  return val;
}


struct object *
evaluate_function (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  struct object *f;

  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  if (CAR (list)->type != TYPE_SYMBOL_NAME && CAR (list)->type != TYPE_SYMBOL
      && !(CAR (list)->type == TYPE_CONS_PAIR && list_length (CAR (list)) == 2
	   && SYMBOL (CAR (CAR (list))) == env->setf_sym
	   && IS_SYMBOL (CAR (CDR (CAR (list)))))
      && !(CAR (list)->type == TYPE_CONS_PAIR
	   && SYMBOL (CAR (CAR (list))) == env->lambda_sym))
    {
      outcome->type = NOT_A_FUNCTION_NAME_OR_LAMBDA_IN_FUNCTION;

      return NULL;
    }

  if (IS_SYMBOL (CAR (list)) || SYMBOL (CAR (CAR (list))) == env->setf_sym)
    {
      f = get_function (IS_SYMBOL (CAR (list)) ? SYMBOL (CAR (list))
			: SYMBOL (CAR (CDR (CAR (list)))), env, 1,
			CAR (list)->type == TYPE_CONS_PAIR, 0, 1);

      if (!f)
	{
	  outcome->type = FUNCTION_NOT_FOUND_IN_EVAL;

	  return NULL;
	}
    }
  else
    {
      f = evaluate_lambda (CDR (CAR (list)), env, outcome);
    }

  return f;
}


struct object *
evaluate_lambda (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  struct object *fun;

  if (list_length (list) < 1 || (CAR (list)->type != TYPE_CONS_PAIR
				 && SYMBOL (CAR (list)) != &nil_object))
    {
      outcome->type = INCORRECT_SYNTAX_IN_DEFUN;
      return NULL;
    }

  fun = create_function (CAR (list), CDR (list), env, outcome, 0, 0);

  if (!fun)
    return NULL;

  return fun;
}


struct object *
evaluate_apply (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *s, *fun, *last, *l, *args, *ret;
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

      fun = get_function (s, env, 1, 0, 0, 0);

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
      add_reference (l, last, 1);
    }

  ret = call_function (fun, args, 0, 0, 1, 0, 0, env, outcome);

  if (length != 1)
    decrement_refcount (args);

  return ret;
}


struct object *
evaluate_funcall (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *fun;

  if (CAR (list)->type == TYPE_FUNCTION)
    {
      fun = CAR (list);
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      fun = get_function (SYMBOL (CAR (list)), env, 1, 0, 0, 0);

      if (!fun)
	{
	  outcome->type = UNKNOWN_FUNCTION;
	  outcome->obj = SYMBOL (CAR (list));
	  return NULL;
	}
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return call_function (fun, CDR (list), 0, 0, 1, 0, 0, env, outcome);
}


struct object *
evaluate_declare (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  outcome->type = DECLARE_NOT_ALLOWED_HERE;

  return NULL;
}


struct object *
evaluate_the (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)) && CAR (list)->type != TYPE_CONS_PAIR)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return evaluate_object (CAR (CDR (list)), env, outcome);
}


struct object *
evaluate_prog1 (struct object *list, struct environment *env,
		struct outcome *outcome)
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
		struct outcome *outcome)
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
evaluate_destructuring_bind (struct object *list, struct environment *env,
			     struct outcome *outcome)
{
  struct object *fun, *ret, *args;

  if (list_length (list) < 2)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  fun = create_function (CAR (list), CDR (CDR (list)), env, outcome, 0, 1);

  if (!fun)
    return NULL;

  args = evaluate_object (CAR (CDR (list)), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!args)
    return NULL;

  if (!IS_LIST (args))
    {
      decrement_refcount (fun);
      decrement_refcount (args);
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = call_function (fun, args, 0, 0, 0, 0, 0, env, outcome);

  decrement_refcount (fun);
  decrement_refcount (args);

  return ret;
}


struct object *
evaluate_deftype (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *typename, *fun;

  if (list_length (list) < 2 || !IS_SYMBOL (CAR (list))
      || !IS_LIST (CAR (CDR (list))))
    {
      outcome->type = INCORRECT_SYNTAX_IN_DEFTYPE;
      return NULL;
    }

  typename = SYMBOL (CAR (list));

  if (typename->value_ptr.symbol->is_type
      && typename->value_ptr.symbol->is_standard_type)
    {
      outcome->type = CANT_REDEFINE_STANDARD_TYPE;
      return NULL;
    }

  fun = create_function (CAR (CDR (list)), CDR (CDR (list)), env, outcome, 1, 0);

  if (!fun)
    return NULL;

  delete_reference (typename, typename->value_ptr.symbol->typespec, 4);
  typename->value_ptr.symbol->typespec = fun;
  add_reference (typename, fun, 4);
  decrement_refcount (fun);

  typename->value_ptr.symbol->is_type = 1;
  typename->value_ptr.symbol->is_standard_type = 0;

  increment_refcount (typename);
  return typename;
}


struct object *
evaluate_define_setf_expander (struct object *list, struct environment *env,
			       struct outcome *outcome)
{
  struct object *fun;

  if (list_length (list) < 2)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)) || !IS_LIST (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  fun = create_function (CAR (CDR (list)), CDR (CDR (list)), env, outcome, 1, 0);

  if (!fun)
    return NULL;

  delete_reference (SYMBOL (CAR (list)),
		    SYMBOL (CAR (list))->value_ptr.symbol->setf_expander, 3);

  SYMBOL (CAR (list))->value_ptr.symbol->setf_expander = fun;
  add_reference (SYMBOL (CAR (list)), fun, 3);
  decrement_refcount (fun);

  increment_refcount (SYMBOL (CAR (list)));
  return SYMBOL (CAR (list));
}


struct object *
builtin_get_setf_expansion (struct object *list, struct environment *env,
			    struct outcome *outcome)
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

  if (!IS_SYMBOL (CAR (CAR (list)))
      || !SYMBOL (CAR (CAR (list)))->value_ptr.symbol->setf_expander)
    {
      outcome->type = NO_SETF_EXPANDER;
      return NULL;
    }

  return call_function (SYMBOL (CAR (CAR (list)))->value_ptr.symbol->
			setf_expander, CDR (CAR (list)), 0, 0, 0, 0, 0, env,
			outcome);
}


struct object *
evaluate_defstruct (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct object *name, *strcl, *funcname,
    *pack = inspect_variable (env->package_sym, env);
  struct structure_class *sc;
  struct structure_field_decl *f, *prev;
  char *constr_name, *acc_name;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (IS_SYMBOL (CAR (list)))
    name = SYMBOL (CAR (list));
  else if (IS_LIST (CAR (list)) && IS_SYMBOL (CAR (CAR (list))))
    name = SYMBOL (CAR (CAR (list)));
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  strcl = alloc_object ();
  strcl->type = TYPE_STRUCTURE_CLASS;

  sc = malloc_and_check (sizeof (*sc));
  strcl->value_ptr.structure_class = sc;

  increment_refcount (name);
  sc->name = name;

  name->value_ptr.symbol->is_type = 1;

  delete_reference (name, name->value_ptr.symbol->typespec, 4);
  name->value_ptr.symbol->typespec = strcl;
  add_reference (name, strcl, 4);
  decrement_refcount (strcl);


  sc->fields = NULL;
  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      f = create_structure_field_decl (CAR (list), env, outcome);

      if (!f)
	return NULL;

      if (sc->fields)
	prev = prev->next = f;
      else
	sc->fields = prev = f;

      list = CDR (list);
    }

  constr_name =
    concatenate_char_vectors (5 + name->value_ptr.symbol->name_len, "MAKE-", 5,
			      name->value_ptr.symbol->name,
			      name->value_ptr.symbol->name_len, (char *)NULL);

  funcname = intern_symbol_by_char_vector (constr_name,
					   5+name->value_ptr.symbol->name_len, 1,
					   EXTERNAL_VISIBILITY, 1, pack);
  free (constr_name);
  increment_refcount (funcname);

  delete_reference (funcname, funcname->value_ptr.symbol->function_cell, 1);
  funcname->value_ptr.symbol->function_cell = alloc_function ();
  add_reference (funcname, funcname->value_ptr.symbol->function_cell, 1);
  decrement_refcount (funcname->value_ptr.symbol->function_cell);

  funcname->value_ptr.symbol->function_cell->value_ptr.function->
    struct_constructor_class = name;

  funcname->value_ptr.symbol->function_cell->value_ptr.function->name = funcname;
  add_reference (funcname->value_ptr.symbol->function_cell, funcname, 0);

  f = sc->fields;

  while (f)
    {
      acc_name = malloc_and_check (name->value_ptr.symbol->name_len + 1 +
				   f->name->value_ptr.symbol->name_len);
      memcpy (acc_name, name->value_ptr.symbol->name,
	      name->value_ptr.symbol->name_len);
      memcpy (acc_name+name->value_ptr.symbol->name_len, "-", 1);
      memcpy (acc_name+name->value_ptr.symbol->name_len+1,
	      f->name->value_ptr.symbol->name,
	      f->name->value_ptr.symbol->name_len);

      funcname = intern_symbol_by_char_vector (acc_name,
					       name->value_ptr.symbol->name_len +
					       1 +
					       f->name->value_ptr.symbol->name_len,
					       1, EXTERNAL_VISIBILITY, 1, pack);
      free (acc_name);
      increment_refcount (funcname);

      delete_reference (funcname, funcname->value_ptr.symbol->function_cell, 1);
      funcname->value_ptr.symbol->function_cell = alloc_function ();
      add_reference (funcname, funcname->value_ptr.symbol->function_cell, 1);
      decrement_refcount (funcname->value_ptr.symbol->function_cell);

      funcname->value_ptr.symbol->function_cell->value_ptr.function->
	struct_accessor_class = name;
      funcname->value_ptr.symbol->function_cell->value_ptr.function->
	struct_accessor_field = f->name;

      funcname->value_ptr.symbol->function_cell->value_ptr.function->name =
	funcname;
      add_reference (funcname->value_ptr.symbol->function_cell, funcname, 0);

      f = f->next;
    }

  increment_refcount (name);
  return name;
}


struct object *
evaluate_defclass (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  struct object *name, *class, *cons;
  struct standard_class *sc;
  struct class_field_decl *f, *prev;
  struct object_list *p;

  if (list_length (list) < 3)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (IS_SYMBOL (CAR (list)))
    name = SYMBOL (CAR (list));
  else if (IS_LIST (CAR (list)) && IS_SYMBOL (CAR (CAR (list))))
    name = SYMBOL (CAR (CAR (list)));
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (!IS_LIST (CAR (CDR (list))) || !IS_LIST (CAR (CDR (CDR (list)))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  class = alloc_object ();
  class->type = TYPE_STANDARD_CLASS;

  sc = malloc_and_check (sizeof (*sc));
  class->value_ptr.standard_class = sc;

  increment_refcount (name);
  sc->name = name;

  name->value_ptr.symbol->is_type = 1;

  delete_reference (name, name->value_ptr.symbol->typespec, 4);
  name->value_ptr.symbol->typespec = class;
  add_reference (name, class, 4);


  sc->parents = NULL;
  cons = CAR (CDR (list));

  while (SYMBOL (cons) != &nil_object)
    {
      if (!IS_SYMBOL (CAR (cons)) || SYMBOL (CAR (cons)) == &nil_object)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (sc->parents)
	p = p->next = malloc_and_check (sizeof (*p));
      else
	sc->parents = p = malloc_and_check (sizeof (*p));

      p->obj = SYMBOL (CAR (cons));

      cons = CDR (cons);
    }

  if (sc->parents)
    {
      p->next = NULL;
    }
  else
    {
      sc->parents = malloc_and_check (sizeof (*sc->parents));
      sc->parents->obj = CREATE_BUILTIN_SYMBOL ("STANDARD-OBJECT");
      sc->parents->next = NULL;
    }


  sc->fields = NULL;
  list = CAR (CDR (CDR (list)));

  while (SYMBOL (list) != &nil_object)
    {
      f = create_class_field_decl (CAR (list), env, outcome);

      if (!f)
	return NULL;

      if (sc->fields)
	prev = prev->next = f;
      else
	sc->fields = prev = f;

      list = CDR (list);
    }

  return class;
}


struct object *
builtin_find_class (struct object *list, struct environment *env,
		    struct outcome *outcome)
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

  if (SYMBOL (CAR (list))->value_ptr.symbol->typespec &&
      SYMBOL (CAR (list))->value_ptr.symbol->typespec->type
      == TYPE_STANDARD_CLASS)
    {
      increment_refcount (SYMBOL (CAR (list))->value_ptr.symbol->typespec);
      return SYMBOL (CAR (list))->value_ptr.symbol->typespec;
    }
  else
    {
      outcome->type = CLASS_NOT_FOUND;
      return NULL;
    }
}


struct object *
builtin_make_instance (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct object *class = NULL, *ret;
  struct standard_object *so;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STANDARD_CLASS)
    {
      class = CAR (list);
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      if (SYMBOL (CAR (list))->value_ptr.symbol->typespec &&
	  SYMBOL (CAR (list))->value_ptr.symbol->typespec->type
	  == TYPE_STANDARD_CLASS)
	{
	  class = SYMBOL (CAR (list))->value_ptr.symbol->typespec;
	}
      else
	{
	  outcome->type = CLASS_NOT_FOUND;
	  return NULL;
	}
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (!is_class_completely_defined (class))
    {
      outcome->type = CLASS_DEFINITION_NOT_COMPLETE;
      return NULL;
    }

  ret = alloc_object ();
  ret->type = TYPE_STANDARD_OBJECT;

  so = malloc_and_check (sizeof (*so));
  so->class_name = class->value_ptr.standard_class->name;
  so->fields = NULL;
  ret->value_ptr.standard_object = so;

  create_object_fields (ret, class);

  return ret;
}


struct object *
builtin_class_of (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STANDARD_OBJECT)
    {
      increment_refcount (CAR (list)->value_ptr.standard_object->class_name->
			  value_ptr.symbol->typespec);
      return CAR (list)->value_ptr.standard_object->class_name->
	value_ptr.symbol->typespec;
    }
  else if (CAR (list)->type == TYPE_STRUCTURE)
    {
      increment_refcount (CAR (list)->value_ptr.structure->class_name->
			  value_ptr.symbol->typespec);
      return CAR (list)->value_ptr.structure->class_name->value_ptr.symbol->
	typespec;
    }
  else if (CAR (list)->type == TYPE_CONDITION)
    {
      increment_refcount (CAR (list)->value_ptr.condition->class_name->
			  value_ptr.symbol->typespec);
      return CAR (list)->value_ptr.condition->class_name->value_ptr.symbol->
	typespec;
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }
}


struct object *
builtin_class_name (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STANDARD_CLASS)
    {
      increment_refcount (CAR (list)->value_ptr.standard_class->name);
      return CAR (list)->value_ptr.standard_class->name;
    }
  else if (CAR (list)->type == TYPE_STRUCTURE_CLASS)
    {
      increment_refcount (CAR (list)->value_ptr.structure_class->name);
      return CAR (list)->value_ptr.structure_class->name;
    }
  else if (CAR (list)->type == TYPE_CONDITION_CLASS)
    {
      increment_refcount (CAR (list)->value_ptr.condition_class->name);
      return CAR (list)->value_ptr.condition_class->name;
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }
}


struct object *
builtin_slot_exists_p (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct object *req;
  struct class_field *f;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STANDARD_OBJECT || !IS_SYMBOL (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  f = CAR (list)->value_ptr.standard_object->fields;
  req = SYMBOL (CAR (CDR (list)));

  while (f)
    {
      if (f->name == req)
	{
	  return &t_object;
	}

      f = f->next;
    }

  return &nil_object;
}


struct object *
builtin_slot_boundp (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  struct object *req;
  struct class_field *f;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STANDARD_OBJECT || !IS_SYMBOL (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  f = CAR (list)->value_ptr.standard_object->fields;
  req = SYMBOL (CAR (CDR (list)));

  while (f)
    {
      if (f->name == req)
	{
	  if (f->value)
	    return &t_object;
	  else
	    return &nil_object;
	}

      f = f->next;
    }

  outcome->type = SLOT_NOT_FOUND;
  return NULL;
}


struct object *
builtin_slot_value (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct class_field *f;
  struct object *req;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STANDARD_OBJECT || !IS_SYMBOL (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  f = CAR (list)->value_ptr.standard_object->fields;
  req = SYMBOL (CAR (CDR (list)));

  while (f)
    {
      if (f->name == req)
	{
	  if (!f->value)
	    {
	      outcome->type = SLOT_NOT_BOUND;
	      return NULL;
	    }

	  increment_refcount (f->value);
	  return f->value;
	}

      f = f->next;
    }

  outcome->type = SLOT_NOT_FOUND;
  return NULL;
}


struct object *
builtin_slot_makunbound (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  struct class_field *f;
  struct object *req;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_STANDARD_OBJECT || !IS_SYMBOL (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  f = CAR (list)->value_ptr.standard_object->fields;
  req = SYMBOL (CAR (CDR (list)));

  while (f)
    {
      if (f->name == req)
	{
	  decrement_refcount (f->value);
	  f->value = NULL;

	  increment_refcount (CAR (list));
	  return CAR (list);
	}

      f = f->next;
    }

  outcome->type = SLOT_NOT_FOUND;
  return NULL;
}


struct object *
evaluate_defgeneric (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  struct object *sym, *fun;

  if (list_length (list) < 2)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)) || !IS_LIST (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  sym = SYMBOL (CAR (list));

  if (sym->value_ptr.symbol->function_cell
      && (sym->value_ptr.symbol->function_cell->type == TYPE_MACRO
	  || !sym->value_ptr.symbol->function_cell->value_ptr.function->is_generic))
    {
      outcome->type = CANT_REDEFINE_AS_GENERIC_FUNCTION;
      return NULL;
    }

  fun = create_function (CAR (CDR (list)), &nil_object, env, outcome, 0, 0);

  if (!fun)
    return NULL;

  fun->value_ptr.function->is_generic = 1;

  if (sym->value_ptr.symbol->function_cell)
    {
      delete_reference (sym, sym->value_ptr.symbol->function_cell, 1);
    }

  sym->value_ptr.symbol->function_cell = fun;
  add_reference (sym, fun, 1);

  fun->value_ptr.function->name = sym;
  add_reference (fun, sym, 0);

  return fun;
}


struct object *
evaluate_defmethod (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct object *fun, *meth;
  struct method *m;
  struct method_list *ml;

  if (list_length (list) < 2)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)) || !IS_LIST (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  fun = get_function (SYMBOL (CAR (list)), env, 0, 0, 1, 0);

  if (fun && (fun->type == TYPE_MACRO || !fun->value_ptr.function->is_generic))
    {
      outcome->type = CANT_REDEFINE_AS_GENERIC_FUNCTION;
      return NULL;
    }

  meth = alloc_object ();
  meth->type = TYPE_METHOD;
  m = meth->value_ptr.method = malloc_and_check (sizeof (*m));
  m->generic_func = NULL;
  m->body = NULL;

  outcome->type = EVAL_OK;
  m->lambda_list = NULL;
  m->lambda_list = parse_lambda_list (CAR (CDR (list)), 0, 1, env, outcome,
				      &m->allow_other_keys);

  if (outcome->type != EVAL_OK)
    {
      free_method (meth);

      return NULL;
    }

  if (!fun)
    {
      fun = create_function (&nil_object, &nil_object, env, outcome, 0, 0);

      if (!fun)
	return NULL;

      fun->value_ptr.function->is_generic = 1;

      SYMBOL (CAR (list))->value_ptr.symbol->function_cell = fun;
      add_reference (SYMBOL (CAR (list)), fun, 1);

      fun->value_ptr.function->name = SYMBOL (CAR (list));
      add_reference (fun, SYMBOL (CAR (list)), 0);
    }
  else if (!are_lambda_lists_congruent (m->lambda_list,
					fun->value_ptr.function->lambda_list))
    {
      free_method (meth);
      outcome->type = LAMBDA_LISTS_NOT_CONGRUENT;
      return NULL;
    }

  m->body = CDR (CDR (list));
  add_reference (meth, CDR (CDR (list)), 1);

  m->generic_func = fun;
  add_reference (meth, fun, 0);

  ml = malloc_and_check (sizeof (*ml));
  ml->reference_strength_factor = 1;
  ml->meth = meth;
  ml->next = fun->value_ptr.function->methods;

  fun->value_ptr.function->methods = ml;
  INC_WEAK_REFCOUNT (meth);

  return meth;
}


struct object *
builtin_next_method_p (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  if (SYMBOL (list) != &nil_object)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (env->next_methods)
    return &t_object;

  return &nil_object;
}


struct object *
evaluate_call_next_method (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  struct object *args, *prevargs, *ret;
  struct method_list *nextmeds;
  struct binding *bins;
  int argsnum, prev_lex_bin_num = env->lex_env_vars_boundary;


  if (!env->next_methods)
    {
      outcome->type = NO_NEXT_METHOD;
      return NULL;
    }

  if (SYMBOL (list) == &nil_object)
    {
      args = env->method_args;
    }
  else
    {
      args = list;
    }


  if (parse_argument_list (args, env->next_methods->meth->value_ptr.method->
			   lambda_list, 0, 0, 0, 0, env, outcome, &bins,
			   &argsnum))
    {
      prevargs = env->method_args;
      env->method_args = args;

      nextmeds = env->next_methods;
      env->next_methods = env->next_methods->next;

      env->vars = chain_bindings (bins, env->vars, NULL, NULL);
      env->lex_env_vars_boundary += argsnum;

      ret = evaluate_body (nextmeds->meth->value_ptr.method->body, 0, NULL, env,
			   outcome);

      env->vars = remove_bindings (env->vars, argsnum);
      env->lex_env_vars_boundary = prev_lex_bin_num;

      env->method_args = prevargs;
      env->next_methods = nextmeds;
    }
  else
    {
      ret = NULL;
    }

  return ret;
}


struct object *
evaluate_no_next_method (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  outcome->type = NO_NEXT_METHOD;
  return NULL;
}


struct object *
evaluate_declaim (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  while (SYMBOL (list) != &nil_object)
    {
      if (!parse_declaration_specifier (CAR (list), 0, env, -1, outcome))
	return NULL;

      list = CDR (list);
    }

  return &t_object;
}


struct object *
builtin_proclaim (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!parse_declaration_specifier (CAR (list), 0, env, -1, outcome))
    return NULL;

  return &t_object;
}


struct object *
evaluate_tagbody (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *ret = evaluate_body (list, 1, NULL, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!ret)
    return NULL;

  decrement_refcount (ret);

  return &nil_object;
}


struct object *
evaluate_go (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type != TYPE_INTEGER && !IS_SYMBOL (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  outcome->tag_to_jump_to = CAR (list);

  return NULL;
}


struct object *
evaluate_block (struct object *list, struct environment *env,
		struct outcome *outcome)
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
		      struct outcome *outcome)
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
  outcome->return_no_value = outcome->no_value;
  outcome->return_other_values = outcome->other_values;
  outcome->no_value = 0;
  outcome->other_values = 0;

  return NULL;
}


struct object *
evaluate_catch (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *tag, *ret;

  if (!list_length (list))
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  tag = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!tag)
    return NULL;

  env->catches = add_block (tag, env->catches);

  ret = evaluate_body (CDR (list), 0, NULL, env, outcome);

  if (!ret && outcome->object_to_catch
      && eq_objects (tag, outcome->object_to_catch) == &t_object)
    {
      decrement_refcount (outcome->object_to_catch);
      outcome->object_to_catch = NULL;

      ret = outcome->return_value;

      outcome->no_value = outcome->return_no_value;
      outcome->other_values = outcome->return_other_values;
      outcome->return_no_value = 0;
      outcome->return_other_values = NULL;
    }

  decrement_refcount (tag);
  env->catches = remove_block (env->catches);

  return ret;
}


struct object *
evaluate_throw (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *tag, *ret;
  struct object_list *l = env->catches;

  if (list_length (list) != 2)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  tag = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!tag)
    return NULL;

  while (l)
    {
      if (eq_objects (tag, l->obj) == &t_object)
	break;

      l = l->next;
    }

  if (!l)
    {
      decrement_refcount (tag);
      outcome->type = CATCH_NOT_FOUND;
      return NULL;
    }

  ret = evaluate_object (CAR (CDR (list)), env, outcome);

  if (!ret)
    {
      decrement_refcount (tag);
      return NULL;
    }

  outcome->object_to_catch = tag;
  outcome->return_value = ret;
  outcome->return_no_value = outcome->no_value;
  outcome->return_other_values = outcome->other_values;
  outcome->no_value = 0;
  outcome->other_values = NULL;

  return NULL;
}


struct object *
evaluate_handler_bind (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  int handlers = 0;
  struct object *cons, *ret = NULL, *res, *hret;
  struct handler_binding *b, *prev, *first = NULL;
  struct handler_binding_frame *f;
  struct object arg;
  struct cons_pair c;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (!IS_LIST (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  cons = CAR (list);

  while (cons->type == TYPE_CONS_PAIR)
    {
      if (CAR (cons)->type != TYPE_CONS_PAIR || list_length (CAR (cons)) != 2
	  || !IS_SYMBOL (CAR (CAR (cons))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  goto cleanup_and_leave;
	}

      res = evaluate_object (CAR (CDR (CAR (cons))), env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!res)
	goto cleanup_and_leave;

      if (res->type != TYPE_FUNCTION)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  goto cleanup_and_leave;
	}

      b = malloc_and_check (sizeof (*b));
      b->condition = SYMBOL (CAR (CAR (cons)));
      b->handler = res;
      b->next = NULL;

      if (!first)
	{
	  first = prev = b;
	}
      else
	{
	  prev = prev->next = b;
	}

      handlers++;

      cons = CDR (cons);
    }

  if (first)
    {
      f = malloc_and_check (sizeof (*f));
      f->frame = first;
      f->next = env->handlers;
      env->handlers = f;
    }

  ret = evaluate_body (CDR (list), 0, NULL, env, outcome);

  if (!ret && first)
    {
      b = first;

      arg.type = TYPE_CONS_PAIR;
      arg.value_ptr.cons_pair = &c;
      arg.refcount1 = 0;
      arg.refcount2 = 1;
      c.car = &nil_object;
      c.cdr = &nil_object;

      while (b)
	{
	  if (does_condition_include_outcome_type (b->condition, outcome->type,
						   env))
	    {
	      hret = call_function (b->handler, &arg, 1, 0, 1, 0, 0, env, outcome);

	      if (!hret)
		goto cleanup_and_leave;

	      decrement_refcount (hret);
	    }

	  b = b->next;
	}
    }

 cleanup_and_leave:
  if (first)
    {
      for (; handlers; handlers--)
	{
	  b = first->next;
	  decrement_refcount (first->handler);
	  free (first);
	  first = b;
	}

      env->handlers = f->next;
      free (f);
    }

  return ret;
}


struct object *
evaluate_restart_bind (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  int restarts = 0;
  struct object *cons, *res, *ret;
  struct restart_binding *b, *prev = NULL, *first = NULL;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (!IS_LIST (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  cons = CAR (list);

  while (cons->type == TYPE_CONS_PAIR)
    {
      if (CAR (cons)->type != TYPE_CONS_PAIR || list_length (CAR (cons)) != 2
	  || !IS_SYMBOL (CAR (CAR (cons))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  goto cleanup_and_leave;
	}

      res = evaluate_object (CAR (CDR (CAR (cons))), env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!res)
	goto cleanup_and_leave;

      if (res->type != TYPE_FUNCTION)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  goto cleanup_and_leave;
	}

      b = malloc_and_check (sizeof (*b));
      b->name = SYMBOL (CAR (CAR (cons)));
      b->restart = res;
      b->next = NULL;

      if (!first)
	{
	  prev = env->restarts;
	  first = b;
	}

      if (env->restarts)
	env->restarts->next = b;

      env->restarts = b;

      restarts++;

      cons = CDR (cons);
    }

  ret = evaluate_body (CDR (list), 0, NULL, env, outcome);

 cleanup_and_leave:
  for (; restarts; restarts--)
    {
      b = first->next;
      decrement_refcount (first->restart);
      free (first);
      first = b;
    }

  env->restarts = prev;

  return ret;
}


struct object *
builtin_invoke_restart (struct object *list, struct environment *env,
			struct outcome *outcome)
{
  struct restart_binding *b = env->restarts;
  struct object *fun;

  if (!list_length (list))
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (IS_SYMBOL (CAR (list)))
    {
      while (b)
	{
	  if (SYMBOL (CAR (list)) == b->name)
	    {
	      fun = b->restart;
	      break;
	    }

	  b = b->next;
	}
    }
  else if (CAR (list)->type == TYPE_FUNCTION)
    {
      while (b)
	{
	  if (CAR (list) == b->restart)
	    {
	      fun = b->restart;
	      break;
	    }

	  b = b->next;
	}
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (!b)
    {
      outcome->type = RESTART_NOT_FOUND;
      return NULL;
    }

  return call_function (fun, CDR (list), 1, 0, 1, 0, 0, env, outcome);
}


struct object *
evaluate_unwind_protect (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  struct object *res, *clres;
  struct object_list *ov;
  int nov;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  res = evaluate_object (CAR (list), env, outcome);
  nov = outcome->no_value;
  ov = outcome->other_values;
  outcome->other_values = NULL;

  clres = evaluate_body (CDR (list), 0, NULL, env, outcome);

  if (clres)
    {
      decrement_refcount (clres);
      outcome->no_value = nov;
      outcome->other_values = ov;

      return res;
    }

  return NULL;
}


struct object *
builtin_signal (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *cond, *ret;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      cond = create_condition_by_c_string ("SIMPLE-CONDITION", env, CAR (list),
					   CDR (list), (struct object *)NULL);
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      if (SYMBOL (CAR (list))->value_ptr.symbol->typespec->type
	  != TYPE_CONDITION_CLASS)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      cond =
	create_condition_by_class (SYMBOL (CAR (list))->value_ptr.symbol->typespec,
				   SYMBOL (CDR (list)) == &nil_object ? &nil_object
				   : CAR (CDR (list)),
				   SYMBOL (CDR (list)) == &nil_object ? &nil_object
				   : CDR (CDR (list)), (struct object *)NULL);
    }
  else if (CAR (list)->type == TYPE_CONDITION)
    {
      cond = CAR (list);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }


  ret = handle_condition (cond, env, outcome);

  if (CAR (list)->type != TYPE_CONDITION)
    decrement_refcount (cond);

  if (!ret)
    return NULL;

  return &nil_object;
}


struct object *
builtin_error (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  struct object *cond, *ret;

  if (!list_length (list))
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      cond = create_condition_by_c_string ("SIMPLE-ERROR", env, CAR (list),
					   CDR (list), (struct object *)NULL);
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      if (SYMBOL (CAR (list))->value_ptr.symbol->typespec->type
	  != TYPE_CONDITION_CLASS)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      cond =
	create_condition_by_class (SYMBOL (CAR (list))->value_ptr.symbol->typespec,
				   SYMBOL (CDR (list)) == &nil_object ? &nil_object
				   : CAR (CDR (list)),
				   SYMBOL (CDR (list)) == &nil_object ? &nil_object
				   : CDR (CDR (list)), (struct object *)NULL);
    }
  else if (CAR (list)->type == TYPE_CONDITION)
    {
      cond = CAR (list);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }


  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      if (CAR (list)->type != TYPE_CONDITION)
	decrement_refcount (cond);

      return NULL;
    }

  outcome->condition = cond;
  return NULL;
}


struct object *
evaluate_define_condition (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  struct object *condcl, *cons;
  struct condition_class *cc;
  struct condition_field_decl *f, *prev;
  struct object_list *p;

  if (list_length (list) < 3)
    {
      outcome->type = TOO_FEW_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)) || !IS_LIST (CAR (CDR (list)))
      || !IS_LIST (CAR (CDR (CDR (list)))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  condcl = alloc_object ();
  condcl->type = TYPE_CONDITION_CLASS;

  cc = malloc_and_check (sizeof (*cc));
  condcl->value_ptr.condition_class = cc;

  increment_refcount (SYMBOL (CAR (list)));
  cc->name = SYMBOL (CAR (list));


  cc->parents = NULL;
  cons = CAR (CDR (list));

  while (SYMBOL (cons) != &nil_object)
    {
      if (!IS_SYMBOL (CAR (cons))
	  || !SYMBOL (CAR (cons))->value_ptr.symbol->is_type
	  || !is_subtype_by_char_vector (SYMBOL (CAR (cons)), "CONDITION", env))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (cc->parents)
	p = p->next = malloc_and_check (sizeof (*p));
      else
	cc->parents = p = malloc_and_check (sizeof (*p));

      p->obj = SYMBOL (CAR (cons));

      cons = CDR (cons);
    }

  if (cc->parents)
    {
      p->next = NULL;
    }
  else
    {
      cc->parents = malloc_and_check (sizeof (*cc->parents));
      cc->parents->obj = CREATE_BUILTIN_SYMBOL ("CONDITION");
      cc->parents->next = NULL;
    }

  cc->fields = NULL;
  cons = CAR (CDR (CDR (list)));

  while (SYMBOL (cons) != &nil_object)
    {
      f = create_condition_field_decl (CAR (cons), env, outcome);

      if (!f)
	return NULL;

      if (cc->fields)
	prev = prev->next = f;
      else
	cc->fields = prev = f;

      cons = CDR (cons);
    }

  SYMBOL (CAR (list))->value_ptr.symbol->is_type = 1;

  delete_reference (SYMBOL (CAR (list)),
		    SYMBOL (CAR (list))->value_ptr.symbol->typespec, 4);
  SYMBOL (CAR (list))->value_ptr.symbol->typespec = condcl;
  add_reference (SYMBOL (CAR (list)), condcl, 4);
  decrement_refcount (condcl);

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_make_condition (struct object *list, struct environment *env,
			struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (!IS_SYMBOL (CAR (list)) || !SYMBOL (CAR (list))->value_ptr.symbol->is_type
      || !is_subtype_by_char_vector (SYMBOL (CAR (list)), "CONDITION", env))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return create_condition_by_class (SYMBOL (CAR (list))->value_ptr.symbol->typespec,
				    (struct object *)NULL);
}


struct object *
builtin_al_print_no_warranty (struct object *list, struct environment *env,
			      struct outcome *outcome)
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
				       struct outcome *outcome)
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


struct object *
builtin_al_exit (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  int val, l = list_length (list);

  if (l > 1)
    {
      outcome->type = TOO_MANY_ARGUMENTS;
      return NULL;
    }

  if (l && CAR (list)->type != TYPE_INTEGER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (l)
    val = mpz_get_si (CAR (list)->value_ptr.integer);
  else
    val = 0;

  exit (val);
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
  fixnum i;

  if (s1->used_size != s2->used_size)
    return 0;

  for (i = 0; i < s1->used_size; i++)
    if (s1->value [i] != s2->value [i])
      return 0;

  return 1;
}


int
equalp_strings (const struct string *s1, const struct string *s2)
{
  fixnum i;
  int single_byte = 1;

  if (s1->used_size != s2->used_size)
    return 0;

  for (i = 0; i < s1->used_size; i++)
    {
      if (IS_LOWEST_BYTE_IN_UTF8 (s1->value [i]) && single_byte)
	{
	  if (toupper (s1->value [i]) != toupper (s2->value [i]))
	    return 0;

	  single_byte = 1;
	}
      else
	{
	  if (s1->value [i] != s2->value [i])
	    return 0;

	  single_byte = 0;
	}
    }

  return 1;
}


struct object *
eq_objects (const struct object *obj1, const struct object *obj2)
{
  if (IS_SYMBOL (obj1))
    obj1 = SYMBOL (obj1);

  if (IS_SYMBOL (obj2))
    obj2 = SYMBOL (obj2);

  if (obj1 == obj2)
    return &t_object;

  return &nil_object;
}


struct object *
eql_objects (struct object *obj1, struct object *obj2)
{
  if (IS_SYMBOL (obj1))
    obj1 = SYMBOL (obj1);

  if (IS_SYMBOL (obj2))
    obj2 = SYMBOL (obj2);

  if (obj1 == obj2)
    return &t_object;

  if ((IS_RATIONAL (obj1) && IS_RATIONAL (obj2))
      || (obj1->type == TYPE_FLOAT && obj2->type == TYPE_FLOAT))
    {
      if (!compare_two_numbers (obj1, obj2))
	return &t_object;
      else
	return &nil_object;
    }

  if (obj1->type == TYPE_CHARACTER && obj2->type == TYPE_CHARACTER)
    {
      if (!strcmp (obj1->value_ptr.character, obj2->value_ptr.character))
	return &t_object;
    }

  return &nil_object;
}


struct object *
equal_objects (struct object *obj1, struct object *obj2)
{
  if (eql_objects (obj1, obj2) == &t_object)
    return &t_object;

  if (obj1->type == TYPE_STRING && obj2->type == TYPE_STRING)
    {
      if (equal_strings (obj1->value_ptr.string, obj2->value_ptr.string))
	return &t_object;

      return &nil_object;
    }

  if (obj1->type == TYPE_CONS_PAIR && obj2->type == TYPE_CONS_PAIR)
    {
      if (equal_objects (CAR (obj1), CAR (obj2)) == &t_object
	  && equal_objects (CDR (obj1), CDR (obj2)) == &t_object)
	{
	  return &t_object;
	}

      return &nil_object;
    }

  return &nil_object;
}


struct object *
equalp_objects (struct object *obj1, struct object *obj2)
{
  int i;

  if (IS_NUMBER (obj1) && IS_NUMBER (obj2))
    {
      if (!compare_two_numbers (obj1, obj2))
	return &t_object;
      else
	return &nil_object;
    }

  if (obj1->type == TYPE_CHARACTER && obj2->type == TYPE_CHARACTER)
    {
      if (strlen (obj1->value_ptr.character) != strlen (obj2->value_ptr.character))
	return &t_object;

      if (strlen (obj1->value_ptr.character) == 1)
	{
	  if (toupper (*obj1->value_ptr.character)
	      == toupper (*obj2->value_ptr.character))
	    return &t_object;

	  return &nil_object;
	}

      if (!strcmp (obj1->value_ptr.character, obj2->value_ptr.character))
	return &t_object;

      return &nil_object;
    }

  if (obj1->type == TYPE_CONS_PAIR && obj2->type == TYPE_CONS_PAIR)
    {
      if (equalp_objects (CAR (obj1), CAR (obj2)) == &t_object
	  && equalp_objects (CDR (obj1), CDR (obj2)) == &t_object)
	{
	  return &t_object;
	}

      return &nil_object;
    }

  if (obj1->type == TYPE_STRING && obj2->type == TYPE_STRING)
    {
      if (equalp_strings (obj1->value_ptr.string, obj2->value_ptr.string))
	return &t_object;

      return &nil_object;
    }

  if (obj1->type == TYPE_ARRAY && obj2->type == TYPE_ARRAY)
    {
      if (!arrays_have_equal_size (obj1->value_ptr.array, obj2->value_ptr.array))
	return &nil_object;

      for (i = 0; i < array_total_size (obj1->value_ptr.array->alloc_size); i++)
	{
	  if (equalp_objects (obj1->value_ptr.array->value [i],
			      obj2->value_ptr.array->value [i]) == &nil_object)
	    return &nil_object;
	}

      return &t_object;
    }

  return &nil_object;
}


int
arrays_have_equal_size (struct array *a1, struct array *a2)
{
  struct array_size *s1 = a1->alloc_size, *s2 = a2->alloc_size;

  while (s1)
    {
      if (!s2 || s1->size != s2->size)
	return 0;

      s1 = s1->next;
      s2 = s2->next;
    }

  if (s2)
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
  size_t i, sz, emp;
  char need_multiple_escape [] = "().,;'#\"\n";
  int do_need_multiple_escape = 0;
  enum object_type t;
  const char *ne, *te;

  sz = len;

  if (is_number (sym, len, 10, &t, &ne, &emp, &te))
    {
      do_need_multiple_escape = 1;
      sz += 2;
    }
  else
    {
      for (i = 0; print_escapes && i < len; i++)
	{
	  if ((strchr (need_multiple_escape, sym [i]) || !sym [i]
	       || isspace ((unsigned char)sym [i])
	       || islower ((unsigned char)sym [i]))
	      && !do_need_multiple_escape)
	    {
	      do_need_multiple_escape = 1;

	      if (str->medium == FILE_STREAM)
		break;

	      sz += 2;
	    }

	  if (sym [i] == '|' || sym [i] == '\\')
	    sz++;
	}
    }

  if (str->medium == STRING_STREAM)
    resize_string_allocation (str->string,
			      str->string->value_ptr.string->used_size + sz);

  if (do_need_multiple_escape && write_to_stream (str, "|", 1) < 0)
    return -1;

  for (i = 0; i < len; i++)
    {
      if (print_escapes && (sym [i] == '|' || sym [i] == '\\')
	  && write_to_stream (str, "\\", 1) < 0)
	return -1;

      if (write_to_stream (str, &sym [i], 1) < 0)
	return -1;
    }

  if (do_need_multiple_escape && write_to_stream (str, "|", 1) < 0)
    return -1;

  return 0;
}


int
print_symbol_name (const struct symbol_name *sym, struct environment *env,
		   struct stream *str)
{
  int pesc = is_printer_escaping_enabled (env);

  if (sym->sym->value_ptr.symbol->home_package)
    return print_symbol (sym->sym, env, str);
  else
    {
      if (pesc && write_to_stream (str, "#:", 2) < 0)
	return -1;

      return print_as_symbol (sym->value, sym->used_size, 1, str);
    }
}


int
print_symbol (const struct object *sym, struct environment *env,
	      struct stream *str)
{
  int pesc = is_printer_escaping_enabled (env), ispres;
  struct object *pack = inspect_variable (env->package_sym, env),
    *home = sym->value_ptr.symbol->home_package;

  if (home == &nil_object)
    {
      if (pesc && write_to_stream (str, "#:", 2) < 0)
	return -1;
    }
  else if (home == env->keyword_package && write_to_stream (str, ":", 1) < 0)
    return -1;

  if (home != &nil_object && home != env->keyword_package
      && !inspect_accessible_symbol (sym, pack, &ispres))
    {
      print_as_symbol (home->value_ptr.package->name,
		       home->value_ptr.package->name_len, pesc, str);

      if (is_external_in_home_package (sym))
	write_to_stream (str, ":", 1);
      else
	write_to_stream (str, "::", 2);
    }

  return print_as_symbol (sym->value_ptr.symbol->name,
			  sym->value_ptr.symbol->name_len, pesc, str);
}


int
print_bignum (const mpz_t z, struct environment *env, struct stream *str)
{
  char *out;
  int ret, base = get_print_base (env);

  if (base == 8)
    gmp_asprintf (&out, "%Zo", z);
  else if (base == 16)
    gmp_asprintf (&out, "%Zx", z);
  else
    gmp_asprintf (&out, "%Zd", z);

  ret = write_to_stream (str, out, strlen (out));
  free (out);
  return ret;
}


int
print_floating (const double f, struct environment *env, struct stream *str)
{
  char *out;
  int l;
  mpf_t fl;

  mpf_init (fl);
  mpf_set_d (fl, f);
  l = gmp_asprintf (&out, "%.Ff", fl);
  mpf_clear (fl);

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
print_as_string (const char *value, size_t sz, struct environment *env,
		 struct stream *str)
{
  fixnum i;
  int pesc = is_printer_escaping_enabled (env);

  if (pesc && write_to_stream (str, "\"", 1) < 0)
    return -1;

  for (i = 0; i < sz; i++)
    {
      if (pesc && (value [i] == '"' || value [i] == '\\')
	  && write_to_stream (str, "\\", 1) < 0)
	return -1;

      if (write_to_stream (str, &value [i], 1) < 0)
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

  return print_as_string (fn->value->value_ptr.string->value,
			  fn->value->value_ptr.string->used_size, env, str);
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
  struct object *parr = inspect_variable (env->print_array_sym, env),
    *pread = inspect_variable (env->print_readably_sym, env);
  fixnum rk = array_rank (array->alloc_size), i,
    totsize = array_total_size (array->alloc_size);
  struct array_size *s;
  int print_space;

  if (SYMBOL (parr) != &nil_object || SYMBOL (pread) != &nil_object)
    {
      if (write_to_stream (str, "#", 1) < 0
	  || (rk != 1 && (write_long_to_stream (str, rk) < 0
			  || write_to_stream (str, "A", 1) < 0)))
	{
	  return -1;
	}

      for (i = 0; i < rk; i++)
	{
	  if (write_to_stream (str, "(", 1) < 0)
	    return -1;
	}

      for (i = 0; i < (array->fill_pointer >= 0 ? array->fill_pointer : totsize);
	   i++)
	{
	  if (i)
	    {
	      print_space = 0;

	      s = array->alloc_size;

	      while (s)
		{
		  if (!(i % array_total_size (s)))
		    {
		      if (write_to_stream (str, ")", 1) < 0)
			return -1;

		      print_space = 1;
		    }

		  s = s->next;
		}

	      if (print_space && write_to_stream (str, " ", 1) < 0)
		return -1;

	      s = array->alloc_size;
	      print_space = 1;

	      while (s)
		{
		  if (!(i % array_total_size (s)))
		    {
		      if (write_to_stream (str, "(", 1) < 0)
			return -1;

		      print_space = 0;
		    }

		  s = s->next;
		}

	      if (print_space && write_to_stream (str, " ", 1) < 0)
		return -1;
	    }

	  if (print_object (array->value [i], env, str) < 0)
	    return -1;
	}

      for (i = 0; i < rk; i++)
	{
	  if (write_to_stream (str, ")", 1) < 0)
	    return -1;
	}

      return 0;
    }
  else if (write_to_stream (str, "#<ARRAY, RANK ", strlen ("#<ARRAY, RANK ")) < 0
	   || write_long_to_stream (str, rk) < 0
	   || write_to_stream (str, ">", 1) < 0)
    return -1;

  return 0;
}


int
print_bitarray (const struct bitarray *array, struct environment *env,
		struct stream *str)
{
  fixnum rk = array_rank (array->alloc_size), i;

  if (rk == 1)
    {
      if (write_to_stream (str, "#*", 2) < 0)
	return -1;

      for (i = 0; i < (array->fill_pointer >= 0 ? array->fill_pointer :
		       array->alloc_size->size); i++)
	{
	  if (mpz_tstbit (array->value, i))
	    {
	      if (write_to_stream (str, "1", 1) < 0)
		return -1;
	    }
	  else
	    {
	      if (write_to_stream (str, "0", 1) < 0)
		return -1;
	    }
	}

      return 0;
    }
  else if (write_to_stream (str, "#<BIT ARRAY, RANK ",
			    strlen ("#<BIT ARRAY, RANK ")) < 0
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
      if (obj->value_ptr.function->is_generic)
	{
	  if (write_to_stream (str, "#<STANDARD-GENERIC-FUNCTION ",
			       strlen ("#<STANDARD-GENERIC-FUNCTION ")) < 0)
	    return -1;
	}
      else if (write_to_stream (str, "#<FUNCTION ", strlen ("#<FUNCTION ")) < 0)
	return -1;

      if (obj->value_ptr.function->builtin_form
	  && write_to_stream (str, "BUILTIN ", strlen ("BUILTIN ")) < 0)
	return -1;

      if (obj->value_ptr.function->is_setf_func)
	{
	  if (write_to_stream (str, "(SETF ",
			       strlen ("(SETF ")) < 0)
	    return -1;
	}

      if (obj->value_ptr.function->name)
	{
	  if (print_symbol (obj->value_ptr.function->name, env, str) < 0)
	    return -1;
	}
      else if (write_to_stream (str, "?", 1) < 0)
	return -1;

      if (obj->value_ptr.function->is_setf_func)
	return write_to_stream (str, ")>", 2);
      else
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
	  if (print_symbol (obj->value_ptr.macro->name, env, str) < 0)
	    return -1;
	}
      else if (write_to_stream (str, "?", 1) < 0)
	return -1;

      return write_to_stream (str, ">", 1);
    }
}


int
print_method (const struct object *obj, struct environment *env,
	      struct stream *str)
{
  if (write_to_stream (str, "#<STANDARD-METHOD ", strlen ("#<STANDARD-METHOD "))
      < 0
      || print_symbol (obj->value_ptr.method->generic_func->
		       value_ptr.function->name, env, str) < 0
      || write_to_stream (str, ">", 1))
      return -1;

  return 0;
}


int
print_object (const struct object *obj, struct environment *env,
	      struct stream *str)
{
  char *out;
  int ret, base;

  if (obj->type == TYPE_CONS_PAIR
      && obj->value_ptr.cons_pair->car == env->quote_sym
      && WAS_A_READER_MACRO (obj))
    {
      if (write_to_stream (str, "'", 1) < 0)
	return -1;

      return print_object (obj->value_ptr.cons_pair->cdr->value_ptr.cons_pair->
			   car, env, str);
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

      if (obj->type == TYPE_INTEGER)
	return print_bignum (obj->value_ptr.integer, env, str);
      else if (obj->type == TYPE_FIXNUM)
	return write_long_to_stream (str, *obj->value_ptr.fixnum);
      else if (obj->type == TYPE_RATIO)
	{
	  base = get_print_base (env);

	  if (base == 8)
	    gmp_asprintf (&out, "%Qo", obj->value_ptr.ratio);
	  else if (base == 16)
	    gmp_asprintf (&out, "%Qx", obj->value_ptr.ratio);
	  else
	    gmp_asprintf (&out, "%Qd", obj->value_ptr.ratio);

	  ret = write_to_stream (str, out, strlen (out));
	  free (out);
	  return ret;
	}
      else if (obj->type == TYPE_FLOAT)
	return print_floating (*obj->value_ptr.floating, env, str);
      else if (obj->type == TYPE_COMPLEX)
	return print_complex (obj->value_ptr.complex, env, str);
      else if (obj->type == TYPE_RANDOM_STATE)
	{
	  return write_to_stream (str, "#<RANDOM-STATE ?>",
				  strlen ("#<RANDOM-STATE ?>"));
	}
      else if (obj->type == TYPE_BYTESPEC)
	return print_bytespec (obj->value_ptr.bytespec, env, str);
      else if (obj->type == TYPE_STRING)
	return print_as_string (obj->value_ptr.string->value,
				obj->value_ptr.string->used_size, env, str);
      else if (obj->type == TYPE_CHARACTER)
	return print_character (obj->value_ptr.character, env, str);
      else if (obj->type == TYPE_FILENAME)
	return print_filename (obj->value_ptr.filename, env, str);
      else if (obj->type == TYPE_SYMBOL_NAME)
	return print_symbol_name (obj->value_ptr.symbol_name, env, str);
      else if (obj->type == TYPE_SYMBOL)
	return print_symbol (obj, env, str);
      else if (obj->type == TYPE_CONS_PAIR)
	return print_list (obj->value_ptr.cons_pair, env, str);
      else if (obj->type == TYPE_ARRAY)
	return print_array (obj->value_ptr.array, env, str);
      else if (obj->type == TYPE_BITARRAY)
	return print_bitarray (obj->value_ptr.bitarray, env, str);
      else if (obj->type == TYPE_HASHTABLE)
	{
	  if (write_to_stream (str, "#<HASH-TABLE ",
			       strlen ("#<HASH-TABLE ")) < 0)
	    {
	      return -1;
	    }

	  if (obj->value_ptr.hashtable->type == HT_EQ)
	    {
	      if (write_to_stream (str, "EQ ", strlen ("EQ ")) < 0)
		{
		  return -1;
		}
	    }
	  else if (obj->value_ptr.hashtable->type == HT_EQL)
	    {
	      if (write_to_stream (str, "EQL ", strlen ("EQL ")) < 0)
		{
		  return -1;
		}
	    }
	  else if (obj->value_ptr.hashtable->type == HT_EQUAL)
	    {
	      if (write_to_stream (str, "EQUAL ", strlen ("EQUAL ")) < 0)
		{
		  return -1;
		}
	    }
	  else
	    {
	      if (write_to_stream (str, "EQUALP ", strlen ("EQUALP ")) < 0)
		{
		  return -1;
		}
	    }

	  if (write_long_to_stream (str,
				    hash_table_count (obj->value_ptr.hashtable)) < 0
	      || write_to_stream (str, "/", 1) < 0
	      || write_long_to_stream (str, LISP_HASHTABLE_SIZE) < 0
	      || write_to_stream (str, ">", 1) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_FUNCTION || obj->type == TYPE_MACRO)
	return print_function_or_macro (obj, env, str);
      else if (obj->type == TYPE_METHOD)
	return print_method (obj, env, str);
      else if (obj->type == TYPE_PACKAGE)
	{
	  if (write_to_stream (str, "#<PACKAGE ", strlen ("#<PACKAGE ")) < 0
	      || print_as_string (obj->value_ptr.package->name,
				  obj->value_ptr.package->name_len, env, str) < 0
	      || write_to_stream (str, ">", 1) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_ENVIRONMENT)
	return write_to_stream (str, "#<ENVIRONMENT ?>",
				strlen ("#<ENVIRONMENT ?>"));
      else if (obj->type == TYPE_STREAM)
	return write_to_stream (str, "#<STREAM ?>", strlen ("#<STREAM ?>"));
      else if (obj->type == TYPE_STRUCTURE_CLASS)
	{
	  if (write_to_stream (str, "#<STRUCTURE CLASS ",
			       strlen ("#<STRUCTURE CLASS ")) < 0
	      || print_symbol (obj->value_ptr.structure_class->name, env, str)
	      || write_to_stream (str, ">", 1))
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_STRUCTURE)
	{
	  if (write_to_stream (str, "#<STRUCTURE OF CLASS ",
			       strlen ("#<STRUCTURE OF CLASS ")) < 0
	      || print_symbol (obj->value_ptr.structure->class_name, env, str)
	      || write_to_stream (str, ">", 1))
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_STANDARD_CLASS)
	{
	  if (write_to_stream (str, "#<STANDARD-CLASS ",
			       strlen ("#<STANDARD-CLASS ")) < 0
	      || print_symbol (obj->value_ptr.standard_class->name, env, str)
	      || write_to_stream (str, ">", 1))
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_STANDARD_OBJECT)
	{
	  if (write_to_stream (str, "#<", strlen ("#<")) < 0
	      || print_symbol (obj->value_ptr.standard_object->class_name, env,
			       str)
	      || write_to_stream (str, " OBJECT ...>", strlen (" OBJECT ...>")) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_CONDITION_CLASS)
	{
	  if (write_to_stream (str, "#<CONDITION-CLASS ",
			       strlen ("#<CONDITION-CLASS ")) < 0
	      || print_symbol (obj->value_ptr.condition_class->name, env, str)
	      || write_to_stream (str, ">", 1))
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_CONDITION)
	{
	  if (write_to_stream (str, "#<CONDITION OF CLASS ",
			       strlen ("#<CONDITION OF CLASS ")) < 0
	      || print_symbol (obj->value_ptr.condition->class_name, env, str)
	      || write_to_stream (str, ">", 1))
	    return -1;

	  return 0;
	}
      else
	return write_to_stream (str, "#<print not implemented>",
				strlen ("#<print not implemented>"));
    }
}


void
print_error (struct outcome *err, struct environment *env)
{
  struct object *std_out = inspect_variable (env->std_out_sym, env);

  fresh_line (std_out->value_ptr.stream);

  if (err->type == CLOSING_PARENTHESIS)
    {
      printf ("read error: mismatched closing parenthesis\n");
    }
  else if (err->type == CLOSING_PARENTHESIS_AFTER_PREFIX)
    {
      printf ("read error: closing parenthesis can't immediately follow "
	      "ticks, backticks, commas, dots and ats\n");
    }
  else if (err->type == INVALID_SHARP_DISPATCH)
    {
      printf ("read error: invalid character used as a sharp dispatch\n");
    }
  else if (err->type == UNKNOWN_SHARP_DISPATCH)
    {
      printf ("read error: character not known as a sharp dispatch\n");
    }
  else if (err->type == WRONG_OBJECT_TYPE_TO_SHARP_MACRO)
    {
      printf ("read error: wrong type of object as content of a sharp macro\n");
    }
  else if (err->type == UNKNOWN_CHARACTER_NAME)
    {
      printf ("read error: unknown character name\n");
    }
  else if (err->type == FUNCTION_NOT_FOUND_IN_READ)
    {
      printf ("read error: function not found\n");
    }
  else if (err->type == WRONG_SYNTAX_IN_SHARP_MACRO_FOR_COMPLEX)
    {
      printf ("read error: wrong syntax in sharp macro for complex number\n");
    }
  else if (err->type == INVALID_FEATURE_TEST)
    {
      printf ("read error: invalid feature test\n");
    }
  else if (err->type == COMMA_WITHOUT_BACKQUOTE)
    {
      printf ("read error: comma can appear only inside a backquoted form\n");
    }
  else if (err->type == TOO_MANY_COMMAS)
    {
      printf ("read error: number of commas can't exceed number of pending "
	      "backquotes\n");
    }
  else if (err->type == SINGLE_DOT)
    {
      printf ("read error: single dot is only allowed inside a list and must "
	      "be followed by exactly one object\n");
    }
  else if (err->type == MULTIPLE_DOTS)
    {
      printf ("read error: symbol names made of non-escaped dots only are "
	      "not allowed\n");
    }
  else if (err->type == NO_OBJ_BEFORE_DOT_IN_LIST)
    {
      printf ("read error: no object before dot in list\n");
    }
  else if (err->type == NO_OBJ_AFTER_DOT_IN_LIST)
    {
      printf ("read error: no object follows dot in list\n");
    }
  else if (err->type == MULTIPLE_OBJS_AFTER_DOT_IN_LIST)
    {
      printf ("read error: more than one object follows dot in list\n");
    }
  else if (err->type == MORE_THAN_A_CONSING_DOT)
    {
      printf ("read error: more than one consing dot not allowed in list\n");
    }
  else if (err->type == TOO_MANY_COLONS)
    {
      printf ("read error: more than two consecutive colons cannot appear in a "
	      "token\n");
    }
  else if (err->type == CANT_BEGIN_WITH_TWO_COLONS_OR_MORE)
    {
      printf ("read error: a token can't begin with two colons or more\n");
    }
  else if (err->type == CANT_END_WITH_PACKAGE_SEPARATOR)
    {
      printf ("read error: a token can't end with a package separator\n");
    }
  else if (err->type == MORE_THAN_A_PACKAGE_SEPARATOR)
    {
      printf ("read error: more than a package separator not allowed in token\n");
    }
  else if (err->type == PACKAGE_NOT_FOUND_IN_READ)
    {
      printf ("read error: package ");
      print_as_symbol (err->obj->value_ptr.package->name,
		       err->obj->value_ptr.package->name_len, 1,
		       std_out->value_ptr.stream);
      printf (" not found\n");
    }
  else if (err->type == SYMBOL_IS_NOT_EXTERNAL_IN_PACKAGE)
    {
      printf ("read error: symbol ");
      print_as_symbol (err->obj->value_ptr.symbol_name->actual_symname,
		       err->obj->value_ptr.symbol_name->actual_symname_used_s, 1,
		       std_out->value_ptr.stream);
      printf (" is not external in package ");
      print_as_symbol (err->obj->value_ptr.symbol_name->value,
		       err->obj->value_ptr.symbol_name->used_size, 1,
		       std_out->value_ptr.stream);
      printf ("\n");
    }
  else if (err->type == PACKAGE_MARKER_IN_SHARP_COLON)
    {
      printf ("read error: a package marker can't appear in a sharp-colon "
	      "macro\n");
    }
  else if (err->type == GOT_EOF_IN_MIDDLE_OF_OBJECT)
    {
      printf ("read error: got end-of-file in the middle of an object\n");
    }
  else if (err->type == GOT_EOF)
    {
      printf ("read error: got end-of-file\n");
    }
  else if (err->type == UNBOUND_SYMBOL)
    {
      printf ("eval error: symbol ");
      print_object (err->obj, env, std_out->value_ptr.stream);
      printf (" not bound to any object\n");
    }
  else if (err->type == UNKNOWN_FUNCTION)
    {
      printf ("eval error: symbol ");
      print_object (err->obj, env, std_out->value_ptr.stream);
      printf (" not bound to any function, macro or special operator\n");
    }
  else if (err->type == INVALID_FUNCTION_CALL)
    {
      printf ("eval error: not a special, macro or function form\n");
    }
  else if (err->type == UNKNOWN_KEYWORD_ARGUMENT)
    {
      printf ("eval error: unknown keyword argument in function or macro "
	      "call\n");
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
      printf ("eval error: comma-at and comma-dot syntax are not allowed at "
	      "top-level of a quasiquote form\n");
    }
  else if (err->type == CANT_SPLICE_AN_ATOM_HERE)
    {
      printf ("eval error: splicing an atom is only allowed at last position of "
	      "a list\n");
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
  else if (err->type == MALFORMED_IF)
    {
      printf ("eval error: incorrect syntax in IF\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_LET)
    {
      printf ("eval error: incorrect syntax in LET or LET*\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_FLET)
    {
      printf ("eval error: incorrect syntax in FLET, LABELS or MACROLET\n");
    }
  else if (err->type == NOT_A_FUNCTION_NAME_OR_LAMBDA_IN_FUNCTION)
    {
      printf ("eval error: FUNCTION takes a function name or a lambda expression\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_DEFUN)
    {
      printf ("eval error: incorrect syntax in DEFUN\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_DEFMACRO)
    {
      printf ("eval error: incorrect syntax in DEFMACRO\n");
    }
  else if (err->type == INCORRECT_SYNTAX_IN_DEFTYPE)
    {
      printf ("eval error: incorrect syntax in DEFTYPE\n");
    }
  else if (err->type == INVALID_LAMBDA_LIST)
    {
      printf ("eval error: lambda list is invalid\n");
    }
  else if (err->type == CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST)
    {
      printf ("eval error: can't use name of constant in a lambda list\n");
    }
  else if (err->type == LAMBDA_LISTS_NOT_CONGRUENT)
    {
      printf ("eval error: lambda lists are not congruent\n");
    }
  else if (err->type == CANT_REDEFINE_SPECIAL_OPERATOR)
    {
      printf ("eval error: redefining special operators is not allowed\n");
    }
  else if (err->type == CANT_REDEFINE_AS_GENERIC_FUNCTION)
    {
      printf ("eval error: can't redefine ordinary function, macro or special "
	      "operator as generic function\n");
    }
  else if (err->type == CANT_REDEFINE_CONSTANT_TO_NONEQL_VALUE)
    {
      printf ("eval error: redefining constants to a non-eql value is not "
	      "allowed\n");
    }
  else if (err->type == CANT_REDEFINE_CONSTANT_BY_DIFFERENT_OPERATOR)
    {
      printf ("eval error: redefining constants by a different operator is not "
	      "allowed\n");
    }
  else if (err->type == CANT_MODIFY_CONSTANT)
    {
      printf ("eval error: modifying constants is not allowed\n");
    }
  else if (err->type == CANT_REBIND_CONSTANT)
    {
      printf ("eval error: rebinding constants is not allowed\n");
    }
  else if (err->type == CANT_REDEFINE_STANDARD_TYPE)
    {
      printf ("eval error: redefining standard types is not allowed\n");
    }
  else if (err->type == TOO_FEW_ARGUMENTS)
    {
      printf ("eval error: too few arguments to function or macro call\n");
    }
  else if (err->type == TOO_MANY_ARGUMENTS)
    {
      printf ("eval error: too many arguments to function or macro call\n");
    }
  else if (err->type == MISMATCH_IN_DESTRUCTURING_CALL)
    {
      printf ("eval error: found mismatch while destructuring argument list\n");
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
  else if (err->type == DECREASING_INTERVAL_NOT_MEANINGFUL)
    {
      printf ("eval error: a decreasing interval is not meaningful\n");
    }
  else if (err->type == INVALID_SIZE)
    {
      printf ("eval error: not a valid size\n");
    }
  else if (err->type == NO_APPLICABLE_METHOD)
    {
      printf ("eval error: no applicable method found\n");
    }
  else if (err->type == NO_NEXT_METHOD)
    {
      printf ("eval error: no next method available\n");
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
  else if (err->type == INVALID_TYPE_SPECIFIER)
    {
      printf ("eval error: invalid type specifier\n");
    }
  else if (err->type == UNKNOWN_TYPE)
    {
      printf ("eval error: type not known\n");
    }
  else if (err->type == CLASS_NOT_FOUND)
    {
      printf ("eval error: class not found\n");
    }
  else if (err->type == CLASS_DEFINITION_NOT_COMPLETE)
    {
      printf ("eval error: class definition is not complete\n");
    }
  else if (err->type == INVALID_GO_TAG)
    {
      printf ("eval error: not a valid go tag\n");
    }
  else if (err->type == TAG_NOT_FOUND)
    {
      printf ("eval error: go tag not found\n");
    }
  else if (err->type == BLOCK_NOT_FOUND)
    {
      printf ("eval error: block not found\n");
    }
  else if (err->type == CATCH_NOT_FOUND)
    {
      printf ("eval error: catch matching throw not found\n");
    }
  else if (err->type == INVALID_ACCESSOR)
    {
      printf ("eval error: not a valid accessor\n");
    }
  else if (err->type == NO_SETF_EXPANDER)
    {
      printf ("eval error: symbol has no associated setf expander\n");
    }
  else if (err->type == SETF_EXPANDER_PRODUCED_NOT_ENOUGH_VALUES)
    {
      printf ("eval error: setf expansion did not produce five values\n");
    }
  else if (err->type == INVALID_SETF_EXPANSION)
    {
      printf ("eval error: not a valid setf expansion\n");
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
  else if (err->type == WRONG_SYNTAX_IN_DECLARE)
    {
      printf ("eval error: wrong syntax in DECLARE\n");
    }
  else if (err->type == WRONG_SYNTAX_IN_DECLARATION)
    {
      printf ("eval error: wrong syntax in declaration\n");
    }
  else if (err->type == UNKNOWN_DECLARATION)
    {
      printf ("eval error: unknown declaration\n");
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
  else if (err->type == SYMBOL_NOT_ACCESSIBLE_IN_PACKAGE)
    {
      printf ("eval error: symbol is not accessible in package\n");
    }
  else if (err->type == SYMBOL_NOT_PRESENT_IN_PACKAGE)
    {
      printf ("eval error: symbol is not present in package\n");
    }
  else if (err->type == IMPORTING_SYMBOL_WOULD_CAUSE_CONFLICT)
    {
      printf ("eval error: importing symbol would cause conflict\n");
    }
  else if (err->type == EXPORTING_SYMBOL_WOULD_CAUSE_CONFLICT)
    {
      printf ("eval error: exporting symbol would cause conflict\n");
    }
  else if (err->type == KEYWORD_PACKAGE_CANT_USE_ANY_PACKAGE)
    {
      printf ("eval error: keyword package can't use any package\n");
    }
  else if (err->type == CANT_USE_KEYWORD_PACKAGE)
    {
      printf ("eval error: can't use keyword package\n");
    }
  else if (err->type == USING_PACKAGE_WOULD_CAUSE_CONFLICT_ON_SYMBOL)
    {
      printf ("eval error: using package ");
      print_as_symbol (err->pack->value_ptr.package->name,
		       err->pack->value_ptr.package->name_len, 1,
		       std_out->value_ptr.stream);
      printf (" would cause conflict on symbol ");
      print_symbol (err->obj, env, std_out->value_ptr.stream);
      printf ("\n");
    }
  else if (err->type == PACKAGE_IS_NOT_IN_USE)
    {
      printf ("eval error: package is not in use\n");
    }
  else if (err->type == SLOT_NOT_FOUND)
    {
      printf ("eval error: slot doesn't exist\n");
    }
  else if (err->type == SLOT_NOT_BOUND)
    {
      printf ("eval error: slot is not bound\n");
    }
  else if (err->type == RESTART_NOT_FOUND)
    {
      printf ("eval error: restart not found\n");
    }

  std_out->value_ptr.stream->dirty_line = 0;
}


void
mark_as_constant (struct object *obj)
{
  struct parameter *p;
  struct method_list *m;
  struct hashtable_record *r;
  int i;

  if (DONT_REFCOUNT (obj))
    return;

  SET_CONSTANT_FLAG (obj);

  if (obj->type == TYPE_SYMBOL_NAME)
    mark_as_constant (obj->value_ptr.symbol_name->sym);
  else if (obj->type == TYPE_SYMBOL)
    {
      mark_as_constant (obj->value_ptr.symbol->typespec);
      mark_as_constant (obj->value_ptr.symbol->value_cell);
      mark_as_constant (obj->value_ptr.symbol->function_cell);
      mark_as_constant (obj->value_ptr.symbol->plist);
      mark_as_constant (obj->value_ptr.symbol->setf_expander);
      mark_as_constant (obj->value_ptr.symbol->setf_func_cell);
    }
  else if (IS_PREFIX (obj->type))
    mark_as_constant (obj->value_ptr.next);
  else if (obj->type == TYPE_CONS_PAIR)
    {
      mark_as_constant (obj->value_ptr.cons_pair->car);
      mark_as_constant (obj->value_ptr.cons_pair->cdr);
    }
  else if (obj->type == TYPE_FILENAME)
    mark_as_constant (obj->value_ptr.filename->value);
  else if (obj->type == TYPE_ARRAY)
    {
      for (i = 0; i < array_total_size (obj->value_ptr.array->alloc_size); i++)
	{
	  mark_as_constant (obj->value_ptr.array->value [i]);
	}
    }
  else if (obj->type == TYPE_HASHTABLE)
    {
      for (i = 0; i < LISP_HASHTABLE_SIZE; i++)
	{
	  r = obj->value_ptr.hashtable->table [i];

	  while (r)
	    {
	      mark_as_constant (r->key);
	      mark_as_constant (r->value);
	    }
	}
    }
  else if (obj->type == TYPE_COMPLEX)
    {
      mark_as_constant (obj->value_ptr.complex->real);
      mark_as_constant (obj->value_ptr.complex->imag);
    }
  else if (obj->type == TYPE_FUNCTION || obj->type == TYPE_MACRO)
    {
      mark_as_constant (obj->value_ptr.function->name);

      p = obj->value_ptr.function->lambda_list;

      while (p)
	{
	  mark_as_constant (p->name);
	  mark_as_constant (p->key);
	  mark_as_constant (p->init_form);
	  mark_as_constant (p->supplied_p_param);
	  p = p->next;
	}

      mark_as_constant (obj->value_ptr.function->body);

      m = obj->value_ptr.function->methods;

      while (m)
	{
	  mark_as_constant (m->meth);
	  m = m->next;
	}
    }
  else if (obj->type == TYPE_METHOD)
    {
      mark_as_constant (obj->value_ptr.method->generic_func);

      p = obj->value_ptr.method->lambda_list;

      while (p)
	{
	  mark_as_constant (p->name);
	  mark_as_constant (p->key);
	  mark_as_constant (p->init_form);
	  mark_as_constant (p->supplied_p_param);
	  p = p->next;
	}

      mark_as_constant (obj->value_ptr.method->body);
    }
  else if (obj->type == TYPE_STREAM)
    {
      if (obj->value_ptr.stream->medium == STRING_STREAM)
	mark_as_constant (obj->value_ptr.stream->string);
    }
}


struct parameter *
parameter_by_index (struct parameter *par, int *ind)
{
  struct parameter *sp;

  while (par && *ind > 7)
    {
      if (!par->name)
	{
	  sp = parameter_by_index (par->sub_lambda_list, ind);

	  if (sp)
	    return sp;
	}
      else
	{
	  *ind -= 8;
	}

      par = par->next;
    }

  return par;
}


int
is_reference_weak (struct object *src, int ind, struct object *dest)
{
  struct parameter *par;
  struct hashtable_record *r;
  struct method_list *ml;

  if (src->type == TYPE_ARRAY)
    {
      return !src->value_ptr.array->reference_strength_factor [ind]
	!= !STRENGTH_FACTOR_OF_OBJECT (dest);
    }
  else if (src->type == TYPE_HASHTABLE)
    {
      r = src->value_ptr.hashtable->table [ind % LISP_HASHTABLE_SIZE];

      while (ind >= LISP_HASHTABLE_SIZE * 2)
	{
	  r = r->next;
	  ind -= LISP_HASHTABLE_SIZE * 2;
	}

      return !(r->reference_strength_factor & (1 << (ind / LISP_HASHTABLE_SIZE)))
	!= !STRENGTH_FACTOR_OF_OBJECT (dest);
    }
  else if (src->type == TYPE_FUNCTION || src->type == TYPE_MACRO)
    {
      if (ind <= 1)
	{
	  return !(src->flags & (0x1 << ind))
	    != !STRENGTH_FACTOR_OF_OBJECT (dest);
	}

      ind -= 2;

      if (ind % 8 == 4)
	{
	  ml = src->value_ptr.function->methods;

	  while (ind > 7)
	    {
	      ind -= 8;
	      ml = ml->next;
	    }

	  return !(ml->reference_strength_factor & (0x1 << ind))
	    != !STRENGTH_FACTOR_OF_OBJECT (dest);
	}

      par = parameter_by_index (src->value_ptr.function->lambda_list, &ind);

      return !(par->reference_strength_factor & (0x1 << ind))
	!= !STRENGTH_FACTOR_OF_OBJECT (dest);
    }
  else if (src->type == TYPE_METHOD)
    {
      if (ind <= 1)
	{
	  return !(src->flags & (0x1 << ind))
	    != !STRENGTH_FACTOR_OF_OBJECT (dest);
	}

      ind -= 2;
      par = src->value_ptr.method->lambda_list;

      while (ind > 3)
	{
	  ind -= 4;
	  par = par->next;
	}

      return !(par->reference_strength_factor & (0x1 << ind))
	!= !STRENGTH_FACTOR_OF_OBJECT (dest);
    }
  else
    {
      return !(src->flags & (0x1 << ind)) != !STRENGTH_FACTOR_OF_OBJECT (dest);
    }
}


void
set_reference_strength_factor (struct object *src, int ind, struct object *dest,
			       int new_weakness, int increase_refcount,
			       int decrease_other_refcount)
{
  struct parameter *par;
  struct hashtable_record *r;
  struct method_list *ml;

  if (src->type == TYPE_ARRAY)
    {
      src->value_ptr.array->reference_strength_factor [ind] =
	!new_weakness != !STRENGTH_FACTOR_OF_OBJECT (dest);
    }
  else if (src->type == TYPE_HASHTABLE)
    {
      r = src->value_ptr.hashtable->table [ind % LISP_HASHTABLE_SIZE];

      while (ind >= LISP_HASHTABLE_SIZE * 2)
	{
	  r = r->next;
	  ind -= LISP_HASHTABLE_SIZE * 2;
	}

      r->reference_strength_factor = (r->reference_strength_factor
				      & ~(1 << (ind / LISP_HASHTABLE_SIZE)))
	| ((!new_weakness != !STRENGTH_FACTOR_OF_OBJECT (dest))
	   << (1 << (ind / LISP_HASHTABLE_SIZE)));
    }
  else if (src->type == TYPE_FUNCTION || src->type == TYPE_MACRO)
    {
      if (ind <= 1)
	{
	  src->flags = (src->flags & ~(1 << ind))
	    | ((!new_weakness != !STRENGTH_FACTOR_OF_OBJECT (dest)) << ind);
	}
      else
	{
	  ind -= 2;

	  if (ind % 8 == 4)
	    {
	      ml = src->value_ptr.function->methods;

	      while (ind > 7)
		{
		  ind -= 8;
		  ml = ml->next;
		}

	      ml->reference_strength_factor =
		!new_weakness != !STRENGTH_FACTOR_OF_OBJECT (dest);
	    }
	  else
	    {
	      par = parameter_by_index (src->value_ptr.function->lambda_list,
					&ind);

	      par->reference_strength_factor = (par->reference_strength_factor
						& ~(1 << ind))
		| ((!new_weakness != !STRENGTH_FACTOR_OF_OBJECT (dest)) << ind);
	    }
	}
    }
  else if (src->type == TYPE_METHOD)
    {
      if (ind <= 1)
	{
	  src->flags = (src->flags & ~(1 << ind))
	    | ((!new_weakness != !STRENGTH_FACTOR_OF_OBJECT (dest)) << ind);
	}
      else
	{
	  ind -= 2;
	  par = src->value_ptr.method->lambda_list;

	  while (ind > 3)
	    {
	      ind -= 4;
	      par = par->next;
	    }

	  par->reference_strength_factor = (par->reference_strength_factor
					    & ~(1 << ind))
	    | ((!new_weakness != !STRENGTH_FACTOR_OF_OBJECT (dest)) << ind);
	}
    }
  else
    {
      src->flags = (src->flags & ~(1 << ind))
	| ((!new_weakness != !STRENGTH_FACTOR_OF_OBJECT (dest)) << ind);
    }

  if (new_weakness)
    {
      increase_refcount && INC_WEAK_REFCOUNT (dest);
      decrease_other_refcount && DEC_STRONG_REFCOUNT (dest);
    }
  else
    {
      increase_refcount && INC_STRONG_REFCOUNT (dest);
      decrease_other_refcount && DEC_WEAK_REFCOUNT (dest);
    }
}


void
add_strong_reference (struct object *src, struct object *dest, int ind)
{
  if (!DONT_REFCOUNT (dest))
    {
      set_reference_strength_factor (src, ind, dest, 0, 1, 0);
    }
}


void
add_reference (struct object *src, struct object *dest, int ind)
{
  if (!DONT_REFCOUNT (dest))
    {
      set_reference_strength_factor (src, ind, dest, 1, 1, 0);
    }
}


void
delete_reference (struct object *src, struct object *dest, int ind)
{
  int depth = 1;

  if (DONT_REFCOUNT (dest))
    return;

  if (src && is_reference_weak (src, ind, dest))
    {
      DEC_WEAK_REFCOUNT (dest);
    }
  else
    {
      DEC_STRONG_REFCOUNT (dest);

      if (!STRONG_REFCOUNT (dest) && !WEAK_REFCOUNT (dest))
	{
	  free_object (dest);
	}
      else if (!STRONG_REFCOUNT (dest) && WEAK_REFCOUNT (dest))
	{
	  weakify_loops (dest, &depth);

	  if (!STRONG_REFCOUNT (dest))
	    {
	      free_object (dest);
	    }
	}
    }
}


void
weakify_loops (struct object *root, int *depth)
{
  (*depth)++;
  FLIP_STRENGTH_FACTOR_OF_OBJECT (root);
  root->mark = 1;

  restore_invariants_at_node (root, root, depth);

  root->mark = 0;
  (*depth)--;
}


void
restore_invariants_at_edge (struct object *src, struct object *dest, int ind,
			    struct object *root, int *depth)
{
  if (DONT_REFCOUNT (dest))
    return;

  if (!is_reference_weak (src, ind, dest))
    {
      if (dest == root)
	{
	  set_reference_strength_factor (src, ind, dest, 1, 1, 1);
	}
      else if (!dest->mark)
	{
	  if (STRONG_REFCOUNT (dest) >= 2)
	    {
	      set_reference_strength_factor (src, ind, dest, 1, 1, 1);
	    }
	  else if (WEAK_REFCOUNT (dest))
	    {
	      weakify_loops (dest, depth);

	      if (!STRONG_REFCOUNT (dest))
		{
		  set_reference_strength_factor (src, ind, dest, 0, 1, 1);
		  dest->mark = 1;
		  restore_invariants_at_node (dest, root, depth);
		  dest->mark = 0;
		}
	    }
	  else
	    {
	      restore_invariants_at_node (dest, root, depth);
	    }
	}
      else if (dest->mark != *depth)
	{
	  dest->mark = *depth;
	  restore_invariants_at_node (dest, root, depth);
	  dest->mark = 1;
	}
    }
}


#define rest_inv_at_edge(next, ind)			\
  restore_invariants_at_edge (node, next, ind, root, depth)


void
restore_invariants_at_lambda_list (struct parameter *par, struct object *node,
				   size_t *i, struct object *root, int *depth)
{
  while (par)
    {
      if (!par->name)
	{
	  restore_invariants_at_lambda_list (par->sub_lambda_list, node, i, root,
					     depth);
	}
      else
	{
	  rest_inv_at_edge (par->name, (*i)++);
	  rest_inv_at_edge (par->init_form, (*i)++);
	  rest_inv_at_edge (par->supplied_p_param, (*i)++);
	  rest_inv_at_edge (par->key, (*i)++);

	  *i += 4;
	}

      par = par->next;
    }
}


void
restore_invariants_at_node (struct object *node, struct object *root, int *depth)
{
  size_t sz, i, j;
  struct parameter *par;
  struct hashtable_record *r;
  struct method_list *ml;

  if (IS_PREFIX (node->type))
    rest_inv_at_edge (node->value_ptr.next, 0);
  else if (node->type == TYPE_SYMBOL_NAME)
    rest_inv_at_edge (node->value_ptr.symbol_name->sym, 0);
  else if (node->type == TYPE_SYMBOL)
    {
      rest_inv_at_edge (node->value_ptr.symbol->value_cell, 0);
      rest_inv_at_edge (node->value_ptr.symbol->function_cell, 1);
      rest_inv_at_edge (node->value_ptr.symbol->setf_func_cell, 2);
      rest_inv_at_edge (node->value_ptr.symbol->setf_expander, 3);
      rest_inv_at_edge (node->value_ptr.symbol->typespec, 4);
      rest_inv_at_edge (node->value_ptr.symbol->plist, 5);
    }
  else if (node->type == TYPE_CONS_PAIR)
    {
      rest_inv_at_edge (node->value_ptr.cons_pair->car, 0);
      rest_inv_at_edge (node->value_ptr.cons_pair->cdr, 1);
    }
  else if (node->type == TYPE_COMPLEX)
    {
      rest_inv_at_edge (node->value_ptr.complex->real, 0);
      rest_inv_at_edge (node->value_ptr.complex->imag, 1);
    }
  else if (node->type == TYPE_ARRAY)
    {
      sz = array_total_size (node->value_ptr.array->alloc_size);

      for (i = 0; sz && i < sz; i++)
	{
	  rest_inv_at_edge (node->value_ptr.array->value [i], i);
	}
    }
  else if (node->type == TYPE_HASHTABLE)
    {
      for (i = 0; i < LISP_HASHTABLE_SIZE; i++)
	{
	  r = node->value_ptr.hashtable->table [i];
	  j = 0;

	  while (r)
	    {
	      rest_inv_at_edge (r->key, i+j*2*LISP_HASHTABLE_SIZE);
	      rest_inv_at_edge (r->value, i+(j*2+1)*LISP_HASHTABLE_SIZE);

	      r = r->next;
	      j++;
	    }
	}
    }
  else if (node->type == TYPE_FILENAME)
    {
      rest_inv_at_edge (node->value_ptr.filename->value, 0);
    }
  else if (node->type == TYPE_STREAM)
    {
      if (node->value_ptr.stream->medium == STRING_STREAM)
	{
	  rest_inv_at_edge (node->value_ptr.stream->string, 0);
	}
    }
  else if (node->type == TYPE_FUNCTION || node->type == TYPE_MACRO)
    {
      rest_inv_at_edge (node->value_ptr.function->name, 0);

      rest_inv_at_edge (node->value_ptr.function->body, 1);

      par = node->value_ptr.function->lambda_list;
      i = 2;

      restore_invariants_at_lambda_list (par, node, &i, root, depth);

      ml = node->value_ptr.function->methods;
      i = 6;

      while (ml)
	{
	  rest_inv_at_edge (ml->meth, i);
	  i += 8;
	  ml = ml->next;
	}
    }
  else if (node->type == TYPE_METHOD)
    {
      rest_inv_at_edge (node->value_ptr.method->generic_func, 0);

      rest_inv_at_edge (node->value_ptr.method->body, 1);

      par = node->value_ptr.method->lambda_list;
      i = 2;

      while (par)
	{
	  rest_inv_at_edge (par->name, i++);
	  rest_inv_at_edge (par->init_form, i++);
	  rest_inv_at_edge (par->supplied_p_param, i++);
	  rest_inv_at_edge (par->key, i++);

	  par = par->next;
	}
    }
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
  else if (obj->type == TYPE_SYMBOL)
    free_symbol (obj);
  else if (IS_PREFIX (obj->type))
    {
      delete_reference (obj, obj->value_ptr.next, 0);
      free (obj);
    }
  else if (obj->type == TYPE_CONS_PAIR)
    free_cons_pair (obj);
  else if (obj->type == TYPE_FILENAME)
    {
      delete_reference (obj, obj->value_ptr.filename->value, 0);
      free (obj->value_ptr.filename);
      free (obj);
    }
  else if (obj->type == TYPE_ARRAY)
    free_array (obj);
  else if (obj->type == TYPE_BITARRAY)
    free_bitarray (obj);
  else if (obj->type == TYPE_HASHTABLE)
    free_hashtable (obj);
  else if (obj->type == TYPE_CHARACTER)
    {
      free (obj->value_ptr.character);
      free (obj);
    }
  else if (obj->type == TYPE_INTEGER)
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
      delete_reference (obj, obj->value_ptr.complex->real, 0);
      delete_reference (obj, obj->value_ptr.complex->imag, 1);

      free (obj->value_ptr.complex);
      free (obj);
    }
  else if (obj->type == TYPE_RANDOM_STATE)
    {
      gmp_randclear (obj->value_ptr.random_state);
      free (obj);
    }
  else if (obj->type == TYPE_BYTESPEC)
    free_bytespec (obj);
  else if (obj->type == TYPE_STRUCTURE_CLASS)
    free_structure_class (obj);
  else if (obj->type == TYPE_STRUCTURE)
    free_structure (obj);
  else if (obj->type == TYPE_STANDARD_CLASS)
    free_standard_class (obj);
  else if (obj->type == TYPE_STANDARD_OBJECT)
    free_standard_object (obj);
  else if (obj->type == TYPE_CONDITION_CLASS)
    free_condition_class (obj);
  else if (obj->type == TYPE_CONDITION)
    free_condition (obj);
  else if (obj->type == TYPE_FUNCTION || obj->type == TYPE_MACRO)
    free_function_or_macro (obj);
  else if (obj->type == TYPE_METHOD)
    free_method (obj);
  else if (obj->type == TYPE_STREAM)
    {
      if (obj->value_ptr.stream->is_open
	  && obj->value_ptr.stream->medium == FILE_STREAM)
	fclose (obj->value_ptr.stream->file);

      if (obj->value_ptr.stream->medium == STRING_STREAM)
	delete_reference (obj, obj->value_ptr.stream->string, 0);

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

  delete_reference (obj, s->sym, 0);

  free (s->value);

  if (s->actual_symname_alloc_s)
    free (s->actual_symname);

  free (s);
  free (obj);
}


void
free_symbol (struct object *obj)
{
  struct object *p = obj->value_ptr.symbol->home_package;
  struct symbol *s = obj->value_ptr.symbol;

  delete_reference (obj, s->value_cell, 0);
  delete_reference (obj, s->function_cell, 1);
  delete_reference (obj, s->setf_func_cell, 2);
  delete_reference (obj, s->setf_expander, 3);
  delete_reference (obj, s->typespec, 4);
  delete_reference (obj, s->plist, 5);

  if (p != &nil_object)
    unintern_symbol (obj, p);

  free (s->name);
  free (s);
  free (obj);
}


void
free_cons_pair (struct object *obj)
{
  delete_reference (obj, CAR (obj), 0);
  delete_reference (obj, CDR (obj), 1);

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
  size_t i, sz = array_total_size (obj->value_ptr.array->alloc_size);

  for (i = 0; i < sz; i++)
    {
      delete_reference (obj, obj->value_ptr.array->value [i], i);
    }

  free_array_size (obj->value_ptr.array->alloc_size);
  free (obj->value_ptr.array->value);
  free (obj->value_ptr.array->reference_strength_factor);
  free (obj->value_ptr.array);
  free (obj);
}


void
free_bitarray (struct object *obj)
{
  free_array_size (obj->value_ptr.bitarray->alloc_size);
  mpz_clear (obj->value_ptr.bitarray->value);
  free (obj->value_ptr.bitarray);
  free (obj);
}


void
free_hashtable (struct object *obj)
{
  clear_hash_table (obj);
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
  free (obj->value_ptr.floating);
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
free_structure_class (struct object *obj)
{
  struct structure_field_decl *f = obj->value_ptr.structure_class->fields, *n;

  while (f)
    {
      n = f->next;
      free (f);
      f = n;
    }

  free (obj->value_ptr.structure_class);
  free (obj);
}


void
free_structure (struct object *obj)
{
  struct structure_field *f = obj->value_ptr.structure->fields, *n;

  while (f)
    {
      n = f->next;
      free (f);
      f = n;
    }

  free (obj->value_ptr.structure);
  free (obj);
}


void
free_standard_class (struct object *obj)
{
  struct object_list *p = obj->value_ptr.standard_class->parents, *n;
  struct class_field_decl *f = obj->value_ptr.standard_class->fields, *nf;

  while (p)
    {
      n = p->next;
      free (p);
      p = n;
    }

  while (f)
    {
      nf = f->next;
      free (f);
      f = nf;
    }

  free (obj->value_ptr.standard_class);
  free (obj);
}


void
free_standard_object (struct object *obj)
{
  struct class_field *f = obj->value_ptr.standard_object->fields, *n;

  while (f)
    {
      n = f->next;
      decrement_refcount (f->value);
      free (f);
      f = n;
    }

  free (obj->value_ptr.standard_object);
  free (obj);
}


void
free_condition_class (struct object *obj)
{
  struct object_list *p = obj->value_ptr.condition_class->parents, *n;
  struct condition_field_decl *f = obj->value_ptr.condition_class->fields, *nf;

  while (p)
    {
      n = p->next;
      free (p);
      p = n;
    }

  while (f)
    {
      nf = f->next;
      free (f);
      f = nf;
    }

  free (obj->value_ptr.condition_class);
  free (obj);
}


void
free_condition (struct object *obj)
{
  struct condition_field *f = obj->value_ptr.condition->fields, *n;

  while (f)
    {
      n = f->next;

      decrement_refcount (f->value);
      free (f);

      f = n;
    }

  free (obj->value_ptr.condition);
  free (obj);
}


void
free_lambda_list_content (struct object *obj, struct parameter *par, int *i)
{
  while (par)
    {
      if (par->name)
	{
	  delete_reference (obj, par->name, (*i)++);
	  delete_reference (obj, par->init_form, (*i)++);
	  delete_reference (obj, par->supplied_p_param, (*i)++);
	  delete_reference (obj, par->key, (*i)++);

	  *i += 4;
	}
      else
	{
	  free_lambda_list_content (obj, par->sub_lambda_list, i);
	}

      par = par->next;
    }
}


void
free_lambda_list_structure (struct parameter *par)
{
  struct parameter *n;

  while (par)
    {
      if (!par->name)
	{
	  free_lambda_list_structure (par->sub_lambda_list);
	}

      n = par->next;
      free (par);
      par = n;
    }
}


void
free_function_or_macro (struct object *obj)
{
  struct method_list *nml, *ml = obj->value_ptr.function->methods;
  struct binding *b, *nx;
  int i = 2;

  delete_reference (obj, obj->value_ptr.function->name, 0);
  delete_reference (obj, obj->value_ptr.function->body, 1);

  free_lambda_list_content (obj, obj->value_ptr.function->lambda_list, &i);
  free_lambda_list_structure (obj->value_ptr.function->lambda_list);

  i = 6;

  while (ml)
    {
      delete_reference (obj, ml->meth, i);

      i += 8;

      nml = ml->next;
      free (ml);
      ml = nml;
    }


  b = obj->value_ptr.function->lex_vars;

  while (b)
    {
      nx = b->next;

      b->closure_bin->refcount--;

      if (!b->closure_bin->refcount)
	{
	  decrement_refcount (b->closure_bin->sym);
	  decrement_refcount (b->closure_bin->obj);
	  free (b->closure_bin);
	}

      free (b);
      b = nx;
    }

  b = obj->value_ptr.function->lex_funcs;

  while (b)
    {
      nx = b->next;

      b->closure_bin->refcount--;

      if (!b->closure_bin->refcount)
	{
	  decrement_refcount (b->closure_bin->sym);
	  decrement_refcount (b->closure_bin->obj);
	  free (b->closure_bin);
	}

      free (b);
      b = nx;
    }

  free (obj->value_ptr.function);
  free (obj);
}


void
free_method (struct object *obj)
{
  struct parameter *n, *l = obj->value_ptr.method->lambda_list;
  int i = 2;

  delete_reference (obj, obj->value_ptr.method->generic_func, 0);
  delete_reference (obj, obj->value_ptr.method->body, 1);

  while (l)
    {
      delete_reference (obj, l->name, i++);
      delete_reference (obj, l->init_form, i++);
      delete_reference (obj, l->supplied_p_param, i++);
      delete_reference (obj, l->key, i++);

      n = l->next;
      free (l);
      l = n;
    }

  free (obj->value_ptr.method);
  free (obj);
}


void
free_sharp_macro_call (struct object *macro)
{
  decrement_refcount (macro->value_ptr.sharp_macro_call->feature_test);
  decrement_refcount (macro->value_ptr.sharp_macro_call->obj);

  free (macro->value_ptr.sharp_macro_call);
  free (macro);
}


void
free_list_structure (struct object *list)
{
  if (list->type == TYPE_CONS_PAIR)
    {
      free_list_structure (CDR (list));
      free (list->value_ptr.cons_pair);
      free (list);
    }
}


void
print_welcome_message (void)
{
  puts ("al Copyright (C) 2022-2024 Andrea Monaco\n"
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
	"Copyright (C) 2022-2024 Andrea Monaco\n"
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
