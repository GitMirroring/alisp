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


#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>



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



#define LISP_STACK_SIZE 8192



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

#define SEQUENCE_LENGTH(s) (SYMBOL (s) == &nil_object ? 0 :		\
			    (s)->type == TYPE_CONS_PAIR ? list_length (s) : \
			    (s)->type == TYPE_ARRAY ? \
			    (s)->value_ptr.array->alloc_size->size :	\
			    (s)->type == TYPE_STRING \
			    ? (s)->value_ptr.string->used_size :	\
			    (s)->type == TYPE_BITARRAY			\
			    ? (s)->value_ptr.bitarray->alloc_size->size : 0)

#define ACTUAL_VECTOR_LENGTH(v) ((v)->value_ptr.array->fill_pointer >= 0 \
				 ? (v)->value_ptr.array->fill_pointer	\
				 : (v)->value_ptr.array->alloc_size->size) \

#define ACTUAL_STRING_LENGTH(s) ((s)->value_ptr.string->fill_pointer >= 0 \
				 ? (s)->value_ptr.string->fill_pointer	\
				 : (s)->value_ptr.string->used_size)

#define ACTUAL_BITVECTOR_LENGTH(v) ((v)->value_ptr.bitarray->fill_pointer >= 0 \
				    ? (v)->value_ptr.bitarray->fill_pointer \
				    : (v)->value_ptr.bitarray->alloc_size->size)

#define ACTUAL_SEQUENCE_LENGTH(s) (SYMBOL (s) == &nil_object ? 0	\
				   : (s)->type == TYPE_CONS_PAIR	\
				   ? list_length (s)			\
				   : (s)->type == TYPE_ARRAY		\
				   ? ACTUAL_VECTOR_LENGTH (s)		\
				   : (s)->type == TYPE_STRING		\
				   ? ACTUAL_STRING_LENGTH (s)		\
				   : (s)->type == TYPE_BITARRAY		\
				   ? ACTUAL_BITVECTOR_LENGTH (s) : 0)


#define IS_SYMBOL(s) ((s)->type == TYPE_SYMBOL || (s)->type == TYPE_SYMBOL_NAME)

#define IS_REAL(s) ((s)->type == TYPE_INTEGER || (s)->type == TYPE_FIXNUM \
		    || (s)->type == TYPE_RATIO || (s)->type == TYPE_FLOAT)

#define IS_RATIONAL(s) ((s)->type == TYPE_INTEGER || (s)->type == TYPE_RATIO)

#define IS_NUMBER(s) (IS_REAL (s) || (s)->type == TYPE_COMPLEX)

#define IS_CLASS(obj) ((obj)->type == TYPE_STRUCTURE_CLASS	\
		       || (obj)->type == TYPE_STANDARD_CLASS)

#define IS_TYPE_SPECIFIER(obj) (IS_SYMBOL (obj) || IS_CLASS (obj)	\
				|| obj->type == TYPE_CONS_PAIR)

#define SYMBOL(s) ((s)->type == TYPE_SYMBOL ? (s) :			\
		   (s)->type == TYPE_SYMBOL_NAME ?			\
		   (s)->value_ptr.symbol_name->sym :			\
		   NULL)

#define IS_STRING_DESIGNATOR(s) ((s)->type == TYPE_CHARACTER || IS_SYMBOL (s) \
				 || (s)->type == TYPE_STRING)

#define IS_CHARACTER_DESIGNATOR(s) ((IS_SYMBOL (s)			\
				     && SYMBOL (s)->value_ptr.symbol->name_len) \
				    || ((s)->type == TYPE_STRING	\
					&& (s)->value_ptr.string->used_size) \
				    || (s)->type == TYPE_CHARACTER)

#define IS_PACKAGE_DESIGNATOR(s) (IS_STRING_DESIGNATOR(s) \
				  || (s)->type == TYPE_PACKAGE)

#define IS_PATHNAME_DESIGNATOR(s) ((s)->type == TYPE_STRING		\
				   || ((s)->type == TYPE_STREAM		\
				       && (s)->value_ptr.stream->type	\
				       == FILE_STREAM)			\
				   || ((s)->type == TYPE_FILENAME))

#define IS_FUNCTION_NAME(s) (IS_SYMBOL (s)				\
			     || ((s)->type == TYPE_CONS_PAIR		\
				 && list_length (s) == 2		\
				 && SYMBOL (CAR (s)) == env->setf_sym	\
				 && IS_SYMBOL (CAR (CDR (s)))))

/*#define HAS_LEAF_TYPE(obj) ((obj)->type & (TYPE_INTEGER | TYPE_FIXNUM	\
					   | TYPE_RATIO | TYPE_FLOAT \
					   | TYPE_BYTESPEC | TYPE_STRING \
					   | TYPE_CHARACTER | TYPE_BITARRAY)*/

#define IS_SELF_EVALUATING(obj) ((obj)->type != TYPE_BACKQUOTE		\
				 && !IS_SYMBOL (obj)			\
				 && (obj)->type != TYPE_CONS_PAIR)

#define DONT_STEP(obj) (IS_SELF_EVALUATING (obj)			\
			|| ((obj)->type == TYPE_CONS_PAIR		\
			    && (CAR (obj) == env->quote_sym		\
				|| CAR (obj) == env->function_sym))	\
			|| (IS_SYMBOL (obj)				\
			    && ((SYMBOL (obj)->value_ptr.symbol->home_package \
				 == env->keyword_package)		\
				|| (SYMBOL (obj) == &t_object)		\
				|| (SYMBOL (obj) == &nil_object))))


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



#define BUILTIN_SYMBOL(sym)						\
  (intern_symbol_by_char_vector ((sym), strlen (sym), 0, EXTERNAL_VISIBILITY, \
				 0, env->cl_package, 0, 0))

#define CREATE_BUILTIN_SYMBOL(sym)					\
  (intern_symbol_by_char_vector ((sym), strlen (sym), 1, EXTERNAL_VISIBILITY, \
				 1, env->cl_package, 0, 0))

#define KEYWORD(name)							\
  (intern_symbol_by_char_vector ((name)+1, strlen (name)-1, 1,		\
				 EXTERNAL_VISIBILITY, 1, env->keyword_package, 1, 0))


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

  struct go_tag_frame *frame;

  struct go_tag *next;
};


struct
go_tag_frame
{
  int refcount;

  struct go_tag *frame;

  struct go_tag_frame *next;
};


struct
block
{
  int refcount;

  struct object *name;

  struct block *next;
};


struct
block_frame
{
  struct block *frame;

  struct block_frame *next;
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

  int is_symbol_macro;

  int prev_special;

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
    SHARP_MACRO_REQUIRES_INTEGER_ARGUMENT,
    WRONG_OBJECT_TYPE_TO_SHARP_MACRO,
    UNKNOWN_CHARACTER_NAME,
    FUNCTION_NOT_FOUND_IN_READ,
    WRONG_SYNTAX_IN_SHARP_MACRO_FOR_COMPLEX,
    INVALID_FEATURE_TEST,
    READ_EVAL_IS_DISABLED,
    CANNOT_USE_READ_LABEL_TWICE,
    READ_LABEL_NOT_DEFINED,

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


    ABORT_TO_TOP_LEVEL,
    ABORT_ONE_LEVEL,


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
    WRONG_NUMBER_OF_AXES,
    OUT_OF_BOUND_INDEX,
    DECREASING_INTERVAL_NOT_MEANINGFUL,
    INVALID_SIZE,
    WRONG_TYPE_OF_STANDARD_VARIABLE,
    NO_APPLICABLE_METHOD,
    NO_PRIMARY_APPLICABLE_METHOD,
    NO_NEXT_METHOD,
    COULD_NOT_RENAME_FILE,
    COULD_NOT_DELETE_FILE,
    COULD_NOT_OPEN_FILE,
    COULD_NOT_OPEN_FILE_FOR_READING,
    COULD_NOT_SEEK_FILE,
    COULD_NOT_TELL_FILE,
    FILE_ALREADY_EXISTS,
    ERROR_READING_FILE,
    ERROR_DURING_OUTPUT,
    ERROR_PERFORMING_STAT_ON_FILE,
    COULD_NOT_OPEN_DIR,
    COULD_NOT_CREATE_DIR,
    ERROR_READING_DIR,
    COULD_NOT_DETERMINE_CWD,
    INVALID_TYPE_SPECIFIER,
    UNKNOWN_TYPE,
    CLASS_NOT_FOUND,
    CLASS_DEFINITION_NOT_COMPLETE,
    CLASS_ANCESTRY_NOT_CONSISTENT,
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
    CANT_SHADOW_IN_KEYWORD_PACKAGE,
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
				  || (t) == SHARP_MACRO_REQUIRES_INTEGER_ARGUMENT \
				  || (t) == WRONG_OBJECT_TYPE_TO_SHARP_MACRO \
				  || (t) == UNKNOWN_CHARACTER_NAME	\
				  || (t) == FUNCTION_NOT_FOUND_IN_READ	\
				  || (t) == WRONG_SYNTAX_IN_SHARP_MACRO_FOR_COMPLEX \
				  || (t) == INVALID_FEATURE_TEST	\
				  || (t) == READ_EVAL_IS_DISABLED	\
				  || (t) == CANNOT_USE_READ_LABEL_TWICE \
				  || (t) == READ_LABEL_NOT_DEFINED	\
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

  struct go_tag *tag_to_jump_to;

  struct block *block_to_leave;

  struct object *catching_tag;

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


struct
refcounted_object_list
{
  int reference_strength_factor;
  struct object *obj;
  struct refcounted_object_list *next;
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
compiler_macro
{
  struct object *name;

  struct object *macro;

  int is_setf;

  struct compiler_macro *next;
};


enum
stepping_flags
  {
    NO_STEPPING,

    STEP_INSIDE_FORM,
    STEP_OVER_FORM,
    STEP_OVER_EXPANSION = 4,

    STEPPING_OVER_FORM = 8
  };


#define PROFILING_HASHTABLE_SIZE 1024

struct
profiling_record
{
  struct object *name;
  int is_setf;

  unsigned counter;
  clock_t time;

  struct profiling_record *next;
};


struct
call_frame
{
  struct object *funcobj;

  struct object *arglist;

  struct binding *args;
  int argsnum;

  struct call_frame *next;
};


struct
read_label
{
  int label;
  struct object *value;
  struct read_label *next;
};


struct
environment
{
  struct binding *vars;
  int lex_env_vars_boundary;

  struct binding *funcs;
  int lex_env_funcs_boundary;

  int only_lexical;


  struct object_list *packages;
  struct object *cl_package, *cluser_package, *keyword_package;

  struct go_tag_frame *go_tag_stack;

  struct block_frame *blocks;

  struct object_list *catches;

  struct handler_binding_frame *handlers;

  struct restart_binding *restarts;

  struct compiler_macro *compiler_macros;


  int stack_depth;


  struct object_list *traced_funcs;

  int debugging_depth;

  enum stepping_flags stepping_flags;
  struct object *next_eval, *watched_obj, *obj_field, *new_value, *last_command;

  int is_profiling;
  struct profiling_record **profiling_data;

  struct call_frame *call_stack;


  struct read_label *read_labels;
  int curr_label;


  struct object *method_args;
  struct method_list *method_list;


  struct object *c_stdout;


  struct object *quote_sym, *function_sym, *lambda_sym, *setf_sym;

  struct object *declare_sym, *ignorable_sym, *ignore_sym, *inline_sym,
    *notinline_sym, *optimize_sym, *compilation_speed_sym, *debug_sym,
    *safety_sym, *space_sym, *speed_sym, *special_sym, *type_sym, *ftype_sym,
    *dynamic_extent_sym;

  struct object *amp_optional_sym, *amp_rest_sym, *amp_body_sym, *amp_key_sym,
    *amp_allow_other_keys_sym, *amp_aux_sym, *amp_whole_sym,
    *key_allow_other_keys_sym;

  struct object *not_sym, *and_sym, *or_sym, *eql_sym, *member_sym,
    *satisfies_sym, *star_sym;

  struct object *gensym_counter_sym;

  struct object *package_sym, *random_state_sym, *std_in_sym, *std_out_sym,
    *err_out_sym, *print_escape_sym, *print_readably_sym, *print_base_sym,
    *print_radix_sym, *print_array_sym, *print_gensym_sym, *print_pretty_sym,
    *print_pprint_dispatch_sym, *read_eval_sym, *read_base_sym,
    *read_suppress_sym, *load_pathname_sym, *load_truename_sym,
    *break_on_signals_sym;

  struct object *abort_sym;

  struct object *al_compile_when_defining_sym, *al_debugging_condition_sym;

  struct object *al_print_always_two_colons;
};


enum
package_record_flags
  {
    INTERNAL_VISIBILITY = 1,
    EXTERNAL_VISIBILITY = 2,
    IS_SHADOWING = 4
  };


struct
package_record
{
  enum package_record_flags flags;

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
  int is_symbol_macro;

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
  enum package_record_flags visibility;

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
  int sub_found_amp_key;
  int sub_allow_other_keys;

  struct object *key;
  struct object *typespec;

  struct object *init_form;
  struct object *supplied_p_param;

  int reference_strength_factor;

  int key_passed;

  struct parameter *next;
};


enum
function_flags
  {
    GENERIC_FUNCTION = 1,
    COMPILED_FUNCTION,
    FOUND_AMP_KEY = 4,
    TRACED_FUNCTION = 8
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

  struct block *encl_blocks;

  struct go_tag_frame *encl_tags;


  struct object *body;

  enum function_flags flags;

  struct method_list *methods;

  struct object *(*builtin_form)
    (struct object *list, struct environment *env, struct outcome *outcome);


  struct object *struct_constructor_class_name;


  struct object *struct_accessor_class_name;
  struct object *struct_accessor_field;


  struct object *struct_predicate_class_name;


  struct object *struct_copyier_class_name;


  struct object *condition_reader_class_name;
  struct object *condition_reader_field;


  struct object *function_macro;


  struct object *macro_function;
};


enum
method_qualifier
  {
    AROUND_METHOD = 1,
    BEFORE_METHOD = 2,
    PRIMARY_METHOD = 4,
    AFTER_METHOD = 8,
    AUXILIARY_METHOD = 11
  };


struct
method
{
  struct object *generic_func;

  struct parameter *lambda_list;
  int found_amp_key;
  int allow_other_keys;

  enum method_qualifier qualifier;

  struct object *(*builtin_method)
    (struct object *list, struct environment *env, struct outcome *outcome);

  struct object *body;

  struct object *object_reader_class;
  struct object *object_reader_field;

  struct object *object_writer_class;
  struct object *object_writer_field;

  struct object *object_accessor_class;
  struct object *object_accessor_field;
};


struct
method_list
{
  int reference_strength_factor;

  struct object *meth;

  struct method_list *next;
};


#define FILLING_CAR(c) ((c)->flags & 0x4)
#define SET_FILLING_CAR(c) ((c)->flags |= 0x4)
#define CLEAR_FILLING_CAR(c) ((c)->flags &= ~0x4)

#define EMPTY_LIST_IN_CAR(c) ((c)->flags & 0x8)
#define SET_EMPTY_LIST_IN_CAR(c) ((c)->flags |= 0x8)
#define CLEAR_EMPTY_LIST_IN_CAR(c) ((c)->flags &= ~0x8)

#define FOUND_DOT(c) ((c)->flags & 0x10)
#define SET_FOUND_DOT(c) ((c)->flags |= 0x10)

#define FILLING_CDR(c) ((c)->flags & 0x20)
#define SET_FILLING_CDR(c) ((c)->flags |= 0x20)
#define CLEAR_FILLING_CDR(c) ((c)->flags &= ~0x20)

#define EMPTY_LIST_IN_CDR(c) ((c)->flags & 0x40)
#define SET_EMPTY_LIST_IN_CDR(c) ((c)->flags |= 0x40)
#define CLEAR_EMPTY_LIST_IN_CDR(c) ((c)->flags &= ~0x40)

struct
cons_pair
{
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


enum
filename_type
  {
    REGULAR_FILENAME,

    UNSPECIFIC_FILENAME,
    WILD_FILENAME
  };


struct
filename
{
  int is_logical;
  struct object *value;
  enum filename_type directory_type;
  enum filename_type name_type;
};


enum
stream_type
  {
    FILE_STREAM,
    STRING_STREAM,
    SYNONYM_STREAM,
    BROADCAST_STREAM
  };


enum
stream_content_type
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

  enum stream_content_type content_type;

  enum stream_direction direction;

  FILE *file;
  struct object *namestring;

  struct object *string;

  struct object *synonym_of;

  struct refcounted_object_list *broadcast_to;


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


enum
field_allocation_type
  {
    UNKNOWN_ALLOCATION,
    INSTANCE_ALLOCATION,
    CLASS_ALLOCATION
  };


struct
class_field_decl
{
  struct object *name;

  enum field_allocation_type alloc_type;

  struct object *value;

  struct object *initform;

  struct object_list *initargs;

  struct class_field_decl *next;
};


struct
precedence_relation
{
  struct object *first;
  struct object *second;
  int is_parent;
  struct precedence_relation *next;
};


struct
standard_class
{
  struct object *name;

  int is_condition_class;

  struct object_list *parents;
  struct object_list *descendants;

  struct object_list *class_precedence_list;

  struct class_field_decl *fields;
};


struct
class_field
{
  struct object *name;

  struct object *value;

  struct class_field_decl *decl;

  int found_key;

  struct class_field *next;
};


struct
standard_object
{
  struct object *class;

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


#define IS_WATCHED(obj) ((obj)->flags & 0x800)

#define SET_WATCHED_FLAG(obj) ((obj)->flags =				\
			       WITH_CHANGED_BIT ((obj)->flags, 11, 1))

#define CLEAR_WATCHED_FLAG(obj) ((obj)->flags =				\
				 WITH_CHANGED_BIT ((obj)->flags, 11, 0))



struct
refcounted_object_list_old
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
 int backts_commas_balance, int preserve_whitespace, int ends_with_eof,
 struct environment *env, struct outcome *outcome, const char **macro_end);
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
			       int decrement_refc, struct environment *env,
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
struct object *alloc_method (void);
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
							  int only_check_presence,
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

/*struct refcounted_object_list **alloc_empty_tailsharing_hash_table
(size_t table_size);
int is_object_in_refcounted_obj_list (const struct object *obj,
				      const struct refcounted_object_list *list);
void prepend_object_to_refcounted_obj_list (struct object *obj,
					    struct refcounted_object_list **list);
struct refcounted_object_list **clone_tailsharing_hash_table
(struct refcounted_object_list **hash_table, size_t table_size);
void free_tailsharing_hash_table (struct refcounted_object_list **hash_table,
size_t table_size);*/

void capture_lexical_environment (struct binding **lex_vars,
				  struct binding **lex_funcs,
				  struct binding *vars, int var_num,
				  struct binding *funcs, int func_num);

struct object *create_function (struct object *lambda_list, struct object *body,
				struct environment *env,
				struct outcome *outcome, int is_macro,
				int allow_destructuring);

struct object *create_empty_generic_function (struct object *name, int is_setf,
					      struct environment *env);

const char *find_end_of_string
(const char *input, size_t size, size_t *new_size, size_t *string_length);
void normalize_string (char *output, const char *input, size_t size);

struct object *alloc_string (fixnum size);
struct object *copy_string (struct object *string);
struct object *create_string_from_sequence (struct object *seq, fixnum size);
struct object *create_string_copying_char_vector (const char *str, fixnum size);
struct object *create_string_with_char_vector (char *str, fixnum size);
struct object *create_string_copying_c_string (const char *str);
void resize_string_allocation (struct object *string, fixnum size);
void increment_string_allocation_respecting_fill_pointer (struct object *string,
							  fixnum incr);
char *copy_string_to_c_string (struct string *str);

char *concatenate_char_vectors (size_t totsize, ...);

struct object *alloc_symbol_name (size_t value_s, size_t actual_symname_s);
void resize_symbol_name (struct object *symname, size_t value_s,
			 size_t actual_symname_s);

const char *find_end_of_symbol_name (const char *input, size_t size,
				     int ends_with_eof, int preserve_whitespace,
				     int already_begun, int found_package_sep,
				     const char **start_of_package_separator,
				     enum package_record_flags *sym_visibility,
				     size_t *name_length,
				     size_t *act_name_length,
				     struct outcome *out);
void copy_symname_with_case_conversion (char *output, const char *input,
					size_t size,
					enum readtable_case read_case,
					int single_escape, int multiple_escape);

void clear_symbol (struct symbol *sym);
struct object *create_symbol (char *name, size_t size, int do_copy);
struct object *create_filename (struct object *string);

int get_directory_file_split (struct object *string);
int get_filename (struct object *string, int *end);

struct object *inspect_pathname_by_designator (struct object *des);

struct object *alloc_vector (fixnum size, int fill_with_nil,
			     int dont_store_size);
struct object *create_vector_from_list (struct object *list, fixnum size);
struct object *alloc_bitvector (fixnum size);
struct object *create_bitvector_from_char_vector (const char *in, size_t sz,
						  size_t req_size);
struct object *fill_axis_from_sequence (struct object *arr, struct object **axis,
					fixnum index, struct array_size *size,
					fixnum rowsize, struct object *seq);
struct object *create_array_from_sequence (struct object *seq, fixnum rank);
struct object *adjust_array_axis (struct object *dest,
				  struct array_size *dest_axis_size,
				  struct object *src,
				  struct array_size *src_axis_size,
				  struct array_size *ind, struct array_size *i);
void resize_vector (struct object *vector, fixnum size);

struct object *create_character (char *character, int do_copy);
struct object *create_character_from_utf8 (char *character, size_t size);
struct object *create_character_from_char (char ch);
struct object *create_character_from_designator (struct object *des);
struct object *get_nth_character (struct object *str, int ind);
fixnum get_nth_character_offset (struct object *str, int ind);
fixnum get_nth_character_preceding_offset (struct object *str, int ind);
int set_nth_character (struct object *str, int ind, char *ch);

struct object *create_file_stream (enum stream_content_type content_type,
				   enum stream_direction direction,
				   struct object *namestring, int overwrite,
				   int if_doesnt_exist, struct outcome *outcome);
struct object *create_stream_from_open_file (enum stream_content_type content_type,
					     enum stream_direction direction,
					     FILE *file);
struct object *create_string_stream (enum stream_direction direction,
				     struct object *instr, int begin, int end);
struct object *create_synonym_stream (struct object *sym);

struct structure_field_decl *create_structure_field_decl
(struct object *fieldform, struct environment *env, struct outcome *outcome);

struct class_field_decl *create_class_field_decl (struct object *class,
						  struct object *fieldform,
						  struct environment *env,
						  struct outcome *outcome);

struct condition_field_decl *create_condition_field_decl
(struct object *fieldform, struct environment *env, struct outcome *outcome);

struct object *define_class (struct object *name, struct object *form,
			     int is_condition_class, struct environment *env,
			     struct outcome *outcome);

struct object *allocate_object_fields (struct object *stdobj,
				       struct object *class);
struct class_field *find_object_field_by_initarg (struct object *stdobj,
						  struct object *initarg);
struct object *fill_object_fields_by_initargs (struct object *stdobj,
					       struct object *class,
					       struct object *initargs,
					       struct environment *env,
					       struct outcome *outcome);
struct object *fill_object_fields (struct object *stdobj, struct object *class,
				   struct object *initargs,
				   struct environment *env,
				   struct outcome *outcome);

void create_condition_fields (struct object *stdobj, struct object *class);

struct object *load_file (const char *filename, int print_each_form,
			  struct environment *env, struct outcome *outcome);

struct object *compile_function (struct object *fun, struct environment *env,
				 struct outcome *outcome);
struct object *compile_form (struct object *form, int backt_comma_bal,
			     struct environment *env, struct outcome *outcome);
int compile_body (struct object *body, int backt_comma_bal,
		  struct environment *env, struct outcome *outcome);

struct object *intern_symbol_by_char_vector (char *name, size_t len,
					     int do_copy,
					     enum package_record_flags vis,
					     int always_create_if_missing,
					     struct object *package,
					     int is_keyword_package,
					     int always_export_if_created);
struct object *intern_symbol_name (struct object *symname,
				   struct environment *env,
				   enum outcome_type *out);
int unintern_symbol (struct object *sym, struct object *pack);

struct binding *create_binding (struct object *sym, struct object *obj,
				enum binding_type type, int inc_refcs);
struct binding *add_binding (struct binding *bin, struct binding *env);
struct binding *chain_bindings (struct binding *bin, struct binding *env,
				int increment_dyn_bin_count, int *num,
				struct binding **last_bin);
struct binding *remove_bindings (struct binding *env, int num,
				 int decrement_dyn_bin_count);
struct binding *find_binding (struct symbol *sym, struct binding *bins,
			      enum binding_type type, int bin_num,
			      int only_lexical);

struct binding *bind_variable (struct object *sym, struct object *val,
			       int increment_dyn_bin_count,
			       struct binding *bins);

struct go_tag_frame *collect_go_tags (struct object *body,
				      struct go_tag_frame *stack,
				      int *found_tags);
struct go_tag_frame *add_go_tag_frame (struct go_tag_frame *stack);
void add_go_tag (struct object *tagname, struct object *tagdest,
		 struct go_tag_frame *frame);
struct go_tag_frame *remove_go_tag_frame (struct go_tag_frame *stack);
struct go_tag *find_go_tag (struct object *tagname, struct go_tag_frame *frame);

struct block_frame *add_block (struct object *name, struct block_frame *blocks);
struct block *remove_block (struct block *blocks);

struct object *create_condition (struct object *type, struct object *args,
				 struct environment *env,
				 struct outcome *outcome);
struct object *create_condition_by_c_string (char *type, struct object *args,
					     struct environment *env,
					     struct outcome *outcome);

int does_condition_include_outcome_type (struct object *cond,
					 enum outcome_type type,
					 struct environment *env);
void add_condition_class (char *name, struct environment *env, int is_standard,
			  ...);
struct object *handle_condition (struct object *cond, struct environment *env,
				 struct outcome *outcome);

struct object *list_lambda_list (struct parameter *par, int allow_other_keys,
				 struct environment *env);

struct object *create_empty_condition_by_c_string (char *classname,
						   struct environment *env);
struct object *raise_unbound_variable (struct object *sym,
				       struct environment *env,
				       struct outcome *outcome);
struct object *raise_undefined_function (struct object *sym,
					 struct environment *env,
					 struct outcome *outcome);
struct object *raise_type_error (struct object *datum, char *type,
				 struct environment *env,
				 struct outcome *outcome);
struct object *raise_file_error (struct object *fn, const char *fs,
				 struct environment *env,
				 struct outcome *outcome);
struct object *raise_al_maximum_stack_depth_exceeded (int maxdepth,
						      struct environment *env,
						      struct outcome *outcome);
struct object *raise_al_wrong_number_of_arguments (int minargs, int maxargs,
						   struct environment *env,
						   struct outcome *outcome);
struct object *raise_program_error (struct environment *env,
				    struct outcome *outcome);
struct object *raise_error (struct environment *env, struct outcome *outcome);

struct object *create_room_pair (char *sym, int val, struct environment *env);
struct object *create_pair (struct object *car, struct object *cdr);
struct object *dump_bindings (struct binding *bin, int lex_boundary,
			      struct environment *env);

int print_specializers_from_lambda_list (struct parameter *par,
					 struct environment *env,
					 struct stream *str);

void print_function_name (struct object *func, struct environment *env);
void print_method_description (struct object *meth, struct environment *env);

void print_bindings_in_reverse (struct binding *bins, int num,
				struct environment *env, struct object *str);

void print_fields (struct object *stdobj, struct environment *env,
		   struct object *str);

void print_backtrace (struct environment *env, int be_verbose);
struct object *list_backtrace (struct environment *env, int be_verbose);

void print_available_restarts (struct environment *env, int show_help,
			       struct object *str);
void print_stepping_help (void);
struct object *enter_debugger (struct object *cond, struct environment *env,
			       struct outcome *outcome);

void add_profiling_data (struct profiling_record **data, struct object *name,
			 int is_setf, clock_t time, clock_t evaltime);

struct call_frame *add_call_frame (struct object *funcobj, struct object *args,
				   struct environment *env, int argsnum,
				   struct call_frame *stack);
struct call_frame *remove_call_frame (struct call_frame *stack);

struct read_label *add_read_label (int label, struct object *value,
				   struct read_label *labels);
void clear_read_labels (struct read_label **label);

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

struct object *define_generic_function (char *name, struct environment *env,
					struct parameter *lambda_list,
					struct object *(*default_method)
					(struct object *list,
					 struct environment *env,
					 struct outcome *outcome));

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
fixnum array_row_major_index (const struct array_size *ind,
			      const struct array_size *sz);

int hash_object_respecting_eq (const struct object *object, size_t table_size);
int hash_object_respecting_eql (const struct object *object, size_t table_size);
int hash_object_respecting_equal (const struct object *object, size_t table_size);
int hash_object_respecting_equalp (const struct object *object,
				   size_t table_size);
int hash_table_count (const struct hashtable *hasht);
void clear_hash_record (struct object *hasht, struct hashtable_record *rec,
			int ind, int depth);
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
struct parameter *parse_lambda_list (struct object *obj, int allow_destructuring,
				     int is_specialized, struct environment *env,
				     struct outcome *outcome, int *found_amp_key,
				     int *allow_other_keys);

struct parameter *create_lambda_list (struct environment *env, ...);
struct parameter *copy_lambda_list (struct parameter *list,
				    int fill_typespecs_with_t);

void count_parameters (struct parameter *par, int *req_params, int *opt_params,
		       int *rest);
int are_lambda_lists_congruent (struct parameter *meth_list,
				int meth_has_amp_key,
				struct parameter *gen_list, int gen_has_amp_key);

int parse_declaration_specifier (struct object *spec, int is_local,
				 struct environment *env, int bin_num,
				 struct outcome *outcome);
int parse_declarations (struct object *body, struct environment *env,
			int bin_num, int allow_docstring,
			struct outcome *outcome, struct object **next);
void undo_special_declarations (struct object *decl, struct environment *env);

struct object *evaluate_body
(struct object *body, int parse_decls, int is_tagbody, int collect_tags,
 struct object *block_name, struct environment *env, struct outcome *outcome);

int parse_argument_list (struct object *arglist, struct parameter *par,
			 int eval_args, int also_pass_name, int is_typespec,
			 int found_amp_key, int allow_other_keys,
			 struct binding *lex_vars, int create_new_lex_env,
			 struct environment *env, struct outcome *outcome,
			 struct binding **bins, int *argsnum, int *closnum);
int destructure_tree (struct object *template, struct object *vals,
		      struct binding **bins, int *binnum,
		      struct outcome *outcome);

void restore_lexical_environment (struct environment *env, struct binding *vars,
				  struct binding *funcs, int *num_vars);

struct object *call_function (struct object *func, struct object *arglist,
			      int eval_args, int also_pass_name,
			      int create_new_lex_env, int expand_and_eval,
			      int is_typespec, struct environment *env,
			      struct outcome *outcome);
struct object *call_structure_constructor (struct object *class_name,
					   struct object *args,
					   struct environment *env,
					   struct outcome *outcome);
struct object *call_structure_predicate (struct object *class_name,
					 struct object *args,
					 struct environment *env,
					 struct outcome *outcome);
struct object *call_structure_copyier (struct object *class_name,
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
struct object *call_method (struct method_list *methlist, struct object *arglist,
			    struct environment *env, struct outcome *outcome);

int is_class_completely_defined (struct object *class);
struct precedence_relation *add_precedence_relations (struct object *class,
						      struct precedence_relation *rels,
						      int *not_completely_defined);
struct object_list *collect_superclasses (struct object *class,
					  struct object_list *scs);
int is_direct_subclass (struct object *first, struct object *second);
struct object *compute_next_element_in_precedence_list
(struct object_list *precedence_list, struct object_list *superclasses,
 struct precedence_relation *prec_relations);
struct object_list *remove_element_from_obj_list (struct object *el,
						  struct object_list *l);
struct precedence_relation *remove_relations_with_given_first
(struct object *el, struct precedence_relation *rels);
int compute_class_precedence_list (struct object *class, struct outcome *outcome);

int is_method_applicable (struct object *meth, struct object *args,
			  struct environment *env, struct outcome *outcome);
int compare_method_specificity (struct object *first, struct object *second,
				struct environment *env);
void add_method (struct object *genfun, struct object *meth);
struct object *find_method (struct object *func, enum method_qualifier qualifier,
			    struct object *c_specifiers,
			    struct parameter *l_specifiers,
			    struct method_list **mlist, int *ind,
			    struct environment *env, struct outcome *outcome);
struct object *dispatch_generic_function_call (struct object *func,
					       struct object *arglist,
					       int eval_args,
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
		   const struct object *second, const struct object *prev,
		   struct environment *env);
int is_subtype (const struct object *firstsp, const struct object *secondsp,
		const struct object *prev, struct environment *env,
		struct outcome *outcome);

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
int type_compiled_function (const struct object *obj,
			    const struct object *typespec,
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
int type_logical_pathname (const struct object *obj, const struct object *typespec,
			   struct environment *env, struct outcome *outcome);
int type_stream (const struct object *obj, const struct object *typespec,
		 struct environment *env, struct outcome *outcome);
int type_file_stream (const struct object *obj, const struct object *typespec,
		      struct environment *env, struct outcome *outcome);
int type_string_stream (const struct object *obj, const struct object *typespec,
			struct environment *env, struct outcome *outcome);
int type_synonym_stream (const struct object *obj, const struct object *typespec,
			struct environment *env, struct outcome *outcome);
int type_broadcast_stream (const struct object *obj,
			   const struct object *typespec,
			   struct environment *env, struct outcome *outcome);
int type_standard_object (const struct object *obj, const struct object *typespec,
			  struct environment *env, struct outcome *outcome);
int type_generic_function (const struct object *obj, const struct object *typespec,
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

int type_al_backquote (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct outcome *outcome);
int type_al_comma (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct outcome *outcome);
int type_al_at (const struct object *obj, const struct object *typespec,
		struct environment *env, struct outcome *outcome);
int type_al_dot (const struct object *obj, const struct object *typespec,
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
struct object *builtin_sxhash
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
struct object *builtin_pathname
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_make_pathname
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_namestring
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_pathname_directory
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_pathname_name
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_pathname_type
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_wild_pathname_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_logical_pathname
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_translate_logical_pathname
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_truename
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_probe_file
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_ensure_directories_exist
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_file_position
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_file_length
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_rename_file
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_delete_file
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_read_line
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_read
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_read_preserving_whitespace
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_read_from_string
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_parse_integer
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_eval
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_compile
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
struct object *builtin_make_synonym_stream
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_synonym_stream_symbol
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_make_broadcast_stream
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_broadcast_stream_streams
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_finish_output
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
struct object *builtin_equalp
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
struct object *builtin_setf_symbol_function (struct object *list,
					     struct environment *env,
					     struct outcome *outcome);
struct object *builtin_setf_fdefinition (struct object *list,
					 struct environment *env,
					 struct outcome *outcome);
struct object *builtin_setf_macro_function (struct object *list,
					    struct environment *env,
					    struct outcome *outcome);

struct object *builtin_method_print_object (struct object *list,
					    struct environment *env,
					    struct outcome *outcome);

int compare_two_numbers (struct object *num1, struct object *num2);
struct object *compare_any_numbers (struct object *list, struct environment *env,
				    struct outcome *outcome,
				    enum number_comparison comp);
int is_zero (const struct object *num);
int is_bit (const struct object *num);
struct object *negate_real_number (struct object *num, int in_place);
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
 enum object_type quotient_type, struct environment *env, struct outcome *outcome);

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
struct object *builtin_rational (struct object *list, struct environment *env,
				 struct outcome *outcome);
struct object *builtin_float (struct object *list, struct environment *env,
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
struct object *builtin_coerce (struct object *list, struct environment *env,
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
struct object *builtin_fdefinition (struct object *list, struct environment *env,
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
struct object *builtin_gensym (struct object *list, struct environment *env,
			       struct outcome *outcome);
struct object *builtin_macroexpand_1 (struct object *list,
				      struct environment *env,
				      struct outcome *outcome);
struct object *builtin_macro_function (struct object *list,
				       struct environment *env,
				       struct outcome *outcome);
struct object *builtin_compiler_macro_function (struct object *list,
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
struct object *builtin_shadow (struct object *list, struct environment *env,
			       struct outcome *outcome);
struct object *builtin_package_shadowing_symbols (struct object *list,
						  struct environment *env,
						  struct outcome *outcome);
struct object *builtin_do_symbols (struct object *list, struct environment *env,
				   struct outcome *outcome);
struct object *builtin_do_external_symbols (struct object *list,
					    struct environment *env,
					    struct outcome *outcome);

struct object *builtin_time (struct object *list, struct environment *env,
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
 int allow_three_elements, int increment_dyn_bin_count);
struct object *evaluate_let
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_let_star
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_progv
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_locally
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
int is_macro (struct object *sym, struct environment *env);

struct object *inspect_variable_by_c_string (char *var,
					     struct environment *env);
struct object *inspect_variable (struct object *sym, struct environment *env);

struct object *set_value (struct object *sym, struct object *value,
			  int expand_symmacros, int eval_value,
			  struct environment *env, struct outcome *outcome);
int set_values_destructuring (struct object *template, struct object *vals,
			      struct environment *env, struct outcome *outcome);
struct object *setf_value (struct object *form, struct object *value,
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
struct object *builtin_constantp
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
struct object *evaluate_psetf
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
struct object *evaluate_and
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_or
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
struct object *evaluate_define_symbol_macro
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_symbol_macrolet
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defstruct
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defclass
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_find_class
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_method_make_instance
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_method_allocate_instance
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_method_initialize_instance
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_method_reinitialize_instance
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_method_change_class
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
struct object *builtin_ensure_generic_function
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_defmethod
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_add_method
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_find_method
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_remove_method
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_next_method_p
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_call_next_method
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_no_next_method
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_function_lambda_expression
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
struct object *builtin_compute_restarts
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_invoke_restart
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_unwind_protect
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_signal
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_error
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_warn
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_define_condition
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_make_condition
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *evaluate_define_compiler_macro
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_room
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_trace
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_untrace
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_invoke_debugger
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_step
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *evaluate_al_loopy_destructuring_bind
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *evaluate_al_loopy_setq
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_string_input_stream_string
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_print_restarts
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_dump_bindings
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_dump_function_bindings
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_dump_captured_env
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_dump_methods
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_dump_fields
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_class_precedence_list
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_start_profiling
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_stop_profiling
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_clear_profiling
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_report_profiling
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_print_backtrace
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_list_backtrace
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_watch
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_unwatch
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_next
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_compile_form
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_print_no_warranty
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_print_terms_and_conditions
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_list_directory
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_directoryp
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_getcwd
(struct object *list, struct environment *env, struct outcome *outcome);

struct object *builtin_al_getenv
(struct object *list, struct environment *env, struct outcome *outcome);
struct object *builtin_al_system
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

struct object *resolve_synonym_stream (struct object *str,
				       struct environment *env,
				       struct outcome *outcome);

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
int print_string (const struct string *s, struct environment *env,
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

int print_object_nicely (struct object *obj, struct environment *env,
			 struct outcome *outcome, struct object *str);

void print_error (struct outcome *err, struct environment *env);

void mark_as_constant (struct object *obj);

struct parameter *parameter_by_index (struct parameter *par, int *ind);
int is_reference_weak (struct object *src, int ind, struct object *dest);
void set_reference_strength_factor (struct object *src, int ind,
				    struct object *dest, int new_weakness,
				    int increase_refcount,
				    int decrease_other_refcount);
void add_strong_reference (struct object *src, struct object *dest, int ind);
void add_reference_to_object_just_read (struct object *src, struct object *dest,
					int ind);
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
void free_lambda_list_content (struct object *obj, struct parameter *par, int *i,
			       int is_method);
void free_lambda_list_structure (struct parameter *par);
void free_method_list (struct object *fun, struct method_list *ml, int ind);
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



int num_objects;
int num_symbols;
int num_numbers;
int num_conses;
int num_strings;
int num_arrays;
int num_functions;



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
  char *wholel = NULL, *cl_path;

  struct command_line_options opts = {1};


  parse_command_line (&opts, argc, argv);

  add_standard_definitions (&env);

  c_stdout = env.c_stdout;


  define_variable ("*AL-ARGC*", create_integer_from_long (argc), &env);

  al_argv = alloc_vector (argc, 0, 0);

  for (i = 0; i < argc; i++)
    {
      al_argv->value_ptr.array->value [i] =
	create_string_copying_c_string (argv [i]);
    }

  define_variable ("*AL-ARGV*", al_argv, &env);


  if (!opts.load_and_exit)
    print_welcome_message ();


  define_variable ("*AL-MODULE-PATH*",
		   create_string_copying_c_string (MODULE_PATH), &env);

  if (opts.load_cl)
    {
      cl_path = concatenate_char_vectors (strlen (MODULE_PATH)+strlen ("cl.lisp"),
					  MODULE_PATH, strlen (MODULE_PATH),
					  "cl.lisp", strlen ("cl.lisp"),
					  (char *)NULL);

      cl_path = append_zero_byte (cl_path, strlen (MODULE_PATH)+strlen ("cl.lisp"));

      if (!opts.load_and_exit)
	printf ("Loading %s... ", cl_path);

      result = load_file (cl_path, 0, &env, &eval_out);

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

      free (cl_path);
    }

  if (opts.load_before_repl)
    {
      if (!opts.load_and_exit)
	printf ("Loading %s...\n", opts.load_before_repl);

      result = load_file (opts.load_before_repl, 0, &env, &eval_out);

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
      result = load_file (opts.load_and_exit, 0, &env, &eval_out);

      exit (0);
    }


#ifdef HAVE_LIBREADLINE
  c = read_history ("al_history");

  if (c && c != ENOENT)
    printf ("could not read line history from al_history: %s\n", strerror (c));
#endif


  while (!end_repl)
    {
      free (wholel);
      env.stack_depth = 0;

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
		  else
		    {
		      printf ("unhandled condition of type ");
		      print_object (eval_out.condition->value_ptr.condition->
				    class_name, &env, c_stdout->value_ptr.stream);
		      fresh_line (c_stdout->value_ptr.stream);
		    }

		  decrement_refcount (eval_out.condition);
		  eval_out.condition = NULL;
		}
	      else if (eval_out.type != ABORT_TO_TOP_LEVEL)
		print_error (&eval_out, &env);
	    }
	  else if (eval_out.no_value)
	    {
	      fresh_line (c_stdout->value_ptr.stream);
	      eval_out.no_value = 0;
	    }
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

	  env.stepping_flags = 0;

	  env.stack_depth = 0;

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
  struct object *rs, *stdobjsym, *stdobjcl;
  struct package_record *rec;
  struct parameter *lambdal;

  env->keyword_package = create_package_from_c_strings ("KEYWORD", (char *)NULL);
  prepend_object_to_obj_list (env->keyword_package, &env->packages);

  env->cl_package = create_package_from_c_strings ("COMMON-LISP", "CL",
						   (char *)NULL);
  prepend_object_to_obj_list (env->cl_package, &env->packages);

  env->cluser_package = create_package_from_c_strings ("COMMON-LISP-USER",
						       "CL-USER",
						       (char *)NULL);
  prepend_object_to_obj_list (env->cluser_package, &env->packages);

  use_package (env->cl_package, env->cluser_package, NULL);


  t_symbol.value_cell = &t_object;
  t_symbol.home_package = env->cl_package;

  nil_symbol.value_cell = &nil_object;
  nil_symbol.home_package = env->cl_package;

  mpz_init (integer_one);
  mpz_set_si (integer_one, 1);


  rec = malloc_and_check (sizeof (*rec));
  rec->flags = EXTERNAL_VISIBILITY;
  rec->sym = &t_object;
  rec->next = NULL;
  env->cl_package->value_ptr.package->symtable
    [hash_char_vector ("T", sizeof ("T"), SYMTABLE_SIZE)] = rec;

  rec = malloc_and_check (sizeof (*rec));
  rec->flags = EXTERNAL_VISIBILITY;
  rec->sym = &nil_object;
  rec->next = NULL;
  env->cl_package->value_ptr.package->symtable
    [hash_char_vector ("NIL", sizeof ("NIL"), SYMTABLE_SIZE)] = rec;


  env->package_sym = intern_symbol_by_char_vector ("*PACKAGE*",
						   strlen ("*PACKAGE*"), 1,
						   EXTERNAL_VISIBILITY, 1,
						   env->cl_package, 0, 0);
  env->package_sym->value_ptr.symbol->is_parameter = 1;
  env->package_sym->value_ptr.symbol->is_special = 1;
  env->package_sym->value_ptr.symbol->value_cell = env->cl_package;


  env->gensym_counter_sym = define_variable ("*GENSYM-COUNTER*",
					     create_integer_from_long (1), env);


  env->random_state_sym =
    intern_symbol_by_char_vector ("*RANDOM-STATE*", strlen ("*RANDOM-STATE*"), 1,
				  EXTERNAL_VISIBILITY, 1, env->cl_package, 0, 0);
  env->random_state_sym->value_ptr.symbol->is_parameter = 1;
  env->random_state_sym->value_ptr.symbol->is_special = 1;
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
  add_builtin_form ("SXHASH", env, builtin_sxhash, TYPE_FUNCTION, NULL, 0);
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
  add_builtin_form ("PATHNAME", env, builtin_pathname, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAKE-PATHNAME", env, builtin_make_pathname, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("NAMESTRING", env, builtin_namestring, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("PATHNAME-NAME", env, builtin_pathname_name, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("PATHNAME-TYPE", env, builtin_pathname_type, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("WILD-PATHNAME-P", env, builtin_wild_pathname_p,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("LOGICAL-PATHNAME", env, builtin_logical_pathname,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("TRANSLATE-LOGICAL-PATHNAME", env,
		    builtin_translate_logical_pathname, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("TRUENAME", env, builtin_truename, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("PROBE-FILE", env, builtin_probe_file, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("ENSURE-DIRECTORIES-EXIST", env,
		    builtin_ensure_directories_exist, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FILE-POSITION", env, builtin_file_position, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("FILE-LENGTH", env, builtin_file_length, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("RENAME-FILE", env, builtin_rename_file, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("DELETE-FILE", env, builtin_delete_file, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("READ-LINE", env, builtin_read_line, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("READ", env, builtin_read, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("READ-PRESERVING-WHITESPACE", env,
		    builtin_read_preserving_whitespace, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("READ-FROM-STRING", env, builtin_read_from_string,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("PARSE-INTEGER", env, builtin_parse_integer, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("EVAL", env, builtin_eval, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("COMPILE", env, builtin_compile, TYPE_FUNCTION, NULL, 0);
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
  add_builtin_form ("MAKE-SYNONYM-STREAM", env, builtin_make_synonym_stream,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SYNONYM-STREAM-SYMBOL", env, builtin_synonym_stream_symbol,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MAKE-BROADCAST-STREAM", env, builtin_make_broadcast_stream,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("BROADCAST-STREAM-STREAMS", env,
		    builtin_broadcast_stream_streams, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FINISH-OUTPUT", env, builtin_finish_output, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("UPPER-CASE-P", env, builtin_upper_case_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("LOWER-CASE-P", env, builtin_lower_case_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("BOTH-CASE-P", env, builtin_both_case_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("EQ", env, builtin_eq, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("EQL", env, builtin_eql, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("EQUALP", env, builtin_equalp, TYPE_FUNCTION, NULL, 0);
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
  add_builtin_form ("RATIONAL", env, builtin_rational, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FLOAT", env, builtin_float, TYPE_FUNCTION, NULL, 0);
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
  add_builtin_form ("LOCALLY", env, evaluate_locally, TYPE_MACRO, NULL, 1);
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
  add_builtin_form ("CONSTANTP", env, builtin_constantp, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DEFPARAMETER", env, evaluate_defparameter, TYPE_MACRO, NULL,
		    0);
  add_builtin_form ("DEFVAR", env, evaluate_defvar, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFUN", env, evaluate_defun, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFMACRO", env, evaluate_defmacro, TYPE_MACRO, NULL, 0);
  add_builtin_form ("SETQ", env, evaluate_setq, TYPE_MACRO, NULL, 1);
  add_builtin_form ("PSETQ", env, evaluate_psetq, TYPE_MACRO, NULL, 0);
  add_builtin_form ("SETF", env, evaluate_setf, TYPE_MACRO, NULL, 0);
  add_builtin_form ("PSETF", env, evaluate_psetf, TYPE_MACRO, NULL, 0);
  add_builtin_form ("FUNCTION", env, evaluate_function, TYPE_MACRO, NULL, 1);
  add_builtin_form ("LAMBDA", env, evaluate_lambda, TYPE_MACRO, NULL, 0);
  add_builtin_form ("APPLY", env, evaluate_apply, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FUNCALL", env, evaluate_funcall, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DECLARE", env, evaluate_declare, TYPE_MACRO, NULL, 0);
  add_builtin_form ("THE", env, evaluate_the, TYPE_MACRO, NULL, 1);
  add_builtin_form ("AND", env, evaluate_and, TYPE_MACRO, NULL, 0);
  add_builtin_form ("OR", env, evaluate_or, TYPE_MACRO, NULL, 0);
  add_builtin_form ("PROG1", env, evaluate_prog1, TYPE_MACRO, NULL, 0);
  add_builtin_form ("PROG2", env, evaluate_prog2, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DESTRUCTURING-BIND", env, evaluate_destructuring_bind,
		    TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFTYPE", env, evaluate_deftype, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFINE-SETF-EXPANDER", env, evaluate_define_setf_expander,
		    TYPE_MACRO, NULL, 0);
  add_builtin_form ("GET-SETF-EXPANSION", env, builtin_get_setf_expansion,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DEFINE-SYMBOL-MACRO", env, evaluate_define_symbol_macro,
		    TYPE_MACRO, NULL, 0);
  add_builtin_form ("SYMBOL-MACROLET", env, evaluate_symbol_macrolet, TYPE_MACRO,
		    NULL, 1);
  add_builtin_form ("DEFSTRUCT", env, evaluate_defstruct, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DEFCLASS", env, evaluate_defclass, TYPE_MACRO, NULL, 0);
  add_builtin_form ("FIND-CLASS", env, builtin_find_class, TYPE_FUNCTION, NULL,
		    0);
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
  add_builtin_form ("ENSURE-GENERIC-FUNCTION", env,
		    builtin_ensure_generic_function, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DEFMETHOD", env, evaluate_defmethod, TYPE_MACRO, NULL, 0);
  add_builtin_form ("ADD-METHOD", env, builtin_add_method, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("FIND-METHOD", env, builtin_find_method, TYPE_FUNCTION, NULL,
		    0);
  add_builtin_form ("REMOVE-METHOD", env, builtin_remove_method, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("NEXT-METHOD-P", env, builtin_next_method_p, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("CALL-NEXT-METHOD", env, evaluate_call_next_method,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("NO-NEXT-METHOD", env, evaluate_no_next_method,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("FUNCTION-LAMBDA-EXPRESSION", env,
		    builtin_function_lambda_expression, TYPE_FUNCTION, NULL, 0);
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
  add_builtin_form ("COMPUTE-RESTARTS", env, builtin_compute_restarts,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("INVOKE-RESTART", env, builtin_invoke_restart, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("UNWIND-PROTECT", env, evaluate_unwind_protect, TYPE_MACRO,
		    NULL, 1);
  add_builtin_form ("SIGNAL", env, builtin_signal, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("ERROR", env, builtin_error, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("WARN", env, builtin_warn, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DEFINE-CONDITION", env, evaluate_define_condition,
		    TYPE_MACRO, NULL, 0);
  add_builtin_form ("MAKE-CONDITION", env, builtin_make_condition, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("DEFINE-COMPILER-MACRO", env, evaluate_define_compiler_macro,
		    TYPE_MACRO, NULL, 0);
  add_builtin_form ("ROOM", env, builtin_room, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("TRACE", env, builtin_trace, TYPE_MACRO, NULL, 0);
  add_builtin_form ("UNTRACE", env, builtin_untrace, TYPE_MACRO, NULL, 0);
  add_builtin_form ("INVOKE-DEBUGGER", env, builtin_invoke_debugger,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("STEP", env, builtin_step, TYPE_MACRO, NULL, 0);
  add_builtin_form ("TYPEP", env, builtin_typep, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("TYPE-OF", env, builtin_type_of, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("SUBTYPEP", env, builtin_subtypep, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("COERCE", env, builtin_coerce, TYPE_FUNCTION, NULL, 0);
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
		    TYPE_FUNCTION, builtin_setf_symbol_function, 0);
  add_builtin_form ("FDEFINITION", env, builtin_fdefinition, TYPE_FUNCTION,
		    builtin_setf_fdefinition, 0);
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
  add_builtin_form ("GENSYM", env, builtin_gensym, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("MACROEXPAND-1", env, builtin_macroexpand_1, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("MACRO-FUNCTION", env, builtin_macro_function, TYPE_FUNCTION,
		    builtin_setf_macro_function, 0);
  add_builtin_form ("COMPILER-MACRO-FUNCTION", env,
		    builtin_compiler_macro_function, TYPE_FUNCTION, NULL, 0);
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
  add_builtin_form ("SHADOW", env, builtin_shadow, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("PACKAGE-SHADOWING-SYMBOLS", env,
		    builtin_package_shadowing_symbols, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("DO-SYMBOLS", env, builtin_do_symbols, TYPE_MACRO, NULL, 0);
  add_builtin_form ("DO-EXTERNAL-SYMBOLS", env, builtin_do_external_symbols,
		    TYPE_MACRO, NULL, 0);
  add_builtin_form ("TIME", env, builtin_time, TYPE_MACRO, NULL, 0);
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
  add_builtin_type ("COMPILED-FUNCTION", env, type_compiled_function, 1,
		    "FUNCTION", (char *)NULL);
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
  add_builtin_type ("LOGICAL-PATHNAME", env, type_logical_pathname, 1, "PATHNAME",
		    (char *)NULL);
  add_builtin_type ("STREAM", env, type_stream, 1, (char *)NULL);
  add_builtin_type ("FILE-STREAM", env, type_file_stream, 1, "STREAM",
		    (char *)NULL);
  add_builtin_type ("STRING-STREAM", env, type_string_stream, 1, "STREAM",
		    (char *)NULL);
  add_builtin_type ("SYNONYM-STREAM", env, type_synonym_stream, 1, "STREAM",
		    (char *)NULL);
  add_builtin_type ("STANDARD-GENERIC-FUNCTION", env, type_generic_function, 1,
		    "GENERIC-FUNCTION", (char *)NULL);
  add_builtin_type ("GENERIC-FUNCTION", env, type_generic_function, 1,
		    "FUNCTION", (char *)NULL);
  add_builtin_type ("CLASS", env, type_class, 1, (char *)NULL);
  add_builtin_type ("STRUCTURE-CLASS", env, type_structure_class, 1,
		    (char *)NULL);
  add_builtin_type ("STANDARD-CLASS", env, type_standard_class, 1,
		    (char *)NULL);
  add_builtin_type ("BUILT-IN-CLASS", env, type_nil, 1, "CLASS", (char *)NULL);


  stdobjcl = alloc_object ();
  stdobjcl->type = TYPE_STANDARD_CLASS;
  stdobjcl->value_ptr.standard_class =
    malloc_and_check (sizeof (*stdobjcl->value_ptr.standard_class));
  stdobjcl->value_ptr.standard_class->is_condition_class = 0;
  stdobjcl->value_ptr.standard_class->parents = NULL;
  stdobjcl->value_ptr.standard_class->descendants = NULL;
  stdobjcl->value_ptr.standard_class->fields = NULL;

  stdobjsym = CREATE_BUILTIN_SYMBOL ("STANDARD-OBJECT");
  stdobjsym->value_ptr.symbol->is_type = 1;
  stdobjsym->value_ptr.symbol->typespec = stdobjcl;
  stdobjcl->value_ptr.standard_class->name = stdobjsym;


  add_condition_class ("PACKAGE-ERROR", env, 1, "ERROR", (char *)NULL, "PACKAGE",
		       (char *)NULL);
  add_condition_class ("UNBOUND-SLOT", env, 1, "CELL-ERROR", (char *)NULL,
		       "INSTANCE", (char *)NULL);
  add_condition_class ("UNDEFINED-FUNCTION", env, 1, "CELL-ERROR", (char *)NULL,
		       (char *)NULL);
  add_condition_class ("UNBOUND-VARIABLE", env, 1, "CELL-ERROR", (char *)NULL,
		       (char *)NULL);
  add_condition_class ("CELL-ERROR", env, 1, "ERROR", (char *)NULL, "NAME",
		       (char *)NULL);
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
  add_condition_class ("PROGRAM-ERROR", env, 1, "ERROR", (char *)NULL,
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
  add_condition_class ("SIMPLE-WARNING", env, 1, "SIMPLE-CONDITION", "WARNING",
		       (char *)NULL, (char *)NULL);
  add_condition_class ("SIMPLE-ERROR", env, 1, "SIMPLE-CONDITION", "ERROR",
		       (char *)NULL, (char *)NULL);
  add_condition_class ("STYLE-WARNING", env, 1, "WARNING", (char *)NULL,
		       (char *)NULL);
  add_condition_class ("WARNING", env, 1, "CONDITION", (char *)NULL,
		       (char *)NULL);
  add_condition_class ("CONDITION", env, 1, (char *)NULL, (char *)NULL);


  add_builtin_type ("RESTART", env, type_restart, 1, (char *)NULL);


  env->amp_optional_sym = intern_symbol_by_char_vector ("&OPTIONAL",
							strlen ("&OPTIONAL"),
							1, EXTERNAL_VISIBILITY,
							1, env->cl_package, 0, 0);
  env->amp_rest_sym = intern_symbol_by_char_vector ("&REST", strlen ("&REST"),
						    1, EXTERNAL_VISIBILITY, 1,
						    env->cl_package, 0, 0);
  env->amp_body_sym = intern_symbol_by_char_vector ("&BODY", strlen ("&BODY"),
						    1, EXTERNAL_VISIBILITY, 1,
						    env->cl_package, 0, 0);
  env->amp_key_sym = intern_symbol_by_char_vector ("&KEY", strlen ("&KEY"), 1,
						   EXTERNAL_VISIBILITY, 1,
						   env->cl_package, 0, 0);
  env->amp_allow_other_keys_sym =
    intern_symbol_by_char_vector ("&ALLOW-OTHER-KEYS",
				  strlen ("&ALLOW-OTHER-KEYS"), 1,
				  EXTERNAL_VISIBILITY, 1, env->cl_package, 0, 0);
  env->amp_aux_sym = intern_symbol_by_char_vector ("&AUX",
						   strlen ("&AUX"), 1,
						   EXTERNAL_VISIBILITY, 1,
						   env->cl_package, 0, 0);
  env->amp_whole_sym = intern_symbol_by_char_vector ("&WHOLE",
						     strlen ("&WHOLE"), 1,
						     EXTERNAL_VISIBILITY, 1,
						     env->cl_package, 0, 0);
  env->key_allow_other_keys_sym =
    intern_symbol_by_char_vector ("ALLOW-OTHER-KEYS",
				  strlen ("ALLOW-OTHER-KEYS"), 1,
				  EXTERNAL_VISIBILITY, 1, env->keyword_package,
				  1, 0);

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

  env->err_out_sym = define_variable ("*ERROR-OUTPUT*", env->c_stdout, env);
  add_reference (env->err_out_sym, env->c_stdout, 0);

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
  env->dynamic_extent_sym = CREATE_BUILTIN_SYMBOL ("DYNAMIC-EXTENT");

  env->print_escape_sym = define_variable ("*PRINT-ESCAPE*", &t_object, env);
  env->print_readably_sym = define_variable ("*PRINT-READABLY*", &nil_object,
					     env);
  env->print_base_sym = define_variable ("*PRINT-BASE*",
					 create_integer_from_long (10), env);
  env->print_radix_sym = define_variable ("*PRINT-RADIX*", &nil_object, env);
  env->print_array_sym = define_variable ("*PRINT-ARRAY*", &t_object, env);
  env->print_gensym_sym = define_variable ("*PRINT-GENSYM*", &t_object, env);
  env->print_pretty_sym = define_variable ("*PRINT-PRETTY*", &nil_object, env);
  env->print_pprint_dispatch_sym = define_variable ("*PRINT-PPRINT-DISPATCH*",
						    &nil_object, env);

  env->read_eval_sym = define_variable ("*READ-EVAL*", &t_object, env);
  env->read_base_sym = define_variable ("*READ-BASE*",
					create_integer_from_long (10), env);
  env->read_suppress_sym = define_variable ("*READ-SUPPRESS*", &nil_object, env);

  define_variable ("*LOAD-PRINT*", &nil_object, env);
  define_variable ("*LOAD-VERBOSE*", &nil_object, env);

  env->load_pathname_sym = define_variable ("*LOAD-PATHNAME*", &nil_object, env);
  env->load_truename_sym = define_variable ("*LOAD-TRUENAME*", &nil_object, env);

  env->break_on_signals_sym = define_variable ("*BREAK-ON-SIGNALS*", &nil_object,
					       env);

  define_variable ("*FEATURES*", &nil_object, env);

  env->abort_sym = CREATE_BUILTIN_SYMBOL ("ABORT");


  define_generic_function ("PRINT-OBJECT", env,
			   create_lambda_list (env, "OBJECT", "STREAM",
					       (char *)NULL),
			   builtin_method_print_object);

  lambdal = create_lambda_list (env, "CLASS", (char *)NULL);
  lambdal->next = alloc_parameter (REST_PARAM, NULL);
  lambdal->next->name = intern_symbol_by_char_vector ("INITARGS",
						      strlen ("INITARGS"), 1,
						      INTERNAL_VISIBILITY, 0,
						      env->cl_package, 0, 0);
  lambdal->next->reference_strength_factor
    = !STRENGTH_FACTOR_OF_OBJECT (lambdal->next->name);
  INC_WEAK_REFCOUNT (lambdal->next->name);

  define_generic_function ("MAKE-INSTANCE", env, lambdal,
			   builtin_method_make_instance);

  define_generic_function ("ALLOCATE-INSTANCE", env,
			   copy_lambda_list (lambdal, 0),
			   builtin_method_allocate_instance);

  define_generic_function ("INITIALIZE-INSTANCE", env,
			   copy_lambda_list (lambdal, 0),
			   builtin_method_initialize_instance);

  define_generic_function ("REINITIALIZE-INSTANCE", env,
			   copy_lambda_list (lambdal, 0),
			   builtin_method_reinitialize_instance);

  lambdal = create_lambda_list (env, "INSTANCE", "NEW-CLASS", (char *)NULL);
  lambdal->next->next = alloc_parameter (REST_PARAM, NULL);
  lambdal->next->next->name =
    intern_symbol_by_char_vector ("INITARGS", strlen ("INITARGS"), 1,
				  INTERNAL_VISIBILITY, 0, env->cl_package, 0, 0);
  lambdal->next->next->reference_strength_factor
    = !STRENGTH_FACTOR_OF_OBJECT (lambdal->next->next->name);
  INC_WEAK_REFCOUNT (lambdal->next->next->name);

  define_generic_function ("CHANGE-CLASS", env, lambdal,
			   builtin_method_change_class);


  env->package_sym->value_ptr.symbol->value_cell = env->cluser_package;

  add_builtin_form ("AL-LOOPY-DESTRUCTURING-BIND", env,
		    evaluate_al_loopy_destructuring_bind, TYPE_MACRO, NULL, 0);
  add_builtin_form ("AL-LOOPY-SETQ", env, evaluate_al_loopy_setq, TYPE_MACRO,
		    NULL, 0);

  add_builtin_form ("AL-STRING-INPUT-STREAM-STRING", env,
		    builtin_al_string_input_stream_string, TYPE_FUNCTION, NULL,
		    0);

  add_builtin_form ("AL-PRINT-RESTARTS", env, builtin_al_print_restarts,
		    TYPE_FUNCTION, NULL, 0);

  add_builtin_form ("AL-DUMP-BINDINGS", env, builtin_al_dump_bindings,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-DUMP-FUNCTION-BINDINGS", env,
		    builtin_al_dump_function_bindings, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-DUMP-CAPTURED-ENV", env, builtin_al_dump_captured_env,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-DUMP-METHODS", env, builtin_al_dump_methods,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-DUMP-FIELDS", env, builtin_al_dump_fields, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("AL-CLASS-PRECEDENCE-LIST", env,
		    builtin_al_class_precedence_list, TYPE_FUNCTION, NULL, 0);

  add_builtin_form ("AL-START-PROFILING", env, builtin_al_start_profiling,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-STOP-PROFILING", env, builtin_al_stop_profiling,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-CLEAR-PROFILING", env, builtin_al_clear_profiling,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-REPORT-PROFILING", env, builtin_al_report_profiling,
		    TYPE_FUNCTION, NULL, 0);

  add_builtin_form ("AL-PRINT-BACKTRACE", env, builtin_al_print_backtrace,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-LIST-BACKTRACE", env, builtin_al_list_backtrace,
		    TYPE_FUNCTION, NULL, 0);

  add_builtin_form ("AL-WATCH", env, builtin_al_watch, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-UNWATCH", env, builtin_al_unwatch, TYPE_FUNCTION, NULL,
		    0);

  add_builtin_form ("AL-NEXT", env, builtin_al_next, TYPE_FUNCTION, NULL, 0);

  add_builtin_form ("AL-COMPILE-FORM", env, builtin_al_compile_form,
		    TYPE_FUNCTION, NULL, 0);

  add_builtin_form ("AL-PRINT-NO-WARRANTY", env, builtin_al_print_no_warranty,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-PRINT-TERMS-AND-CONDITIONS", env,
		    builtin_al_print_terms_and_conditions, TYPE_FUNCTION, NULL,
		    0);

  add_builtin_form ("AL-PATHNAME-DIRECTORY", env, builtin_al_pathname_directory,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-LIST-DIRECTORY", env, builtin_al_list_directory,
		    TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-DIRECTORYP", env, builtin_al_directoryp, TYPE_FUNCTION,
		    NULL, 0);
  add_builtin_form ("AL-GETCWD", env, builtin_al_getcwd, TYPE_FUNCTION, NULL, 0);

  add_builtin_form ("AL-GETENV", env, builtin_al_getenv, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-SYSTEM", env, builtin_al_system, TYPE_FUNCTION, NULL, 0);
  add_builtin_form ("AL-EXIT", env, builtin_al_exit, TYPE_FUNCTION, NULL, 0);

  env->al_compile_when_defining_sym =
    define_variable ("*AL-COMPILE-WHEN-DEFINING*", &nil_object, env);
  env->al_debugging_condition_sym =
    define_variable ("*AL-DEBUGGING-CONDITION*", &nil_object, env);

  define_variable ("*AL-PPRINT-DEPTH*", create_integer_from_long (0), env);

  env->al_print_always_two_colons =
    define_variable ("*AL-PRINT-ALWAYS-TWO-COLONS*", &nil_object, env);


  add_builtin_type ("AL-BACKQUOTE", env, type_al_backquote, 1, (char *)NULL);
  add_builtin_type ("AL-COMMA", env, type_al_comma, 1, (char *)NULL);
  add_builtin_type ("AL-AT", env, type_al_at, 1, (char *)NULL);
  add_builtin_type ("AL-DOT", env, type_al_dot, 1, (char *)NULL);

  add_condition_class ("AL-MAXIMUM-STACK-DEPTH-EXCEEDED", env, 1, "PROGRAM-ERROR",
		       (char *)NULL, "MAX-DEPTH", (char *)NULL);
  add_condition_class ("AL-WRONG-NUMBER-OF-ARGUMENTS", env, 1, "PROGRAM-ERROR",
		       (char *)NULL, "MAX-ARGS", "MIN-ARGS", (char *)NULL);

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
  size_t s = pack->nicks ? pack->nicks->name_len : pack->name_len, ts;
  char *ret;
  int i;

  if (env->debugging_depth)
    {
      ts = s + 9 + env->debugging_depth-1;
    }
  else
    {
      ts = s + 5;
    }

  ret = malloc_and_check (ts);

  ret [0] = '[';

  memcpy (ret+1, pack->nicks ? pack->nicks->name : pack->name, s);
  i = s+1;

  ret [i++] = ']';

  if (env->debugging_depth)
    {
      memcpy (ret+i, " dbg", 4);
      i += 4;

      for (; i < ts-2; i++)
	ret [i] = '>';

      ret [ts-2] = ' ';
      ret [ts-1] = 0;
    }
  else
    {
      memcpy (ret+i, "> ", 2);
      ret [i+2] = 0;
    }

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
    *skip = inspect_variable (env->read_suppress_sym, env), *obs;
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
	{
	  token = accumulate_token (stream, preserve_whitespace, &tokensize,
				    &tokenlength, outcome);

	  if (IS_READ_OR_EVAL_ERROR (outcome->type))
	    {
	      free (token);
	      return outcome->type;
	    }

	  if (outcome->single_escape || outcome->multiple_escape)
	    {
	      free (token);
	      CLEAR_READER_STATUS (*outcome);
	      return GOT_EOF_IN_MIDDLE_OF_OBJECT;
	    }
	}
      else
	{
	  tokenlength = size;
	}

      out = read_symbol_name (&ob, input ? input : token, tokenlength,
			      input == NULL, preserve_whitespace, obj_end,
			      CASE_UPCASE, outcome);

      if (!input)
	free (token);

      if (out == COMPLETE_OBJECT && !(obs = intern_symbol_name (ob, env, &out)))
	{
	  increment_refcount (ob);
	  outcome->obj = ob;
	  return out;
	}
      else if (out == COMPLETE_OBJECT)
	ob = obs;
    }
  else if (ob->type == TYPE_SHARP_MACRO_CALL)
    {
      out = read_sharp_macro_call (&ob, input, size, stream,
				   backts_commas_balance, preserve_whitespace,
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

  fresh_line (env->c_stdout->value_ptr.stream);
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
      fresh_line (env->c_stdout->value_ptr.stream);
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
      clear_read_labels (&env->read_labels);

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
      clear_read_labels (&env->read_labels);

      return NULL;
    }
  else if (IS_READ_OR_EVAL_ERROR (read_out))
    {
      outcome->type = read_out;
      print_error (outcome, env);
      clear_read_labels (&env->read_labels);

      return NULL;
    }
  else
    {
      outcome->type = read_out;

      free (*wholeline);

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

      clear_read_labels (&env->read_labels);
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

  fresh_line (env->c_stdout->value_ptr.stream);
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
  struct object *last_pref, *ob = NULL, *call, *obs,
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
					   backts_commas_balance,
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

	  if (IS_READ_OR_EVAL_ERROR (out))
	    return out;

	  if (input)
	    {
	      size = size - (*obj_end - input);
	      input = *obj_end;
	    }

	  if (out == UNCLOSED_EMPTY_LIST)
	    {
	      SET_EMPTY_LIST_IN_CAR (ob->value_ptr.cons_pair->cdr);
	      out = UNCLOSED_NONEMPTY_LIST;
	    }
	  else if (out != COMPLETE_OBJECT)
	    {
	      SET_FILLING_CAR (ob->value_ptr.cons_pair->cdr);
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
	    {
	      token = accumulate_token (stream, preserve_whitespace, &tokensize,
					&tokenlength, outcome);

	      if (IS_READ_OR_EVAL_ERROR (outcome->type))
		{
		  free (token);
		  return outcome->type;
		}

	      if (outcome->single_escape || outcome->multiple_escape)
		{
		  free (token);
		  CLEAR_READER_STATUS (*outcome);
		  return GOT_EOF_IN_MIDDLE_OF_OBJECT;
		}
	    }
	  else
	    {
	      tokenlength = size;
	    }

	  numbase = get_read_base (env);

	  if (is_number (input ? input : token, tokenlength, numbase, &numtype,
			 &num_end, &exp_mark_pos, obj_end))
	    {
	      ob = create_number (input ? input : token, num_end
				  - (input ? input : token) + 1,
				  exp_mark_pos, numbase, numtype);
	      out = COMPLETE_OBJECT;

	      if (!input)
		free (token);
	    }
	  else
	    {
	      out = read_symbol_name (&ob, input ? input : token, tokenlength,
				      input == NULL || ends_with_eof,
				      preserve_whitespace, obj_end, CASE_UPCASE,
				      outcome);

	      if (!input)
		free (token);

	      if (out == COMPLETE_OBJECT &&
		  !(obs = intern_symbol_name (ob, env, &out)))
		{
		  increment_refcount (ob);
		  outcome->obj = ob;
		  return out;
		}
	      else if (out == COMPLETE_OBJECT)
		ob = obs;
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
      if (FOUND_DOT (ob))
	found_dot = 1;

      if (FILLING_CAR (ob) || FILLING_CDR (ob))
	{
	  out = read_object_continued (FILLING_CAR (ob)
				       ? &ob->value_ptr.cons_pair->car
				       : &ob->value_ptr.cons_pair->cdr,
				       backts_commas_balance, 0, input, size,
				       stream, preserve_whitespace,
				       ends_with_eof, env, outcome, &obj_beg,
				       &obj_end);

	  if (out == COMPLETE_OBJECT)
	    {
	      add_reference_to_object_just_read (ob, FILLING_CAR (ob)
						 ? ob->value_ptr.cons_pair->car
						 : ob->value_ptr.cons_pair->cdr,
						 FILLING_CAR (ob) ? 0 : 1);
	      CLEAR_FILLING_CAR (ob);
	      CLEAR_FILLING_CDR (ob);
	    }
	  else if (out == UNCLOSED_EMPTY_LIST)
	    {
	      FILLING_CAR (ob) ? SET_EMPTY_LIST_IN_CAR (ob)
		: SET_EMPTY_LIST_IN_CDR (ob);

	      CLEAR_FILLING_CAR (ob);
	      CLEAR_FILLING_CDR (ob);
	    }

	  if (IS_INCOMPLETE_OBJECT (out) || IS_READ_OR_EVAL_ERROR (out))
	    return out;
	}
      else if (EMPTY_LIST_IN_CAR (ob) || EMPTY_LIST_IN_CDR (ob))
	{
	  out = read_object_continued (EMPTY_LIST_IN_CAR (ob)
				       ? &ob->value_ptr.cons_pair->car
				       : &ob->value_ptr.cons_pair->cdr,
				       backts_commas_balance, 1, input, size,
				       stream, preserve_whitespace,
				       ends_with_eof, env, outcome, &obj_beg,
				       &obj_end);

	  if (out == COMPLETE_OBJECT)
	    {
	      add_reference_to_object_just_read (ob, EMPTY_LIST_IN_CAR (ob)
						 ? ob->value_ptr.cons_pair->car
						 : ob->value_ptr.cons_pair->cdr,
						 EMPTY_LIST_IN_CAR (ob) ? 0 : 1);
	      CLEAR_EMPTY_LIST_IN_CAR (ob);
	      CLEAR_EMPTY_LIST_IN_CDR (ob);
	    }

	  if (IS_INCOMPLETE_OBJECT (out) && out != UNCLOSED_EMPTY_LIST)
	    {
	      EMPTY_LIST_IN_CAR (ob) ? SET_FILLING_CAR (ob)
		: SET_FILLING_CDR (ob);

	      CLEAR_EMPTY_LIST_IN_CAR (ob);
	      CLEAR_EMPTY_LIST_IN_CDR (ob);
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


  while (out != NO_OBJECT && out != UNCLOSED_EMPTY_LIST)
    {
      if (out == CLOSING_PARENTHESIS)
	{
	  if (!last_cons)
	    {
	      *list_end = obj_end;
	      *obj = &nil_object;
	      return COMPLETE_OBJECT;
	    }

	  if (FOUND_DOT (last_cons) && !last_cons->value_ptr.cons_pair->cdr)
	    return NO_OBJ_AFTER_DOT_IN_LIST;

	  if (!FOUND_DOT (last_cons))
	    last_cons->value_ptr.cons_pair->cdr = &nil_object;

	  *list_end = obj_end;
	  return COMPLETE_OBJECT;
	}
      else if (out == SINGLE_DOT)
	{
	  if (!last_cons)
	    return NO_OBJ_BEFORE_DOT_IN_LIST;

	  if (FOUND_DOT (last_cons))
	    return MORE_THAN_A_CONSING_DOT;

	  SET_FOUND_DOT (last_cons);
	}
      else if (IS_READ_OR_EVAL_ERROR (out))
	{
	  return out;
	}
      else if (out == COMPLETE_OBJECT || IS_INCOMPLETE_OBJECT (out))
	{
	  if (last_cons && FOUND_DOT (last_cons)
	      && last_cons->value_ptr.cons_pair->cdr
	      && !FILLING_CDR (last_cons))
	    return MULTIPLE_OBJS_AFTER_DOT_IN_LIST;
	  else if (last_cons && FOUND_DOT (last_cons))
	    {
	      last_cons->value_ptr.cons_pair->cdr = car;

	      if (IS_INCOMPLETE_OBJECT (out))
		{
		  SET_FILLING_CDR (last_cons);

		  return UNCLOSED_NONEMPTY_LIST;
		}

	      add_reference_to_object_just_read (last_cons, car, 1);
	    }
	  else
	    {
	      cons = alloc_empty_cons_pair ();
	      cons->value_ptr.cons_pair->car = car;

	      if (last_cons)
		last_cons = last_cons->value_ptr.cons_pair->cdr = cons;
	      else
		{
		  *obj = last_cons = cons;

		  if (env->curr_label >= 0)
		    {
		      env->read_labels = add_read_label (env->curr_label,
							 last_cons,
							 env->read_labels);
		      env->curr_label = -1;
		    }
		}

	      if (IS_INCOMPLETE_OBJECT (out))
		{
		  SET_FILLING_CAR (cons);

		  return UNCLOSED_NONEMPTY_LIST;
		}

	      add_reference_to_object_just_read (cons, car, 0);
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
      if (last_cons && FOUND_DOT (last_cons))
	{
	  last_cons->value_ptr.cons_pair->cdr = car;
	  SET_EMPTY_LIST_IN_CDR (last_cons);
	}
      else
	{
	  cons = alloc_empty_cons_pair ();
	  cons->value_ptr.cons_pair->car = car;
	  SET_EMPTY_LIST_IN_CAR (cons);

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
  enum package_record_flags visib;
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
					 : start_of_pack_sep + 2, size -
					 (start_of_pack_sep-input) -
					 (visib == EXTERNAL_VISIBILITY ? 1 : 2),
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

      if (ch == ',' && !(*backts_commas_balance))
	return TOO_MANY_COMMAS;

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
		       FILE *stream, int backts_commas_balance,
		       int preserve_whitespace, int ends_with_eof,
		       struct environment *env, struct outcome *outcome,
		       const char **macro_end)
{
  int arg, tokenlength, tokensize, base;
  const char *obj_b, *num_e;
  char *token;
  struct object *prevpack, *obs;
  struct sharp_macro_call *call;
  struct read_label *lb;
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
	  set_value (env->package_sym, env->keyword_package, 0, 0, env, outcome);
	  out = read_object_continued (&call->feature_test,
				       backts_commas_balance,
				       call->feat_test_is_empty_list, input,
				       size, stream, preserve_whitespace,
				       ends_with_eof, env, outcome, &obj_b,
				       macro_end);
	  set_value (env->package_sym, prevpack, 0, 0, env, outcome);

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

	  set_value (env->package_sym, NULL, 0, 0, env, outcome);

	  call->obj = NULL;
	  out = read_object_continued (&call->obj, 0, call->is_empty_list, input,
				       size, stream, preserve_whitespace,
				       ends_with_eof, env, outcome, &obj_b,
				       macro_end);

	  set_value (env->package_sym, prevpack, 0, 0, env, outcome);

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

      /*if (call->dispatch_ch == '\\'
	  && call->obj->value_ptr.symbol_name->packname_present)
	{
	  return WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	  }*/

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

  if (!strchr ("'\\.pP(aA:cC+-*sSbBoOxXrR=#", call->dispatch_ch))
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

	  if (outcome->single_escape || outcome->multiple_escape)
	    {
	      free (token);
	      CLEAR_READER_STATUS (*outcome);
	      return GOT_EOF_IN_MIDDLE_OF_OBJECT;
	    }
	}
      else
	{
	  tokenlength = size;
	}

      outcome->single_escape = 1;
      out = read_symbol_name (&call->obj, input ? input : token, tokenlength,
			      input == NULL, preserve_whitespace, macro_end,
			      CASE_UPCASE, outcome);

      if (!input)
	free (token);

      if (out == COMPLETE_OBJECT && !(obs = intern_symbol_name (call->obj, env,
								&out)))
	{
	  increment_refcount (call->obj);
	  outcome->obj = call->obj;
	  return out;
	}
      else if (out == COMPLETE_OBJECT)
	call->obj = obs;

      /*if (call->obj->value_ptr.symbol_name->packname_present)
	{
	  return WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	  }*/

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

      set_value (env->package_sym, env->keyword_package, 0, 0, env, outcome);

      call->feature_test = NULL;
      out = read_object (&call->feature_test, 0, input, size, stream,
			 preserve_whitespace, ends_with_eof, env, outcome,
			 &obj_b, macro_end);

      set_value (env->package_sym, prevpack, 0, 0, env, outcome);

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
	  out = read_object (&call->obj, backts_commas_balance, input, size,
			     stream, preserve_whitespace, ends_with_eof, env,
			     outcome, &obj_b, macro_end);

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
	  if (arg == -1)
	    {
	      return SHARP_MACRO_REQUIRES_INTEGER_ARGUMENT;
	    }
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

      set_value (env->package_sym, NULL, 0, 0, env, outcome);

      call->obj = NULL;
      out = read_object (&call->obj, 0, input, size, stream, preserve_whitespace,
			 ends_with_eof, env, outcome, &obj_b, macro_end);

      set_value (env->package_sym, prevpack, 0, 0, env, outcome);

      if (out == UNCLOSED_EMPTY_LIST)
	call->is_empty_list = 1;

      if (IS_INCOMPLETE_OBJECT (out))
	return INCOMPLETE_SHARP_MACRO_CALL;

      return out;
    }
  else if (call->dispatch_ch == '=')
    {
      if (arg == -1)
	{
	  return SHARP_MACRO_REQUIRES_INTEGER_ARGUMENT;
	}

      lb = env->read_labels;

      while (lb)
	{
	  if (call->arg == lb->label)
	    {
	      return CANNOT_USE_READ_LABEL_TWICE;
	    }

	  lb = lb->next;
	}

      env->curr_label = arg;

      call->obj = NULL;
      out = read_object (&call->obj, backts_commas_balance, input, size, stream,
			 preserve_whitespace, ends_with_eof, env, outcome, &obj_b,
			 macro_end);

      if (out == UNCLOSED_EMPTY_LIST)
	call->is_empty_list = 1;

      if (IS_INCOMPLETE_OBJECT (out))
	return INCOMPLETE_SHARP_MACRO_CALL;

      return out;
    }
  else if (call->dispatch_ch == '#')
    {
      if (arg == -1)
	{
	  return SHARP_MACRO_REQUIRES_INTEGER_ARGUMENT;
	}

      if (input)
	{
	  *macro_end = input-1;
	}

      return COMPLETE_OBJECT;
    }

  call->obj = NULL;
  out = read_object (&call->obj, backts_commas_balance, input, size, stream,
		     preserve_whitespace, ends_with_eof, env, outcome, &obj_b,
		     macro_end);

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
  struct symbol *s;
  struct read_label *lb;
  int l, ch;

  if (macro_call->dispatch_ch == '\'')
    {
      ret = alloc_empty_cons_pair ();

      SET_READER_MACRO_FLAG (ret);

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
      if (obj->type != TYPE_SYMBOL)
	{
	  outcome->type = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

	  return NULL;
	}

      s = obj->value_ptr.symbol;

      if (char_vector_utf8_length (s->name, s->name_len) == 1)
	{
	  return create_character_from_utf8 (s->name, s->name_len);
	}

      ch = s->name [0];
      s->name [0] = toupper ((unsigned char)s->name [0]);

      if (eqmem (s->name, s->name_len, "NEWLINE", strlen ("NEWLINE")))
	ret = create_character ("\n", 1);
      else if (eqmem (s->name, s->name_len, "SPACE", strlen ("SPACE")))
	ret = create_character (" ", 1);
      else if (eqmem (s->name, s->name_len, "TAB", strlen ("TAB")))
	ret = create_character ("\t", 1);
      else if (eqmem (s->name, s->name_len, "BACKSPACE", strlen ("BACKSPACE")))
	ret = create_character ("\b", 1);
      else if (eqmem (s->name, s->name_len, "PAGE", strlen ("PAGE")))
	ret = create_character ("\f", 1);
      else if (eqmem (s->name, s->name_len, "RETURN", strlen ("RETURN")))
	ret = create_character ("\r", 1);
      else if (eqmem (s->name, s->name_len, "LINEFEED", strlen ("LINEFEED")))
	ret = create_character ("\n", 1);
      else
	{
	  s->name [0] = ch;
	  outcome->type = UNKNOWN_CHARACTER_NAME;

	  return NULL;
	}

      s->name [0] = ch;
      return ret;
    }
  else if (macro_call->dispatch_ch == '.')
    {
      if (SYMBOL (inspect_variable (env->read_eval_sym, env)) == &nil_object)
	{
	  outcome->type = READ_EVAL_IS_DISABLED;
	  return NULL;
	}

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
    {
      return create_vector_from_list (obj, macro_call->arg);
    }
  else if (macro_call->dispatch_ch == 'a' || macro_call->dispatch_ch == 'A')
    {
      if (!IS_SEQUENCE (obj))
	{
	  outcome->type = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	  return NULL;
	}

      ret = create_array_from_sequence (obj, macro_call->arg);

      if (!ret)
	outcome->type = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;

      return ret;
    }
  else if (macro_call->dispatch_ch == ':')
    {
      if (!obj)
	{
	  outcome->type = PACKAGE_MARKER_IN_SHARP_COLON;

	  return NULL;
	}

      if (!IS_SYMBOL (obj))
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
  else if (macro_call->dispatch_ch == 's' || macro_call->dispatch_ch == 'S')
    {
      if (obj->type != TYPE_CONS_PAIR || !IS_SYMBOL (CAR (obj))
	  || !SYMBOL (CAR (obj))->value_ptr.symbol->typespec
	  || SYMBOL (CAR (obj))->value_ptr.symbol->typespec->type
	  != TYPE_STRUCTURE_CLASS)
	{
	  outcome->type = WRONG_OBJECT_TYPE_TO_SHARP_MACRO;
	  return NULL;
	}

      return call_structure_constructor (SYMBOL (CAR (obj)), CDR (obj), env,
					 outcome);
    }
  else if (strchr ("*bBoOxXrR", macro_call->dispatch_ch))
    {
      increment_refcount (obj);
      return obj;
    }
  else if (macro_call->dispatch_ch == '=')
    {
      if (env->curr_label >= 0)
	{
	  env->read_labels = add_read_label (macro_call->arg, obj,
					     env->read_labels);
	  env->curr_label = -1;
	}

      increment_refcount (obj);
      return obj;
    }
  else if (macro_call->dispatch_ch == '#')
    {
      lb = env->read_labels;

      while (lb)
	{
	  if (macro_call->arg == lb->label)
	    {
	      increment_refcount (lb->value);
	      return lb->value;
	    }

	  lb = lb->next;
	}

      outcome->type = READ_LABEL_NOT_DEFINED;
      return NULL;
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

	      if (out == COMPLETE_OBJECT)
		{
		  if (input)
		    {
		      input = *obj_end+1;
		      size = (input + size) - *obj_end - 1;
		    }
		}
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

	      if (out == COMPLETE_OBJECT)
		{
		  if (input)
		    {
		      input = *obj_end+1;
		      size = (input + size) - *obj_end - 1;
		    }
		}
	    }
	} while (out == NO_OBJECT ||
		 (!IS_READ_OR_EVAL_ERROR (out) && !IS_INCOMPLETE_OBJECT (out)
		  && *list_depth));

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
	      else
		unget_char (ch, &input, &size, stream);

	      return COMPLETE_OBJECT;
	    }
	  else
	    outc->single_escape = 0;
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
  int ch, onlydots = 1;
  char *outbuf;

  *token_size = 16;
  outbuf = malloc_and_check (*token_size);
  *token_length = 0;

  out->type = NO_OBJECT;

  while ((ch = fgetc (stream)) != EOF)
    {
      if (ch == '\\')
	{
	  out->single_escape = !out->single_escape;
	  onlydots = 0;
	}
      else if (ch == '|' && !out->single_escape)
	{
	  out->multiple_escape = !out->multiple_escape;
	  onlydots = 0;
	}
      else
	{
	  if ((isspace ((unsigned char)ch)
	       || strchr (TERMINATING_MACRO_CHARS, ch))
	      && !out->single_escape && !out->multiple_escape)
	    {
	      if (preserve_whitespace || strchr (TERMINATING_MACRO_CHARS, ch))
		ungetc (ch, stream);

	      if (onlydots && *token_length == 1)
		out->type = SINGLE_DOT;
	      else if (onlydots)
		out->type = MULTIPLE_DOTS;

	      return outbuf;
	    }

	  if (ch != '.' || out->single_escape || out->multiple_escape)
	    onlydots = 0;

	  out->single_escape = 0;
	}

      if (*token_length == *token_size)
	{
	  *token_size <<= 1;
	  outbuf = realloc_and_check (outbuf, *token_size);
	}

      outbuf [(*token_length)++] = ch;
    }

  if (onlydots && *token_length == 1)
    out->type = SINGLE_DOT;
  else if (onlydots)
    out->type = MULTIPLE_DOTS;

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

  num_numbers++;

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

  num_numbers++;

  return obj;
}


struct object *
alloc_complex (void)
{
  struct object *ret = alloc_object ();

  ret->type = TYPE_COMPLEX;
  ret->value_ptr.complex = malloc_and_check (sizeof (*ret->value_ptr.complex));
  ret->value_ptr.complex->real = NULL;
  ret->value_ptr.complex->imag = NULL;

  num_numbers++;

  return ret;
}


struct object *
create_complex (struct object *real, struct object *imag, int decrement_refc,
		struct environment *env, struct outcome *outcome)
{
  struct object *ret, *r, *i;
  enum object_type t;

  if (!IS_REAL (real))
    {
      return raise_type_error (real, "CL:REAL", env, outcome);
    }

  if (imag && !IS_REAL (imag))
    {
      return raise_type_error (imag, "CL:REAL", env, outcome);
    }

  if (IS_RATIONAL (real) && (!imag || ((imag->type == TYPE_INTEGER
					&& !mpz_sgn (imag->value_ptr.integer))
				       || (imag->type == TYPE_RATIO
					   && !mpq_sgn (imag->value_ptr.ratio)))))
    {
      increment_refcount (real);

      if (decrement_refc)
	{
	  decrement_refcount (real);
	  decrement_refcount (imag);
	}

      return real;
    }

  if (!imag)
    {
      ret = alloc_complex ();

      add_reference (ret, real, 0);

      if (decrement_refc)
	decrement_refcount (real);

      ret->value_ptr.complex->real = real;
      ret->value_ptr.complex->imag = create_floating_from_double (0.0);

      add_reference (ret, ret->value_ptr.complex->imag, 1);
      decrement_refcount (ret->value_ptr.complex->imag);

      return ret;
    }

  t = highest_num_type (real->type, imag->type);
  r = promote_number (real, t);
  i = promote_number (imag, t);

  if (decrement_refc)
    {
      decrement_refcount (real);
      decrement_refcount (imag);
    }

  ret = alloc_complex ();
  ret->value_ptr.complex->real = convert_to_integer_if_possible (r);
  ret->value_ptr.complex->imag = convert_to_integer_if_possible (i);

  add_reference (ret, ret->value_ptr.complex->real, 0);
  decrement_refcount (ret->value_ptr.complex->real);

  add_reference (ret, ret->value_ptr.complex->imag, 1);
  decrement_refcount (ret->value_ptr.complex->imag);

  return ret;
}


struct object *
create_integer_from_long (long num)
{
  struct object *obj = alloc_object ();

  obj->type = TYPE_INTEGER;

  mpz_init (obj->value_ptr.integer);
  mpz_set_si (obj->value_ptr.integer, num);

  num_numbers++;

  return obj;
}


struct object *
create_integer_from_unsigned_long (unsigned long num)
{
  struct object *obj = alloc_object ();

  obj->type = TYPE_INTEGER;

  mpz_init (obj->value_ptr.integer);
  mpz_set_ui (obj->value_ptr.integer, num);

  num_numbers++;

  return obj;
}


struct object *
convert_to_integer_if_possible (struct object *rat)
{
  mpz_t num, den;

  if (rat->type != TYPE_RATIO)
    return rat;

  mpz_init (den);
  mpq_get_den (den, rat->value_ptr.ratio);

  if (!mpz_cmp (den, integer_one))
    {
      mpz_init (num);
      mpz_set (num, mpq_numref (rat->value_ptr.ratio));
      mpq_clear (rat->value_ptr.ratio);

      rat->type = TYPE_INTEGER;
      mpz_init (rat->value_ptr.integer);
      mpz_set (rat->value_ptr.integer, num);

      mpz_clear (num);
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

  num_numbers++;

  return obj;
}


struct object *
create_floating_from_double (double d)
{
  struct object *obj = alloc_object ();

  obj->type = TYPE_FLOAT;

  obj->value_ptr.floating = malloc_and_check (sizeof (*obj->value_ptr.floating));
  *obj->value_ptr.floating = d;

  num_numbers++;

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
  obj->mark = 0;

  num_objects++;

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

  cons->car = NULL;
  cons->cdr = NULL;

  obj->type = TYPE_CONS_PAIR;
  obj->value_ptr.cons_pair = cons;

  num_conses++;

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
  fun->encl_blocks = NULL;
  fun->encl_tags = NULL;
  fun->body = NULL;
  fun->flags = 0;
  fun->methods = NULL;
  fun->builtin_form = NULL;
  fun->struct_constructor_class_name = NULL;
  fun->struct_accessor_class_name = NULL;
  fun->struct_predicate_class_name = NULL;
  fun->struct_copyier_class_name = NULL;
  fun->condition_reader_class_name = NULL;
  fun->function_macro = NULL;
  fun->macro_function = NULL;

  obj->type = TYPE_FUNCTION;
  obj->value_ptr.function = fun;

  num_functions++;

  return obj;
}


struct object *
alloc_method (void)
{
  struct method *m;
  struct object *meth = alloc_object ();

  meth->type = TYPE_METHOD;
  m = meth->value_ptr.method = malloc_and_check (sizeof (*m));
  m->qualifier = PRIMARY_METHOD;
  m->body = NULL;
  m->builtin_method = NULL;
  m->object_reader_class = NULL;
  m->object_writer_class = NULL;
  m->object_accessor_class = NULL;
  m->generic_func = NULL;

  return meth;
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
	  if (cur->flags & EXTERNAL_VISIBILITY && sym == cur->sym)
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
				   int only_check_presence, int *is_present)
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

  if (only_check_presence)
    {
      *is_present = 0;
      return NULL;
    }

  uses = pack->value_ptr.package->uses;

  while (uses)
    {
      cur = uses->obj->value_ptr.package->symtable [ind];

      while (cur)
	{
	  if (cur->flags & EXTERNAL_VISIBILITY
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

  return entry->flags & EXTERNAL_VISIBILITY;
}


int
import_symbol (struct object *sym, struct object *pack,
	       struct package_record **rec)
{
  int pres, ind;
  struct package_record *r, *new_cell;

  if ((r = inspect_accessible_symbol_by_name (sym->value_ptr.symbol->name,
					      sym->value_ptr.symbol->name_len,
					      pack, 0, &pres))
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
      new_cell->flags = INTERNAL_VISIBILITY;
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
  struct package_record *r, *r2;
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
	  if (r->flags & EXTERNAL_VISIBILITY
	      && (r2 = inspect_accessible_symbol_by_name
		  (r->sym->value_ptr.symbol->name,
		   r->sym->value_ptr.symbol->name_len, pack, 0, &pres))
	      && (r2->sym != r->sym)
	      && (!pres || !(r2->flags & IS_SHADOWING)))
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

	  free (p);
	  break;
	}

      prev = p;
      p = p->next;
    }

  p = used->value_ptr.package->used_by, prev = NULL;

  while (p)
    {
      if (p->obj == pack)
	{
	  if (prev)
	    {
	      prev->next = p->next;
	    }
	  else
	    {
	      used->value_ptr.package->used_by = p->next;
	    }

	  free (p);
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


/*
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
*/


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
  int found_amp_key;

  outcome->type = EVAL_OK;
  f->lambda_list = parse_lambda_list (lambda_list, allow_destructuring, 0, env,
				      outcome, &found_amp_key,
				      &f->allow_other_keys);

  if (outcome->type != EVAL_OK)
    {
      free_function_or_macro (fun);

      return NULL;
    }

  if (found_amp_key)
    f->flags |= FOUND_AMP_KEY;

  capture_lexical_environment (&f->lex_vars, &f->lex_funcs, env->vars,
			       env->lex_env_vars_boundary, env->funcs,
			       env->lex_env_funcs_boundary);

  f->encl_blocks = env->blocks ? env->blocks->frame : NULL;

  if (f->encl_blocks)
    f->encl_blocks->refcount++;

  f->encl_tags = env->go_tag_stack;

  if (f->encl_tags)
    f->encl_tags->refcount++;

  f->body = body;
  add_reference (fun, body, 1);

  return fun;
}


struct object *
create_empty_generic_function (struct object *name, int is_setf,
			       struct environment *env)
{
  struct outcome out;
  struct object *fun = create_function (&nil_object, &nil_object, env, &out, 0,
					0);
  fun->value_ptr.function->flags |= GENERIC_FUNCTION;
  fun->value_ptr.function->is_setf_func = is_setf;

  if (is_setf)
    {
      name->value_ptr.symbol->setf_func_cell = fun;
      add_reference (name, fun, 2);
    }
  else
    {
      name->value_ptr.symbol->function_cell = fun;
      add_reference (name, fun, 1);
    }

  fun->value_ptr.function->name = name;
  add_reference (fun, name, 0);

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

  num_strings++;

  return obj;
}


struct object *
copy_string (struct object *string)
{
  struct object *ret = alloc_string (string->value_ptr.string->used_size);
  int i;

  ret->value_ptr.string->used_size = string->value_ptr.string->used_size;

  for (i = 0; i < ret->value_ptr.string->used_size; i++)
    {
      ret->value_ptr.string->value [i] = string->value_ptr.string->value [i];
    }

  return ret;
}


struct object *
create_string_from_sequence (struct object *seq, fixnum size)
{
  struct object *ret, *cons;
  fixnum allocs = 0, chars, i, j;

  if (seq->type == TYPE_BITARRAY)
    return NULL;

  if (seq->type == TYPE_CONS_PAIR)
    {
      cons = seq;

      while (SYMBOL (cons) != &nil_object)
	{
	  if (CAR (cons)->type != TYPE_CHARACTER)
	    return NULL;

	  allocs += strlen (CAR (cons)->value_ptr.character);
	  cons = CDR (cons);
	}
    }
  else if (seq->type == TYPE_ARRAY)
    {
      for (i = 0; i < seq->value_ptr.array->alloc_size->size; i++)
	{
	  if (seq->value_ptr.array->value [i]->type != TYPE_CHARACTER)
	    return NULL;

	  allocs += strlen (seq->value_ptr.array->value [i]->value_ptr.character);
	}
    }
  else
    allocs = seq->value_ptr.string->used_size;

  ret = alloc_string (allocs);

  if (seq->type == TYPE_CONS_PAIR)
    {
      cons = seq;
      i = 0;

      while (SYMBOL (cons) != &nil_object)
	{
	  memcpy (ret->value_ptr.string->value+i, CAR (cons)->value_ptr.character,
		  strlen (CAR (cons)->value_ptr.character));
	  i += strlen (CAR (cons)->value_ptr.character);
	  cons = CDR (cons);
	}
    }
  else if (seq->type == TYPE_ARRAY)
    {
      j = 0;

      for (i = 0; i < seq->value_ptr.array->alloc_size->size; i++)
	{
	  memcpy (ret->value_ptr.string->value+j, seq->value_ptr.array->value [i]
		  ->value_ptr.character,
		  strlen (seq->value_ptr.array->value [i]->value_ptr.character));
	  j += strlen (seq->value_ptr.array->value [i]->value_ptr.character);
	}
    }
  else
    {
      chars = 0;

      for (i = 0; i < seq->value_ptr.string->used_size; i++)
	{
	  ret->value_ptr.string->value [i] = seq->value_ptr.string->value [i];

	  if (IS_LOWEST_BYTE_IN_UTF8 (seq->value_ptr.string->value [i]))
	    chars++;
	}

      if (chars != size)
	return NULL;
    }

  ret->value_ptr.string->used_size = allocs;
  return ret;
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

  num_strings++;

  return ret;
}


struct object *
create_string_copying_c_string (const char *str)
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


void
increment_string_allocation_respecting_fill_pointer (struct object *string,
						     fixnum incr)
{
  string->value_ptr.string->used_size = string->value_ptr.string->fill_pointer
    +incr;

  if (string->value_ptr.string->used_size
      <= string->value_ptr.string->alloc_size)
    return;

  string->value_ptr.string->value =
    realloc_and_check (string->value_ptr.string->value,
		       string->value_ptr.string->used_size);

  string->value_ptr.string->alloc_size = string->value_ptr.string->used_size;
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
			 enum package_record_flags *sym_visibility,
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

	  if (input [i] != '.' || out->single_escape || out->multiple_escape)
	    just_dots = 0;

	  (**length)++;
	  out->single_escape = 0;
	}
      i++;
    }

  if (ends_with_eof && (out->single_escape || out->multiple_escape))
    {
      CLEAR_READER_STATUS (*out);
      out->type = GOT_EOF_IN_MIDDLE_OF_OBJECT;
      return NULL;
    }
  else if (ends_with_eof)
    {
      return input+size-1;
    }

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


void
clear_symbol (struct symbol *sym)
{
  sym->is_type = 0;
  sym->builtin_type = NULL;
  sym->typespec = NULL;
  sym->parent_types = NULL;
  sym->is_const = 0;
  sym->is_parameter = 0;
  sym->is_special = 0;
  sym->value_dyn_bins_num = 0;
  sym->value_cell = NULL;
  sym->is_symbol_macro = 0;
  sym->function_dyn_bins_num = 0;
  sym->function_cell = NULL;
  sym->plist = &nil_object;
  sym->setf_expander = NULL;
  sym->home_package = NULL;
  sym->setf_func_dyn_bins_num = 0;
  sym->setf_func_cell = NULL;
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

  clear_symbol (sym);

  obj->value_ptr.symbol = sym;

  num_symbols++;

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

  fn->is_logical = 0;
  fn->directory_type = fn->name_type = REGULAR_FILENAME;

  return obj;
}


int
get_directory_file_split (struct object *string)
{
  int i;

  for (i = string->value_ptr.string->used_size-1; i >= 0; i--)
    {
      if (string->value_ptr.string->value [i] == '/')
	return i;
    }

  return i;
}


int
get_filename (struct object *string, int *end)
{
  int i;

  *end = -1;

  for (i = string->value_ptr.string->used_size-1; i >= 0; i--)
    {
      if (string->value_ptr.string->value [i] == '.' && *end == -1)
	*end = i;

      if (string->value_ptr.string->value [i] == '/')
	{
	  if (*end == i+1)
	    *end = -1;

	  return i;
	}
    }

  return i;
}


struct object *
inspect_pathname_by_designator (struct object *des)
{
  if (des->type == TYPE_STRING)
    {
      return des;
    }
  else if (des->type == TYPE_STREAM)
    {
      return des->value_ptr.stream->namestring;
    }
  else
    {
      return des->value_ptr.filename->value;
    }
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

  num_arrays++;

  return obj;
}


struct object *
create_vector_from_list (struct object *list, fixnum size)
{
  struct object *obj = alloc_object (), *lastobj = &nil_object;
  struct array *vec = malloc_and_check (sizeof (*vec));
  struct array_size *sz = malloc_and_check (sizeof (*sz));
  fixnum i, l = list_length (list), actsz = (size >= 0 ? size : l);

  obj->type = TYPE_ARRAY;

  sz->size = actsz;
  sz->next = NULL;

  vec->alloc_size = sz;
  vec->fill_pointer = -1;
  vec->value = calloc_and_check (actsz, sizeof (*vec->value));
  vec->reference_strength_factor = calloc_and_check (actsz, sizeof (int));

  obj->value_ptr.array = vec;

  for (i = 0; i < actsz && i < l; i++)
    {
      vec->value [i] = CAR (list);
      add_reference (obj, vec->value [i], i);
      lastobj = CAR (list);
      list = CDR (list);
    }

  for (; i < size; i++)
    {
      vec->value [i] = lastobj;
      add_reference (obj, vec->value [i], i);
    }

  num_arrays++;

  return obj;
}


struct object *
alloc_bitvector (fixnum size)
{
  struct object *ret = alloc_object ();

  ret->type = TYPE_BITARRAY;
  ret->value_ptr.bitarray = malloc_and_check (sizeof (*ret->value_ptr.bitarray));

  ret->value_ptr.bitarray->alloc_size =
    malloc_and_check (sizeof (*ret->value_ptr.bitarray->alloc_size));
  ret->value_ptr.bitarray->alloc_size->size = size;
  ret->value_ptr.bitarray->alloc_size->next = NULL;

  ret->value_ptr.bitarray->fill_pointer = -1;

  mpz_init (ret->value_ptr.bitarray->value);

  return ret;
}


struct object *
create_bitvector_from_char_vector (const char *in, size_t sz, size_t req_size)
{
  struct object *ret = alloc_bitvector (req_size);
  size_t i;
  char l;

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


struct object *
fill_axis_from_sequence (struct object *arr, struct object **axis, fixnum index,
			 struct array_size *size, fixnum rowsize,
			 struct object *seq)
{
  fixnum i;
  struct object *el;

  if (!size)
    {
      *axis = seq;
      add_reference (arr, seq, index);
    }
  else if (size->next)
    {
      for (i = 0; i < size->size; i++)
	{
	  el = elt (seq, i);

	  if (!IS_SEQUENCE (el))
	    return NULL;

	  if (SEQUENCE_LENGTH (el) != size->next->size)
	    return NULL;

	  if (!fill_axis_from_sequence (arr,
					&axis [i*(rowsize/size->size)],
					index+i*(rowsize/size->size), size->next,
					rowsize/size->size, el))
	    {
	      return NULL;
	    }

	  if (seq->type == TYPE_STRING || seq->type == TYPE_BITARRAY)
	    decrement_refcount (el);
	}
    }
  else
    {
      for (i = 0; i < size->size; i++)
	{
	  axis [i] = elt (seq, i);
	  add_reference (arr, axis [i], index+i);

	  if (seq->type == TYPE_STRING || seq->type == TYPE_BITARRAY)
	    decrement_refcount (axis [i]);
	}
    }

  return &t_object;
}


struct object *
create_array_from_sequence (struct object *seq, fixnum rank)
{
  struct object *obj = alloc_object (), *s = seq, *el;
  struct array *arr = malloc_and_check (sizeof (*arr));
  struct array_size *size = malloc_and_check (sizeof (*size)), *sz = size;
  fixnum i, totsize, rowsize;

  obj->type = TYPE_ARRAY;

  sz->size = totsize = SEQUENCE_LENGTH (s);

  for (i = 1; i < rank; i++)
    {
      sz->next = malloc_and_check (sizeof (*sz));
      sz = sz->next;
      s = elt (s, 0);

      if (!IS_SEQUENCE (s))
	return NULL;

      sz->size = SEQUENCE_LENGTH (s);
      totsize *= sz->size;
    }

  sz->next = NULL;

  arr->alloc_size = size;
  arr->fill_pointer = -1;
  arr->value = calloc_and_check (totsize, sizeof (*arr->value));
  arr->reference_strength_factor = calloc_and_check (totsize, sizeof (int));

  obj->value_ptr.array = arr;

  rowsize = totsize / size->size;

  for (i = 0; i < size->size; i++)
    {
      el = elt (seq, i);

      if (size->next && !IS_SEQUENCE (el))
	return NULL;

      if (size->next && SEQUENCE_LENGTH (el) != size->next->size)
	return NULL;

      if (!fill_axis_from_sequence (obj, &arr->value [i*rowsize], i*rowsize,
				    size->next, rowsize, el))
	return NULL;

      if (seq->type == TYPE_STRING || seq->type == TYPE_BITARRAY)
	decrement_refcount (el);
    }

  return obj;
}


struct object *
adjust_array_axis (struct object *dest, struct array_size *dest_axis_size,
		   struct object *src, struct array_size *src_axis_size,
		   struct array_size *ind, struct array_size *i)
{
  fixnum j;
  size_t s, d;

  if (i->next)
    {
      for (j = 0; j < dest_axis_size->size && j < src_axis_size->size; j++)
	{
	  i->size = j;
	  adjust_array_axis (dest, dest_axis_size->next, src,
			     src_axis_size->next, ind, i->next);
	}
    }
  else
    {
      for (j = 0; j < dest_axis_size->size && j < src_axis_size->size; j++)
	{
	  i->size = j;

	  s = array_row_major_index (ind, src->value_ptr.array->alloc_size);
	  d = array_row_major_index (ind, dest->value_ptr.array->alloc_size);

	  dest->value_ptr.array->value [d] = src->value_ptr.array->value [s];
	  add_reference (dest, dest->value_ptr.array->value [d], d);
	}
    }

  return dest;
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
create_character_from_designator (struct object *des)
{
  char *s, *buf;
  int l, i;

  if (des->type == TYPE_CHARACTER)
    {
      increment_refcount (des);
      return des;
    }

  if (IS_SYMBOL (des))
    {
      s = SYMBOL (des)->value_ptr.symbol->name;
      l = SYMBOL (des)->value_ptr.symbol->name_len;
    }
  else
    {
      s = des->value_ptr.string->value;
      l = des->value_ptr.string->used_size;
    }

  for (i = 0; i < l; i++)
    {
      if (IS_LOWEST_BYTE_IN_UTF8 (s [i]))
	{
	  i++;
	  break;
	}
    }

  buf = malloc_and_check (i+1);
  strncpy (buf, s, i);
  buf [i] = 0;

  return create_character (buf, 0);
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


fixnum
get_nth_character_preceding_offset (struct object *str, int ind)
{
  fixnum i;

  if (!ind)
    return 0;

  for (i = 0; i < str->value_ptr.string->used_size; i++)
    {
      if (IS_LOWEST_BYTE_IN_UTF8 (str->value_ptr.string->value [i]))
	ind--;

      if (!ind)
	return i;
    }

  return -1;
}


int
set_nth_character (struct object *str, int ind, char *ch)
{
  char *c = str->value_ptr.string->value, *bk;
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
	  bk = str->value_ptr.string->value;
	  str->value_ptr.string->value =
	    realloc_and_check (str->value_ptr.string->value, newsz);
	  c = str->value_ptr.string->value + (c-bk);

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
	       off < str->value_ptr.string->used_size+strlen(ch)-csz; off++)
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
create_file_stream (enum stream_content_type content_type,
		    enum stream_direction direction, struct object *namestring,
		    int overwrite, int if_doesnt_exist, struct outcome *outcome)
{
  struct object *obj;
  struct stream *str;
  char *fn = copy_string_to_c_string (namestring->value_ptr.string);
  FILE *f;

  if (direction == INPUT_STREAM)
    f = fopen (fn, "rb");
  else if (direction == OUTPUT_STREAM || direction == BIDIRECTIONAL_STREAM)
    {
      f = fopen (fn, "r");

      if (!f && !if_doesnt_exist)
	{
	  free (fn);
	  fclose (f);
	  outcome->type = COULD_NOT_OPEN_FILE;
	  return NULL;
	}

      if (f && !overwrite)
	{
	  free (fn);
	  fclose (f);
	  outcome->type = FILE_ALREADY_EXISTS;
	  return NULL;
	}

      if (f)
	fclose (f);

      if (direction == OUTPUT_STREAM)
	f = fopen (fn, "wb");
      else
	f = fopen (fn, "wb+");
    }

  free (fn);

  if (!f)
    {
      outcome->type = COULD_NOT_OPEN_FILE;
      return NULL;
    }

  obj = alloc_object ();
  str = malloc_and_check (sizeof (*str));

  str->type = FILE_STREAM;
  str->file = f;
  str->content_type = content_type;
  str->direction = direction;
  str->is_open = 1;
  str->dirty_line = 0;

  obj->type = TYPE_STREAM;
  obj->value_ptr.stream = str;

  str->namestring = namestring;
  add_reference (obj, namestring, 0);

  return obj;
}


struct object *
create_stream_from_open_file (enum stream_content_type content_type,
			      enum stream_direction direction, FILE *file)
{
  struct object *obj = alloc_object ();
  struct stream *str = malloc_and_check (sizeof (*str));

  str->type = FILE_STREAM;
  str->namestring = &nil_object;
  str->content_type = content_type;
  str->direction = direction;
  str->is_open = 1;
  str->dirty_line = 0;
  str->file = file;

  obj->type = TYPE_STREAM;
  obj->value_ptr.stream = str;

  return obj;
}


struct object *
create_string_stream (enum stream_direction direction, struct object *instr,
		      int begin, int end)
{
  struct object *obj = alloc_object ();
  struct stream *str = malloc_and_check (sizeof (*str));
  fixnum begoff, endoff;

  str->type = STRING_STREAM;
  str->direction = direction;
  str->is_open = 1;
  str->dirty_line = 0;

  obj->type = TYPE_STREAM;
  obj->value_ptr.stream = str;

  if (direction == INPUT_STREAM)
    {
      if (begin < 0 || (end >= 0 && begin > end))
	return NULL;

      if (!begin && end < 0)
	{
	  add_reference (obj, instr, 0);
	  str->string = instr;
	}
      else
	{
	  begoff = get_nth_character_offset (instr, begin);

	  if (begoff < 0)
	    return NULL;

	  if (end >= 0)
	    {
	      endoff = get_nth_character_preceding_offset (instr, end);

	      if (endoff < 0)
		return NULL;
	    }

	  if (end < 0)
	    {
	      str->string = create_string_copying_char_vector
		(instr->value_ptr.string->value+begoff,
		 instr->value_ptr.string->used_size-begoff);
	    }
	  else
	    {
	      str->string = create_string_copying_char_vector
		(instr->value_ptr.string->value+begoff, endoff-begoff+1);
	    }
	}
    }
  else
    {
      if (instr)
	{
	  add_reference (obj, instr, 0);
	  str->string = instr;
	}
      else
	str->string = alloc_string (0);
    }

  return obj;
}


struct object *
create_synonym_stream (struct object *sym)
{
  struct object *obj = alloc_object ();
  struct stream *str = malloc_and_check (sizeof (*str));

  str->type = SYNONYM_STREAM;
  str->is_open = 1;
  str->synonym_of = sym;
  str->direction = NO_DIRECTION;

  obj->type = TYPE_STREAM;
  obj->value_ptr.stream = str;

  add_reference (obj, sym, 0);

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
create_class_field_decl (struct object *class, struct object *fieldform,
			 struct environment *env, struct outcome *outcome)
{
  struct object *name, *initform = NULL, *reader = NULL, *writer = NULL,
    *accessor = NULL, *fun, *meth, *funcname,
    *classname = class->value_ptr.standard_class->name;
  struct object_list *initargs = NULL, *l;
  enum field_allocation_type alloctype = UNKNOWN_ALLOCATION;
  struct class_field_decl *ret;
  struct parameter *lambdal;

  if (IS_SYMBOL (fieldform))
    name = SYMBOL (fieldform);
  else if (fieldform->type == TYPE_CONS_PAIR && IS_SYMBOL (CAR (fieldform)))
    {
      name = SYMBOL (CAR (fieldform));
      fieldform = CDR (fieldform);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  while (fieldform->type == TYPE_CONS_PAIR)
    {
      if (symbol_equals (CAR (fieldform), ":INITFORM", env))
	{
	  if (SYMBOL (CDR (fieldform)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (initform)
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  initform = CAR (CDR (fieldform));
	}
      else if (symbol_equals (CAR (fieldform), ":INITARG", env))
	{
	  if (SYMBOL (CDR (fieldform)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  l = malloc_and_check (sizeof (*l));
	  l->obj = CAR (CDR (fieldform));
	  increment_refcount (l->obj);
	  l->next = initargs;
	  initargs = l;
	}
      else if (symbol_equals (CAR (fieldform), ":READER", env))
	{
	  if (SYMBOL (CDR (fieldform)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!IS_SYMBOL (CAR (CDR (fieldform))))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  reader = SYMBOL (CAR (CDR (fieldform)));
	  fun = reader->value_ptr.symbol->function_cell;

	  if (fun && (fun->type == TYPE_MACRO
		      || !(fun->value_ptr.function->flags & GENERIC_FUNCTION)))
	    {
	      outcome->type = CANT_REDEFINE_AS_GENERIC_FUNCTION;
	      return NULL;
	    }

	  lambdal = create_lambda_list (env, "OBJ", (char *)NULL);
	  lambdal->typespec = classname;
	  increment_refcount (classname);

	  if (!fun)
	    {
	      fun = create_empty_generic_function (reader, 0, env);
	      fun->value_ptr.function->lambda_list = copy_lambda_list (lambdal,
								       0);
	      decrement_refcount (fun);
	    }
	  else
	    {
	      if (!are_lambda_lists_congruent (lambdal, 0,
					       fun->value_ptr.function->lambda_list,
					       fun->value_ptr.function->flags
					       & FOUND_AMP_KEY))
		{
		  outcome->type = LAMBDA_LISTS_NOT_CONGRUENT;
		  return NULL;
		}
	    }

	  meth = alloc_method ();
	  meth->value_ptr.method->lambda_list = lambdal;
	  meth->value_ptr.method->object_reader_class = class;
	  meth->value_ptr.method->object_reader_field = name;
	  add_method (fun, meth);
	  decrement_refcount (meth);
	}
      else if (symbol_equals (CAR (fieldform), ":WRITER", env))
	{
	  if (SYMBOL (CDR (fieldform)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!IS_FUNCTION_NAME (CAR (CDR (fieldform))))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  writer = CAR (CDR (fieldform));
	  funcname = writer->type == TYPE_CONS_PAIR ? SYMBOL (CAR (CDR (writer)))
	    : SYMBOL (writer);
	  fun = writer->type == TYPE_CONS_PAIR
	    ? funcname->value_ptr.symbol->setf_func_cell
	    : funcname->value_ptr.symbol->function_cell;

	  if (fun && (fun->type == TYPE_MACRO
		      || !(fun->value_ptr.function->flags & GENERIC_FUNCTION)))
	    {
	      outcome->type = CANT_REDEFINE_AS_GENERIC_FUNCTION;
	      return NULL;
	    }

	  lambdal = create_lambda_list (env, "NEWVAL", "OBJ", (char *)NULL);
	  lambdal->typespec = &t_object;
	  lambdal->next->typespec = classname;
	  increment_refcount (classname);

	  if (!fun)
	    {
	      fun = create_empty_generic_function (funcname,
						   writer->type == TYPE_CONS_PAIR,
						   env);
	      fun->value_ptr.function->lambda_list = copy_lambda_list (lambdal,
								       0);
	      decrement_refcount (fun);
	    }
	  else
	    {
	      if (!are_lambda_lists_congruent (lambdal, 0,
					       fun->value_ptr.function->lambda_list,
					       fun->value_ptr.function->flags
					       & FOUND_AMP_KEY))
		{
		  outcome->type = LAMBDA_LISTS_NOT_CONGRUENT;
		  return NULL;
		}
	    }

	  meth = alloc_method ();
	  meth->value_ptr.method->lambda_list = lambdal;
	  meth->value_ptr.method->object_writer_class = class;
	  meth->value_ptr.method->object_writer_field = name;
	  add_method (fun, meth);
	  decrement_refcount (meth);
	}
      else if (symbol_equals (CAR (fieldform), ":ACCESSOR", env))
	{
	  if (SYMBOL (CDR (fieldform)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!IS_SYMBOL (CAR (CDR (fieldform))))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  accessor = SYMBOL (CAR (CDR (fieldform)));
	  fun = accessor->value_ptr.symbol->function_cell;

	  if (fun && (fun->type == TYPE_MACRO
		      || !(fun->value_ptr.function->flags & GENERIC_FUNCTION)))
	    {
	      outcome->type = CANT_REDEFINE_AS_GENERIC_FUNCTION;
	      return NULL;
	    }

	  lambdal = create_lambda_list (env, "OBJ", (char *)NULL);
	  lambdal->typespec = classname;
	  increment_refcount (classname);

	  if (!fun)
	    {
	      fun = create_empty_generic_function (accessor, 0, env);
	      fun->value_ptr.function->lambda_list = copy_lambda_list (lambdal,
								       0);
	      decrement_refcount (fun);
	    }
	  else
	    {
	      if (!are_lambda_lists_congruent (lambdal, 0,
					       fun->value_ptr.function->lambda_list,
					       fun->value_ptr.function->flags
					       & FOUND_AMP_KEY))
		{
		  outcome->type = LAMBDA_LISTS_NOT_CONGRUENT;
		  return NULL;
		}
	    }

	  meth = alloc_method ();
	  meth->value_ptr.method->lambda_list = lambdal;
	  meth->value_ptr.method->object_accessor_class = class;
	  meth->value_ptr.method->object_accessor_field = name;
	  add_method (fun, meth);
	  decrement_refcount (meth);


	  fun = accessor->value_ptr.symbol->setf_func_cell;

	  if (fun && (fun->type == TYPE_MACRO
		      || !(fun->value_ptr.function->flags & GENERIC_FUNCTION)))
	    {
	      outcome->type = CANT_REDEFINE_AS_GENERIC_FUNCTION;
	      return NULL;
	    }

	  lambdal = create_lambda_list (env, "NEWVAL", "OBJ", (char *)NULL);
	  lambdal->typespec = &t_object;
	  lambdal->next->typespec = classname;
	  increment_refcount (classname);

	  if (!fun)
	    {
	      fun = create_empty_generic_function (accessor, 1, env);
	      fun->value_ptr.function->lambda_list = copy_lambda_list (lambdal, 0);
	      decrement_refcount (fun);
	    }
	  else
	    {
	      if (!are_lambda_lists_congruent (lambdal, 0,
					       fun->value_ptr.function->lambda_list,
					       fun->value_ptr.function->flags
					       & FOUND_AMP_KEY))
		{
		  outcome->type = LAMBDA_LISTS_NOT_CONGRUENT;
		  return NULL;
		}
	    }

	  meth = alloc_method ();
	  meth->value_ptr.method->lambda_list = lambdal;
	  meth->value_ptr.method->object_accessor_class = class;
	  meth->value_ptr.method->object_accessor_field = name;
	  add_method (fun, meth);
	  decrement_refcount (meth);
	}
      else if (symbol_equals (CAR (fieldform), ":ALLOCATION", env))
	{
	  if (SYMBOL (CDR (fieldform)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (alloctype != UNKNOWN_ALLOCATION)
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  if (symbol_equals (CAR (CDR (fieldform)), ":INSTANCE", env))
	    alloctype = INSTANCE_ALLOCATION;
	  else if (symbol_equals (CAR (CDR (fieldform)), ":CLASS", env))
	    alloctype = CLASS_ALLOCATION;
	  else
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }
	}
      else if (symbol_equals (CAR (fieldform), ":DOCUMENTATION", env))
	{
	  if (SYMBOL (CDR (fieldform)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (CAR (CDR (fieldform))->type != TYPE_STRING)
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }
	}
      else if (symbol_equals (CAR (fieldform), ":TYPE", env))
	{
	  if (SYMBOL (CDR (fieldform)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }
	}
      else
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      fieldform = CDR (CDR (fieldform));
    }

  ret = malloc_and_check (sizeof (*ret));

  if (alloctype == UNKNOWN_ALLOCATION)
    alloctype = INSTANCE_ALLOCATION;

  ret->name = name;

  ret->alloc_type = alloctype;

  ret->value = NULL;

  if (initform)
    increment_refcount (initform);

  ret->initform = initform;
  ret->initargs = initargs;
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


struct object *
define_class (struct object *name, struct object *form, int is_condition_class,
	      struct environment *env, struct outcome *outcome)
{
  struct object *class = alloc_object (), *cons;
  struct standard_class *sc = malloc_and_check (sizeof (*sc));
  struct class_field_decl *f, *prev;
  struct object_list *p;

  class->type = TYPE_STANDARD_CLASS;
  class->value_ptr.standard_class = sc;

  increment_refcount (name);
  sc->name = name;
  sc->is_condition_class = is_condition_class;

  name->value_ptr.symbol->is_type = 1;

  delete_reference (name, name->value_ptr.symbol->typespec, 4);
  name->value_ptr.symbol->typespec = class;
  add_reference (name, class, 4);


  sc->parents = NULL;
  cons = CAR (form);

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
      sc->parents->obj = CREATE_BUILTIN_SYMBOL (is_condition_class ? "CONDITION"
						: "STANDARD-OBJECT");
      sc->parents->next = NULL;
    }

  sc->descendants = NULL;
  sc->class_precedence_list = NULL;
  sc->fields = NULL;
  form = CAR (CDR (form));

  while (SYMBOL (form) != &nil_object)
    {
      f = create_class_field_decl (class, CAR (form), env, outcome);

      if (!f)
	return NULL;

      if (sc->fields)
	prev = prev->next = f;
      else
	sc->fields = prev = f;

      form = CDR (form);
    }

  return class;
}


struct object *
allocate_object_fields (struct object *stdobj, struct object *class)
{
  struct class_field_decl *fd;
  struct object_list *p = stdobj->value_ptr.standard_object
    ->class->value_ptr.standard_class->class_precedence_list;
  struct class_field *f;

  while (p)
    {
      fd = p->obj->value_ptr.standard_class->fields;

      while (fd)
	{
	  f = stdobj->value_ptr.standard_object->fields;

	  while (f)
	    {
	      if (f->decl->name == fd->name)
		break;

	      f = f->next;
	    }

	  if (f)
	    {
	      fd = fd->next;
	      continue;
	    }

	  f = malloc_and_check (sizeof (*f));

	  if (fd->alloc_type == INSTANCE_ALLOCATION)
	    f->name = fd->name;
	  else
	    f->name = NULL;

	  f->value = NULL;
	  f->decl = fd;
	  f->next = stdobj->value_ptr.standard_object->fields;
	  stdobj->value_ptr.standard_object->fields = f;

	  fd = fd->next;
	}

      p = p->next;
    }

  return stdobj;
}


struct class_field *
find_object_field_by_initarg (struct object *stdobj, struct object *initarg)
{
  struct class_field_decl *fd;
  struct class_field *f;
  struct object_list *l, *p = stdobj->value_ptr.standard_object->class->
    value_ptr.standard_class->class_precedence_list;

  while (p)
    {
      fd = p->obj->value_ptr.standard_class->fields;

      while (fd)
	{
	  l = fd->initargs;

	  while (l)
	    {
	      if (eq_objects (l->obj, initarg) == &t_object)
		{
		  f = stdobj->value_ptr.standard_object->fields;

		  while (f)
		    {
		      if (f->decl->name == fd->name)
			return f;

		      f = f->next;
		    }
		}

	      l = l->next;
	    }

	  fd = fd->next;
	}

      p = p->next;
    }

  return NULL;
}


struct object *
fill_object_fields_by_initargs (struct object *stdobj, struct object *class,
				struct object *initargs, struct environment *env,
				struct outcome *outcome)
{
  struct class_field *f = stdobj->value_ptr.standard_object->fields;

  while (f)
    {
      f->found_key = 0;
      f = f->next;
    }

  while (SYMBOL (initargs) != &nil_object)
    {
      if (!(f = find_object_field_by_initarg (stdobj, CAR (initargs))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (SYMBOL (CDR (initargs)) == &nil_object)
	{
	  outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	  return NULL;
	}

      if (!f->found_key)
	{
	  if (!f->name)
	    {
	      decrement_refcount (f->decl->value);
	      f->decl->value = CAR (CDR (initargs));
	      increment_refcount (f->decl->value);
	    }
	  else
	    {
	      decrement_refcount (f->value);
	      f->value = CAR (CDR (initargs));
	      increment_refcount (f->value);
	    }
	}

      f->found_key = 1;
      initargs = CDR (CDR (initargs));
    }

  return stdobj;
}


struct object *
fill_object_fields (struct object *stdobj, struct object *class,
		    struct object *initargs, struct environment *env,
		    struct outcome *outcome)
{
  struct class_field *f;

  if (!fill_object_fields_by_initargs (stdobj, class, initargs, env, outcome))
    return NULL;

  f = stdobj->value_ptr.standard_object->fields;

  while (f)
    {
      if (!f->name)
	{
	  if (!f->decl->value && f->decl->initform)
	    {
	      f->decl->value = evaluate_object (f->decl->initform, env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!f->decl->value)
		return NULL;
	    }
	}
      else
	{
	  if (!f->value && f->decl->initform)
	    {
	      f->value = evaluate_object (f->decl->initform, env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!f->value)
		return NULL;
	    }
	}

      f = f->next;
    }

  return stdobj;
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
load_file (const char *filename, int print_each_form, struct environment *env,
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
      return raise_file_error (NULL, filename, env, outcome);
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
  clear_read_labels (&env->read_labels);

  while (1)
    {
      if (out == COMPLETE_OBJECT || out == SKIPPED_OBJECT)
	{
	  if (out == COMPLETE_OBJECT)
	    {
	      if (print_each_form)
		{
		  print_object (obj, env, env->c_stdout->value_ptr.stream);
		  printf (" ");
		}

	      res = evaluate_object (obj, env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!res)
		{
		  free (buf);
		  fclose (f);

		  return NULL;
		}

	      if (print_each_form)
		{
		  printf ("-> ");
		  print_object (res, env, env->c_stdout->value_ptr.stream);
		  printf ("\n\n");
		}

	      decrement_refcount (res);
	      decrement_refcount (obj);
	    }

	  obj = NULL;
	  out = read_object (&obj, 0, in, sz, NULL, 0, 1, env, outcome, &obj_b,
			     &obj_e);
	  sz = sz - (obj_e + 1 - in);
	  in = obj_e + 1;
	  clear_read_labels (&env->read_labels);
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
compile_function (struct object *fun, struct environment *env,
		  struct outcome *outcome)
{
  struct method_list *ml;

  if (fun->value_ptr.function->flags & COMPILED_FUNCTION)
    return fun;

  if (fun->value_ptr.function->flags & GENERIC_FUNCTION)
    {
      ml = fun->value_ptr.function->methods;

      while (ml)
	{
	  if (!compile_body (ml->meth->value_ptr.method->body, 0, env, outcome))
	    return NULL;

	  ml = ml->next;
	}
    }
  else if (!compile_body (fun->value_ptr.function->body, 0, env, outcome))
    return NULL;

  fun->value_ptr.function->flags |= COMPILED_FUNCTION;

  return fun;
}


struct object *
compile_form (struct object *form, int backt_comma_bal, struct environment *env,
	      struct outcome *outcome)
{
  struct object *prevform = NULL, *mac, *args, *fun, *cons, *val;
  int expanded = 0;

  while (!backt_comma_bal && form->type == TYPE_CONS_PAIR && IS_SYMBOL (CAR (form))
	 && (mac = get_function (CAR (form), env, 0, 0, 0, 0))
	 && mac->type == TYPE_MACRO && !mac->value_ptr.function->builtin_form)
    {
      expanded = 1;

      if (mac->value_ptr.macro->macro_function)
	{
	  args = alloc_empty_list (2);
	  args->value_ptr.cons_pair->car = form;
	  args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = &nil_object;

	  form = call_function (mac->value_ptr.macro->macro_function, args,
				0, 0, 0, 0, 0, env, outcome);
	  free_list_structure (args);
	}
      else
	{
	  form = call_function (mac, form, 0, 1, 1, 0, 0, env, outcome);
	}

      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      decrement_refcount (prevform);
      prevform = form;

      if (!form)
	return NULL;
    }

  if (!expanded)
    {
      increment_refcount (form);
    }

  if (backt_comma_bal && form->type == TYPE_CONS_PAIR)
    {
      if (!compile_body (form, backt_comma_bal, env, outcome))
	return NULL;
    }
  else if (form->type == TYPE_CONS_PAIR && IS_SYMBOL (CAR (form)))
    {
      if (!(fun = get_function (CAR (form), env, 0, 0, 0, 0))
	  || fun->type == TYPE_FUNCTION)
	{
	  if (!compile_body (CDR (form), backt_comma_bal, env, outcome))
	    return NULL;
	}
      else if (SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("IF")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("PROGN")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("BLOCK")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("TAGBODY")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("MULTIPLE-VALUE-CALL")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("AND")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("OR"))
	{
	  if (!compile_body (CDR (form), backt_comma_bal, env, outcome))
	    return NULL;
	}
      else if (SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("LET")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("LET*")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("HANDLER-BIND")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("RESTART-BIND"))
	{
	  cons = CAR (CDR (form));

	  while (cons->type == TYPE_CONS_PAIR)
	    {
	      if (CAR (cons)->type == TYPE_CONS_PAIR
		  && !compile_body (CDR (CAR (cons)), backt_comma_bal, env,
				    outcome))
		{
		  return NULL;
		}

	      cons = CDR (cons);
	    }

	  if (!compile_body (CDR (CDR (form)), backt_comma_bal, env, outcome))
	    return NULL;
	}
      else if (SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("LAMBDA")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("DOLIST")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("DOTIMES")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("DO-SYMBOLS")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("DO-EXTERNAL-SYMBOLS")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("DESTRUCTURING-BIND")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("AL-LOOPY-DESTRUCTURING-BIND")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("RETURN-FROM"))
	{
	  if (!compile_body (CDR (CDR (form)), backt_comma_bal, env, outcome))
	    return NULL;
	}
      else if (SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("FUNCTION"))
	{
	  if (CAR (CDR (form))->type == TYPE_CONS_PAIR
	      && SYMBOL (CAR (CAR (CDR (form)))) == BUILTIN_SYMBOL ("LAMBDA"))
	    {
	      if (!compile_body (CDR (CDR (CAR (CDR (form)))), backt_comma_bal,
				 env, outcome))
		return NULL;
	    }
	}
      else if (SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("FLET")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("LABELS")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("MACROLET"))
	{
	  cons = CAR (CDR (form));

	  while (cons->type == TYPE_CONS_PAIR)
	    {
	      if (!compile_body (CDR (CDR (CAR (cons))), backt_comma_bal, env,
				 outcome))
		return NULL;

	      cons = CDR (cons);
	    }

	  if (!compile_body (CDR (CDR (form)), backt_comma_bal, env, outcome))
	    return NULL;
	}
      else if (SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("DO")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("DO*")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("DEFUN")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("DEFMACRO"))
	{
	  if (!compile_body (CDR (CDR (CDR (form))), backt_comma_bal, env,
			     outcome))
	    return NULL;
	}
      else if (SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("SETQ")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("SETF")
	       || SYMBOL (CAR (form)) == BUILTIN_SYMBOL ("AL-LOOPY-SETQ"))
	{
	  cons = CDR (form);

	  while (SYMBOL (cons) != &nil_object)
	    {
	      cons = CDR (cons);

	      if (SYMBOL (cons) == &nil_object)
		break;

	      val = compile_form (CAR (cons), backt_comma_bal, env, outcome);

	      if (!val)
		return NULL;

	      delete_reference (cons, CAR (cons), 0);
	      cons->value_ptr.cons_pair->car = val;
	      add_reference (cons, CAR (cons), 0);
	      decrement_refcount (val);

	      cons = CDR (cons);
	    }
	}
    }

  if (form->type == TYPE_BACKQUOTE)
    {
      if (!compile_form (form->value_ptr.next, backt_comma_bal+1, env, outcome))
	return NULL;
    }
  else if (form->type == TYPE_COMMA)
    {
      val = compile_form (form->value_ptr.next, backt_comma_bal-1, env, outcome);

      if (!val)
	return NULL;

      if (backt_comma_bal == 1 && form->value_ptr.next->type != TYPE_AT
	  && form->value_ptr.next->type != TYPE_DOT)
	{
	  delete_reference (form, form->value_ptr.next, 0);
	  form->value_ptr.next = val;
	  add_reference (form, val, 0);
	  decrement_refcount (val);
	}
    }
  else if (form->type == TYPE_AT || form->type == TYPE_DOT)
    {
      val = compile_form (form->value_ptr.next, backt_comma_bal, env, outcome);

      if (!val)
	return NULL;

      if (!backt_comma_bal)
	{
	  delete_reference (form, form->value_ptr.next, 0);
	  form->value_ptr.next = val;
	  add_reference (form, val, 0);
	  decrement_refcount (val);
	}
    }


  return form;
}


int
compile_body (struct object *body, int backt_comma_bal, struct environment *env,
	      struct outcome *outcome)
{
  struct object *car;

  if (SYMBOL (body) == &nil_object)
    return 1;

  while (body->type == TYPE_CONS_PAIR)
    {
      car = compile_form (CAR (body), backt_comma_bal, env, outcome);

      if (!car)
	return 0;

      delete_reference (body, CAR (body), 0);
      body->value_ptr.cons_pair->car = car;
      add_reference (body, CAR (body), 0);
      decrement_refcount (car);

      body = CDR (body);
    }

  return 1;
}


struct object *
intern_symbol_by_char_vector (char *name, size_t len, int do_copy,
			      enum package_record_flags vis,
			      int always_create_if_missing,
			      struct object *package, int is_keyword_package,
			      int always_export_if_created)
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
	      && cur->flags & INTERNAL_VISIBILITY)
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
	      if (cur->flags & EXTERNAL_VISIBILITY
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

  if (is_keyword_package)
    {
      sym->value_ptr.symbol->is_const = 1;
      sym->value_ptr.symbol->value_cell = sym;
      add_reference (sym, sym, 0);
    }

  new_sym = malloc_and_check (sizeof (*new_sym));

  if (always_export_if_created)
    new_sym->flags = EXTERNAL_VISIBILITY;
  else
    new_sym->flags = vis;

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
  struct symbol *sym;
  char *name;
  struct package_record *cell, *cur, *new_cell;
  struct object_list *uses;
  enum package_record_flags vis = s->packname_present ? s->visibility
    : pack == env->keyword_package ? EXTERNAL_VISIBILITY : INTERNAL_VISIBILITY;
  int ind, len;

  if (!pack)
    {
      sym = malloc_and_check (sizeof (*sym));
      clear_symbol (sym);
      sym->name = s->value;
      sym->name_len = s->used_size;
      sym->home_package = &nil_object;

      if (s->actual_symname_alloc_s)
	free (s->actual_symname);

      symname->type = TYPE_SYMBOL;
      symname->value_ptr.symbol = sym;
      free (s);
      return symname;
    }


  if (s->packname_present && s->used_size)
    {
      pack = find_package (s->value, s->used_size, env);

      if (!pack)
	{
	  *out = PACKAGE_NOT_FOUND_IN_READ;
	  return NULL;
	}
    }
  else if (s->packname_present)
    pack = env->keyword_package;

  name = s->packname_present ? s->actual_symname : s->value;
  len = s->packname_present ? s->actual_symname_used_s : s->used_size;

  ind = hash_char_vector (name, len, SYMTABLE_SIZE);
  cur = cell = pack->value_ptr.package->symtable [ind];

  while (cur)
    {
      if (eqmem (cur->sym->value_ptr.symbol->name,
		 cur->sym->value_ptr.symbol->name_len, name, len))
	{
	  if (vis == EXTERNAL_VISIBILITY
	      && cur->flags & INTERNAL_VISIBILITY)
	    {
	      *out = SYMBOL_IS_NOT_EXTERNAL_IN_PACKAGE;
	      return NULL;
	    }

	  free_symbol_name (symname);
	  increment_refcount (cur->sym);
	  return cur->sym;
	}

      cur = cur->next;
    }

  if (vis == INTERNAL_VISIBILITY)
    {
      uses = pack->value_ptr.package->uses;

      while (uses)
	{
	  cur = uses->obj->value_ptr.package->symtable [ind];

	  while (cur)
	    {
	      if (cur->flags & EXTERNAL_VISIBILITY
		  && eqmem (cur->sym->value_ptr.symbol->name,
			    cur->sym->value_ptr.symbol->name_len, name, len))
		{
		  free_symbol_name (symname);
		  increment_refcount (cur->sym);
		  return cur->sym;
		}

	      cur = cur->next;
	    }

	  uses = uses->next;
	}
    }

  if (vis == EXTERNAL_VISIBILITY && pack != env->keyword_package)
    {
      *out = SYMBOL_IS_NOT_EXTERNAL_IN_PACKAGE;
      return NULL;
    }


  sym = malloc_and_check (sizeof (*sym));
  clear_symbol (sym);
  sym->name = name;
  sym->name_len = len;
  sym->home_package = pack;

  symname->type = TYPE_SYMBOL;
  symname->value_ptr.symbol = sym;

  if (s->packname_present)
    free (s->value);

  free (s);

  if (pack == env->keyword_package)
    {
      sym->is_const = 1;
      sym->value_cell = symname;
      add_reference (symname, symname, 0);
    }

  new_cell = malloc_and_check (sizeof (*new_cell));
  new_cell->flags = vis;
  new_cell->sym = symname;
  increment_refcount (symname);
  new_cell->next = cell;

  pack->value_ptr.package->symtable [ind] = new_cell;

  num_symbols++;
  return symname;
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
  bin->is_symbol_macro = 0;
  bin->prev_special = -1;
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
chain_bindings (struct binding *bin, struct binding *env,
		int increment_dyn_bin_count, int *num, struct binding **last_bin)
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
      if (increment_dyn_bin_count && b->type == DYNAMIC_BINDING)
	b->sym->value_ptr.symbol->value_dyn_bins_num++;

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
remove_bindings (struct binding *env, int num, int decrement_dyn_bin_count)
{
  struct binding *b;

  if (!num)
    return env;  

  b = env->next;

  if (decrement_dyn_bin_count && env->type == DYNAMIC_BINDING
      && !(env->type & DELETED_BINDING))
    env->sym->value_ptr.symbol->value_dyn_bins_num--;

  if (env->prev_special >= 0)
    {
      if (env->sym)
	env->sym->value_ptr.symbol->is_special = env->prev_special;
      else
	env->closure_bin->sym->value_ptr.symbol->is_special = env->prev_special;
    }

  env->refcount--;

  if (!env->refcount)
    {
      if (env->sym)
	{
	  decrement_refcount (env->sym);
	  decrement_refcount (env->obj);
	}
      else
	{
	  env->closure_bin->refcount--;
	}

      free (env);
    }

  if (num == 1)
    return b;
  else
    return remove_bindings (b, num-1, decrement_dyn_bin_count);
}


struct binding *
find_binding (struct symbol *sym, struct binding *bins, enum binding_type type,
	      int bin_num, int only_lexical)
{
  while (bins && (type != LEXICAL_BINDING || bin_num))
    {
      if ((type == ANY_BINDING || bins->type & type)
	  && (bins->type == DYNAMIC_BINDING || bin_num)
	  && (bins->type == LEXICAL_BINDING || !only_lexical)
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
bind_variable (struct object *sym, struct object *val,
	       int increment_dyn_bin_count, struct binding *bins)
{
  struct binding *b;

  if (sym->value_ptr.symbol->is_parameter)
    {
      if (increment_dyn_bin_count)
	sym->value_ptr.symbol->value_dyn_bins_num++;

      increment_refcount (sym);
      return add_binding (create_binding (sym, val, DYNAMIC_BINDING, 0),
			  bins);
    }
  else
    {
      b = create_binding (sym, val, LEXICAL_BINDING, 0);
      b->prev_special = sym->value_ptr.symbol->is_special;
      sym->value_ptr.symbol->is_special = 0;

      increment_refcount (sym);
      return add_binding (b, bins);
    }
}


struct go_tag_frame *
collect_go_tags (struct object *body, struct go_tag_frame *stack,
		 int *found_tags)
{
  struct object *car, *destfind, *dest;
  struct go_tag_frame *ret = stack;

  *found_tags = 0;

  while (SYMBOL (body) != &nil_object)
    {
      car = CAR (body);

      if (IS_SYMBOL (car) || car->type == TYPE_INTEGER)
	{
	  destfind = CDR (body);

	  while (SYMBOL (destfind) != &nil_object && (dest = CAR (destfind))
		 && (IS_SYMBOL (dest) || dest->type == TYPE_INTEGER))
	    destfind = CDR (destfind);

	  if (!*found_tags)
	    {
	      ret = add_go_tag_frame (stack);
	      *found_tags = 1;
	    }

	  add_go_tag (car, destfind, ret);
	}

      body = CDR (body);
    }

  return ret;
}


struct go_tag_frame *
add_go_tag_frame (struct go_tag_frame *stack)
{
  struct go_tag_frame *f = malloc_and_check (sizeof (*f));

  f->refcount = 1;
  f->frame = NULL;
  f->next = stack;

  if (stack)
    stack->refcount++;

  return f;
}


void
add_go_tag (struct object *tagname, struct object *tagdest,
	    struct go_tag_frame *frame)
{
  struct go_tag *new = malloc_and_check (sizeof (*new));

  new->name = tagname;
  new->dest = tagdest;
  new->frame = frame;
  new->next = frame->frame;
  frame->frame = new;
}


struct go_tag_frame *
remove_go_tag_frame (struct go_tag_frame *stack)
{
  struct go_tag_frame *next = stack->next;
  struct go_tag *n, *t = stack->frame;

  stack->refcount--;

  if (!stack->refcount)
    {
      while (t)
	{
	  n = t->next;
	  free (t);
	  t = n;
	}

      free (stack);

      if (next)
	remove_go_tag_frame (next);
    }

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


struct block_frame *
add_block (struct object *name, struct block_frame *blocks)
{
  struct block *b = malloc_and_check (sizeof (*b));
  struct block_frame *f;

  b->name = name;
  b->refcount = 1;

  if (!blocks)
    {
      f = malloc_and_check (sizeof (*blocks));
      f->frame = b;
      b->next = NULL;
      f->next = blocks;
      blocks = f;
    }
  else
    {
      b->next = blocks->frame;
      blocks->frame = b;

      if (b->next)
	b->next->refcount++;
    }

  return blocks;
}


struct block *
remove_block (struct block *blocks)
{
  struct block *n = blocks->next;

  blocks->refcount--;

  if (!blocks->refcount)
    {
      free (blocks);

      if (n)
	remove_block (n);
    }

  return n;
}


struct object *
create_condition (struct object *type, struct object *args,
		  struct environment *env, struct outcome *outcome)
{
  struct object *ret;
  struct standard_object *so;

  if (!type->value_ptr.standard_class->class_precedence_list
      && !compute_class_precedence_list (type, outcome))
    {
      return NULL;
    }

  ret = alloc_object ();
  ret->type = TYPE_STANDARD_OBJECT;

  so = malloc_and_check (sizeof (*so));
  so->class = type;
  increment_refcount (type);
  so->fields = NULL;
  ret->value_ptr.standard_object = so;

  allocate_object_fields (ret, type);

  return fill_object_fields (ret, type, args, env, outcome);
}


struct object *
create_condition_by_c_string (char *type, struct object *args,
			      struct environment *env, struct outcome *outcome)
{
  struct object *sym = intern_symbol_by_char_vector (type, strlen (type), 1,
						     EXTERNAL_VISIBILITY, 0,
						     env->cl_package, 0, 0);

  return create_condition (sym->value_ptr.symbol->typespec, args, env, outcome);
}


int
does_condition_include_outcome_type (struct object *cond, enum outcome_type type,
				     struct environment *env)
{
  if (!IS_SYMBOL (cond))
    return 0;

  if ((SYMBOL (cond) == BUILTIN_SYMBOL ("TYPE-ERROR")
      && type == WRONG_TYPE_OF_ARGUMENT)
      || (SYMBOL (cond) == BUILTIN_SYMBOL ("DIVISION-BY-ZERO")
       && type == CANT_DIVIDE_BY_ZERO)
      || (SYMBOL (cond) == BUILTIN_SYMBOL ("ARITHMETIC-ERROR")
	  && type == CANT_DIVIDE_BY_ZERO)
      || (SYMBOL (cond) == BUILTIN_SYMBOL ("ERROR") && type > EVAL_OK))
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
					 EXTERNAL_VISIBILITY, 1, pack, 0, 0),
    *par, *rs;
  struct object_list *l;
  struct standard_class *cc;
  struct class_field_decl *f, *prev;

  va_start (valist, is_standard);

  sym->value_ptr.symbol->is_type = 1;
  sym->value_ptr.symbol->is_standard_type = is_standard;

  condcl = alloc_object ();
  condcl->type = TYPE_STANDARD_CLASS;

  cc = malloc_and_check (sizeof (*cc));
  condcl->value_ptr.standard_class = cc;

  increment_refcount (sym);
  cc->name = sym;
  cc->is_condition_class = 1;
  cc->parents = NULL;
  cc->descendants = NULL;
  cc->class_precedence_list = NULL;

  while ((s = va_arg (valist, char *)))
    {
      par = intern_symbol_by_char_vector (s, strlen (s), 1, INTERNAL_VISIBILITY,
					  1, pack, 0, 1);

      prepend_object_to_obj_list (par, &cc->parents);
    }

  cc->fields = NULL;

  while ((s = va_arg (valist, char *)))
    {
      par = intern_symbol_by_char_vector (s, strlen (s), 1, INTERNAL_VISIBILITY,
					  1, pack, 0, 0);

      f = create_class_field_decl (condcl, par, env, NULL);

      l = malloc_and_check (sizeof (*l));
      l->obj = intern_symbol_by_char_vector (s, strlen (s), 1,
					     EXTERNAL_VISIBILITY, 1,
					     env->keyword_package, 1, 0);
      increment_refcount (l->obj);
      l->next = NULL;
      f->initargs = l;

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
					 EXTERNAL_VISIBILITY, 1, pack, 0, 0);
      increment_refcount (rs);
      rs->value_ptr.symbol->function_cell = alloc_function ();
      rs->value_ptr.symbol->function_cell->value_ptr.function->
	condition_reader_class_name = sym;
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
  struct object *arg;

  if (!env->handlers)
    return &nil_object;

  b = env->handlers->frame;

  arg = alloc_empty_cons_pair ();
  arg->value_ptr.cons_pair->car = cond;
  add_reference (arg, cond, 0);
  arg->value_ptr.cons_pair->cdr = &nil_object;

  while (b)
    {
      if (is_subtype (cond->value_ptr.condition->class_name, b->condition, NULL,
		      env, outcome))
	{
	  f = env->handlers;
	  env->handlers = env->handlers->next;

	  hret = call_function (b->handler, arg, 1, 0, 1, 0, 0, env, outcome);

	  env->handlers = f;

	  if (!hret)
	    {
	      decrement_refcount (arg);
	      return NULL;
	    }

	  decrement_refcount (hret);
	}

      b = b->next;
    }

  decrement_refcount (arg);

  return &nil_object;
}


struct object *
list_lambda_list (struct parameter *par, int allow_other_keys,
		  struct environment *env)
{
  struct object *ret = NULL, *cons;
  enum parameter_type t = REQUIRED_PARAM;

  while (par)
    {
      if (!par->name)
	{
	  if (!ret)
	    ret = cons = alloc_empty_cons_pair ();
	  else
	    {
	      cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	      cons = CDR (cons);
	    }

	  cons->value_ptr.cons_pair->car =
	    list_lambda_list (par->sub_lambda_list, par->sub_allow_other_keys,
			      env);

	  par = par->next;
	  continue;
	}

      if (par->type == AUXILIARY_VAR && t < par->type && allow_other_keys)
	{
	  if (!ret)
	    ret = cons = alloc_empty_cons_pair ();
	  else
	    {
	      cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	      cons = CDR (cons);
	    }

	  cons->value_ptr.cons_pair->car = env->amp_allow_other_keys_sym;
	  add_reference (cons, CAR (cons), 0);
	}

      if (t < par->type)
	{
	  if (!ret)
	    ret = cons = alloc_empty_cons_pair ();
	  else
	    {
	      cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	      cons = CDR (cons);
	    }

	  cons->value_ptr.cons_pair->car
	    = par->type == OPTIONAL_PARAM ? env->amp_optional_sym :
	    par->type == REST_PARAM ? env->amp_rest_sym :
	    par->type == KEYWORD_PARAM ? env->amp_key_sym :
	    par->type == AUXILIARY_VAR ? env->amp_aux_sym : NULL;
	  add_reference (cons, CAR (cons), 0);
	}

      if (!ret)
	ret = cons = alloc_empty_cons_pair ();
      else
	{
	  cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	  cons = CDR (cons);
	}

      cons->value_ptr.cons_pair->car = par->name;
      add_reference (cons, CAR (cons), 0);

      t = par->type;
      par = par->next;
    }

  if (allow_other_keys && t == KEYWORD_PARAM)
    {
      if (!ret)
	ret = cons = alloc_empty_cons_pair ();
      else
	{
	  cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	  cons = CDR (cons);
	}

      cons->value_ptr.cons_pair->car = env->amp_allow_other_keys_sym;
      add_reference (cons, CAR (cons), 0);
    }

  if (!ret)
    ret = &nil_object;
  else
    cons->value_ptr.cons_pair->cdr = &nil_object;

  return ret;
}


struct object *
create_empty_condition_by_c_string (char *classname, struct environment *env)
{
  struct object *ret, *class, *class_name;
  struct standard_object *so;

  class_name = intern_symbol_by_char_vector (classname, strlen (classname), 0,
					     INTERNAL_VISIBILITY, 0,
					     env->cluser_package, 0, 0);

  class = SYMBOL (class_name)->value_ptr.symbol->typespec;

  if (!class->value_ptr.standard_class->class_precedence_list
      && !compute_class_precedence_list (class, NULL))
    {
      return NULL;
    }

  ret = alloc_object ();
  ret->type = TYPE_STANDARD_OBJECT;

  so = malloc_and_check (sizeof (*so));
  so->class = class;
  increment_refcount (class);
  so->fields = NULL;
  ret->value_ptr.standard_object = so;

  return allocate_object_fields (ret, class);
}


struct object *
raise_unbound_variable (struct object *sym, struct environment *env,
			struct outcome *outcome)
{
  struct object *cond = create_empty_condition_by_c_string ("UNBOUND-VARIABLE",
							    env), *ret;

  cond->value_ptr.standard_object->fields->value = sym;
  increment_refcount (sym);

  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      decrement_refcount (cond);
      return NULL;
    }

  return enter_debugger (cond, env, outcome);
}


struct object *
raise_undefined_function (struct object *sym, struct environment *env,
			  struct outcome *outcome)
{
  struct object *cond = create_empty_condition_by_c_string ("UNDEFINED-FUNCTION",
							    env), *ret;

  cond->value_ptr.standard_object->fields->value = sym;
  increment_refcount (sym);

  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      decrement_refcount (cond);
      return NULL;
    }

  return enter_debugger (cond, env, outcome);
}


struct object *
raise_type_error (struct object *datum, char *type, struct environment *env,
		  struct outcome *outcome)
{
  struct object *cond = create_empty_condition_by_c_string ("TYPE-ERROR", env),
    *ret;
  const char *b, *e;

  cond->value_ptr.standard_object->fields->value = datum;
  increment_refcount (datum);

  read_object (&cond->value_ptr.standard_object->fields->next->value, 0, type,
	       strlen (type), NULL, 0, 1, env, outcome, &b, &e);

  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      decrement_refcount (cond);
      return NULL;
    }

  return enter_debugger (cond, env, outcome);
}


struct object *
raise_file_error (struct object *fn, const char *fs, struct environment *env,
		  struct outcome *outcome)
{
  struct object *cond = create_empty_condition_by_c_string ("FILE-ERROR", env),
    *ret;

  if (!fn)
    {
      fn = create_filename (create_string_copying_c_string (fs));
      decrement_refcount (fn->value_ptr.filename->value);
    }
  else
    increment_refcount (fn);

  cond->value_ptr.standard_object->fields->value = fn;

  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      decrement_refcount (cond);
      return NULL;
    }

  return enter_debugger (cond, env, outcome);
}


struct object *
raise_al_maximum_stack_depth_exceeded (int maxdepth, struct environment *env,
				       struct outcome *outcome)
{
  struct object *cond =
    create_empty_condition_by_c_string ("AL-MAXIMUM-STACK-DEPTH-EXCEEDED", env),
    *ret, *depth;

  depth = create_integer_from_long (maxdepth);

  cond->value_ptr.standard_object->fields->value = depth;

  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      decrement_refcount (cond);
      return NULL;
    }

  return enter_debugger (cond, env, outcome);
}


struct object *
raise_al_wrong_number_of_arguments (int minargs, int maxargs,
				    struct environment *env,
				    struct outcome *outcome)
{
  struct object *cond =
    create_empty_condition_by_c_string ("AL-WRONG-NUMBER-OF-ARGUMENTS", env),
    *ret;

  cond->value_ptr.standard_object->fields->value =
    create_integer_from_long (minargs);
  cond->value_ptr.standard_object->fields->next->value =
    create_integer_from_long (maxargs);

  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      decrement_refcount (cond);
      return NULL;
    }

  return enter_debugger (cond, env, outcome);
}


struct object *
raise_program_error (struct environment *env, struct outcome *outcome)
{
  struct object *cond = create_empty_condition_by_c_string ("PROGRAM-ERROR",
							    env), *ret;

  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      decrement_refcount (cond);
      return NULL;
    }

  return enter_debugger (cond, env, outcome);
}


struct object *
raise_error (struct environment *env, struct outcome *outcome)
{
  struct object *cond = create_empty_condition_by_c_string ("ERROR", env),
    *ret;

  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      decrement_refcount (cond);
      return NULL;
    }

  return enter_debugger (cond, env, outcome);
}


struct object *
create_room_pair (char *sym, int val, struct environment *env)
{
  struct object *ret = alloc_empty_list (2);

  ret->value_ptr.cons_pair->car = BUILTIN_SYMBOL (sym);
  add_reference (ret, CAR (ret), 0);

  ret->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car =
    create_integer_from_long (val);

  return ret;
}


struct object *
create_pair (struct object *car, struct object *cdr)
{
  struct object *ret = alloc_empty_cons_pair ();

  ret->value_ptr.cons_pair->car = car;
  add_reference (ret, car, 0);

  ret->value_ptr.cons_pair->cdr = cdr;
  add_reference (ret, cdr, 1);

  return ret;
}


struct object *
dump_bindings (struct binding *bin, int lex_boundary, struct environment *env)
{
  struct object *ret = &nil_object, *cons, *l;
  int i = 0;

  while (bin)
    {
      if (ret == &nil_object)
	ret = cons = alloc_empty_cons_pair ();
      else
	cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

      cons->value_ptr.cons_pair->car = l = alloc_empty_list (3);
      cons->value_ptr.cons_pair->cdr = &nil_object;

      l->value_ptr.cons_pair->car = bin->sym ? bin->sym : bin->closure_bin->sym;
      add_reference (l, CAR (l), 0);

      l->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car
	= bin->sym ? bin->obj : bin->closure_bin->obj;
      add_reference (CDR (l), CAR (CDR (l)), 0);

      l->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr->
	value_ptr.cons_pair->car = bin->type == LEXICAL_BINDING
	? KEYWORD (":LEXICAL") : KEYWORD (":SPECIAL");
      add_reference (CDR (CDR (l)), CAR (CDR (CDR (l))), 0);

      if (bin->type == LEXICAL_BINDING && i < lex_boundary)
	{
	  l->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr->
	    value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	  l->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr->
	    value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = &t_object;
	  l->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr->
	    value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr = &nil_object;
	}

      i++;
      bin = bin->next;
    }

  return ret;
}


int
print_specializers_from_lambda_list (struct parameter *par,
				     struct environment *env, struct stream *str)
{
  if (write_to_stream (str, "(", 1) < 0)
    return -1;

  while (par && par->type == REQUIRED_PARAM)
    {
      if (print_object (par->typespec, env, str) < 0)
	return -1;

      if (par->next && par->next->type == REQUIRED_PARAM)
	{
	  if (write_to_stream (str, " ", 1) < 0)
	    return -1;
	}

      par = par->next;
    }

  if (write_to_stream (str, ")", 1) < 0)
    return -1;

  return 0;
}


void
print_function_name (struct object *func, struct environment *env)
{
  if (func->value_ptr.function->name)
    {
      if (func->value_ptr.function->is_setf_func)
	printf ("(SETF ");

      print_symbol (func->value_ptr.function->name, env, env->c_stdout->
		    value_ptr.stream);

      if (func->value_ptr.function->is_setf_func)
	printf (")");
    }
  else
    printf ("?");
}


void
print_method_description (struct object *meth, struct environment *env)
{
  switch (meth->value_ptr.method->qualifier)
    {
    case AROUND_METHOD:
      printf (":AROUND ");
      break;
    case BEFORE_METHOD:
      printf (":BEFORE ");
      break;
    case AFTER_METHOD:
      printf (":AFTER ");
      break;
    default:
      break;
    }

  print_specializers_from_lambda_list (meth->value_ptr.method->lambda_list,
				       env, env->c_stdout->value_ptr.stream);
}


void
print_bindings_in_reverse (struct binding *bins, int num,
			   struct environment *env, struct object *str)
{
  struct binding *b;
  int i;

  printf ("(");

  for (; num; num--)
    {
      b = bins;

      for (i = 1; i < num; i++)
	b = b->next;

      print_object (b->sym ? b->sym : b->closure_bin->sym, env,
		    str->value_ptr.stream);
      printf ("=");
      print_object (b->sym ? b->obj : b->closure_bin->obj, env,
		    str->value_ptr.stream);

      if (num > 1)
	printf (" ");
    }

  printf (")");
}


void
print_fields (struct object *stdobj, struct environment *env,
	      struct object *str)
{
  struct class_field *f = stdobj->value_ptr.standard_object->fields;

  if (!f)
    {
      printf ("NIL");
      return;
    }

  printf ("(");

  while (f)
    {
      print_object (f->decl->name, env, str->value_ptr.stream);

      if (f->value || (!f->name && f->decl->value))
	{
	  printf ("=");
	  print_object (f->name ? f->value : f->decl->value, env,
			str->value_ptr.stream);
	}

      if (f->next)
	printf (" ");

      f = f->next;
    }

  printf (")");
}


void
print_backtrace (struct environment *env, int be_verbose)
{
  int i = 0;
  struct call_frame *f = env->call_stack;
  struct binding *b;

  while (f)
    {
      if (!be_verbose && f->funcobj->type == TYPE_MACRO
	  && f->funcobj->value_ptr.macro->builtin_form)
	{
	  f = f->next;
	  continue;
	}

      printf ("%d: ", i);

      if (f->funcobj->type == TYPE_FUNCTION || f->funcobj->type == TYPE_MACRO)
	{
	  print_function_name (f->funcobj, env);
	  printf (" ");
	}
      else
	{
	  printf ("method ");
	  print_method_description (f->funcobj, env);
	  printf (" of ");
	  print_function_name (f->funcobj->value_ptr.method->generic_func, env);
	  printf (" ");
	}

      if (f->argsnum < 0)
	print_object (f->arglist, env, env->c_stdout->value_ptr.stream);
      else
	{
	  b = f->args;

	  if (!b)
	    printf ("NIL");
	  else
	    printf ("(");

	  while (b)
	    {
	      print_object (b->closure_bin->sym, env,
			    env->c_stdout->value_ptr.stream);
	      printf ("=");
	      print_object (b->closure_bin->obj, env,
			    env->c_stdout->value_ptr.stream);

	      if (b->next)
		printf (" ");
	      else
		printf (")");

	      b = b->next;
	    }
	}

      printf ("\n");

      if (f->next)
	printf ("\n");

      i++;
      f = f->next;
    }
}


struct object *
list_backtrace (struct environment *env, int be_verbose)
{
  struct object *ret = &nil_object, *cons;
  struct call_frame *f = env->call_stack;

  while (f)
    {
      if (!be_verbose && f->funcobj->type == TYPE_MACRO
	  && f->funcobj->value_ptr.macro->builtin_form)
	{
	  f = f->next;
	  continue;
	}

      if (ret == &nil_object)
	ret = cons = alloc_empty_cons_pair ();
      else
	{
	  cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	  cons = CDR (cons);
	}

      cons->value_ptr.cons_pair->car = f->arglist;
      add_reference (cons, CAR (cons), 0);
      cons->value_ptr.cons_pair->cdr = &nil_object;

      f = f->next;
    }

  return ret;
}


void
print_available_restarts (struct environment *env, int show_help,
			  struct object *str)
{
  struct restart_binding *r = env->restarts;
  int ind = 0;

  printf ("available restarts (from most recently established):\n");

  if (show_help)
    printf ("(select by number)\n"
	    "(type h or ? for help about stepping)\n");

  while (r)
    {
      printf ("%d: ", ind);
      print_object (r->name, env, str->value_ptr.stream);
      printf ("\n");

      ind++;
      r = r->next;
    }

  if (env->debugging_depth > 1)
    {
      printf ("%d: ABORT ONE LEVEL OF DEBUGGING\n", ind++);
    }

  printf ("%d: ABORT\n", ind++);

  str->value_ptr.stream->dirty_line = 0;
}


void
print_stepping_help (void)
{
  printf ("Debugger help:\n\n"
	  " h or ?  Print this help\n"
	  " c       Resume execution\n"
	  " n       Step over next form\n"
	  " s       Step inside next form\n"
	  " x       Expand macro\n"
	  " bt      Print backtrace\n\n");
}


struct object *
enter_debugger (struct object *cond, struct environment *env,
		struct outcome *outcome)
{
  struct object *obj, *result, *fun;
  struct object_list *vals;
  struct restart_binding *r = env->restarts;
  int end_repl = 0, restnum = 1, restind;
  const char *input_left;
  char *wholel = NULL;
  size_t input_left_s;


  env->debugging_depth++;

  while (r)
    {
      restnum++;
      r = r->next;
    }

  if (env->debugging_depth > 1)
    restnum++;

  fresh_line (env->c_stdout->value_ptr.stream);

  if (cond)
    {
      printf ("\nentered debugger with condition ");
      print_object (cond, env, env->c_stdout->value_ptr.stream);
      printf ("\nof fields ");
      print_fields (cond, env, env->c_stdout);
      printf ("\n(condition object bound to CL-USER:*AL-DEBUGGING-CONDITION*)\n\n");

      print_available_restarts (env, 1, env->c_stdout);
      printf ("\n");
    }
  else if (env->watched_obj)
    {
      if (env->watched_obj->type == TYPE_STANDARD_OBJECT)
	{
	  printf ("standard object ");
	  print_object (env->watched_obj, env, env->c_stdout->value_ptr.stream);
	  printf (" changing field ");
	  print_object (env->obj_field, env, env->c_stdout->value_ptr.stream);
	  printf (" to ");
	  print_object (env->new_value, env, env->c_stdout->value_ptr.stream);
	  printf ("\n\n");
	}
      else if (env->watched_obj->type == TYPE_HASHTABLE)
	{
	  printf ("hash table ");
	  print_object (env->watched_obj, env, env->c_stdout->value_ptr.stream);

	  if (env->obj_field && env->new_value)
	    {
	      printf (" setting key ");
	      print_object (env->obj_field, env, env->c_stdout->value_ptr.stream);
	      printf (" to ");
	      print_object (env->new_value, env, env->c_stdout->value_ptr.stream);
	      printf ("\n\n");
	    }
	  else if (env->obj_field)
	    {
	      printf (" clearing key ");
	      print_object (env->obj_field, env, env->c_stdout->value_ptr.stream);
	      printf ("\n\n");
	    }
	  else
	    {
	      printf (" clearing completely\n\n");
	    }
	}

      env->watched_obj = env->obj_field = env->new_value = NULL;
    }
  else if (env->stepping_flags)
    {
      printf ("\n");
      print_object (env->next_eval, env, env->c_stdout->value_ptr.stream);

      if (env->next_eval->type == TYPE_CONS_PAIR
	  && IS_SYMBOL (CAR (env->next_eval))
	  && is_macro (SYMBOL (CAR (env->next_eval)), env))
	{
	  printf (" (macro)\n");
	}
      else
	printf ("\n");
    }

  env->c_stdout->value_ptr.stream->dirty_line = 0;


  if (cond)
    increment_refcount (cond);

  env->vars = bind_variable (env->al_debugging_condition_sym,
			     cond ? cond : &nil_object, 1, env->vars);
  env->lex_env_vars_boundary++;

  while (!end_repl)
    {
      free (wholel);
      env->stack_depth = 0;
      obj = read_object_interactively (env, outcome, &input_left, &input_left_s,
				       &wholel);

      if (!obj && !IS_READ_OR_EVAL_ERROR (outcome->type))
	{
	  obj = env->last_command;

	  if (obj)
	    increment_refcount (obj);
	}
      else if (!obj)
	outcome->type = NO_OBJECT;

      while (obj)
	{
	  decrement_refcount (env->last_command);
	  env->last_command = obj;
	  increment_refcount (obj);

	  if (obj->type == TYPE_INTEGER
	      && mpz_cmp_si (obj->value_ptr.integer, 0) >= 0
	      && mpz_cmp_si (obj->value_ptr.integer, restnum) < 0)
	    {
	      restind = mpz_get_si (obj->value_ptr.integer);

	      if (env->debugging_depth > 1 && restind == restnum-2)
		{
		  printf ("\n");
		  env->debugging_depth--;
		  outcome->type = ABORT_ONE_LEVEL;
		  free (wholel);
		  decrement_refcount (obj);
		  env->vars = remove_bindings (env->vars, 1, 1);
		  env->lex_env_vars_boundary--;
		  return NULL;
		}
	      else if (restind == restnum-1)
		{
		  printf ("\n");
		  env->debugging_depth--;
		  outcome->type = ABORT_TO_TOP_LEVEL;
		  free (wholel);
		  decrement_refcount (obj);
		  env->vars = remove_bindings (env->vars, 1, 1);
		  env->lex_env_vars_boundary--;
		  return NULL;
		}
	      else
		{
		  r = env->restarts;

		  while (restind)
		    {
		      r = r->next;
		      restind--;
		    }

		  result = call_function (r->restart, &nil_object, 0, 0, 1, 0, 0,
					  env, outcome);
		}
	    }
	  else if (IS_SYMBOL (obj)
		   && (symbol_equals (obj, "H", env)
		       || symbol_equals (obj, "?", env)
		       || symbol_equals (obj, "C", env)
		       || symbol_equals (obj, "N", env)
		       || symbol_equals (obj, "X", env)
		       || symbol_equals (obj, "S", env)
		       || symbol_equals (obj, "BT", env)))
	    {
	      if (symbol_equals (obj, "H", env) || symbol_equals (obj, "?", env))
		{
		  print_stepping_help ();
		  result = &nil_object;
		  outcome->no_value = 1;
		}
	      else if (symbol_equals (obj, "BT", env))
		{
		  print_backtrace (env, 0);
		  result = &nil_object;
		  outcome->no_value = 1;
		}
	      else
		{
		  if (symbol_equals (obj, "C", env))
		    env->stepping_flags = 0;
		  else if (symbol_equals (obj, "N", env))
		    env->stepping_flags = STEP_OVER_FORM;
		  else if (symbol_equals (obj, "X", env))
		    {
		      if (env->next_eval
			  && env->next_eval->type == TYPE_CONS_PAIR
			  && IS_SYMBOL (CAR (env->next_eval))
			  && (fun = get_function (SYMBOL (CAR (env->next_eval)),
						  env, 0, 0, 0, 0))
			  && fun->type == TYPE_MACRO
			  && !fun->value_ptr.macro->builtin_form)
			{
			  env->stepping_flags = STEP_OVER_EXPANSION;
			}
		      else
			env->stepping_flags = STEP_OVER_FORM;
		    }
		  else if (symbol_equals (obj, "S", env))
		    env->stepping_flags = STEP_INSIDE_FORM;

		  env->debugging_depth--;
		  free (wholel);
		  decrement_refcount (obj);
		  env->vars = remove_bindings (env->vars, 1, 1);
		  env->lex_env_vars_boundary--;
		  return &nil_object;
		}
	    }
	  else
	    {
	      env->stepping_flags = 0;
	      result = evaluate_object (obj, env, outcome);
	    }

	  if (!result && (outcome->tag_to_jump_to || outcome->block_to_leave))
	    {
	      env->debugging_depth--;
	      env->vars = remove_bindings (env->vars, 1, 1);
	      env->lex_env_vars_boundary--;
	      return NULL;
	    }

	  if (!result)
	    {
	      if (outcome->condition)
		{
		  if (is_subtype_by_char_vector
		      (outcome->condition->value_ptr.condition->class_name,
		       "SIMPLE-CONDITION", env)
		      || is_subtype_by_char_vector
		      (outcome->condition->value_ptr.condition->class_name,
		       "TYPE-ERROR", env))
		    {
		      print_object (outcome->condition->value_ptr.condition->
				    fields->value, env,
				    env->c_stdout->value_ptr.stream);
		      fresh_line (env->c_stdout->value_ptr.stream);
		    }
		  else
		    {
		      printf ("unhandled condition of type ");
		      print_object (outcome->condition->value_ptr.condition->
				    class_name, env,
				    env->c_stdout->value_ptr.stream);
		      fresh_line (env->c_stdout->value_ptr.stream);
		    }

		  decrement_refcount (outcome->condition);
		  outcome->condition = NULL;
		}
	      else if (outcome->type == ABORT_TO_TOP_LEVEL)
		{
		  env->debugging_depth--;
		  free (wholel);
		  decrement_refcount (result);
		  decrement_refcount (obj);
		  env->vars = remove_bindings (env->vars, 1, 1);
		  env->lex_env_vars_boundary--;
		  return NULL;
		}
	      else if (outcome->type != ABORT_ONE_LEVEL)
		{
		  print_error (outcome, env);
		}
	    }
	  else if (outcome->no_value)
	    {
	      fresh_line (env->c_stdout->value_ptr.stream);
	      outcome->no_value = 0;
	    }
	  else
	    {
	      fresh_line (env->c_stdout->value_ptr.stream);

	      print_object (result, env, env->c_stdout->value_ptr.stream);
	      printf ("\n");
	      env->c_stdout->value_ptr.stream->dirty_line = 0;

	      vals = outcome->other_values;

	      while (vals)
		{
		  print_object (vals->obj, env, env->c_stdout->value_ptr.stream);
		  printf ("\n");
		  env->c_stdout->value_ptr.stream->dirty_line = 0;
		  vals = vals->next;
		}

	      free_object_list (outcome->other_values);

	      outcome->other_values = NULL;
	    }

	  decrement_refcount (result);
	  decrement_refcount (obj);

	  env->stack_depth = 0;

	  if (input_left && input_left_s > 0)
	    obj = read_object_interactively_continued (input_left, input_left_s,
						       env, outcome, &input_left,
						       &input_left_s, &wholel);
	  else
	    obj = NULL;
	}
    }


  env->debugging_depth--;
  env->vars = remove_bindings (env->vars, 1, 1);
  env->lex_env_vars_boundary--;

  return &t_object;
}


void
add_profiling_data (struct profiling_record **data, struct object *name,
		    int is_setf, clock_t time, clock_t evaltime)
{
  int ind = hash_object (name, PROFILING_HASHTABLE_SIZE);
  struct profiling_record *r = data [ind];

  while (r)
    {
      if (r->name == name && !is_setf == !r->is_setf)
	break;

      r = r->next;
    }

  if (!r)
    {
      r = malloc_and_check (sizeof (*r));
      r->name = name;
      r->is_setf = is_setf;
      r->counter = 0;
      r->time = 0;
      r->next = data [ind];
      data [ind] = r;
    }

  r->counter += 1;
  r->time += time + evaltime;
}


struct call_frame *
add_call_frame (struct object *funcobj, struct object *args,
		struct environment *env, int argsnum, struct call_frame *stack)
{
  struct call_frame *ret = malloc_and_check (sizeof (*ret));
  struct binding *b, *bin;

  increment_refcount (funcobj);
  ret->funcobj = funcobj;
  ret->argsnum = argsnum;
  ret->next = stack;

  increment_refcount (args);
  ret->arglist = args;

  ret->args = NULL;

  if (argsnum >= 0)
    {
      b = env->vars;

      for (; argsnum; argsnum--)
	{
	  while (!b->sym)
	    {
	      b = b->closure_bin;
	    }

	  bin = malloc_and_check (sizeof (*bin));
	  bin->type = LEXICAL_BINDING;
	  bin->refcount = 0;
	  bin->sym = NULL;
	  bin->obj = NULL;
	  bin->closure_bin = b;
	  bin->next = ret->args;
	  ret->args = bin;

	  b->refcount++;

	  b = b->next;
	}
    }

  return ret;
}


struct call_frame *
remove_call_frame (struct call_frame *stack)
{
  struct call_frame *ret = stack->next;
  struct binding *b, *n;

  decrement_refcount (stack->funcobj);

  if (stack->arglist)
    decrement_refcount (stack->arglist);

  if (stack->argsnum >= 0)
    {
      b = stack->args;

      while (b)
	{
	  b->closure_bin->refcount--;

	  if (!b->closure_bin->refcount)
	    {
	      decrement_refcount (b->closure_bin->sym);
	      decrement_refcount (b->closure_bin->obj);
	      free (b->closure_bin);
	    }

	  n = b->next;
	  free (b);
	  b = n;
	}
    }

  free (stack);

  return ret;
}


struct read_label *
add_read_label (int label, struct object *value, struct read_label *labels)
{
  struct read_label *lb = malloc_and_check (sizeof (*lb));
  lb->label = label;
  lb->value = value;
  lb->next = labels;

  return lb;
}


void
clear_read_labels (struct read_label **label)
{
  if (*label)
    {
      clear_read_labels (&(*label)->next);
      free (*label);
      *label = NULL;
    }
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
						     pack, 0, 0);
  struct object *par;

  va_start (valist, is_standard);

  sym->value_ptr.symbol->is_type = 1;
  sym->value_ptr.symbol->is_standard_type = is_standard;
  sym->value_ptr.symbol->builtin_type = builtin_type;

  while ((s = va_arg (valist, char *)))
    {
      par = intern_symbol_by_char_vector (s, strlen (s), 1,
					  EXTERNAL_VISIBILITY, 1, pack, 0, 0);

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
						     pack, 0, 0);
  struct object *fun = alloc_function ();
  struct function *f = fun->value_ptr.function;

  fun->type = type;

  sym->value_ptr.symbol->function_cell = fun;

  f->name = sym;
  add_reference (fun, sym, 0);

  f->is_special_operator = is_special_operator;
  f->builtin_form = builtin_form;
  f->flags = COMPILED_FUNCTION;

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
define_generic_function (char *name, struct environment *env,
			 struct parameter *lambda_list,
			 struct object *(*default_method)
			 (struct object *list, struct environment *env,
			  struct outcome *outcome))
{
  struct object *pack = inspect_variable (env->package_sym, env),
    *sym = intern_symbol_by_char_vector (name, strlen (name), 1,
					 EXTERNAL_VISIBILITY, 1, pack, 0, 0),
    *fun = alloc_function (), *meth;
  struct function *f = fun->value_ptr.function;
  struct method *m;
  struct method_list *ml;

  fun->type = TYPE_FUNCTION;

  sym->value_ptr.symbol->function_cell = fun;

  f->name = sym;
  add_reference (fun, sym, 0);

  f->lambda_list = lambda_list;

  f->flags = GENERIC_FUNCTION;

  meth = alloc_object ();
  meth->type = TYPE_METHOD;
  m = meth->value_ptr.method = malloc_and_check (sizeof (*m));
  m->generic_func = fun;
  add_reference (meth, fun, 0);
  m->qualifier = PRIMARY_METHOD;
  m->lambda_list = copy_lambda_list (lambda_list, 1);
  m->builtin_method = default_method;
  m->body = NULL;

  ml = malloc_and_check (sizeof (*ml));
  ml->reference_strength_factor = 0;
  ml->meth = meth;
  ml->next = fun->value_ptr.function->methods;

  fun->value_ptr.function->methods = ml;

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

  if (!sym->value_ptr.symbol->is_parameter)
    {
      sym->value_ptr.symbol->is_parameter = 1;
      sym->value_ptr.symbol->is_special++;
    }

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
						     pack, 0, 0);

  sym->value_ptr.symbol->is_const = 1;
  sym->value_ptr.symbol->value_cell = value;
  mark_as_constant (value);

  return sym;
}


struct object *
define_variable (char *name, struct object *value, struct environment *env)
{
  struct object *pack = inspect_variable (env->package_sym, env);
  struct object *sym = intern_symbol_by_char_vector (name, strlen (name), 1,
						     EXTERNAL_VISIBILITY, 1,
						     pack, 0, 0);

  sym->value_ptr.symbol->is_parameter = 1;
  sym->value_ptr.symbol->is_special = 1;
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
  else if (seq->type == TYPE_BITARRAY)
    {
      return create_integer_from_long
	(mpz_tstbit (seq->value_ptr.bitarray->value, ind));
    }
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
  else if (seq->type == TYPE_BITARRAY)
    {
      if (is_zero (val))
	mpz_clrbit (seq->value_ptr.bitarray->value, ind);
      else
	mpz_setbit (seq->value_ptr.bitarray->value, ind);
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
      else if (SYMBOL (list) == &nil_object)
	return &nil_object;
      else
	return NULL;
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

      cons = CDR (cons);

      if (prefix)
	{
	  cons->value_ptr.cons_pair->car = copy_prefix (prefix, NULL, &lastpref);

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
  else
    cons->value_ptr.cons_pair->cdr = &nil_object;

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


fixnum
array_row_major_index (const struct array_size *ind, const struct array_size *sz)
{
  fixnum ret = 0, totsize = array_total_size (sz);

  while (ind)
    {
      totsize /= sz->size;
      ret += ind->size*totsize;

      ind = ind->next;
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
clear_hash_record (struct object *hasht, struct hashtable_record *rec, int ind,
		   int depth)
{
  if (rec->next)
    {
      clear_hash_record (hasht, rec->next, ind, depth+1);
    }

  delete_reference (hasht, rec->key, ind+depth*2*LISP_HASHTABLE_SIZE);
  delete_reference (hasht, rec->value, ind+(depth*2+1)*LISP_HASHTABLE_SIZE);

  free (rec);
}


void
clear_hash_table (struct object *hasht)
{
  size_t i;
  struct hashtable_record *r;

  for (i = 0; i < LISP_HASHTABLE_SIZE; i++)
    {
      r = hasht->value_ptr.hashtable->table [i];

      if (r)
	clear_hash_record (hasht, r, i, 0);

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
  struct object *car, *eqlobj, *eqlts;
  struct parameter *first = NULL;

  *last = NULL;

  while (obj && obj->type == TYPE_CONS_PAIR)
    {
      car = CAR (obj);

      if (IS_LIST (car) && allow_destructuring)
	{
	  if (!first)
	    *last = first = alloc_parameter (REQUIRED_PARAM, NULL);
	  else
	    *last = (*last)->next = alloc_parameter (REQUIRED_PARAM, NULL);

	  outcome->type = EVAL_OK;
	  (*last)->sub_lambda_list = parse_lambda_list (car, 1, 0, env, outcome,
							&(*last)->
							sub_found_amp_key,
							&(*last)->
							sub_allow_other_keys);

	  if (outcome->type != EVAL_OK)
	    return NULL;
	}
      else if (car->type == TYPE_CONS_PAIR && is_specialized)
	{
	  if (list_length (car) != 2 || !IS_SYMBOL (CAR (car))
	      || (!IS_SYMBOL (CAR (CDR (car)))
		  && CAR (CDR (car))->type != TYPE_STANDARD_CLASS
		  && CAR (CDR (car))->type != TYPE_CONDITION_CLASS
		  && (CAR (CDR (car))->type != TYPE_CONS_PAIR
		      || list_length (CAR (CDR (car))) != 2
		      || SYMBOL (CAR (CAR (CDR (car)))) != env->eql_sym)))
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

	  if (IS_SYMBOL (CAR (CDR (car))))
	    {
	      (*last)->typespec = SYMBOL (CAR (CDR (car)));
	      increment_refcount ((*last)->typespec);
	    }
	  else if (CAR (CDR (car))->type == TYPE_CONS_PAIR)
	    {
	      eqlobj = evaluate_object (CAR (CDR (CAR (CDR (car)))), env,
					outcome);

	      if (!eqlobj)
		return NULL;

	      eqlts = alloc_empty_cons_pair ();
	      eqlts->value_ptr.cons_pair->car = env->eql_sym;
	      add_reference (eqlts, CAR (eqlts), 0);
	      eqlts->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	      eqlts->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = eqlobj;
	      add_reference (CDR (eqlts), CAR (CDR (eqlts)), 0);
	      decrement_refcount (eqlobj);
	      eqlts->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr
		= &nil_object;

	      (*last)->typespec = eqlts;
	    }
	  else
	    {
	      (*last)->typespec = CAR (CDR (car));
	      increment_refcount ((*last)->typespec);
	    }
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
					      env->keyword_package, 1, 0);

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
		 1, EXTERNAL_VISIBILITY, 1, env->keyword_package, 1, 0);
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
		   struct outcome *outcome, int *found_amp_key,
		   int *allow_other_keys)
{
  struct parameter *first = NULL, *last = NULL, *p, *ls;
  struct object *car, *restsym, *wholesym;
  int dotted_ok = 0, l;

  *found_amp_key = 0, *allow_other_keys = 0;

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

  if ((p = parse_required_parameters (obj, &ls, &obj, allow_destructuring,
				      is_specialized, env, outcome)))
    {
      if (first)
	last->next = p;
      else
	first = p;

      last = ls;
    }

  if (outcome->type != EVAL_OK)
    return NULL;

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && SYMBOL (car) == env->amp_optional_sym)
    {
      if ((p = parse_optional_parameters (CDR (obj), &ls, &obj, env, outcome)))
	{
	  if (first)
	    last->next = p;
	  else
	    first = p;

	  last = ls;
	}

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
	  && (SYMBOL (CDR (obj)) == &nil_object
	      || (!IS_SYMBOL (CAR (CDR (obj)))
		  && (CAR (CDR (obj))->type != TYPE_CONS_PAIR
		      || !allow_destructuring))))
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
	}

      if (obj->type != TYPE_CONS_PAIR && !IS_SYMBOL (obj))
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return NULL;
	}

      if (obj->type == TYPE_CONS_PAIR && IS_LIST (CAR (CDR (obj))))
	{
	  if (!first)
	    last = first = alloc_parameter (REST_PARAM, NULL);
	  else
	    last = last->next = alloc_parameter (REST_PARAM, NULL);

	  outcome->type = EVAL_OK;
	  last->sub_lambda_list = parse_lambda_list (CAR (CDR (obj)), 1, 0, env,
						     outcome,
						     &last->
						     sub_found_amp_key,
						     &last->
						     sub_allow_other_keys);

	  if (outcome->type != EVAL_OK)
	    return NULL;

	  obj = CDR (CDR (obj));
	}
      else
	{
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
    }

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && SYMBOL (car) == env->amp_key_sym)
    {
      *found_amp_key = 1;

      if ((p = parse_keyword_parameters (CDR (obj), &ls, &obj, env, outcome)))
	{
	  if (first)
	    last->next = p;
	  else
	    first = p;

	  last = ls;
	}

      if (outcome->type != EVAL_OK)
	return NULL;
    }

  if (obj && obj->type == TYPE_CONS_PAIR && (car = CAR (obj))
      && SYMBOL (car) == env->amp_allow_other_keys_sym)
    {
      if (!*found_amp_key)
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


struct parameter *
create_lambda_list (struct environment *env, ...)
{
  struct parameter *ret = NULL, *par;
  struct object *pack = inspect_variable (env->package_sym, env);
  va_list valist;
  char *n;

  va_start (valist, env);

  while ((n = va_arg (valist, char *)))
    {
      if (ret)
	par = par->next
	  = alloc_parameter (REQUIRED_PARAM,
			     intern_symbol_by_char_vector (n, strlen (n), 1,
							   INTERNAL_VISIBILITY,
							   1, pack, 0, 0));
      else
	ret = par =
	  alloc_parameter (REQUIRED_PARAM,
			   intern_symbol_by_char_vector (n, strlen (n), 1,
							 INTERNAL_VISIBILITY, 1,
							 pack, 0, 0));

      par->reference_strength_factor = !STRENGTH_FACTOR_OF_OBJECT (par->name);
      INC_WEAK_REFCOUNT (par->name);
    }

  va_end (valist);

  return ret;
}


struct parameter *
copy_lambda_list (struct parameter *list, int fill_typespecs_with_t)
{
  struct parameter *ret = NULL, *par;

  while (list)
    {
      if (ret)
	par = par->next = alloc_parameter (list->type, list->name);
      else
	ret = par = alloc_parameter (list->type, list->name);

      par->reference_strength_factor = !STRENGTH_FACTOR_OF_OBJECT (list->name);
      INC_WEAK_REFCOUNT (list->name);

      if (fill_typespecs_with_t)
	par->typespec = &t_object;

      list = list->next;
    }

  if (ret)
    par->next = NULL;

  return ret;
}


void
count_parameters (struct parameter *par, int *req_params, int *opt_params,
		  int *rest)
{
  *req_params = 0, *opt_params = 0, *rest = 0;

  while (par)
    {
      if (par->type == REQUIRED_PARAM)
	(*req_params)++;

      if (par->type == OPTIONAL_PARAM)
	(*opt_params)++;

      if (par->type == REST_PARAM)
	*rest = 1;

      par = par->next;
    }
}


int
are_lambda_lists_congruent (struct parameter *meth_list, int meth_has_amp_key,
			    struct parameter *gen_list, int gen_has_amp_key)
{
  int req1, req2, opt1, opt2, r1, r2;

  if (!gen_list)
    return 1;

  count_parameters (meth_list, &req1, &opt1, &r1);
  count_parameters (gen_list, &req2, &opt2, &r2);

  if (req1 == req2 && opt1 == opt2
      && (r1 || meth_has_amp_key) == (r2 || gen_has_amp_key))
    return 1;

  return 0;
}


int
parse_declaration_specifier (struct object *spec, int is_local,
			     struct environment *env, int bin_num,
			     struct outcome *outcome)
{
  struct object *form, *declid, *sym;
  struct binding *vars;
  int bn;

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
	      bn = bin_num;

	      for (; bn; bn--)
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
	      if (!SYMBOL (CAR (form))->value_ptr.symbol->is_parameter)
		{
		  SYMBOL (CAR (form))->value_ptr.symbol->is_parameter = 1;
		  SYMBOL (CAR (form))->value_ptr.symbol->is_special++;
		}
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
  else if (declid != env->dynamic_extent_sym)
    {
      outcome->type = UNKNOWN_DECLARATION;
      return 0;
    }

  return 1;
}


int
parse_declarations (struct object *body, struct environment *env, int bin_num,
		    int allow_docstring, struct outcome *outcome,
		    struct object **next)
{
  struct object *forms;

  *next = body;

  while (SYMBOL (*next) != &nil_object)
    {
      if (allow_docstring && CAR (*next)->type == TYPE_STRING
	  && SYMBOL (CDR (*next)) != &nil_object)
	{
	  allow_docstring = 0;
	  *next = CDR (*next);
	  continue;
	}

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
  int found_string = 0;

  while (SYMBOL (decl) != &nil_object
	 && ((CAR (decl)->type == TYPE_CONS_PAIR
	      && SYMBOL (CAR (CAR (decl))) == env->declare_sym)
	     || (!found_string && CAR (decl)->type == TYPE_STRING)))
    {
      if (CAR (decl)->type == TYPE_STRING)
	{
	  found_string = 1;
	  decl = CDR (decl);
	  continue;
	}

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
evaluate_body (struct object *body, int parse_decls, int is_tagbody,
	       int collect_tags, struct object *block_name,
	       struct environment *env, struct outcome *outcome)
{
  struct object *res = &nil_object, *firstdecl = body;
  int found_tags;

  if (parse_decls >= 0)
    {
      if (!parse_declarations (body, env, parse_decls, 0, outcome, &body))
	return NULL;
    }

  if (collect_tags)
    {
      env->go_tag_stack = collect_go_tags (body, env->go_tag_stack, &found_tags);
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
		  if (is_tagbody)
		    {
		      if (outcome->tag_to_jump_to->frame
			  == env->go_tag_stack)
			{
			  body = outcome->tag_to_jump_to->dest;
			  outcome->tag_to_jump_to = NULL;
			  res = &nil_object;
			}
		      else
			goto cleanup_and_leave;
		    }
		  else
		    goto cleanup_and_leave;
		}
	      else if (outcome->block_to_leave)
		{
		  if (block_name && outcome->block_to_leave == env->blocks->frame)
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
  if (collect_tags && found_tags)
    {
      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);
    }

  if (block_name)
    env->blocks->frame = remove_block (env->blocks->frame);

  undo_special_declarations (firstdecl, env);

  return res;
}


int
parse_argument_list (struct object *arglist, struct parameter *par,
		     int eval_args, int also_pass_name, int is_typespec,
		     int found_amp_key, int allow_other_keys,
		     struct binding *lex_vars, int create_new_lex_env,
		     struct environment *env, struct outcome *outcome,
		     struct binding **bins, int *argsnum, int *closnum)
{
  struct parameter *findk, *opts = NULL;
  struct object *val, *args = NULL, *as, *key_allow_other_k = NULL;
  struct binding *subbins, *lastbin = NULL;
  int rest_found = 0, subargs, found_unknown_key = 0, binsnum, subclos,
    prev_lex_bin_num = env->lex_env_vars_boundary, reqpars = 0, optpars = 0;

  *bins = NULL, *argsnum = 0;


  if (par && par->type == WHOLE_PARAM)
    {
      increment_refcount (arglist);
      *bins = bind_variable (par->name, arglist, 0, *bins);

      (*argsnum)++;

      par = par->next;
    }

  if (also_pass_name && SYMBOL (arglist) != &nil_object)
    arglist = CDR (arglist);

  while (SYMBOL (arglist) != &nil_object && par
	 && (par->type == REQUIRED_PARAM || par->type == OPTIONAL_PARAM))
    {
      if (par->type == REQUIRED_PARAM)
	reqpars++;
      else
	optpars++;

      if (!par->name)
	{
	  if (!IS_LIST (CAR (arglist)))
	    {
	      outcome->type = MISMATCH_IN_DESTRUCTURING_CALL;
	      return 0;
	    }

	  if (!parse_argument_list (CAR (arglist), par->sub_lambda_list,
				    eval_args, 0, is_typespec,
				    par->sub_found_amp_key,
				    par->sub_allow_other_keys, NULL, 0, env,
				    outcome, &subbins, &subargs, &subclos))
	    {
	      remove_bindings (*bins, *argsnum, 0);
	      return 0;
	    }

	  *argsnum += subargs;
	}
      else
	{
	  if (eval_args)
	    {
	      val = evaluate_object (CAR (arglist), env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!val)
		{
		  remove_bindings (*bins, *argsnum, 0);
		  return 0;
		}
	    }
	  else
	    {
	      increment_refcount (CAR (arglist));
	      val = CAR (arglist);
	    }

	  *bins = bind_variable (par->name, val, 0, *bins);

	  (*argsnum)++;

	  if (par->type == OPTIONAL_PARAM && par->supplied_p_param)
	    {
	      *bins = bind_variable (par->supplied_p_param, &t_object, 0, *bins);

	      (*argsnum)++;
	    }
	}

      par = par->next;

      arglist = CDR (arglist);
    }

  if ((par && par->type == REQUIRED_PARAM)
      || (SYMBOL (arglist) != &nil_object
	  && !found_amp_key && (!par || par->type != REST_PARAM)))
    {
      while (par && (par->type == REQUIRED_PARAM || par->type == OPTIONAL_PARAM))
	{
	  if (par->type == REQUIRED_PARAM)
	    reqpars++;
	  else
	    optpars++;

	  par = par->next;
	}

      remove_bindings (*bins, *argsnum, 0);
      raise_al_wrong_number_of_arguments (reqpars, reqpars+optpars, env, outcome);
      return 0;
    }


  if (par && par->type == OPTIONAL_PARAM)
    opts = par;

  while (par && par->type == OPTIONAL_PARAM)
    par = par->next;


  if (found_amp_key || (par && (par->type == REST_PARAM) && par->name))
    {
      if (eval_args)
	{
	  args = evaluate_through_list (arglist, env, outcome);

	  if (!args)
	    {
	      remove_bindings (*bins, *argsnum, 0);
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
      if (!par->name)
	{
	  if (!IS_LIST (arglist))
	    {
	      remove_bindings (*bins, *argsnum, 0);
	      outcome->type = MISMATCH_IN_DESTRUCTURING_CALL;
	      return 0;
	    }

	  if (!parse_argument_list (arglist, par->sub_lambda_list, eval_args, 0,
				    is_typespec, par->sub_found_amp_key,
				    par->sub_allow_other_keys, NULL, 0, env,
				    outcome, &subbins, &subargs, &subclos))
	    {
	      remove_bindings (*bins, *argsnum, 0);
	      return 0;
	    }

	  *argsnum += subargs;
	}
      else
	{
	  *bins = bind_variable (par->name, args, 0, *bins);

	  (*argsnum)++;

	  par = par->next;

	  rest_found = 1;
	}
    }

  as = args;

  if (found_amp_key)
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
		  remove_bindings (*bins, *argsnum, 0);
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
	      remove_bindings (*bins, *argsnum, 0);
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return 0;
	    }

	  if (!findk->key_passed)
	    {
	      increment_refcount (CAR (as));

	      *bins = bind_variable (findk->name, CAR (as), 0, *bins);
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
    }

  if (args && !rest_found)
    decrement_refcount (args);

  if (found_unknown_key && !allow_other_keys
      && (!key_allow_other_k || SYMBOL (key_allow_other_k) == &nil_object))
    {
      remove_bindings (*bins, *argsnum, 0);
      return raise_program_error (env, outcome) ? 1 : 0;
    }


  restore_lexical_environment (env, lex_vars, NULL, closnum);
  binsnum = *argsnum + *closnum;

  env->vars = chain_bindings (*bins, env->vars, 1, NULL, &lastbin);

  if (create_new_lex_env)
    env->lex_env_vars_boundary = binsnum;
  else
    env->lex_env_vars_boundary += binsnum;


  while (opts && opts->type == OPTIONAL_PARAM)
    {
      if (opts->init_form)
	{
	  val = evaluate_object (opts->init_form, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!val)
	    {
	      env->vars = remove_bindings (env->vars, *argsnum, 1);
	      env->lex_env_vars_boundary = prev_lex_bin_num;
	      return 0;
	    }

	  env->vars = bind_variable (opts->name, val, 1, env->vars);
	  env->lex_env_vars_boundary++, (*argsnum)++;
	}
      else
	{
	  if (is_typespec)
	    increment_refcount (env->star_sym);

	  env->vars = bind_variable (opts->name, is_typespec ? env->star_sym
				     : &nil_object, 1, env->vars);
	  env->lex_env_vars_boundary++, (*argsnum)++;
	}

      if (opts->supplied_p_param)
	{
	  env->vars = bind_variable (opts->supplied_p_param, &nil_object, 1,
				     env->vars);
	  env->lex_env_vars_boundary++, (*argsnum)++;
	}

      opts = opts->next;
    }

  while (par && par->type == KEYWORD_PARAM)
    {
      if (!par->key_passed)
	{
	  if (par->init_form)
	    {
	      val = evaluate_object (par->init_form, env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!val)
		{
		  env->vars = remove_bindings (env->vars, *argsnum, 1);
		  env->lex_env_vars_boundary = prev_lex_bin_num;
		  return 0;
		}
	    }
	  else
	    val = &nil_object;

	  env->vars = bind_variable (par->name, val, 1, env->vars);
	  env->lex_env_vars_boundary++, (*argsnum)++;

	  if (par->supplied_p_param)
	    {
	      env->vars = bind_variable (par->supplied_p_param, &nil_object, 1,
					 env->vars);
	      env->lex_env_vars_boundary++, (*argsnum)++;
	    }
	}
      else if (par->supplied_p_param)
	{
	  env->vars = bind_variable (par->supplied_p_param, &t_object, 1,
				     env->vars);
	  env->lex_env_vars_boundary++, (*argsnum)++;
	}

      par = par->next;
    }

  while (par && par->type == AUXILIARY_VAR)
    {
      if (par->init_form)
	{
	  val = evaluate_object (par->init_form, env, outcome);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	  if (!val)
	    {
	      env->vars = remove_bindings (env->vars, *argsnum, 1);
	      env->lex_env_vars_boundary = prev_lex_bin_num;
	      return 0;
	    }

	  env->vars = bind_variable (par->name, val, 1, env->vars);
	  env->lex_env_vars_boundary++, (*argsnum)++;
	}
      else
	{
	  env->vars = bind_variable (par->name, &nil_object, 1, env->vars);
	  env->lex_env_vars_boundary++, (*argsnum)++;
	}

      par = par->next;
    }

  return 1;
}


int
destructure_tree (struct object *template, struct object *vals,
		  struct binding **bins, int *binnum, struct outcome *outcome)
{
  struct binding *subbins;
  int subnum;

  *bins = NULL, *binnum = 0;

  if (!IS_LIST (template))
    {
      outcome->type = INVALID_LAMBDA_LIST;
      return 0;
    }

  if (!IS_LIST (vals))
    {
      outcome->type = MISMATCH_IN_DESTRUCTURING_CALL;
      return 0;
    }

  while (template->type == TYPE_CONS_PAIR && vals->type == TYPE_CONS_PAIR)
    {
      if (CAR (template)->type == TYPE_CONS_PAIR && IS_LIST (CAR (vals)))
	{
	  if (!destructure_tree (CAR (template), CAR (vals), &subbins, &subnum,
				 outcome))
	    return 0;

	  *binnum += subnum;
	  *bins = chain_bindings (subbins, *bins, 0, NULL, NULL);
	}
      else if (IS_SYMBOL (CAR (template)))
	{
	  if (SYMBOL (CAR (template))->value_ptr.symbol->is_const
	      && SYMBOL (CAR (template)) != &nil_object)
	    {
	      outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	      return 0;
	    }

	  if (SYMBOL (CAR (template)) != &nil_object)
	    {
	      increment_refcount (CAR (vals));
	      *bins = bind_variable (SYMBOL (CAR (template)), CAR (vals), 0,
				     *bins);
	      (*binnum)++;
	    }
	}
      else
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return 0;
	}

      template = CDR (template);
      vals = CDR (vals);
    }

  while (template->type == TYPE_CONS_PAIR)
    {
      if (CAR (template)->type == TYPE_CONS_PAIR)
	{
	  if (!destructure_tree (CAR (template), &nil_object, &subbins, &subnum,
				 outcome))
	    return 0;

	  *binnum += subnum;
	  *bins = chain_bindings (subbins, *bins, 0, NULL, NULL);
	}
      else if (IS_SYMBOL (CAR (template)))
	{
	  if (SYMBOL (CAR (template))->value_ptr.symbol->is_const
	      && SYMBOL (CAR (template)) != &nil_object)
	    {
	      outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	      return 0;
	    }

	  if (SYMBOL (CAR (template)) != &nil_object)
	    {
	      *bins = bind_variable (SYMBOL (CAR (template)), &nil_object, 0,
				     *bins);
	      (*binnum)++;
	    }
	}
      else
	{
      	  outcome->type = INVALID_LAMBDA_LIST;
	  return 0;
	}

      template = CDR (template);
    }

  if (SYMBOL (template) != &nil_object)
    {
      if (!IS_SYMBOL (template))
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return 0;
	}

      if (SYMBOL (template)->value_ptr.symbol->is_const)
	{
	  outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	  return 0;
	}

      increment_refcount (vals);
      *bins = bind_variable (SYMBOL (template), vals, 0, *bins);
      (*binnum)++;
    }

  return 1;
}


void
restore_lexical_environment (struct environment *env, struct binding *vars,
			     struct binding *funcs, int *num_vars)
{
  struct binding *b;

  *num_vars = 0;

  while (vars)
    {
      b = malloc_and_check (sizeof (*b));

      b->type = LEXICAL_BINDING;
      b->refcount = 1;
      b->sym = NULL;
      b->obj = NULL;
      b->closure_bin = vars->closure_bin;
      b->next = env->vars;
      b->closure_bin->refcount++;

      b->prev_special = b->closure_bin->sym->value_ptr.symbol->is_special;
      b->closure_bin->sym->value_ptr.symbol->is_special = 0;

      env->vars = b;

      (*num_vars)++;

      vars = vars->next;
    }
}


struct object *
call_function (struct object *func, struct object *arglist, int eval_args,
	       int also_pass_name, int create_new_lex_env, int expand_and_eval,
	       int is_typespec, struct environment *env, struct outcome *outcome)
{
  struct binding *bins;
  struct block_frame *f;
  struct go_tag_frame *prevf;
  struct object *ret, *ret2, *args = NULL, *body;
  int argsnum, closnum, prev_lex_bin_num = env->lex_env_vars_boundary,
    stepping_over_this_macroexp = env->stepping_flags & STEP_OVER_EXPANSION
    && !(env->stepping_flags & STEPPING_OVER_FORM), onlylex = env->only_lexical;
  unsigned isprof = 0;
  clock_t time, evaltime = 0;


  env->stack_depth++;

  if (env->stack_depth > LISP_STACK_SIZE)
    {
      return raise_al_maximum_stack_depth_exceeded (LISP_STACK_SIZE,
						    env, outcome);
    }

  if (stepping_over_this_macroexp)
    env->stepping_flags |= STEPPING_OVER_FORM;

  if (func->value_ptr.function->builtin_form)
    {
      if (eval_args)
	{
	  args = evaluate_through_list (arglist, env, outcome);

	  if (!args)
	    {
	      env->stack_depth--;
	      return NULL;
	    }
	}
      else
	args = arglist;

      if (func->value_ptr.function->name != env->quote_sym
	  && func->value_ptr.function->name != env->function_sym
	  && (func->value_ptr.function->flags & TRACED_FUNCTION
	      || (env->stepping_flags &&
		  !(env->stepping_flags & STEPPING_OVER_FORM))))
	{
	  printf ("calling builtin ");
	  print_function_name (func, env);
	  printf (" ");
	  print_object (args, env, env->c_stdout->value_ptr.stream);
	  printf ("\n");
	  env->c_stdout->value_ptr.stream->dirty_line = 0;
	}

      env->call_stack = add_call_frame (func, args, env, -1, env->call_stack);

      if (env->is_profiling && func->value_ptr.function->name)
	{
	  isprof = 1;
	  time = clock ();
	}

      ret = func->value_ptr.function->builtin_form (args, env, outcome);

      if (isprof && env->is_profiling)
	{
	  time = clock () - time;
	  add_profiling_data (env->profiling_data,
			      SYMBOL (func->value_ptr.function->name),
			      func->value_ptr.function->is_setf_func, time,
			      evaltime);
	}

      env->call_stack = remove_call_frame (env->call_stack);

      if (func->value_ptr.function->name != env->quote_sym
	  && func->value_ptr.function->name != env->function_sym && ret
	  && ((func->value_ptr.function->flags & TRACED_FUNCTION)
	      || (env->stepping_flags
		  && !(env->stepping_flags & STEPPING_OVER_FORM))))
	{
	  printf ("builtin ");
	  print_function_name (func, env);
	  printf (" returned ");
	  print_object (ret, env, env->c_stdout->value_ptr.stream);
	  printf ("\n");
	  env->c_stdout->value_ptr.stream->dirty_line = 0;
	}

      if (eval_args)
	decrement_refcount (args);

      env->stack_depth--;

      return ret;
    }
  else if (func->value_ptr.function->struct_constructor_class_name)
    {
      args = evaluate_through_list (arglist, env, outcome);

      if (!args)
	{
	  env->stack_depth--;
	  return NULL;
	}

      ret = call_structure_constructor (func->value_ptr.function->
					struct_constructor_class_name, args, env,
					outcome);

      decrement_refcount (args);

      env->stack_depth--;
      return ret;
    }
  else if (func->value_ptr.function->struct_predicate_class_name)
    {
      args = evaluate_through_list (arglist, env, outcome);

      if (!args)
	{
	  env->stack_depth--;
	  return NULL;
	}

      ret = call_structure_predicate (func->value_ptr.function->
				      struct_predicate_class_name, args, env,
				      outcome);

      decrement_refcount (args);

      env->stack_depth--;
      return ret;
    }
  else if (func->value_ptr.function->struct_copyier_class_name)
    {
      args = evaluate_through_list (arglist, env, outcome);

      if (!args)
	{
	  env->stack_depth--;
	  return NULL;
	}

      ret = call_structure_copyier (func->value_ptr.function->
				    struct_copyier_class_name, args, env,
				    outcome);

      decrement_refcount (args);

      env->stack_depth--;
      return ret;
    }
  else if (func->value_ptr.function->struct_accessor_class_name)
    {
      args = evaluate_through_list (arglist, env, outcome);

      if (!args)
	{
	  env->stack_depth--;
	  return NULL;
	}

      ret = call_structure_accessor (func->value_ptr.function->
				     struct_accessor_class_name,
				     func->value_ptr.function->
				     struct_accessor_field, args, NULL, env,
				     outcome);

      decrement_refcount (args);

      env->stack_depth--;
      return ret;
    }
  else if (func->value_ptr.function->condition_reader_class_name)
    {
      args = evaluate_through_list (arglist, env, outcome);

      if (!args)
	{
	  env->stack_depth--;
	  return NULL;
	}

      ret = call_condition_reader (func->value_ptr.function->
				   condition_reader_class_name,
				   func->value_ptr.function->
				   condition_reader_field, args, env, outcome);

      decrement_refcount (args);

      env->stack_depth--;
      return ret;
    }
  else if (func->value_ptr.function->function_macro)
    {
      if (list_length (arglist) != 2)
	{
	  outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
	  return NULL;
	}

      if (!IS_LIST (CAR (arglist)))
	{
	  return raise_type_error (CAR (arglist), "CL:LIST", env, outcome);
	}

      ret = call_function (func->value_ptr.function->function_macro,
			   CAR (arglist), 0, 1, 1, 0, 0, env, outcome);

      env->stack_depth--;
      return ret;
    }
  else if (func->value_ptr.function->macro_function)
    {
      args = alloc_empty_list (2);
      args->value_ptr.cons_pair->car = arglist;
      args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = &nil_object;

      ret = call_function (func->value_ptr.macro->macro_function, args, 0, 0, 0,
			   1, 0, env, outcome);

      free_list_structure (args);

      env->stack_depth--;
      return ret;
    }


  if (func->value_ptr.function->flags & GENERIC_FUNCTION)
    {
      ret = dispatch_generic_function_call (func, arglist, eval_args, env,
					    outcome);
      env->stack_depth--;
      return ret;
    }


  env->only_lexical = expand_and_eval;

  if (parse_argument_list (arglist, func->value_ptr.function->lambda_list,
			   eval_args, also_pass_name, is_typespec,
			   func->value_ptr.function->flags & FOUND_AMP_KEY,
			   func->value_ptr.function->allow_other_keys,
			   func->value_ptr.function->lex_vars,
			   create_new_lex_env, env, outcome, &bins, &argsnum,
			   &closnum))
    {
      if (func->value_ptr.function->flags & TRACED_FUNCTION
	  || (env->stepping_flags &&
	      !(env->stepping_flags & STEPPING_OVER_FORM)))
	{
	  printf ("calling ");
	  print_function_name (func, env);
	  printf (" ");
	  print_bindings_in_reverse (env->vars, argsnum, env, env->c_stdout);
	  printf ("\n");
	  env->c_stdout->value_ptr.stream->dirty_line = 0;
	}

      env->call_stack = add_call_frame (func, arglist, env, argsnum,
					env->call_stack);

      if (env->is_profiling && func->value_ptr.function->name)
	{
	  isprof = 1;
	  time = clock ();
	}

      if (!parse_declarations (func->value_ptr.function->body, env,
			       argsnum+closnum, 1, outcome, &body))
	{
	  ret = NULL;
	}
      else
	{
	  f = malloc_and_check (sizeof (*f));
	  f->next = env->blocks;
	  env->blocks = f;

	  if (func->value_ptr.function->name)
	    {
	      f->frame = malloc_and_check (sizeof (*f->frame));
	      f->frame->name = func->value_ptr.function->name;
	      f->frame->refcount = 1;
	      f->frame->next = func->value_ptr.function->encl_blocks;

	      if (f->frame->next)
		f->frame->next->refcount++;
	    }
	  else
	    f->frame = func->value_ptr.function->encl_blocks;

	  prevf = env->go_tag_stack;
	  env->go_tag_stack = func->value_ptr.function->encl_tags;

	  ret = evaluate_body (body, -1, 0, 0, NULL, env, outcome);

	  env->go_tag_stack = prevf;

	  if (!ret && outcome->block_to_leave && func->value_ptr.function->name
	      && outcome->block_to_leave == f->frame)
	    {
	      outcome->block_to_leave = NULL;

	      ret = outcome->return_value;

	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	    }

	  if (func->value_ptr.function->name)
	    env->blocks->frame = remove_block (env->blocks->frame);

	  f = env->blocks->next;
	  free (env->blocks);
	  env->blocks = f;
	}

      undo_special_declarations (func->value_ptr.function->body, env);

      if (isprof)
	{
	  time = clock () - time;
	}

      env->call_stack = remove_call_frame (env->call_stack);

      if (ret && ((func->value_ptr.function->flags & TRACED_FUNCTION)
		  || (env->stepping_flags
		      && !(env->stepping_flags & STEPPING_OVER_FORM))))
	{
	  print_function_name (func, env);
	  printf (" returned ");
	  print_object (ret, env, env->c_stdout->value_ptr.stream);
	  printf ("\n");
	  env->c_stdout->value_ptr.stream->dirty_line = 0;
	}

      env->vars = remove_bindings (env->vars, argsnum+closnum, 1);
      env->lex_env_vars_boundary = prev_lex_bin_num;
    }
  else
    {
      ret = NULL;
    }


  env->only_lexical = onlylex;

  if (ret && expand_and_eval)
    {
      if (isprof)
	{
	  evaltime = clock ();
	}

      if (stepping_over_this_macroexp
	  && (env->stepping_flags == (STEP_OVER_EXPANSION | STEPPING_OVER_FORM)))
	{
	  printf (" -> ");
	  print_object (ret, env, env->c_stdout->value_ptr.stream);
	  printf ("\n");
	  env->c_stdout->value_ptr.stream->dirty_line = 0;
	  env->stepping_flags = env->stepping_flags & ~STEPPING_OVER_FORM;
	}

      ret2 = evaluate_object (ret, env, outcome);

      if (isprof)
	{
	  evaltime = clock () - evaltime;
	}

      decrement_refcount (ret);

      ret = ret2;
    }

  if (isprof && env->is_profiling)
    {
      add_profiling_data (env->profiling_data,
			  SYMBOL (func->value_ptr.function->name),
			  func->value_ptr.function->is_setf_func, time,
			  evaltime);
    }

  if (stepping_over_this_macroexp
      && env->stepping_flags == (STEP_OVER_EXPANSION | STEPPING_OVER_FORM))
    env->stepping_flags = env->stepping_flags & ~STEPPING_OVER_FORM;


  env->stack_depth--;
  return ret;
}


struct object *
call_structure_constructor (struct object *class_name, struct object *args,
			    struct environment *env, struct outcome *outcome)
{
  struct object *ret, *allow_other_keys = NULL;
  struct structure *s;
  struct structure_field *f;
  struct structure_field_decl *fd;
  int found_unknown_key = 0;

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
      f->value = NULL;

      fd = fd->next;
    }

  if (s->fields)
    f->next = NULL;


  while (SYMBOL (args) != &nil_object)
    {
      f = s->fields;

      while (f)
	{
	  if (eqmem (f->name->value_ptr.symbol->name,
		     f->name->value_ptr.symbol->name_len,
		     IS_SYMBOL (CAR (args))
		     ? SYMBOL (CAR (args))->value_ptr.symbol->name : "",
		     IS_SYMBOL (CAR (args))
		     ? SYMBOL (CAR (args))->value_ptr.symbol->name_len : 0))
	    {
	      if (SYMBOL (CDR (args)) == &nil_object)
		{
		  outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
		  return NULL;
		}

	      if (!f->value)
		{
		  f->value = CAR (CDR (args));
		  increment_refcount (f->value);
		}

	      break;
	    }

	  f = f->next;
	}

      if (SYMBOL (CAR (args)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (args)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (args));
	}

      if (!f && SYMBOL (CAR (args)) != env->key_allow_other_keys_sym)
	{
	  found_unknown_key = 1;
	}

      if (SYMBOL (CDR (args)) == &nil_object)
	{
	  outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	  return NULL;
	}

      args = CDR (CDR (args));
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }


  f = s->fields;

  while (f)
    {
      if (!f->value)
	{
	  f->value = &nil_object;
	}

      f = f->next;
    }

  return ret;
}


struct object *
call_structure_predicate (struct object *class_name, struct object *args,
			  struct environment *env, struct outcome *outcome)
{
  if (list_length (args) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (args)->type == TYPE_STRUCTURE
      && CAR (args)->value_ptr.structure->class_name == class_name)
    return &t_object;

  return &nil_object;
}


struct object *
call_structure_copyier (struct object *class_name, struct object *args,
			struct environment *env, struct outcome *outcome)
{
  struct object *ret;
  struct structure *s;
  struct structure_field *fs, *f;

  if (list_length (args) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (args)->type != TYPE_STRUCTURE
      || CAR (args)->value_ptr.structure->class_name != class_name)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = alloc_object ();
  ret->type = TYPE_STRUCTURE;

  s = malloc_and_check (sizeof (*s));
  s->class_name = class_name;
  s->fields = NULL;
  ret->value_ptr.structure = s;

  fs = CAR (args)->value_ptr.structure->fields;

  while (fs)
    {
      if (s->fields)
	{
	  f->next = malloc_and_check (sizeof (*f));
	  f = f->next;
	}
      else
	s->fields = f = malloc_and_check (sizeof (*f));

      f->name = fs->name;
      f->value = fs->value;
      increment_refcount (f->value);

      fs = fs->next;
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
	    {
	      decrement_refcount (f->value);
	      f->value = newval;
	    }

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
  struct class_field *f;

  if (list_length (args) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (args)->type != TYPE_STANDARD_OBJECT
      || !is_subtype (CAR (args)->value_ptr.standard_object->class, class_name,
		      NULL, env, outcome))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  f = CAR (args)->value_ptr.standard_object->fields;

  while (f)
    {
      if (f->decl->name == field)
	{
	  if (!f->value)
	    return &nil_object;

	  increment_refcount (f->value);
	  return f->value;
	}

      f = f->next;
    }

  return NULL;
}


struct object *
call_method (struct method_list *methlist, struct object *arglist,
	     struct environment *env, struct outcome *outcome)
{
  struct binding *bins;
  struct object *ret, *func = methlist->meth->value_ptr.method->generic_func,
    *body;
  struct method_list *methl;
  struct class_field *f;
  int argsnum, closnum, prev_lex_bin_num = env->lex_env_vars_boundary;

  if (!methlist->meth->value_ptr.method->body)
    {
      if (methlist->meth->value_ptr.method->builtin_method)
	{
	  if (func->value_ptr.function->flags & TRACED_FUNCTION
	      || (env->stepping_flags &&
		  !(env->stepping_flags & STEPPING_OVER_FORM)))
	    {
	      printf ("calling builtin method ");
	      print_method_description (methlist->meth, env);
	      printf (" of ");
	      print_function_name (func, env);
	      printf (" ");
	      print_object (arglist, env, env->c_stdout->value_ptr.stream);
	      printf ("\n");
	      env->c_stdout->value_ptr.stream->dirty_line = 0;
	    }

	  env->call_stack = add_call_frame (methlist->meth, arglist, env, -1,
					    env->call_stack);

	  ret = methlist->meth->value_ptr.method->builtin_method (arglist, env,
								  outcome);

	  env->call_stack = remove_call_frame (env->call_stack);

	  if (ret && ((func->value_ptr.function->flags & TRACED_FUNCTION)
		      || (env->stepping_flags
			  && !(env->stepping_flags & STEPPING_OVER_FORM))))
	    {
	      printf ("builtin method ");
	      print_method_description (methlist->meth, env);
	      printf (" of ");
	      print_function_name (func, env);
	      printf (" returned ");
	      print_object (ret, env, env->c_stdout->value_ptr.stream);
	      printf ("\n");
	      env->c_stdout->value_ptr.stream->dirty_line = 0;
	    }

	  return ret;
	}
      else if (methlist->meth->value_ptr.method->object_reader_class)
	{
	  if (list_length (arglist) != 1)
	    {
	      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
	    }

	  if (CAR (arglist)->type != TYPE_STANDARD_OBJECT
	      || !is_subtype (CAR (arglist)->value_ptr.standard_object->class,
			      methlist->meth->value_ptr.method->
			      object_reader_class, NULL, env, outcome))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  f = CAR (arglist)->value_ptr.standard_object->fields;

	  while (f)
	    {
	      if (f->decl->name
		  == methlist->meth->value_ptr.method->object_reader_field)
		{
		  if ((f->name && !f->value) || (!f->name && !f->decl->value))
		    {
		      outcome->type = SLOT_NOT_BOUND;
		      return NULL;
		    }

		  if (f->name)
		    {
		      increment_refcount (f->value);
		      return f->value;
		    }
		  else
		    {
		      increment_refcount (f->decl->value);
		      return f->decl->value;
		    }
		}

	      f = f->next;
	    }

	  return NULL;
	}
      else if (methlist->meth->value_ptr.method->object_writer_class)
	{
	  if (list_length (arglist) != 2)
	    {
	      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
	    }

	  if (CAR (CDR (arglist))->type != TYPE_STANDARD_OBJECT
	      || !is_subtype (CAR (CDR (arglist))->value_ptr.standard_object->
			      class,
			      methlist->meth->value_ptr.method->
			      object_writer_class, NULL, env, outcome))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  f = CAR (CDR (arglist))->value_ptr.standard_object->fields;

	  while (f)
	    {
	      if (f->decl->name
		  == methlist->meth->value_ptr.method->object_writer_field)
		{
		  if (IS_WATCHED (CAR (CDR (arglist))))
		    {
		      env->watched_obj = CAR (CDR (arglist));
		      env->obj_field = f->decl->name;
		      env->new_value = CAR (arglist);

		      if (!enter_debugger (NULL, env, outcome))
			return NULL;
		    }

		  if (f->name)
		    {
		      decrement_refcount (f->value);
		      f->value = CAR (arglist);
		    }
		  else
		    {
		      decrement_refcount (f->decl->value);
		      f->decl->value = CAR (arglist);
		    }

		  increment_refcount (CAR (arglist));
		  increment_refcount (CAR (arglist));
		  return CAR (arglist);
		}

	      f = f->next;
	    }

	  return NULL;
	}
      else if (methlist->meth->value_ptr.method->object_accessor_class
	       && !methlist->meth->value_ptr.method->generic_func->
	       value_ptr.function->is_setf_func)
	{
	  if (list_length (arglist) != 1)
	    {
	      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
	    }

	  if (CAR (arglist)->type != TYPE_STANDARD_OBJECT
	      || !is_subtype (CAR (arglist)->value_ptr.standard_object->class,
			      methlist->meth->value_ptr.method->
			      object_accessor_class, NULL, env, outcome))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  f = CAR (arglist)->value_ptr.standard_object->fields;

	  while (f)
	    {
	      if (f->decl->name
		  == methlist->meth->value_ptr.method->object_accessor_field)
		{
		  if ((f->name && !f->value) || (!f->name && !f->decl->value))
		    {
		      outcome->type = SLOT_NOT_BOUND;
		      return NULL;
		    }

		  if (f->name)
		    {
		      increment_refcount (f->value);
		      return f->value;
		    }
		  else
		    {
		      increment_refcount (f->decl->value);
		      return f->decl->value;
		    }
		}

	      f = f->next;
	    }

	  return NULL;
	}
      else if (methlist->meth->value_ptr.method->object_accessor_class)
	{
	  if (list_length (arglist) != 2)
	    {
	      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
	    }

	  if (CAR (CDR (arglist))->type != TYPE_STANDARD_OBJECT
	      || !is_subtype (CAR (CDR (arglist))->value_ptr.standard_object->
			      class,
			      methlist->meth->value_ptr.method->
			      object_accessor_class, NULL, env, outcome))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  f = CAR (CDR (arglist))->value_ptr.standard_object->fields;

	  while (f)
	    {
	      if (f->decl->name
		  == methlist->meth->value_ptr.method->object_accessor_field)
		{
		  if (IS_WATCHED (CAR (CDR (arglist))))
		    {
		      env->watched_obj = CAR (CDR (arglist));
		      env->obj_field = f->decl->name;
		      env->new_value = CAR (arglist);

		      if (!enter_debugger (NULL, env, outcome))
			return NULL;
		    }

		  if (f->name)
		    {
		      decrement_refcount (f->value);
		      f->value = CAR (arglist);
		    }
		  else
		    {
		      decrement_refcount (f->decl->value);
		      f->decl->value = CAR (arglist);
		    }

		  increment_refcount (CAR (arglist));
		  increment_refcount (CAR (arglist));
		  return CAR (arglist);
		}

	      f = f->next;
	    }

	  return NULL;
	}
    }


  if (parse_argument_list (arglist, methlist->meth->value_ptr.method->lambda_list,
			   0, 0, 0, methlist->meth->value_ptr.method->found_amp_key,
			   func->value_ptr.function->allow_other_keys, NULL, 1,
			   env, outcome, &bins, &argsnum, &closnum))
    {
      env->method_args = arglist;
      methl = env->method_list;
      env->method_list = methlist;

      if (func->value_ptr.function->flags & TRACED_FUNCTION
	  || (env->stepping_flags &&
	      !(env->stepping_flags & STEPPING_OVER_FORM)))
	{
	  printf ("calling method ");
	  print_method_description (methlist->meth, env);
	  printf (" of ");
	  print_function_name (func, env);
	  printf (" ");
	  print_bindings_in_reverse (env->vars, argsnum, env, env->c_stdout);
	  printf ("\n");
	  env->c_stdout->value_ptr.stream->dirty_line = 0;
	}

      env->call_stack = add_call_frame (methlist->meth, arglist, env, argsnum,
					env->call_stack);

      if (!parse_declarations (methlist->meth->value_ptr.method->body, env,
			       argsnum+closnum, 1, outcome, &body))
	{
	  ret = NULL;
	}
      else
	{
	  ret = evaluate_body (body, -1, 0, 0, NULL, env, outcome);

	  if (ret && ((func->value_ptr.function->flags & TRACED_FUNCTION)
		      || (env->stepping_flags
			  && !(env->stepping_flags & STEPPING_OVER_FORM))))
	    {
	      printf ("method ");
	      print_method_description (methlist->meth, env);
	      printf (" of ");
	      print_function_name (func, env);
	      printf (" returned ");
	      print_object (ret, env, env->c_stdout->value_ptr.stream);
	      printf ("\n");
	      env->c_stdout->value_ptr.stream->dirty_line = 0;
	    }
	}

      undo_special_declarations (methlist->meth->value_ptr.method->body, env);

      env->call_stack = remove_call_frame (env->call_stack);

      env->vars = remove_bindings (env->vars, argsnum+closnum, 1);
      env->lex_env_vars_boundary = prev_lex_bin_num;

      env->method_args = NULL;
      env->method_list = methl;
    }
  else
    {
      ret = NULL;
    }

  return ret;
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


struct precedence_relation *
add_precedence_relations (struct object *class, struct precedence_relation *rels,
			  int *not_completely_defined)
{
  struct precedence_relation *r = rels;
  struct object_list *p = class->value_ptr.standard_class->parents;

  *not_completely_defined = 0;

  while (p)
    {
      if (IS_SYMBOL (p->obj) && SYMBOL (p->obj)->value_ptr.symbol->typespec
	  && SYMBOL (p->obj)->value_ptr.symbol->typespec->type
	  == TYPE_STANDARD_CLASS)
	{
	  p->obj = SYMBOL (p->obj)->value_ptr.symbol->typespec;
	  increment_refcount (p->obj);
	}
      else if (p->obj->type != TYPE_STANDARD_CLASS)
	{
	  *not_completely_defined = 1;
	  return NULL;
	}

      p = p->next;
    }

  while (r)
    {
      if (r->is_parent && r->first == class)
	return rels;

      r = r->next;
    }

  p = class->value_ptr.standard_class->parents;

  if (p)
    {
      r = malloc_and_check (sizeof (*r));
      r->first = class;
      r->second = p->obj;
      r->is_parent = 1;

      r->next = rels;
      rels = r;
    }

  while (p)
    {
      if (!(rels = add_precedence_relations (p->obj, rels,
					     not_completely_defined)))
	{
	  return NULL;
	}

      if (!p->next)
	break;

      r = malloc_and_check (sizeof (*r));
      r->first = p->obj;
      r->second = p->next->obj;
      r->is_parent = 0;

      r->next = rels;
      rels = r;

      p = p->next;
    }

  return rels;
}


struct object_list *
collect_superclasses (struct object *class, struct object_list *scs)
{
  struct object_list *s = scs, *l;

  while (s)
    {
      if (s->obj == class)
	return scs;

      s = s->next;
    }

  l = malloc_and_check (sizeof (*l));
  l->obj = class;
  l->next = scs;
  scs = l;


  s = class->value_ptr.standard_class->parents;

  while (s)
    {
      scs = collect_superclasses (s->obj, scs);
      s = s->next;
    }

  return scs;
}


int
is_direct_subclass (struct object *first, struct object *second)
{
  struct object_list *p = first->value_ptr.standard_class->parents;

  while (p)
    {
      if (p->obj == second)
	return 1;

      p = p->next;
    }

  return 0;
}


struct object *
compute_next_element_in_precedence_list (struct object_list *precedence_list,
					 struct object_list *superclasses,
					 struct precedence_relation *prec_relations)
{
  struct object_list *s = superclasses, *woprec = NULL, *w;
  struct precedence_relation *pr;
  struct object *rightmost_subcl, *ret;
  int passed_rightmost;

  while (s)
    {
      pr = prec_relations;

      while (pr)
	{
	  if (s->obj == pr->second)
	    break;

	  pr = pr->next;
	}

      if (!pr)
	{
	  w = malloc_and_check (sizeof (*w));
	  w->obj = s->obj;
	  w->next = woprec;
	  woprec = w;
	}

      s = s->next;
    }

  if (!woprec)
    return NULL;

  if (woprec->next)
    {
      rightmost_subcl = NULL;
      ret = NULL;
      w = woprec;

      while (w)
	{
	  s = precedence_list;
	  passed_rightmost = 0;

	  while (s)
	    {
	      if (is_direct_subclass (s->obj, w->obj)
		  && (!rightmost_subcl || passed_rightmost))
		{
		  rightmost_subcl = s->obj;
		  ret = w->obj;
		}

	      if (s->obj == rightmost_subcl)
		passed_rightmost = 1;

	      s = s->next;
	    }

	  w = w->next;
	}
    }
  else
    ret = woprec->obj;

  if (!ret)
    return NULL;

  while (woprec)
    {
      w = woprec->next;
      free (woprec);
      woprec = w;
    }

  return ret;
}


struct object_list *
remove_element_from_obj_list (struct object *el, struct object_list *l)
{
  struct object_list *p = NULL, *ret = l;

  while (l)
    {
      if (l->obj == el)
	{
	  if (p)
	    p->next = l->next;

	  if (ret == l)
	    ret = ret->next;

	  free (l);
	  return ret;
	}

      p = l;
      l = l->next;
    }

  return ret;
}


struct precedence_relation *
remove_relations_with_given_first (struct object *el,
				   struct precedence_relation *rels)
{
  struct precedence_relation *pr = NULL, *ret = rels, *tmp;

  while (rels)
    {
      if (rels->first == el)
	{
	  if (pr)
	    pr->next = rels->next;

	  if (ret == rels)
	    ret = ret->next;

	  tmp = rels->next;
	  free (rels);
	  rels = tmp;
	}
      else
	{
	  pr = rels;
	  rels = rels->next;
	}
    }

  return ret;
}


int
compute_class_precedence_list (struct object *class, struct outcome *outcome)
{
  int ncd;
  struct precedence_relation *prec = add_precedence_relations (class, NULL,
							       &ncd);
  struct object_list *scs, *preclist = NULL, *last = NULL;
  struct object *nextel;

  if (ncd)
    {
      outcome->type = CLASS_DEFINITION_NOT_COMPLETE;
      return 0;
    }

  scs = collect_superclasses (class, NULL);

  while (scs)
    {
      nextel = compute_next_element_in_precedence_list (preclist, scs, prec);

      if (!nextel)
	{
	  outcome->type = CLASS_ANCESTRY_NOT_CONSISTENT;
	  return 0;
	}

      if (preclist)
	last = last->next = malloc_and_check (sizeof (*last));
      else
	preclist = last = malloc_and_check (sizeof (*last));

      last->obj = nextel;
      last->next = NULL;

      scs = remove_element_from_obj_list (nextel, scs);
      prec = remove_relations_with_given_first (nextel, prec);
    }

  class->value_ptr.standard_class->class_precedence_list = preclist;

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
compare_method_specificity (struct object *first, struct object *second,
			    struct environment *env)
{
  struct parameter *par = first->value_ptr.method->lambda_list,
    *par2 = second->value_ptr.method->lambda_list;
  int ret1, ret2;

  while (par && par->type == REQUIRED_PARAM)
    {
      if (par->typespec->type == TYPE_CONS_PAIR
	  && par2->typespec->type != TYPE_CONS_PAIR)
	{
	  return -1;
	}

      if (par->typespec->type != TYPE_CONS_PAIR
	  && par2->typespec->type == TYPE_CONS_PAIR)
	{
	  return 1;
	}

      if (par->typespec->type != TYPE_CONS_PAIR
	  && par2->typespec->type != TYPE_CONS_PAIR)
	{
	  ret1 = is_subtype (par->typespec, par2->typespec, NULL, env, NULL);
	  ret2 = is_subtype (par2->typespec, par->typespec, NULL, env, NULL);

	  if (ret1 && !ret2)
	    {
	      return -1;
	    }

	  if (!ret1 && ret2)
	    {
	      return 1;
	    }
	}

      par = par->next;
      par2 = par2->next;
    }

  return 0;
}


void
add_method (struct object *genfun, struct object *meth)
{
  struct method_list *ml = malloc_and_check (sizeof (*ml));

  ml->meth = meth;

  ml->reference_strength_factor = !STRENGTH_FACTOR_OF_OBJECT (meth);
  INC_WEAK_REFCOUNT (meth);

  ml->next = genfun->value_ptr.function->methods;

  genfun->value_ptr.function->methods = ml;

  meth->value_ptr.method->generic_func = genfun;
  add_reference (meth, genfun, 0);
}


struct object *
find_method (struct object *func, enum method_qualifier qualifier,
	     struct object *c_specifiers, struct parameter *l_specifiers,
	     struct method_list **mlist, int *ind, struct environment *env,
	     struct outcome *outcome)
{
  struct method_list *ml = func->value_ptr.function->methods;
  struct parameter *par, *sp;
  struct object *cons;
  int i = 0;

  while (ml)
    {
      if (qualifier != ml->meth->value_ptr.method->qualifier)
	{
	  ml = ml->next;
	  continue;
	}

      par = ml->meth->value_ptr.method->lambda_list;

      if (c_specifiers)
	cons = c_specifiers;
      else
	sp = l_specifiers;

      while (par && par->type == REQUIRED_PARAM)
	{
	  if (c_specifiers && SYMBOL (cons) == &nil_object)
	    break;

	  if (IS_SYMBOL (par->typespec))
	    {
	      if (c_specifiers)
		{
		  if (!is_subtype (par->typespec, CAR (cons), NULL, env, outcome)
		      || !is_subtype (CAR (cons), par->typespec, NULL, env,
				      outcome))
		    break;
		}
	      else
		{
		  if (!is_subtype (par->typespec, sp->typespec, NULL, env,
				   outcome)
		      || !is_subtype (sp->typespec, par->typespec, NULL, env,
				      outcome))
		    break;
		}
	    }
	  else
	    {
	      if (c_specifiers)
		{
		  if (CAR (cons)->type != TYPE_CONS_PAIR
		      || list_length (CAR (cons)) != 2
		      || CAR (CAR (cons)) != env->eql_sym)
		    break;

		  if (eql_objects (CAR (CDR (par->typespec)),
				   CAR (CDR (CAR (cons)))) == &nil_object)
		    break;
		}
	      else
		{
		  if (sp->typespec->type != TYPE_CONS_PAIR
		      || list_length (sp->typespec) != 2
		      || CAR (sp->typespec) != env->eql_sym)
		    break;

		  if (eql_objects (CAR (CDR (par->typespec)),
				   CAR (CDR (sp->typespec))) == &nil_object)
		    break;
		}
	    }

	  if (c_specifiers)
	    cons = CDR (cons);
	  else
	    sp = sp->next;

	  par = par->next;
	}

      if (!par || par->type != REQUIRED_PARAM)
	{
	  if (mlist)
	    *mlist = ml;

	  if (ind)
	    *ind = i;

	  return ml->meth;
	}

      i++;
      ml = ml->next;
    }

  return &nil_object;
}


struct object *
dispatch_generic_function_call (struct object *func, struct object *arglist,
				int eval_args, struct environment *env,
				struct outcome *outcome)
{
  struct object *args, *ret = NULL, *res, *tmp, *margs;
  struct method_list *applm = NULL, *lapplm, *mlist,
    *ml = func->value_ptr.function->methods;
  int applnum = 0, i, found_primary = 0, isprof = 0;
  clock_t time;

  if (eval_args)
    {
      args = evaluate_through_list (arglist, env, outcome);

      if (!args)
	return NULL;
    }
  else
    {
      increment_refcount (arglist);
      args = arglist;
    }

  if (func->value_ptr.function->flags & TRACED_FUNCTION
      || (env->stepping_flags &&
	  !(env->stepping_flags & STEPPING_OVER_FORM)))
    {
      printf ("calling generic function ");
      print_function_name (func, env);
      printf (" ");
      print_object (args, env, env->c_stdout->value_ptr.stream);
      printf ("\n");
      env->c_stdout->value_ptr.stream->dirty_line = 0;
    }

  if (env->is_profiling)
    {
      isprof = 1;
      time = clock ();
    }

  while (ml)
    {
      if (is_method_applicable (ml->meth, args, env, outcome))
	{
	  if (ml->meth->value_ptr.method->qualifier == PRIMARY_METHOD)
	    found_primary = 1;

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

  if (!found_primary)
    {
      outcome->type = NO_PRIMARY_APPLICABLE_METHOD;
      return NULL;
    }


  while (applnum > 1)
    {
      lapplm = applm;

      for (i = 0; i < applnum-1; i++)
	{
	  if (lapplm->meth->value_ptr.method->qualifier
	      > lapplm->next->meth->value_ptr.method->qualifier
	      || (lapplm->meth->value_ptr.method->qualifier
		  == lapplm->next->meth->value_ptr.method->qualifier
		  && ((lapplm->meth->value_ptr.method->qualifier == AFTER_METHOD
		       && compare_method_specificity (lapplm->meth,
						      lapplm->next->meth, env) < 0)
		      || (lapplm->meth->value_ptr.method->qualifier
			  != AFTER_METHOD
			  && compare_method_specificity (lapplm->meth,
							 lapplm->next->meth,
							 env) > 0))))
	    {
	      tmp = lapplm->meth;
	      lapplm->meth = lapplm->next->meth;
	      lapplm->next->meth = tmp;
	    }

	  lapplm = lapplm->next;
	}

      applnum--;
    }


  margs = env->method_args;
  mlist = env->method_list;

  lapplm = applm;

  while (lapplm)
    {
      res = call_method (lapplm, args, env, outcome);

      if (!res)
	{
	  ret = NULL;
	  break;
	}

      if (lapplm->meth->value_ptr.method->qualifier == AROUND_METHOD)
	{
	  decrement_refcount (ret);
	  ret = res;
	  break;
	}
      else if (lapplm->meth->value_ptr.method->qualifier == BEFORE_METHOD
	       || lapplm->meth->value_ptr.method->qualifier == AFTER_METHOD)
	{
	  lapplm = lapplm->next;
	  decrement_refcount (res);
	}
      else
	{
	  lapplm = lapplm->next;
	  decrement_refcount (ret);
	  ret = res;

	  while (lapplm && lapplm->meth->value_ptr.method->qualifier
		 == PRIMARY_METHOD)
	    {
	      lapplm = lapplm->next;
	    }
	}
    }

  decrement_refcount (args);

  while (applm)
    {
      lapplm = applm->next;
      free (applm);
      applm = lapplm;
    }

  env->method_args = margs;
  env->method_list = mlist;

  if (isprof && env->is_profiling)
    {
      add_profiling_data (env->profiling_data,
			  SYMBOL (func->value_ptr.function->name),
			  func->value_ptr.function->is_setf_func,
			  clock () - time, 0);
    }

  if (ret && ((func->value_ptr.function->flags & TRACED_FUNCTION)
	      || (env->stepping_flags
		  && !(env->stepping_flags & STEPPING_OVER_FORM))))
    {
      printf ("generic function ");
      print_function_name (func, env);
      printf (" returned ");
      print_object (ret, env, env->c_stdout->value_ptr.stream);
      printf ("\n");
      env->c_stdout->value_ptr.stream->dirty_line = 0;
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
      && !IS_SYMBOL (typespec) && typespec->type != TYPE_STANDARD_CLASS
      && typespec->type != TYPE_STRUCTURE_CLASS)
    {
      outcome->type = INVALID_TYPE_SPECIFIER;
      return -1;
    }

  if (typespec->type == TYPE_CONS_PAIR)
    sym = SYMBOL (CAR (typespec));
  else if (typespec->type == TYPE_STANDARD_CLASS)
    sym = typespec->value_ptr.standard_class->name;
  else if (typespec->type == TYPE_STRUCTURE_CLASS)
    sym = typespec->value_ptr.structure_class->name;
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
	  num_objects--;

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
	    && (obj->value_ptr.standard_object->class->value_ptr.standard_class
		->name == sym
		|| is_descendant (NULL, obj->value_ptr.standard_object->
				  class->value_ptr.standard_class->parents,
				  sym, NULL, env));
	}
      else if (sym->value_ptr.symbol->is_type
	       && sym->value_ptr.symbol->typespec->type == TYPE_CONDITION_CLASS)
	{
	  return obj->type == TYPE_CONDITION
	    && (obj->value_ptr.condition->class_name == sym
		|| is_descendant (NULL, obj->value_ptr.condition->class_name->
				  value_ptr.symbol->typespec->
				  value_ptr.condition_class->parents,
				  sym, NULL, env));
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
						   env->cl_package, 0, 0),
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
						   env->cl_package, 0, 0), NULL,
		     env, NULL);
}


int
is_descendant (const struct object *first, const struct object_list *parents,
	       const struct object *second, const struct object *prev,
	       struct environment *env)
{
  const struct object_list *p = parents;
  int ret;

  while (p)
    {
      if ((IS_SYMBOL (p->obj) && p->obj == SYMBOL (second))
	  || p->obj == SYMBOL (second)->value_ptr.symbol->typespec)
	return 1;

      p = p->next;
    }

  p = parents;

  while (p)
    {
      if (p->obj != prev)
	{
	  ret = is_subtype (IS_SYMBOL (p->obj) ? p->obj
			    : p->obj->value_ptr.standard_class->name,
			    SYMBOL (second), first ? SYMBOL (first)
			    : NULL, env, NULL);

	  if (ret)
	    return 1;
	}

      p = p->next;
    }

  return 0;
}


int
is_subtype (const struct object *firstsp, const struct object *secondsp,
	    const struct object *prev, struct environment *env,
	    struct outcome *outcome)
{
  struct object_list *p;
  const struct object *firstsym, *secondsym, *first, *second;
  int ret;

  if (firstsp->type == TYPE_CONS_PAIR && IS_SYMBOL (CAR (firstsp)))
    firstsym = SYMBOL (CAR (firstsp));
  else if (IS_SYMBOL (firstsp))
    firstsym = SYMBOL (firstsp);

  if (secondsp->type == TYPE_CONS_PAIR && IS_SYMBOL (CAR (secondsp)))
    secondsym = SYMBOL (CAR (secondsp));
  else if (IS_SYMBOL (secondsp))
    secondsym = SYMBOL (secondsp);

  if (((firstsp->type == TYPE_CONS_PAIR && IS_SYMBOL (CAR (firstsp)))
       || IS_SYMBOL (firstsp))
      && firstsym->value_ptr.symbol->is_type
      && firstsym->value_ptr.symbol->typespec
      && firstsym->value_ptr.symbol->typespec->type == TYPE_FUNCTION)
    {
      first = call_function (firstsym->value_ptr.symbol->typespec,
			     firstsp->type == TYPE_CONS_PAIR ? CDR (firstsp)
			     : &nil_object, 0, 0, 0, 0, 1, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!first)
	return -2;
    }
  else
    first = firstsp;

  if (((secondsp->type == TYPE_CONS_PAIR && IS_SYMBOL (CAR (secondsp)))
       || IS_SYMBOL (secondsp))
      && secondsym->value_ptr.symbol->is_type
      && secondsym->value_ptr.symbol->typespec
      && secondsym->value_ptr.symbol->typespec->type == TYPE_FUNCTION)
    {
      second = call_function (secondsym->value_ptr.symbol->typespec,
			      secondsp->type == TYPE_CONS_PAIR ? CDR (secondsp)
			      : &nil_object, 0, 0, 0, 0, 1, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!second)
	return -2;
    }
  else
    second = secondsp;


  if (SYMBOL (first) == &nil_object || SYMBOL (second) == &t_object)
    return 1;

  if (IS_SYMBOL (second) && type_starts_with (first, SYMBOL (second)))
    return 1;

  if (IS_CLASS (first) || IS_CLASS (second))
    {
      if (first->type == TYPE_STANDARD_CLASS)
	first = first->value_ptr.standard_class->name;

      if (first->type == TYPE_STRUCTURE_CLASS)
	first = first->value_ptr.structure_class->name;

      if (second->type == TYPE_STANDARD_CLASS)
	second = second->value_ptr.standard_class->name;

      if (second->type == TYPE_STRUCTURE_CLASS)
	second = second->value_ptr.structure_class->name;

      if (SYMBOL (first) == SYMBOL (second))
	return 1;
    }


  if (IS_SYMBOL (first) && IS_SYMBOL (second))
    {
      p = SYMBOL (first)->value_ptr.symbol->builtin_type
	? SYMBOL (first)->value_ptr.symbol->parent_types
	: SYMBOL (first)->value_ptr.symbol->typespec->type == TYPE_STANDARD_CLASS
	? SYMBOL (first)->value_ptr.symbol->typespec->value_ptr.standard_class->
	parents
	: SYMBOL (first)->value_ptr.symbol->typespec->type == TYPE_CONDITION_CLASS
	? SYMBOL (first)->value_ptr.symbol->typespec->value_ptr.condition_class->
	parents : NULL;

      ret = is_descendant (SYMBOL (first), p, SYMBOL (second), prev, env);

      if (ret)
	return 1;
    }

  if (first->type == TYPE_CONS_PAIR)
    {
      if (SYMBOL (CAR (first)) == env->or_sym)
	{
	  first = CDR (first);

	  while (SYMBOL (first) != &nil_object)
	    {
	      ret = is_subtype (CAR (first), second, NULL, env, outcome);

	      if (ret <= 0)
		return ret;

	      first = CDR (first);
	    }

	  return 1;
	}

      if (SYMBOL (CAR (first)) == env->and_sym)
	{
	  first = CDR (first);

	  while (SYMBOL (first) != &nil_object)
	    {
	      ret = is_subtype (CAR (first), second, NULL, env, outcome);

	      if (ret)
		return ret;

	      first = CDR (first);
	    }

	  return -1;
	}
    }

  if (second->type == TYPE_CONS_PAIR)
    {
      if (SYMBOL (CAR (second)) == env->or_sym)
	{
	  first = CDR (second);

	  while (SYMBOL (second) != &nil_object)
	    {
	      ret = is_subtype (first, CAR (second), NULL, env, outcome);

	      if (ret)
		return ret;

	      second = CDR (second);
	    }

	  return -1;
	}

      if (SYMBOL (CAR (second)) == env->and_sym)
	{
	  second = CDR (second);

	  while (SYMBOL (second) != &nil_object)
	    {
	      ret = is_subtype (first, CAR (second), NULL, env, outcome);

	      if (ret <= 0)
		return ret;

	      second = CDR (second);
	    }

	  return 1;
	}
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
  int stepping_over_this_form;

  if (env->stepping_flags && !(env->stepping_flags & STEPPING_OVER_FORM)
      && !DONT_STEP (obj))
    {
      env->next_eval = obj;

      if (!enter_debugger (NULL, env, outcome))
	return NULL;
    }

  stepping_over_this_form = env->stepping_flags & STEP_OVER_FORM
    && !(env->stepping_flags & STEPPING_OVER_FORM);

  if (stepping_over_this_form && !DONT_STEP (obj) && !IS_SYMBOL (obj))
    env->stepping_flags |= STEPPING_OVER_FORM;

  if (obj->type == TYPE_BACKQUOTE)
    {
      ret = apply_backquote (obj->value_ptr.next, 1, env, outcome, 1, NULL,
			     NULL);

      if (ret && env->stepping_flags
	  && !(env->stepping_flags & STEPPING_OVER_FORM))
	{
	  printf ("backquote evaluated to ");
	  print_object (ret, env, env->c_stdout->value_ptr.stream);
	  printf ("\n");
	  env->c_stdout->value_ptr.stream->dirty_line = 0;
	}
    }
  else if (obj->type == TYPE_SYMBOL || obj->type == TYPE_SYMBOL_NAME)
    {
      sym = SYMBOL (obj);

      if (sym->value_ptr.symbol->is_const
	  || sym->value_ptr.symbol->is_symbol_macro
	  || (sym->value_ptr.symbol->is_special && !env->only_lexical))
	{
	  ret = get_dynamic_value (sym, env);

	  if (!ret)
	    {
	      return raise_unbound_variable (sym, env, outcome);
	    }
	  else if (sym->value_ptr.symbol->is_symbol_macro)
	    {
	      decrement_refcount (ret);
	      ret = evaluate_object (ret, env, outcome);
	    }
	}
      else
	{
	  bind = find_binding (sym->value_ptr.symbol, env->vars,
			       LEXICAL_BINDING, env->lex_env_vars_boundary,
			       env->only_lexical);

	  if (bind)
	    {
	      if (bind->is_symbol_macro)
		{
		  ret = evaluate_object (bind->obj, env, outcome);
		}
	      else
		{
		  increment_refcount (bind->obj);
		  ret = bind->obj;
		}
	    }
	  else if (sym->value_ptr.symbol->value_dyn_bins_num
		   && !env->only_lexical)
	    {
	      bind = find_binding (sym->value_ptr.symbol, env->vars,
				   DYNAMIC_BINDING, -1, 0);
	      increment_refcount (bind->obj);
	      ret = bind->obj;
	    }
	  else if (sym->value_ptr.symbol->value_cell)
	    {
	      increment_refcount (sym->value_ptr.symbol->value_cell);
	      return sym->value_ptr.symbol->value_cell;
	    }
	  else
	    {
	      return raise_unbound_variable (sym, env, outcome);
	    }
	}
    }
  else if (obj->type == TYPE_CONS_PAIR)
    {
      ret = evaluate_list (obj, env, outcome);
    }
  else
    {
      increment_refcount (obj);
      return obj;
    }

  if (!DONT_STEP (obj) &&
      ((IS_SYMBOL (obj)
	&& env->stepping_flags && !(env->stepping_flags & STEPPING_OVER_FORM))
       || ((obj->type == TYPE_CONS_PAIR || obj->type == TYPE_BACKQUOTE)
	   && stepping_over_this_form
	   && (env->stepping_flags & (STEP_OVER_FORM | STEP_OVER_EXPANSION))
	   && (env->stepping_flags & STEPPING_OVER_FORM))))
    {
      if (ret)
	{
	  printf (" -> ");
	  print_object (ret, env, env->c_stdout->value_ptr.stream);
	  printf ("\n");
	  env->c_stdout->value_ptr.stream->dirty_line = 0;
	}

      if (env->stepping_flags & (STEP_OVER_FORM | STEP_OVER_EXPANSION)
	  && env->stepping_flags & STEPPING_OVER_FORM)
	env->stepping_flags = env->stepping_flags & ~STEPPING_OVER_FORM;
    }

  return ret;
}


struct object *
apply_backquote (struct object *form, int backts_commas_balance,
		 struct environment *env, struct outcome *outcome,
		 int forbid_splicing, int *do_splice, struct object **last_pref)
{
  struct object *obj, *ret, *retform, *retcons, *reading_cons, *cons, *lastpr,
    *tmp, *lp, *last_fresh, *first_nonfresh;
  int do_spl;

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
      retform = last_fresh = NULL;

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

	  if (ret == obj)
	    {
	      if (!retform)
		{
		  retform = reading_cons;
		  increment_refcount (retform);
		}

	      if (last_fresh && !CDR (last_fresh))
		{
		  last_fresh->value_ptr.cons_pair->cdr = reading_cons;
		  add_reference (last_fresh, reading_cons, 1);
		}

	      decrement_refcount (ret);
	    }
	  else
	    {
	      if (retform)
		{
		  if (last_fresh)
		    cons = first_nonfresh;
		  else
		    {
		      cons = form;
		      decrement_refcount (form);
		    }

		  while (cons != reading_cons)
		    {
		      if (!last_fresh)
			retform = last_fresh = retcons = alloc_empty_cons_pair ();
		      else
			{
			  retcons = alloc_empty_cons_pair ();
			  delete_reference (last_fresh, CDR (last_fresh), 1);
			  last_fresh->value_ptr.cons_pair->cdr = retcons;
			  add_reference (last_fresh, retcons, 1);
			  decrement_refcount (retcons);
			  last_fresh = retcons;
			}

		      retcons->value_ptr.cons_pair->car = CAR (cons);
		      add_reference (retcons, CAR (cons), 0);

		      first_nonfresh = cons = CDR (cons);
		    }
		}

	      if (!do_spl)
		{
		  if (reading_cons->type == TYPE_CONS_PAIR)
		    {
		      if (!last_fresh)
			retform = last_fresh = retcons = alloc_empty_cons_pair ();
		      else
			{
			  retcons = alloc_empty_cons_pair ();
			  delete_reference (last_fresh, CDR (last_fresh), 1);
			  last_fresh->value_ptr.cons_pair->cdr = retcons;
			  add_reference (last_fresh, retcons, 1);
			  decrement_refcount (retcons);
			  last_fresh = retcons;
			}

		      retcons->value_ptr.cons_pair->car = ret;
		      add_reference (retcons, ret, 0);
		      decrement_refcount (ret);

		      first_nonfresh = CDR (reading_cons);
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
		      add_reference (lp, ret, 0);
		      decrement_refcount (ret);
		    }
		  else
		    tmp = ret;

		  if (last_fresh)
		    retcons->value_ptr.cons_pair->cdr = tmp;
		  else
		    retform = tmp;
		}
	      else if (SYMBOL (ret) == &nil_object)
		{
		  if (retform)
		    first_nonfresh = CDR (reading_cons);
		}
	      else if (SYMBOL (ret) != &nil_object)
		{
		  if (do_spl == 2
		      || (SYMBOL (CDR (reading_cons)) == &nil_object && !lastpr))
		    {
		      if (!retform)
			retform = ret;
		      else
			{
			  last_fresh->value_ptr.cons_pair->cdr = ret;
			  add_reference (last_fresh, ret, 1);
			  decrement_refcount (ret);
			}

		      if (lastpr)
			{
			  retcons = ret;

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

			  last_fresh = retcons;
			}
		      else
			last_fresh = last_cons_pair (ret);

		      if (do_spl == 2)
			last_fresh->value_ptr.cons_pair->cdr = NULL;

		      first_nonfresh = CDR (reading_cons);
		    }
		  else
		    {
		      cons = ret;

		      while (SYMBOL (cons) != &nil_object)
			{
			  if (!last_fresh)
			    retform = last_fresh = retcons =
			      alloc_empty_cons_pair ();
			  else
			    {
			      retcons = alloc_empty_cons_pair ();
			      last_fresh->value_ptr.cons_pair->cdr = retcons;
			      add_reference (last_fresh, retcons, 1);
			      decrement_refcount (retcons);
			      last_fresh = retcons;
			    }

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

		      first_nonfresh = CDR (reading_cons);
		      decrement_refcount (ret);
		    }
		}
	    }

	  if (reading_cons->type != TYPE_CONS_PAIR)
	    break;
	  else
	    reading_cons = CDR (reading_cons);
	}

      if (!retform)
	return &nil_object;

      if (last_fresh && !last_fresh->value_ptr.cons_pair->cdr)
	last_fresh->value_ptr.cons_pair->cdr = &nil_object;

      return retform;
    }

  increment_refcount (form);
  return form;
}


struct object *
evaluate_list (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  struct object *sym = NULL, *fun, *ret;

  if (is_dotted_list (list))
    {
      outcome->type = DOTTED_LIST_NOT_ALLOWED_HERE;

      return NULL;
    }

  if (IS_SYMBOL (CAR (list)))
    {
      sym = SYMBOL (CAR (list));
      fun = get_function (sym, env, 0, 0, 0, 0);

      if (!fun)
	return raise_undefined_function (sym, env, outcome);
    }
  else if (CAR (list)->type == TYPE_CONS_PAIR
	   && SYMBOL (CAR (CAR (list))) == env->lambda_sym)
    {
      fun = evaluate_object (CAR (list), env, outcome);

      if (!fun)
	return NULL;
    }
  else
    {
      outcome->type = INVALID_FUNCTION_CALL;
      increment_refcount (CAR (list));
      outcome->obj = CAR (list);
      return NULL;
    }


  if (fun->type == TYPE_FUNCTION)
    ret = call_function (fun, CDR (list), 1, 0, 1, 0, 0, env, outcome);
  else if (fun->value_ptr.macro->builtin_form)
    ret = call_function (fun, CDR (list), 0, 0, 0, 0, 0, env, outcome);
  else
    ret = call_function (fun, list, 0, 1, 0, 1, 0, env, outcome);

  if (!sym)
    decrement_refcount (fun);

  return ret;
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
type_compiled_function (const struct object *obj, const struct object *typespec,
			struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FUNCTION
    && (obj->value_ptr.function->flags & COMPILED_FUNCTION);
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
type_logical_pathname (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FILENAME && obj->value_ptr.filename->is_logical;
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
    && obj->value_ptr.stream->type == FILE_STREAM;
}


int
type_string_stream (const struct object *obj, const struct object *typespec,
		    struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STREAM
    && obj->value_ptr.stream->type == STRING_STREAM;
}


int
type_synonym_stream (const struct object *obj, const struct object *typespec,
		     struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STREAM
    && obj->value_ptr.stream->type == SYNONYM_STREAM;
}


int
type_broadcast_stream (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STREAM
    && obj->value_ptr.stream->type == BROADCAST_STREAM;
}


int
type_standard_object (const struct object *obj, const struct object *typespec,
		      struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_STANDARD_OBJECT || obj->type == TYPE_STANDARD_CLASS
    || obj->type == TYPE_STRUCTURE_CLASS;
}


int
type_generic_function (const struct object *obj, const struct object *typespec,
		       struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_FUNCTION
    && (obj->value_ptr.function->flags & GENERIC_FUNCTION);
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


int
type_al_backquote (const struct object *obj, const struct object *typespec,
		   struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_BACKQUOTE;
}


int
type_al_comma (const struct object *obj, const struct object *typespec,
	       struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_COMMA;
}


int
type_al_at (const struct object *obj, const struct object *typespec,
	    struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_AT;
}


int
type_al_dot (const struct object *obj, const struct object *typespec,
	     struct environment *env, struct outcome *outcome)
{
  return obj->type == TYPE_DOT;
}


struct object *
builtin_car (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_LIST (CAR (list)))
    return raise_type_error (CAR (list), "CL:LIST", env, outcome);

  if (SYMBOL (CAR (list)) == &nil_object)
    return &nil_object;

  increment_refcount (CAR (CAR (list)));

  return CAR (CAR (list));
}


struct object *
builtin_cdr (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_LIST (CAR (list)))
    return raise_type_error (CAR (list), "CL:LIST", env, outcome);

  if (SYMBOL (CAR (list)) == &nil_object)
    return &nil_object;

  increment_refcount (CDR (CAR (list)));

  return CDR (CAR (list));
}


struct object *
builtin_rplaca (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_CONS_PAIR)
    return raise_type_error (CAR (list), "CL:CONS", env, outcome);

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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_CONS_PAIR)
    return raise_type_error (CAR (list), "CL:CONS", env, outcome);

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

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
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

      if (!IS_LIST (obj))
	return raise_type_error (obj, "CL:LIST", env, outcome);
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
  struct object *ret = &nil_object, *lastlist, *lastcons;

  while (SYMBOL (list) != &nil_object)
    {
      if (SYMBOL (CDR (list)) != &nil_object && !IS_LIST (CAR (list)))
	{
	  return raise_type_error (CAR (list), "CL:LIST", env, outcome);
	}

      if (ret == &nil_object)
	{
	  increment_refcount (CAR (list));
	  ret = CAR (list);
	}
      else
	{
	  lastcons = last_cons_pair (lastlist);
	  delete_reference (lastcons, CDR (lastcons), 1);
	  lastcons->value_ptr.cons_pair->cdr = CAR (list);
	  add_reference (lastcons, CAR (list), 1);
	}

      if (CAR (list)->type == TYPE_CONS_PAIR)
	lastlist = CAR (list);

      list = CDR (list);
    }

  return ret;
}


struct object *
builtin_nth (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_INTEGER
      || mpz_cmp_si (CAR (list)->value_ptr.integer, 0) < 0)
    {
      return raise_type_error (CAR (list), "(CL:INTEGER 0)", env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_CONS_PAIR
      && SYMBOL (CAR (CDR (list))) != &nil_object)
    {
      return raise_type_error (CAR (CDR (list)), "CL:LIST", env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_INTEGER
      || mpz_cmp_si (CAR (list)->value_ptr.integer, 0) < 0)
    {
      return raise_type_error (CAR (list), "(CL:INTEGER 0)", env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_CONS_PAIR
      && SYMBOL (CAR (CDR (list))) != &nil_object)
    {
      return raise_type_error (CAR (CDR (list)), "CL:LIST", env, outcome);
    }

  ret = nthcdr (mpz_get_ui (CAR (list)->value_ptr.integer), CAR (CDR (list)));

  if (!ret)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (CDR (list)), "CL:INTEGER", env, outcome);
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
	  outcome->type = WRONG_NUMBER_OF_AXES;
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
	  outcome->type = WRONG_NUMBER_OF_AXES;
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
      return raise_type_error (CAR (list), "CL:SEQUENCE", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  arr = CAR (list);

  if (arr->type == TYPE_STRING)
    {
      list = CDR (list);

      if (l != 2)
	{
	  return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
	}

      if (CAR (list)->type != TYPE_INTEGER)
	{
	  return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
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

  return raise_type_error (arr, "CL:ARRAY", env, outcome);
}


struct object *
builtin_row_major_aref (struct object *list, struct environment *env,
			struct outcome *outcome)
{
  int ind;
  struct object *ret;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (!IS_ARRAY (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:ARRAY", env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (CDR (list)), "CL:INTEGER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_LIST (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:LIST", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SEQUENCE (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SEQUENCE", env, outcome);
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      ret = copy_string (CAR (list));
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
      return raise_al_wrong_number_of_arguments (2, 3, env, outcome);
    }

  if (!IS_SEQUENCE (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SEQUENCE", env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (CDR (list)), "CL:INTEGER", env, outcome);
    }

  if (l == 3 && SYMBOL (CAR (CDR (CDR (list)))) != &nil_object
      && CAR (CDR (CDR (list)))->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (CDR (CDR (list))), "(CL:OR CL:INTEGER "
			       "CL:NULL)", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_CONS_PAIR && SYMBOL (CAR (list)) != &nil_object)
    {
      return raise_type_error (CAR (list), "CL:LIST", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      return raise_type_error (CAR (list), "CL:SEQUENCE", env, outcome);
    }
}


struct object *
builtin_fill_pointer (struct object *list, struct environment *env,
		      struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
  int indx, tot = 1, fillp = -1, found_unknown_key = 0, i, rowsize;
  struct object *ret, *cons, *dims, *fp = NULL, *initial_contents = NULL,
    *element_type = NULL, *allow_other_keys = NULL, *el;
  struct array_size *size = NULL, *sz;
  enum object_type objt;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
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
      else if (symbol_equals (CAR (list), ":ELEMENT-TYPE", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!element_type)
	    element_type = CAR (CDR (list));

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":INITIAL-CONTENTS", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (IS_SEQUENCE (CAR (CDR (list))))
	    {
	      if (!initial_contents)
		initial_contents = CAR (CDR (list));
	    }
	  else
	    {
	      return raise_type_error (CAR (CDR (list)), "CL:SEQUENCE", env,
				       outcome);
	    }

	  list = CDR (list);
	}
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }

  if (!element_type)
    element_type = &t_object;

  if (fp && fp->type == TYPE_INTEGER)
    {
      fillp = mpz_get_si (fp->value_ptr.integer);

      if (fillp < 0)
	{
	  return raise_type_error (fp, "(CL:INTEGER 0)", env, outcome);
	}
    }

  if (is_subtype_by_char_vector (element_type, "CHARACTER", env))
    objt = TYPE_STRING;
  else
    objt = TYPE_ARRAY;

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

      if (objt != TYPE_STRING)
	{
	  ret = alloc_vector (indx, 1, 0);

	  if (fp && SYMBOL (fp) == &t_object)
	    ret->value_ptr.array->fill_pointer = indx;
	  else
	    ret->value_ptr.array->fill_pointer = fillp;
	}
    }
  else if (dims->type == TYPE_CONS_PAIR)
    {
      cons = dims;

      if (fp && list_length (dims) != 1)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (objt != TYPE_STRING || list_length (dims) != 1)
	{
	  while (SYMBOL (cons) != &nil_object)
	    {
	      if (CAR (cons)->type != TYPE_INTEGER)
		{
		  return raise_type_error (CAR (cons), "CL:INTEGER", env,
					   outcome);
		}

	      indx = mpz_get_si (CAR (cons)->value_ptr.integer);

	      if (indx < 0)
		{
		  outcome->type = INVALID_SIZE;
		  return NULL;
		}

	      if (size)
		{
		  sz->next = malloc_and_check (sizeof (*sz->next));
		  sz = sz->next;
		}
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

	  sz->next = NULL;

	  ret = alloc_vector (tot, 1, 1);

	  if (fp && SYMBOL (fp) == &t_object)
	    ret->value_ptr.array->fill_pointer = indx;
	  else
	    ret->value_ptr.array->fill_pointer = fillp;

	  ret->value_ptr.array->alloc_size = size;
	}
      else
	{
	  if (CAR (dims)->type != TYPE_INTEGER)
	    {
	      return raise_type_error (CAR (dims), "CL:INTEGER", env,
				       outcome);
	    }

	  indx = mpz_get_si (CAR (cons)->value_ptr.integer);

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
	}
    }
  else if (SYMBOL (dims) == &nil_object)
    {
      if (fp)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      ret = alloc_vector (1, 1, 1);
      ret->value_ptr.array->alloc_size = NULL;
    }
  else
    {
      return raise_type_error (dims, "(CL:OR CL:INTEGER CL:LIST)", env, outcome);
    }

  if (initial_contents)
    {
      if (objt == TYPE_STRING
	  && (dims->type == TYPE_INTEGER || list_length (dims) != 1))
	{
	  if (!IS_SEQUENCE (initial_contents))
	    {
	      return raise_type_error (initial_contents, "CL:SEQUENCE", env,
				       outcome);
	    }

	  ret = create_string_from_sequence (initial_contents, indx);

	  if (!ret)
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  if (fp && SYMBOL (fp) == &t_object)
	    ret->value_ptr.string->fill_pointer = indx;
	  else
	    ret->value_ptr.string->fill_pointer = fillp;
	}
      else
	{
	  rowsize = 1;
	  sz = ret->value_ptr.array->alloc_size->next;

	  while (sz)
	    {
	      rowsize *= sz->size;
	      sz = sz->next;
	    }

	  for (i = 0; i < ret->value_ptr.array->alloc_size->size; i++)
	    {
	      el = elt (initial_contents, i);

	      if (ret->value_ptr.array->alloc_size->next
		  && (!IS_SEQUENCE (el)
		      || SEQUENCE_LENGTH (el)
		      != ret->value_ptr.array->alloc_size->next->size))
		{
		  outcome->type = WRONG_TYPE_OF_ARGUMENT;
		  return NULL;
		}

	      if (!fill_axis_from_sequence (ret, &ret->value_ptr.array->value
					    [i*rowsize], i*rowsize,
					    ret->value_ptr.array->alloc_size->next,
					    rowsize, el))
		{
		  return NULL;
		}

	      if (initial_contents->type == TYPE_STRING
		  || initial_contents->type == TYPE_BITARRAY)
		{
		  decrement_refcount (el);
		}
	    }
	}
    }
  else if (objt == TYPE_STRING
	   && (dims->type == TYPE_INTEGER
	       || (dims->type == TYPE_CONS_PAIR && list_length (dims) == 1)))
    {
      ret = alloc_string (indx);

      for (i = 0; i < indx; i++)
	ret->value_ptr.string->value [i] = 0;

      ret->value_ptr.string->used_size = indx;

      if (fp && SYMBOL (fp) == &t_object)
	ret->value_ptr.string->fill_pointer = indx;
      else
	ret->value_ptr.string->fill_pointer = fillp;
    }

  return ret;
}


struct object *
builtin_vector (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  return create_vector_from_list (list, -1);
}


struct object *
builtin_array_has_fill_pointer_p (struct object *list, struct environment *env,
				  struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_ARRAY (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:ARRAY", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
  else if (arr->type == TYPE_ARRAY || arr->type == TYPE_BITARRAY)
    {
      sz = arr->type == TYPE_ARRAY ? arr->value_ptr.array->alloc_size
	: arr->value_ptr.bitarray->alloc_size;

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
      return raise_type_error (CAR (list), "CL:ARRAY", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_ARRAY (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:ARRAY", env, outcome);
    }

  arr = CAR (list);
  list = CDR (list);

  if (arr->type == TYPE_STRING)
    {
      if (l != 2)
	{
	  return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
	}

      if (CAR (list)->type != TYPE_INTEGER)
	{
	  return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
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
	  outcome->type = WRONG_NUMBER_OF_AXES;
	  return NULL;
	}

      if (CAR (list)->type != TYPE_INTEGER)
	{
	  return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
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
      outcome->type = WRONG_NUMBER_OF_AXES;
      return NULL;
    }

  return create_integer_from_long (tot);
}


struct object *
builtin_adjust_array (struct object *list, struct environment *env,
		      struct outcome *outcome)
{
  fixnum newsz, oldsz, i, newchsz, r, newtotsz;
  struct object *cons, *ret;
  struct array_size *ind, *in, *size, *s;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_CONS_PAIR
      && CAR (CDR (list))->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (CDR (list)), "(CL:OR CL:CONS CL:INTEGER)",
			       env, outcome);
    }

  if (CAR (CDR (list))->type == TYPE_CONS_PAIR
      && CAR (CAR (CDR (list)))->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (CAR (CDR (list))), "CL:INTEGER", env,
			       outcome);
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

      if (CAR (list)->value_ptr.array->alloc_size->next)
	{
	  ind = in = malloc_and_check (sizeof (*in));
	  size = s = malloc_and_check (sizeof (*s));
	  cons = CAR (CDR (list));
	  s->size = mpz_get_si (CAR (cons)->value_ptr.integer);

	  r = array_rank (CAR (list)->value_ptr.array->alloc_size);

	  for (i = 1; i < r; i++)
	    {
	      in->next = malloc_and_check (sizeof (*in));
	      s->next = malloc_and_check (sizeof (*s));
	      in = in->next;
	      s = s->next;

	      cons = CDR (cons);
	      s->size = mpz_get_si (CAR (cons)->value_ptr.integer);
	    }

	  s->next = in->next = NULL;

	  newtotsz = array_total_size (size);

	  ret = alloc_vector (newtotsz, 1, 1);
	  ret->value_ptr.array->alloc_size = size;

	  for (i = 0; i < CAR (list)->value_ptr.array->alloc_size->size
		 && i < ret->value_ptr.array->alloc_size->size; i++)
	    {
	      ind->size = i;
	      adjust_array_axis (ret, ret->value_ptr.array->alloc_size->next,
				 CAR (list),
				 CAR (list)->value_ptr.array->alloc_size->next,
				 ind, ind->next);
	    }

	  while (ind)
	    {
	      in = ind->next;
	      free (ind);
	      ind = in;
	    }

	  return ret;
	}
      else
	{
	  if (CAR (list)->value_ptr.array->alloc_size->size > newsz)
	    {
	      for (i = newsz; i < CAR (list)->value_ptr.array->alloc_size->size;
		   i++)
		{
		  delete_reference (CAR (list),
				    CAR (list)->value_ptr.array->value [i], i);
		}
	    }

	  CAR (list)->value_ptr.array->value =
	    realloc_and_check (CAR (list)->value_ptr.array->value, newsz
			       * sizeof (*CAR (list)->value_ptr.array->value));

	  if (CAR (list)->value_ptr.array->alloc_size->size < newsz)
	    {
	      for (i = CAR (list)->value_ptr.array->alloc_size->size; i < newsz;
		   i++)
		{
		  CAR (list)->value_ptr.array->value [i] = &nil_object;
		}
	    }

	  CAR (list)->value_ptr.array->alloc_size->size = newsz;

	  CAR (list)->value_ptr.array->reference_strength_factor =
	    realloc_and_check (CAR (list)->value_ptr.array->reference_strength_factor,
			       newsz * sizeof (int));
	}
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
      return raise_type_error (CAR (list), "CL:ARRAY", env, outcome);
    }

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_sxhash (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  return create_integer_from_long (hash_object_respecting_equal (CAR (list),
								 1024));
}


struct object *
builtin_make_hash_table (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  struct object *ret, *allow_other_keys = NULL, *fun, *name;
  struct hashtable *ht;
  enum hashtable_type type = HT_NONE;
  int found_unknown_key = 0;

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":TEST", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (IS_SYMBOL (CAR (CDR (list))))
	    {
	      fun = get_function (SYMBOL (CAR (CDR (list))), env, 1, 0, 1, 0);

	      if (!fun)
		{
		  return raise_undefined_function (SYMBOL (CAR (CDR (list))), env,
						   outcome);
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

	  name = fun->value_ptr.function->name;

	  if (name && symbol_equals (name, "EQ", env))
	    {
	      if (!type)
		type = HT_EQ;
	    }
	  else if (name && symbol_equals (name, "EQL", env))
	    {
	      if (!type)
		type = HT_EQL;
	    }
	  else if (name && symbol_equals (name, "EQUAL", env))
	    {
	      if (!type)
		type = HT_EQUAL;
	    }
	  else if (name && symbol_equals (name, "EQUALP", env))
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
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_HASHTABLE)
    {
      return raise_type_error (CAR (list), "CL:HASH-TABLE", env, outcome);
    }

  return create_integer_from_long (LISP_HASHTABLE_SIZE);
}


struct object *
builtin_hash_table_count (struct object *list, struct environment *env,
			  struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_HASHTABLE)
    {
      return raise_type_error (CAR (list), "CL:HASH-TABLE", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_HASHTABLE)
    {
      return raise_type_error (CAR (list), "CL:HASH-TABLE", env, outcome);
    }

  t = CAR (list)->value_ptr.hashtable->type;

  switch (t)
    {
    case HT_EQ:
      ret = intern_symbol_by_char_vector ("EQ", strlen ("EQ"), 1,
					  EXTERNAL_VISIBILITY, 1,
					  env->cl_package, 0, 0);
      break;
    case HT_EQL:
      ret = intern_symbol_by_char_vector ("EQL", strlen ("EQL"), 1,
					  EXTERNAL_VISIBILITY, 1,
					  env->cl_package, 0, 0);
      break;
    case HT_EQUAL:
      ret = intern_symbol_by_char_vector ("EQUAL", strlen ("EQUAL"), 1,
					  EXTERNAL_VISIBILITY, 1,
					  env->cl_package, 0, 0);
      break;
    case HT_EQUALP:
      ret = intern_symbol_by_char_vector ("EQUALP", strlen ("EQUALP"), 1,
					  EXTERNAL_VISIBILITY, 1,
					  env->cl_package, 0, 0);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_HASHTABLE)
    {
      return raise_type_error (CAR (CDR (list)), "CL:HASH-TABLE", env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_HASHTABLE)
    {
      return raise_type_error (CAR (CDR (list)), "CL:HASH-TABLE", env, outcome);
    }

  r = find_hashtable_record (CAR (list), CAR (CDR (list)), &ind, &j, &prev);

  if (r)
    {
      if (IS_WATCHED (CAR (CDR (list))))
	{
	  env->watched_obj = CAR (CDR (list));
	  env->obj_field = CAR (list);
	  env->new_value = NULL;

	  if (!enter_debugger (NULL, env, outcome))
	    return NULL;
	}

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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_HASHTABLE)
    {
      return raise_type_error (CAR (list), "CL:HASH-TABLE", env, outcome);
    }

  if (IS_WATCHED (CAR (list)))
    {
      env->watched_obj = CAR (list);
      env->obj_field = NULL;
      env->new_value = NULL;

      if (!enter_debugger (NULL, env, outcome))
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type == TYPE_SYMBOL_NAME || CAR (list)->type == TYPE_SYMBOL)
    {
      fun = get_function (SYMBOL (CAR (list)), env, 1, 0, 0, 0);

      if (!fun)
	{
	  return raise_undefined_function (SYMBOL (CAR (list)), env, outcome);
	}
    }
  else if (CAR (list)->type == TYPE_FUNCTION)
    {
      fun = CAR (list);
    }
  else
    {
      return raise_type_error (CAR (list), "(CL:OR CL:SYMBOL CL:FUNCTION)", env,
			       outcome);
    }

  if (CAR (CDR (list))->type != TYPE_HASHTABLE)
    {
      return raise_type_error (CAR (CDR (list)), "CL:HASH-TABLE", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_CONS_PAIR && SYMBOL (CAR (list)) != &nil_object)
    {
      return raise_type_error (CAR (list), "CL:LIST", env, outcome);
    }

  if (length == 2 && CAR (CDR (list))->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (CDR (list)), "CL:INTEGER", env, outcome);
    }

  if (length == 2)
    {
      n = mpz_get_si (CAR (CDR (list))->value_ptr.integer);

      if (n < 0)
	{
	  return raise_type_error (CAR (CDR (list)), "(CL:INTEGER 0)", env,
				   outcome);
	}
    }

  ret = nthcdr (list_length (CAR (list)) - n, CAR (list));

  increment_refcount (ret);
  return ret;
}


struct object *
builtin_pathname (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      ret = create_filename (CAR (list));
    }
  else if (CAR (list)->type == TYPE_STREAM
	   && CAR (list)->value_ptr.stream->type == FILE_STREAM)
    {
      ret = create_filename (CAR (list)->value_ptr.stream->namestring);
    }
  else if (CAR (list)->type == TYPE_FILENAME)
    {
      ret = CAR (list);
      increment_refcount (ret);
    }
  else
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  return ret;
}


struct object *
builtin_make_pathname (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct object *directory = NULL, *name = NULL, *type = NULL, *defaults = NULL,
    *ret, *allow_other_keys = NULL, *value, *cons;
  int found_unknown_key = 0, size = 0, i, s, s2;
  enum filename_type dir_type = REGULAR_FILENAME, name_type = REGULAR_FILENAME;

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":HOST", env)
	  || symbol_equals (CAR (list), ":DEVICE", env)
	  || symbol_equals (CAR (list), ":VERSION", env)
	  || symbol_equals (CAR (list), ":CASE", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":DIRECTORY", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!directory)
	    {
	      directory = CAR (CDR (list));
	    }

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":NAME", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!name)
	    {
	      name = CAR (CDR (list));
	    }

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":TYPE", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!type)
	    {
	      type = CAR (CDR (list));
	    }

	  list = CDR (list);
	}
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":DEFAULTS", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!defaults)
	    {
	      defaults = CAR (CDR (list));
	    }

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }

  if (defaults && !IS_PATHNAME_DESIGNATOR (defaults))
    {
      return raise_type_error (defaults, "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }
  else if (defaults)
    defaults = inspect_pathname_by_designator (defaults);

  if (directory)
    {
      if (symbol_equals (directory, ":WILD", env))
	{
	  size = 4;
	  dir_type = WILD_FILENAME;
	}
      else if (directory->type == TYPE_CONS_PAIR)
	{
	  if (symbol_equals (CAR (directory), ":ABSOLUTE", env))
	    {
	      if (SYMBOL (CDR (directory)) == &nil_object
		  || CAR (CDR (directory))->type != TYPE_STRING
		  || !CAR (CDR (directory))->value_ptr.string->used_size
		  || CAR (CDR (directory))->value_ptr.string->value [0] != '/')
		{
		  size = 1;
		}
	    }
	  else if (symbol_equals (CAR (directory), ":RELATIVE", env))
	    size = 0;
	  else
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  cons = CDR (directory);

	  while (SYMBOL (cons) != &nil_object)
	    {
	      if (CAR (cons)->type == TYPE_STRING)
		{
		  size += CAR (cons)->value_ptr.string->used_size;

		  if ((!CAR (cons)->value_ptr.string->used_size
		       || CAR (cons)->value_ptr.string->value
		       [CAR (cons)->value_ptr.string->used_size-1] != '/')
		      && (SYMBOL (CDR (cons)) == &nil_object
			  || CAR (CDR (cons))->type != TYPE_STRING
			  || !CAR (CDR (cons))->value_ptr.string->used_size
			  || CAR (CDR (cons))->value_ptr.string->value [0] != '/'))
		    {
		      size++;
		    }
		}
	      else if (symbol_equals (CAR (cons), ":WILD", env))
		{
		  size += 2;
		  dir_type = WILD_FILENAME;
		}
	      else if (symbol_equals (CAR (cons), ":WILD-INFERIORS", env))
		{
		  size += 3;
		  dir_type = WILD_FILENAME;
		}
	      else
		{
		  outcome->type = WRONG_TYPE_OF_ARGUMENT;
		  return NULL;
		}

	      cons = CDR (cons);
	    }
	}
      else if (directory->type == TYPE_STRING)
	{
	  size += directory->value_ptr.string->used_size
	    + (!directory->value_ptr.string->used_size
	       || directory->value_ptr.string->value [0] != '/')
	    + (!directory->value_ptr.string->used_size
	       || directory->value_ptr.string->value
	       [directory->value_ptr.string->used_size-1] != '/');
	}
      else if (SYMBOL (directory) != &nil_object)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}
    }

  if (!directory && defaults)
    {
      s = get_directory_file_split (defaults);

      if (s >= 0)
	size += s+1;
    }

  if (name)
    {
      if (symbol_equals (name, ":WILD", env))
	{
	  name = NULL;
	  size += 1;
	  name_type = WILD_FILENAME;
	}
      else if (name->type == TYPE_STRING)
	{
	  size += name->value_ptr.string->used_size;
	}
      else if (SYMBOL (name) != &nil_object)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}
    }

  if (!name && defaults)
    {
      s = get_filename (defaults, &s2);

      size += (s2 >= 0 ? s2 : defaults->value_ptr.string->used_size)-s-1;
    }

  if (type)
    {
      if (type->type == TYPE_STRING)
	{
	  size += type->value_ptr.string->used_size+1;
	}
      else if (SYMBOL (type) != &nil_object
	       && !symbol_equals (type, ":WILD", env))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}
    }

  if (!type && defaults)
    {
      s = get_filename (defaults, &s2);

      if (s2 >= 0)
	size += defaults->value_ptr.string->used_size-s2;
    }


  value = alloc_string (size);
  i = 0;

  if (directory && symbol_equals (directory, ":WILD", env))
    {
      memcpy (value->value_ptr.string->value, "/**/", 4);
      i += 4;
    }
  else if (directory && directory->type == TYPE_STRING)
    {
      if (!directory->value_ptr.string->used_size
	  || directory->value_ptr.string->value [0] != '/')
	{
	  memcpy (value->value_ptr.string->value, "/", 1);
	  i++;
	}

      memcpy (value->value_ptr.string->value+i,
	      directory->value_ptr.string->value,
	      directory->value_ptr.string->used_size);
      i += directory->value_ptr.string->used_size;

      if (!directory->value_ptr.string->used_size
	  || directory->value_ptr.string->value
	  [directory->value_ptr.string->used_size-1] != '/')
      {
	memcpy (value->value_ptr.string->value+i, "/", 1);
	i++;
      }
    }
  else if (directory && directory->type == TYPE_CONS_PAIR)
    {
      if (symbol_equals (CAR (directory), ":ABSOLUTE", env))
	{
	  if (SYMBOL (CDR (directory)) == &nil_object
	      || CAR (CDR (directory))->type != TYPE_STRING
	      || !CAR (CDR (directory))->value_ptr.string->used_size
	      || CAR (CDR (directory))->value_ptr.string->value [0] != '/')
	    {
	      memcpy (value->value_ptr.string->value, "/", 1);
	      i++;
	    }
	}

      cons = CDR (directory);

      while (SYMBOL (cons) != &nil_object)
	{
	  if (CAR (cons)->type == TYPE_STRING)
	    {
	      memcpy (value->value_ptr.string->value+i,
		      CAR (cons)->value_ptr.string->value,
		      CAR (cons)->value_ptr.string->used_size);
	      i += CAR (cons)->value_ptr.string->used_size;

	      if ((!CAR (cons)->value_ptr.string->used_size
		   || CAR (cons)->value_ptr.string->value
		   [CAR (cons)->value_ptr.string->used_size-1] != '/')
		  && (SYMBOL (CDR (cons)) == &nil_object
		      || CAR (CDR (cons))->type != TYPE_STRING
		      || !CAR (CDR (cons))->value_ptr.string->used_size
		      || CAR (CDR (cons))->value_ptr.string->value [0] != '/'))
		{
		  memcpy (value->value_ptr.string->value+i, "/", 1);
		  i++;
		}
	    }
	  else if (symbol_equals (CAR (cons), ":WILD", env))
	    {
	      memcpy (value->value_ptr.string->value+i, "*/", 2);
	      i += 2;
	    }
	  else
	    {
	      memcpy (value->value_ptr.string->value+i, "**/", 3);
	      i += 3;
	    }

	  cons = CDR (cons);
	}
    }
  else if ((!directory || SYMBOL (directory) != &nil_object) && defaults)
    {
      memcpy (value->value_ptr.string->value, defaults->value_ptr.string->value,
	      s+1);
      i += s+1;
    }

  if (name_type == WILD_FILENAME)
    {
      memcpy (value->value_ptr.string->value+i, "*", 1);
      i++;
    }
  else if (name && name->type == TYPE_STRING)
    {
      memcpy (value->value_ptr.string->value+i,
	      name->value_ptr.string->value, name->value_ptr.string->used_size);
      i += name->value_ptr.string->used_size;
    }
  else if ((!name || SYMBOL (name) != &nil_object) && defaults)
    {
      memcpy (value->value_ptr.string->value+i,
	      defaults->value_ptr.string->value+s+1,
	      (s2 >= 0 ? s2 : defaults->value_ptr.string->used_size)-s-1);
      i += (s2 >= 0 ? s2 : defaults->value_ptr.string->used_size)-s-1;
    }

  if (type && type->type == TYPE_STRING)
    {
      memcpy (value->value_ptr.string->value+i, ".", 1);
      i++;
      memcpy (value->value_ptr.string->value+i,
	      type->value_ptr.string->value, type->value_ptr.string->used_size);
      i += type->value_ptr.string->used_size;
    }
  else if ((!type || !IS_SYMBOL (type)) && defaults)
    {
      if (s2 >= 0)
	{
	  memcpy (value->value_ptr.string->value+i, ".", 1);
	  i++;
	  memcpy (value->value_ptr.string->value+i,
		  defaults->value_ptr.string->value+s2+1,
		  defaults->value_ptr.string->used_size-s2-1);
	  size += defaults->value_ptr.string->used_size-s2;
	}
    }

  value->value_ptr.string->used_size = i;

  ret = create_filename (value);
  decrement_refcount (ret->value_ptr.filename->value);

  ret->value_ptr.filename->directory_type = dir_type;
  ret->value_ptr.filename->name_type = name_type;

  return ret;
}


struct object *
builtin_namestring (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct object *ns;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = inspect_pathname_by_designator (CAR (list));

  increment_refcount (ns);
  return ns;
}


struct object *
builtin_al_pathname_directory (struct object *list, struct environment *env,
			       struct outcome *outcome)
{
  struct object *ns, *allow_other_keys = NULL;
  int found_unknown_key = 0, s;

  if (SYMBOL (list) == &nil_object)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = inspect_pathname_by_designator (CAR (list));

  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":CASE", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }

  s = get_directory_file_split (ns);

  if (s < 0)
    return &nil_object;

  if (s == ns->value_ptr.string->used_size-1)
    {
      increment_refcount (ns);
      return ns;
    }

  return create_string_copying_char_vector (ns->value_ptr.string->value, s+1);
}


struct object *
builtin_pathname_name (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct object *ns, *allow_other_keys = NULL;
  int found_unknown_key = 0, s, dot;

  if (SYMBOL (list) == &nil_object)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = inspect_pathname_by_designator (CAR (list));

  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":CASE", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }

  if (!ns->value_ptr.string->used_size)
    return &nil_object;

  if (ns->value_ptr.string->value [ns->value_ptr.string->used_size-1] == '.'
      && (ns->value_ptr.string->used_size == 1
	  || ns->value_ptr.string->value [ns->value_ptr.string->used_size-2] == '/'))
    {
      return create_string_copying_c_string (".");
    }

  s = get_filename (ns, &dot);

  if (s < 0 && dot < 0)
    {
      increment_refcount (ns);
      return ns;
    }

  if (s < 0)
    return create_string_copying_char_vector (ns->value_ptr.string->value, dot);

  if (s == ns->value_ptr.string->used_size-1)
    return &nil_object;

  return create_string_copying_char_vector (ns->value_ptr.string->value+s+1,
					    dot >= 0 ? dot-s-1 :
					    ns->value_ptr.string->used_size-s-1);
}


struct object *
builtin_pathname_type (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct object *ns, *allow_other_keys = NULL;
  int found_unknown_key = 0, i;

  if (SYMBOL (list) == &nil_object)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = inspect_pathname_by_designator (CAR (list));

  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":CASE", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }

  if (!ns->value_ptr.string->used_size
      || ns->value_ptr.string->value [ns->value_ptr.string->used_size-1] == '/')
    return &nil_object;

  if (ns->value_ptr.string->value [ns->value_ptr.string->used_size-1] == '.'
      && ns->value_ptr.string->used_size > 1
      && ns->value_ptr.string->value [ns->value_ptr.string->used_size-2] != '/')
    return alloc_string (0);


  for (i = ns->value_ptr.string->used_size-2; i >= 0; i--)
    {
      if (ns->value_ptr.string->value [i] == '/')
	return &nil_object;

      if (ns->value_ptr.string->value [i] == '.')
	return create_string_copying_char_vector
	  (ns->value_ptr.string->value+i+1, ns->value_ptr.string->used_size-i-1);
    }

  return &nil_object;
}


struct object *
builtin_wild_pathname_p (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  int l = list_length (list);

  if (!l || l > 2)
    {
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  if (l == 2 && !IS_SYMBOL (CAR (CDR (list))))
    {
      return raise_type_error (CAR (CDR (list)), "CL:SYMBOL", env, outcome);
    }

  if (l == 1 || symbol_equals (CAR (CDR (list)), ":NAME", env))
    {
      if (CAR (list)->type == TYPE_STREAM || CAR (list)->type == TYPE_STRING
	  || CAR (list)->value_ptr.filename->name_type != WILD_FILENAME)
	return &nil_object;

      return &t_object;
    }
  else if (symbol_equals (CAR (CDR (list)), ":DIRECTORY", env))
    {
      return CAR (list)->value_ptr.filename->directory_type == WILD_FILENAME
	? &t_object : &nil_object;
    }
  else if (symbol_equals (CAR (CDR (list)), ":HOST", env)
	   || symbol_equals (CAR (CDR (list)), ":DEVICE", env)
	   || symbol_equals (CAR (CDR (list)), ":TYPE", env)
	   || symbol_equals (CAR (CDR (list)), ":VERSION", env))
    {
      return &nil_object;
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }
}


struct object *
builtin_logical_pathname (struct object *list, struct environment *env,
			  struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ret = create_filename (copy_string (inspect_pathname_by_designator (CAR (list))));
  ret->value_ptr.filename->is_logical = 1;
  decrement_refcount (ret->value_ptr.filename->value);
  return ret;
}


struct object *
builtin_translate_logical_pathname (struct object *list, struct environment *env,
				    struct outcome *outcome)
{
  struct object *ret;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ret = create_filename (copy_string (inspect_pathname_by_designator (CAR (list))));
  decrement_refcount (ret->value_ptr.filename->value);
  return ret;
}


struct object *
builtin_truename (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  FILE *f;
  char *fn;
  struct object *ns;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = inspect_pathname_by_designator (CAR (list));

  fn = copy_string_to_c_string (ns->value_ptr.string);

  f = fopen (fn, "r");

  free (fn);

  if (!f)
    {
      return raise_file_error (CAR (list), NULL, env, outcome);
    }

  fclose (f);

  return create_filename (ns);
}


struct object *
builtin_probe_file (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  FILE *f;
  char *fn;
  struct object *ns;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = inspect_pathname_by_designator (CAR (list));

  fn = copy_string_to_c_string (ns->value_ptr.string);

  f = fopen (fn, "r");

  free (fn);

  if (!f)
    {
      return &nil_object;
    }

  fclose (f);

  return create_filename (ns);
}


struct object *
builtin_ensure_directories_exist (struct object *list, struct environment *env,
				  struct outcome *outcome)
{
  struct object *ns;
  char *fn;
  int i, finished = 0, created = 0, ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = inspect_pathname_by_designator (CAR (list));

  fn = copy_string_to_c_string (ns->value_ptr.string);
  i = 1;

  while (1)
    {
      for (; fn [i] && fn [i] != '/'; i++);

      if (!fn [i] && fn [i-1] != '/')
	break;
      else if (!fn [i])
	finished = 1;

      fn [i] = 0;

      ret = mkdir (fn, 0755);

      if (ret && errno != EEXIST)
	{
	  free (fn);
	  outcome->type = COULD_NOT_CREATE_DIR;
	  return NULL;
	}
      else if (!ret)
	created = 1;

      if (finished)
	break;

      fn [i] = '/';
      i++;
    }

  free (fn);

  prepend_object_to_obj_list (created ? &t_object : &nil_object,
			      &outcome->other_values);

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_file_position (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  int l = list_length (list);
  long ret;

  if (!l || l > 2)
    {
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM
      || CAR (list)->value_ptr.stream->type != FILE_STREAM)
    {
      return raise_type_error (CAR (list), "CL:FILE-STREAM", env, outcome);
    }

  if (l == 2 && (CAR (CDR (list))->type != TYPE_INTEGER
		 || mpz_cmp_si (CAR (CDR (list))->value_ptr.integer, 0) < 0))
    {
      return raise_type_error (CAR (CDR (list)), "(CL:INTEGER 0)", env, outcome);
    }

  if (l == 1)
    {
      ret = ftell (CAR (list)->value_ptr.stream->file);

      if (ret < 0)
	return &nil_object;

      return create_integer_from_long (ret);
    }

  if (fseek (CAR (list)->value_ptr.stream->file,
	     mpz_get_si (CAR (CDR (list))->value_ptr.integer), SEEK_SET))
    {
      return &nil_object;
    }

  return &t_object;
}


struct object *
builtin_file_length (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  FILE *f;
  long oldp, l;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM
      || CAR (list)->value_ptr.stream->type != FILE_STREAM)
    {
      return raise_type_error (CAR (list), "CL:FILE-STREAM", env, outcome);
    }

  f = CAR (list)->value_ptr.stream->file;

  if ((oldp = ftell (f)) == -1)
    return &nil_object;

  if (fseek (f, 0l, SEEK_END))
    return &nil_object;

  if ((l = ftell (f)) == -1)
    return &nil_object;

  if (fseek (f, oldp, SEEK_SET))
    return &nil_object;

  return create_integer_from_long (l);
}


struct object *
builtin_rename_file (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  char *oldn, *newn;
  int ret;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (CDR (list)))
      || CAR (CDR (list))->type == TYPE_STREAM)
    {
      return raise_type_error (CAR (CDR (list)), "(CL:OR CL:STRING CL:PATHNAME)",
			       env, outcome);
    }

  oldn = copy_string_to_c_string
    (inspect_pathname_by_designator (CAR (list))->value_ptr.string);

  newn = copy_string_to_c_string
    (inspect_pathname_by_designator (CAR (CDR (list)))->value_ptr.string);

  ret = rename (oldn, newn);

  free (oldn);
  free (newn);

  if (ret)
    {
      return raise_file_error (inspect_pathname_by_designator (CAR (list)), NULL,
			       env, outcome);
    }

  prepend_object_to_obj_list
    (create_filename (inspect_pathname_by_designator (CAR (CDR (list)))),
     &outcome->other_values);
  prepend_object_to_obj_list
    (create_filename (inspect_pathname_by_designator (CAR (list))),
     &outcome->other_values);

  return create_filename (inspect_pathname_by_designator (CAR (CDR (list))));
}


struct object *
builtin_delete_file (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  char *fn;
  int ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  fn = copy_string_to_c_string
    (inspect_pathname_by_designator (CAR (list))->value_ptr.string);

  ret = remove (fn);

  free (fn);

  if (ret)
    {
      return raise_file_error (inspect_pathname_by_designator (CAR (list)), NULL,
			       env, outcome);
    }

  return &t_object;
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
      return raise_al_wrong_number_of_arguments (0, 1, env, outcome);
    }

  if (l && CAR (list)->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (list), "CL:STREAM", env, outcome);
    }

  str = l ? CAR (list) : inspect_variable (env->std_in_sym, env);
  s = str->value_ptr.stream;

  if (s->type == SYNONYM_STREAM &&
      !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  s = str->value_ptr.stream;

  if (s->type == FILE_STREAM)
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
      ret->value_ptr.string->fill_pointer = -1;
    }
  else if (s->type == STRING_STREAM)
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
  struct object *str, *ret = NULL, *newstr, *eofval = NULL;
  enum outcome_type out;
  const char *objbeg, *objend;

  if (l > 3)
    {
      return raise_al_wrong_number_of_arguments (0, 3, env, outcome);
    }

  if (l && CAR (list)->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (list), "CL:STREAM", env, outcome);
    }

  if (l)
    str = CAR (list);
  else
    str = inspect_variable (env->std_in_sym, env);

  s = str->value_ptr.stream;

  if (s->type == SYNONYM_STREAM
      && !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  s = str->value_ptr.stream;

  if (l >= 2 && SYMBOL (CAR (CDR (list))) == &nil_object)
    eofval = &nil_object;

  if (l >= 3 && eofval)
    eofval = CAR (CDR (CDR (list)));

  if (s->type == STRING_STREAM)
    objend = s->string->value_ptr.string->value;

  do
    {
      out = read_object (&ret, 0, s->type == STRING_STREAM ? objend : NULL,
			 s->type == STRING_STREAM ?
			 s->string->value_ptr.string->used_size
			 -(objend-s->string->value_ptr.string->value) : 0,
			 s->type == FILE_STREAM ? s->file : NULL,
			 0, 1, env, outcome, &objbeg, &objend);
      clear_read_labels (&env->read_labels);

      if (out == SKIPPED_OBJECT)
	objend++;
    } while (out == SKIPPED_OBJECT);

  if (IS_READ_OR_EVAL_ERROR (out))
    {
      CLEAR_READER_STATUS (*outcome);
      outcome->type = out;
      return NULL;
    }

  if (IS_INCOMPLETE_OBJECT (out) || out == NO_OBJECT)
    {
      CLEAR_READER_STATUS (*outcome);

      if (s->type == FILE_STREAM && ferror (s->file))
	outcome->type = ERROR_READING_FILE;
      else if (out == NO_OBJECT)
	{
	  if (eofval)
	    {
	      increment_refcount (eofval);
	      return eofval;
	    }

	  outcome->type = GOT_EOF;
	}
      else
	outcome->type = GOT_EOF_IN_MIDDLE_OF_OBJECT;

      if (s->type == FILE_STREAM)
	clearerr (s->file);

      return NULL;
    }

  if (s->type == STRING_STREAM)
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
  struct object *str, *ret = NULL, *newstr, *eofval = NULL;
  enum outcome_type out;
  const char *objbeg, *objend;

  if (l > 3)
    {
      return raise_al_wrong_number_of_arguments (0, 3, env, outcome);
    }

  if (l && CAR (list)->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (list), "CL:STREAM", env, outcome);
    }

  if (l)
    str = CAR (list);
  else
    str = inspect_variable (env->std_in_sym, env);

  s = str->value_ptr.stream;

  if (s->type == SYNONYM_STREAM
      && !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  s = str->value_ptr.stream;

  if (l >= 2 && SYMBOL (CAR (CDR (list))) == &nil_object)
    eofval = &nil_object;

  if (l >= 3 && eofval)
    eofval = CAR (CDR (CDR (list)));

  if (s->type == STRING_STREAM)
    objend = s->string->value_ptr.string->value;

  do
    {
      out = read_object (&ret, 0, s->type == STRING_STREAM ? objend : NULL,
			 s->type == STRING_STREAM ?
			 s->string->value_ptr.string->used_size
			 -(objend-s->string->value_ptr.string->value) : 0,
			 s->type == FILE_STREAM ? s->file : NULL,
			 1, 1, env, outcome, &objbeg, &objend);
      clear_read_labels (&env->read_labels);

      if (out == SKIPPED_OBJECT)
	objend++;
    } while (out == SKIPPED_OBJECT);

  if (IS_READ_OR_EVAL_ERROR (out))
    {
      CLEAR_READER_STATUS (*outcome);
      outcome->type = out;
      return NULL;
    }

  if (IS_INCOMPLETE_OBJECT (out) || out == NO_OBJECT)
    {
      CLEAR_READER_STATUS (*outcome);

      if (s->type == FILE_STREAM && ferror (s->file))
	outcome->type = ERROR_READING_FILE;
      else if (out == NO_OBJECT)
	{
	  if (eofval)
	    {
	      increment_refcount (eofval);
	      return eofval;
	    }

	  outcome->type = GOT_EOF;
	}
      else
	outcome->type = GOT_EOF_IN_MIDDLE_OF_OBJECT;

      if (s->type == FILE_STREAM)
	clearerr (s->file);

      return NULL;
    }

  if (s->type == STRING_STREAM)
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
builtin_read_from_string (struct object *list, struct environment *env,
			  struct outcome *outcome)
{
  struct object *ret = NULL;
  enum outcome_type out;
  const char *objbeg, *objend;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      return raise_type_error (CAR (list), "CL:STRING", env, outcome);
    }

  objend = CAR (list)->value_ptr.string->value;

  do
    {
      out = read_object (&ret, 0, objend, CAR (list)->value_ptr.string->used_size
			 -(objend-CAR (list)->value_ptr.string->value), NULL, 0,
			 1, env, outcome, &objbeg, &objend);
      clear_read_labels (&env->read_labels);

      if (out == SKIPPED_OBJECT)
	objend++;
    } while (out == SKIPPED_OBJECT);

  if (IS_READ_OR_EVAL_ERROR (out))
    {
      CLEAR_READER_STATUS (*outcome);
      outcome->type = out;
      return NULL;
    }

  if (IS_INCOMPLETE_OBJECT (out))
    {
      CLEAR_READER_STATUS (*outcome);
      outcome->type = GOT_EOF_IN_MIDDLE_OF_OBJECT;
      return NULL;
    }

  if (out == NO_OBJECT)
    {
      CLEAR_READER_STATUS (*outcome);
      outcome->type = GOT_EOF;
      return NULL;
    }

  prepend_object_to_obj_list
    (create_integer_from_long (objend - CAR (list)->value_ptr.string->value + 1),
     &outcome->other_values);

  return ret;
}


struct object *
builtin_parse_integer (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  unsigned char ch;
  const char *in;
  size_t sz, epos;
  enum object_type t;
  const char *nend, *tokend;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      return raise_type_error (CAR (list), "CL:STRING", env, outcome);
    }

  in = CAR (list)->value_ptr.string->value;
  sz = CAR (list)->value_ptr.string->used_size;

  if (!next_nonspace_char (&ch, &in, &sz, NULL)
      || !is_number (in-1, sz+1, 10, &t, &nend, &epos, &tokend)
      || t != TYPE_INTEGER)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  prepend_object_to_obj_list
    (create_integer_from_long (CAR (list)->value_ptr.string->used_size),
     &outcome->other_values);

  return create_number (in-1, nend-in+2, epos, 10, TYPE_INTEGER);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  env->lex_env_vars_boundary = env->lex_env_funcs_boundary = 0;

  ret = evaluate_object (CAR (list), env, outcome);

  env->lex_env_vars_boundary = lex_vars;
  env->lex_env_funcs_boundary = lex_funcs;

  return ret;
}


struct object *
builtin_compile (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  int l = list_length (list);
  struct object *fun = NULL;

  if (!l || l > 2)
    {
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  if (SYMBOL (CAR (list)) == &nil_object && l == 1)
    {
      outcome->type = WRONG_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  if (l == 1)
    {
      if (!(fun = get_function (SYMBOL (CAR (list)), env, 0, 0, 0, 0)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}
    }
  else
    {
      fun = CAR (CDR (list));

      if (fun->type != TYPE_FUNCTION)
	{
	  return raise_type_error (CAR (CDR (list)), "CL:FUNCTION", env, outcome);
	}
    }

  fun = compile_function (fun, env, outcome);

  if (!fun)
    return NULL;

  if (l == 2 && SYMBOL (CAR (list)) != &nil_object)
    {
      delete_reference (SYMBOL (CAR (list)),
			SYMBOL (CAR (list))->value_ptr.symbol->function_cell, 1);
      SYMBOL (CAR (list))->value_ptr.symbol->function_cell = fun;
      add_reference (SYMBOL (CAR (list)), fun, 1);
    }

  prepend_object_to_obj_list (&nil_object, &outcome->other_values);
  prepend_object_to_obj_list (&nil_object, &outcome->other_values);

  if (SYMBOL (CAR (list)) == &nil_object)
    {
      increment_refcount (fun);
      return fun;
    }

  increment_refcount (SYMBOL (CAR (list)));
  return SYMBOL (CAR (list));
}


struct object *
builtin_write (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  struct object *obj, *str = NULL, *allow_other_keys = NULL,
    *ppretty = inspect_variable (env->print_pretty_sym, env);
  int found_unknown_key = 0, ret;

  if (list_length (list) < 1)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
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
	      return raise_type_error (CAR (CDR (list)), "CL:STREAM", env,
				       outcome);
	    }

	  list = CDR (list);
	}
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }

  if (!str)
    str = inspect_variable (env->std_out_sym, env);

  if (str->value_ptr.stream->type == SYNONYM_STREAM
      && !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  if (SYMBOL (ppretty) == &nil_object)
    {
      ret = print_object (obj, env, str->value_ptr.stream);

      if (ret < 0)
	outcome->type = ERROR_DURING_OUTPUT;
    }
  else
    ret = print_object_nicely (obj, env, outcome, str);

  if (ret < 0)
    return NULL;

  increment_refcount (obj);
  return obj;
}


struct object *
builtin_write_string (struct object *list, struct environment *env,
		      struct outcome *outcome)
{
  struct string *s;
  struct object *str;
  int l;

  if (!(l = list_length (list)) || l > 2)
    {
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      return raise_type_error (CAR (list), "CL:STRING", env, outcome);
    }

  if (l == 2 && CAR (CDR (list))->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (CDR (list)), "CL:STREAM", env, outcome);
    }

  s = CAR (list)->value_ptr.string;

  str = l == 2 ? CAR (CDR (list))
    : inspect_variable (env->std_out_sym, env);

  if (str->value_ptr.stream->type == SYNONYM_STREAM
      && !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  write_to_stream (str->value_ptr.stream, s->value, s->used_size);

  if (s->value [s->used_size - 1] == '\n')
    str->value_ptr.stream->dirty_line = 0;
  else
    str->value_ptr.stream->dirty_line = 1;

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_write_char (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct object *str;
  int l;

  if (!(l = list_length (list)) || l > 2)
    {
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      return raise_type_error (CAR (list), "CL:CHARACTER", env, outcome);
    }

  if (l == 2 && CAR (CDR (list))->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (CDR (list)), "CL:STREAM", env, outcome);
    }

  if (l == 2)
    str = CAR (CDR (list));
  else
    str = inspect_variable (env->std_out_sym, env);

  if (str->value_ptr.stream->type == SYNONYM_STREAM
      && !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  write_to_stream (str->value_ptr.stream, CAR (list)->value_ptr.character,
		   strlen (CAR (list)->value_ptr.character));

  if (!strcmp (CAR (list)->value_ptr.character, "\n"))
    str->value_ptr.stream->dirty_line = 0;
  else
    str->value_ptr.stream->dirty_line = 1;

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_write_byte (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  char b;
  struct object *str;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (CDR (list)), "CL:STREAM", env, outcome);
    }

  b = mpz_get_ui (CAR (list)->value_ptr.integer);

  str = CAR (CDR (list));

  if (str->value_ptr.stream->type == SYNONYM_STREAM
      && !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  write_to_stream (str->value_ptr.stream, &b, 1);

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
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  std_out = inspect_variable (env->std_out_sym, env);

  if (std_out->value_ptr.stream->type == SYNONYM_STREAM
      && !(std_out = resolve_synonym_stream (std_out, env, outcome)))
    {
      return NULL;
    }

  return fresh_line (std_out->value_ptr.stream);
}


struct object *
builtin_load (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  int l = list_length (list), found_unknown_key = 0;
  char *fn;
  struct object *ret, *pack = inspect_variable (env->package_sym, env),
    *verbose = NULL, *print = NULL, *allow_other_keys = NULL, *ns, *loadpn;

  if (!l)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = CAR (list);
  fn = copy_string_to_c_string
    (inspect_pathname_by_designator (ns)->value_ptr.string);

  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":VERBOSE", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!verbose)
	    verbose = CAR (CDR (list));

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":PRINT", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!print)
	    print = CAR (CDR (list));

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":EXTERNAL-FORMAT", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }

  loadpn = create_filename (inspect_pathname_by_designator (ns));
  env->vars = bind_variable (env->load_pathname_sym, loadpn, 1, env->vars);
  increment_refcount (loadpn);
  env->vars = bind_variable (env->load_truename_sym, loadpn, 1, env->vars);
  env->lex_env_vars_boundary += 2;

  if (!verbose)
    verbose = inspect_variable (BUILTIN_SYMBOL ("*LOAD-VERBOSE*"), env);

  if (!print)
    print = inspect_variable (BUILTIN_SYMBOL ("*LOAD-PRINT*"), env);

  if (SYMBOL (verbose) != &nil_object)
    printf (";;; Loading file %s...\n", fn);

  ret = load_file (fn, SYMBOL (print) != &nil_object, env, outcome);

  if (SYMBOL (verbose) != &nil_object && ret)
    {
      printf (";;; Loading file %s returned ", fn);
      print_object (ret, env, env->c_stdout->value_ptr.stream);
      printf ("\n");
    }

  set_value (env->package_sym, pack, 0, 0, env, outcome);

  env->vars = remove_bindings (env->vars, 2, 1);

  free (fn);

  return ret;
}


struct object *
builtin_open (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  enum stream_direction dir = -1;
  struct object *ns, *allow_other_keys = NULL, *ret;
  int found_unknown_key = 0, ifexists = -1, ifdoesntexist = -1;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = inspect_pathname_by_designator (CAR (list));
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
	      if (dir == -1)
		dir = INPUT_STREAM;
	    }
	  else if (symbol_equals (CAR (CDR (list)), ":OUTPUT", env))
	    {
	      if (dir == -1)
		dir = OUTPUT_STREAM;
	    }
	  else if (symbol_equals (CAR (CDR (list)), ":IO", env))
	    {
	      if (dir == -1)
		dir = BIDIRECTIONAL_STREAM;
	    }
	  else if (symbol_equals (CAR (CDR (list)), ":PROBE", env))
	    {
	      if (dir == -1)
		dir = NO_DIRECTION;
	    }
	  else
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":IF-EXISTS", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (symbol_equals (CAR (CDR (list)), ":OVERWRITE", env))
	    {
	      if (ifexists == -1)
		ifexists = 1;
	    }
	  else if (symbol_equals (CAR (CDR (list)), ":ERROR", env))
	    {
	      if (ifexists == -1)
		ifexists = 0;
	    }
	  else if (SYMBOL (CAR (CDR (list))) == &nil_object)
	    {
	      if (ifexists == -1)
		ifexists = 2;
	    }
	  else
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":IF-DOES-NOT-EXIST", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (symbol_equals (CAR (CDR (list)), ":CREATE", env))
	    {
	      if (ifdoesntexist == -1)
		ifdoesntexist = 1;
	    }
	  else
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":ELEMENT-TYPE", env)
	       || symbol_equals (CAR (list), ":EXTERNAL-FORMAT", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }

  if (dir == -1)
    dir = INPUT_STREAM;

  if (ifexists == -1)
    ifexists = 0;

  if (ifdoesntexist == -1)
    ifdoesntexist = 1;

  ret = create_file_stream (BINARY_STREAM, dir, ns, ifexists == 1, ifdoesntexist,
			    outcome);

  if (!ret && outcome->type == FILE_ALREADY_EXISTS && ifexists == 2)
    return &nil_object;

  return ret;
}


struct object *
builtin_close (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  struct object *str;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (list), "CL:STREAM", env, outcome);
    }

  str = CAR (list);

  if (str->value_ptr.stream->type == SYNONYM_STREAM
      && !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  if (!str->value_ptr.stream->is_open)
    return &nil_object;

  if (str->value_ptr.stream->type == FILE_STREAM)
    fclose (str->value_ptr.stream->file);

  str->value_ptr.stream->is_open = 0;

  return &t_object;
}


struct object *
builtin_open_stream_p (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct object *str;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (list), "CL:STREAM", env, outcome);
    }

  str = CAR (list);

  if (str->value_ptr.stream->type == SYNONYM_STREAM
      && !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  if (str->value_ptr.stream->is_open)
    return &t_object;
  else
    return &nil_object;
}


struct object *
builtin_input_stream_p (struct object *list, struct environment *env,
			struct outcome *outcome)
{
  struct object *str;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (list), "CL:STREAM", env, outcome);
    }

  str = CAR (list);

  if (str->value_ptr.stream->type == SYNONYM_STREAM
      && !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  if (str->value_ptr.stream->direction & INPUT_STREAM)
    return &t_object;
  else
    return &nil_object;
}


struct object *
builtin_output_stream_p (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  struct object *str;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (list), "CL:STREAM", env, outcome);
    }

  str = CAR (list);

  if (str->value_ptr.stream->type == SYNONYM_STREAM
      && !(str = resolve_synonym_stream (str, env, outcome)))
    {
      return NULL;
    }

  if (str->value_ptr.stream->direction & OUTPUT_STREAM)
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (list), "CL:STREAM", env, outcome);
    }

  return &nil_object;
}


struct object *
builtin_make_string_input_stream (struct object *list, struct environment *env,
				  struct outcome *outcome)
{
  int l = list_length (list), beg, end;
  struct object *ret;

  if (!l || l > 3)
    {
      return raise_al_wrong_number_of_arguments (1, 3, env, outcome);
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      return raise_type_error (CAR (list), "CL:STRING", env, outcome);
    }

  if (l >= 2 && CAR (CDR (list))->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (CDR (list)), "CL:INTEGER", env, outcome);
    }

  if (l == 3 && CAR (CDR (CDR (list)))->type != TYPE_INTEGER
      && SYMBOL (CAR (CDR (CDR (list)))) != &nil_object)
    {
      return raise_type_error (CAR (CDR (CDR (list))), "(CL:OR CL:INTEGER "
			       "CL:NULL)", env, outcome);
    }

  if (l >= 2)
    beg = mpz_get_si (CAR (CDR (list))->value_ptr.integer);
  else
    beg = 0;

  if (l == 3 && CAR (CDR (CDR (list)))->type == TYPE_INTEGER)
    end = mpz_get_si (CAR (CDR (CDR (list)))->value_ptr.integer);
  else
    end = -1;

  ret = create_string_stream (INPUT_STREAM, CAR (list), beg, end);

  if (!ret)
    outcome->type = WRONG_TYPE_OF_ARGUMENT;

  return ret;
}


struct object *
builtin_make_string_output_stream (struct object *list, struct environment *env,
				   struct outcome *outcome)
{
  int l = list_length (list);
  struct object *str;

  if (l > 1)
    {
      return raise_al_wrong_number_of_arguments (0, 1, env, outcome);
    }

  if (l && CAR (list)->type != TYPE_STRING && SYMBOL (CAR (list)) != &nil_object)
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:NULL)", env,
			       outcome);
    }

  str = (l && CAR (list)->type == TYPE_STRING) ? CAR (list) : NULL;

  return create_string_stream (OUTPUT_STREAM, str, -1, -1);
}


struct object *
builtin_get_output_stream_string (struct object *list, struct environment *env,
				  struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM
      || CAR (list)->value_ptr.stream->type != STRING_STREAM)
    {
      return raise_type_error (CAR (list), "CL:STRING-STREAM", env, outcome);
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
builtin_make_synonym_stream (struct object *list, struct environment *env,
			     struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  return create_synonym_stream (SYMBOL (CAR (list)));
}


struct object *
builtin_synonym_stream_symbol (struct object *list, struct environment *env,
			       struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM
      || CAR (list)->value_ptr.stream->type != SYNONYM_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  increment_refcount (CAR (list)->value_ptr.stream->synonym_of);
  return CAR (list)->value_ptr.stream->synonym_of;
}


struct object *
builtin_make_broadcast_stream (struct object *list, struct environment *env,
			       struct outcome *outcome)
{
  struct object *obj = alloc_object ();
  struct stream *str = malloc_and_check (sizeof (*str));
  struct refcounted_object_list *l;

  str->type = BROADCAST_STREAM;
  str->broadcast_to = NULL;
  str->direction = OUTPUT_STREAM;
  str->is_open = 1;

  obj->type = TYPE_STREAM;
  obj->value_ptr.stream = str;

  while (SYMBOL (list) != &nil_object)
    {
      if (CAR (list)->type != TYPE_STREAM
	  || CAR (list)->value_ptr.stream->direction != OUTPUT_STREAM)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      l = malloc_and_check (sizeof (*l));
      l->obj = CAR (list);
      l->reference_strength_factor = !STRENGTH_FACTOR_OF_OBJECT (CAR (list));
      l->next = str->broadcast_to;
      str->broadcast_to = l;

      INC_WEAK_REFCOUNT (CAR (list));

      list = CDR (list);
    }

  return obj;
}


struct object *
builtin_broadcast_stream_streams (struct object *list, struct environment *env,
				  struct outcome *outcome)
{
  struct refcounted_object_list *s;
  struct object *ret = &nil_object, *cons;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM
      || CAR (list)->value_ptr.stream->type != BROADCAST_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  s = CAR (list)->value_ptr.stream->broadcast_to;

  while (s)
    {
      cons = alloc_empty_cons_pair ();
      cons->value_ptr.cons_pair->car = s->obj;
      add_reference (cons, CAR (cons), 0);
      cons->value_ptr.cons_pair->cdr = ret;
      ret = cons;

      s = s->next;
    }

  return ret;
}


struct object *
builtin_finish_output (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  int l = list_length (list);
  struct object *str;

  if (l > 1)
    {
      return raise_al_wrong_number_of_arguments (0, 1, env, outcome);
    }

  if (l && CAR (list)->type != TYPE_STREAM
      && CAR (list)->value_ptr.stream->direction != OUTPUT_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  str = l ? CAR (list) : inspect_variable (env->std_out_sym, env);

  if (str->value_ptr.stream->type == FILE_STREAM)
    {
      fflush (str->value_ptr.stream->file);
    }

  return &nil_object;
}


struct object *
builtin_upper_case_p (struct object *list, struct environment *env,
		      struct outcome *outcome)
{
  char *ch;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      return raise_type_error (CAR (list), "CL:CHARACTER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      return raise_type_error (CAR (list), "CL:CHARACTER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      return raise_type_error (CAR (list), "CL:CHARACTER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  return eq_objects (CAR (list), CAR (CDR (list)));
}


struct object *
builtin_eql (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  return eql_objects (CAR (list), CAR (CDR (list)));
}


struct object *
builtin_equalp (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  return equalp_objects (CAR (list), CAR (CDR (list)));
}


struct object *
builtin_not (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
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
	      return raise_type_error (nth (i, list), "CL:STRING", env, outcome);
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
	      return raise_type_error (nth (i, list), "CL:LIST", env, outcome);
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
  int l = list_length (list), bin_num = 0, found_tags = 0;
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
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome, 1, 0);

      if (!bin)
	{
	  if (outcome->block_to_leave == env->blocks->frame)
	    {
	      outcome->block_to_leave = NULL;
	      ret = outcome->return_value;

	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	    }
	  else
	    ret = NULL;

	  env->vars = chain_bindings (bins, env->vars, 1, NULL, NULL);
	  goto cleanup_and_leave;
	}

      bins = add_binding (bin, bins);
      bin_num++;

      bind_forms = CDR (bind_forms);
    }

  env->vars = chain_bindings (bins, env->vars, 1, NULL, NULL);

  env->lex_env_vars_boundary += bin_num;

  test_form = CAR (CAR (CDR (list)));

  testres = evaluate_object (test_form, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!testres)
    {
      if (outcome->block_to_leave == env->blocks->frame)
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

  env->go_tag_stack = collect_go_tags (body, env->go_tag_stack, &found_tags);

  while (SYMBOL (testres) == &nil_object)
    {
      decrement_refcount (testres);

      bodyres = evaluate_body (body, bin_num, found_tags, 0, NULL, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!bodyres)
	{
	  if (outcome->block_to_leave == env->blocks->frame)
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
		  if (outcome->block_to_leave == env->blocks->frame)
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
	    set_value (SYMBOL (CAR (CAR (bind_forms))), lastincr->obj, 0, 0, env,
		       outcome);

	  decrement_refcount (lastincr->obj);
	  bind_forms = CDR (bind_forms);
	  lastincr = lastincr->next;
	}

      testres = evaluate_object (test_form, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!testres)
	{
	  if (outcome->block_to_leave == env->blocks->frame)
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

  if (found_tags)
    {
      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);
      found_tags = 0;
    }

  ret = evaluate_body (CDR (CAR (CDR (list))), bin_num, 0, 0, NULL, env, outcome);

  if (!ret && outcome->block_to_leave == env->blocks->frame)
    {
      outcome->block_to_leave = NULL;
      ret = outcome->return_value;

      outcome->no_value = outcome->return_no_value;
      outcome->other_values = outcome->return_other_values;
    }

 cleanup_and_leave:
  env->vars = remove_bindings (env->vars, bin_num, 1);

  env->lex_env_vars_boundary -= bin_num;

  env->blocks->frame = remove_block (env->blocks->frame);

  if (found_tags)
    env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);

  free_object_list_structure (incr);

  return ret;
}


struct object *
builtin_do_star (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  int l = list_length (list), bin_num = 0, found_tags = 0;
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
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome, 1, 1);

      if (!bin)
	{
	  if (outcome->block_to_leave == env->blocks->frame)
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
      if (outcome->block_to_leave == env->blocks->frame)
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

  env->go_tag_stack = collect_go_tags (body, env->go_tag_stack, &found_tags);

  while (SYMBOL (testres) == &nil_object)
    {
      decrement_refcount (testres);

      bodyres = evaluate_body (body, bin_num, found_tags, 0, NULL, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!bodyres)
	{
	  if (outcome->block_to_leave == env->blocks->frame)
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
			       CAR (CDR (CDR (CAR (bind_forms)))), 0, 1, env,
			       outcome);

	      if (!res)
		{
		  if (outcome->block_to_leave == env->blocks->frame)
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
	  if (outcome->block_to_leave == env->blocks->frame)
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

  if (found_tags)
    {
      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);
      found_tags = 0;
    }

  ret = evaluate_body (CDR (CAR (CDR (list))), bin_num, 0, 0, NULL, env, outcome);

  if (!ret && outcome->block_to_leave == env->blocks->frame)
    {
      outcome->block_to_leave = NULL;
      ret = outcome->return_value;

      outcome->no_value = outcome->return_no_value;
      outcome->other_values = outcome->return_other_values;
    }

 cleanup_and_leave:
  env->vars = remove_bindings (env->vars, bin_num, 1);

  env->lex_env_vars_boundary -= bin_num;

  env->blocks->frame = remove_block (env->blocks->frame);

  if (found_tags)
    env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);

  return ret;
}


struct object *
builtin_dotimes (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  struct object *var, *count, *ret;
  int cnt, l, i, found_tags;

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
      if (outcome->block_to_leave == env->blocks->frame)
	{
	  env->blocks->frame = remove_block (env->blocks->frame);
	  outcome->block_to_leave = NULL;
	  outcome->no_value = outcome->return_no_value;
	  outcome->other_values = outcome->return_other_values;
	  return outcome->return_value;
	}
      else
	{
	  env->blocks->frame = remove_block (env->blocks->frame);
	  return NULL;
	}
    }

  if (count->type != TYPE_INTEGER)
    {
      env->blocks->frame = remove_block (env->blocks->frame);
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      decrement_refcount (count);
      return NULL;
    }

  cnt = mpz_get_si (count->value_ptr.integer);
  decrement_refcount (count);
  var = SYMBOL (CAR (CAR (list)));

  env->go_tag_stack = collect_go_tags (CDR (list), env->go_tag_stack,
				       &found_tags);

  for (i = 0; i < cnt; i++)
    {
      env->vars = bind_variable (var, create_integer_from_long (i), 1, env->vars);

      env->lex_env_vars_boundary++;

      ret = evaluate_body (CDR (list), 1, found_tags, 0, NULL, env, outcome);

      env->lex_env_vars_boundary--;

      env->vars = remove_bindings (env->vars, 1, 1);

      if (!ret)
	{
	  if (found_tags)
	    env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);

	  if (outcome->block_to_leave == env->blocks->frame)
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      outcome->block_to_leave = NULL;
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	      return outcome->return_value;
	    }
	  else
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      return NULL;
	    }
	}

      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
      decrement_refcount (ret);
    }

  if (found_tags)
    {
      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);
    }

  if (l == 3)
    {
      env->vars = bind_variable (var, create_integer_from_long (i), 1, env->vars);

      env->lex_env_vars_boundary++;

      ret = evaluate_object (nth (2, CAR (list)), env, outcome);

      env->lex_env_vars_boundary--;

      env->vars = remove_bindings (env->vars, 1, 1);

      if (!ret)
	{
	  if (outcome->block_to_leave == env->blocks->frame)
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      outcome->block_to_leave = NULL;
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	      return outcome->return_value;
	    }
	  else
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      return NULL;
	    }
	}

      env->blocks->frame = remove_block (env->blocks->frame);

      return ret;
    }

  env->blocks->frame = remove_block (env->blocks->frame);

  return &nil_object;
}


struct object *
builtin_dolist (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *var, *lst, *cons, *ret;
  int l, found_tags;

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
      if (outcome->block_to_leave == env->blocks->frame)
	{
	  env->blocks->frame = remove_block (env->blocks->frame);
	  outcome->block_to_leave = NULL;
	  outcome->no_value = outcome->return_no_value;
	  outcome->other_values = outcome->return_other_values;
	  return outcome->return_value;
	}
      else
	{
	  env->blocks->frame = remove_block (env->blocks->frame);
	  return NULL;
	}
    }

  if (lst->type != TYPE_CONS_PAIR && SYMBOL (lst) != &nil_object)
    {
      env->blocks->frame = remove_block (env->blocks->frame);
      outcome->type = INCORRECT_SYNTAX_IN_LOOP_CONSTRUCT;
      decrement_refcount (lst);
      return NULL;
    }

  var = SYMBOL (CAR (CAR (list)));
  cons = lst;

  env->go_tag_stack = collect_go_tags (CDR (list), env->go_tag_stack,
				       &found_tags);

  while (cons->type == TYPE_CONS_PAIR)
    {
      increment_refcount (CAR (cons));
      env->vars = bind_variable (var, CAR (cons), 1, env->vars);

      env->lex_env_vars_boundary++;

      ret = evaluate_body (CDR (list), 1, found_tags, 0, NULL, env, outcome);

      env->lex_env_vars_boundary--;

      env->vars = remove_bindings (env->vars, 1, 1);

      if (!ret)
	{
	  decrement_refcount (lst);

	  if (found_tags)
	    env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);

	  if (outcome->block_to_leave == env->blocks->frame)
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      outcome->block_to_leave = NULL;
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	      return outcome->return_value;
	    }
	  else
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      return NULL;
	    }
	}

      decrement_refcount (ret);

      cons = CDR (cons);
    }

  if (found_tags)
    {
      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);
    }

  if (SYMBOL (cons) != &nil_object)
    {
      env->blocks->frame = remove_block (env->blocks->frame);
      return raise_type_error (cons, "CL:CONS", env, outcome);
    }

  if (l == 3)
    {
      env->vars = bind_variable (var, &nil_object, 1, env->vars);

      env->lex_env_vars_boundary++;

      ret = evaluate_object (nth (2, CAR (list)), env, outcome);

      env->lex_env_vars_boundary--;

      env->vars = remove_bindings (env->vars, 1, 1);

      decrement_refcount (lst);

      if (!ret)
	{
	  if (outcome->block_to_leave == env->blocks->frame)
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      outcome->block_to_leave = NULL;
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	      return outcome->return_value;
	    }
	  else
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      return NULL;
	    }
	}

      env->blocks->frame = remove_block (env->blocks->frame);

      return ret;
    }

  env->blocks->frame = remove_block (env->blocks->frame);
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
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
    }

  if (CAR (list)->type == TYPE_SYMBOL_NAME || CAR (list)->type == TYPE_SYMBOL)
    {
      fun = get_function (SYMBOL (CAR (list)), env, 1, 0, 0, 0);

      if (!fun)
	{
	  return raise_undefined_function (SYMBOL (CAR (list)), env, outcome);
	}
    }
  else if (CAR (list)->type == TYPE_FUNCTION)
    {
      fun = CAR (list);
    }
  else
    {
      return raise_type_error (CAR (list), "(CL:OR CL:SYMBOL CL:FUNCTION)", env,
			       outcome);
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
      return raise_al_wrong_number_of_arguments (3, -1, env, outcome);
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
	  return raise_undefined_function (SYMBOL (CAR (CDR (list))), env, outcome);
	}
    }
  else if (CAR (CDR (list))->type == TYPE_FUNCTION)
    {
      fun = CAR (CDR (list));
    }
  else
    {
      return raise_type_error (CAR (CDR (list)), "(CL:OR CL:SYMBOL CL:FUNCTION)",
			       env, outcome);
    }


  for (i = 2; i < l; i++)
    {
      if (!IS_SEQUENCE (nth (i, list)))
	{
	  return raise_type_error (nth (i, list), "CL:SEQUENCE", env, outcome);
	}

      if (min == -1 || (ACTUAL_SEQUENCE_LENGTH (nth (i, list)) < min))
	min = ACTUAL_SEQUENCE_LENGTH (nth (i, list));

      if (!min && is_subtype_by_char_vector (CAR (list), "LIST", env))
	return &nil_object;
    }

  if (SYMBOL (CAR (list)) == &nil_object)
    ret = &nil_object;
  else if (is_subtype_by_char_vector (CAR (list), "LIST", env))
    ret = alloc_empty_list (min);
  else if (is_subtype_by_char_vector (CAR (list), "STRING", env))
    ret = alloc_string (min);
  else if (is_subtype_by_char_vector (CAR (list), "BIT-VECTOR", env))
    ret = alloc_bitvector (min);
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
	  if (nth (j, list)->type == TYPE_STRING
	      || nth (j, list)->type == TYPE_BITARRAY)
	    {
	      decrement_refcount (CAR (argscons));
	    }

	  argscons = CDR (argscons);
	}


      if (!val)
	{
	  free_list_structure (args);
	  return NULL;
	}

      if ((ret->type == TYPE_STRING && val->type != TYPE_CHARACTER)
	  || (ret->type == TYPE_BITARRAY && !is_bit (val)))
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type == TYPE_SYMBOL_NAME || CAR (list)->type == TYPE_SYMBOL)
    {
      fun = get_function (SYMBOL (CAR (list)), env, 1, 0, 0, 0);

      if (!fun)
	{
	  return raise_undefined_function (SYMBOL (CAR (list)), env, outcome);
	}
    }
  else if (CAR (list)->type == TYPE_FUNCTION)
    {
      fun = CAR (list);
    }
  else
    {
      return raise_type_error (CAR (list), "(CL:OR CL:SYMBOL CL:FUNCTION)", env,
			       outcome);
    }

  if (!IS_SEQUENCE (CAR (CDR (list))))
    {
      return raise_type_error (CAR (CDR (list)), "CL:SEQUENCE", env, outcome);
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
      sz = ACTUAL_STRING_LENGTH (seq);

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

      for (i = 0; i < ACTUAL_VECTOR_LENGTH (seq); i++)
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

	  decrement_refcount (CAR (arg));
	  decrement_refcount (res);
	}

      resize_vector (ret, j);
    }

  free (arg->value_ptr.cons_pair);
  free (arg);
  num_conses--;
  num_objects--;

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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SEQUENCE (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SEQUENCE", env, outcome);
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
      sz = ACTUAL_STRING_LENGTH (seq);

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
      sz = ACTUAL_VECTOR_LENGTH (seq);

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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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
      return raise_type_error (CAR (list), "CL:CONS", env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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
      return raise_type_error (CAR (list), "CL:CONS", env, outcome);
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
      return raise_al_wrong_number_of_arguments (3, 3, env, outcome);
    }

  newval = CAR (list);
  list = CDR (list);

  if (CAR (list)->type != TYPE_INTEGER
      || (i = mpz_get_si (CAR (list)->value_ptr.integer)) < 0)
    {
      return raise_type_error (CAR (list), "(CL:INTEGER 0)", env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_CONS_PAIR)
    {
      return raise_type_error (CAR (CDR (list)), "CL:CONS", env, outcome);
    }

  cons = nthcdr (i, CAR (CDR (list)));

  if (!cons)
    {
      outcome->type = OUT_OF_BOUND_INDEX;
      return NULL;
    }

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
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
    }

  newval = CAR (list);
  list = CDR (list);

  if (!IS_ARRAY (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:ARRAY", env, outcome);
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      if (l != 3)
	{
	  return raise_al_wrong_number_of_arguments (3, 3, env, outcome);
	}

      if (CAR (CDR (list))->type != TYPE_INTEGER)
	{
	  return raise_type_error (CAR (CDR (list)), "CL:INTEGER", env, outcome);
	}

      ind = mpz_get_si (CAR (CDR (list))->value_ptr.integer);

      if (ind < 0)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

      if (newval->type != TYPE_CHARACTER)
	{
	  return raise_type_error (newval, "CL:CHARACTER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (3, 3, env, outcome);
    }

  newval = CAR (list);
  list = CDR (list);

  if (!IS_SEQUENCE (CAR (list)) || SYMBOL (CAR (list)) == &nil_object
      || CAR (CDR (list))->type != TYPE_INTEGER)
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
	  return raise_type_error (newval, "CL:CHARACTER", env, outcome);
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
      cons = nthcdr (ind, CAR (list));

      if (!cons)
	{
	  outcome->type = OUT_OF_BOUND_INDEX;
	  return NULL;
	}

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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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
      return raise_al_wrong_number_of_arguments (3, 3, env, outcome);
    }

  newval = CAR (list);
  list = CDR (list);

  if (CAR (CDR (list))->type != TYPE_HASHTABLE)
    {
      return raise_type_error (CAR (CDR (list)), "CL:HASH-TABLE", env, outcome);
    }

  if (IS_WATCHED (CAR (CDR (list))))
    {
      env->watched_obj = CAR (CDR (list));
      env->obj_field = CAR (list);
      env->new_value = newval;

      if (!enter_debugger (NULL, env, outcome))
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  newval = CAR (list);
  list = CDR (list);

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (3, 3, env, outcome);
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
      if (f->decl->name == req)
	{
	  increment_refcount (newval);

	  if (IS_WATCHED (CAR (list)))
	    {
	      env->watched_obj = CAR (list);
	      env->obj_field = f->decl->name;
	      env->new_value = newval;

	      if (!enter_debugger (NULL, env, outcome))
		return NULL;
	    }

	  if (f->name)
	    {
	      decrement_refcount (f->value);
	      f->value = newval;
	    }
	  else
	    {
	      decrement_refcount (f->decl->value);
	      f->decl->value = newval;
	    }

	  increment_refcount (newval);
	  return newval;
	}

      f = f->next;
    }

  outcome->type = SLOT_NOT_FOUND;
  return NULL;
}


struct object *
builtin_setf_symbol_function (struct object *list, struct environment *env,
			      struct outcome *outcome)
{
  struct object *sym;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_FUNCTION)
    {
      return raise_type_error (CAR (list), "CL:FUNCTION", env, outcome);
    }

  if (!IS_SYMBOL (CAR (CDR (list))))
    {
      return raise_type_error (CAR (CDR (list)), "CL:SYMBOL", env, outcome);
    }

  sym = SYMBOL (CAR (CDR (list)));

  if (sym->value_ptr.symbol->function_cell)
    {
      delete_reference (sym, sym->value_ptr.symbol->function_cell, 1);
    }

  sym->value_ptr.symbol->function_cell = CAR (list);
  add_reference (sym, CAR (list), 1);

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_setf_fdefinition (struct object *list, struct environment *env,
			  struct outcome *outcome)
{
  struct object *sym;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_FUNCTION)
    {
      return raise_type_error (CAR (list), "CL:FUNCTION", env, outcome);
    }

  if (!IS_FUNCTION_NAME (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  sym = IS_SYMBOL (CAR (CDR (list))) ? SYMBOL (CAR (CDR (list)))
    : SYMBOL (CAR (CDR (CAR (CDR (list)))));

  if (IS_SYMBOL (CAR (CDR (list))))
    {
      if (sym->value_ptr.symbol->function_cell)
	{
	  delete_reference (sym, sym->value_ptr.symbol->function_cell, 1);
	}

      sym->value_ptr.symbol->function_cell = CAR (list);
      add_reference (sym, CAR (list), 1);
    }
  else
    {
      if (sym->value_ptr.symbol->setf_func_cell)
	{
	  delete_reference (sym, sym->value_ptr.symbol->setf_func_cell, 2);
	}

      sym->value_ptr.symbol->setf_func_cell = CAR (list);
      add_reference (sym, CAR (list), 2);
    }

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_setf_macro_function (struct object *list, struct environment *env,
			     struct outcome *outcome)
{
  struct object *sym, *newval;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  newval = CAR (list);
  list = CDR (list);

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  if (newval->type != TYPE_FUNCTION)
    {
      return raise_type_error (newval, "CL:FUNCTION", env, outcome);
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


struct object *
builtin_method_print_object (struct object *list, struct environment *env,
			     struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (CDR (list))->type != TYPE_STREAM)
    {
      return raise_type_error (CAR (CDR (list)), "CL:STREAM", env, outcome);
    }

  if (print_object (CAR (list), env, CAR (CDR (list))->value_ptr.stream) < 0)
    {
      outcome->type = ERROR_DURING_OUTPUT;
      return NULL;
    }

  increment_refcount (CAR (list));
  return CAR (list);
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
negate_real_number (struct object *num, int in_place)
{
  struct object *ret;

  if (!in_place)
    {
      ret = alloc_number (num->type);
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
      *ret->value_ptr.floating = -*num->value_ptr.floating;
    }

  return ret;
}


struct object *
reciprocate_number (struct object *num)
{
  struct object *ret, *x, *x2, *y, *y2, *x2py2, *r, *i;

  num_numbers++;

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
      decrement_refcount (y);
      decrement_refcount (x2py2);

      i = negate_real_number (i, 1);

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
	  i = negate_real_number (n2->value_ptr.complex->imag, 0);
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

  num_numbers++;

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

  num_numbers++;

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
				 struct environment *env,
				 struct outcome *outcome)
{
  int l = list_length (args);
  enum object_type rem_type, op_type;
  struct object *div_, *div, *num, *ret, *ret2;
  mpz_t tmp;
  mpf_t q, r, half, divf, numf;

  if (!l)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (l > 2)
    {
      return raise_al_wrong_number_of_arguments (0, 2, env, outcome);
    }

  if (!IS_REAL (CAR (args)))
    {
      return raise_type_error (CAR (args), "CL:REAL", env, outcome);
    }

  if (l == 2)
    {
      if (!IS_REAL (CAR (CDR (args))))
	{
	  return raise_type_error (CAR (CDR (args)), "CL:REAL", env, outcome);
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
      return raise_type_error (CAR (list), "CL:NUMBER", env, outcome);
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
	  return raise_type_error (CAR (list), "CL:NUMBER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }
  else if (l == 1)
    {
      ret = create_integer_from_long (0);
    }
  else
    {
      if (!IS_NUMBER (CAR (list)))
	{
	  return raise_type_error (CAR (list), "CL:NUMBER", env, outcome);
	}

      ret = copy_number (CAR (list));
      list = CDR (list);
    }


  while (SYMBOL (list) != &nil_object)
    {
      if (!IS_NUMBER (CAR (list)))
	{
	  return raise_type_error (CAR (list), "CL:NUMBER", env, outcome);
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
      return raise_type_error (CAR (list), "CL:NUMBER", env, outcome);
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
	  return raise_type_error (CAR (list), "CL:NUMBER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }
  else if (l == 1)
    {
      ret = create_integer_from_long (1);
    }
  else
    {
      if (!IS_NUMBER (CAR (list)))
	{
	  return raise_type_error (CAR (list), "CL:NUMBER", env, outcome);
	}

      ret = copy_number (CAR (list));
      list = CDR (list);
    }


  while (SYMBOL (list) != &nil_object)
    {
      if (!IS_NUMBER (CAR (list)))
	{
	  return raise_type_error (CAR (list), "CL:NUMBER", env, outcome);
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
  return perform_division_with_remainder (list, FLOOR, TYPE_INTEGER, env, outcome);
}


struct object *
builtin_ffloor (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  return perform_division_with_remainder (list, FLOOR, TYPE_FLOAT, env, outcome);
}


struct object *
builtin_ceiling (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  return perform_division_with_remainder (list, CEILING, TYPE_INTEGER, env,
					  outcome);
}


struct object *
builtin_fceiling (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  return perform_division_with_remainder (list, CEILING, TYPE_FLOAT, env, outcome);
}


struct object *
builtin_truncate (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  return perform_division_with_remainder (list, TRUNCATE, TYPE_INTEGER, env,
					  outcome);
}


struct object *
builtin_ftruncate (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  return perform_division_with_remainder (list, TRUNCATE, TYPE_FLOAT, env,
					  outcome);
}


struct object *
builtin_round (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  return perform_division_with_remainder (list, ROUND_TO_NEAREST, TYPE_INTEGER,
					  env, outcome);
}


struct object *
builtin_fround (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  return perform_division_with_remainder (list, ROUND_TO_NEAREST, TYPE_FLOAT,
					  env, outcome);
}


struct object *
builtin_numerator (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_RATIONAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:RATIONAL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_RATIONAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:RATIONAL", env, outcome);
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
builtin_rational (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (IS_RATIONAL (CAR (list)))
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }
  else if (CAR (list)->type == TYPE_FLOAT)
    {
      ret = alloc_number (TYPE_RATIO);
      mpq_set_d (ret->value_ptr.ratio, *CAR (list)->value_ptr.floating);
      return ret;
    }
  else
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
    }
}


struct object *
builtin_float (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  int l = list_length (list);
  struct object *ret;

  if (!l || l > 2)
    {
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (!IS_REAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
    }

  if (CAR (list)->type == TYPE_FLOAT)
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }

  ret = alloc_number (TYPE_FLOAT);
  *ret->value_ptr.floating = convert_number_to_double (CAR (list));

  return ret;
}


struct object *
builtin_sqrt (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  struct object *num, *ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_REAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      return raise_type_error (num, "CL:NUMBER", env, outcome);
    }
}


struct object *
builtin_imagpart (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *num;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      return raise_type_error (num, "CL:NUMBER", env, outcome);
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
      return raise_type_error (CAR (list), "CL:NUMBER", env, outcome);
    }

  for (i = 0; i + 1 < l; i++)
    {
      first = CAR (list);

      if (!IS_NUMBER (first))
	{
	  return raise_type_error (first, "CL:NUMBER", env, outcome);
	}

      cons = CDR (list);

      for (j = i + 1; j < l; j++)
	{
	  second = CAR (cons);

	  if (!IS_NUMBER (second))
	    {
	      return raise_type_error (second, "CL:NUMBER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  cur = list;

  if (!IS_REAL (CAR (cur)))
    {
      return raise_type_error (CAR (cur), "CL:REAL", env, outcome);
    }

  for (i = 1; i < l; i++)
    {
      cur = CDR (cur);

      if (!IS_REAL (CAR (cur)))
	{
	  return raise_type_error (CAR (cur), "CL:REAL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  cur = list;

  if (!IS_REAL (CAR (cur)))
    {
      return raise_type_error (CAR (cur), "CL:REAL", env, outcome);
    }

  for (i = 1; i < l; i++)
    {
      cur = CDR (cur);

      if (!IS_REAL (CAR (cur)))
	{
	  return raise_type_error (CAR (cur), "CL:REAL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_REAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
    }

  return create_floating_from_double (sin (convert_number_to_double (CAR (list))));
}


struct object *
builtin_cos (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_REAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
    }

  return create_floating_from_double (cos (convert_number_to_double (CAR (list))));
}


struct object *
builtin_tan (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_REAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
    }

  return create_floating_from_double (tan (convert_number_to_double (CAR (list))));
}


struct object *
builtin_sinh (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_REAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
    }

  return create_floating_from_double (sinh (convert_number_to_double (CAR (list))));
}


struct object *
builtin_cosh (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_REAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
    }

  return create_floating_from_double (cosh (convert_number_to_double (CAR (list))));
}


struct object *
builtin_tanh (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_REAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_REAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (!IS_REAL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:REAL", env, outcome);
    }

  if (!IS_REAL (CAR (CDR (list))))
    {
      return raise_type_error (CAR (CDR (list)), "CL:REAL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
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
      return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
    }

  ret = alloc_number (TYPE_INTEGER);
  mpz_set (ret->value_ptr.integer, CAR (list)->value_ptr.integer);

  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (CAR (list)->type != TYPE_INTEGER)
	{
	  return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (0, 1, env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      ret = CAR (list)->value_ptr.standard_object->class->
	value_ptr.standard_class->name;
    }
  else if (CAR (list)->type == TYPE_CONDITION)
    {
      ret = CAR (list)->value_ptr.condition->class_name;
    }
  else if (CAR (list)->type == TYPE_BACKQUOTE)
    {
      ret =
	intern_symbol_by_char_vector ("AL-BACKQUOTE", strlen ("AL-BACKQUOTE"), 0,
				      EXTERNAL_VISIBILITY, 0,
				      env->cluser_package, 0, 0);
    }
  else if (CAR (list)->type == TYPE_COMMA)
    {
      ret =
	intern_symbol_by_char_vector ("AL-COMMA", strlen ("AL-COMMA"), 0,
				      EXTERNAL_VISIBILITY, 0,
				      env->cluser_package, 0, 0);
    }
  else if (CAR (list)->type == TYPE_AT)
    {
      ret =
	intern_symbol_by_char_vector ("AL-AT", strlen ("AL-AT"), 0,
				      EXTERNAL_VISIBILITY, 0,
				      env->cluser_package, 0, 0);
    }
  else if (CAR (list)->type == TYPE_DOT)
    {
      ret =
	intern_symbol_by_char_vector ("AL-DOT", strlen ("AL-DOT"), 0,
				      EXTERNAL_VISIBILITY, 0,
				      env->cluser_package, 0, 0);
    }

  increment_refcount (ret);
  return ret;
}


struct object *
builtin_subtypep (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  int ret;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (!IS_TYPE_SPECIFIER (CAR (list)) || !IS_TYPE_SPECIFIER (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ret = is_subtype (CAR (list), CAR (CDR (list)), NULL, env, outcome);

  if (ret >= 0)
    {
      prepend_object_to_obj_list (&t_object, &outcome->other_values);
      return ret ? &t_object : &nil_object;
    }
  else
    {
      prepend_object_to_obj_list (&nil_object, &outcome->other_values);
      return &nil_object;
    }
}


struct object *
builtin_coerce (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  int res, i, l;
  struct object *ret, *el;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  res = check_type (CAR (list), CAR (CDR (list)), env, outcome);

  if (res < 0)
    return NULL;

  if (res)
    {
      increment_refcount (CAR (list));
      return CAR (list);
    }

  if (is_subtype_by_char_vector (CAR (CDR (list)), "SEQUENCE", env) > 0)
    {
      if (!IS_SEQUENCE (CAR (list)))
	{
	  return raise_type_error (CAR (list), "CL:SEQUENCE", env, outcome);
	}

      l = ACTUAL_SEQUENCE_LENGTH (CAR (list));

      if (is_subtype_by_char_vector (CAR (CDR (list)), "LIST", env))
	ret = alloc_empty_list (l);
      else if (is_subtype_by_char_vector (CAR (CDR (list)), "STRING", env))
	ret = alloc_string (l);
      else if (is_subtype_by_char_vector (CAR (CDR (list)), "BIT-VECTOR", env))
	ret = alloc_bitvector (l);
      else
	ret = alloc_vector (l, 0, 0);

      for (i = 0; i < ACTUAL_SEQUENCE_LENGTH (CAR (list)); i++)
	{
	  el = elt (CAR (list), i);

	  if ((ret->type == TYPE_STRING && el->type != TYPE_CHARACTER)
	      || (ret->type == TYPE_BITARRAY && !is_bit (el)))
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  set_elt (ret, i, el);

	  if (CAR (list)->type == TYPE_STRING
	      || CAR (list)->type == TYPE_BITARRAY)
	    decrement_refcount (el);
	}

      return ret;
    }
  else if (is_subtype_by_char_vector (CAR (CDR (list)), "CHARACTER", env) > 0)
    {
      if (!IS_CHARACTER_DESIGNATOR (CAR (list)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      return create_character_from_designator (CAR (list));
    }
  else if (is_subtype_by_char_vector (CAR (CDR (list)), "COMPLEX", env) > 0)
    {
      if (!IS_REAL (CAR (list)))
	{
	  return raise_type_error (CAR (list), "CL:REAL", env, outcome);
	}

      return create_complex (CAR (list), NULL, 0, env, outcome);
    }
  else if (is_subtype_by_char_vector (CAR (CDR (list)), "FLOAT", env) > 0)
    {
      if (!IS_REAL (CAR (list)))
	{
	  return raise_type_error (CAR (list), "CL:REAL", env, outcome);
	}

      if (CAR (list)->type == TYPE_FLOAT)
	{
	  increment_refcount (CAR (list));
	  return CAR (list);
	}

	ret = alloc_number (TYPE_FLOAT);
	*ret->value_ptr.floating = convert_number_to_double (CAR (list));

	return ret;
    }
  else if (is_subtype_by_char_vector (CAR (CDR (list)), "FUNCTION", env) > 0)
    {
      if (!IS_SYMBOL (CAR (list)))
	{
	  return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
	}

      return get_function (CAR (list), env, 1, 0, 1, 1);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }
}


struct object *
builtin_make_string (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  int sz, found_unknown_key = 0, i;
  struct object *ret, *initial_element = NULL, *allow_other_keys = NULL;

  if (SYMBOL (list) == &nil_object)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (CAR (list)->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
    }

  sz = mpz_get_si (CAR (list)->value_ptr.integer);

  if (sz < 0)
    {
      outcome->type = INVALID_SIZE;

      return NULL;
    }


  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":ELEMENT-TYPE", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  list = CDR (list);
	}
      else if (symbol_equals (CAR (list), ":INITIAL-ELEMENT", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!initial_element)
	    initial_element = CAR (CDR (list));

	  list = CDR (list);
	}
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }

  if (initial_element && initial_element->type != TYPE_CHARACTER)
    {
      return raise_type_error (initial_element, "CL:CHARACTER", env, outcome);
    }

  ret = alloc_object ();

  ret->type = TYPE_STRING;
  ret->value_ptr.string = malloc_and_check (sizeof (*ret->value_ptr.string));

  if (initial_element)
    {
      sz *= strlen (initial_element->value_ptr.character);
      ret->value_ptr.string->value = malloc_and_check (sz);

      i = 0;

      while (i < sz)
	{
	  memcpy (ret->value_ptr.string->value+i,
		  initial_element->value_ptr.character,
		  strlen (initial_element->value_ptr.character));
	  i += strlen (initial_element->value_ptr.character);
	}
    }
  else
    ret->value_ptr.string->value = calloc_and_check (sz, 1);

  ret->value_ptr.string->alloc_size = ret->value_ptr.string->used_size = sz;
  ret->value_ptr.string->fill_pointer = -1;

  num_strings++;

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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      return raise_type_error (CAR (list), "CL:STRING", env, outcome);
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  return raise_type_error (CAR (CDR (list)), "(CL:OR CL:PACKAGE CL:STRING"
				   " CL:SYMBOL CL:CHARACTER)", env, outcome);
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
					   pack, 0, &ispr);

  if (ent)
    {
      ret = ent->sym;
      ret2 = KEYWORD (ispr ?
		      (ent->flags & INTERNAL_VISIBILITY ?
		       ":INTERNAL" : ":EXTERNAL") : ":INHERITED");

      increment_refcount (ret2);
    }
  else
    {
      ret = intern_symbol_by_char_vector (CAR (list)->value_ptr.string->value,
					  CAR (list)->value_ptr.string->used_size,
					  1, pack == env->keyword_package
					  ? EXTERNAL_VISIBILITY
					  : INTERNAL_VISIBILITY, 1, pack,
					  pack == env->keyword_package, 0);
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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      return raise_type_error (CAR (list), "CL:STRING", env, outcome);
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  return raise_type_error (CAR (CDR (list)), "(CL:OR CL:PACKAGE CL:STRING"
				   " CL:SYMBOL CL:CHARACTER)", env, outcome);
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
					   pack, 0, &ispr);

  if (ent)
    {
      ret = ent->sym;
      increment_refcount (ret);

      ret2 = KEYWORD (ispr ?
		      (ent->flags & INTERNAL_VISIBILITY ? ":INTERNAL"
		       : ":EXTERNAL") : ":INHERITED");
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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  return raise_type_error (CAR (CDR (list)), "(CL:OR CL:PACKAGE CL:STRING"
				   " CL:SYMBOL CL:CHARACTER)", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      return raise_type_error (CAR (list), "CL:STRING", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL)
    {
      return raise_type_error (s, "CL:SYMBOL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL)
    {
      return raise_type_error (s, "CL:SYMBOL", env, outcome);
    }

  ret = get_dynamic_value (SYMBOL (s), env);

  if (!ret)
    {
      return raise_unbound_variable (s, env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  b = find_binding (SYMBOL (CAR (list))->value_ptr.symbol, env->vars,
		    DYNAMIC_BINDING, -1, env->only_lexical);

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

      if (!SYMBOL (CAR (list))->value_ptr.symbol->is_parameter)
	{
	  SYMBOL (CAR (list))->value_ptr.symbol->is_parameter = 1;
	  SYMBOL (CAR (list))->value_ptr.symbol->is_special++;
	}
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL)
    {
      return raise_type_error (s, "CL:SYMBOL", env, outcome);
    }

  ret = get_function (SYMBOL (s), env, 0, 0, 1, 1);

  if (!ret)
    {
      return raise_undefined_function (SYMBOL (s), env, outcome);
    }

  return ret;
}


struct object *
builtin_fdefinition (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  struct object *s, *ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL
      && !(s->type == TYPE_CONS_PAIR && list_length (s) == 2
	   && SYMBOL (CAR (s)) == env->setf_sym && IS_SYMBOL (CAR (CDR (s)))))
    {
      return raise_type_error (s, "CL:SYMBOL", env, outcome);
    }

  ret = get_function (IS_SYMBOL (s) ? SYMBOL (s) : SYMBOL (CAR (CDR (s))), env,
		      0, s->type == TYPE_CONS_PAIR, 1, 1);

  if (!ret)
    {
      return raise_undefined_function (IS_SYMBOL (s) ? SYMBOL (s) : s, env,
				       outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  return SYMBOL (CAR (list))->value_ptr.symbol->home_package;
}


struct object *
builtin_symbol_plist (struct object *list, struct environment *env,
		      struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  delete_reference (SYMBOL (CAR (list)),
		    SYMBOL (CAR (list))->value_ptr.symbol->function_cell, 1);
  SYMBOL (CAR (list))->value_ptr.symbol->function_cell = NULL;

  increment_refcount (SYMBOL (CAR (list)));
  return SYMBOL (CAR (list));
}


struct object *
builtin_gensym (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  char *buf, *pr, *out;
  struct object *num, *ret, *newcnt;
  size_t s;

  if (list_length (list) > 1)
    {
      return raise_al_wrong_number_of_arguments (0, 1, env, outcome);
    }

  if (SYMBOL (list) == &nil_object)
    {
      num = inspect_variable (env->gensym_counter_sym, env);
      pr = "G";
      s = 1;
    }
  else if (CAR (list)->type == TYPE_STRING)
    {
      num = inspect_variable (env->gensym_counter_sym, env);
      pr = CAR (list)->value_ptr.string->value;
      s = CAR (list)->value_ptr.string->used_size;
    }
  else if (CAR (list)->type == TYPE_INTEGER
	   && mpz_cmp_si (CAR (list)->value_ptr.integer, 0) >= 0)
    {
      num = CAR (list);
      pr = "G";
      s = 1;
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  gmp_asprintf (&buf, "%Zd", num->value_ptr.integer);

  if (SYMBOL (list) == &nil_object || CAR (list)->type == TYPE_STRING)
    {
      newcnt = alloc_number (TYPE_INTEGER);
      mpz_set (newcnt->value_ptr.integer, num->value_ptr.integer);
      mpz_add_ui (newcnt->value_ptr.integer, newcnt->value_ptr.integer, 1);

      set_value (env->gensym_counter_sym, newcnt, 0, 0, env, outcome);

      decrement_refcount (newcnt);
    }

  if (!buf)
    {
      fprintf (stderr, "could not allocate memory.  Exiting...\n");
      exit (1);
    }

  out = malloc_and_check (s+strlen (buf));
  strncpy (out, pr, s);
  strncpy (out+s, buf, strlen (buf));

  ret = create_symbol (out, s+strlen (buf), 0);
  free (buf);

  ret->value_ptr.symbol->home_package = &nil_object;

  return ret;
}


struct object *
builtin_macroexpand_1 (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct object *ret, *ret2, *mac, *args;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
  else if (IS_SYMBOL (CAR (list))
	   && SYMBOL (CAR (list))->value_ptr.symbol->is_symbol_macro)
    {
      increment_refcount (SYMBOL (CAR (list))->value_ptr.symbol->value_cell);
      ret = SYMBOL (CAR (list))->value_ptr.symbol->value_cell;

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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
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
builtin_compiler_macro_function (struct object *list, struct environment *env,
				 struct outcome *outcome)
{
  struct object *sym, *ret;
  struct compiler_macro *cm;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list))
      && !(CAR (list)->type == TYPE_CONS_PAIR
	   && list_length (CAR (list)) == 2
	   && SYMBOL (CAR (CAR (list))) == env->setf_sym
	   && IS_SYMBOL (CAR (CDR (CAR (list))))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  sym = IS_SYMBOL (CAR (list)) ? SYMBOL (CAR (list))
    : SYMBOL (CAR (CDR (CAR (list))));


  cm = env->compiler_macros;

  while (cm)
    {
      if (cm->name == sym && cm->is_setf == (CAR (list)->type == TYPE_CONS_PAIR))
	break;

      cm = cm->next;
    }

  if (!cm)
    return &nil_object;

  ret = alloc_function ();

  ret->value_ptr.function->function_macro = cm->macro;

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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      strncpy (ret->value_ptr.string->value, CAR (list)->value_ptr.character, l);
      ret->value_ptr.string->used_size = l;

      return ret;
    }
  else
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:SYMBOL "
			       "CL:CHARACTER)", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  ch = CAR (list);

  if (ch->type != TYPE_CHARACTER)
    {
      return raise_type_error (ch, "CL:CHARACTER", env, outcome);
    }

  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (CAR (list)->type != TYPE_CHARACTER)
	{
	  return raise_type_error (CAR (list), "CL:CHARACTER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      return raise_type_error (CAR (list), "CL:CHARACTER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      return raise_type_error (CAR (list), "CL:CHARACTER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      return raise_type_error (CAR (list), "CL:CHARACTER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      return raise_type_error (CAR (list), "CL:CHARACTER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_CHARACTER)
    {
      return raise_type_error (CAR (list), "CL:CHARACTER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      return raise_type_error (CAR (list), "(CL:OR CL:PACKAGE CL:STRING CL:SYMBOL"
			       " CL:CHARACTER)", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:PACKAGE CL:STRING CL:SYMBOL"
			       " CL:CHARACTER)", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:PACKAGE CL:STRING"
			       " CL:SYMBOL CL:CHARACTER)", env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 3, env, outcome);
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:PACKAGE CL:STRING"
			       " CL:SYMBOL CL:CHARACTER)", env, outcome);
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
    {
      return raise_type_error (CAR (CDR (list)), "(CL:OR CL:PACKAGE CL:STRING"
			       " CL:SYMBOL CL:CHARACTER)", env, outcome);
    }

  if (l == 3)
    {
      if (!IS_LIST (CAR (CDR (CDR (list)))))
	{
	  return raise_type_error (CAR (CDR (CDR (list))), "CL:LIST", env,
				   outcome);
	}

      cons = CAR (CDR (CDR (list)));

      while (SYMBOL (cons) != &nil_object)
	{
	  if (!IS_STRING_DESIGNATOR (CAR (cons)))
	    {
	      return raise_type_error (CAR (cons), "(CL:OR CL:STRING"
				       " CL:SYMBOL CL:CHARACTER)", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:PACKAGE CL:STRING"
			       " CL:SYMBOL CL:CHARACTER)", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:PACKAGE CL:STRING"
			       " CL:SYMBOL CL:CHARACTER)", env, outcome);
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
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
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
  struct object *ret, *nicks = NULL, *args, *allow_other_keys = NULL;
  int found_unknown_key = 0;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STRING && CAR (list)->type != TYPE_CHARACTER
      && !IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:SYMBOL "
			       "CL:CHARACTER)", env, outcome);
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
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STRING && CAR (list)->type != TYPE_CHARACTER
      && !IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:SYMBOL "
			       "CL:CHARACTER)", env, outcome);
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

  return set_value (env->package_sym, pack, 0, 1, env, outcome);
}


struct object *
builtin_import (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  int l = list_length (list), ret;
  struct object *pack, *cons;

  if (!l || l > 2)
    {
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  return raise_type_error (CAR (CDR (list)), "(CL:OR CL:PACKAGE CL:STRING"
				   " CL:SYMBOL CL:CHARACTER)", env, outcome);
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
	      return raise_type_error (CAR (cons), "CL:SYMBOL", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  return raise_type_error (CAR (CDR (list)), "(CL:OR CL:PACKAGE CL:STRING"
				   " CL:SYMBOL CL:CHARACTER)", env, outcome);
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

      if (!pres || rec->flags & INTERNAL_VISIBILITY)
	{
	  used = pack->value_ptr.package->used_by;

	  while (used)
	    {
	      if (inspect_accessible_symbol_by_name (sym->value_ptr.symbol->name,
						     sym->value_ptr.symbol->name_len,
						     used->obj, 0, &pres2))
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

	  rec->flags = EXTERNAL_VISIBILITY | (rec->flags & IS_SHADOWING);
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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  return raise_type_error (CAR (CDR (list)), "(CL:OR CL:PACKAGE CL:STRING"
				   " CL:SYMBOL CL:CHARACTER)", env, outcome);
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

      if (rec->flags & INTERNAL_VISIBILITY)
	return &t_object;

      if (!pres)
	{
	  outcome->type = SYMBOL_NOT_PRESENT_IN_PACKAGE;
	  return NULL;
	}

      rec->flags = INTERNAL_VISIBILITY | (rec->flags & IS_SHADOWING);

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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  return raise_type_error (CAR (CDR (list)), "(CL:OR CL:PACKAGE CL:STRING"
				   " CL:SYMBOL CL:CHARACTER)", env, outcome);
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
	  return raise_type_error (des, "(CL:OR CL:PACKAGE CL:STRING CL:SYMBOL "
				   "CL:CHARACTER)", env, outcome);
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
	  increment_refcount (conf);
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
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  return raise_type_error (CAR (CDR (list)), "(CL:OR CL:PACKAGE "
				   "CL:STRING CL:SYMBOL CL:CHARACTER)", env,
				   outcome);
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
	  return raise_type_error (des, "(CL:OR CL:PACKAGE CL:STRING CL:SYMBOL "
				   "CL:CHARACTER)", env, outcome);
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
builtin_shadow (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  int l = list_length (list), ind;
  struct object *pack, *cons = NULL, *des, *sym;
  struct package_record *r;
  char *s;
  int sl, p;

  if (!l || l > 2)
    {
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (l == 2)
    {
      if (!IS_PACKAGE_DESIGNATOR (CAR (CDR (list))))
	{
	  return raise_type_error (CAR (CDR (list)), "(CL:OR CL:PACKAGE "
				   "CL:STRING CL:SYMBOL CL:CHARACTER)", env,
				   outcome);
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
      outcome->type = CANT_SHADOW_IN_KEYWORD_PACKAGE;
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
      if (des->type == TYPE_STRING)
	{
	  s = des->value_ptr.string->value;
	  sl = des->value_ptr.string->used_size;
	}
      else if (des->type == TYPE_CHARACTER)
	{
	  s = des->value_ptr.character;
	  sl = strlen (s);
	}
      else if (IS_SYMBOL (des))
	{
	  s = SYMBOL (des)->value_ptr.symbol->name;
	  sl = SYMBOL (des)->value_ptr.symbol->name_len;
	}
      else
	{
	  return raise_type_error (des, "(CL:OR CL:STRING CL:SYMBOL "
				   "CL:CHARACTER)", env, outcome);
	}

      r = inspect_accessible_symbol_by_name (s, sl, pack, 1, &p);

      if (!r)
	{
	  sym = create_symbol (s, sl, 1);
	  sym->value_ptr.symbol->home_package = pack;

	  r = malloc_and_check (sizeof (*r));
	  r->flags = INTERNAL_VISIBILITY;
	  r->sym = sym;

	  ind = hash_char_vector (s, sl, SYMTABLE_SIZE);
	  r->next = pack->value_ptr.package->symtable [ind];
	  pack->value_ptr.package->symtable [ind] = r;
	}

      r->flags |= IS_SHADOWING;

      if (cons)
	{
	  cons = CDR (cons);
	  des = CAR (cons);
	}
    } while (cons && SYMBOL (cons) != &nil_object);

  return &t_object;
}


struct object *
builtin_package_shadowing_symbols (struct object *list, struct environment *env,
				   struct outcome *outcome)
{
  struct object *pack, *ret = &nil_object, *cons;
  struct package_record *r;
  int i;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PACKAGE_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:PACKAGE CL:STRING "
			       "CL:SYMBOL CL:CHARACTER)", env, outcome);
    }

  pack = inspect_package_by_designator (CAR (list), env);

  if (!pack)
    {
      outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
      return NULL;
    }

  for (i = 0; i < SYMTABLE_SIZE; i++)
    {
      r = pack->value_ptr.package->symtable [i];

      while (r)
	{
	  if (r->flags & IS_SHADOWING)
	    {
	      cons = alloc_empty_cons_pair ();

	      cons->value_ptr.cons_pair->car = r->sym;
	      add_reference (cons, r->sym, 0);

	      cons->value_ptr.cons_pair->cdr = ret;
	      ret = cons;
	    }

	  r = r->next;
	}
    }

  return ret;
}


struct object *
builtin_do_symbols (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct object *des, *pack, *var, *ret, *p;
  struct object_list *u = NULL;
  struct package_record *rec;
  int l, i, found_tags;

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
	  env->blocks->frame = remove_block (env->blocks->frame);
	  return raise_type_error (des, "(CL:OR CL:PACKAGE CL:STRING CL:SYMBOL "
				   "CL:CHARACTER)", env, outcome);
	}

      pack = inspect_package_by_designator (des, env);

      if (!pack)
	{
	  env->blocks->frame = remove_block (env->blocks->frame);
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}

      decrement_refcount (des);
    }
  else
    pack = inspect_variable (env->package_sym, env);


  var = SYMBOL (CAR (CAR (list)));
  p = pack;

  env->go_tag_stack = collect_go_tags (CDR (list), env->go_tag_stack,
				       &found_tags);

  while (1)
    {
      for (i = 0; i < SYMTABLE_SIZE; i++)
	{
	  rec = p->value_ptr.package->symtable [i];

	  while (rec)
	    {
	      if (u && !(rec->flags & EXTERNAL_VISIBILITY))
		{
		  rec = rec->next;
		  continue;
		}

	      increment_refcount (rec->sym);
	      env->vars = bind_variable (var, rec->sym, 1, env->vars);

	      env->lex_env_vars_boundary++;

	      ret = evaluate_body (CDR (list), 1, found_tags, 0, NULL, env,
				   outcome);

	      env->lex_env_vars_boundary--;

	      env->vars = remove_bindings (env->vars, 1, 1);

	      if (!ret)
		{
		  if (found_tags)
		    env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);

		  if (outcome->block_to_leave == env->blocks->frame)
		    {
		      env->blocks->frame = remove_block (env->blocks->frame);
		      outcome->block_to_leave = NULL;
		      outcome->no_value = outcome->return_no_value;
		      outcome->other_values = outcome->return_other_values;
		      return outcome->return_value;
		    }
		  else
		    {
		      env->blocks->frame = remove_block (env->blocks->frame);
		      return NULL;
		    }
		}

	      decrement_refcount (ret);

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

  if (found_tags)
    {
      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);
    }

  if (l == 3)
    {
      env->vars = bind_variable (var, &nil_object, 1, env->vars);

      env->lex_env_vars_boundary++;

      ret = evaluate_object (nth (2, CAR (list)), env, outcome);

      env->lex_env_vars_boundary--;

      env->vars = remove_bindings (env->vars, 1, 1);

      if (!ret)
	{
	  if (outcome->block_to_leave == env->blocks->frame)
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      outcome->block_to_leave = NULL;
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	      return outcome->return_value;
	    }
	  else
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      return NULL;
	    }
	}

      env->blocks->frame = remove_block (env->blocks->frame);

      return ret;
    }

  env->blocks->frame = remove_block (env->blocks->frame);
  return &nil_object;
}


struct object *
builtin_do_external_symbols (struct object *list, struct environment *env,
			     struct outcome *outcome)
{
  struct object *des, *pack, *var, *ret;
  struct package_record *rec;
  int l, i, found_tags;

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
	  return raise_type_error (des, "(CL:OR CL:PACKAGE CL:STRING "
				   "CL:SYMBOL CL:CHARACTER)", env, outcome);
	}

      pack = inspect_package_by_designator (des, env);

      if (!pack)
	{
	  env->blocks->frame = remove_block (env->blocks->frame);
	  outcome->type = PACKAGE_NOT_FOUND_IN_EVAL;
	  return NULL;
	}

      decrement_refcount (des);
    }
  else
    pack = inspect_variable (env->package_sym, env);


  var = SYMBOL (CAR (CAR (list)));

  env->go_tag_stack = collect_go_tags (CDR (list), env->go_tag_stack,
				       &found_tags);

  for (i = 0; i < SYMTABLE_SIZE; i++)
    {
      rec = pack->value_ptr.package->symtable [i];

      while (rec)
	{
	  if (!(rec->flags & EXTERNAL_VISIBILITY))
	    {
	      rec = rec->next;
	      continue;
	    }

	  increment_refcount (rec->sym);
	  env->vars = bind_variable (var, rec->sym, 1, env->vars);

	  env->lex_env_vars_boundary++;

	  ret = evaluate_body (CDR (list), 1, found_tags, 0, NULL, env, outcome);

	  env->lex_env_vars_boundary--;

	  env->vars = remove_bindings (env->vars, 1, 1);

	  if (!ret)
	    {
	      if (found_tags)
		env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);

	      if (outcome->block_to_leave == env->blocks->frame)
		{
		  env->blocks->frame = remove_block (env->blocks->frame);
		  outcome->block_to_leave = NULL;
		  outcome->no_value = outcome->return_no_value;
		  outcome->other_values = outcome->return_other_values;
		  return outcome->return_value;
		}
	      else
		{
		  env->blocks->frame = remove_block (env->blocks->frame);
		  return NULL;
		}
	    }

	  decrement_refcount (ret);

	  rec = rec->next;
	}
    }

  if (found_tags)
    {
      env->go_tag_stack = remove_go_tag_frame (env->go_tag_stack);
    }

  if (l == 3)
    {
      env->vars = bind_variable (var, &nil_object, 1, env->vars);

      env->lex_env_vars_boundary++;

      ret = evaluate_object (nth (2, CAR (list)), env, outcome);

      env->lex_env_vars_boundary--;

      env->vars = remove_bindings (env->vars, 1, 1);

      if (!ret)
	{
	  if (outcome->block_to_leave == env->blocks->frame)
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      outcome->block_to_leave = NULL;
	      outcome->no_value = outcome->return_no_value;
	      outcome->other_values = outcome->return_other_values;
	      return outcome->return_value;
	    }
	  else
	    {
	      env->blocks->frame = remove_block (env->blocks->frame);
	      return NULL;
	    }
	}

      env->blocks->frame = remove_block (env->blocks->frame);

      return ret;
    }

  env->blocks->frame = remove_block (env->blocks->frame);
  return &nil_object;
}


struct object *
builtin_time (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  struct object *ret;
  clock_t t;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  t = clock ();

  ret = evaluate_object (CAR (list), env, outcome);

  if (!ret)
    return NULL;

  t = clock ()-t;

  printf ("process time: %lu * (1/%lu s)\n", t, CLOCKS_PER_SEC);

  return ret;
}


struct object *
builtin_get_internal_run_time (struct object *list, struct environment *env,
			       struct outcome *outcome)
{
  if (list_length (list))
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
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
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
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
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
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
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  return create_string_copying_c_string (PACKAGE_VERSION);
}


struct object *
builtin_software_type (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  if (SYMBOL (list) != &nil_object)
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  return &nil_object;
}


struct object *
builtin_software_version (struct object *list, struct environment *env,
			  struct outcome *outcome)
{
  if (SYMBOL (list) != &nil_object)
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  return &nil_object;
}


struct binding *
create_binding_from_let_form (struct object *form, struct environment *env,
			      struct outcome *outcome, int allow_three_elements,
			      int increment_dyn_bin_count)
{
  struct object *sym, *val;
  struct binding *b;
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
      if (increment_dyn_bin_count)
	sym->value_ptr.symbol->value_dyn_bins_num++;

      return create_binding (sym, val, DYNAMIC_BINDING, 0);
    }
  else
    {
      b = create_binding (sym, val, LEXICAL_BINDING, 0);
      b->prev_special = sym->value_ptr.symbol->is_special;
      sym->value_ptr.symbol->is_special = 0;

      return b;
    }
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
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome, 0, 0);

      if (!bin)
	{
	  remove_bindings (bins, bin_num, 0);
	  return NULL;
	}

      bins = add_binding (bin, bins);
      bin_num++;

      bind_forms = CDR (bind_forms);
    }

  env->vars = chain_bindings (bins, env->vars, 1, NULL, NULL);

  env->lex_env_vars_boundary += bin_num;

  if (!parse_declarations (CDR (list), env, bin_num, 0, outcome, &body))
    {
      res = NULL;
      goto cleanup_and_leave;
    }

  res = evaluate_body (body, -1, 0, 0, NULL, env, outcome);

  undo_special_declarations (CDR (list), env);

 cleanup_and_leave:
  env->vars = remove_bindings (env->vars, bin_num, 1);

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
      bin = create_binding_from_let_form (CAR (bind_forms), env, outcome, 0, 1);

      if (!bin)
	{
	  env->vars = remove_bindings (env->vars, bin_num, 1);
	  env->lex_env_vars_boundary -= bin_num;
	  return NULL;
	}

      env->vars = add_binding (bin, env->vars);
      env->lex_env_vars_boundary++, bin_num++;

      bind_forms = CDR (bind_forms);
    }

  if (!parse_declarations (CDR (list), env, bin_num, 0, outcome, &body))
    {
      env->vars = remove_bindings (env->vars, bin_num, 1);
      env->lex_env_vars_boundary -= bin_num;
      return NULL;
    }

  res = evaluate_body (body, -1, 0, 0, NULL, env, outcome);

  undo_special_declarations (CDR (list), env);

  env->vars = remove_bindings (env->vars, bin_num, 1);

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
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
    }

  syms = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!syms)
    return NULL;

  vals = evaluate_object (CAR (CDR (list)), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!vals)
    return NULL;

  if (!IS_LIST (syms))
    {
      return raise_type_error (syms, "CL:LIST", env, outcome);
    }

  if (!IS_LIST (vals))
    {
      return raise_type_error (vals, "CL:LIST", env, outcome);
    }

  cons1 = syms, cons2 = vals;

  while (SYMBOL (cons1) != &nil_object && SYMBOL (cons2) != &nil_object)
    {
      if (!IS_SYMBOL (CAR (cons1)))
	{
	  env->vars = chain_bindings (bins, env->vars, 1, NULL, NULL);
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

      binnum++;

      cons1 = CDR (cons1);
      cons2 = CDR (cons2);
    }

  env->vars = chain_bindings (bins, env->vars, 1, NULL, NULL);
  env->lex_env_vars_boundary += binnum;
  bins = NULL;

  ret = evaluate_body (CDR (CDR (list)), -1, 0, 0, NULL, env, outcome);

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


struct object *
evaluate_locally (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *res, *body;

  if (!parse_declarations (list, env, 0, 0, outcome, &body))
    return NULL;

  res = evaluate_body (body, -1, 0, 0, NULL, env, outcome);

  undo_special_declarations (list, env);

  return res;
}


struct binding *
create_binding_from_flet_form (struct object *form, struct environment *env,
			       struct outcome *outcome,
			       enum object_type type)
{
  struct object *sym, *fun;

  if (form->type == TYPE_CONS_PAIR)
    {
      if (list_length (form) < 2
	  || (!IS_SYMBOL (CAR (form))
	      && (type == TYPE_MACRO
		  || !(CAR (form)->type == TYPE_CONS_PAIR
		       && list_length (CAR (form)) == 2
		       && SYMBOL (CAR (CAR (form))) == env->setf_sym
		       && IS_SYMBOL (CAR (CDR (CAR (form))))))))
	{
	  outcome->type = INCORRECT_SYNTAX_IN_LET;
	  return NULL;
	}

      sym = IS_SYMBOL (CAR (form)) ? SYMBOL (CAR (form))
	: SYMBOL (CAR (CDR (CAR (form))));

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
      fun->value_ptr.function->is_setf_func = CAR (form)->type == TYPE_CONS_PAIR;
      fun->value_ptr.function->name = sym;
      add_reference (fun, sym, 0);
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

  env->funcs = chain_bindings (bins, env->funcs, 0, NULL, NULL);

  env->lex_env_funcs_boundary += bin_num;

  res = evaluate_body (body, 0, 0, 0, NULL, env, outcome);

  env->funcs = remove_bindings (env->funcs, bin_num, 0);

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

  res = evaluate_body (body, 0, 0, 0, NULL, env, outcome);

  env->funcs = remove_bindings (env->funcs, bin_num, 0);

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

  env->funcs = chain_bindings (bins, env->funcs, 0, NULL, NULL);

  env->lex_env_funcs_boundary += bin_num;

  res = evaluate_body (body, 0, 0, 0, NULL, env, outcome);

  env->funcs = remove_bindings (env->funcs, bin_num, 0);

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

  if (s->value_dyn_bins_num)
    {
      b = find_binding (s, env->vars, DYNAMIC_BINDING, env->lex_env_vars_boundary,
			env->only_lexical);

      if (b)
	{
	  increment_refcount (b->obj);
	  return b->obj;
	}
    }

  if (s->value_cell)
    increment_refcount (s->value_cell);

  return s->value_cell;
}


struct object *
get_function (struct object *sym, struct environment *env, int only_functions,
	      int setf_func, int only_globals, int increment_refc)
{
  struct object *f;
  struct binding *b = env->funcs;
  int lexb = env->lex_env_funcs_boundary;

  if (!only_globals)
    {
      while (b && lexb)
	{
	  if (SYMBOL (sym) == b->sym
	      && !setf_func == !b->obj->value_ptr.function->is_setf_func)
	    break;

	  b = b->next;
	  lexb--;
	}

      if (!lexb)
	b = NULL;
    }

  if (only_globals || !b)
    {
      f = !setf_func ? SYMBOL (sym)->value_ptr.symbol->function_cell
	: SYMBOL (sym)->value_ptr.symbol->setf_func_cell;
    }
  else
    f = b->obj;

  if (f && f->type != TYPE_FUNCTION && only_functions)
    return NULL;

  if (increment_refc && f)
    increment_refcount (f);

  return f;
}


int
is_macro (struct object *sym, struct environment *env)
{
  struct object *fun = get_function (sym, env, 0, 0, 0, 0);

  return fun && fun->type == TYPE_MACRO && !fun->value_ptr.macro->builtin_form;
}


struct object *
inspect_variable_by_c_string (char *var, struct environment *env)
{
  struct object *sym = intern_symbol_by_char_vector (var, strlen (var), 0,
						     INTERNAL_VISIBILITY, 0,
						     env->cluser_package, 0, 0);

  if (sym)
    return inspect_variable (sym, env);

  return NULL;
}


struct object *
inspect_variable (struct object *sym, struct environment *env)
{
  struct symbol *s = sym->value_ptr.symbol;
  struct binding *b;

  if (s->is_const)
    {
      return s->value_cell;
    }

  if (s->value_dyn_bins_num)
    {
      b = find_binding (s, env->vars, DYNAMIC_BINDING, -1, env->only_lexical);

      if (b)
	{
	  return b->obj;
	}
    }

  return s->value_cell;
}


struct object *
set_value (struct object *sym, struct object *value, int expand_symmacros,
	   int eval_value, struct environment *env, struct outcome *outcome)
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

  if (s->is_special)
    {
      if (!s->value_dyn_bins_num)
	{
	  delete_reference (sym, s->value_cell, 0);
	  add_reference (sym, val, 0);
	  s->value_cell = val;
	}
      else
	{
	  b = find_binding (s, env->vars, DYNAMIC_BINDING, -1, env->only_lexical);
	  decrement_refcount (b->obj);
	  b->obj = val;

	  if (val)
	    increment_refcount (val);
	}
    }
  else
    {
      b = find_binding (s, env->vars, LEXICAL_BINDING,
			env->lex_env_vars_boundary, env->only_lexical);

      if (b)
	{
	  if (expand_symmacros && b->is_symbol_macro)
	    {
	      return setf_value (b->obj, val, 0, env, outcome);
	    }

	  decrement_refcount (b->obj);
	  b->obj = val;
	  increment_refcount (val);
	}
      else
	{
	  if (!sym->value_ptr.symbol->is_parameter)
	    {
	      sym->value_ptr.symbol->is_parameter = 1;
	      sym->value_ptr.symbol->is_special++;
	    }

	  if (expand_symmacros && sym->value_ptr.symbol->is_symbol_macro)
	    {
	      return setf_value (sym->value_ptr.symbol->value_cell, val, 0, env,
				 outcome);
	    }

	  sym->value_ptr.symbol->value_cell = val;
	  add_reference (sym, val, 0);
	}
    }

  return val;
}


int
set_values_destructuring (struct object *template, struct object *vals,
			  struct environment *env, struct outcome *outcome)
{
  if (template->type != TYPE_CONS_PAIR && !IS_SYMBOL (template))
    {
      outcome->type = INVALID_LAMBDA_LIST;
      return 0;
    }

  if (template->type == TYPE_CONS_PAIR && !IS_LIST (vals))
    {
      outcome->type = MISMATCH_IN_DESTRUCTURING_CALL;
      return 0;
    }

  while (template->type == TYPE_CONS_PAIR && vals->type == TYPE_CONS_PAIR)
    {
      if (CAR (template)->type == TYPE_CONS_PAIR && IS_LIST (CAR (vals)))
	{
	  if (!set_values_destructuring (CAR (template), CAR (vals), env,
					 outcome))
	    return 0;
	}
      else if (IS_SYMBOL (CAR (template)))
	{
	  if (SYMBOL (CAR (template))->value_ptr.symbol->is_const
	      && SYMBOL (CAR (template)) != &nil_object)
	    {
	      outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	      return 0;
	    }

	  if (SYMBOL (CAR (template)) != &nil_object)
	    {
	      set_value (SYMBOL (CAR (template)), CAR (vals), 0, 0, env,
			 outcome);
	    }
	}
      else
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return 0;
	}

      template = CDR (template);
      vals = CDR (vals);
    }

  while (template->type == TYPE_CONS_PAIR)
    {
      if (CAR (template)->type == TYPE_CONS_PAIR)
	{
	  if (!set_values_destructuring (CAR (template), &nil_object, env,
					 outcome))
	    return 0;
	}
      else if (IS_SYMBOL (CAR (template)))
	{
	  if (SYMBOL (CAR (template))->value_ptr.symbol->is_const
	      && SYMBOL (CAR (template)) != &nil_object)
	    {
	      outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	      return 0;
	    }

	  if (SYMBOL (CAR (template)) != &nil_object)
	    {
	      set_value (SYMBOL (CAR (template)), &nil_object, 0, 0, env,
			 outcome);
	    }
	}
      else
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return 0;
	}

      template = CDR (template);
    }

  if (SYMBOL (template) != &nil_object)
    {
      if (!IS_SYMBOL (template))
	{
	  outcome->type = INVALID_LAMBDA_LIST;
	  return 0;
	}

      if (SYMBOL (template)->value_ptr.symbol->is_const)
	{
	  outcome->type = CANT_USE_CONSTANT_NAME_IN_LAMBDA_LIST;
	  return 0;
	}

      set_value (SYMBOL (template), vals, 0, 0, env, outcome);
    }

  return 1;
}


struct object *
setf_value (struct object *form, struct object *value, int eval_value,
	    struct environment *env, struct outcome *outcome)
{
  struct object *exp, *cons1, *cons2, *res, *val, *args, *fun;
  struct object_list *expvals, *l;
  int binsnum = 0;


  if (IS_SYMBOL (form))
    {
      return set_value (SYMBOL (form), value, 1, eval_value, env, outcome);
    }
  else if (form->type == TYPE_CONS_PAIR)
    {
      if (!IS_SYMBOL (CAR (form)))
	{
	  outcome->type = INVALID_ACCESSOR;
	  return NULL;
	}

      if (SYMBOL (CAR (form))->value_ptr.symbol->setf_expander)
	{
	  exp = call_function (SYMBOL (CAR (form))->value_ptr.symbol->
			       setf_expander, CDR (form), 0, 0, 0, 0, 0, env,
			       outcome);

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

	      env->vars = bind_variable (SYMBOL (CAR (cons1)), res, 1, env->vars);
	      binsnum++;

	      cons1 = CDR (cons1);
	      cons2 = CDR (cons2);
	    }

	  decrement_refcount (exp);
	  decrement_refcount (expvals->obj);

	  if (eval_value)
	    {
	      value = evaluate_object (value, env, outcome);

	      if (!value)
		return NULL;
	    }

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
		  env->vars = bind_variable (SYMBOL (CAR (cons1)), value, 1,
					     env->vars);
		  l = outcome->other_values;
		}
	      else
		{
		  env->vars = bind_variable (SYMBOL (CAR (cons1)), l->obj, 1,
					     env->vars);
		  l = l->next;
		}

	      binsnum++;

	      cons1 = CDR (cons1);

	      if (!l)
		break;
	    }

	  while (SYMBOL (cons1) != &nil_object)
	    {
	      if (!IS_SYMBOL (CAR (cons1)))
		{
		  outcome->type = INVALID_SETF_EXPANSION;
		  return NULL;
		}

	      env->vars = bind_variable (SYMBOL (CAR (cons1)), &nil_object, 0,
					 env->vars);

	      binsnum++;
	      cons1 = CDR (cons1);
	    }

	  decrement_refcount (expvals->next->obj);

	  free_object_list_structure (outcome->other_values);
	  outcome->other_values = NULL;

	  env->lex_env_vars_boundary += binsnum;

	  val = evaluate_object (expvals->next->next->obj, env, outcome);

	  free_object_list (expvals->next->next);
	  expvals->next->next = NULL;
	  free_object_list_structure (expvals);

	  env->vars = remove_bindings (env->vars, binsnum, 1);
	  env->lex_env_vars_boundary -= binsnum;
	}
      else if ((fun = SYMBOL (CAR (form))->value_ptr.symbol->
		function_cell)
	       && fun->value_ptr.function->struct_accessor_class_name)
	{
	  args = evaluate_through_list (CDR (form), env, outcome);

	  if (!args)
	    return NULL;

	  if (eval_value)
	    {
	      value = evaluate_object (value, env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!value)
		return NULL;
	    }

	  val = call_structure_accessor (fun->value_ptr.function->
					 struct_accessor_class_name,
					 fun->value_ptr.function->
					 struct_accessor_field, args, value,
					 env, outcome);

	  decrement_refcount (args);
	}
      else
	{
	  args = evaluate_through_list (CDR (form), env, outcome);

	  if (!args)
	    return NULL;

	  if (eval_value)
	    {
	      value = evaluate_object (value, env, outcome);
	      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

	      if (!value)
		return NULL;
	    }

	  fun = get_function (SYMBOL (CAR (form)), env, 1, 1, 0, 0);

	  if (!fun)
	    {
	      outcome->type = INVALID_ACCESSOR;
	      return NULL;
	    }

	  cons1 = alloc_empty_cons_pair ();
	  cons1->value_ptr.cons_pair->car = value;
	  add_reference (cons1, value, 0);
	  decrement_refcount (value);
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
      return raise_type_error (form, "(CL:OR CL:SYMBOL CL:CONS)", env, outcome);
    }

  return val;
}


struct object *
evaluate_quote (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
  return evaluate_body (list, -1, 0, 0, NULL, env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_LIST (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:LIST", env, outcome);
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
  add_reference (ret, res, 0);
  decrement_refcount (res);

  vals = outcome->other_values;

  while (vals)
    {
      cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

      cons->value_ptr.cons_pair->car = vals->obj;
      add_reference (cons, vals->obj, 0);
      decrement_refcount (vals->obj);

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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  fun = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!fun)
    return NULL;

  if (IS_SYMBOL (fun))
    {
      fun = get_function (SYMBOL (fun), env, 1, 0, 0, 0);

      if (!fun)
	{
	  return raise_undefined_function (SYMBOL (fun), env, outcome);
	}

      increment_refcount (fun);
    }
  else if (fun->type != TYPE_FUNCTION)
    {
      return raise_type_error (fun, "(CL:OR CL:SYMBOL CL:FUNCTION)", env, outcome);
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
	  add_reference (cons, res, 0);

	  l = outcome->other_values;

	  while (l)
	    {
	      cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();

	      cons->value_ptr.cons_pair->car = l->obj;
	      add_reference (cons, l->obj, 0);

	      l = l->next;
	    }

	  decrement_refcount (res);
	  free_object_list (outcome->other_values);
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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (CAR (list)->type != TYPE_CONS_PAIR)
    {
      return raise_type_error (CAR (list), "CL:CONS", env, outcome);
    }

  cons = CAR (list);

  while (SYMBOL (cons) != &nil_object)
    {
      if (!IS_SYMBOL (CAR (cons)))
	{
	  return raise_type_error (CAR (cons), "CL:SYMBOL", env, outcome);
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
      return evaluate_body (CDR (list), -1, 0, 0, NULL, env, outcome);
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
  int l = list_length (list);

  if (l < 2 || l > 3)
    {
      return raise_al_wrong_number_of_arguments (2, 3, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  if (l == 3 && CAR (CDR (CDR (list)))->type != TYPE_STRING)
    {
      return raise_type_error (CAR (CDR (CDR (list))), "CL:STRING", env, outcome);
    }

  return define_constant (SYMBOL (CAR (list)), CAR (CDR (list)), env, outcome);
}


struct object *
builtin_constantp (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  int l = list_length (list);

  if (!l || l > 2)
    {
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (IS_NUMBER (CAR (list)) || CAR (list)->type == TYPE_CHARACTER
      || IS_ARRAY (CAR (list)) || CAR (list)->type == TYPE_HASHTABLE
      || CAR (list)->type == TYPE_PACKAGE || CAR (list)->type == TYPE_FILENAME
      || CAR (list)->type == TYPE_STREAM
      || CAR (list)->type == TYPE_STRUCTURE_CLASS
      || CAR (list)->type == TYPE_STRUCTURE
      || CAR (list)->type == TYPE_STANDARD_CLASS
      || CAR (list)->type == TYPE_STANDARD_OBJECT
      || CAR (list)->type == TYPE_CONDITION_CLASS
      || CAR (list)->type == TYPE_CONDITION
      || CAR (list)->type == TYPE_FUNCTION || CAR (list)->type == TYPE_MACRO
      || CAR (list)->type == TYPE_METHOD)
    return &t_object;

  if (IS_SYMBOL (CAR (list)) && SYMBOL (CAR (list))->value_ptr.symbol->is_const)
    return &t_object;

  if (IS_CONSTANT (CAR (list)))
    return &t_object;

  if (CAR (list)->type == TYPE_CONS_PAIR
      && SYMBOL (CAR (CAR (list))) == env->quote_sym)
    return &t_object;

  return &nil_object;
}


struct object *
evaluate_defparameter (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct object *s;
  int l = list_length (list);

  if (l < 2 || l > 3)
    {
      return raise_al_wrong_number_of_arguments (2, 3, env, outcome);
    }

  s = CAR (list);

  if (s->type != TYPE_SYMBOL && s->type != TYPE_SYMBOL_NAME)
    {
      return raise_type_error (s, "CL:SYMBOL", env, outcome);
    }

  if (l == 3 && CAR (CDR (CDR (list)))->type != TYPE_STRING)
    {
      return raise_type_error (CAR (CDR (CDR (list))), "CL:STRING", env, outcome);
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

  if (!l || l > 3)
    {
      return raise_al_wrong_number_of_arguments (1, 3, env, outcome);
    }

  if (s->type != TYPE_SYMBOL_NAME && s->type != TYPE_SYMBOL)
    {
      return raise_type_error (s, "CL:SYMBOL", env, outcome);
    }

  if (l == 3 && CAR (CDR (CDR (list)))->type != TYPE_STRING)
    {
      return raise_type_error (CAR (CDR (CDR (list))), "CL:STRING", env, outcome);
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
	  s->value_ptr.symbol->is_parameter = 1;
	  s->value_ptr.symbol->is_special++;
	}
    }
  else
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

  if (SYMBOL (inspect_variable (env->al_compile_when_defining_sym, env))
      != &nil_object)
    {
      fun = compile_function (fun, env, outcome);

      if (!fun)
	return NULL;
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

  if (SYMBOL (inspect_variable (env->al_compile_when_defining_sym, env))
      != &nil_object)
    {
      mac = compile_function (mac, env, outcome);

      if (!mac)
	return NULL;
    }

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
	  return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
	}

      ret = set_value (SYMBOL (CAR (list)), CAR (CDR (list)), 1, 1, env,
		       outcome);

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
	  return raise_type_error (CAR (cons), "CL:SYMBOL", env, outcome);
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
      set_value (SYMBOL (CAR (cons)), last->obj, 1, 0, env, outcome);

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
  struct object *ret = &nil_object;

  if (list_length (list) % 2)
    {
      outcome->type = ODD_NUMBER_OF_ARGUMENTS;
      return NULL;
    }

  while (SYMBOL (list) != &nil_object)
    {
      decrement_refcount (ret);

      ret = setf_value (CAR (list), CAR (CDR (list)), 1, env, outcome);

      if (!ret)
	return NULL;

      list = CDR (CDR (list));
    }

  return ret;
}


struct object *
evaluate_psetf (struct object *list, struct environment *env,
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
      setf_value (CAR (cons), last->obj, 0, env, outcome);

      cons = CDR (CDR (cons));
      last = last->next;
    }

  free_object_list (ls);

  return &nil_object;
}


struct object *
evaluate_function (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  struct object *f;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
	  return raise_undefined_function (IS_SYMBOL (CAR (list))
					   ? SYMBOL (CAR (list)) : CAR (list),
					   env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
    }

  if (CAR (list)->type != TYPE_SYMBOL_NAME && CAR (list)->type != TYPE_SYMBOL
      && CAR (list)->type != TYPE_FUNCTION)
    {
      return raise_type_error (CAR (list), "(CL:OR CL:SYMBOL CL:FUNCTION)", env,
			       outcome);
    }

  if (CAR (list)->type == TYPE_SYMBOL_NAME || CAR (list)->type == TYPE_SYMBOL)
    {
      s = SYMBOL (CAR (list));

      fun = get_function (s, env, 1, 0, 0, 0);

      if (!fun)
	{
	  return raise_undefined_function (s, env, outcome);
	}
    }
  else
    fun = CAR (list);

  list = CDR (list);
  length--;
  last = nth (length - 1, list);

  if (last->type != TYPE_CONS_PAIR && SYMBOL (last) != &nil_object)
    {
      return raise_type_error (last, "CL:LIST", env, outcome);
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
	  return raise_undefined_function (SYMBOL (CAR (list)), env, outcome);
	}
    }
  else
    {
      return raise_type_error (CAR (list), "(CL:OR CL:SYMBOL CL:FUNCTION)", env,
			       outcome);
    }

  return call_function (fun, CDR (list), 0, 0, 1, 0, 0, env, outcome);
}


struct object *
evaluate_declare (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  return raise_program_error (env, outcome);
}


struct object *
evaluate_the (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (!IS_TYPE_SPECIFIER (CAR (list)))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return evaluate_object (CAR (CDR (list)), env, outcome);
}


struct object *
evaluate_and (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  struct object *val = &t_object;

  while (SYMBOL (list) != &nil_object)
    {
      decrement_refcount (val);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      val = evaluate_object (CAR (list), env, outcome);

      if (!val)
	return NULL;

      if (SYMBOL (val) == &nil_object)
	{
	  decrement_refcount (val);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
	  return &nil_object;
	}

      list = CDR (list);
    }

  return val;
}


struct object *
evaluate_or (struct object *list, struct environment *env,
	     struct outcome *outcome)
{
  struct object *val = &nil_object;

  while (SYMBOL (list) != &nil_object)
    {
      val = evaluate_object (CAR (list), env, outcome);

      if (!val)
	return NULL;

      if (SYMBOL (val) != &nil_object)
	{
	  list = CDR (list);
	  break;
	}

      if (SYMBOL (CDR (list)) != &nil_object)
	{
	  decrement_refcount (val);
	  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);
	}

      list = CDR (list);
    }

  if (SYMBOL (list) != &nil_object)
    CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  return val;
}


struct object *
evaluate_prog1 (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *tmp, *ret;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  ret = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!ret)
    return NULL;

  tmp = evaluate_body (CDR (list), -1, 0, 0, NULL, env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
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

  tmp = evaluate_body (CDR (CDR (list)), -1, 0, 0, NULL, env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
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
      return raise_type_error (args, "CL:LIST", env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  if (!IS_LIST (CAR (CDR (list))))
    {
      return raise_type_error (CAR (CDR (list)), "CL:LIST", env, outcome);
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
  struct object *tmp, *l, *cons, *val, *newv, *func;
  int len;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (IS_SYMBOL (CAR (list)))
    {
      prepend_object_to_obj_list (SYMBOL (CAR (list)), &outcome->other_values);
      increment_refcount (SYMBOL (CAR (list)));

      tmp = builtin_gensym (&nil_object, env, outcome);

      l = alloc_empty_list (3);
      l->value_ptr.cons_pair->car = BUILTIN_SYMBOL ("SETQ");
      add_reference (l, CAR (l), 0);

      l->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = SYMBOL (CAR (list));
      add_reference (CDR (l), SYMBOL (CAR (list)), 0);

      l->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr->
	value_ptr.cons_pair->car = tmp;

      prepend_object_to_obj_list (l, &outcome->other_values);

      l = alloc_empty_list (1);
      l->value_ptr.cons_pair->car = tmp;
      add_reference (l, CAR (l), 0);
      prepend_object_to_obj_list (l, &outcome->other_values);

      prepend_object_to_obj_list (&nil_object, &outcome->other_values);

      return &nil_object;
    }
  else if (CAR (list)->type == TYPE_CONS_PAIR && IS_SYMBOL (CAR (CAR (list))))
    {
      if (SYMBOL (CAR (CAR (list)))->value_ptr.symbol->setf_expander)
	{
	  return call_function (SYMBOL (CAR (CAR (list)))->value_ptr.symbol->
				setf_expander, CDR (CAR (list)), 0, 0, 0, 0, 0,
				env, outcome);
	}

      len = list_length (CAR (list));

      if (len == 1)
	l = &nil_object;
      else
	l = alloc_empty_list (list_length (CAR (list))-1);

      cons = l;

      while (cons != &nil_object)
	{
	  cons->value_ptr.cons_pair->car = builtin_gensym (&nil_object, env,
							   outcome);
	  cons = CDR (cons);
	}

      val = alloc_empty_cons_pair ();
      val->value_ptr.cons_pair->car = SYMBOL (CAR (CAR (list)));
      add_reference (val, CAR (val), 0);
      val->value_ptr.cons_pair->cdr = l;
      add_reference (val, l, 1);
      prepend_object_to_obj_list (val, &outcome->other_values);

      func = alloc_empty_list (2);
      func->value_ptr.cons_pair->car = env->function_sym;
      add_reference (func, CAR (func), 0);

      func->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car
	= alloc_empty_list (2);
      func->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car->
	value_ptr.cons_pair->car = env->setf_sym;
      add_reference (CAR (CDR (func)), CAR (CAR (CDR (func))), 0);
      func->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car->
	value_ptr.cons_pair->cdr->value_ptr.cons_pair->car
	= SYMBOL (CAR (CAR (list)));
      add_reference (CDR (CAR (CDR (func))), CAR (CDR (CAR (CDR (func)))), 0);

      newv = builtin_gensym (&nil_object, env, outcome);

      val = alloc_empty_list (3);
      val->value_ptr.cons_pair->car = BUILTIN_SYMBOL ("FUNCALL");
      add_reference (val, CAR (val), 0);
      val->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = func;
      val->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr->
	value_ptr.cons_pair->car = newv;
      val->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr->
	value_ptr.cons_pair->cdr = l;
      add_reference (CDR (CDR (val)), CDR (CDR (CDR (val))), 1);
      prepend_object_to_obj_list (val, &outcome->other_values);

      val = alloc_empty_list (1);
      val->value_ptr.cons_pair->car = newv;
      add_reference (val, CAR (val), 0);
      prepend_object_to_obj_list (val, &outcome->other_values);

      increment_refcount (CDR (CAR (list)));
      prepend_object_to_obj_list (CDR (CAR (list)), &outcome->other_values);

      return l;
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }
}


struct object *
evaluate_define_symbol_macro (struct object *list, struct environment *env,
			      struct outcome *outcome)
{
  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list))
      || (SYMBOL (CAR (list))->value_ptr.symbol->value_cell
	  && !SYMBOL (CAR (list))->value_ptr.symbol->is_symbol_macro))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  delete_reference (SYMBOL (CAR (list)),
		    SYMBOL (CAR (list))->value_ptr.symbol->value_cell, 0);
  SYMBOL (CAR (list))->value_ptr.symbol->value_cell = CAR (CDR (list));
  add_reference (SYMBOL (CAR (list)), CAR (CDR (list)), 0);
  SYMBOL (CAR (list))->value_ptr.symbol->is_symbol_macro = 1;

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
evaluate_symbol_macrolet (struct object *list, struct environment *env,
			  struct outcome *outcome)
{
  int bin_num = 0;
  struct object *res, *bind_forms;
  struct binding *bin;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_LIST (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:LIST", env, outcome);
    }

  bind_forms = CAR (list);

  while (SYMBOL (bind_forms) != &nil_object)
    {
      if (CAR (bind_forms)->type != TYPE_CONS_PAIR
	  || list_length (CAR (bind_forms)) != 2)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      bin = create_binding (CAR (CAR (bind_forms)), CAR (CDR (CAR (bind_forms))),
			    LEXICAL_BINDING, 1);
      bin->is_symbol_macro = 1;

      env->vars = add_binding (bin, env->vars);
      env->lex_env_vars_boundary++, bin_num++;

      bind_forms = CDR (bind_forms);
    }

  res = evaluate_body (CDR (list), bin_num, 0, 0, NULL, env, outcome);

  env->vars = remove_bindings (env->vars, bin_num, 1);

  env->lex_env_vars_boundary -= bin_num;

  return res;
}


struct object *
evaluate_defstruct (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct object *name, *strcl, *funcname, *cons, *opt, *predname = NULL,
    *copiername = NULL, *pack = inspect_variable (env->package_sym, env);
  struct structure_class *sc;
  struct structure_field_decl *f, *prev;
  char *constr_name, *pred_name, *cop_name, *acc_name;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (IS_SYMBOL (CAR (list)))
    name = SYMBOL (CAR (list));
  else if (IS_LIST (CAR (list)))
    {
      if (!IS_SYMBOL (CAR (CAR (list))))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      name = SYMBOL (CAR (CAR (list)));

      cons = CDR (CAR (list));

      while (cons->type == TYPE_CONS_PAIR)
	{
	  if (IS_SYMBOL (CAR (cons)))
	    opt = SYMBOL (CAR (cons));
	  else if (CAR (cons)->type == TYPE_CONS_PAIR
		   && IS_SYMBOL (CAR (CAR ((cons)))))
	    opt = SYMBOL (CAR (CAR (cons)));
	  else
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
	      return NULL;
	    }

	  if (symbol_equals (opt, ":PREDICATE", env)
	      && CAR (cons)->type == TYPE_CONS_PAIR
	      && list_length (CAR (cons)) > 1
	      && IS_SYMBOL (CAR (CDR (CAR (cons)))))
	    predname = SYMBOL (CAR (CDR (CAR (cons))));
	  else if (symbol_equals (opt, ":COPIER", env)
		   && CAR (cons)->type == TYPE_CONS_PAIR
		   && list_length (CAR (cons)) > 1
		   && IS_SYMBOL (CAR (CDR (CAR (cons)))))
	    copiername = SYMBOL (CAR (CDR (CAR (cons))));
	  else
	    {
	      outcome->type = WRONG_TYPE_OF_ARGUMENT;
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

  if (list->type == TYPE_CONS_PAIR && CAR (list)->type == TYPE_STRING)
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
					   INTERNAL_VISIBILITY, 1, pack, 0, 0);
  free (constr_name);
  increment_refcount (funcname);

  delete_reference (funcname, funcname->value_ptr.symbol->function_cell, 1);
  funcname->value_ptr.symbol->function_cell = alloc_function ();
  add_reference (funcname, funcname->value_ptr.symbol->function_cell, 1);
  decrement_refcount (funcname->value_ptr.symbol->function_cell);

  funcname->value_ptr.symbol->function_cell->value_ptr.function->
    struct_constructor_class_name = name;

  funcname->value_ptr.symbol->function_cell->value_ptr.function->name = funcname;
  add_reference (funcname->value_ptr.symbol->function_cell, funcname, 0);

  if (!predname || SYMBOL (predname) != &nil_object)
    {
      if (!predname)
	{
	  pred_name =
	    concatenate_char_vectors (2 + name->value_ptr.symbol->name_len,
				      name->value_ptr.symbol->name,
				      name->value_ptr.symbol->name_len, "-P", 2,
				      (char *)NULL);

	  predname = intern_symbol_by_char_vector (pred_name,
						   2+name->value_ptr.symbol->name_len,
						   1, INTERNAL_VISIBILITY, 1,
						   pack, 0, 0);
	  free (pred_name);
	  increment_refcount (predname);
	}

      delete_reference (predname, predname->value_ptr.symbol->function_cell, 1);
      predname->value_ptr.symbol->function_cell = alloc_function ();
      add_reference (predname, predname->value_ptr.symbol->function_cell, 1);
      decrement_refcount (predname->value_ptr.symbol->function_cell);

      predname->value_ptr.symbol->function_cell->value_ptr.function->
	struct_predicate_class_name = name;

      predname->value_ptr.symbol->function_cell->value_ptr.function->name
	= predname;
      add_reference (predname->value_ptr.symbol->function_cell, predname, 0);
    }

  if (!copiername || SYMBOL (copiername) != &nil_object)
    {
      if (!copiername)
	{
	  cop_name =
	    concatenate_char_vectors (5 + name->value_ptr.symbol->name_len, "COPY-",
				      5, name->value_ptr.symbol->name,
				      name->value_ptr.symbol->name_len,
				      (char *)NULL);

	  copiername =
	    intern_symbol_by_char_vector (cop_name,
					  5+name->value_ptr.symbol->name_len, 1,
					  INTERNAL_VISIBILITY, 1, pack, 0, 0);
	  free (cop_name);
	  increment_refcount (copiername);
	}

      delete_reference (copiername, copiername->value_ptr.symbol->function_cell,
			1);
      copiername->value_ptr.symbol->function_cell = alloc_function ();
      add_reference (copiername, copiername->value_ptr.symbol->function_cell, 1);
      decrement_refcount (copiername->value_ptr.symbol->function_cell);

      copiername->value_ptr.symbol->function_cell->value_ptr.function->
	struct_copyier_class_name = name;

      copiername->value_ptr.symbol->function_cell->value_ptr.function->name =
	copiername;
      add_reference (copiername->value_ptr.symbol->function_cell, copiername, 0);
    }

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
					       1, INTERNAL_VISIBILITY, 1, pack,
					       0, 0);
      free (acc_name);
      increment_refcount (funcname);

      delete_reference (funcname, funcname->value_ptr.symbol->function_cell, 1);
      funcname->value_ptr.symbol->function_cell = alloc_function ();
      add_reference (funcname, funcname->value_ptr.symbol->function_cell, 1);
      decrement_refcount (funcname->value_ptr.symbol->function_cell);

      funcname->value_ptr.symbol->function_cell->value_ptr.function->
	struct_accessor_class_name = name;
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
  struct object *name;

  if (list_length (list) < 3)
    {
      return raise_al_wrong_number_of_arguments (3, -1, env, outcome);
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

  if (!IS_LIST (CAR (CDR (list))))
    {
      return raise_type_error (CAR (CDR (list)), "CL:LIST", env, outcome);
    }

  if (!IS_LIST (CAR (CDR (CDR (list)))))
    {
      return raise_type_error (CAR (CDR (CDR (list))), "CL:LIST", env, outcome);
    }

  return define_class (name, CDR (list), 0, env, outcome);
}


struct object *
builtin_find_class (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  int l = list_length (list);
  struct object *errorp = &t_object;

  if (!l || l > 3)
    {
      return raise_al_wrong_number_of_arguments (1, 3, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  if (l > 1)
    errorp = CAR (CDR (list));

  if (SYMBOL (CAR (list))->value_ptr.symbol->typespec &&
      SYMBOL (CAR (list))->value_ptr.symbol->typespec->type
      == TYPE_STANDARD_CLASS)
    {
      increment_refcount (SYMBOL (CAR (list))->value_ptr.symbol->typespec);
      return SYMBOL (CAR (list))->value_ptr.symbol->typespec;
    }
  else if (SYMBOL (errorp) == &nil_object)
    return &nil_object;

  return raise_error (env, outcome);
}


struct object *
builtin_method_make_instance (struct object *list, struct environment *env,
			      struct outcome *outcome)
{
  struct object *class = NULL, *ret;
  struct standard_object *so;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
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

  if (!class->value_ptr.standard_class->class_precedence_list
      && !compute_class_precedence_list (class, outcome))
    {
      return NULL;
    }

  ret = alloc_object ();
  ret->type = TYPE_STANDARD_OBJECT;

  so = malloc_and_check (sizeof (*so));
  so->class = class;
  increment_refcount (class);
  so->fields = NULL;
  ret->value_ptr.standard_object = so;

  allocate_object_fields (ret, class);

  return fill_object_fields (ret, class, CDR (list), env, outcome);
}


struct object *
builtin_method_allocate_instance (struct object *list, struct environment *env,
				  struct outcome *outcome)
{
  struct object *ret;
  struct standard_object *so;

  if (SYMBOL (list) == &nil_object)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STANDARD_CLASS)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (!CAR (list)->value_ptr.standard_class->class_precedence_list
      && !compute_class_precedence_list (CAR (list), outcome))
    {
      return NULL;
    }

  ret = alloc_object ();
  ret->type = TYPE_STANDARD_OBJECT;

  so = malloc_and_check (sizeof (*so));
  so->class = CAR (list);
  increment_refcount (CAR (list));
  so->fields = NULL;
  ret->value_ptr.standard_object = so;

  allocate_object_fields (ret, CAR (list));

  return ret;
}


struct object *
builtin_method_initialize_instance (struct object *list, struct environment *env,
				    struct outcome *outcome)
{
  if (SYMBOL (list) == &nil_object)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STANDARD_OBJECT)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  increment_refcount (CAR (list));
  return fill_object_fields (CAR (list), CAR (list)->value_ptr.standard_object->
			     class, CDR (list), env, outcome);
}


struct object *
builtin_method_reinitialize_instance (struct object *list,
				      struct environment *env,
				      struct outcome *outcome)
{
  if (SYMBOL (list) == &nil_object)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STANDARD_OBJECT)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  increment_refcount (CAR (list));
  return fill_object_fields_by_initargs (CAR (list),
					 CAR (list)->value_ptr.standard_object->
					 class, CDR (list), env, outcome);
}


struct object *
builtin_method_change_class (struct object *list, struct environment *env,
			     struct outcome *outcome)
{
  struct object *obj, *newcl = NULL;
  struct class_field_decl *fd;
  struct object_list *p;
  struct class_field *f, *prev = NULL, *tmp;


  if (CAR (list)->type != TYPE_STANDARD_OBJECT)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (CAR (CDR (list))->type == TYPE_STANDARD_CLASS)
    {
      newcl = CAR (CDR (list));
    }
  else if (IS_SYMBOL (CAR (CDR (list))))
    {
      newcl = SYMBOL (CAR (CDR (list)))->value_ptr.symbol->typespec;

      if (newcl && newcl->type != TYPE_STANDARD_CLASS)
	newcl = NULL;
    }

  if (!newcl)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  obj = CAR (list);

  if (!newcl->value_ptr.standard_class->class_precedence_list
      && !compute_class_precedence_list (newcl, outcome))
    {
      return NULL;
    }

  f = obj->value_ptr.standard_object->fields;

  while (f)
    {
      p = newcl->value_ptr.standard_class->class_precedence_list;

      while (p)
	{
	  fd = p->obj->value_ptr.standard_class->fields;

	  while (fd)
	    {
	      if (fd->name == f->decl->name)
		{
		  f->decl = fd;
		  break;
		}

	      fd = fd->next;
	    }

	  if (fd)
	    break;

	  p = p->next;
	}

      if (!p)
	{
	  if (prev)
	    prev->next = f->next;
	  else
	    obj->value_ptr.standard_object->fields = f->next;

	  tmp = f->next;

	  decrement_refcount (f->value);
	  free (f);

	  f = tmp;
	}
      else
	{
	  prev = f;
	  f = f->next;
	}
    }

  p = newcl->value_ptr.standard_class->class_precedence_list;

  while (p)
    {
      fd = p->obj->value_ptr.standard_class->fields;

      while (fd)
	{
	  f = obj->value_ptr.standard_object->fields;

	  while (f)
	    {
	      if (f->decl->name == fd->name)
		break;

	      f = f->next;
	    }

	  if (f)
	    {
	      fd = fd->next;
	      continue;
	    }

	  f = malloc_and_check (sizeof (*f));

	  if (fd->alloc_type == INSTANCE_ALLOCATION)
	    f->name = fd->name;
	  else
	    f->name = NULL;

	  f->value = NULL;
	  f->decl = fd;
	  f->next = obj->value_ptr.standard_object->fields;
	  obj->value_ptr.standard_object->fields = f;

	  fd = fd->next;
	}

      p = p->next;
    }

  increment_refcount (newcl);
  obj->value_ptr.standard_object->class = newcl;

  increment_refcount (obj);

  return fill_object_fields (obj, newcl, CDR (CDR (list)), env, outcome);
}


struct object *
builtin_class_of (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type == TYPE_STANDARD_OBJECT)
    {
      increment_refcount (CAR (list)->value_ptr.standard_object->class);
      return CAR (list)->value_ptr.standard_object->class;
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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
      if (f->decl->name == req)
	{
	  if ((f->name && !f->value) || (!f->name && !f->decl->value))
	    {
	      outcome->type = SLOT_NOT_BOUND;
	      return NULL;
	    }

	  if (f->name)
	    {
	      increment_refcount (f->value);
	      return f->value;
	    }
	  else
	    {
	      increment_refcount (f->decl->value);
	      return f->decl->value;
	    }
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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
    }

  if ((!IS_SYMBOL (CAR (list))
       && !(CAR (list)->type == TYPE_CONS_PAIR
	    && list_length (CAR (list)) == 2
	    && SYMBOL (CAR (CAR (list))) == env->setf_sym
	    && IS_SYMBOL (CAR (CDR (CAR (list))))))
      || !IS_LIST (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  sym = IS_SYMBOL (CAR (list)) ? SYMBOL (CAR (list))
    : SYMBOL (CAR (CDR (CAR (list))));

  if (sym->value_ptr.symbol->function_cell
      && (sym->value_ptr.symbol->function_cell->type == TYPE_MACRO
	  || !(sym->value_ptr.symbol->function_cell->value_ptr.function->
	       flags & GENERIC_FUNCTION)))
    {
      outcome->type = CANT_REDEFINE_AS_GENERIC_FUNCTION;
      return NULL;
    }

  fun = create_function (CAR (CDR (list)), &nil_object, env, outcome, 0, 0);

  if (!fun)
    return NULL;

  fun->value_ptr.function->flags |= GENERIC_FUNCTION;

  if (SYMBOL (inspect_variable (env->al_compile_when_defining_sym, env))
      != &nil_object)
    {
      fun->value_ptr.function->flags |= COMPILED_FUNCTION;
    }

  if (IS_SYMBOL (CAR (list)))
    {
      if (sym->value_ptr.symbol->function_cell)
	{
	  delete_reference (sym, sym->value_ptr.symbol->function_cell, 1);
	}

      sym->value_ptr.symbol->function_cell = fun;
      add_reference (sym, fun, 1);
    }
  else
    {
      if (sym->value_ptr.symbol->setf_func_cell)
	{
	  delete_reference (sym, sym->value_ptr.symbol->setf_func_cell, 2);
	}

      sym->value_ptr.symbol->setf_func_cell = fun;
      add_reference (sym, fun, 2);

      fun->value_ptr.function->is_setf_func = 1;
    }

  fun->value_ptr.function->name = sym;
  add_reference (fun, sym, 0);

  return fun;
}


struct object *
builtin_ensure_generic_function (struct object *list, struct environment *env,
				 struct outcome *outcome)
{
  struct object *sym, *lambdal = NULL, *allow_other_keys = NULL, *fun;
  struct parameter *ll;
  int found_unknown_key = 0, i, found_amp_key;

  if (SYMBOL (list) == &nil_object)
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  sym = SYMBOL (CAR (list));

  if (sym->value_ptr.symbol->function_cell
      && (sym->value_ptr.symbol->function_cell->type == TYPE_MACRO
	  || !(sym->value_ptr.symbol->function_cell->value_ptr.function->
	       flags & GENERIC_FUNCTION)))
    {
      outcome->type = CANT_REDEFINE_AS_GENERIC_FUNCTION;
      return NULL;
    }


  list = CDR (list);

  while (SYMBOL (list) != &nil_object)
    {
      if (symbol_equals (CAR (list), ":LAMBDA-LIST", env))
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!IS_LIST (CAR (CDR (list))))
	    {
	      return raise_type_error (CAR (CDR (list)), "CL:LIST", env, outcome);
	    }

	  if (!lambdal)
	    lambdal = CAR (CDR (list));

	  list = CDR (list);
	}
      else if (SYMBOL (CAR (list)) == env->key_allow_other_keys_sym)
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  if (!allow_other_keys)
	    allow_other_keys = CAR (CDR (list));

	  list = CDR (list);
	}
      else
	{
	  if (SYMBOL (CDR (list)) == &nil_object)
	    {
	      outcome->type = ODD_NUMBER_OF_KEYWORD_ARGUMENTS;
	      return NULL;
	    }

	  found_unknown_key = 1;

	  list = CDR (list);
	}

      list = CDR (list);
    }

  if (found_unknown_key && (!allow_other_keys
			    || SYMBOL (allow_other_keys) == &nil_object))
    {
      return raise_program_error (env, outcome);
    }


  fun = sym->value_ptr.symbol->function_cell;

  if (!fun)
    {
      fun = create_function (lambdal ? lambdal : &nil_object, &nil_object, env,
			     outcome, 0, 0);

      if (!fun)
	return NULL;

      fun->value_ptr.function->flags |= GENERIC_FUNCTION;

      if (SYMBOL (inspect_variable (env->al_compile_when_defining_sym, env))
	  != &nil_object)
	{
	  fun->value_ptr.function->flags |= COMPILED_FUNCTION;
	}
    }
  else
    {
      outcome->type = EVAL_OK;
      ll = parse_lambda_list (lambdal ? lambdal : &nil_object, 0, 1, env,
			      outcome, &found_amp_key,
			      &fun->value_ptr.function->allow_other_keys);

      if (outcome->type != EVAL_OK)
	return NULL;

      if (found_amp_key)
	fun->value_ptr.function->flags |= FOUND_AMP_KEY;

      i = 2;
      free_lambda_list_content (fun, fun->value_ptr.function->lambda_list, &i,
				0);
      free_lambda_list_structure (fun->value_ptr.function->lambda_list);

      fun->value_ptr.function->lambda_list = ll;
    }


  if (!sym->value_ptr.symbol->function_cell)
    {
      sym->value_ptr.symbol->function_cell = fun;
      add_reference (sym, fun, 1);

      fun->value_ptr.function->name = sym;
      add_reference (fun, sym, 0);

      return fun;
    }

  increment_refcount (fun);
  return fun;
}


struct object *
evaluate_defmethod (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct object *fun, *meth, *lambdal, *sym;
  struct method *m;
  struct method_list *ml;
  enum method_qualifier q = PRIMARY_METHOD;

  if (list_length (list) < 2)
    {
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
    }

  if ((!IS_SYMBOL (CAR (list))
       && !(CAR (list)->type == TYPE_CONS_PAIR
	    && list_length (CAR (list)) == 2
	    && SYMBOL (CAR (CAR (list))) == env->setf_sym
	    && IS_SYMBOL (CAR (CDR (CAR (list))))))
      || (!IS_SYMBOL (CAR (CDR (list)))
	  && CAR (CDR (list))->type != TYPE_CONS_PAIR))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  sym = IS_SYMBOL (CAR (list)) ? SYMBOL (CAR (list))
    : SYMBOL (CAR (CDR (CAR (list))));

  fun = get_function (sym, env, 0, CAR (list)->type == TYPE_CONS_PAIR, 1, 0);

  if (fun && (fun->type == TYPE_MACRO
	      || !(fun->value_ptr.function->flags & GENERIC_FUNCTION)))
    {
      outcome->type = CANT_REDEFINE_AS_GENERIC_FUNCTION;
      return NULL;
    }

  if (!IS_LIST (CAR (CDR (list))))
    {
      if (symbol_equals (CAR (CDR (list)), ":AROUND", env))
	q = AROUND_METHOD;
      else if (symbol_equals (CAR (CDR (list)), ":BEFORE", env))
	q = BEFORE_METHOD;
      else if (symbol_equals (CAR (CDR (list)), ":AFTER", env))
	q = AFTER_METHOD;
      else
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      lambdal = CAR (CDR (CDR (list)));
    }
  else
    lambdal = CAR (CDR (list));

  meth = alloc_object ();
  meth->type = TYPE_METHOD;
  m = meth->value_ptr.method = malloc_and_check (sizeof (*m));
  m->generic_func = NULL;
  m->qualifier = q;
  m->body = NULL;

  outcome->type = EVAL_OK;
  m->lambda_list = NULL;
  m->lambda_list = parse_lambda_list (lambdal, 0, 1, env, outcome,
				      &m->found_amp_key, &m->allow_other_keys);

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

      fun->value_ptr.function->flags |= GENERIC_FUNCTION;

      if (SYMBOL (inspect_variable (env->al_compile_when_defining_sym, env))
	  != &nil_object)
	{
	  fun->value_ptr.function->flags |= COMPILED_FUNCTION;
	}

      if (CAR (list)->type == TYPE_CONS_PAIR)
	{
	  sym->value_ptr.symbol->setf_func_cell = fun;
	  add_reference (sym, fun, 2);
	  fun->value_ptr.function->is_setf_func = 1;
	}
      else
	{
	  sym->value_ptr.symbol->function_cell = fun;
	  add_reference (SYMBOL (CAR (list)), fun, 1);
	}

      fun->value_ptr.function->name = sym;
      add_reference (fun, sym, 0);

      decrement_refcount (fun);
    }
  else if (!are_lambda_lists_congruent (m->lambda_list, m->found_amp_key,
					fun->value_ptr.function->lambda_list,
					fun->value_ptr.function->flags
					& FOUND_AMP_KEY))
    {
      free_method (meth);
      outcome->type = LAMBDA_LISTS_NOT_CONGRUENT;
      return NULL;
    }

  if (m->allow_other_keys)
    fun->value_ptr.function->allow_other_keys = 1;

  m->body = q == PRIMARY_METHOD ? CDR (CDR (list)) : CDR (CDR (CDR (list)));
  add_reference (meth, m->body, 1);

  m->generic_func = fun;
  add_reference (meth, fun, 0);

  if (fun->value_ptr.function->flags & COMPILED_FUNCTION)
    {
      if (!compile_body (m->body, 0, env, outcome))
	return NULL;
    }

  ml = malloc_and_check (sizeof (*ml));
  ml->reference_strength_factor = 1;
  ml->meth = meth;
  ml->next = fun->value_ptr.function->methods;

  fun->value_ptr.function->methods = ml;
  INC_WEAK_REFCOUNT (meth);

  return meth;
}


struct object *
builtin_add_method (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  struct object *meth;
  struct method_list *ml;
  int ind;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_FUNCTION
      || !(CAR (list)->value_ptr.function->flags & GENERIC_FUNCTION)
      || CAR (CDR (list))->type != TYPE_METHOD)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  meth = find_method (CAR (list), CAR (CDR (list))->value_ptr.method->qualifier,
		      NULL, CAR (CDR (list))->value_ptr.method->lambda_list, &ml,
		      &ind, env, outcome);

  if (!meth)
    return NULL;

  if (meth == &nil_object)
    {
      ml = malloc_and_check (sizeof (*ml));
      ml->meth = CAR (CDR (list));
      ml->next = CAR (list)->value_ptr.function->methods;
      CAR (list)->value_ptr.function->methods = ml;
      add_reference (CAR (list), ml->meth, 0);
    }
  else
    {
      delete_reference (CAR (list), ml->meth, 6+ind*8);
      ml->meth = CAR (CDR (list));
      add_reference (CAR (list), ml->meth, 6+ind*8);
    }

  if (CAR (list)->value_ptr.function->flags & COMPILED_FUNCTION)
    {
      if (!compile_body (ml->meth->value_ptr.method->body, 0, env, outcome))
	return NULL;
    }

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_find_method (struct object *list, struct environment *env,
		     struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 3)
    {
      return raise_al_wrong_number_of_arguments (3, 3, env, outcome);
    }

  if (CAR (list)->type != TYPE_FUNCTION
      || !(CAR (list)->value_ptr.function->flags & GENERIC_FUNCTION)
      || !IS_LIST (CAR (CDR (list))) || !IS_LIST (CAR (CDR (CDR (list)))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (list_length (CAR (CDR (list))) > 1)
    return &nil_object;

  ret = find_method (CAR (list),
		     CAR (CDR (list)) == &nil_object ? PRIMARY_METHOD
		     : SYMBOL (CAR (CAR (CDR (list)))) == KEYWORD (":BEFORE")
		     ? BEFORE_METHOD :
		     SYMBOL (CAR (CAR (CDR (list)))) == KEYWORD (":AFTER")
		     ? AFTER_METHOD
		     : SYMBOL (CAR (CAR (CDR (list)))) == KEYWORD (":AROUND")
		     ? AROUND_METHOD : 0, CAR (CDR (CDR (list))), NULL, NULL,
		     NULL, env, outcome);

  if (!ret)
    return NULL;

  increment_refcount (ret);
  return ret;
}


struct object *
builtin_remove_method (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct method_list *ml, *prev = NULL;
  int i = 6;

  if (list_length (list) != 2)
    {
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
    }

  if (CAR (list)->type != TYPE_FUNCTION
      || !(CAR (list)->value_ptr.function->flags & GENERIC_FUNCTION)
      || CAR (CDR (list))->type != TYPE_METHOD)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  ml = CAR (list)->value_ptr.function->methods;

  while (ml)
    {
      if (ml->meth == CAR (CDR (list)))
	{
	  delete_reference (CAR (list), ml->meth, i);

	  if (prev)
	    prev->next = ml->next;
	  else
	    CAR (list)->value_ptr.function->methods = ml->next;

	  free (ml);
	  break;
	}

      i += 8;
      prev = ml;
      ml = ml->next;
    }

  increment_refcount (CAR (list));
  return CAR (list);
}


struct object *
builtin_next_method_p (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  if (SYMBOL (list) != &nil_object)
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  if (env->method_list && env->method_list->next)
    return &t_object;

  return &nil_object;
}


struct object *
evaluate_call_next_method (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  struct object *args, *prevargs, *res, *ret = NULL;
  struct method_list *medl;


  if (!env->method_list || !env->method_list->next
      || env->method_list->meth->value_ptr.method->qualifier & (BEFORE_METHOD
								| AFTER_METHOD))
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

  prevargs = env->method_args;
  env->method_args = args;

  medl = env->method_list;
  env->method_list = env->method_list->next;

  while (env->method_list)
    {
      res = call_method (env->method_list, args, env, outcome);

      if (!res)
	{
	  ret = NULL;
	  break;
	}

      if (env->method_list->meth->value_ptr.method->qualifier == AROUND_METHOD)
	{
	  decrement_refcount (ret);
	  ret = res;
	  break;
	}
      else if (env->method_list->meth->value_ptr.method->qualifier
	       == BEFORE_METHOD
	       || env->method_list->meth->value_ptr.method->qualifier
	       == AFTER_METHOD)
	{
	  env->method_list = env->method_list->next;
	  decrement_refcount (res);
	}
      else
	{
	  env->method_list = env->method_list->next;
	  decrement_refcount (ret);
	  ret = res;

	  while (env->method_list
		 && env->method_list->meth->value_ptr.method->qualifier
		 == PRIMARY_METHOD)
	    {
	      env->method_list = env->method_list->next;
	    }
	}
    }

  env->method_args = prevargs;
  env->method_list = medl;

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
builtin_function_lambda_expression (struct object *list, struct environment *env,
				    struct outcome *outcome)
{
  struct object *ret, *cons, *mac;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type == TYPE_FUNCTION)
    {
      ret = alloc_empty_cons_pair ();

      ret->value_ptr.cons_pair->car = env->lambda_sym;
      add_reference (ret, CAR (ret), 0);

      ret->value_ptr.cons_pair->cdr = cons = alloc_empty_cons_pair ();
      cons->value_ptr.cons_pair->car
	= list_lambda_list (CAR (list)->value_ptr.function->lambda_list,
			    CAR (list)->value_ptr.function->allow_other_keys,
			    env);

      cons->value_ptr.cons_pair->cdr = CAR (list)->value_ptr.function->body
	? CAR (list)->value_ptr.function->body : &nil_object;
      add_reference (cons, CDR (cons), 1);

      if (CAR (list)->value_ptr.function->function_macro)
	{
	  mac = alloc_empty_cons_pair ();

	  mac->value_ptr.cons_pair->car = env->lambda_sym;
	  add_reference (mac, CAR (mac), 0);

	  mac->value_ptr.cons_pair->cdr = cons = alloc_empty_cons_pair ();
	  cons->value_ptr.cons_pair->car
	    = list_lambda_list (CAR (list)->value_ptr.function->function_macro
				->value_ptr.macro->lambda_list,
				CAR (list)->value_ptr.function->function_macro
				->value_ptr.macro->allow_other_keys, env);

	  cons->value_ptr.cons_pair->cdr = CAR (list)->value_ptr.function->
	    function_macro->value_ptr.function->body;
	  add_reference (cons, CDR (cons), 1);
	  prepend_object_to_obj_list (mac, &outcome->other_values);
	}
      else
	{
	  prepend_object_to_obj_list (CAR (list)->value_ptr.function->name ?
				      CAR (list)->value_ptr.function->name :
				      &nil_object, &outcome->other_values);
	  increment_refcount (outcome->other_values->obj);
	}

      prepend_object_to_obj_list (CAR (list)->value_ptr.function->lex_vars ?
				  &t_object : &nil_object, &outcome->other_values);
    }
  else if (CAR (list)->type == TYPE_METHOD)
    {
      ret = CAR (list)->value_ptr.method->body
	? CAR (list)->value_ptr.method->body : &nil_object;
      increment_refcount (ret);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return ret;
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
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!parse_declaration_specifier (CAR (list), 0, env, -1, outcome))
    return NULL;

  return &t_object;
}


struct object *
evaluate_tagbody (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  struct object *ret = evaluate_body (list, -1, 1, 1, NULL, env, outcome);
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
  struct go_tag_frame *f = env->go_tag_stack;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_INTEGER && !IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:SYMBOL CL:INTEGER)", env,
			       outcome);
    }

  while (f)
    {
      if ((outcome->tag_to_jump_to = find_go_tag (CAR (list), f)))
	break;

      f = f->next;
    }

  if (!outcome->tag_to_jump_to)
    outcome->type = TAG_NOT_FOUND;

  return NULL;
}


struct object *
evaluate_block (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  return evaluate_body (CDR (list), -1, 0, 0, SYMBOL (CAR (list)), env, outcome);
}


struct object *
evaluate_return_from (struct object *list, struct environment *env,
		      struct outcome *outcome)
{
  int l = list_length (list);
  struct block *b = env->blocks ? env->blocks->frame : NULL;
  struct object *ret;

  if (!l || l > 2)
    {
      return raise_al_wrong_number_of_arguments (1, 2, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  if (l == 2)
    {
      ret = evaluate_object (CAR (CDR (list)), env, outcome);

      if (!ret)
	return NULL;
    }
  else
    ret = &nil_object;

  while (b)
    {
      if (SYMBOL (CAR (list)) == b->name)
	{
	  outcome->block_to_leave = b;
	  break;
	}

      b = b->next;
    }

  if (!b)
    {
      outcome->type = BLOCK_NOT_FOUND;
      outcome->no_value = 0;
      outcome->other_values = NULL;
      return NULL;
    }

  outcome->return_value = ret;
  outcome->return_no_value = outcome->no_value;
  outcome->return_other_values = outcome->other_values;
  outcome->no_value = 0;
  outcome->other_values = NULL;

  return NULL;
}


struct object *
evaluate_catch (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *tag, *ret;
  struct object_list *c;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  tag = evaluate_object (CAR (list), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!tag)
    return NULL;

  c = malloc_and_check (sizeof (*c));
  c->obj = tag;
  c->next = env->catches;
  env->catches = c;

  ret = evaluate_body (CDR (list), -1, 0, 0, NULL, env, outcome);

  if (!ret && outcome->catching_tag
      && eq_objects (tag, outcome->catching_tag) == &t_object)
    {
      decrement_refcount (outcome->catching_tag);
      outcome->catching_tag = NULL;

      ret = outcome->return_value;

      outcome->no_value = outcome->return_no_value;
      outcome->other_values = outcome->return_other_values;
      outcome->return_no_value = 0;
      outcome->return_other_values = NULL;
    }

  decrement_refcount (tag);

  c = env->catches->next;
  free (env->catches);
  env->catches = c;

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
      return raise_al_wrong_number_of_arguments (2, 2, env, outcome);
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

  outcome->catching_tag = tag;
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
  struct object *arg = NULL;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_LIST (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:LIST", env, outcome);
    }

  cons = CAR (list);

  while (cons->type == TYPE_CONS_PAIR)
    {
      if (CAR (cons)->type != TYPE_CONS_PAIR || list_length (CAR (cons)) != 2
	  || !IS_TYPE_SPECIFIER (CAR (CAR (cons))))
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
      b->condition = CAR (CAR (cons));
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

  ret = evaluate_body (CDR (list), -1, 0, 0, NULL, env, outcome);

  if (!ret && first)
    {
      b = first;

      arg = alloc_empty_cons_pair ();
      arg->value_ptr.cons_pair->car = &nil_object;
      arg->value_ptr.cons_pair->cdr = &nil_object;

      while (b)
	{
	  if (does_condition_include_outcome_type (b->condition, outcome->type,
						   env))
	    {
	      hret = call_function (b->handler, arg, 1, 0, 1, 0, 0, env, outcome);

	      if (!hret)
		goto cleanup_and_leave;

	      decrement_refcount (hret);
	    }

	  b = b->next;
	}
    }

 cleanup_and_leave:
  decrement_refcount (arg);

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
  struct object *cons, *res, *ret = NULL;
  struct restart_binding *b, *last = NULL;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_LIST (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:LIST", env, outcome);
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

      if (!last)
	{
	  b->next = env->restarts;
	  last = env->restarts = b;
	}
      else
	{
	  b->next = last->next;
	  last->next = b;
	  last = b;
	}

      restarts++;

      cons = CDR (cons);
    }

  ret = evaluate_body (CDR (list), -1, 0, 0, NULL, env, outcome);

 cleanup_and_leave:
  for (; restarts; restarts--)
    {
      b = env->restarts->next;
      decrement_refcount (env->restarts->restart);
      free (env->restarts);
      env->restarts = b;
    }

  return ret;
}


struct object *
builtin_compute_restarts (struct object *list, struct environment *env,
			  struct outcome *outcome)
{
  struct restart_binding *r = env->restarts;
  struct object *ret = &nil_object, *cons;

  if (list_length (list) > 1)
    {
      return raise_al_wrong_number_of_arguments (0, 1, env, outcome);
    }

  while (r)
    {
      if (ret != &nil_object)
	cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
      else
	ret = cons = alloc_empty_cons_pair ();

      cons->value_ptr.cons_pair->car = alloc_empty_cons_pair ();
      cons->value_ptr.cons_pair->car->value_ptr.cons_pair->car = r->name;
      add_reference (CAR (cons), CAR (CAR (cons)), 0);
      cons->value_ptr.cons_pair->car->value_ptr.cons_pair->cdr = r->restart;
      add_reference (CAR (cons), CDR (CAR (cons)), 1);

      r = r->next;
    }

  if (ret != &nil_object)
    cons->value_ptr.cons_pair->cdr = &nil_object;

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
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
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
      return raise_type_error (CAR (list), "(CL:OR CL:SYMBOL CL:FUNCTION)", env,
			       outcome);
    }

  if (!b && SYMBOL (CAR (list)) == env->abort_sym)
    {
      outcome->type = ABORT_TO_TOP_LEVEL;
      return NULL;
    }

  if (!b)
    {
      outcome->type = RESTART_NOT_FOUND;
      return NULL;
    }

  return call_function (fun, CDR (list), 0, 0, 1, 0, 0, env, outcome);
}


struct object *
evaluate_unwind_protect (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  struct object *res, *clres, *obj, *pack;
  struct object_list *ov;
  enum outcome_type type;
  int nov;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  res = evaluate_object (CAR (list), env, outcome);
  nov = outcome->no_value;
  ov = outcome->other_values;
  type = outcome->type;
  obj = outcome->obj;
  pack = outcome->pack;
  outcome->other_values = NULL;

  clres = evaluate_body (CDR (list), -1, 0, 0, NULL, env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!clres)
    {
      decrement_refcount (res);
      return NULL;
    }

  decrement_refcount (clres);

  outcome->no_value = nov;
  outcome->other_values = ov;
  outcome->type = type;
  outcome->obj = obj;
  outcome->pack = pack;

  return res;
}


struct object *
builtin_signal (struct object *list, struct environment *env,
		struct outcome *outcome)
{
  struct object *cond, *ret, *bos;
  int res;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      cond = create_condition_by_c_string ("SIMPLE-CONDITION", &nil_object, env,
					   outcome);
      cond->value_ptr.standard_object->fields->value = CAR (list);
      increment_refcount (CAR (list));
      cond->value_ptr.standard_object->fields->next->value = CDR (list);
      increment_refcount (CDR (list));
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      if (!SYMBOL (CAR (list))->value_ptr.symbol->typespec
	  || SYMBOL (CAR (list))->value_ptr.symbol->typespec->type
	  != TYPE_STANDARD_CLASS
	  || !SYMBOL (CAR (list))->value_ptr.symbol->typespec->
	  value_ptr.standard_class->is_condition_class)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      cond = create_condition (SYMBOL (CAR (list))->value_ptr.symbol->typespec,
			       CDR (list), env, outcome);

      if (!cond)
	return NULL;
    }
  else if (CAR (list)->type == TYPE_STANDARD_OBJECT &&
	   is_subtype_by_char_vector (CAR (list)->value_ptr.standard_object->class,
				      "CONDITION", env))
    {
      cond = CAR (list);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }


  if (!IS_SYMBOL (bos = inspect_variable (env->break_on_signals_sym, env)))
    {
      outcome->type = WRONG_TYPE_OF_STANDARD_VARIABLE;
      return NULL;
    }

  res = check_type (cond, bos, env, outcome);

  if (res < 0)
    return NULL;

  if (res)
    {
      if (!enter_debugger (NULL, env, outcome))
	return NULL;
    }


  ret = handle_condition (cond, env, outcome);

  if (CAR (list)->type != TYPE_STANDARD_OBJECT)
    decrement_refcount (cond);

  if (!ret)
    return NULL;

  return &nil_object;
}


struct object *
builtin_error (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  struct object *cond, *ret, *bos;
  int res;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      cond = create_condition_by_c_string ("SIMPLE-ERROR", &nil_object, env,
					   outcome);
      cond->value_ptr.standard_object->fields->value = CAR (list);
      increment_refcount (CAR (list));
      cond->value_ptr.standard_object->fields->next->value = CDR (list);
      increment_refcount (CDR (list));
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      if (!SYMBOL (CAR (list))->value_ptr.symbol->typespec
	  || SYMBOL (CAR (list))->value_ptr.symbol->typespec->type
	  != TYPE_STANDARD_CLASS
	  || !SYMBOL (CAR (list))->value_ptr.symbol->typespec->
	  value_ptr.standard_class->is_condition_class)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      cond = create_condition (SYMBOL (CAR (list))->value_ptr.symbol->typespec,
			       CDR (list), env, outcome);

      if (!cond)
	return NULL;
    }
  else if (CAR (list)->type == TYPE_STANDARD_OBJECT &&
	   is_subtype_by_char_vector (CAR (list)->value_ptr.standard_object->class,
				      "CONDITION", env))
    {
      increment_refcount (CAR (list));
      cond = CAR (list);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }


  if (!IS_SYMBOL (bos = inspect_variable (env->break_on_signals_sym, env)))
    {
      outcome->type = WRONG_TYPE_OF_STANDARD_VARIABLE;
      return NULL;
    }

  res = check_type (cond, bos, env, outcome);

  if (res < 0)
    return NULL;

  if (res)
    {
      if (!enter_debugger (NULL, env, outcome))
	return NULL;
    }


  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      decrement_refcount (cond);
      return NULL;
    }

  ret = enter_debugger (cond, env, outcome);

  decrement_refcount (cond);

  return ret;
}


struct object *
builtin_warn (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  struct object *cond, *ret, *bos;
  int res;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (CAR (list)->type == TYPE_STRING)
    {
      cond = create_condition_by_c_string ("SIMPLE-WARNING", &nil_object, env,
					   outcome);
      cond->value_ptr.standard_object->fields->value = CAR (list);
      increment_refcount (CAR (list));
      cond->value_ptr.standard_object->fields->next->value = CDR (list);
      increment_refcount (CDR (list));
    }
  else if (IS_SYMBOL (CAR (list)))
    {
      if (!SYMBOL (CAR (list))->value_ptr.symbol->typespec
	  || SYMBOL (CAR (list))->value_ptr.symbol->typespec->type
	  != TYPE_STANDARD_CLASS
	  || !SYMBOL (CAR (list))->value_ptr.symbol->typespec->
	  value_ptr.standard_class->is_condition_class)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      cond = create_condition (SYMBOL (CAR (list))->value_ptr.symbol->typespec,
			       CDR (list), env, outcome);

      if (!cond)
	return NULL;
    }
  else if (CAR (list)->type == TYPE_STANDARD_OBJECT &&
	   is_subtype_by_char_vector (CAR (list)->value_ptr.standard_object->class,
				      "CONDITION", env))
    {
      increment_refcount (CAR (list));
      cond = CAR (list);
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }


  if (!IS_SYMBOL (bos = inspect_variable (env->break_on_signals_sym, env)))
    {
      outcome->type = WRONG_TYPE_OF_STANDARD_VARIABLE;
      return NULL;
    }

  res = check_type (cond, bos, env, outcome);

  if (res < 0)
    return NULL;

  if (res)
    {
      if (!enter_debugger (NULL, env, outcome))
	return NULL;
    }


  ret = handle_condition (cond, env, outcome);

  if (!ret)
    {
      decrement_refcount (cond);
      return NULL;
    }

  if (is_subtype_by_char_vector (cond->value_ptr.standard_object->class,
				 "SIMPLE-WARNING", env))
    {
      printf ("emitted ");
      print_object (cond->value_ptr.standard_object->class->
		    value_ptr.standard_class->name, env,
		    env->c_stdout->value_ptr.stream);
      printf (": ");
      print_object (cond->value_ptr.standard_object->fields->value, env,
		    env->c_stdout->value_ptr.stream);
      printf ("\n");
      env->c_stdout->value_ptr.stream->dirty_line = 0;
    }
  else if (is_subtype_by_char_vector (cond->value_ptr.standard_object->class,
				      "WARNING", env))
    {
      printf ("emitted ");
      print_object (cond->value_ptr.standard_object->class->
		    value_ptr.standard_class->name, env,
		    env->c_stdout->value_ptr.stream);
      printf ("\n");
      env->c_stdout->value_ptr.stream->dirty_line = 0;
    }

  decrement_refcount (cond);

  return ret;
}


struct object *
evaluate_define_condition (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  struct object *condcl;

  if (list_length (list) < 3)
    {
      return raise_al_wrong_number_of_arguments (3, -1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)))
    {
      return raise_type_error (CAR (list), "CL:SYMBOL", env, outcome);
    }

  if (!IS_LIST (CAR (CDR (list))))
    {
      return raise_type_error (CAR (CDR (list)), "CL:LIST", env, outcome);
    }

  if (!IS_LIST (CAR (CDR (CDR (list)))))
    {
      return raise_type_error (CAR (CDR (CDR (list))), "CL:LIST", env, outcome);
    }

  condcl = define_class (SYMBOL (CAR (list)), CDR (list), 1, env, outcome);
  decrement_refcount (condcl);

  increment_refcount (SYMBOL (CAR (list)));
  return SYMBOL (CAR (list));
}


struct object *
builtin_make_condition (struct object *list, struct environment *env,
			struct outcome *outcome)
{
  struct object *ret, *class;
  struct standard_object *so;

  if (!list_length (list))
    {
      return raise_al_wrong_number_of_arguments (1, -1, env, outcome);
    }

  if (!IS_SYMBOL (CAR (list)) || !SYMBOL (CAR (list))->value_ptr.symbol->is_type
      || !is_subtype_by_char_vector (SYMBOL (CAR (list)), "CONDITION", env))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  class = SYMBOL (CAR (list))->value_ptr.symbol->typespec;

  if (!class->value_ptr.standard_class->class_precedence_list
      && !compute_class_precedence_list (class, outcome))
    {
      return NULL;
    }

  ret = alloc_object ();
  ret->type = TYPE_STANDARD_OBJECT;

  so = malloc_and_check (sizeof (*so));
  so->class = class;
  increment_refcount (class);
  so->fields = NULL;
  ret->value_ptr.standard_object = so;

  allocate_object_fields (ret, class);

  return fill_object_fields (ret, class, CDR (list), env, outcome);
}


struct object *
evaluate_define_compiler_macro (struct object *list, struct environment *env,
				struct outcome *outcome)
{
  struct object *mac, *sym, *ret;
  struct compiler_macro *cm;

  if (list_length (list) < 2)
    {
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
    }

  if ((!IS_SYMBOL (CAR (list))
       && !(CAR (list)->type == TYPE_CONS_PAIR
	    && list_length (CAR (list)) == 2
	    && SYMBOL (CAR (CAR (list))) == env->setf_sym
	    && IS_SYMBOL (CAR (CDR (CAR (list))))))
      || !IS_LIST (CAR (CDR (list))))
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  sym = IS_SYMBOL (CAR (list)) ? SYMBOL (CAR (list))
    : SYMBOL (CAR (CDR (CAR (list))));

  mac = create_function (CAR (CDR (list)), CDR (CDR (list)), env, outcome, 1, 1);

  if (!mac)
    return NULL;

  mac->type = TYPE_MACRO;

  cm = env->compiler_macros;

  while (cm)
    {
      if (cm->name == sym && cm->is_setf == (CAR (list)->type == TYPE_CONS_PAIR))
	break;

      cm = cm->next;
    }

  if (!cm)
    {
      cm = malloc_and_check (sizeof (*cm));
      cm->name = sym;
      cm->is_setf = CAR (list)->type == TYPE_CONS_PAIR;
      cm->next = env->compiler_macros;
      env->compiler_macros = cm;
    }
  else
    decrement_refcount (cm->macro);

  cm->macro = mac;

  if (CAR (list)->type == TYPE_CONS_PAIR)
    {
      ret = alloc_empty_cons_pair ();

      ret->value_ptr.cons_pair->car = env->setf_sym;
      add_reference (ret, CAR (ret), 0);

      ret->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
      ret->value_ptr.cons_pair->cdr->value_ptr.cons_pair->cdr = &nil_object;

      ret->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = sym;
      add_reference (CDR (ret), CAR (CDR (ret)), 0);

      return ret;
    }

  increment_refcount (sym);
  return sym;
}


struct object *
builtin_room (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  struct object *ret, *cons;

  if (list_length (list) > 1)
    {
      return raise_al_wrong_number_of_arguments (0, 1, env, outcome);
    }

  cons = ret = alloc_empty_list (7);

  cons->value_ptr.cons_pair->car = create_room_pair ("T", num_objects, env);
  cons = CDR (cons);

  cons->value_ptr.cons_pair->car = create_room_pair ("SYMBOL", num_symbols, env);
  cons = CDR (cons);

  cons->value_ptr.cons_pair->car = create_room_pair ("NUMBER", num_numbers, env);
  cons = CDR (cons);

  cons->value_ptr.cons_pair->car = create_room_pair ("CONS", num_conses, env);
  cons = CDR (cons);

  cons->value_ptr.cons_pair->car = create_room_pair ("STRING", num_strings, env);
  cons = CDR (cons);

  cons->value_ptr.cons_pair->car = create_room_pair ("ARRAY", num_arrays, env);
  cons = CDR (cons);

  cons->value_ptr.cons_pair->car = create_room_pair ("FUNCTION", num_functions,
						     env);

  return ret;
}


struct object *
builtin_trace (struct object *list, struct environment *env,
	       struct outcome *outcome)
{
  struct object *ret = &nil_object, *cons, *fun;
  struct object_list *l;

  if (SYMBOL (list) == &nil_object)
    {
      l = env->traced_funcs;

      while (l)
	{
	  cons = alloc_empty_cons_pair ();
	  cons->value_ptr.cons_pair->car = l->obj;
	  add_reference (cons, l->obj, 0);

	  cons->value_ptr.cons_pair->cdr = ret;
	  ret = cons;

	  l = l->next;
	}

      return ret;
    }

  while (SYMBOL (list) != &nil_object)
    {
      if ((!IS_SYMBOL (CAR (list)) && !(CAR (list)->type == TYPE_CONS_PAIR
					&& list_length (CAR (list)) == 2
					&& SYMBOL (CAR (CAR (list))) == env->setf_sym
					&& IS_SYMBOL (CAR (CDR (CAR (list))))))
	  || !(fun = get_function (CAR (list)->type == TYPE_CONS_PAIR
				   ? SYMBOL (CAR (CDR (CAR (list))))
				   : SYMBOL (CAR (list)), env, 1,
				   CAR (list)->type == TYPE_CONS_PAIR, 0, 0)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (!(fun->value_ptr.function->flags & TRACED_FUNCTION))
	{
	  fun->value_ptr.function->flags |= TRACED_FUNCTION;

	  l = malloc_and_check (sizeof (*l));
	  l->obj = fun;
	  l->next = env->traced_funcs;
	  env->traced_funcs = l;
	}

      list = CDR (list);
    }

  return &t_object;
}


struct object *
builtin_untrace (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  struct object *fun;
  struct object_list *l, *prev = NULL;

  if (SYMBOL (list) == &nil_object)
    {
      while (env->traced_funcs)
	{
	  l = env->traced_funcs->next;
	  env->traced_funcs->obj->value_ptr.function->flags &= ~TRACED_FUNCTION;
	  free (env->traced_funcs);
	  env->traced_funcs = l;
	}

      return &t_object;
    }

  while (SYMBOL (list) != &nil_object)
    {
      if ((!IS_SYMBOL (CAR (list)) && !(CAR (list)->type == TYPE_CONS_PAIR
					&& list_length (CAR (list)) == 2
					&& SYMBOL (CAR (CAR (list))) == env->setf_sym
					&& IS_SYMBOL (CAR (CDR (CAR (list))))))
	  || !(fun = get_function (CAR (list)->type == TYPE_CONS_PAIR
				   ? SYMBOL (CAR (CDR (CAR (list))))
				   : SYMBOL (CAR (list)), env, 1,
				   CAR (list)->type == TYPE_CONS_PAIR, 0, 0)))
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (fun->value_ptr.function->flags & TRACED_FUNCTION)
	{
	  fun->value_ptr.function->flags &= ~TRACED_FUNCTION;

	  l = env->traced_funcs;

	  while (l)
	    {
	      if (l->obj == fun)
		{
		  if (prev)
		    prev->next = l->next;
		  else
		    env->traced_funcs = l->next;

		  free (l);
		  break;
		}

	      prev = l;
	      l = l->next;
	    }
	}

      list = CDR (list);
    }

  return &t_object;
}


struct object *
builtin_invoke_debugger (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STANDARD_OBJECT
      && !CAR (list)->value_ptr.standard_object->class->
      value_ptr.standard_class->is_condition_class)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return enter_debugger (CAR (list), env, outcome);
}


struct object *
builtin_step (struct object *list, struct environment *env,
	      struct outcome *outcome)
{
  struct object *ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  env->stepping_flags = STEP_INSIDE_FORM;

  ret = evaluate_object (CAR (list), env, outcome);

  env->stepping_flags = 0;

  return ret;
}


struct object *
evaluate_al_loopy_destructuring_bind (struct object *list,
				      struct environment *env,
				      struct outcome *outcome)
{
  struct binding *bins;
  int binnum, prev_lex_bin_num = env->lex_env_vars_boundary;
  struct object *ret, *vals;

  if (list_length (list) < 2)
    {
      return raise_al_wrong_number_of_arguments (2, -1, env, outcome);
    }

  vals = evaluate_object (CAR (CDR (list)), env, outcome);
  CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

  if (!vals)
    return NULL;

  if (destructure_tree (CAR (list), vals, &bins, &binnum, outcome))
    {
      env->vars = chain_bindings (bins, env->vars, 1, NULL, NULL);
      env->lex_env_vars_boundary += binnum;

      ret = evaluate_body (CDR (CDR (list)), binnum, 0, 0, NULL, env, outcome);

      env->vars = remove_bindings (env->vars, binnum, 1);

      env->lex_env_vars_boundary = prev_lex_bin_num;
    }
  else
    {
      ret = NULL;
    }

  decrement_refcount (vals);

  return ret;
}


struct object *
evaluate_al_loopy_setq (struct object *list, struct environment *env,
			struct outcome *outcome)
{
  struct object *ret = &nil_object, *vals;

  if (list_length (list) % 2)
    {
      outcome->type = ODD_NUMBER_OF_ARGUMENTS;

      return NULL;
    }

  while (SYMBOL (list) != &nil_object)
    {
      decrement_refcount (ret);

      vals = evaluate_object (CAR (CDR (list)), env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      if (!vals)
	return NULL;

      if (!set_values_destructuring (CAR (list), vals, env, outcome))
	return NULL;

      decrement_refcount (vals);

      list = CDR (CDR (list));
    }

  return ret;
}


struct object *
builtin_al_string_input_stream_string (struct object *list,
				       struct environment *env,
				       struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STREAM
      || CAR (list)->value_ptr.stream->type != STRING_STREAM
      || CAR (list)->value_ptr.stream->direction != INPUT_STREAM)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  increment_refcount (CAR (list)->value_ptr.stream->string);
  return CAR (list)->value_ptr.stream->string;
}


struct object *
builtin_al_print_restarts (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  if (list_length (list))
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  print_available_restarts (env, 0, env->c_stdout);
  printf ("\n");

  return &t_object;
}


struct object *
builtin_al_dump_bindings (struct object *list, struct environment *env,
			  struct outcome *outcome)
{
  if (SYMBOL (list) != &nil_object)
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  return dump_bindings (env->vars, env->lex_env_vars_boundary, env);
}


struct object *
builtin_al_dump_function_bindings (struct object *list, struct environment *env,
				   struct outcome *outcome)
{
  if (SYMBOL (list) != &nil_object)
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  return dump_bindings (env->funcs, env->lex_env_funcs_boundary, env);
}


struct object *
builtin_al_dump_captured_env (struct object *list, struct environment *env,
			      struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_FUNCTION)
    {
      return raise_type_error (CAR (list), "CL:FUNCTION", env, outcome);
    }

  return dump_bindings (CAR (list)->value_ptr.function->lex_vars, 0, env);
}


struct object *
builtin_al_dump_methods (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  struct method_list *ml;
  struct object *ret = &nil_object, *cons;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_FUNCTION
      || !(CAR (list)->value_ptr.function->flags & GENERIC_FUNCTION))
    {
      return raise_type_error (CAR (list), "CL:GENERIC-FUNCTION", env, outcome);
    }

  ml = CAR (list)->value_ptr.function->methods;

  while (ml)
    {
      cons = alloc_empty_cons_pair ();
      cons->value_ptr.cons_pair->car = ml->meth;
      add_reference (cons, CAR (cons), 0);
      cons->value_ptr.cons_pair->cdr = ret;
      ret = cons;

      ml = ml->next;
    }

  return ret;
}


struct object *
builtin_al_dump_fields (struct object *list, struct environment *env,
			struct outcome *outcome)
{
  struct object *obj, *ret = &nil_object, *cons;
  struct structure_field *sf;
  struct class_field *of;
  struct class_field_decl *cfd;
  struct condition_field *cf;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  obj = CAR (list);

  if (obj->type == TYPE_STRUCTURE)
    {
      sf = obj->value_ptr.structure->fields;

      while (sf)
	{
	  cons = alloc_empty_cons_pair ();

	  cons->value_ptr.cons_pair->car = create_pair (sf->name, sf->value);
	  cons->value_ptr.cons_pair->cdr = ret;
	  ret = cons;

	  sf = sf->next;
	}
    }
  else if (obj->type == TYPE_STANDARD_OBJECT)
    {
      of = obj->value_ptr.standard_object->fields;

      while (of)
	{
	  cons = alloc_empty_cons_pair ();

	  if (of->value)
	    {
	      cons->value_ptr.cons_pair->car =
		create_pair (of->decl->name,
			     of->name ? of->value : of->decl->value);
	    }
	  else
	    {
	      cons->value_ptr.cons_pair->car = of->decl->name;
	      add_reference (cons, CAR (cons), 0);
	    }

	  cons->value_ptr.cons_pair->cdr = ret;
	  ret = cons;

	  of = of->next;
	}
    }
  else if (obj->type == TYPE_CONDITION)
    {
      cf = obj->value_ptr.condition->fields;

      while (cf)
	{
	  cons = alloc_empty_cons_pair ();

	  cons->value_ptr.cons_pair->car = create_pair (cf->name, cf->value);
	  cons->value_ptr.cons_pair->cdr = ret;
	  ret = cons;

	  cf = cf->next;
	}
    }
  else if (obj->type == TYPE_STANDARD_CLASS)
    {
      cfd = obj->value_ptr.standard_class->fields;

      while (cfd)
	{
	  cons = alloc_empty_cons_pair ();

	  cons->value_ptr.cons_pair->car = cfd->name;
	  add_reference (cons, cfd->name, 0);

	  cons->value_ptr.cons_pair->cdr = ret;
	  ret = cons;

	  cfd = cfd->next;
	}
    }
  else
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  return ret;
}


struct object *
builtin_al_class_precedence_list (struct object *list, struct environment *env,
				  struct outcome *outcome)
{
  struct object_list *l;
  struct object *ret, *cons;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STANDARD_CLASS)
    {
      outcome->type = WRONG_TYPE_OF_ARGUMENT;
      return NULL;
    }

  if (!CAR (list)->value_ptr.standard_class->class_precedence_list)
    {
      if (!compute_class_precedence_list (CAR (list), outcome))
	return NULL;
    }

  l = CAR (list)->value_ptr.standard_class->class_precedence_list;

  ret = cons = alloc_empty_cons_pair ();
  ret->value_ptr.cons_pair->car = l->obj;
  add_reference (ret, CAR (ret), 0);

  l = l->next;

  while (l)
    {
      cons = cons->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
      cons->value_ptr.cons_pair->car = l->obj;
      add_reference (cons, CAR (cons), 0);

      l = l->next;
    }

  cons->value_ptr.cons_pair->cdr = &nil_object;

  return ret;
}


struct object *
builtin_al_start_profiling (struct object *list, struct environment *env,
			    struct outcome *outcome)
{
  int i;

  if (SYMBOL (list) != &nil_object)
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  if (!env->profiling_data)
    {
      env->profiling_data = malloc_and_check (PROFILING_HASHTABLE_SIZE
					      * sizeof (*env->profiling_data));

      for (i = 0; i < PROFILING_HASHTABLE_SIZE; i++)
	env->profiling_data [i] = NULL;
    }

  env->is_profiling = 1;

  return &t_object;
}


struct object *
builtin_al_stop_profiling (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  if (SYMBOL (list) != &nil_object)
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  env->is_profiling = 0;

  return &t_object;
}


struct object *
builtin_al_clear_profiling (struct object *list, struct environment *env,
			    struct outcome *outcome)
{
  struct profiling_record *r, *n;
  int i;

  if (SYMBOL (list) != &nil_object)
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  if (env->profiling_data)
    {
      for (i = 0; i < PROFILING_HASHTABLE_SIZE; i++)
	{
	  r = env->profiling_data [i];

	  while (r)
	    {
	      n = r->next;
	      free (r);
	      r = n;
	    }
	}

      free (env->profiling_data);
      env->profiling_data = NULL;
    }

  return &t_object;
}


struct object *
builtin_al_report_profiling (struct object *list, struct environment *env,
			     struct outcome *outcome)
{
  struct object *ret = &nil_object, *cons, *car;
  struct profiling_record *r;
  int i;

  if (SYMBOL (list) != &nil_object)
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  if (!env->profiling_data)
    return ret;

  for (i = 0; i < PROFILING_HASHTABLE_SIZE; i++)
    {
      r = env->profiling_data [i];

      while (r)
	{
	  cons = alloc_empty_cons_pair ();
	  cons->value_ptr.cons_pair->cdr = ret;
	  ret = cons;

	  car = alloc_empty_cons_pair ();
	  cons->value_ptr.cons_pair->car = car;

	  if (r->is_setf)
	    {
	      car->value_ptr.cons_pair->car = alloc_empty_list (2);
	      car->value_ptr.cons_pair->car->value_ptr.cons_pair->car =
		env->setf_sym;
	      add_reference (CAR (car), CAR (CAR (car)), 0);
	      car->value_ptr.cons_pair->car->value_ptr.cons_pair->cdr->
		value_ptr.cons_pair->car = r->name;
	      add_reference (CDR (CAR (car)), r->name, 0);
	    }
	  else
	    {
	      car->value_ptr.cons_pair->car = r->name;
	      add_reference (car, r->name, 0);
	    }

	  car->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	  car = CDR (car);
	  car->value_ptr.cons_pair->car =
	    create_integer_from_unsigned_long (r->counter);
	  car->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	  car = CDR (car);
	  car->value_ptr.cons_pair->car =
	    create_integer_from_unsigned_long (r->time);
	  car->value_ptr.cons_pair->cdr = alloc_empty_cons_pair ();
	  car = CDR (car);
	  car->value_ptr.cons_pair->car =
	    create_floating_from_double ((double)r->time / r->counter);
	  car->value_ptr.cons_pair->cdr = &nil_object;

	  r = r->next;
	}
    }

  return ret;
}


struct object *
builtin_al_print_backtrace (struct object *list, struct environment *env,
			    struct outcome *outcome)
{
  int l = list_length (list);

  if (l > 1)
    {
      return raise_al_wrong_number_of_arguments (0, 1, env, outcome);
    }

  print_backtrace (env, l && SYMBOL (CAR (list)) != &nil_object);

  return &t_object;
}


struct object *
builtin_al_list_backtrace (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  int l = list_length (list);

  if (l > 1)
    {
      return raise_al_wrong_number_of_arguments (0, 1, env, outcome);
    }

  return list_backtrace (env, l && SYMBOL (CAR (list)) != &nil_object);
}


struct object *
builtin_al_watch (struct object *list, struct environment *env,
		  struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  SET_WATCHED_FLAG (CAR (list));

  if (CAR (list)->type == TYPE_STANDARD_OBJECT
      || CAR (list)->type == TYPE_HASHTABLE)
    return &t_object;

  return &nil_object;
}


struct object *
builtin_al_unwatch (struct object *list, struct environment *env,
		    struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  CLEAR_WATCHED_FLAG (CAR (list));

  return &t_object;
}


struct object *
builtin_al_next (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (IS_PREFIX (CAR (list)->type))
    {
      increment_refcount (CAR (list)->value_ptr.next);
      return CAR (list)->value_ptr.next;
    }

  outcome->type = WRONG_TYPE_OF_ARGUMENT;
  return NULL;
}


struct object *
builtin_al_compile_form (struct object *list, struct environment *env,
			 struct outcome *outcome)
{
  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  return compile_form (CAR (list), 0, env, outcome);
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
builtin_al_list_directory (struct object *list, struct environment *env,
			   struct outcome *outcome)
{
  struct object *ns, *ret = &nil_object, *cons;
  DIR *d;
  struct dirent *ent;
  char *fn;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = inspect_pathname_by_designator (CAR (list));

  fn = copy_string_to_c_string (ns->value_ptr.string);

  d = opendir (fn);

  free (fn);

  if (!d)
    {
      outcome->type = COULD_NOT_OPEN_DIR;
      return NULL;
    }

  errno = 0;

  while ((ent = readdir (d)))
    {
      cons = alloc_empty_cons_pair ();
      cons->value_ptr.cons_pair->car = create_string_copying_c_string
	(ent->d_name);
      cons->value_ptr.cons_pair->cdr = ret;
      ret = cons;
    }

  if (errno)
    {
      outcome->type = ERROR_READING_DIR;
      return NULL;
    }

  closedir (d);

  return ret;
}


struct object *
builtin_al_directoryp (struct object *list, struct environment *env,
		       struct outcome *outcome)
{
  struct object *ns;
  struct stat st;
  char *fn;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (!IS_PATHNAME_DESIGNATOR (CAR (list)))
    {
      return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:FILE-STREAM "
			       "CL:PATHNAME)", env, outcome);
    }

  ns = inspect_pathname_by_designator (CAR (list));

  fn = copy_string_to_c_string (ns->value_ptr.string);

  if (stat (fn, &st))
    {
      free (fn);
      outcome->type = ERROR_PERFORMING_STAT_ON_FILE;
      return NULL;
    }

  free (fn);

  if ((st.st_mode & S_IFMT) == S_IFDIR)
    return &t_object;

  return &nil_object;
}


struct object *
builtin_al_getcwd (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  char *buf = NULL, *res;
  int sz = 64, len;
  struct object *ret;

  if (list_length (list))
    {
      return raise_al_wrong_number_of_arguments (0, 0, env, outcome);
    }

  do
    {
      free (buf);
      sz *= 2;
      buf = malloc_and_check (sz);

      res = getcwd (buf, sz);
    } while (!res && errno == ERANGE);

  if (!res)
    {
      outcome->type = COULD_NOT_DETERMINE_CWD;
      return NULL;
    }

  len = strlen (buf);

  if (buf [len-1] != '/')
    {
      if (len == sz-1)
	{
	  buf = realloc_and_check (buf, sz+1);
	  sz++;
	}

      buf [len] = '/';
      buf [len+1] = 0;
      len++;
    }

  ret = create_string_with_char_vector (buf, len);
  ret->value_ptr.string->fill_pointer = -1;

  return ret;
}


struct object *
builtin_al_getenv (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  char *envvar, *envval;
  struct object *ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (CAR (list)->type != TYPE_STRING)
    {
      return raise_type_error (CAR (list), "CL:STRING", env, outcome);
    }

  envvar = copy_string_to_c_string (CAR (list)->value_ptr.string);

  envval = getenv (envvar);

  free (envvar);

  if (!envval)
    return &nil_object;

  ret = create_string_copying_c_string (envval);

  return ret;
}


struct object *
builtin_al_system (struct object *list, struct environment *env,
		   struct outcome *outcome)
{
  char *com;
  int ret;

  if (list_length (list) != 1)
    {
      return raise_al_wrong_number_of_arguments (1, 1, env, outcome);
    }

  if (SYMBOL (CAR (list)) == &nil_object)
    {
      if (system (NULL))
	return &t_object;

      return &nil_object;
    }
  else if (CAR (list)->type == TYPE_STRING)
    {
      com = copy_string_to_c_string (CAR (list)->value_ptr.string);

      ret = system (com);

      free (com);

      return create_integer_from_long (ret);
    }

  return raise_type_error (CAR (list), "(CL:OR CL:STRING CL:NULL)", env, outcome);
}


struct object *
builtin_al_exit (struct object *list, struct environment *env,
		 struct outcome *outcome)
{
  int val, l = list_length (list);

  if (l > 1)
    {
      return raise_al_wrong_number_of_arguments (0, 1, env, outcome);
    }

  if (l && CAR (list)->type != TYPE_INTEGER)
    {
      return raise_type_error (CAR (list), "CL:INTEGER", env, outcome);
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
  struct structure_field *f1, *f2;

  if (eq_objects (obj1, obj2) == &t_object)
    return &t_object;

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

  if (obj1->type == TYPE_STRUCTURE && obj2->type == TYPE_STRUCTURE)
    {
      if (obj1->value_ptr.structure->class_name
	  != obj2->value_ptr.structure->class_name)
	return &nil_object;

      f1 = obj1->value_ptr.structure->fields;
      f2 = obj2->value_ptr.structure->fields;

      while (f1)
	{
	  if (equalp_objects (f1->value, f2->value) == &nil_object)
	    return &nil_object;

	  f1 = f1->next;
	  f2 = f2->next;
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
  struct refcounted_object_list *l;

  if (stream->type == BROADCAST_STREAM)
    {
      l = stream->broadcast_to;

      while (l)
	{
	  if (write_to_stream (l->obj->value_ptr.stream, str, size) < 0)
	    return -1;

	  l = l->next;
	}

      return 0;
    }
  else if (stream->type == FILE_STREAM)
    {
      if (fwrite (str, 1, size, stream->file) < size)
	return -1;

      if (str [size-1] == '\n')
	stream->dirty_line = 0;
      else
	stream->dirty_line = 1;

      return 0;
    }
  else
    {
      s = stream->string->value_ptr.string;

      if (s->fill_pointer < 0)
	{
	  resize_string_allocation (stream->string, s->used_size + size);

	  for (i = 0; i < size; i++)
	    s->value [s->used_size + i] = str [i];

	  s->used_size = s->alloc_size;
	}
      else
	{
	  increment_string_allocation_respecting_fill_pointer (stream->string,
							       size);

	  for (i = 0; i < size; i++)
	    s->value [s->fill_pointer + i] = str [i];

	  s->fill_pointer = s->used_size;
	}

      if (str [size-1] == '\n')
	stream->dirty_line = 0;
      else
	stream->dirty_line = 1;

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

  if (stream->type == STRING_STREAM)
    {
      if (stream->string->value_ptr.string->fill_pointer < 0)
	resize_string_allocation (stream->string,
				  stream->string->value_ptr.string->used_size+size);
      else
	increment_string_allocation_respecting_fill_pointer (stream->string,
							     size);
    }

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


struct object *
resolve_synonym_stream (struct object *str, struct environment *env,
			struct outcome *outcome)
{
  while (1)
    {
      str = inspect_variable (str->value_ptr.stream->synonym_of, env);

      if (!str || str->type != TYPE_STREAM)
	{
	  outcome->type = WRONG_TYPE_OF_ARGUMENT;
	  return NULL;
	}

      if (str->value_ptr.stream->type != SYNONYM_STREAM)
	return str;
    }
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

  if (is_number (sym, len, 10, &t, &ne, &emp, &te) || !len)
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

	      if (str->type == FILE_STREAM)
		break;

	      sz += 2;
	    }

	  if (sym [i] == '|' || sym [i] == '\\')
	    sz++;
	}
    }

  if (str->type == STRING_STREAM)
    {
      if (str->string->value_ptr.string->fill_pointer < 0)
	resize_string_allocation (str->string,
				  str->string->value_ptr.string->used_size+sz);
      else
	increment_string_allocation_respecting_fill_pointer (str->string, sz);
    }

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
    *home = sym->value_ptr.symbol->home_package,
    *patc = inspect_variable (env->al_print_always_two_colons, env);

  if (pesc)
    {
      if (home == &nil_object)
	{
	  if (inspect_variable (env->print_gensym_sym, env) != &nil_object
	      && write_to_stream (str, "#:", 2) < 0)
	    return -1;
	}
      else if (home == env->keyword_package && write_to_stream (str, ":", 1) < 0)
	return -1;
      else if (home != &nil_object && home != env->keyword_package
	       && !inspect_accessible_symbol (sym, pack, &ispres))
	{
	  print_as_symbol (home->value_ptr.package->name,
			   home->value_ptr.package->name_len, pesc, str);

	  if (is_external_in_home_package (sym) && SYMBOL (patc) == &nil_object)
	    {
	      if (write_to_stream (str, ":", 1) < 0)
		return -1;
	    }
	  else if (write_to_stream (str, "::", 2) < 0)
	    return -1;
	}
    }

  return print_as_symbol (sym->value_ptr.symbol->name,
			  sym->value_ptr.symbol->name_len, pesc, str);
}


int
print_bignum (const mpz_t z, struct environment *env, struct stream *str)
{
  char *out;
  int ret, base = get_print_base (env);
  struct object *rad = inspect_variable (env->print_radix_sym, env);

  if (base == 8)
    {
      if (rad != &nil_object && write_to_stream (str, "#o", 2) < 0)
	return -1;

      gmp_asprintf (&out, "%Zo", z);
    }
  else if (base == 16)
    {
      if (rad != &nil_object && write_to_stream (str, "#x", 2) < 0)
	return -1;

      gmp_asprintf (&out, "%Zx", z);
    }
  else
    gmp_asprintf (&out, "%Zd", z);

  if (!out)
    {
      fprintf (stderr, "could not allocate memory.  Exiting...\n");
      exit (1);
    }

  ret = write_to_stream (str, out, strlen (out));
  free (out);

  if (ret < 0)
    return -1;

  if (rad != &nil_object && base != 8 && base != 16
      && write_to_stream (str, ".", 1) < 0)
    return -1;

  return 0;
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

  if (!out)
    {
      fprintf (stderr, "could not allocate memory.  Exiting...\n");
      exit (1);
    }

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
print_string (const struct string *s, struct environment *env,
	      struct stream *str)
{
  fixnum i, chars = 0;
  int pesc = is_printer_escaping_enabled (env);

  if (pesc && write_to_stream (str, "\"", 1) < 0)
    return -1;

  for (i = 0; i < s->used_size; i++)
    {
      if (s->fill_pointer >= 0 && i >= s->fill_pointer)
	break;

      if (pesc && (s->value [i] == '"' || s->value [i] == '\\')
	  && write_to_stream (str, "\\", 1) < 0)
	return -1;

      if (write_to_stream (str, &s->value [i], 1) < 0)
	return -1;

      if (IS_LOWEST_BYTE_IN_UTF8 (s->value [i]))
	chars++;
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
      if (obj->value_ptr.function->flags & GENERIC_FUNCTION)
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
      < 0)
    return -1;

  if (obj->value_ptr.method->generic_func->value_ptr.function->is_setf_func)
    {
      if (write_to_stream (str, "(SETF ", strlen ("(SETF ")) < 0)
	return -1;
    }

  if (print_symbol (obj->value_ptr.method->generic_func->
		    value_ptr.function->name, env, str) < 0)
    return -1;

  if (obj->value_ptr.method->generic_func->value_ptr.function->is_setf_func)
    {
      if (write_to_stream (str, ")", strlen (")")) < 0)
	return -1;
    }

  if (obj->value_ptr.method->qualifier == AROUND_METHOD)
    {
      if (write_to_stream (str, " :AROUND ", strlen (" :AROUND ")) < 0)
	return -1;
    }
  else if (obj->value_ptr.method->qualifier == BEFORE_METHOD)
    {
      if (write_to_stream (str, " :BEFORE ", strlen (" :BEFORE ")) < 0)
	return -1;
    }
  else if (obj->value_ptr.method->qualifier == AFTER_METHOD)
    {
      if (write_to_stream (str, " :AFTER ", strlen (" :AFTER ")) < 0)
	return -1;
    }
  else if (write_to_stream (str, " ", strlen (" ")) < 0)
    return -1;

  if (print_specializers_from_lambda_list (obj->value_ptr.method->lambda_list,
					   env, str) < 0)
    return -1;

  if (write_to_stream (str, ">", 1) < 0)
    return -1;

  return 0;
}


int
print_object (const struct object *obj, struct environment *env,
	      struct stream *str)
{
  char *out;
  int ret, base;
  struct object *rad;

  if (obj->type == TYPE_CONS_PAIR
      && obj->value_ptr.cons_pair->car == env->quote_sym
      && WAS_A_READER_MACRO (obj))
    {
      if (write_to_stream (str, "'", 1) < 0)
	return -1;

      return print_object (obj->value_ptr.cons_pair->cdr->value_ptr.cons_pair->
			   car, env, str);
    }
  else if (obj->type == TYPE_CONS_PAIR
      && obj->value_ptr.cons_pair->car == env->function_sym
      && WAS_A_READER_MACRO (obj))
    {
      if (write_to_stream (str, "#'", 2) < 0)
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
	  rad = inspect_variable (env->print_radix_sym, env);

	  if (base == 8)
	    {
	      if (rad != &nil_object && write_to_stream (str, "#o", 2) < 0)
		return -1;

	      gmp_asprintf (&out, "%Qo", obj->value_ptr.ratio);
	    }
	  else if (base == 16)
	    {
	      if (rad != &nil_object && write_to_stream (str, "#x", 2) < 0)
		return -1;

	      gmp_asprintf (&out, "%Qx", obj->value_ptr.ratio);
	    }
	  else
	    {
	      if (rad != &nil_object && write_to_stream (str, "#10r", 4) < 0)
		return -1;

	      gmp_asprintf (&out, "%Qd", obj->value_ptr.ratio);
	    }

	  if (!out)
	    {
	      fprintf (stderr, "could not allocate memory.  Exiting...\n");
	      exit (1);
	    }

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
	return print_string (obj->value_ptr.string, env, str);
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
	{
	  if (write_to_stream (str, "#<STREAM", strlen ("#<STREAM")) < 0)
	    return -1;

	  if (obj->value_ptr.stream->type == FILE_STREAM
	      || obj->value_ptr.stream->type == STRING_STREAM)
	    {
	      if (obj->value_ptr.stream->type == FILE_STREAM
		  && write_to_stream (str, " FILE", strlen (" FILE")) < 0)
		return -1;

	      if (obj->value_ptr.stream->type == STRING_STREAM
		  && write_to_stream (str, " STRING", strlen (" STRING")) < 0)
		return -1;

	      if (obj->value_ptr.stream->direction == INPUT_STREAM
		  && write_to_stream (str, " INPUT", strlen (" INPUT")) < 0)
		return -1;

	      if (obj->value_ptr.stream->direction == OUTPUT_STREAM
		  && write_to_stream (str, " OUTPUT", strlen (" OUTPUT")) < 0)
		return -1;

	      if (obj->value_ptr.stream->direction == BIDIRECTIONAL_STREAM
		  && write_to_stream (str, " BIDIRECTIONAL",
				      strlen (" BIDIRECTIONAL")) < 0)
		return -1;
	    }
	  else if (write_to_stream (str, " ?", strlen (" ?")) < 0)
	    return -1;

	  if (write_to_stream (str, ">", strlen (">")) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_STRUCTURE_CLASS)
	{
	  if (write_to_stream (str, "#<STRUCTURE CLASS ",
			       strlen ("#<STRUCTURE CLASS ")) < 0
	      || print_symbol (obj->value_ptr.structure_class->name, env, str)
	      || write_to_stream (str, ">", 1) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_STRUCTURE)
	{
	  if (write_to_stream (str, "#<STRUCTURE OF CLASS ",
			       strlen ("#<STRUCTURE OF CLASS ")) < 0
	      || print_symbol (obj->value_ptr.structure->class_name, env, str)
	      || write_to_stream (str, ">", 1) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_STANDARD_CLASS)
	{
	  if (write_to_stream (str, "#<STANDARD-CLASS ",
			       strlen ("#<STANDARD-CLASS ")) < 0
	      || print_symbol (obj->value_ptr.standard_class->name, env, str)
	      || write_to_stream (str, ">", 1) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_STANDARD_OBJECT)
	{
	  if (write_to_stream (str, "#<", strlen ("#<")) < 0
	      || print_symbol (obj->value_ptr.standard_object->class->
			       value_ptr.standard_class->name, env, str)
	      || write_to_stream (str, " OBJECT ...>", strlen (" OBJECT ...>")) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_CONDITION_CLASS)
	{
	  if (write_to_stream (str, "#<CONDITION-CLASS ",
			       strlen ("#<CONDITION-CLASS ")) < 0
	      || print_symbol (obj->value_ptr.condition_class->name, env, str)
	      || write_to_stream (str, ">", 1) < 0)
	    return -1;

	  return 0;
	}
      else if (obj->type == TYPE_CONDITION)
	{
	  if (write_to_stream (str, "#<CONDITION OF CLASS ",
			       strlen ("#<CONDITION OF CLASS ")) < 0
	      || print_symbol (obj->value_ptr.condition->class_name, env, str)
	      || write_to_stream (str, ">", 1) < 0)
	    return -1;

	  return 0;
	}
      else
	return write_to_stream (str, "#<print not implemented>",
				strlen ("#<print not implemented>"));
    }
}


int
print_object_nicely (struct object *obj, struct environment *env,
		     struct outcome *outcome, struct object *str)
{
  struct object *disptbl = inspect_variable (env->print_pprint_dispatch_sym, env),
    *func = NULL, *priority, *args, *res;
  int ret;

  if (IS_LIST (disptbl))
    {
      while (SYMBOL (disptbl) != &nil_object)
	{
	  if (CAR (disptbl)->type == TYPE_CONS_PAIR
	      && list_length (CAR (disptbl)) == 3)
	    {
	      ret = check_type (obj, CAR (CAR (disptbl)), env, outcome);

	      if (ret == -1)
		return -1;

	      if (ret == 1)
		{
		  if (!func
		      || compare_two_numbers (priority,
					      CAR (CDR (CDR (CAR (disptbl)))))
		      > 0)
		    {
		      func = CAR (CDR (CAR (disptbl)));
		      priority = CAR (CDR (CDR (CAR (disptbl))));
		    }
		}
	    }

	  disptbl = CDR (disptbl);
	}
    }

  if (func)
    {
      args = alloc_empty_list (2);

      args->value_ptr.cons_pair->car = str;
      add_reference (args, str, 0);

      args->value_ptr.cons_pair->cdr->value_ptr.cons_pair->car = obj;
      add_reference (CDR (args), obj, 0);

      res = call_function (func, args, 0, 0, 1, 0, 0, env, outcome);
      CLEAR_MULTIPLE_OR_NO_VALUES (*outcome);

      decrement_refcount (args);

      if (!res)
	return -1;

      decrement_refcount (res);

      return 0;
    }

  ret = print_object (obj, env, str->value_ptr.stream);

  if (ret < 0)
    outcome->type = ERROR_DURING_OUTPUT;

  return ret;
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
  else if (err->type == SHARP_MACRO_REQUIRES_INTEGER_ARGUMENT)
    {
      printf ("read error: sharp macro requires integer argument\n");
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
  else if (err->type == READ_EVAL_IS_DISABLED)
    {
      printf ("read error: cannot use #. macro because *READ-EVAL* is NIL\n");
    }
  else if (err->type == CANNOT_USE_READ_LABEL_TWICE)
    {
      printf ("read error: cannot use read label twice\n");
    }
  else if (err->type == READ_LABEL_NOT_DEFINED)
    {
      printf ("read error: read label is not defined\n");
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
      printf (" not bound to any value\n");
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
  else if (err->type == WRONG_NUMBER_OF_AXES)
    {
      printf ("eval error: wrong number of axes\n");
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
  else if (err->type == WRONG_TYPE_OF_STANDARD_VARIABLE)
    {
      printf ("eval error: wrong type of a standard variable\n");
    }
  else if (err->type == NO_APPLICABLE_METHOD)
    {
      printf ("eval error: no applicable method found\n");
    }
  else if (err->type == NO_PRIMARY_APPLICABLE_METHOD)
    {
      printf ("eval error: no primary method is applicable\n");
    }
  else if (err->type == NO_NEXT_METHOD)
    {
      printf ("eval error: no next method available\n");
    }
  else if (err->type == COULD_NOT_RENAME_FILE)
    {
      printf ("file error: could not rename file\n");
    }
  else if (err->type == COULD_NOT_DELETE_FILE)
    {
      printf ("file error: could not delete file\n");
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
  else if (err->type == FILE_ALREADY_EXISTS)
    {
      printf ("file error: file already exists\n");
    }
  else if (err->type == ERROR_READING_FILE)
    {
      printf ("file error: could not read file\n");
    }
  else if (err->type == ERROR_DURING_OUTPUT)
    {
      printf ("eval error: there was an error during output\n");
    }
  else if (err->type == ERROR_PERFORMING_STAT_ON_FILE)
    {
      printf ("eval error: there was an error while performing stat on a file\n");
    }
  else if (err->type == COULD_NOT_OPEN_DIR)
    {
      printf ("file error: could not open directory\n");
    }
  else if (err->type == COULD_NOT_CREATE_DIR)
    {
      printf ("file error: could not create directory\n");
    }
  else if (err->type == ERROR_READING_DIR)
    {
      printf ("file error: could not read directory\n");
    }
  else if (err->type == COULD_NOT_DETERMINE_CWD)
    {
      printf ("eval error: could not determine current working directory\n");
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
  else if (err->type == CLASS_ANCESTRY_NOT_CONSISTENT)
    {
      printf ("eval error: class ancestry is not consistent\n");
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
  else if (err->type == CANT_SHADOW_IN_KEYWORD_PACKAGE)
    {
      printf ("eval error: shadowing is not meaningful in keyword package\n");
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
  struct refcounted_object_list *l;
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
      if (obj->value_ptr.stream->type == STRING_STREAM)
	mark_as_constant (obj->value_ptr.stream->string);
      else if (obj->value_ptr.stream->type == FILE_STREAM)
	mark_as_constant (obj->value_ptr.stream->namestring);
      else if (obj->value_ptr.stream->type == SYNONYM_STREAM)
	mark_as_constant (obj->value_ptr.stream->synonym_of);
      else
	{
	  l = obj->value_ptr.stream->broadcast_to;

	  while (l)
	    {
	      mark_as_constant (l->obj);
	      l = l->next;
	    }
	}
    }
}


struct parameter *
parameter_by_index (struct parameter *par, int *ind)
{
  struct parameter *sp;

  while (par && (*ind > 7 || !par->name))
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
  struct refcounted_object_list *l;

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

	  return !ml->reference_strength_factor
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
  else if (src->type == TYPE_STREAM
	   && src->value_ptr.stream->type == BROADCAST_STREAM)
    {
      l = src->value_ptr.stream->broadcast_to;

      while (ind)
	{
	  l = l->next;
	  ind--;
	}

      return !l->reference_strength_factor != !STRENGTH_FACTOR_OF_OBJECT (dest);
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
  struct refcounted_object_list *l;

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
  else if (src->type == TYPE_STREAM
	   && src->value_ptr.stream->type == BROADCAST_STREAM)
    {
      l = src->value_ptr.stream->broadcast_to;

      while (ind)
	{
	  l = l->next;
	  ind--;
	}

      l->reference_strength_factor
	= !new_weakness != !STRENGTH_FACTOR_OF_OBJECT (dest);
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
add_reference_to_object_just_read (struct object *src, struct object *dest,
				   int ind)
{
  if (!DONT_REFCOUNT (dest))
    {
      if (STRONG_REFCOUNT (dest) > 1)
	add_reference (src, dest, ind);
      else
	add_strong_reference (src, dest, ind);

      DEC_STRONG_REFCOUNT (dest);
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
  struct refcounted_object_list *l;

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
      if (node->value_ptr.stream->type == STRING_STREAM)
	{
	  rest_inv_at_edge (node->value_ptr.stream->string, 0);
	}
      else if (node->value_ptr.stream->type == FILE_STREAM)
	{
	  rest_inv_at_edge (node->value_ptr.stream->namestring, 0);
	}
      else if (node->value_ptr.stream->type == SYNONYM_STREAM)
	{
	  rest_inv_at_edge (node->value_ptr.stream->synonym_of, 0);
	}
      else
	{
	  l = node->value_ptr.stream->broadcast_to;
	  i = 0;

	  while (l)
	    {
	      rest_inv_at_edge (l->obj, i++);
	      l = l->next;
	    }
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
  struct refcounted_object_list *n;

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

      num_numbers--;
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

      num_numbers--;
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
	  && obj->value_ptr.stream->type == FILE_STREAM)
	fclose (obj->value_ptr.stream->file);

      if (obj->value_ptr.stream->type == STRING_STREAM)
	delete_reference (obj, obj->value_ptr.stream->string, 0);
      else if (obj->value_ptr.stream->type == FILE_STREAM)
	delete_reference (obj, obj->value_ptr.stream->namestring, 0);
      else if (obj->value_ptr.stream->type == SYNONYM_STREAM)
	delete_reference (obj, obj->value_ptr.stream->synonym_of, 0);
      else
	{
	  while (obj->value_ptr.stream->broadcast_to)
	    {
	      delete_reference (obj, obj->value_ptr.stream->broadcast_to->obj, 0);
	      n = obj->value_ptr.stream->broadcast_to->next;
	      free (obj->value_ptr.stream->broadcast_to);
	      obj->value_ptr.stream->broadcast_to = n;
	    }
	}

      free (obj->value_ptr.stream);
      free (obj);
    }

  num_objects--;
}


void
free_string (struct object *obj)
{
  free (obj->value_ptr.string->value);
  free (obj->value_ptr.string);
  free (obj);

  num_strings--;
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

  num_symbols--;
}


void
free_cons_pair (struct object *obj)
{
  delete_reference (obj, CAR (obj), 0);
  delete_reference (obj, CDR (obj), 1);

  free (obj->value_ptr.cons_pair);
  free (obj);

  num_conses--;
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

  num_arrays--;
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

  num_numbers--;
}


void
free_ratio (struct object *obj)
{
  mpq_clear (obj->value_ptr.ratio);
  free (obj);

  num_numbers--;
}


void
free_float (struct object *obj)
{
  free (obj->value_ptr.floating);
  free (obj);

  num_numbers--;
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
      decrement_refcount (f->value);
      free (f);
      f = n;
    }

  free (obj->value_ptr.structure);
  free (obj);
}


void
free_standard_class (struct object *obj)
{
  struct object_list *p = obj->value_ptr.standard_class->parents, *n, *l;
  struct class_field_decl *f = obj->value_ptr.standard_class->fields, *nf;

  while (p)
    {
      n = p->next;
      free (p);
      p = n;
    }

  p = obj->value_ptr.standard_class->class_precedence_list;

  while (p)
    {
      n = p->next;
      free (p);
      p = n;
    }

  while (f)
    {
      nf = f->next;
      decrement_refcount (f->initform);

      l = f->initargs;

      while (l)
	{
	  n = l->next;
	  decrement_refcount (l->obj);
	  free (l);
	  l = n;
	}

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

  decrement_refcount (obj->value_ptr.standard_object->class);

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
free_lambda_list_content (struct object *obj, struct parameter *par, int *i,
			  int is_method)
{
  while (par)
    {
      if (par->name)
	{
	  delete_reference (obj, par->name, (*i)++);
	  delete_reference (obj, par->init_form, (*i)++);
	  delete_reference (obj, par->supplied_p_param, (*i)++);
	  delete_reference (obj, par->key, (*i)++);

	  decrement_refcount (par->typespec);

	  if (!is_method)
	    *i += 4;
	}
      else
	{
	  free_lambda_list_content (obj, par->sub_lambda_list, i, is_method);
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
free_method_list (struct object *fun, struct method_list *ml, int ind)
{
  if (!ml)
    return;

  free_method_list (fun, ml->next, ind+8);

  delete_reference (fun, ml->meth, ind);

  free (ml);
}


void
free_function_or_macro (struct object *obj)
{
  struct binding *b, *nx;
  int i = 2;

  delete_reference (obj, obj->value_ptr.function->name, 0);
  delete_reference (obj, obj->value_ptr.function->body, 1);

  free_lambda_list_content (obj, obj->value_ptr.function->lambda_list, &i, 0);
  free_lambda_list_structure (obj->value_ptr.function->lambda_list);
  obj->value_ptr.function->lambda_list = NULL;

  free_method_list (obj, obj->value_ptr.function->methods, 6);
  obj->value_ptr.function->methods = NULL;

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

  if (obj->value_ptr.function->encl_blocks)
    remove_block (obj->value_ptr.function->encl_blocks);

  if (obj->value_ptr.function->encl_tags)
    remove_go_tag_frame (obj->value_ptr.function->encl_tags);

  free (obj->value_ptr.function);
  free (obj);

  num_functions--;
}


void
free_method (struct object *obj)
{
  int i = 2;

  delete_reference (obj, obj->value_ptr.method->generic_func, 0);
  delete_reference (obj, obj->value_ptr.method->body, 1);

  free_lambda_list_content (obj, obj->value_ptr.method->lambda_list, &i, 1);
  free_lambda_list_structure (obj->value_ptr.method->lambda_list);
  obj->value_ptr.method->lambda_list = NULL;

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
      num_conses--;
      num_objects--;
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
