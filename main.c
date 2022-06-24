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



#define _GNU_SOURCE


#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include <getopt.h>

#include <readline/readline.h>
#include <readline/history.h>



/* this is a slight abuse of terminology: commas, quotes and
 * backquotes are not Lisp objects.  But by treating them as objects, we
 * can implement them as a linked list before the proper object */


struct
comma
{
  struct object *next;
};


struct
quote
{
  struct object *next;
};


struct
backquote
{
  struct object *next;
};


/* not a C string. not null-terminated and explicit size. null bytes are allowed inside */

struct
string
{
  char *value;
  size_t alloc_size;
  size_t used_size;
};


struct
symbol
{
  char *name;
  size_t name_len;
  struct package *package;  
};


struct
binding
{
  struct symbol *symbol;
  struct object *object;
};


struct
environment
{
  struct binding *bindings;
  size_t alloc_size;
  size_t used_size;
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
  struct string *name;

  struct object *init_form;
  struct string *supplied_p_param;
  
  struct parameter *next;
};


struct
function
{
  struct parameter *lambda_list;
  int allow_other_keys;
  struct object *body;
  size_t body_alloc_size;
  size_t body_used_size;
};


enum
object_type
  {
    TYPE_NIL,
    TYPE_COMMA,
    TYPE_QUOTE,
    TYPE_BACKQUOTE,
    TYPE_SYMBOL,
    TYPE_NUMBER,
    TYPE_CONS_PAIR,
    TYPE_CHARACTER,
    TYPE_STRING,
    TYPE_ARRAY,
    TYPE_HASHTABLE,
    TYPE_ENVIRONMENT,
    TYPE_PACKAGE,
    TYPE_PATHNAME,
    TYPE_STREAM,
    TYPE_STRUCTURE,
    TYPE_CONDITION,
    TYPE_T
  };


struct
cons_pair
{
  int filling_car;  /* this is used when car is a list and we are still reading it */
  struct object *car;
  struct object *cdr;
};


union
object_ptr_union
{
  struct comma *comma;
  struct quote *quote;
  struct backquote *backquote;
  struct symbol *symbol;
  struct cons_pair *cons_pair;
  struct string *string;
  struct environment *environment;
};


struct
object
{
  int quoted;
  int refcount;
  enum object_type type;
  union object_ptr_union value_ptr;  /* only when type is TYPE_NIL, this is allowed to be NULL */
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
    INTERNAL_VISIBLITY,
    EXTERNAL_VISIBILITY
  };


/* a single record in a package */
struct
package_record
{
  struct string symbol_name;
  struct symbol *symbol;
  enum package_record_visibility visibility;
};


struct
package
{
  struct string name;
  struct string *nicks;
  size_t nicks_size;
  struct package_record *records;
  size_t recs_alloc_size;
  size_t recs_used_size;
};


enum
number_type
  {
    NUMBER_REAL,
    NUMBER_COMPLEX
  };


union
number_union
{
  double real;
  double complex [2];
};


struct
number
{
  enum number_type number_type;
  union number_union number;
};


enum
eval_outcome
  {
    EVAL_OK
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
    SEMICOLON,
    DOT,
    TOKEN,
    BEGIN_MULTILINE_COMMENT,
    VERTICAL_BAR,
    SHARP
  };


enum
read_outcome
  {
    NO_OBJECT = 0,
    
    COMPLETE_OBJECT = 1,
    
    CLOSING_PARENTHESIS = 1 << 1,

    INCOMPLETE_LIST = 1 << 2,
    INCOMPLETE_STRING = 1 << 3,
    INCOMPLETE_SYMBOL = 1 << 4,
    
    UNFINISHED_SINGLELINE_COMMENT = 1 << 5,
    UNFINISHED_MULTILINE_COMMENT = 1 << 6
  };



enum read_outcome read_object (struct object **object, const char *input, size_t size, const char **obj_begin, const char **obj_end,
			       int *out_arg);
enum read_outcome read_list (struct object **object, const char *input, size_t size, const char **list_end, int *out_arg);
enum read_outcome read_string (struct object **object, const char *input, size_t size, const char **string_end);
enum read_outcome read_symbol (struct object **object, const char *input, size_t size, const char **symbol_end);
enum read_outcome read_sharp_macro (struct object **object, const char *input, size_t size, const char **macro_end);

enum element find_element (const char *input, size_t size, const char **elem_begin);
const char *find_string_delimiter (const char *input, size_t size);

const char *jump_to_end_of_space_block (const char *input, size_t size, size_t *new_size);
const char *find_multiline_comment_delimiter (const char *input, size_t size, size_t *new_size);
const char *jump_to_end_of_multiline_comment (const char *input, size_t size, size_t *depth_or_new_size);
const char *jump_to_end_of_line (const char *input, size_t size, size_t *new_size);

int is_number (const char *token, const char *end, int radix);
struct object *alloc_number (const char *begin, const char *end, int radix);

void print_range (const char *begin, const char *end);
char *append_newline (char *string);

void *malloc_and_check (size_t size);
void *realloc_and_check (void *ptr, size_t size);

struct object *alloc_empty_string (void);
struct object *alloc_empty_cons_pair (void);
struct object *alloc_environment (size_t size);

char *normalize_symbol_name (const char *input, size_t size, const char **symbol_name_end, size_t *name_size);
struct object *create_symbol (char *name, size_t size);
struct object *intern_symbol (char *name, size_t size, struct object_list **symbols);
void add_binding (struct symbol *sym, struct object *obj, struct environment *env);

struct parameter *parse_lambda_list (const char *input, size_t size);
struct object *call_function (const struct function *func, const struct cons_pair *arglist);

struct object *evaluate_object (struct object *obj, struct environment *env, enum eval_outcome *outcome);

int eqmem (const char *s1, size_t n1, const char *s2, size_t n2);
int equal_strings (const struct string *s1, const struct string *s2);

void print_symbol (const struct symbol *sym);
void print_string (const struct string *str);
void print_list (const struct cons_pair *list);
void print_object (const struct object *obj);

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



int
main (int argc, char *argv [])
{
  int end_repl = 0, input_exhausted = 0;
  int need_another_line = 0;
  char *line;
  const char *begin, *end;

  int list_depth = 0, multiline_comment_depth = 0;
  int input_needs_continuation = 0;

  int out_arg;
  
  struct package current_package;

  struct object *form;

  enum read_outcome out;
  
  
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


  print_welcome_message ();
  
  while (!end_repl)
    {
      line = readline ("al> ");
      line = append_newline (line);
      
      form = NULL;
      out = read_object (&form, line, strlen (line), &begin, &end, &out_arg);
      
      while (out ==  INCOMPLETE_LIST
	     || out == INCOMPLETE_STRING
	     || out == INCOMPLETE_SYMBOL
	     || out == UNFINISHED_SINGLELINE_COMMENT
	     || out == UNFINISHED_MULTILINE_COMMENT)
	{
	  if (line && *line)
	    {
	      line [strlen (line)-1] = 0;
	      add_history (line);
	    }

	  line = readline ("> ");
	  line = append_newline (line);
	  out = read_object (&form, line, strlen (line), &begin, &end, &out_arg);
	}
    }
  
  return 0;
}


const char *
jump_to_end_of_space_block (const char *input, size_t size, size_t *new_size)
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
  
  *new_size = 1;
  return input+size-1;
}


enum read_outcome 
read_object (struct object **object, const char *input, size_t size, const char **obj_begin,
	     const char **obj_end, int *out_arg)
{
  enum read_outcome out = NO_OBJECT;
  int end_loop = 0;
  //const char *list_end;
  struct object *obj;

  obj = NULL;
  
  while (!end_loop)
    {
      input = jump_to_end_of_space_block (input, size, &size);

      if (!input)
	return out;

      if (*input == ')')
	{
	  *obj_end = input;
	  return CLOSING_PARENTHESIS;
	}
      else if (*input == '(')
	{
	  *obj_begin = input;
	  out = read_list (&obj, input+1, size-1, obj_end, out_arg);
	  return out;
	}
      else if (*input == '"')
	{
	  *obj_begin = input;
	  out = read_string (&obj, input+1, size-1, obj_end);
	  return out;
	}
      else if (*input == '\'')
	{
	  
	}
      else if (*input == ';')
	{
	  input = jump_to_end_of_line (input, size, &size);

	  if (!input)
	    return UNFINISHED_SINGLELINE_COMMENT;
	}
    }

  return 0;
}


enum read_outcome 
read_list (struct object **object, const char *input, size_t size, const char **list_end, int *out_arg)
{
  struct object *obj;
  const char *obj_beg, *obj_end;
  enum read_outcome out;

  while (*object)
    {
      if ((*object)->value_ptr.cons_pair->filling_car)
	{
	  out = read_list (&(*object)->value_ptr.cons_pair->car, input, size, list_end, out_arg);
	}

      *object = (*object)->value_ptr.cons_pair->cdr;      
    }
  
  obj = NULL;

  out = read_object (&obj, input, size, &obj_beg, &obj_end, out_arg);

  while (out != NO_OBJECT)
    {
      if (out == CLOSING_PARENTHESIS)
	{
	  *list_end = obj_end;
	  
	  return COMPLETE_OBJECT;
	}
      else if (out == COMPLETE_OBJECT)
	{
	  *object = alloc_empty_cons_pair ();
	  (*object)->value_ptr.cons_pair->car = obj;
	}

      if (obj_end == input + size)
	break;
      
      obj = NULL;
      out = read_object (&obj, obj_end+1, size, &obj_beg, &obj_end, out_arg);
    }

  (*object)->value_ptr.cons_pair->filling_car = 1;
  
  return INCOMPLETE_LIST;
}


enum read_outcome 
read_string (struct object **object, const char *input, size_t size, const char **string_end)
{
  int i, j;
  size_t length;
  struct string *str;
  enum read_outcome out = COMPLETE_OBJECT;
  
  if (!*object)
    {
      *object = alloc_empty_string ();
    }

  str = (*object)->value_ptr.string;
  
  if (*input == '"')
    {
      *string_end = input;
      return out;
    }

  i = length = 1;
  
  while (i < size)
    {
      if (input [i] == '"' && input [i-1] != '\\')
	break;

      if (input [i] != '\\')
	length++;	
    }

  if (i == size)
    out = INCOMPLETE_STRING;

  str->value = realloc_and_check (str->value, str->alloc_size + sizeof (char) * length);

  for (i = 0, j = 0; i < length; i++)
    {
      if (input [i] == '\\')
	i++;
      else
	str->value [j++] = input [i];
    }
  
  return out;
}


enum read_outcome 
read_symbol (struct object **object, const char *input, size_t size, const char **symbol_end)
{
  size_t name_size;
  
  char *name = normalize_symbol_name (input, size, symbol_end, &name_size);
  
  if (!*object)
    {
      *object = malloc_and_check (sizeof (**object));
      (*object)->type = TYPE_SYMBOL;
      (*object)->refcount = 1;
      (*object)->value_ptr.symbol = malloc_and_check (sizeof (struct symbol));
      (*object)->value_ptr.symbol->name_len = 0;
    }

  if ((*object)->value_ptr.symbol->name_len == 0)
    {
      (*object)->value_ptr.symbol->name = name;
      (*object)->value_ptr.symbol->name_len = name_size;
    }
  else
    {
      (*object)->value_ptr.symbol->name = realloc_and_check ((*object)->value_ptr.symbol->name,
							     (*object)->value_ptr.symbol->name_len + name_size);
      (*object)->value_ptr.symbol->name_len = (*object)->value_ptr.symbol->name_len + name_size;
      memcpy ((*object)->value_ptr.symbol->name + (*object)->value_ptr.symbol->name_len,
	      name, name_size);
    }

  if (*symbol_end)
    return COMPLETE_OBJECT;
  else
    return INCOMPLETE_SYMBOL;
}
  

enum read_outcome 
read_sharp_macro (struct object **object, const char *input, size_t size, const char **macro_end)
{
  return 0;
}


enum element
find_element (const char *input, size_t size, const char **elem_begin)
{
  int i = 0;
  int single_escape = 0;
  enum element el = NONE;
  
  while (i < size && el == NONE)
    {
      if (!single_escape)
	{
	  switch (input [i])
	    {
	    case '\\':
	      single_escape = 1;
	      break;
	    case '(':
	      el = BEGIN_LIST;
	      break;
	    case ')':
	      el = END_LIST;
	      break;
	    case '"':
	      el = STRING_DELIMITER;
	      break;
	    case '\'':
	      el = QUOTE;
	      break;
	    case '`':
	      el = BACKQUOTE;
	      break;
	    case ',':
	      el = COMMA;
	      break;
	    case ';':
	      el = SEMICOLON;
	      break;
	    case '.':
	      el = DOT;
	      break;
	    case '#':
	      if (i+1 < size && input [i+1] == '|')
		el = BEGIN_MULTILINE_COMMENT;
	      else
		el = SHARP;
	      break;
	    case '|':
	      el = VERTICAL_BAR;
	      break;
	    default:
	      if (!isspace (input [i]))
		{
		  *elem_begin = input;
		  return TOKEN;
		}
	    }
	}
      else
	{
	  el = TOKEN;
	}

      *elem_begin = input;
      i++;
    }
  
  return el;
}


const char *
find_string_delimiter (const char *input, size_t size)
{
  int i;

  if (size > 0 && *input == '"')
    return input;

  for (i = 1; i < size; i++)
    {
      if (input [i] == '"' && input [i-1] != '\\')
	return input+i;
    }

  return NULL;
}


const char *
find_multiline_comment_delimiter (const char *input, size_t size, size_t *new_size)
{
  const char *comm_begin, *comm_end, *delim;

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
jump_to_end_of_multiline_comment (const char *input, size_t size, size_t *depth_or_new_size)
{
  int comm_depth = 1;
  const char *delim = NULL;

  delim = find_multiline_comment_delimiter (input, size, &size);

  while (delim)
    {
      if (*delim == '#')
	comm_depth++;
      else
	comm_depth--;

      delim = find_multiline_comment_delimiter (delim+1, size-1, &size);
    }

  if (!comm_depth)
    {
      *depth_or_new_size = size-1;
      return delim+1;
    }

  *depth_or_new_size = comm_depth;
  
  return NULL;    
}


const char *
jump_to_end_of_line (const char *input, size_t size, size_t *new_size)
{
  const char *eol =  memmem (input, size, "\n", 1);

  if (!eol)
    return NULL;

  *new_size = size - (sizeof (char) * (eol - input));

  return eol;
}


int
is_number (const char *token, const char *end, int radix)
{
  int response = 1, i = 0;
  int found_decimal_point = 0;
  char *exponent_marker = 0;
  //int found_digit_after_exponent_marker = 0;
  int found_slash = 0;

  char decimal_digits [] = "0123456789";
  char digits [] = "00112233445566778899aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ";
  char exponent_markers [] = "dDeEfFlLsS";

  digits [radix * 2] = 0;
  
  while (token < end && response)
    {
      if (strchr (decimal_digits, *token))
	{

	}
      else if (strchr (digits, *token))
	{

	}
      else if (*token == '+' && *token == '-')
	{
	  if (i > 0 && token - exponent_marker > 0)
	    response = 0;
	}
      else if (*token == '/')
	{
	  if (found_slash)
	    response = 0;
	  else
	    found_slash = 1;
	}
      else if (*token == '.')
	{
	  if (found_decimal_point)
	    response = 0;
	  else
	    found_decimal_point = 1;
	}
      else if ((exponent_marker = strchr (exponent_markers, *token)))
	{
	  if (!found_decimal_point)
	    response = 0;
	}
      
      token++, i++;
    }

  return response;
}


struct object *
alloc_number (const char *begin, const char *end, int radix)
{
  return NULL;
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
alloc_empty_string (void)
{
  struct object *obj = malloc_and_check (sizeof (*obj));

  obj->type = TYPE_STRING;
  obj->refcount = 0;

  obj->value_ptr.string = malloc_and_check (sizeof (*obj->value_ptr.string));
  obj->value_ptr.string->alloc_size = obj->value_ptr.string->used_size = 0;

  return obj;
}


struct object *
alloc_empty_cons_pair (void)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct cons_pair *cons = malloc_and_check (sizeof (*cons));
  struct object *car = malloc_and_check (sizeof (*car));
  
  obj->type = TYPE_CONS_PAIR;
  obj->refcount = 0;
  obj->value_ptr.cons_pair = cons;
  
  car->type = TYPE_NIL;
  
  obj->value_ptr.cons_pair->car = car;
  obj->value_ptr.cons_pair->cdr = NULL;

  return obj;
}


struct object *
alloc_environment (size_t size)
{
  struct object *obj = malloc_and_check (sizeof (*obj));
  struct environment *env = malloc_and_check (sizeof (*env));
  struct binding *bins = malloc_and_check (sizeof (*bins) * size);
  
  obj->type = TYPE_ENVIRONMENT;
  obj->value_ptr.environment = env;

  obj->value_ptr.environment->bindings = bins;
  obj->value_ptr.environment->alloc_size = size;
  obj->value_ptr.environment->used_size = 0;
  
  return obj;
}


char *
normalize_symbol_name (const char *input, size_t size, const char **symbol_name_end, size_t *name_size)
{
  char *name;
  int i = 0, j;
  
  int single_escape = 0;
  int multiple_escape = 0;
    
  while (i < size)
    {
      if (input [i] == '\\')
	{
	  if (!single_escape)
	    {
	      single_escape = 1;
	    }
	  else
	    {
	      single_escape = 0;
	      (*name_size)++;
	    }
	}
      else if (input [i] == '|' && !single_escape)
	{
	  multiple_escape = (multiple_escape ? 0 : 1);
	}
      else
	{
	  if (isspace (input [i]) && !single_escape && !multiple_escape)
	    {
	      *symbol_name_end = input+i;
	      break;
	    }

	  (*name_size)++;
	}
      i++;
    }

  if (i == size || single_escape || multiple_escape)
    {
      *symbol_name_end = NULL;
    }
  
  name = malloc_and_check (*name_size * sizeof (char));

  single_escape = 0;
  multiple_escape = 0;
  
  for (i = 0, j = 0; i < *name_size; i++)
    {
      if (input [i] == '\\')
	{
	  if (!single_escape)
	    {
	      single_escape = 1;
	    }
	  else
	    {
	      name [j++] = '\\';
	      single_escape = 0;
	    }
	}
      else if (input [i] == '|' && !single_escape)
	{
	  multiple_escape = (multiple_escape ? 0 : 1);
	}
      else
	{
	  if (single_escape || multiple_escape)
	    name [j++] = input [i];
	  else
	    name [j++] = toupper (input [i]);
	}
    }

  return name;  
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

  obj->value_ptr.symbol = sym;

  return obj;
}


struct object *
intern_symbol (char *name, size_t size, struct object_list **symbols)
{
  struct object *obj, *sym;
  struct object_list *cur, *new_sym;
  
  if (!*symbols)
    {
      *symbols = malloc_and_check (sizeof (**symbols));
      obj = create_symbol (name, size);
      (*symbols)->obj = obj;
      (*symbols)->next = NULL;

      return obj;
    }
  
  cur = *symbols;

  while (cur)
    {
      if (eqmem (cur->obj->value_ptr.symbol->name, cur->obj->value_ptr.symbol->name_len, name, size))
	return cur->obj;

      cur = cur->next;
    }

  sym = create_symbol (name, size);
  
  new_sym = malloc_and_check (sizeof (*new_sym));
  new_sym->obj = sym;
  new_sym->next = *symbols;

  *symbols = new_sym;

  return sym;  
}


void
add_binding (struct symbol *sym, struct object *obj, struct environment *env)
{
  struct object *b = malloc_and_check (sizeof (*b));

  if (env->alloc_size <= env->used_size)
    {
      env->bindings = realloc_and_check (env->bindings, env->used_size + 1);
      env->alloc_size = env->used_size + 1;
    }

  env->bindings [env->used_size].symbol = sym;
  env->bindings [env->used_size].object = obj;
  env->used_size++;
}


struct parameter *
parse_lambda_list (const char *input, size_t size)
{
  return NULL;
}


struct object *
call_function (const struct function *func, const struct cons_pair *arglist)
{
  return NULL;
}


struct object *
evaluate_object (struct object *obj, struct environment *env, enum eval_outcome *outcome)
{
  if (obj->type == TYPE_T || obj->type == TYPE_NIL ||
      obj->type == TYPE_NUMBER || obj->type == TYPE_CHARACTER ||
      obj->type == TYPE_STRING)
    {
      return obj;
    }
  else if (obj->type == TYPE_QUOTE)
    {
      return obj->value_ptr.quote->next;
    }
  else if (obj->type == TYPE_SYMBOL)
    {
      if (obj->value_ptr.symbol->name [0] == ':')
	return obj;

      for (int i = 0; i < env->used_size; i++)
	{
	  if (eqmem (env->bindings [i].symbol->name, env->bindings [i].symbol->name_len,
		     obj->value_ptr.symbol->name, obj->value_ptr.symbol->name_len))
	    return env->bindings [i].object;
	}

      return NULL;
    }
  
  return NULL;
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
  char need_escape [] = "().,;'#\"\n";
  int do_need_escape = 0;

  /* first we make a pass to understand if we need vertical quotes */
  for (i = 0; i < sym->name_len && !do_need_escape; i++)
    {
      if (strchr (need_escape, nm [i]) || !nm [i])
	do_need_escape = 1;
    }

  if (do_need_escape)
    putchar ('|');

  for (i = 0; i < sym->name_len; i++)
    {
      if (nm [i] == '|' && do_need_escape)
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
      if (str->value [i] == '"')
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
  else if (obj->type == TYPE_STRING)
    print_string (obj->value_ptr.string);
  else if (obj->type == TYPE_SYMBOL)
    print_symbol (obj->value_ptr.symbol);
  else if (obj->type == TYPE_CONS_PAIR)
    print_list (obj->value_ptr.cons_pair);
  else
    printf ("<print not implemented>");
}


int
decrement_refcount (struct object *obj)
{
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
  else if (obj->type == TYPE_SYMBOL)
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
  puts ("al  Copyright (C) 2022 Andrea G. Monaco\n"
	"This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\n"
	"This is free software, and you are welcome to redistribute it\n"
	"under certain conditions; type `show c' for details.\n");
}


void
print_version (void)
{
  puts ("al 0.1\n"
	"Copyright (C) 2022 Andrea G. Monaco\n"
	"License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>\n"
	"This is free software: you are free to change and redistribute it.\n"
	"There is NO WARRANTY, to the extent permitted by law.");
}


void
print_help (void)
{

}
