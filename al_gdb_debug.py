# Copyright (C) 2022 Andrea G. Monaco
# 
# This file is part of alisp, a lisp implementation.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#


import gdb



class AlPrintObject (gdb.Command):
    """Print a lisp object in a nice way and show the refcounts"""

    def __init__ (self):
        super (AlPrintObject, self).__init__ ("al_print_object", gdb.COMMAND_USER)


    def invoke (self, arg, from_tty):
        out = print_object (arg, False)
        gdb.write ("%s\n%s\n" % out)



def print_object (arg, in_cons):
    if in_cons:
        type = gdb.execute ("output %s->value_ptr.cons_pair->car->type" % arg, False, True)
    else:
        type = gdb.execute ("output %s->type" % arg, False, True)

    if type == "TYPE_STRING":
        out = print_string (arg, in_cons)
    elif type == "TYPE_SYMBOL_NAME":
        out = print_symbol_name (arg, in_cons)
    elif type == "TYPE_SYMBOL":
        out = print_symbol (arg, in_cons)
    elif type == "TYPE_CONS_PAIR":
        out = print_cons (arg, in_cons)
    elif type == "TYPE_QUOTE" or type == "TYPE_BACKQUOTE" or type == "TYPE_COMMA" or type == "TYPE_AT" or type == "TYPE_DOT":
        out = print_prefix (arg, type, in_cons)
    else:
        if type == "TYPE_INTEGER":
            obj = "#<INTEGER>"
        elif type == "TYPE_RATIO":
            obj = "#<RATIO>"
        elif type == "TYPE_FLOATING":
            obj = "#<FLOAT>"
        elif type == "TYPE_FUNCTION":
            obj = "#<FUNCTION>"
        elif type == "TYPE_MACRO":
            obj = "#<MACRO>"
        else:
            obj = "???"

        ref = print_refcount (arg, in_cons)

        out = justify_right (obj, ref)


    return out



def print_string (arg, in_cons):
    if in_cons:
        str = "%s->value_ptr.cons_pair->car" % arg
    else:
        str = arg

    if gdb.execute ("output %s->value_ptr.string->used_size" % str, False, True) == "0":
        obj = "\"\""
    else:
        obj = gdb.execute ("output *%s->value_ptr.string->value@%s->value_ptr.string->used_size" % (str, str), False, True)

    ref = print_refcount (arg, in_cons)

    return justify_right (obj, ref)



def print_symbol_name (arg, in_cons):
    if in_cons:
        sym = "%s->value_ptr.cons_pair->car" % arg
    else:
        sym = arg

    obj = gdb.execute ("output *%s->value_ptr.symbol_name->value@%s->value_ptr.string->used_size" % (sym, sym), False, True)
    obj = obj [1 : len(obj) - 1]

    ref = print_refcount (arg, in_cons)

    return justify_right (obj, ref)



def print_symbol (arg, in_cons):
    if in_cons:
        sym = "%s->value_ptr.cons_pair->car" % arg
    else:
        sym = arg

    obj = gdb.execute ("output *%s->value_ptr.symbol->name@%s->value_ptr.symbol->name_len" % (sym, sym), False, True)
    obj = obj [1 : len (obj) - 1]

    ref = print_refcount (arg, in_cons)

    return justify_right (obj, ref)



def print_prefix (arg, type, in_cons):
    if in_cons:
        pr = "%s->value_ptr.cons_pair->car" % arg
    else:
        pr = arg

    if type == "TYPE_QUOTE":
        obj = "'"
    elif type == "TYPE_BACKQUOTE":
        obj = "`"
    elif type == "TYPE_COMMA":
        obj = ","
    elif type == "TYPE_AT":
        obj = "@"
    else:
        obj = "."

    ref = print_refcount (arg, in_cons)

    (obj, ref) = justify_right (obj, ref)

    obj += " "
    ref += " "

    out = print_object (pr + "->value_ptr.next", False)

    obj += out [0]
    ref += out [1]

    return (obj, ref)



def print_refcount (arg, in_cons):
    out = gdb.execute ("output %s->refcount" % arg, False, True)

    if in_cons:
        out += "(%s)" % gdb.execute ("output %s->value_ptr.cons_pair->car->refcount" % arg, False, True)

    return out



def justify_right (str1, str2):
    field = max (len (str1), len (str2))

    return (str1.rjust (field), str2.rjust (field))



def nthcdr (arg, n):
    for i in range (n):
        arg = arg + "->value_ptr.cons_pair->cdr"

    return arg



def nth (arg, n):
    return nthcdr (arg, n) + "->value_ptr.cons_pair->car"



def print_cons (arg, in_cons):
    if in_cons:
        o = "("
        r = print_refcount (arg, False)
        (obj, ref) = justify_right (o, r)
        obj += " "
        ref += " "
        arg = "%s->value_ptr.cons_pair->car" % arg
    else:
        obj = "("
        ref = " "

    list_length = int (gdb.execute ("output list_length (%s)" % arg, False, True))

    for i in range (list_length):
        out = print_object (nthcdr (arg, i), True)

        obj += out [0]
        ref += out [1]

        if i < list_length - 1:
            obj += " "
            ref += " "

    last_cdr = gdb.execute ("output nthcdr (%d, %s)" % (list_length, arg), False, True)

    if len (last_cdr.split ()) <= 4:
        out = print_object (nthcdr (arg, list_length), False)
        obj = "%s . %s" % (obj, out [0])
        ref = "%s   %s" % (ref, out [1])

    obj += ")"
    ref += " "

    return (obj, ref)




AlPrintObject ()
