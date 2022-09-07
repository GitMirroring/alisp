# Copyright (C) 2022 Andrea G. Monaco
# 
# This file is part of al.
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
        print_object (arg, False, True)



def print_object (arg, in_list, do_print):
    type = gdb.execute ("out %s->type" % arg, False, True)

    if type == "TYPE_STRING":
        out = print_string (arg, do_print)
    elif type == "TYPE_SYMBOL_NAME":
        out = print_symbol_name (arg, do_print)
    elif type == "TYPE_SYMBOL":
        out = print_symbol (arg, do_print, True)
    elif type == "TYPE_CONS_PAIR":
        out = print_list (arg, do_print)
    else:
        if type == "TYPE_INTEGER":
            out = "#<INTEGER>"
        elif type == "TYPE_RATIO":
            out = "#<RATIO>"
        elif type == "TYPE_FLOATING":
            out = "#<FLOAT>"
        elif type == "TYPE_FUNCTION":
            out = "#<FUNCTION>"
        else:
            out = "???"

        if do_print:
            gdb.write (out)

    if type != "TYPE_CONS_PAIR" and do_print:
        gdb.write ("\n")
        print_refcount (arg, False, do_print)

    if do_print:
        gdb.write ("\n")

    return out



def print_string (arg, do_print):
    if gdb.execute ("output %s->value_ptr.string->used_size" % arg, False, True) == "0":
        out = "\"\""
    else:
        out = gdb.execute ("output *%s->value_ptr.string->value@%s->value_ptr.string->used_size" % (arg, arg), False, True)

    if do_print:
        gdb.write (out)

    return out



def print_symbol_name (arg, do_print):
    out = gdb.execute ("output *%s->value_ptr.symbol_name->value@%s->value_ptr.string->used_size" % (arg, arg), False, True)
    out = out [1 : len(out) - 1]

    if do_print:
        gdb.write (out)

    return out



def print_symbol (arg, do_print, verbose):
    out = gdb.execute ("output *%s->value_ptr.symbol->name@%s->value_ptr.symbol->name_len" % (arg, arg), False, True)
    out = out [1 : len(out) - 1]

    if do_print:
        gdb.write (out)

    return out



def print_refcount (arg, in_list, do_print):
    out = gdb.execute ("output %s->refcount" % arg, False, True)

    if in_list:
        out += "(%s)" % gdb.execute ("output %s->value_ptr.cons_pair->car->refcount" % arg, False, True)

    if do_print:
        gdb.write (out)

    return out



def nthcdr (arg, n):
    for i in range (n):
        arg = arg + "->value_ptr.cons_pair->cdr"

    return arg



def nth (arg, n):
    return nthcdr (arg, n) + "->value_ptr.cons_pair->car"



def print_list (arg, do_print):
    list_length = int (gdb.execute ("output list_length (%s)" % arg, False, True))
    obj_out = "("
    refc_out = " "

    for i in range (list_length):
        o_out = print_object (nth (arg, i), True, False)

        if (type (o_out) == str):
            r_out = print_refcount (nthcdr (arg, i), True, False)

            field_l = max (len (o_out), len (r_out))

            obj_out += o_out.rjust (field_l)
            refc_out += r_out.rjust (field_l)

            if i < list_length - 1:
                obj_out = obj_out + " "
                refc_out = refc_out + " "

        else:
            r = print_refcount (nth (arg, i), False, False)
            obj_out += " " * (len (r) - 1)
            obj_out += "( "
            refc_out += r
            refc_out += " "

            obj_out += o_out [0] [1:]
            refc_out += o_out [1] [1:]


    obj_out += ")"
    refc_out += " "  

    if (do_print):
        gdb.write ("%s\n%s" % (obj_out, refc_out))

    return (obj_out, refc_out)





AlPrintObject ()
