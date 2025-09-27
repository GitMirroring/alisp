# Copyright (C) 2022-2025 Andrea Monaco
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
        desc = print_object (arg)
        gdb.write ("%s\n%s\n" % (desc [0], desc [1]))




def print_object (arg):
    type = gdb.execute ("output %s->type" % arg, False, True)

    if type == "TYPE_STRING":
        return print_string (arg)
    elif type == "TYPE_SYMBOL_NAME":
        return print_symbol_name (arg)
    elif type == "TYPE_SYMBOL":
        return print_symbol (arg)
    elif type == "TYPE_CONS_PAIR":
        return print_list (arg)
    elif type == "TYPE_QUOTE" or type == "TYPE_BACKQUOTE" or type == "TYPE_COMMA" or type == "TYPE_AT" or type == "TYPE_DOT":
        return print_prefix (arg)
    else:
        if type == "TYPE_INTEGER":
            return justify_right (["#<INTEGER>", print_refcounts (arg)])
        elif type == "TYPE_RATIO":
            return justify_right (["#<RATIO>", print_refcounts (arg)])
        elif type == "TYPE_FLOAT":
            return justify_right (["#<FLOAT>", print_refcounts (arg)])
        elif type == "TYPE_COMPLEX":
            return justify_right (["#<COMPLEX>", print_refcounts (arg)])
        elif type == "TYPE_CHARACTER":
            return justify_right (["#<CHARACTER>", print_refcounts (arg)])
        elif type == "TYPE_FUNCTION":
            return justify_right (["#<FUNCTION>", print_refcounts (arg)])
        elif type == "TYPE_MACRO":
            return justify_right (["#<MACRO>", print_refcounts (arg)])
        elif type == "TYPE_ARRAY":
            return justify_right (["#<ARRAY>", print_refcounts (arg)])
        elif type == "TYPE_HASHTABLE":
            return justify_right (["#<HASHTABLE>", print_refcounts (arg)])
        elif type == "TYPE_STREAM":
            return justify_right (["#<STREAM>", print_refcounts (arg)])
        elif type == "TYPE_FILENAME":
            return justify_right (["#<FILENAME>", print_refcounts (arg)])
        elif type == "TYPE_PACKAGE":
            return justify_right (["#<PACKAGE>", print_refcounts (arg)])
        else:
            return justify_right (["#<???>", print_refcounts (arg)])



def justify_right (list):
    field = max (len (list [0]), len (list [1]))

    return [list [0].rjust (field), list [1].rjust (field)]



def print_refcounts (arg):
    if gdb.execute ("output %s->flags & 0x100" % arg, False, True) == "0":
        weakr = gdb.execute ("output %s->refcount1" % arg, False, True)
        strongr = gdb.execute ("output %s->refcount2" % arg, False, True)
    else:
        weakr = gdb.execute ("output %s->refcount2" % arg, False, True)
        strongr = gdb.execute ("output %s->refcount1" % arg, False, True)

    return "%s(%s)" % (strongr, weakr)



def print_string (arg):
    ret = list ()

    if gdb.execute ("output %s->value_ptr.string->used_size" % arg, False, True) == "0":
        ret.append ("\"\"")
    else:
        val = gdb.execute ("output *%s->value_ptr.string->value@%s->value_ptr.string->used_size" % (arg, arg), False, True)
        ret.append ("%s" % val)

    ret.append (print_refcounts (arg))

    return justify_right (ret)



def print_symbol_name (arg):
    ret = list ()

    obj = gdb.execute ("output *%s->value_ptr.symbol_name->value@%s->value_ptr.symbol_name->used_size" % (arg, arg), False, True)
    obj = obj [1 : len(obj) - 1]

    ret.append (obj)

    ret.append (print_refcounts (arg))

    return justify_right (ret)



def print_symbol (arg):
    ret = list ()

    obj = gdb.execute ("output *%s->value_ptr.symbol->name@%s->value_ptr.symbol->name_len" % (arg, arg), False, True)
    obj = obj [1 : len (obj) - 1]

    ret.append (obj)

    ret.append (print_refcounts (arg))

    return justify_right (ret)



def print_prefix (arg):
    ret = list ()
    type = gdb.execute ("output %s->type" % arg, False, True)

    if type == "TYPE_QUOTE":
        ret.append ("'")
    elif type == "TYPE_BACKQUOTE":
        ret.append ("`")
    elif type == "TYPE_COMMA":
        ret.append (",")
    elif type == "TYPE_AT":
        ret.append ("@")
    else:
        ret.append (".")

    ret.append (print_refcounts (arg))

    ret = justify_right (ret)

    next = print_object ("%s->value_ptr.next" % arg)

    ret [0] = "%s %s" % (ret [0], next [0])
    ret [1] = "%s %s" % (ret [1], next [1])

    return ret



def print_list (arg):
    ret = ["(", " "]

    while True:
        obj = [" ", print_refcounts (arg)]
        obj = justify_right (obj)

        ret = [ret [0] + obj [0] + " ", ret [1] + obj [1] + " "]


        car = arg + "->value_ptr.cons_pair->car"

        obj = print_object (car)
        obj = justify_right (obj)

        ret = [ret [0] + obj [0], ret [1] + obj [1]]


        arg = arg + "->value_ptr.cons_pair->cdr"

        cdr = gdb.execute ("output %s" % arg, False, True).split ()

        if (len (cdr) > 4 and cdr [4] == "<nil_object>"):
            ret = [ret [0] + ")", ret [1] + " "]
            break

        ret = [ret [0] + " ", ret [1] + " "]

    return ret





AlPrintObject ()
