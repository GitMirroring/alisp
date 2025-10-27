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



AC_PREREQ([2.69])
AC_INIT([al], [1.2], [andrea.monaco@autistici.org])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])


AC_CONFIG_SRCDIR([main.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])


AC_PROG_CC


AC_CHECK_HEADERS([stdlib.h string.h locale.h])


AC_TYPE_SIZE_T



AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memchr memmem strchr strerror])


AC_ARG_WITH([readline],
            [AS_HELP_STRING([--with-readline],
              [support command line editing @<:@default=check@:>@])],
            [],
            [with_readline=check])


LIBREADLINE=

AS_IF([test "x$with_readline" != xno],
      [AC_SEARCH_LIBS([readline], [readline],
                        [AC_SUBST([LIBREADLINE], ["-lreadline"])
                         AC_DEFINE([HAVE_LIBREADLINE], [1],
                                     [Define if you have libreadline])
                        ],
                        [if test "x$with_readline" != xcheck; then
                          AC_MSG_FAILURE(
                            [user gave --with-readline, but GNU readline wasn't found])
                          fi
		          ], )])


AC_SEARCH_LIBS([__gmpz_init], [gmp], , [AC_MSG_ERROR([GNU mp not found])])

AC_SEARCH_LIBS([sin], [m], , [AC_MSG_ERROR([standard math library not found])])



AC_ARG_VAR(AL_FIXNUM, [whether a fixnum maps to an `int' or a `long' @<:@default=int@:>@])

if test "x$AL_FIXNUM" = x -o "x$AL_FIXNUM" = int; then
   AC_DEFINE([FIXNUM_IS_INT], [1], [Tells that a fixnum maps to an int])
else
   AC_DEFINE([FIXNUM_IS_LONG], [1], [Tells that a fixnum maps to a long])
fi


AC_ARG_VAR(AL_MODULE_PATH, [slash-terminated path where al looks for cl.lisp and for modules like ASDF @<:@default=""@:>@])

AC_DEFINE_UNQUOTED([MODULE_PATH], ["$AL_MODULE_PATH"], [Where to look for cl.lisp and for modules like ASDF])



AC_OUTPUT
