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



AC_PREREQ([2.69])
AC_INIT([al], [0.4], [andrea.monaco@autistici.org])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_CONFIG_SRCDIR([main.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])


AC_PROG_CC


AC_SEARCH_LIBS([readline], [readline], , [AC_MSG_ERROR([GNU readline not found])])
AC_SEARCH_LIBS([__gmpz_init], [gmp], , [AC_MSG_ERROR([GNU mp not found])])


AC_CHECK_HEADERS([stdlib.h string.h])


AC_TYPE_SIZE_T


AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memchr strchr])



AC_OUTPUT
