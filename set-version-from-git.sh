#!/bin/sh

# Copyright (C) 2023-2025 Andrea Monaco
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


# Set version in a meaningful way in configure.ac if we are in a git
# clone



latest_tag=`git tag | tail -n 1`

base_version=`echo $latest_tag | cut -c 2-`

base_commit=`git log $latest_tag | head -n 1 | cut -d " " -f 2`

latest_commit=`git log | head -n 1 | cut -d " " -f 2`

trunc_commit=`echo $latest_commit | cut -c 1-5`



if [ "$base_commit" = "$latest_commit" ]; then
    version_string="$base_version"
else
    version_string="${base_version}+git${trunc_commit}"
fi



sed -i "s/^AC_INIT.*/AC_INIT([al], [${version_string}], [andrea.monaco@autistici.org])/" configure.ac
