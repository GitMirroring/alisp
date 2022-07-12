#!/usr/bin/perl -w
use strict;

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


use IPC::Open2;



my $total_tests = 0;

my $passed_tests = 0;

my $failed_tests = 0;


my $pid = open2 (my $al_out, my $al_in, './al');



for ((1..5))
{
    <$al_out>;
}



# reader tests

make_test ("`'`\"\"", "'`\"\"");
make_test ("'(((\n" .
	   ")))", "((()))");
make_test ("\"\"", "\"\"");
make_test ("` ( ;\n" .
           " \"\")", "(\"\")");
make_test ("#|\n" .
	   "|# \"\"", "\"\"");
make_test (" #| #|\n" .
	   "|##|\n" .
	   "|#\n" .
	   "  |# \"\"", "\"\"");
make_test ("'( \"\" #||# )", "(\"\")");
make_test ("'asd\\f|gh|j", "|ASDfghJ|");
make_test ("'\\\n ", "|\n|");
make_test (":\\asd\\\\f", ":|aSD\\\\F|");


# eval tests

make_test ("nil", "()");
make_test ("NIL", "()");
make_test ("t", "T");
make_test ("(if nil 1)", "()");
make_test ("(common-lisp:if '(1) 2)", "2");


# arithmetic tests

make_test ("(+)", "0");
make_test ("(+ 1 2 3)", "6");
make_test ("(+ 1/2 2 3 1000)", "2011/2");
make_test ("(+ .1 .1 .1)", "0.3e0");
make_test ("(- 3.5)", "-0.35e1");
make_test ("(- 3 4.5)", "-0.15e1");
make_test ("(*)", "1");
make_test ("(* 3 5)", "15");



print "\ntotal tests: " . $total_tests . "\n";
print "passed " . $passed_tests . " (" . $passed_tests / $total_tests * 100 . "%), failed " . $failed_tests . "\n";



kill 'TERM', $pid;
waitpid ($pid, 0);





sub make_test
{
    print $_[0] . " -> ";

    my $in = $_[0] . "\n";

    my @inlines = split("\n", $in);

    foreach my $l (@inlines)
    {
	print $al_in $l . "\n";

	<$al_out>;
    }

    my @expected_outlines = split("\n", $_[1]);

    my $result = 1;

    for (my $i = 0; $i < scalar (@expected_outlines); $i++)
    {
	my $out = <$al_out>;

	print $out;

	chomp ($out);

	if ($out ne $expected_outlines [$i])
	{
	    $result = 0;
	}
    }


    if ($result == 1)
    {
	print "OK!\n";
	$passed_tests++;
    }
    else
    {
	print "FAIL, expected " . $_[1] . " instead\n";
	$failed_tests++;
    }

    print "\n";

    $total_tests++;
}
