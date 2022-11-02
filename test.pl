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


# Test script



use IPC::Open2;



my $total_tests = 0;

my $passed_tests = 0;

my $failed_tests = 0;


my $pid = open2 (my $al_out, my $al_in, './al -q');



for ((1..5))
{
    <$al_out>;
}



# reader tests

make_test ("`'`\"\"", "'`\"\"");
make_test ("'(((\n" .
	   ")))", "((()))");
make_test ("\"\"", "\"\"");
make_test ("` ( ; asd\n" .
           " \"\")", "(\"\")");
make_test ("#|\n" .
	   "|# \"\"", "\"\"");
make_test (" #| #|\n" .
	   "|##|\n" .
	   "|#\n" .
	   "  |# \"\"", "\"\"");
make_test ("'(1 . 2)", "(1 . 2)");
make_test ("'(1 . (2 3))", "(1 2 3)");
make_test ("'(1 . 2\n" .
	   ")", "(1 . 2)");
make_test ("'(1 .\n" .
	   "2)", "(1 . 2)");
make_test ("'(1\n" .
	   ". 2)", "(1 . 2)");
make_test ("'(1 . (\n" .
	   "2))", "(1 2)");
make_test ("'( \"\" #||# )", "(\"\")");
make_test ("'asd\\f|gh|j", "|ASDfghJ|");
make_test ("'\\\n ", "|\n|");
make_test (":\\asd\\\\f", ":|aSD\\\\F|");


# now we extensively test backtick notation.  maybe some of these
# cases are not specified by the standard or are undefined behavior,
# so I chose the most consistent behavior to me

make_test ("`,'a", "A");
make_test ("``,a", "`,A");
make_test ("`,`a", "A");
make_test ("``(a ,,(+ 1 2))", "`(A ,3)");
make_test ("``(a ,,(+ 1 2) ,(+ 3 4))", "`(A ,3 ,(+ 3 4))");
make_test ("``(a ,(+ ,1 2))", "`(A ,(+ 1 2))");
make_test ("`(,1 . ,2)", "(1 . 2)");
make_test ("`(,1 ,2 . ,3)", "(1 2 . 3)");
make_test ("`(if ``(progn ,,1))", "(IF ``(PROGN ,,1))");
make_test ("'@", "@");
make_test ("`(,@())", "()");
make_test ("`(,.())", "()");
make_test ("`(,@() ,.())", "()");
make_test ("`(1 2 ,@() ,.() 3)", "(1 2 3)");
make_test ("``(1 2 ,,@() ,,.())", "`(1 2)");
make_test ("``(,@())", "`(,@())");
make_test ("`(1 ,\@5)", "(1 . 5)");
make_test ("`(,1 ,@(cdr '(1 2 3 4)))", "(1 2 3 4)");
make_test ("`(,1 ,@(cdr '(1 2 3 4)) ,5)", "(1 2 3 4 5)");
make_test ("`(,@(cdr '(0 1 2 3 4)) 5)", "(1 2 3 4 5)");
make_test ("`(,@(cdr '(0 1 2 3)) ,4)", "(1 2 3 4)");
make_test ("`(,.(cdr '(0 1 2 3)) ,4)", "(1 2 3 4)");
make_test ("``(,,.(cdr '(0 1 2 3)) ,4)", "`(,1 ,2 ,3 ,4)");


# other reader tests

make_test ("#\\a", "#\\a");
make_test ("#\\κ", "#\\κ");
make_test ("#\\newLINE", "#\\Newline");
make_test ("#.\"\"", "\"\"");
make_test ("#p\".\"", "#P\".\"");
make_test ("#(1 2 3)", "#(1 2 3)");
make_test ("#(1 2\n" .
	   "3)", "#(1 2 3)");
make_test ("#(\n" .
	   ")", "#()");
make_test ("'#:a", "#:A");


# eval tests

make_test ("nil", "()");
make_test ("NIL", "()");
make_test ("t", "T");
make_test ("(progn)", "()");
make_test ("(progn 1 2 3)", "3");
make_test ("(if nil 1)", "()");
make_test ("(common-lisp:if '(1) 2)", "2");
make_test ("(cl:eq 1 1)", "()");
make_test ("(eq 'a 'a)", "T");
make_test ("(null 1)", "()");
make_test ("(not nil)", "T");
make_test ("(cons 1 2)", "(1 . 2)");
make_test ("(list 1 2 3)", "(1 2 3)");
make_test ("(let ((a 2)) a)", "2");
make_test ("(let ((x 0)) x x x)", "0");
make_test ("(defconstant a 8)", "A");
make_test ("a", "8");
make_test ("(defparameter b 9)", "B");
make_test ("(defvar b (0))", "B");
make_test ("b", "9");
make_test ("(setf b 10)", "10");
make_test ("(setf b 9)", "9");
make_test ("(setf c 4)", "4");
make_test ("c", "4");
make_test ("(setf c (+ c 1))", "5");
make_test ("c", "5");
make_test ("(let ((b 10)) b)", "10");
make_test ("(let ((b 1)) (setf b 2) b)", "2");
make_test ("b", "9");
make_test ("(let* ((x 1) (y x)) y)", "1");
make_test ("(defun f ())", "F");
make_test ("#'f", "#<FUNCTION F>");
make_test ("#'car", "#<FUNCTION BUILTIN CAR>");
make_test ("(function f)", "#<FUNCTION F>");
make_test ("(function car)", "#<FUNCTION BUILTIN CAR>");
make_test ("(apply #'+ 1 2 ())", "3");
make_test ("(apply #'+ '(1 2))", "3");
make_test ("(apply #'car '(1 2) ())", "1");
make_test ("(apply 'car '(1 2) ())", "1");
make_test ("(apply (lambda (x) (car x)) '(1 2) ())", "1");
make_test ("(f)", "()");
make_test ("(defun f () \"\")", "F");
make_test ("(f)", "\"\"");
make_test ("(defun f (x) \"\")", "F");
make_test ("(f 0)", "\"\"");
make_test ("(defun fun (x y) y)", "FUN");
make_test ("(fun 1 2)", "2");
make_test ("(defun fun (b) b)", "FUN");
make_test ("(fun 3)", "3");
make_test ("b", "9");
make_test ("(defun g (x &optional y) y)", "G");
make_test ("(g 1)", "()");
make_test ("(g 1 2)", "2");
make_test ("(defun g2 (&optional (x \"a\")) x)", "G2");
make_test ("(g2)", "\"a\"");
make_test ("(g2 4)", "4");
make_test ("(defun g3 (&optional (x \"a\" provided)) provided)", "G3");
make_test ("(g3)", "()");
make_test ("(g3 7)", "T");
make_test ("(defun fun (x &rest y) y)", "FUN");
make_test ("(fun 5 6 7 8)", "(6 7 8)");
make_test ("(defun g4 (x &optional y &rest z) z)", "G4");
make_test ("(g4 1 2 3 4 5)", "(3 4 5)");
make_test ("(load \"cl.lisp\")", "T");
make_test ("(1+ 5)", "6");
make_test ("(typep '(1 . 2) 'cons)", "T");
make_test ("(typep 0 'integer)", "T");
make_test ("(typep 1/2 'ratio)", "T");
make_test ("(typep \"abc\" 'string)", "T");
make_test ("(typep \"abc\" '(string))", "T");
make_test ("(typep 0 'string)", "()");
make_test ("(typep '(1 2) 'list)", "T");
make_test ("(typep #(1 2) 'sequence)", "T");
make_test ("(typep \"\" t)", "T");
make_test ("(typep 0 'nil)", "()");
make_test ("(typep nil nil)", "()");
make_test ("(typep nil 'null)", "T");
make_test ("(typep #p\"\" 'pathname)", "T");
make_test ("(boundp 'b)", "T");
make_test ("(boundp 'g3)", "()");
make_test ("(symbol-value 'b)", "9");
make_test ("(let ((b 10)) (symbol-value 'b))", "10");
make_test ("(fboundp 'b)", "()");
make_test ("(fboundp 'g3)", "T");
make_test ("(symbol-function 'fun)", "#<FUNCTION FUN>");
make_test ("(symbol-function 'defparameter)", "#<MACRO BUILTIN DEFPARAMETER>");
make_test ("(symbol-function 'if)", "#<SPECIAL OPERATOR IF>");
make_test ("(car '(1 2))", "1");
make_test ("(cdr '(1 2))", "(2)");
make_test ("(car ())", "()");
make_test ("(cdr ())", "()");
make_test ("(nth 0 '(0))", "0");
make_test ("(nth 1 '(0))", "()");
make_test ("(nthcdr 1 ())", "()");
make_test ("(nthcdr 2 '(a b c))", "(C)");
make_test ("(nthcdr 1 '(0 . 1))", "1");
make_test ("(length ())", "0");
make_test ("(length \"aaa\")", "3");
make_test ("(length #(1 2))", "2");
make_test ("(length '(0 1 2))", "3");
make_test ("(second '(0))", "()");
make_test ("(fifth '(0 1 2 3 4))", "4");
make_test ("(when t \"\" \"\" \"\")", "\"\"");
make_test ("(unless nil \"\" \"\" \"\")", "\"\"");
make_test ("(tagbody\n" .
	   "  (write \"1\")\n" .
	   "  (go jmp)\n" .
	   "  (write \"2\")\n" .
	   "  jmp\n" .
	   "  (write \"3\"))", "\"1\"\n\"3\"\n()");
make_test ("(tagbody 1 (go 3) 2 3 (go 4) 4 (write \"\"))", "\"\"\n()");
make_test ("(tagbody 1 2 (tagbody 3 (go 4)) 4 (write \"\"))", "\"\"\n()");
make_test ("(cddr '(0 1 2))", "(2)");
make_test ("(cddddr '(0 1 2 3 4))", "(4)");
make_test ("(cadddr '(0 1 2 3 4))", "3");
make_test ("(caar '((4) 1 2 3))", "4");
make_test ("(incf c)", "6");
make_test ("(incf c 3)", "9");
make_test ("(decf c)", "8");
make_test ("(decf c c)", "0");
make_test ("(and)", "T");
make_test ("(and t)", "T");
make_test ("(and t t t)", "T");
make_test ("(and t nil t)", "()");
make_test ("(and (= 1 1) t)", "T");
make_test ("(and t (= 1 2) t)", "()");
make_test ("(or)", "()");
make_test ("(or nil nil nil)", "()");
make_test ("(or (= 1 2) t)", "T");
make_test ("(or t nil t)", "T");


# arithmetic tests

make_test ("(+)", "0");
make_test ("(+ 1 2 3)", "6");
make_test ("(+ 1/2 2 3 1000)", "2011/2");
make_test ("(+ a b)", "17");
make_test ("(+ .1 .1 .1)", "0.3e0");
make_test ("(- 3.5)", "-0.35e1");
make_test ("(- 3 4.5)", "-0.15e1");
make_test ("(*)", "1");
make_test ("(* 3 5)", "15");
make_test ("(= 1)", "T");
make_test ("(= 1 1)", "T");
make_test ("(= 1 2)", "()");
make_test ("(= 1 1 1.0 1/1)", "T");
make_test ("(= 0 -0.0)", "T");


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

    my $out;

    for (my $i = 0; $i < scalar (@expected_outlines); $i++)
    {
	$out = <$al_out>;

	if (not defined ($out) and eof ($al_out))
	{
	    print "\n\nEOF reached, probably al crashed\n";

	    exit;
	}

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
