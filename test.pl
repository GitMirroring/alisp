#!/usr/bin/perl -w
use strict;

# Copyright (C) 2022-2023 Andrea G. Monaco
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


# Test script



use IPC::Open2;



my $total_tests = 0;

my $passed_tests = 0;

my $failed_tests = 0;


my $pid = open2 (my $al_out, my $al_in, './al -q');



for ((1..5))
{
    if (eof ($al_out))
    {
	print "EOF reached from al, it probably crashed\n";

	exit;
    }

    <$al_out>;
}



# reader tests

make_test ("`'`\"\"", "'`\"\"");
make_test ("'(((\n" .
	   ")))", "((NIL))");
make_test ("'(\"\"`\"\")", "(\"\" `\"\")");
make_test ("'(\n" .
	   "(\n" .
	   ";\n" .
	   ")\n" .
	   ")", "(NIL)");
make_test ("\"\"", "\"\"");
make_test ("\"\" \"\" \"\"", "\"\"\n\"\"\n\"\"");
make_test ("` ( ; asd\n" .
           " \"\")", "(\"\")");
make_test ("`';\na", "'A");
make_test ("#|\n" .
	   "|# \"\"", "\"\"");
make_test ("#|\n" .
	   "|# #|\n" .
	   "|# \"\"", "\"\"");
make_test (" #| #|\n" .
	   "|##|\n" .
	   "|#\n" .
	   "  |# \"\"", "\"\"");
make_test ("'\n(\n)", "NIL");
make_test ("'(1 . 2)", "(1 . 2)");
make_test ("'(1 . (2 3))", "(1 2 3)");
make_test ("'(1 . 2\n" .
	   ")", "(1 . 2)");
make_test ("'(1 .\n" .
	   "2)", "(1 . 2)");
make_test ("'(1\n" .
	   ". 2)", "(1 . 2)");
make_test ("'(1 .\n(2)\n)", "(1 2)");
make_test ("'(1 . (\n" .
	   "2))", "(1 2)");
make_test ("'( \"\" #||# )", "(\"\")");
make_test ("'asd\\f|gh|j", "|ASDfghJ|");
make_test ("'\\\\", "\\\\");
make_test ("'\\123", "|123|");
make_test ("'123\\ ", "|123 |");
make_test ("'\\.", "|.|");
make_test ("'\\..", "|..|");
make_test ("'\\\n ", "|\n|");
make_test ("'a\\\n ", "|A\n|");
make_test ("'|aaa\nbb|c", "|aaa\nbbC|");
make_test ("'a\\\n..", "|A\n..|");
make_test ("'|aaa\n:bbb|", "|aaa\n:bbb|");
make_test (":\\asd\\\\f", ":|aSD\\\\F|");
make_test ("'cl:car", "CAR");
make_test ("'cl-user::car", "CAR");
make_test ("(car ''0)", "QUOTE");
make_test ("(car '#'car)", "FUNCTION");


# now we extensively test backtick notation.  maybe some of these
# cases are not specified by the standard or are undefined behavior,
# so I chose the most consistent behavior to me

make_test ("`,'a", "A");
make_test ("``,a", "`,A");
make_test ("`,`a", "A");
make_test ("`(1 . 2)", "(1 . 2)");
make_test ("``(a ,,(+ 1 2))", "`(A ,3)");
make_test ("``(a ,,(+ 1 2) ,(+ 3 4))", "`(A ,3 ,(+ 3 4))");
make_test ("``(a ,(+ ,1 2))", "`(A ,(+ 1 2))");
make_test ("`(,1 . ,2)", "(1 . 2)");
make_test ("`(,1 ,2 . ,3)", "(1 2 . 3)");
#make_test ("`',(car ())", "'NIL");
make_test ("`',(car ())", "(QUOTE NIL)");
make_test ("`(if ``(progn ,,1))", "(IF ``(PROGN ,,1))");
make_test ("'@", "@");
make_test ("`(,@())", "NIL");
make_test ("`(,.())", "NIL");
make_test ("`(,@() ,.())", "NIL");
make_test ("`(1 2 ,@() ,.() 3)", "(1 2 3)");
make_test ("``(1 2 ,,@() ,,.())", "`(1 2)");
make_test ("``(,@())", "`(,\@NIL)");
make_test ("`(,\@5)", "5");
make_test ("`(1 ,\@5)", "(1 . 5)");
make_test ("`(,\@'(3 4))", "(3 4)");
make_test ("`(1 2 ,\@'(3 . 4))", "(1 2 3 . 4)");
make_test ("`(,@``(3 4))", "`(3 4)");
make_test ("`(,1 ,@(cdr '(1 2 3 4)))", "(1 2 3 4)");
make_test ("`(,1 ,@(cdr '(1 2 3 4)) ,5)", "(1 2 3 4 5)");
make_test ("`(,@(cdr '(0 1 2 3 4)) 5)", "(1 2 3 4 5)");
make_test ("`(,@(cdr '(0 1 2 3)) ,4)", "(1 2 3 4)");
make_test ("`(,.(cdr '(0 1 2 3)) ,4)", "(1 2 3 4)");
make_test ("``(,,@(cdr '(0 1 2 3)) ,4)", "`(,1 ,2 ,3 ,4)");
make_test ("``(,,.(cdr '(0 1 2 3)) ,4)", "`(,1 ,2 ,3 ,4)");


# other reader tests

make_test ("#\\a", "#\\a");
make_test ("#\\κ", "#\\κ");
make_test ("#\\,", "#\\,");
make_test ("#\\\\", "#\\\\");
make_test ("#\\|", "#\\|");
make_test ("#\\newLINE", "#\\Newline");
make_test ("#\\\n ", "#\\Newline");
make_test ("#\\Newline", "#\\Newline");
make_test ("#.\"\"", "\"\"");
make_test ("#. (+ 1 2)", "3");
make_test ("#p\".\"", "#P\".\"");
make_test ("#(a b c)", "#(A B C)");
make_test ("#(1 2 3)", "#(1 2 3)");
make_test ("#(1 2\n" .
	   "3)", "#(1 2 3)");
make_test ("#(\n" .
	   ")", "#()");
make_test ("#(\n" .
	   "\n" .
	   ")", "#()");
make_test ("#(\n" .
	   "0\n" .
	   ")", "#(0)");
make_test ("#(\n" .
	   "(\n" .
	   ")\n" .
	   ")", "#(NIL)");
make_test ("'#:a", "#:A");


# eval tests

make_test ("nil", "NIL");
make_test ("NIL", "NIL");
make_test ("t", "T");
make_test ("(progn)", "NIL");
make_test ("(progn 1 2 3)", "3");
make_test ("(progn (values 1 2 3))", "1\n2\n3");
make_test ("(values (+ 1 0) 2 3)", "1\n2\n3");
make_test ("(values-list '(1 2 3))", "1\n2\n3");
make_test ("(multiple-value-list (values))", "NIL");
make_test ("(multiple-value-list (values 1 2 3))", "(1 2 3)");
make_test ("(multiple-value-call (lambda ()))", "NIL");
make_test ("(multiple-value-call #'+ (floor 5 2) (values) 1)", "4");
make_test ("(if nil 1)", "NIL");
make_test ("(common-lisp:if '(1) 2)", "2");
make_test ("(cl:eq 1 1)", "NIL");
make_test ("(eq 'a 'a)", "T");
make_test ("(eql 'a 'a)", "T");
make_test ("(eql 'a 'b)", "NIL");
make_test ("(eql 0 0)", "T");
make_test ("(eql 0 0.0)", "NIL");
make_test ("(eql 2 2/1)", "T");
make_test ("(eql 2.0 2.0)", "T");
make_test ("(eql #\\a #\\a)", "T");
make_test ("(eql #\\A #\\a)", "NIL");
make_test ("(null 1)", "NIL");
make_test ("(not nil)", "T");
make_test ("(cons 1 2)", "(1 . 2)");
make_test ("(list)", "NIL");
make_test ("(list 1 2 3)", "(1 2 3)");
make_test ("(list* 0)", "0");
make_test ("(list* 0 1 2)", "(0 1 . 2)");
make_test ("(append ())", "NIL");
make_test ("(append nil nil 0)", "0");
make_test ("(append '(1 2) '(3 4))", "(1 2 3 4)");
make_test ("(append '(1) (values))", "(1)");
make_test ("(nconc)", "NIL");
make_test ("(nconc 0)", "0");
make_test ("(nconc (list 'a 'b 'c) (list 'd 'e))", "(A B C D E)");
make_test ("(nconc (list 'a 'b 'c) 'd)", "(A B C . D)");
make_test ("(nconc (list 'a 'b 'c) NIL (list 'd 'e) NIL 'f)", "(A B C D E . F)");
make_test ("(let ((a 2)) a)", "2");
make_test ("(let ((x 0)) x x x)", "0");
make_test ("(let ((a)) a)", "NIL");
make_test ("(let () (declare (ignore a b (function c)) (ignorable x (function y))) (declare (ignore)) (+ 1 2))", "3");
make_test ("(let () (declare (inline a b (setf c)) (notinline x (setf y))) (+ 1 2))", "3");
make_test ("(let () (declare (optimize safety speed (compilation-speed 0) (debug 3))) (+ 1 2))", "3");
make_test ("(let ((x 0)) (declare (special x)) x)", "0");
make_test ("(let ((x 0)) (declare (special x)) (let ((x 1)) x))", "1");
make_test ("(let ((x 0)) (declare (special x)) (let ((x 1)) (eval 'x)))", "0");
make_test ("(let ((x 0)) (let ((x 1)) (declare (special x)) (let () x)))", "1");
make_test ("(defun func () x)", "FUNC");
make_test ("(let ((x 10)) (declare (special x)) (func))", "10");
make_test ("(let ((x 12)) (declare (special x)) (let ((x 13)) (write (func)) (write x)))", "1213\n13");
make_test ("(let* ((x 0)) (declare (special x)) x)", "0");
make_test ("(let* ((x 0)) (declare (special x)) (let ((x 1)) x))", "1");
make_test ("(let* ((x 0)) (declare (special x)) (let ((x 1)) (eval 'x)))", "0");
make_test ("(let* ((a)) a)", "NIL");
make_test ("(flet ((a () (write \"\"))) (a))", "\"\"\n\"\"");
make_test ("(labels ((a NIL (write \"\"))) (a))", "\"\"\n\"\"");
make_test ("(labels ((a () (b)) (b () (write \"\"))) (a))", "\"\"\n\"\"");
make_test ("(macrolet ((w (a) `(write ,a))) (w \"\"))", "\"\"\n\"\"");
make_test ("(eval-when (:compile-toplevel compile) 0)", "NIL");
make_test ("(eval-when (:load-toplevel load) 0)", "NIL");
make_test ("(eval-when (:execute eval load compile) 0)", "0");
make_test ("(defconstant a 8)", "A");
make_test ("(defconstant a 8)", "A");
make_test ("a", "8");
make_test ("(defparameter b 9)", "B");
make_test ("(defvar b (0))", "B");
make_test ("b", "9");
make_test ("(setf b 10)", "10");
make_test ("(setf b 9)", "9");
make_test ("(setf c 4)", "4");
make_test ("c", "4");
make_test ("(setf)", "NIL");
make_test ("(let ((c 0)) c)", "0");
make_test ("(setf c (+ c 1))", "5");
make_test ("c", "5");
make_test ("(setq)", "NIL");
make_test ("(setq b 9 c 4 c (+ c 1))", "5");
make_test ("(psetq)", "NIL");
make_test ("(psetq xx 0)", "NIL");
make_test ("(psetq xx 1 yy xx)", "NIL");
make_test ("xx", "1");
make_test ("yy", "0");
make_test ("(setf tt (list 'a 'b 'd))", "(A B D)");
make_test ("(setf (car tt) 'f)", "F");
make_test ("tt", "(F B D)");
make_test ("(setf (cdr tt) 'g)", "G");
make_test ("tt", "(F . G)");
make_test ("(defparameter str \"abcdef\")", "STR");
make_test ("(setf (aref str 2) #\\z)", "#\\z");
make_test ("str", "\"abzdef\"");
make_test ("(defparameter arr (make-array 3))", "ARR");
make_test ("(setf (aref arr 2) \"aaa\")", "\"aaa\"");
make_test ("arr", "#(NIL NIL \"aaa\")");
make_test ("(let ((b 10)) b)", "10");
make_test ("(let ((b 1)) (setf b 2) b)", "2");
make_test ("b", "9");
make_test ("(let ((s \"aaa\")) (list (setf (elt s 1) #\\b) s))", "(#\\b \"aba\")");
make_test ("(let ((l (list 'a 'b 'c))) (list (setf (elt l 2) 'd) l))", "(D (A B D))");
make_test ("(let* ((x 1) (y x)) y)", "1");
make_test ("(defun f ())", "F");
make_test ("#'f", "#<FUNCTION F>");
make_test ("#'car", "#<FUNCTION BUILTIN CAR>");
make_test ("#'(lambda nil nil)", "#<FUNCTION ?>");
make_test ("(flet ((foo ())) #'foo)", "#<FUNCTION ?>");
make_test ("(function f)", "#<FUNCTION F>");
make_test ("(flet ((foo ())) (function foo))", "#<FUNCTION ?>");
make_test ("(function car)", "#<FUNCTION BUILTIN CAR>");
make_test ("(function (lambda nil nil))", "#<FUNCTION ?>");
make_test ("(apply #'+ 1 2 ())", "3");
make_test ("(apply #'+ '(1 2))", "3");
make_test ("(apply #'car '(1 2) ())", "1");
make_test ("(apply 'car '(1 2) ())", "1");
make_test ("(apply (lambda (x) (car x)) '(1 2) ())", "1");
make_test ("(let ((s 0)) (funcall (lambda (s) s) 1))", "1");
make_test ("(funcall #'+ 1 2 3)", "6");
make_test ("(funcall 'car '(1 2 3))", "1");
make_test ("(f)", "NIL");
make_test ("(defun f () \"\")", "F");
make_test ("(f)", "\"\"");
make_test ("(defun aaa () aaa aaa)", "AAA");
make_test ("(defun f (x) \"\")", "F");
make_test ("(f 0)", "\"\"");
make_test ("(defun f () (values 1 2))", "F");
make_test ("(f)", "1\n2");
make_test ("(defun fun (x y) y)", "FUN");
make_test ("(fun 1 2)", "2");
make_test ("(defun fun (b) b)", "FUN");
make_test ("(fun 3)", "3");
make_test ("b", "9");
make_test ("(defun g (x &optional y) y)", "G");
make_test ("(g 1)", "NIL");
make_test ("(g 1 2)", "2");
make_test ("(defun g2 (&optional (x \"a\")) x)", "G2");
make_test ("(g2)", "\"a\"");
make_test ("(g2 4)", "4");
make_test ("(defun g3 (&optional (x \"a\" provided)) (list x provided))", "G3");
make_test ("(g3)", "(\"a\" NIL)");
make_test ("(g3 7)", "(7 T)");
make_test ("(defun g4 (x &optional y &rest z) z)", "G4");
make_test ("(g4 1 2 3 (+ 3 1) 5)", "(3 4 5)");
make_test ("(defun fun (&rest x) x)", "FUN");
make_test ("(fun)", "NIL");
make_test ("(defun fun (x &rest y) y)", "FUN");
make_test ("(fun 5 6 7 8)", "(6 7 8)");
make_test ("(defun fun2 (&rest r &key x y z) y)", "FUN2");
make_test ("(fun2 :y 0)", "0");
make_test ("(fun2 :y 0 :y 1)", "0");
make_test ("(fun2 :x 0)", "NIL");
make_test ("(fun2 :z 'tt :y (+ 1 2))", "3");
make_test ("(fun2)", "NIL");
make_test ("(defun fun2 (&key ((:k aa) 1 supp)) (list aa supp))", "FUN2");
make_test ("(fun2)", "(1 NIL)");
make_test ("(fun2 :k 1)", "(1 T)");
make_test ("(defun fun2 (&rest r &key x y z) r)", "FUN2");
make_test ("(fun2 :y (+ 1 2) :z 'tt)", "(:Y 3 :Z TT)");
make_test ("(fun2)", "NIL");
make_test ("(defun fun (x &rest y &key z) y)", "FUN");
make_test ("(fun 1 :z 0)", "(:Z 0)");
make_test ("(defun fun (x &optional y &rest z &key k) z)", "FUN");
make_test ("(fun 0 1 :k 2)", "(:K 2)");
make_test ("(defun f (&key &allow-other-keys))", "F");
make_test ("(f :a 0)", "NIL");
make_test ("(defun f (&key x &allow-other-keys) x)", "F");
make_test ("(f :x 0 :y 1)", "0");
make_test ("(f)", "NIL");
make_test ("(let ((x 0)) (defun inc () (setf x (+ x 1))))", "INC");
make_test ("(inc)", "1");
make_test ("(inc)", "2");
make_test ("(inc)", "3");
make_test ("(defmacro f () (f))", "F");
make_test ("(defmacro f () (f))", "F");
make_test ("(load \"cl.lisp\")", "T");
make_test ("(open-stream-p *standard-input*)", "T");
make_test ("(input-stream-p *standard-input*)", "T");
make_test ("(input-stream-p *standard-output*)", "NIL");
make_test ("(output-stream-p *standard-input*)", "NIL");
make_test ("(output-stream-p *standard-output*)", "T");
make_test ("(interactive-stream-p *standard-input*)", "NIL");
make_test ("(upper-case-p #\\A)", "T");
make_test ("(upper-case-p #\\a)", "NIL");
make_test ("(upper-case-p #\\2)", "NIL");
make_test ("(lower-case-p #\\A)", "NIL");
make_test ("(lower-case-p #\\a)", "T");
make_test ("(both-case-p #\\a)", "T");
make_test ("(both-case-p #\\1)", "NIL");
make_test ("(identity \"hi\")", "\"hi\"");
make_test ("(1+ 5)", "6");
make_test ("(1- .5)", "-0.5");
make_test ("(byte 20 100)", "#<BYTE-SPECIFIER size 20 position 100>");
make_test ("(byte-size (byte 20 100))", "20");
make_test ("(byte-position (byte 20 100))", "100");

make_test ("(macroexpand-1 '(incf var))", "(SETF VAR (+ VAR 1))\nT");
make_test ("(macroexpand-1 '(+ 0))", "(+ 0)\nNIL");
make_test ("(defmacro test2 nil (+ 1 2))", "TEST2");
make_test ("(defmacro test nil '(test2))", "TEST");
make_test ("(macroexpand '(test2))", "3\nT");
make_test ("(macroexpand '(test))", "3\nT");
make_test ("(macroexpand '(+ 1 2))", "(+ 1 2)\nNIL");

make_test ("(typep '(1 . 2) 'cons)", "T");
make_test ("(typep () 'atom)", "T");
make_test ("(typep (cons 1 2) 'atom)", "NIL");
make_test ("(typep 0 'number)", "T");
make_test ("(typep 0.1 'real)", "T");
make_test ("(typep 0 'rational)", "T");
make_test ("(typep 0.1 'rational)", "NIL");
make_test ("(typep 0 'integer)", "T");
make_test ("(typep 0 'bignum)", "T");
make_test ("(typep 0 'fixnum)", "NIL");
make_test ("(typep 1/2 'ratio)", "T");
make_test ("(typep 3/1 'ratio)", "NIL");
make_test ("(typep 0.1 'float)", "T");
make_test ("(typep 0.1 'single-float)", "T");
make_test ("(typep 0.1 'double-float)", "T");
make_test ("(typep 0.1 'short-float)", "T");
make_test ("(typep 0.1 'long-float)", "T");
make_test ("(typep 1.0 'complex)", "NIL");
make_test ("(typep (complex 1 1) 'complex)", "T");
make_test ("(typep \"abc\" 'string)", "T");
make_test ("(typep \"abc\" '(string))", "T");
make_test ("(typep \"abc\" 'array)", "T");
make_test ("(typep 0 'string)", "NIL");
make_test ("(typep (make-hash-table) 'hash-table)", "T");
make_test ("(typep '(1 2) 'list)", "T");
make_test ("(typep #(1 2) 'sequence)", "T");
make_test ("(typep \"\" t)", "T");
make_test ("(typep 0 'nil)", "NIL");
make_test ("(typep nil nil)", "NIL");
make_test ("(typep nil 'null)", "T");
make_test ("(typep 'aaa 'symbol)", "T");
make_test ("(typep :aaa 'keyword)", "T");
make_test ("(typep 'aaa 'keyword)", "NIL");
make_test ("(typep nil 'boolean)", "T");
make_test ("(typep #p\"\" 'pathname)", "T");
make_test ("(typep (open #p\"README\") 'stream)", "T");
make_test ("(typep (open #p\"README\") 'file-stream)", "T");
make_test ("(typep (make-string-input-stream \"hello\") 'string-stream)", "T");
make_test ("(typep 0 '(or integer string))", "T");
make_test ("(typep 0 '(and integer string))", "NIL");
make_test ("(typep 0 '(not string))", "T");
make_test ("(type-of 'a)", "SYMBOL");
make_test ("(type-of \"aaa\")", "STRING");
make_test ("(type-of #\\a)", "CHARACTER");
make_test ("(type-of '(1 . 2))", "CONS");
make_test ("(type-of 0)", "INTEGER");
make_test ("(type-of 1/2)", "RATIO");
make_test ("(type-of 1.0)", "FLOAT");
make_test ("(type-of (complex 0 1))", "COMPLEX");
make_test ("(type-of #'car)", "FUNCTION");
make_test ("(type-of *package*)", "PACKAGE");
make_test ("(type-of #(0 1 2))", "ARRAY");
make_test ("(type-of (make-hash-table))", "HASH-TABLE");
make_test ("(type-of #p\"aaa\")", "PATHNAME");
make_test ("(type-of *standard-output*)", "STREAM");
make_test ("(deftype not-integer () `(not integer))", "NOT-INTEGER");
make_test ("(typep 0 'not-integer)", "NIL");
make_test ("(typep \"\" 'not-integer)", "T");
make_test ("(define-setf-expander foo ())", "FOO");
make_test ("(get-setf-expansion '(foo))", "NIL");

make_test ("(make-string 3)", "\"\0\0\0\"");
make_test ("(intern \"hi\")", "|hi|\nNIL");
make_test ("(intern \"hi\")", "|hi|\n:INTERNAL");
make_test ("(intern \"hi\" 'keyword)", ":|hi|\nNIL");
make_test ("(intern \"hi\" 'keyword)", ":|hi|\n:EXTERNAL");
make_test ("(find-symbol \"CAR\")", "CAR\n:INHERITED");
make_test ("(find-symbol \"CAR\" 'cl)", "CAR\n:EXTERNAL");
make_test ("(find-symbol \"car\")", "NIL\nNIL");
make_test ("(unintern :hi)", "NIL");
make_test ("(unintern '|hi|)", "T");
make_test ("(intern \"hi\")", "|hi|\nNIL");
make_test ("(unintern :|hi| 'keyword)", "T");
make_test ("(intern \"hi\" 'keyword)", ":|hi|\nNIL");
make_test ("(make-symbol \"aaa\")", "#:|aaa|");
make_test ("(boundp 'b)", "T");
make_test ("(boundp 'g3)", "NIL");
make_test ("(let ((var 0)) (boundp 'var))", "NIL");
make_test ("(let ((var 0)) (declare (special var)) (boundp 'var))", "T");
make_test ("(symbol-value 'b)", "9");
make_test ("(let ((b 10)) (symbol-value 'b))", "10");
make_test ("(setf (symbol-value 'foo) 0)", "0");
make_test ("foo", "0");
make_test ("(let ((b 11)) (set 'b 12) b)", "12");
make_test ("(fboundp 'b)", "NIL");
make_test ("(fboundp 'g3)", "T");
make_test ("(symbol-function 'fun)", "#<FUNCTION FUN>");
make_test ("(symbol-function 'defparameter)", "#<MACRO BUILTIN DEFPARAMETER>");
make_test ("(symbol-function 'if)", "#<SPECIAL OPERATOR IF>");
make_test ("(fdefinition 'fun)", "#<FUNCTION FUN>");
make_test ("(fdefinition 'car)", "#<FUNCTION BUILTIN CAR>");
make_test ("(funcall (complement #'numberp) \"\")", "T");
make_test ("(symbol-name 'aaa)", "\"AAA\"");
make_test ("(symbol-name :bbb)", "\"BBB\"");
make_test ("(symbol-package 'if)", "#<PACKAGE \"COMMON-LISP\">");
make_test ("(symbol-package 'caar)", "#<PACKAGE \"COMMON-LISP\">");
make_test ("(symbol-package 'ggg)", "#<PACKAGE \"COMMON-LISP-USER\">");
make_test ("(symbol-package :eee)", "#<PACKAGE \"KEYWORD\">");
make_test ("(symbol-package '#:aaa)", "NIL");
make_test ("(symbol-package (make-symbol \"aaa\"))", "NIL");
make_test ("(symbol-plist 's)", "NIL");
make_test ("(setf (symbol-plist 's) (list :a 0 :b 1))", "(:A 0 :B 1)");
make_test ("(symbol-plist 's)", "(:A 0 :B 1)");
make_test ("(get 's :b)", "1");
make_test ("(special-operator-p 'if)", "T");
make_test ("(special-operator-p 'car)", "NIL");
make_test ("(special-operator-p 'aaa)", "NIL");
make_test ("(let ((varb 0)) (declare (special varb)) (makunbound 'varb) (boundp 'varb))", "NIL");
make_test ("(defparameter ha 0)", "HA");
make_test ("(boundp 'ha)", "T");
make_test ("(makunbound 'ha)", "HA");
make_test ("(boundp 'ha)", "NIL");
make_test ("(defun what ())", "WHAT");
make_test ("(fboundp 'what)", "T");
make_test ("(fmakunbound 'what)", "WHAT");
make_test ("(fboundp 'what)", "NIL");
make_test ("(equal #\\a #\\a)", "T");
make_test ("(equal 0 0)", "T");
make_test ("(equal 0 0.0)", "NIL");
make_test ("(equal (cons 1 2) 1)", "NIL");
make_test ("(equal (cons 'a 'b) (cons 'a 'b))", "T");
make_test ("(equal '(1 2 3) '(1 2 3))", "T");
make_test ("(equal '(1 2 . 4) '(1 2 . 3))", "NIL");
make_test ("(equal (make-array '(1 2 3)) (make-array '(1 2 3)))", "NIL");
make_test ("(equalp 0 0)", "T");
make_test ("(equalp 0 0.0)", "T");
make_test ("(equalp '(1 2 3) '(1 2 3))", "T");
make_test ("(equalp '(1 2 . 4) '(1 2 . 3))", "NIL");
make_test ("(equalp (make-array '(1 2 3)) (make-array '(1 2 3)))", "T");
make_test ("(string \"ccc\")", "\"ccc\"");
make_test ("(string 'ddd)", "\"DDD\"");
make_test ("(string #\\F)", "\"F\"");
make_test ("(string= \"aaa\" \"aaa\")", "T");
make_test ("(string= \"aaa\" \"aab\")", "NIL");
make_test ("(string/= \"aaa\" \"aaa\")", "NIL");
make_test ("(string/= \"aaa\" \"aab\")", "T");
make_test ("(char= #\\a)", "T");
make_test ("(char= #\\a #\\b)", "NIL");
make_test ("(char= #\\a #\\a #\\a)", "T");
make_test ("(char= #\\a #\\a #\\b)", "NIL");
make_test ("(char-equal #\\a)", "T");
make_test ("(char-equal #\\a #\\a)", "T");
make_test ("(char-equal #\\a #\\A)", "T");
make_test ("(char-equal #\\a #\\A #\\a)", "T");
make_test ("(char-equal #\\a #\\A #\\b)", "NIL");
make_test ("(char-upcase #\\a)", "#\\A");
make_test ("(char-upcase #\\A)", "#\\A");
make_test ("(char-upcase #\\3)", "#\\3");
make_test ("(char-downcase #\\B)", "#\\b");
make_test ("(char-downcase #\\b)", "#\\b");
make_test ("(char-downcase #\\@)", "#\\@");
make_test ("(alpha-char-p #\\a)", "T");
make_test ("(alpha-char-p #\\3)", "NIL");
make_test ("(alphanumericp #\\a)", "T");
make_test ("(alphanumericp #\\3)", "T");
make_test ("(alphanumericp #\\#)", "NIL");
make_test ("(digit-char 2)", "#\\2");
make_test ("(digit-char 10)", "NIL");
make_test ("(digit-char 10 16)", "#\\A");
make_test ("(digit-char 16 16)", "NIL");
make_test ("(digit-char-p #\\2)", "2");
make_test ("(digit-char-p #\\A)", "NIL");
make_test ("(digit-char-p #\\a)", "NIL");
make_test ("(digit-char-p #\\A 16)", "10");
make_test ("(digit-char-p #\\a 16)", "10");
make_test ("(digit-char-p #\\g 16)", "NIL");
make_test ("(string-upcase \"Hello\")", "\"HELLO\"");
make_test ("(string-downcase \"Hello\")", "\"hello\"");
make_test ("(string-capitalize \"this is a Good day\")", "\"This Is A Good Day\"");
make_test ("(string-left-trim '(#\\a #\\b) \"aabbabahello\")", "\"hello\"");
make_test ("(string-left-trim '(#\\a #\\b) \"aabbab\")", "\"\"");
make_test ("(string-right-trim '(#\\a #\\b) \"helloaabbab\")", "\"hello\"");
make_test ("(string-right-trim '(#\\a #\\b) \"aabbab\")", "\"\"");
make_test ("(string-trim '(#\\a #\\b) \"baahelloaabbab\")", "\"hello\"");
make_test ("(car '(1 2))", "1");
make_test ("(cdr '(1 2))", "(2)");
make_test ("(car ())", "NIL");
make_test ("(cdr ())", "NIL");
make_test ("(setf tt (list 'a 'b 'd))", "(A B D)");
make_test ("(rplaca tt 'f)", "(F B D)");
make_test ("tt", "(F B D)");
make_test ("(rplacd tt 'g)", "(F . G)");
make_test ("tt", "(F . G)");
make_test ("(nth 0 '(0))", "0");
make_test ("(nth 1 '(0))", "NIL");
make_test ("(nthcdr 1 ())", "NIL");
make_test ("(nthcdr 2 '(a b c))", "(C)");
make_test ("(nthcdr 1 '(0 . 1))", "1");
make_test ("(nth-value 0 (values 10 9))", "10");
make_test ("(nth-value 1 (values 10 9))", "9");
make_test ("(nth-value 2 (values 10 9))", "NIL");
make_test ("(nth-value 3 (values 10 9))", "NIL");
make_test ("(elt '(0 3) 1)", "3");
make_test ("(elt \"abc\" 0)", "#\\a");
make_test ("(elt #(3 2 1) 1)", "2");
make_test ("(aref \"abc\" 2)", "#\\c");
make_test ("(aref #(1 2 3) 1)", "2");
make_test ("(aref (make-array nil))", "NIL");
make_test ("(aref (make-array '(1 2 3)) 0 1 2)", "NIL");
make_test ("(row-major-aref #(0 1 2) 1)", "1");
make_test ("(row-major-aref (make-array '(3 2 1)) 5)", "NIL");
make_test ("(char \"àbcdef\" 2)", "#\\c");
make_test ("(subseq nil 0)", "NIL");
make_test ("(subseq \"hello\" 1)", "\"ello\"");
make_test ("(subseq \"hello\" 1 3)", "\"el\"");
make_test ("(subseq \"hello\" 1 1)", "\"\"");
make_test ("(subseq #(0 1 2) 1 3)", "#(1 2)");
make_test ("(subseq #(0 1 2) 1 1)", "#()");
make_test ("(subseq '(0 1 2) 1 nil)", "(1 2)");
make_test ("(subseq '(0 1 2) 1 1)", "NIL");
make_test ("(length ())", "0");
make_test ("(length \"aaa\")", "3");
make_test ("(length #(1 2))", "2");
make_test ("(length '(0 1 2))", "3");
make_test ("(list-length '(0 1 2 3))", "4");
make_test ("(make-array 4)", "#(NIL NIL NIL NIL)");
make_test ("(make-array '(1 2 3))", "#<ARRAY, RANK 3>");
make_test ("(make-array nil)", "#<ARRAY, RANK 0>");
make_test ("(vector)", "#()");
make_test ("(vector 'a 'b \"\")", "#(A B \"\")");
make_test ("(array-has-fill-pointer-p \"aaa\")", "NIL");
make_test ("(array-rank #())", "1");
make_test ("(array-rank (make-array nil))", "0");
make_test ("(array-rank (make-array '(1 2)))", "2");
make_test ("(array-dimensions \"aaa\")", "(3)");
make_test ("(array-dimensions #(1 2 3 4))", "(4)");
make_test ("(array-dimension \"aaa\" 0)", "3");
make_test ("(array-total-size \"aaa\")", "3");
make_test ("(array-in-bounds-p (make-array nil))", "T");
make_test ("(array-in-bounds-p \"aaa\" 3)", "NIL");
make_test ("(array-in-bounds-p (make-array '(1 2 3)) 0 1 2)", "T");
make_test ("(array-in-bounds-p (make-array '(1 2 3)) 1 1 2)", "NIL");
make_test ("(array-row-major-index \"aaa\" 1)", "1");
make_test ("(array-row-major-index #(0 1 2) 0)", "0");
make_test ("(array-row-major-index (make-array nil))", "0");
make_test ("(array-row-major-index (make-array '(1 2 3)) 0 0 0)", "0");
make_test ("(array-row-major-index (make-array '(2 3 4)) 0 0 1)", "1");
make_test ("(array-row-major-index (make-array '(2 3 4)) 0 2 0)", "8");
make_test ("(array-row-major-index (make-array '(2 3 4)) 1 0 0)", "12");
make_test ("(defparameter tbl (make-hash-table))", "TBL");
make_test ("tbl", "#<HASH-TABLE EQ 0/1024>");
make_test ("(hash-table-size tbl)", "1024");
make_test ("(hash-table-count tbl)", "0");
make_test ("(hash-table-test tbl)", "EQ");
make_test ("(gethash 'k tbl)", "NIL\nNIL");
make_test ("(setf (gethash 'k tbl) 10)", "10");
make_test ("(gethash 'k tbl)", "10\nT");
make_test ("(remhash 'k tbl)", "T");
make_test ("(gethash 'k tbl)", "NIL\nNIL");
make_test ("(last '(1 2 3))", "(3)");
make_test ("(last '(1 2 3) 0)", "NIL");
make_test ("(last '(1 2 3) 2)", "(2 3)");
make_test ("(read-line)\nabc", "\"abc\"\nNIL", 1);
make_test ("(read-line (make-string-input-stream \"hello world\"))", "\"hello world\"\nT");
make_test ("(read-line (make-string-input-stream \"hello world\n" .
	   "\"))", "\"hello world\"\nNIL");
make_test ("(read)\n\"hello\"\"world\" 12 `(1 2 3) #\\c aa  ", "\"hello\"", 1);
make_test ("(read)", "\"world\"");
make_test ("(read)", "12");
make_test ("(read)", "`(1 2 3)");
make_test ("(read)", "#\\c");
make_test ("(read)", "AA");
make_test ("(read-line)", "\" \"\nNIL");
make_test ("(defparameter inp (make-string-input-stream \"\\\"hello\\\"\\\"world\\\" 12 `(1 2 3) #\\\\c aa  \"))", "INP");
make_test ("(read inp)", "\"hello\"");
make_test ("(read inp)", "\"world\"");
make_test ("(read inp)", "12");
make_test ("(read inp)", "`(1 2 3)");
make_test ("(read inp)", "#\\c");
make_test ("(read inp)", "AA");
make_test ("(read-line inp)", "\" \"\nT");
make_test ("(read (make-string-input-stream \"hello\"))", "HELLO");
make_test ("(read (make-string-input-stream \"123\"))", "123");
make_test ("(read-preserving-whitespace)\n\"hello\"\"world\" 12 `(1 2 3) #\\c aa  ", "\"hello\"", 1);
make_test ("(read-preserving-whitespace)", "\"world\"");
make_test ("(read-preserving-whitespace)", "12");
make_test ("(read-preserving-whitespace)", "`(1 2 3)");
make_test ("(read-preserving-whitespace)", "#\\c");
make_test ("(read-preserving-whitespace)", "AA");
make_test ("(read-line)", "\"  \"\nNIL");
make_test ("(defparameter inp2 (make-string-input-stream \"\\\"hello\\\"\\\"world\\\" 12 `(1 2 3) #\\\\c aa  \"))", "INP2");
make_test ("(read-preserving-whitespace inp2)", "\"hello\"");
make_test ("(read-preserving-whitespace inp2)", "\"world\"");
make_test ("(read-preserving-whitespace inp2)", "12");
make_test ("(read-preserving-whitespace inp2)", "`(1 2 3)");
make_test ("(read-preserving-whitespace inp2)", "#\\c");
make_test ("(read-preserving-whitespace inp2)", "AA");
make_test ("(read-line inp2)", "\"  \"\nT");
make_test ("(read-preserving-whitespace (make-string-input-stream \"hello\"))", "HELLO");
make_test ("(read-preserving-whitespace (make-string-input-stream \"123\"))", "123");
make_test ("(eval '(write \"\"))", "\"\"\n\"\"");
make_test ("(setq var 10)", "10");
make_test ("(let ((var 12)) (eval 'var))", "10");
make_test ("(write \"\")", "\"\"\n\"\"");
make_test ("(write-string \"aaa\\n\")", "aaan\n\"aaan\"");
make_test ("(write-string \"\n" .
	   "\")", "\n\"\n\"");
make_test ("(write-char #\\a)", "a\n#\\a");
make_test ("(write-char #\\newline)", "\n#\\Newline");
make_test ("(write-byte (char-code #\\a) *standard-output*)", "a97");
make_test ("(fresh-line)", "NIL");
make_test ("(terpri)", "\nNIL");
make_test ("(write-line \"aaa\")", "aaa\n\"aaa\"");
make_test ("(write-sequence \"hello world\" *standard-output* :start 1 :end 5)", "ello\n\"hello world\"");
make_test ("(prin1 '|Aa,a|)", "|Aa,a|\n|Aa,a|");
make_test ("(prin1 #\\a)", "#\\a\n#\\a");
make_test ("(prin1 \"aaa\" *standard-output*)", "\"aaa\"\n\"aaa\"");
make_test ("(princ '|Aa,a|)", "Aa,a\n|Aa,a|");
make_test ("(princ #\\a)", "a\n#\\a");
make_test ("(princ \"aaa\" *standard-output*)", "aaa\n\"aaa\"");
make_test ("(print \"aaa\" *standard-output*)", "\n\"aaa\" \n\"aaa\"");
make_test ("(let ((c 0)) (loop (if (= c 2) (return 0)) (incf c)))", "0");
make_test ("(loop initially (write 'hello) for i from 1 to 5 finally (write 'world) do (write i))", "HELLO12345WORLD\nNIL");
make_test ("(loop for i from 1 to 5 do (write i))", "12345\nNIL");
make_test ("(loop for i from 1 below 5 do (write i))", "1234\nNIL");
make_test ("(loop for i from 10 downto 5 by 2 do (write i))", "1086\nNIL");
make_test ("(loop for i upfrom 5 to 10 do (write i))", "5678910\nNIL");
make_test ("(loop for i from 5 upto 11 for j downfrom 20 above 15 do (write i) (write j))", "520619718817916\nNIL");
make_test ("(loop named foo for i from 0 do (write i) (if (> i 4) (return-from foo 0)))", "012345\n0");
make_test ("(loop as i from 0 do (if (> i 5) (return 10) (write i)) finally (write 'finished))", "012345\n10");
make_test ("(format t \"aaa\")", "aaa\nNIL");
make_test ("(format t \"aaa~~\")", "aaa~\nNIL");
make_test ("(format t \"aaa~%~%\")", "aaa\n\nNIL");
make_test ("(format t \"aa~&~&bb\")", "aa\nbb\nNIL");
make_test ("(format t \"the number is ~a and the list is ~s\" 10 '(1 2 3))",
	   "the number is 10 and the list is (1 2 3)\nNIL");
make_test ("(defparameter str (open \"writetest\" :direction :output :direction :input))", "STR");
make_test ("(write \"hello\" :stream str)", "\"hello\"");
make_test ("(write #\\a :stream str :stream *standard-input*)", "#\\a");
make_test ("(write 1 :stream str)", "1");
make_test ("(write 1/2 :stream str)", "1/2");
make_test ("(write 0.1 :stream str)", "0.1");
make_test ("(write (complex 1 1) :stream str)", "#C(1 1)");
make_test ("(write-char #\\newline str)", "#\\Newline");
make_test ("(write '(1 2 3) :stream str)", "(1 2 3)");
make_test ("(write #(1 2 3) :stream str)", "#(1 2 3)");
make_test ("(close str)", "T");
make_test ("(make-string-input-stream \"aaa\")", "#<STREAM ?>");
make_test ("(defparameter str (make-string-output-stream))", "STR");
make_test ("(write 'aaa :stream str)", "AAA");
make_test ("(write \"bbb\" :stream str)", "\"bbb\"");
make_test ("(write 'ccc :stream str)", "CCC");
make_test ("(write #\\c :stream str)", "#\\c");
make_test ("(write 100 :stream str)", "100");
make_test ("(write 1.2 :stream str)", "1.2");
make_test ("(get-output-stream-string str)", "\"AAA\\\"bbb\\\"CCC#\\\\c1001.2\"");
make_test ("(get-output-stream-string str)", "\"\"");
make_test ("(gensym)", "#:G21");
make_test ("(gensym)", "#:G22");
make_test ("(gensym 5)", "#:G5");
make_test ("(gensym \"A\")", "#:A23");
make_test ("(gensym)", "#:G24");
make_test ("(gentemp)", "T1");
make_test ("(gentemp \"S\")", "S2");
make_test ("'s3", "S3");
make_test ("(gentemp \"S\")", "S4");
make_test ("(second '(0))", "NIL");
make_test ("(fifth '(0 1 2 3 4))", "4");
make_test ("(endp '(1 . 2))", "NIL");
make_test ("(endp ())", "T");
make_test ("(when t \"\" \"\" \"\")", "\"\"");
make_test ("(when nil \"\" \"\" \"\")", "NIL");
make_test ("(unless nil \"\" \"\" \"\")", "\"\"");
make_test ("(the integer 0)", "0");
make_test ("(the integer (values 0 1))", "0\n1");
make_test ("(prog1 2 (write \"hello\"))", "\"hello\"\n2");
make_test ("(prog2 2 3 (write \"hello\"))", "\"hello\"\n3");
make_test ("(destructuring-bind ((x)) '((0)) x)", "0");
make_test ("(destructuring-bind (x (y (z))) '(0 (1 (2))) (list x y z))", "(0 1 2)");
make_test ("(destructuring-bind (x (y z) w) '(0 ((1 2) 3) 4) (list x y z w))", "(0 (1 2) 3 4)");
make_test ("(defstruct ship x y)", "SHIP");
make_test ("#'make-ship", "#<FUNCTION MAKE-SHIP>");
make_test ("#'ship-x", "#<FUNCTION SHIP-X>");
make_test ("(defparameter s1 (make-ship))", "S1");
make_test ("s1", "#<STRUCTURE OF CLASS SHIP>");
make_test ("(ship-x s1)", "NIL");
make_test ("(ship-y s1)", "NIL");
make_test ("(setf (ship-x s1) 0)", "0");
make_test ("(setf (ship-y s1) 1)", "1");
make_test ("(ship-x s1)", "0");
make_test ("(ship-y s1)", "1");
make_test ("(tagbody\n" .
	   "  (write \"1\")\n" .
	   "  (go jmp)\n" .
	   "  (write \"2\")\n" .
	   "  jmp\n" .
	   "  (write \"3\"))", "\"1\"\"3\"\nNIL");
make_test ("(tagbody 1 (go 3) 2 3 (go 4) 4 (write \"\"))", "\"\"\nNIL");
make_test ("(tagbody 1 2 (tagbody 3 (go 4)) 4 (write \"\"))", "\"\"\nNIL");
make_test ("(tagbody (write 1) (block nil (write 2) (go c)) c (write 4))", "124\nNIL");
make_test ("(block test (write 1) (write 2) (return-from test 10) (write 3))", "12\n10");
make_test ("(block test (write 1) (block test2 (write 2) (return-from test 10) (write 3)))", "12\n10");
make_test ("(block test (write 1) (block test2 (write 2) (return-from test (values 10 11)) (write 3)))", "12\n10\n11");
make_test ("(block nil (return (values 11 12)))", "11\n12");
make_test ("(defun test () (write 1) (write 2) (return-from test 10) (write 3))", "TEST");
make_test ("(test)", "12\n10");
make_test ("(defun f () (throw 'label (values 0 1 2)))", "F");
make_test ("(catch 'label (f))", "0\n1\n2");
make_test ("(block nil (handler-bind ((file-error (lambda (e) (write 0))) (division-by-zero (lambda (e) (return 1)))) (/ 1 0)))", "1");
make_test ("(block nil (handler-bind ((division-by-zero (lambda (e) (write 0))) (arithmetic-error (lambda (e) (write 1))) (error (lambda (e) (return 2)))) (/ 1 0)))", "01\n2");
make_test ("(block nil (handler-bind ((division-by-zero (lambda (e) (return 0)))) (handler-bind ((arithmetic-error (lambda (e) (write 1)))) (/ 1 0))))", "1\n0");
make_test ("(block test (unwind-protect 0 (write 'whatever)))", "WHATEVER\n0");
make_test ("(block test (unwind-protect (return-from test 0) (write 'whatever)))", "WHATEVER\n0");
make_test ("(block test (unwind-protect (return-from test (values 0 1)) (write 'whatever)))", "WHATEVER\n0\n1");
make_test ("(cddr '(0 1 2))", "(2)");
make_test ("(cddddr '(0 1 2 3 4))", "(4)");
make_test ("(cadddr '(0 1 2 3 4))", "3");
make_test ("(caar '((4) 1 2 3))", "4");
make_test ("(incf c)", "6");
make_test ("(incf c 3)", "9");
make_test ("(let ((i 0)) (incf i))", "1");
make_test ("(decf c)", "8");
make_test ("(decf c c)", "0");
make_test ("(and)", "T");
make_test ("(and t)", "T");
make_test ("(and (+ 1 2))", "3");
make_test ("(and t t t)", "T");
make_test ("(and t nil t)", "NIL");
make_test ("(and (= 1 1) t)", "T");
make_test ("(and t (= 1 2) t)", "NIL");
make_test ("(and 1 2 (+ 1 2))", "3");
make_test ("(and 0 (values 1 2))", "1\n2");
make_test ("(or)", "NIL");
make_test ("(or (+ 1 2))", "3");
make_test ("(or nil nil nil)", "NIL");
make_test ("(or (= 1 2) t)", "T");
make_test ("(or t nil t)", "T");
make_test ("(or nil nil (+ 1 2))", "3");
make_test ("(or nil nil (+ 1 2) nil)", "3");
make_test ("(or (values 1 2))", "1\n2");
make_test ("(or (values 1 2) nil)", "1");
make_test ("(concatenate 'string)", "\"\"");
make_test ("(concatenate 'string \"\")", "\"\"");
make_test ("(concatenate 'string \"aa\" \"bb\")", "\"aabb\"");
make_test ("(do (x (y 0) (z 0 (1+ z))) ((>= z 10) (values x y z)) (write z))", "0123456789\nNIL\n0\n10");
make_test ("(do ((x 0 (1+ x)) (y 0 (1+ x))) ((>= y 10)) (write y))", "0123456789\nNIL");
make_test ("(do ((dd 0 (1+ dd))) (nil) (if (> dd 5) (return 10) (write dd)))", "012345\n10");
make_test ("(do* (x (y 0) (z 0 (1+ z))) ((>= z 10) (values x y z)) (write z))", "0123456789\nNIL\n0\n10");
make_test ("(do* ((x 0 (1+ x)) (y x (1+ x))) ((>= y 10)) (write y))", "023456789\nNIL");
make_test ("(do* ((dd 0 (1+ dd))) (nil) (if (> dd 5) (return 10) (write dd)))", "012345\n10");
make_test ("(dotimes (i 0 i))", "0");
make_test ("(dotimes (tmp 10 tmp))", "10");
make_test ("(dotimes (i 10) (write i) (if (= i 5) (return (values 11 12))))", "012345\n11\n12");
make_test ("(dolist (i nil) (write i))", "NIL");
make_test ("(dolist (i nil (values 0 1)))", "0\n1");
make_test ("(dolist (i '(0 1 2 3)) (write i))", "0123\nNIL");
make_test ("(dolist (i '(0 1 2 3)) (if (= i 2) (return (values 12 13))) (write i))", "01\n12\n13");
make_test ("(mapcar #'car '((1 a) (2 b) (3 c)))", "(1 2 3)");
make_test ("(mapcar #'abs '(3 -4 2 -5 -6))", "(3 4 2 5 6)");
make_test ("(mapcar #'cons '(a b c) '(1 2 3))", "((A . 1) (B . 2) (C . 3))");
make_test ("(let ((s 0)) (write (mapcar (lambda (s) s) '(1 2 3))) s)", "(1 2 3)\n0");
make_test ("(remove 3 '(1 2 3 4 3 2 1))", "(1 2 4 2 1)");
make_test ("(remove #\\e \"abcdefg\")", "\"abcdfg\"");
make_test ("(remove-if #'numberp #(1 2 3))", "#()");
make_test ("(remove-if-not #'listp #(()))", "#(NIL)");
make_test ("(reverse '(0 1 2 3))", "(3 2 1 0)");
make_test ("(reverse \"hello\")", "\"olleh\"");
make_test ("(reverse #(0 1 2 3))", "#(3 2 1 0)");
make_test ("(nreverse '(0 1 2 3))", "(3 2 1 0)");
make_test ("(nreverse \"hello\")", "\"olleh\"");
make_test ("(nreverse #(0 1 2 3))", "#(3 2 1 0)");
make_test ("(adjoin 0 '(0 1 2))", "(0 1 2)");
make_test ("(adjoin 0 '(1 2))", "(0 1 2)");
make_test ("(fill \"abcdef\" #\\g)", "\"gggggg\"");
make_test ("(fill '(0 1 2 3 4 5) 0 :start 1 :end 4)", "(0 0 0 0 4 5)");
make_test ("(defparameter ll NIL)", "LL");
make_test ("(push (1+ 0) ll)", "(1)");
make_test ("ll", "(1)");
make_test ("(pop ll)", "1");
make_test ("ll", "NIL");
make_test ("(cond (t (+ 1 2)))", "3");
make_test ("(cond (nil 2) (t 3) (nil 4))", "3");
make_test ("(cond ((= 2 (+ 1 2)) (+ 0 10)) ((= 3 (+ 1 2)) (+ 12) (+ 0 11)))", "11");
make_test ("(case (+ 1 2) (3 (+ 1 5)))", "6");
make_test ("(case (+ 1 2) (4 (+ 1 5)))", "NIL");
make_test ("(case (+ 1 2) (2 10) ((4 3) (+ 1 5)))", "6");
make_test ("(case (+ 1 2) (2 10) ((4 3) (+ 1 5)) (otherwise 12))", "6");
make_test ("(case (+ 1 2) (2 10) ((4 5) (+ 1 5)) (otherwise 11 12))", "12");
make_test ("(multiple-value-bind (x y) (floor 2 1) (list x y))", "(2 0)");
make_test ("(multiple-value-bind (x) (floor 2 1) (list x))", "(2)");
make_test ("(multiple-value-bind (x y z) (floor 2 1) (list x y z))", "(2 0 NIL)");
make_test ("(every #'evenp '(0 2 4))", "T");
make_test ("(every #'< '(0 1 2) '(1 1))", "NIL");
make_test ("(some #'< '(0 1 2) '(1 1))", "T");
make_test ("(notany #'oddp '(1 3 4))", "NIL");
make_test ("(notevery #'oddp '(1 3 4))", "T");
make_test ("(member 3 '(1 2 3 4))", "(3 4)");
make_test ("(member 5 '(1 2 3))",  "NIL");
make_test ("(member-if #'evenp '(1 2 3 4))", "(2 3 4)");
make_test ("(member-if #'listp '(nil))", "(NIL)");
make_test ("(member-if-not #'evenp '(0 2 4))", "NIL");
make_test ("(find #\\3 \"012\")", "NIL");
make_test ("(find #\\3 \"0123\")", "#\\3");
make_test ("(find-if 'digit-char-p \"0123\")", "#\\0");
make_test ("(find-if-not 'digit-char-p \"0123\")", "NIL");
make_test ("(assoc 2 '((0 . 10) (1 . 11) (2 . 12)))", "(2 . 12)");
make_test ("(assoc 3 '((0 . 10) (1 . 11) (2 . 12)))", "NIL");
make_test ("(assoc-if 'zerop '((0 . 10) (1 . 11) (2 . 12)))", "(0 . 10)");
make_test ("(assoc-if-not 'zerop '((0 . 10) (1 . 11) (2 . 12)))", "(1 . 11)");
make_test ("(position 2 '(3 2 1 0))", "1");
make_test ("(position 4 '(3 2 1 0))", "NIL");
make_test ("(position-if 'evenp '(3 2 1 0))", "1");
make_test ("(position-if 'evenp '(3 1))", "NIL");
make_test ("(position-if-not #'evenp '(3 1))", "0");
make_test ("(count #\\a \"abcabc\")", "2");
make_test ("(count 2 '(1 2 3))", "1");
make_test ("(count-if #'upper-case-p \"aAbBcCdD\")", "4");
make_test ("(count-if-not #'evenp #(0 1 2))", "1");
make_test ("(machine-type)", "NIL");
make_test ("(machine-instance)", "NIL");
make_test ("(machine-version)", "NIL");

make_test ("#+common-lisp \"\" \"\"", "\"\"\n\"\"");
make_test ("#-common-lisp \"\" \"\"", "\"\"");
make_test ("#+(and common-lisp foo) \"\" \"\"", "\"\"");
make_test ("#+(or common-lisp foo) \"\" \"\"", "\"\"\n\"\"");
make_test ("#-(not common-lisp) \"\" \"\"", "\"\"\n\"\"");
make_test ("#+common-lisp 'x", "X");
make_test ("#+common-lisp 0", "0");
make_test ("#-common-lisp 0 0", "0");
make_test ("#-common-lisp 'aaa 'bbb", "BBB");
make_test ("#+common-lisp '(1 2 3) \"\"", "(1 2 3)\n\"\"");
make_test ("#+foo '(1 2 3) \"\"", "\"\"");
make_test ("#+foo '((1 2) 3) 0", "0");
make_test ("#+(and\ncommon-lisp) 0 0", "0\n0");
make_test ("#-(\nand common-lisp) 0 0", "0");
make_test ("#+\n(and common-lisp) 0 0", "0\n0");
make_test ("#+common-lisp (\n)", "NIL");
make_test ("#+common-lisp '(1\n2)", "(1 2)");
make_test ("#-common-lisp (\n) 0", "0");
make_test ("#+foo \"aaa\nbbb\" 0", "0");
make_test ("#+foo (1\n2 (3)) 0", "0");

make_test ("*package*", "#<PACKAGE \"COMMON-LISP-USER\">");
make_test ("(find-package *package*)", "#<PACKAGE \"COMMON-LISP-USER\">");
make_test ("(find-package \"CL\")", "#<PACKAGE \"COMMON-LISP\">");
make_test ("(find-package 'cl-user)", "#<PACKAGE \"COMMON-LISP-USER\">");
make_test ("(find-package #\\a)", "NIL");
make_test ("(package-name \"CL\")", "\"COMMON-LISP\"");
make_test ("(package-name *package*)", "\"COMMON-LISP-USER\"");
make_test ("(package-nicknames *package*)", "(\"CL-USER\")");
make_test ("(package-nicknames (symbol-package :fff))", "NIL");
make_test ("(package-use-list *package*)", "(#<PACKAGE \"COMMON-LISP\">)");
make_test ("(package-use-list (symbol-package :fff))", "NIL");
make_test ("(package-used-by-list *package*)", "NIL");
make_test ("(package-used-by-list (symbol-package 'if))", "(#<PACKAGE \"COMMON-LISP-USER\">)");
make_test ("(list-all-packages)", "(#<PACKAGE \"COMMON-LISP-USER\"> #<PACKAGE \"COMMON-LISP\"> #<PACKAGE \"KEYWORD\">)");
make_test ("(make-package \"test\")", "#<PACKAGE \"test\">");
make_test ("(list-all-packages)", "(#<PACKAGE \"test\"> #<PACKAGE \"COMMON-LISP-USER\"> #<PACKAGE \"COMMON-LISP\"> #<PACKAGE \"KEYWORD\">)");
make_test ("(rename-package \"test\" \"test\")", "#<PACKAGE \"test\">");
make_test ("(rename-package \"test\" \"TEST\" '(nick1 #\\n \"nick3\"))", "#<PACKAGE \"TEST\">");
make_test ("(list-all-packages)", "(#<PACKAGE \"TEST\"> #<PACKAGE \"COMMON-LISP-USER\"> #<PACKAGE \"COMMON-LISP\"> #<PACKAGE \"KEYWORD\">)");
make_test ("(package-nicknames \"TEST\")", "(\"NICK1\" \"n\" \"nick3\")");
make_test ("(in-package \"CL\")", "#<PACKAGE \"COMMON-LISP\">");
make_test ("*package*", "#<PACKAGE \"COMMON-LISP\">");
make_test ("(in-package cl-user)", "#<PACKAGE \"COMMON-LISP-USER\">");
make_test ("*package*", "#<PACKAGE \"COMMON-LISP-USER\">");
make_test ("(use-package 'cl)", "T");
make_test ("(make-package \"newtest\")", "#<PACKAGE \"newtest\">");
make_test ("(in-package \"newtest\")", "#<PACKAGE \"newtest\">");
make_test ("(cl:package-use-list cl:*package*)", "COMMON-LISP:NIL");
make_test ("(cl:use-package 'cl)", "T");
make_test ("(cl:use-package nil \"newtest\")", "T");
make_test ("(cl:package-use-list cl:*package*)", "(#<PACKAGE \"COMMON-LISP\">)");
make_test ("(cadr '(0 1))", "1");
make_test ("(cl:import 'cl:car)", "T");
make_test ("(cl:import '(cl:car) '|newtest|)", "T");
make_test ("(cl:export nil \"newtest\")", "T");
make_test ("(cl:export 'cdr)", "T");
make_test ("'|newtest|:cdr", "CDR");
make_test ("(cl:export 'jjj)", "T");
make_test ("(cl:export '(jjj) cl:*package*)", "T");
make_test ("'|newtest|:jjj", "JJJ");
make_test ("(cl:unexport nil \"newtest\")", "T");
make_test ("(cl:unexport '(jjj) cl:*package*)", "T");
make_test ("(car ())", "NIL");
make_test ("(cl:unuse-package 'cl)", "COMMON-LISP:T");
make_test ("(cl:unuse-package cl:nil \"newtest\")", "COMMON-LISP:T");
make_test ("(cl:package-use-list cl:*package*)", "COMMON-LISP:NIL");
make_test ("(car ())", "COMMON-LISP:NIL");
make_test ("(cl:in-package cl-user)", "#<PACKAGE \"COMMON-LISP-USER\">");
make_test ("(defpackage \"aaa\" (:nicknames \"bbb\" |ccc|) (:use cl-user) (:export \"sym\") (:intern \"eee\" \"fff\") (:import-from \"CL\" car caar) (:import-from \"CL-USER\" al-argc))", "#<PACKAGE \"aaa\">");
make_test ("(package-nicknames \"aaa\")", "(\"bbb\" \"ccc\")");
make_test ("(package-use-list \"aaa\")", "(#<PACKAGE \"COMMON-LISP-USER\">)");
make_test ("'|aaa|:|sym|", "|aaa|:|sym|");
make_test ("(defpackage \"aaa\" (:nicknames \"bbb\" |ccc|) (:use cl-user) (:export \"sym\") (:intern \"eee\" \"fff\") (:import-from \"CL\" car caar) (:import-from \"CL-USER\" al-argc))", "#<PACKAGE \"aaa\">");


# arithmetic tests

make_test ("2/4", "1/2");
make_test ("0.1s2", "10.0");
make_test ("0.0", "0.0");
make_test ("0.1", "0.1");
make_test ("(+)", "0");
make_test ("(+ 1)", "1");
make_test ("(+ 1 2 3)", "6");
make_test ("(+ 1/2 2 3 1000)", "2011/2");
make_test ("(+ a b)", "17");
make_test ("(+ .1 .1 .1)", "0.3");
make_test ("(- 3.5)", "-3.5");
make_test ("(- 3 4.5)", "-1.5");
make_test ("(*)", "1");
make_test ("(* 1)", "1");
make_test ("(* 3 5)", "15");
make_test ("(/ 12 4)", "3");
make_test ("(/ 13 4)", "13/4");
make_test ("(/ -8)", "-1/8");
make_test ("(/ 3 4 5)", "3/20");
make_test ("(/ 0.5)", "2.0");
make_test ("(/ 60 -2 3 5.0)", "-2.0");
make_test ("(floor 1)", "1\n0");
make_test ("(floor 1/2)", "0\n1/2");
make_test ("(ffloor 1)", "1.0\n0");
make_test ("(ffloor 1/2)", "0.0\n1/2");
make_test ("(ceiling 2)", "2\n0");
make_test ("(ceiling 1/2 1)", "1\n-1/2");
make_test ("(fceiling 2)", "2.0\n0");
make_test ("(fceiling 1/2 1)", "1.0\n-1/2");
make_test ("(truncate -3 2)", "-1\n-1");
make_test ("(truncate -1)", "-1\n0");
make_test ("(truncate -.1)", "0\n-0.1");
make_test ("(ftruncate -3 2)", "-1.0\n-1");
make_test ("(ftruncate -1)", "-1.0\n0");
make_test ("(ftruncate -.1)", "0.0\n-0.1");
make_test ("(round 5)", "5\n0");
make_test ("(round .5)", "0\n0.5");
make_test ("(round -1.5)", "-2\n0.5");
make_test ("(fround 5)", "5.0\n0");
make_test ("(fround .5)", "0.0\n0.5");
make_test ("(fround -1.5)", "-2.0\n0.5");
make_test ("(numerator 2)", "2");
make_test ("(numerator 2/3)", "2");
make_test ("(denominator 2)", "1");
make_test ("(denominator 2/3)", "3");
make_test ("(sqrt 9)", "3.0");
make_test ("(sqrt 9.0)", "3.0");
make_test ("(isqrt 10)", "3");
make_test ("(complex 1)", "1");
make_test ("(complex 1.0)", "#C(1.0 0.0)");
make_test ("(complex 1 1)", "#C(1 1)");
make_test ("(complex 1 0)", "1");
make_test ("(complex 1 0.0)", "#C(1.0 0.0)");
make_test ("(complex 1.0 1)", "#C(1.0 1.0)");
make_test ("(complex 1 1.0)", "#C(1.0 1.0)");
make_test ("#c (0 1)", "#C(0 1)");
make_test ("#C(1 1.0)", "#C(1.0 1.0)");
make_test ("(realpart 1)", "1");
make_test ("(realpart (complex 1 2.0))", "1.0");
make_test ("(imagpart 1)", "0");
make_test ("(imagpart (complex 1 1/2))", "1/2");
make_test ("(conjugate 1)", "1");
make_test ("(conjugate (complex 1 1))", "#C(1 -1)");
make_test ("(= 1)", "T");
make_test ("(= 1 1)", "T");
make_test ("(= 1 2)", "NIL");
make_test ("(= 1 1 1.0 1/1)", "T");
make_test ("(= 0 -0.0)", "T");
make_test ("(/= 0)", "T");
make_test ("(/= 0 1 2.0)", "T");
make_test ("(/= 0 1.0 2 1)", "NIL");
make_test ("(< 0)", "T");
make_test ("(< 0 1.0 2)", "T");
make_test ("(< 0 0.0 1)", "NIL");
make_test ("(<= 0 1 1)", "T");
make_test ("(<= 1 0)", "NIL");
make_test ("(> 1.1 0)", "T");
make_test ("(> 1 1)", "NIL");
make_test ("(>= 1 1.0)", "T");
make_test ("(min 0)", "0");
make_test ("(min 0 1 2)", "0");
make_test ("(min 0 1/2 -0.1)", "-0.1");
make_test ("(max 0)", "0");
make_test ("(max 0 1.0 2)", "2");
make_test ("(max 0 1/2 -0.1)", "1/2");
make_test ("(minusp 0)", "NIL");
make_test ("(minusp -0.5)", "T");
make_test ("(plusp 1)", "T");
make_test ("(plusp -1)", "NIL");
make_test ("(abs 0)", "0");
make_test ("(abs -0.5)", "0.5");
make_test ("(abs 1/2)", "1/2");
make_test ("(zerop -0)", "T");
make_test ("(zerop 1/2)", "NIL");
make_test ("(mod 2.5 1)", "0.5");
make_test ("(mod -2.5 1)", "0.5");
make_test ("(rem 2.5 1)", "0.5");
make_test ("(rem -2.5 1)", "-0.5");
make_test ("(evenp 0)", "T");
make_test ("(evenp 1)", "NIL");
make_test ("(oddp 0)", "NIL");
make_test ("(oddp -1)", "T");



print "\ntotal tests: " . $total_tests . "\n";
print "passed " . $passed_tests . " (" . $passed_tests / $total_tests * 100 . "%), failed " . $failed_tests . "\n";



kill 'TERM', $pid;
waitpid ($pid, 0);





sub make_test
{
    my $skip_only_first_line;
    my $i = 0;

    if (not defined ($_[2]))
    {
	$skip_only_first_line = 0;
    }
    else
    {
	$skip_only_first_line = $_[2];
    }

    print $_[0] . " -> ";

    my $in = $_[0] . "\n";

    my @inlines = split("\n", $in);

    foreach my $l (@inlines)
    {
	print $al_in $l . "\n";

	if ($i == 0 or $skip_only_first_line == 0)
	{
	    <$al_out>;
	}

	$i++;
    }

    my @expected_outlines = split("\n", $_[1]);

    my $result = 1;

    my $out;

    for ($i = 0; $i < scalar (@expected_outlines); $i++)
    {
	$out = <$al_out>;

	if (not defined ($out) and eof ($al_out))
	{
	    print "\n\nGot EOF from al, it probably crashed\n";

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
