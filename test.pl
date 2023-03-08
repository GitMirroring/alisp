#!/usr/bin/perl -w
use strict;

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
	   ")))", "((()))");
make_test ("'(\"\"`\"\")", "(\"\" `\"\")");
make_test ("'(\n" .
	   "(\n" .
	   ";\n" .
	   ")\n" .
	   ")", "(())");
make_test ("\"\"", "\"\"");
make_test ("` ( ; asd\n" .
           " \"\")", "(\"\")");
make_test ("#|\n" .
	   "|# \"\"", "\"\"");
make_test ("#|\n" .
	   "|# #|\n" .
	   "|# \"\"", "\"\"");
make_test (" #| #|\n" .
	   "|##|\n" .
	   "|#\n" .
	   "  |# \"\"", "\"\"");
make_test ("2/4", "1/2");
make_test ("0.1s2", "10");
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
make_test ("'a\\\n ", "|A\n|");
make_test (":\\asd\\\\f", ":|aSD\\\\F|");


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
make_test ("`(if ``(progn ,,1))", "(IF ``(PROGN ,,1))");
make_test ("'@", "@");
make_test ("`(,@())", "()");
make_test ("`(,.())", "()");
make_test ("`(,@() ,.())", "()");
make_test ("`(1 2 ,@() ,.() 3)", "(1 2 3)");
make_test ("``(1 2 ,,@() ,,.())", "`(1 2)");
make_test ("``(,@())", "`(,@())");
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
make_test ("#.\"\"", "\"\"");
make_test ("#p\".\"", "#P\".\"");
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
	   ")", "#(())");
make_test ("'#:a", "#:A");


# eval tests

make_test ("nil", "()");
make_test ("NIL", "()");
make_test ("t", "T");
make_test ("(progn)", "()");
make_test ("(progn 1 2 3)", "3");
make_test ("(values (+ 1 0) 2 3)", "1\n2\n3");
make_test ("(values-list '(1 2 3))", "1\n2\n3");
make_test ("(multiple-value-list (values))", "()");
make_test ("(multiple-value-list (values 1 2 3))", "(1 2 3)");
make_test ("(if nil 1)", "()");
make_test ("(common-lisp:if '(1) 2)", "2");
make_test ("(cl:eq 1 1)", "()");
make_test ("(eq 'a 'a)", "T");
make_test ("(eql 'a 'a)", "T");
make_test ("(eql 'a 'b)", "()");
make_test ("(eql 0 0)", "T");
make_test ("(eql 0 0.0)", "()");
make_test ("(eql 2 2/1)", "T");
make_test ("(eql 2.0 2.0)", "T");
make_test ("(eql #\\a #\\a)", "T");
make_test ("(eql #\\A #\\a)", "()");
make_test ("(null 1)", "()");
make_test ("(not nil)", "T");
make_test ("(cons 1 2)", "(1 . 2)");
make_test ("(list 1 2 3)", "(1 2 3)");
make_test ("(list* 0)", "0");
make_test ("(list* 0 1 2)", "(0 1 . 2)");
make_test ("(append ())", "()");
make_test ("(append '(1 2) '(3 4))", "(1 2 3 4)");
make_test ("(append '(1) (values))", "(1)");
make_test ("(let ((a 2)) a)", "2");
make_test ("(let ((x 0)) x x x)", "0");
make_test ("(flet ((a () (write \"\"))) (a))", "\"\"\n\"\"");
make_test ("(labels ((a () (write \"\"))) (a))", "\"\"\n\"\"");
make_test ("(labels ((a () (b)) (b () (write \"\"))) (a))", "\"\"\n\"\"");
make_test ("(macrolet ((w (a) `(write ,a))) (w \"\"))", "\"\"\n\"\"");
make_test ("(defconstant a 8)", "A");
make_test ("a", "8");
make_test ("(defparameter b 9)", "B");
make_test ("(defvar b (0))", "B");
make_test ("b", "9");
make_test ("(setf b 10)", "10");
make_test ("(setf b 9)", "9");
make_test ("(setf c 4)", "4");
make_test ("c", "4");
make_test ("(setf)", "()");
make_test ("(let ((c 0)) c)", "0");
make_test ("(setf c (+ c 1))", "5");
make_test ("c", "5");
make_test ("(setq)", "()");
make_test ("(setq b 9 c 4 c (+ c 1))", "5");
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
make_test ("arr", "#(() () \"aaa\")");
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
make_test ("(funcall #'+ 1 2 3)", "6");
make_test ("(funcall 'car '(1 2 3))", "1");
make_test ("(f)", "()");
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
make_test ("(g 1)", "()");
make_test ("(g 1 2)", "2");
make_test ("(defun g2 (&optional (x \"a\")) x)", "G2");
make_test ("(g2)", "\"a\"");
make_test ("(g2 4)", "4");
make_test ("(defun g3 (&optional (x \"a\" provided)) provided)", "G3");
make_test ("(g3)", "()");
make_test ("(g3 7)", "T");
make_test ("(defun g4 (x &optional y &rest z) z)", "G4");
make_test ("(g4 1 2 3 (+ 3 1) 5)", "(3 4 5)");
make_test ("(defun fun (&rest x) x)", "FUN");
make_test ("(fun)", "()");
make_test ("(defun fun (x &rest y) y)", "FUN");
make_test ("(fun 5 6 7 8)", "(6 7 8)");
make_test ("(defun fun2 (&rest r &key a b c) b)", "FUN2");
make_test ("(fun2 :b 0)", "0");
make_test ("(fun2 :a 0)", "()");
make_test ("(fun2 :c 'tt :b (+ 1 2))", "3");
make_test ("(fun2)", "()");
make_test ("(defun fun2 (&rest r &key a b c) r)", "FUN2");
make_test ("(fun2 :b (+ 1 2) :c 'tt)", "(:B 3 :C TT)");
make_test ("(fun2)", "()");
make_test ("(let ((x 0)) (defun inc () (setf x (+ x 1))))", "INC");
make_test ("(inc)", "1");
make_test ("(inc)", "2");
make_test ("(inc)", "3");
make_test ("(defmacro f () (f))", "F");
make_test ("(defmacro f () (f))", "F");
make_test ("(load \"cl.lisp\")", "T");
make_test ("(open-stream-p *standard-input*)", "T");
make_test ("(input-stream-p *standard-input*)", "T");
make_test ("(input-stream-p *standard-output*)", "()");
make_test ("(output-stream-p *standard-input*)", "()");
make_test ("(output-stream-p *standard-output*)", "T");
make_test ("(upper-case-p #\\A)", "T");
make_test ("(upper-case-p #\\a)", "()");
make_test ("(upper-case-p #\\2)", "()");
make_test ("(lower-case-p #\\A)", "()");
make_test ("(lower-case-p #\\a)", "T");
make_test ("(both-case-p #\\a)", "T");
make_test ("(both-case-p #\\1)", "()");
make_test ("(identity \"hi\")", "\"hi\"");
make_test ("(1+ 5)", "6");
make_test ("(1- .5)", "-0.5");
make_test ("(typep '(1 . 2) 'cons)", "T");
make_test ("(typep () 'atom)", "T");
make_test ("(typep (cons 1 2) 'atom)", "()");
make_test ("(typep 0 'number)", "T");
make_test ("(typep 0.1 'real)", "T");
make_test ("(typep 0 'rational)", "T");
make_test ("(typep 0.1 'rational)", "()");
make_test ("(typep 0 'integer)", "T");
make_test ("(typep 0 'bignum)", "T");
make_test ("(typep 0 'fixnum)", "()");
make_test ("(typep 1/2 'ratio)", "T");
make_test ("(typep 3/1 'ratio)", "()");
make_test ("(typep 0.1 'float)", "T");
make_test ("(typep 0.1 'single-float)", "T");
make_test ("(typep 0.1 'double-float)", "T");
make_test ("(typep 0.1 'short-float)", "T");
make_test ("(typep 0.1 'long-float)", "T");
make_test ("(typep \"abc\" 'string)", "T");
make_test ("(typep \"abc\" '(string))", "T");
make_test ("(typep \"abc\" 'array)", "T");
make_test ("(typep 0 'string)", "()");
make_test ("(typep '(1 2) 'list)", "T");
make_test ("(typep #(1 2) 'sequence)", "T");
make_test ("(typep \"\" t)", "T");
make_test ("(typep 0 'nil)", "()");
make_test ("(typep nil nil)", "()");
make_test ("(typep nil 'null)", "T");
make_test ("(typep 'aaa 'symbol)", "T");
make_test ("(typep nil 'boolean)", "T");
make_test ("(typep #p\"\" 'pathname)", "T");
make_test ("(typep (open #p\"README\") 'stream)", "T");
make_test ("(make-string 3)", "\"\0\0\0\"");
make_test ("(make-symbol \"aaa\")", "#:|aaa|");
make_test ("(boundp 'b)", "T");
make_test ("(boundp 'g3)", "()");
make_test ("(symbol-value 'b)", "9");
make_test ("(let ((b 10)) (symbol-value 'b))", "10");
make_test ("(fboundp 'b)", "()");
make_test ("(fboundp 'g3)", "T");
make_test ("(symbol-function 'fun)", "#<FUNCTION FUN>");
make_test ("(symbol-function 'defparameter)", "#<MACRO BUILTIN DEFPARAMETER>");
make_test ("(symbol-function 'if)", "#<SPECIAL OPERATOR IF>");
make_test ("(fdefinition 'fun)", "#<FUNCTION FUN>");
make_test ("(fdefinition 'car)", "#<FUNCTION BUILTIN CAR>");
make_test ("(funcall (complement #'numberp) \"\")", "T");
make_test ("(symbol-name 'aaa)", "\"AAA\"");
make_test ("(symbol-name :bbb)", "\"BBB\"");
make_test ("(special-operator-p 'if)", "T");
make_test ("(special-operator-p 'car)", "()");
make_test ("(special-operator-p 'aaa)", "()");
make_test ("(equal #\\a #\\a)", "T");
make_test ("(equal 0 0)", "T");
make_test ("(equal 0 0.0)", "()");
make_test ("(equal (cons 'a 'b) (cons 'a 'b))", "T");
make_test ("(equal (make-array '(1 2 3)) (make-array '(1 2 3)))", "()");
make_test ("(equalp 0 0)", "T");
make_test ("(equalp 0 0.0)", "T");
make_test ("(equalp (make-array '(1 2 3)) (make-array '(1 2 3)))", "T");
make_test ("(string \"ccc\")", "\"ccc\"");
make_test ("(string 'ddd)", "\"DDD\"");
make_test ("(string #\\F)", "\"F\"");
make_test ("(string= \"aaa\" \"aaa\")", "T");
make_test ("(string= \"aaa\" \"aab\")", "()");
make_test ("(string/= \"aaa\" \"aaa\")", "()");
make_test ("(string/= \"aaa\" \"aab\")", "T");
make_test ("(char= #\\a)", "T");
make_test ("(char= #\\a #\\b)", "()");
make_test ("(char= #\\a #\\a #\\a)", "T");
make_test ("(char= #\\a #\\a #\\b)", "()");
make_test ("(char-equal #\\a)", "T");
make_test ("(char-equal #\\a #\\a)", "T");
make_test ("(char-equal #\\a #\\A)", "T");
make_test ("(char-equal #\\a #\\A #\\a)", "T");
make_test ("(char-equal #\\a #\\A #\\b)", "()");
make_test ("(char-upcase #\\a)", "#\\A");
make_test ("(char-upcase #\\A)", "#\\A");
make_test ("(char-upcase #\\3)", "#\\3");
make_test ("(char-downcase #\\B)", "#\\b");
make_test ("(char-downcase #\\b)", "#\\b");
make_test ("(char-downcase #\\@)", "#\\@");
make_test ("(alpha-char-p #\\a)", "T");
make_test ("(alpha-char-p #\\3)", "()");
make_test ("(alphanumericp #\\a)", "T");
make_test ("(alphanumericp #\\3)", "T");
make_test ("(alphanumericp #\\#)", "()");
make_test ("(digit-char 2)", "#\\2");
make_test ("(digit-char 10)", "()");
make_test ("(digit-char 10 16)", "#\\A");
make_test ("(digit-char 16 16)", "()");
make_test ("(digit-char-p #\\2)", "2");
make_test ("(digit-char-p #\\A)", "()");
make_test ("(digit-char-p #\\a)", "()");
make_test ("(digit-char-p #\\A 16)", "10");
make_test ("(digit-char-p #\\a 16)", "10");
make_test ("(digit-char-p #\\g 16)", "()");
make_test ("(string-upcase \"Hello\")", "\"HELLO\"");
make_test ("(string-downcase \"Hello\")", "\"hello\"");
make_test ("(string-capitalize \"this is a Good day\")", "\"This Is A Good Day\"");
make_test ("(car '(1 2))", "1");
make_test ("(cdr '(1 2))", "(2)");
make_test ("(car ())", "()");
make_test ("(cdr ())", "()");
make_test ("(setf tt (list 'a 'b 'd))", "(A B D)");
make_test ("(rplaca tt 'f)", "(F B D)");
make_test ("tt", "(F B D)");
make_test ("(rplacd tt 'g)", "(F . G)");
make_test ("tt", "(F . G)");
make_test ("(nth 0 '(0))", "0");
make_test ("(nth 1 '(0))", "()");
make_test ("(nthcdr 1 ())", "()");
make_test ("(nthcdr 2 '(a b c))", "(C)");
make_test ("(nthcdr 1 '(0 . 1))", "1");
make_test ("(nth-value 0 (values 10 9))", "10");
make_test ("(nth-value 1 (values 10 9))", "9");
make_test ("(nth-value 2 (values 10 9))", "()");
make_test ("(nth-value 3 (values 10 9))", "()");
make_test ("(elt '(0 3) 1)", "3");
make_test ("(elt \"abc\" 0)", "#\\a");
make_test ("(elt #(3 2 1) 1)", "2");
make_test ("(aref \"abc\" 2)", "#\\c");
make_test ("(aref #(1 2 3) 1)", "2");
make_test ("(aref (make-array nil))", "()");
make_test ("(aref (make-array '(1 2 3)) 0 1 2)", "()");
make_test ("(row-major-aref #(0 1 2) 1)", "1");
make_test ("(row-major-aref (make-array '(3 2 1)) 5)", "()");
make_test ("(char \"àbcdef\" 2)", "#\\c");
make_test ("(length ())", "0");
make_test ("(length \"aaa\")", "3");
make_test ("(length #(1 2))", "2");
make_test ("(length '(0 1 2))", "3");
make_test ("(list-length '(0 1 2 3))", "4");
make_test ("(make-array 4)", "#(() () () ())");
make_test ("(make-array '(1 2 3))", "#<ARRAY, RANK 3>");
make_test ("(make-array nil)", "#<ARRAY, RANK 0>");
make_test ("(array-has-fill-pointer-p \"aaa\")", "()");
make_test ("(array-rank #())", "1");
make_test ("(array-rank (make-array nil))", "0");
make_test ("(array-rank (make-array '(1 2)))", "2");
make_test ("(array-dimensions \"aaa\")", "(3)");
make_test ("(array-dimensions #(1 2 3 4))", "(4)");
make_test ("(array-dimension \"aaa\" 0)", "3");
make_test ("(array-total-size \"aaa\")", "3");
make_test ("(array-in-bounds-p (make-array nil))", "T");
make_test ("(array-in-bounds-p \"aaa\" 3)", "()");
make_test ("(array-in-bounds-p (make-array '(1 2 3)) 0 1 2)", "T");
make_test ("(array-in-bounds-p (make-array '(1 2 3)) 1 1 2)", "()");
make_test ("(array-row-major-index \"aaa\" 1)", "1");
make_test ("(array-row-major-index #(0 1 2) 0)", "0");
make_test ("(array-row-major-index (make-array nil))", "0");
make_test ("(array-row-major-index (make-array '(1 2 3)) 0 0 0)", "0");
make_test ("(array-row-major-index (make-array '(2 3 4)) 0 0 1)", "1");
make_test ("(array-row-major-index (make-array '(2 3 4)) 0 2 0)", "8");
make_test ("(array-row-major-index (make-array '(2 3 4)) 1 0 0)", "12");
make_test ("(last '(1 2 3))", "(3)");
make_test ("(last '(1 2 3) 0)", "()");
make_test ("(last '(1 2 3) 2)", "(2 3)");
make_test ("(eval '(write \"\"))", "\"\"\n\"\"");
make_test ("(setq var 10)", "10");
make_test ("(let ((var 12)) (eval 'var))", "10");
make_test ("(write \"\")", "\"\"\n\"\"");
make_test ("(write-string \"aaa\\n\")", "aaan\n\"aaan\"");
make_test ("(write-string \"\n" .
	   "\")", "\n\"\n\"");
make_test ("(write-char #\\a)", "a\n#\\a");
make_test ("(write-char #\\newline)", "\n#\\Newline");
make_test ("(fresh-line)", "()");
make_test ("(terpri)", "\n()");
make_test ("(write-line \"aaa\")", "aaa\n\"aaa\"");
make_test ("(prin1 '|Aa,a|)", "|Aa,a|\n|Aa,a|");
make_test ("(prin1 #\\a)", "#\\a\n#\\a");
make_test ("(prin1 \"aaa\")", "\"aaa\"\n\"aaa\"");
make_test ("(princ '|Aa,a|)", "Aa,a\n|Aa,a|");
make_test ("(princ #\\a)", "a\n#\\a");
make_test ("(princ \"aaa\")", "aaa\n\"aaa\"");
make_test ("(format t \"aaa\")", "aaa\n()");
make_test ("(format t \"aaa~~\")", "aaa~\n()");
make_test ("(format t \"aaa~%~%\")", "aaa\n\n()");
make_test ("(format t \"aa~&~&bb\")", "aa\nbb\n()");
make_test ("(format t \"the number is ~a and the list is ~s\" 10 '(1 2 3))",
	   "the number is 10 and the list is (1 2 3)\n()");
make_test ("(second '(0))", "()");
make_test ("(fifth '(0 1 2 3 4))", "4");
make_test ("(endp '(1 . 2))", "()");
make_test ("(endp ())", "T");
make_test ("(when t \"\" \"\" \"\")", "\"\"");
make_test ("(when nil \"\" \"\" \"\")", "()");
make_test ("(unless nil \"\" \"\" \"\")", "\"\"");
make_test ("(tagbody\n" .
	   "  (write \"1\")\n" .
	   "  (go jmp)\n" .
	   "  (write \"2\")\n" .
	   "  jmp\n" .
	   "  (write \"3\"))", "\"1\"\"3\"\n()");
make_test ("(tagbody 1 (go 3) 2 3 (go 4) 4 (write \"\"))", "\"\"\n()");
make_test ("(tagbody 1 2 (tagbody 3 (go 4)) 4 (write \"\"))", "\"\"\n()");
make_test ("(block test (write 1) (write 2) (return-from test 10) (write 3))", "12\n10");
make_test ("(block test (write 1) (block test2 (write 2) (return-from test 10) (write 3)))", "12\n10");
make_test ("(block test (write 1) (block test2 (write 2) (return-from test (values 10 11)) (write 3)))", "12\n10\n11");
make_test ("(block nil (return (values 11 12)))", "11\n12");
make_test ("(defun test () (write 1) (write 2) (return-from test 10) (write 3))", "TEST");
make_test ("(test)", "12\n10");
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
make_test ("(and t t t)", "T");
make_test ("(and t nil t)", "()");
make_test ("(and (= 1 1) t)", "T");
make_test ("(and t (= 1 2) t)", "()");
make_test ("(or)", "()");
make_test ("(or nil nil nil)", "()");
make_test ("(or (= 1 2) t)", "T");
make_test ("(or t nil t)", "T");
make_test ("(concatenate 'string)", "\"\"");
make_test ("(concatenate 'string \"\")", "\"\"");
make_test ("(concatenate 'string \"aa\" \"bb\")", "\"aabb\"");
make_test ("(dotimes (tmp 10 tmp))", "10");
make_test ("(mapcar #'car '((1 a) (2 b) (3 c)))", "(1 2 3)");
make_test ("(mapcar #'abs '(3 -4 2 -5 -6))", "(3 4 2 5 6)");
make_test ("(mapcar #'cons '(a b c) '(1 2 3))", "((A . 1) (B . 2) (C . 3))");
make_test ("(remove 3 '(1 2 3 4 3 2 1))", "(1 2 4 2 1)");
make_test ("(remove #\\e \"abcdefg\")", "\"abcdfg\"");
make_test ("(remove-if #'numberp #(1 2 3))", "#()");
make_test ("(remove-if-not #'listp #(()))", "#(())");
make_test ("(reverse '(0 1 2 3))", "(3 2 1 0)");
make_test ("(reverse \"hello\")", "\"olleh\"");
make_test ("(reverse #(0 1 2 3))", "#(3 2 1 0)");
make_test ("(nreverse '(0 1 2 3))", "(3 2 1 0)");
make_test ("(nreverse \"hello\")", "\"olleh\"");
make_test ("(nreverse #(0 1 2 3))", "#(3 2 1 0)");
make_test ("(adjoin 0 '(0 1 2))", "(0 1 2)");
make_test ("(adjoin 0 '(1 2))", "(0 1 2)");
make_test ("(cond (t (+ 1 2)))", "3");
make_test ("(cond (nil 2) (t 3) (nil 4))", "3");
make_test ("(every #'evenp '(0 2 4))", "T");
make_test ("(every #'< '(0 1 2) '(1 1))", "()");
make_test ("(some #'< '(0 1 2) '(1 1))", "T");
make_test ("(notany #'oddp '(1 3 4))", "()");
make_test ("(notevery #'oddp '(1 3 4))", "T");
make_test ("(member 3 '(1 2 3 4))", "(3 4)");
make_test ("(member 5 '(1 2 3))",  "()");
make_test ("(member-if #'evenp '(1 2 3 4))", "(2 3 4)");
make_test ("(member-if #'listp '(nil))", "(())");
make_test ("(member-if-not #'evenp '(0 2 4))", "()");
make_test ("(count #\\a \"abcabc\")", "2");
make_test ("(count 2 '(1 2 3))", "1");
make_test ("(count-if #'upper-case-p \"aAbBcCdD\")", "4");
make_test ("(count-if-not #'evenp #(0 1 2))", "1");


# arithmetic tests

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
make_test ("(/ 0.5)", "2");
make_test ("(/ 60 -2 3 5.0)", "-2");
make_test ("(floor 1)", "1\n0");
make_test ("(floor 1/2)", "0\n1/2");
make_test ("(ceiling 2)", "2\n0");
make_test ("(ceiling 1/2 1)", "1\n-1/2");
make_test ("(truncate -3 2)", "-1\n-1");
make_test ("(truncate -1)", "-1\n0");
make_test ("(truncate -.1)", "0\n-0.1");
make_test ("(round 5)", "5\n0");
make_test ("(round .5)", "0\n0.5");
make_test ("(round -1.5)", "-2\n0.5");
make_test ("(sqrt 9)", "3");
make_test ("(sqrt 9.0)", "3");
make_test ("(isqrt 10)", "3");
make_test ("(= 1)", "T");
make_test ("(= 1 1)", "T");
make_test ("(= 1 2)", "()");
make_test ("(= 1 1 1.0 1/1)", "T");
make_test ("(= 0 -0.0)", "T");
make_test ("(/= 0)", "T");
make_test ("(/= 0 1 2.0)", "T");
make_test ("(/= 0 1.0 2 1)", "()");
make_test ("(< 0)", "T");
make_test ("(< 0 1.0 2)", "T");
make_test ("(< 0 0.0 1)", "()");
make_test ("(<= 0 1 1)", "T");
make_test ("(<= 1 0)", "()");
make_test ("(> 1.1 0)", "T");
make_test ("(> 1 1)", "()");
make_test ("(>= 1 1.0)", "T");
make_test ("(min 0)", "0");
make_test ("(min 0 1 2)", "0");
make_test ("(min 0 1/2 -0.1)", "-0.1");
make_test ("(max 0)", "0");
make_test ("(max 0 1.0 2)", "2");
make_test ("(max 0 1/2 -0.1)", "1/2");
make_test ("(minusp 0)", "()");
make_test ("(minusp -0.5)", "T");
make_test ("(plusp 1)", "T");
make_test ("(plusp -1)", "()");
make_test ("(abs 0)", "0");
make_test ("(abs -0.5)", "0.5");
make_test ("(abs 1/2)", "1/2");
make_test ("(zerop -0)", "T");
make_test ("(zerop 1/2)", "()");
make_test ("(mod 2.5 1)", "0.5");
make_test ("(mod -2.5 1)", "0.5");
make_test ("(rem 2.5 1)", "0.5");
make_test ("(rem -2.5 1)", "-0.5");
make_test ("(evenp 0)", "T");
make_test ("(evenp 1)", "()");
make_test ("(oddp 0)", "()");
make_test ("(oddp -1)", "T");



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
	    print "\n\nEOF reached from al, it probably crashed\n";

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
