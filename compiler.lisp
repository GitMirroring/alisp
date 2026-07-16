;;;;  Copyright (C) 2022-2026 Andrea Monaco

;;;;  This file is part of alisp, a lisp implementation.

;;;;  al is free software: you can redistribute it and/or modify it
;;;;  under the terms of the GNU General Public License as published
;;;;  by the Free Software Foundation, either version 3 of the
;;;;  License, or (at your option) any later version.

;;;;  al is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU General Public License for more details.

;;;;  You should have received a copy of the GNU General Public License
;;;;  along with al.  If not, see <https://www.gnu.org/licenses/>.



(in-package cl)



(defparameter al::*expand-compiler-macros* t)
(export 'al::*expand-compiler-macros* 'al)


(defparameter al::*compiler-macro-registry* (make-hash-table :test 'equal))
(export 'al::*compiler-macro-registry* 'al)


(defun compiler-macro-function (name)
  (values (gethash name al:*compiler-macro-registry*)))


(defun (setf compiler-macro-function) (newval name)
  (setf (gethash name al:*compiler-macro-registry*) newval))


(defmacro define-compiler-macro (name lambdal &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn
       (setf (compiler-macro-function ',name)
	     (lambda (form env)
	       (al:with-macro-arguments ,lambdal
		 env form
		 . ,body)))
       ',name)))



(defmacro with-compilation-unit (opts &rest forms)
  `(progn
     ,@forms))



(defun macroexpand-body (form)
  (let ((cons form))
    (while cons
      (setf (car cons) (macroexpand-form-deeply (car cons)))
      (setq cons (cdr cons)))
    form))


(defun macroexpand-form-deeply (form)
  (if (and
       al:*expand-compiler-macros*
       (consp form)
       (symbolp (car form)))
      (let ((compmac (compiler-macro-function (car form))))
	(if compmac
	    (setq form (funcall compmac form nil)))))
  (setq form (macroexpand form))
  (cond
    ((atom form) form)
    ((member (car form) '(go) :test #'eq)
     form)
    ((eq (car form) 'function)
     (if (and (consp (cadr form))
	      (eq (caadr form) 'lambda))
	 (macroexpand-body (cddadr form)))
     form)
    ((member (car form) '(if progn block tagbody multiple-value-call
			  multiple-value-prog1 and or catch throw progv
			  unwind-protect locally) :test #'eq)
     (macroexpand-body (cdr form))
     form)
    ((member (car form) '(let let* flet labels macrolet symbol-macrolet dotimes
			  dolist handler-bind restart-bind return-from eval-when
			  the) :test #'eq)
     (macroexpand-body (cddr form))
     form)
    ((member (car form) '(setq setf) :test #'eq)
     (let ((cons (cdr form)))
       (while cons
	 (setf (car cons) (macroexpand-form-deeply (car cons)))
	 (setq cons (cddr cons))))
     form)
    ((not (special-operator-p (car form)))
     (macroexpand-body (cdr form))
     form)
    (t
     form)))



(defun macroexpand-cdr-nondestructively (form)  ;; the non-destructive variants are not used
  (let ((cdr (macroexpand-body (cdr form))))
    (if (eq cdr (cdr form))
	form
	(cons (car form) cdr))))


(defun macroexpand-form-deeply-nondestructively (form)
  (if (and
       al:*expand-compiler-macros*
       (consp form)
       (symbolp (car form)))
      (let ((compmac (compiler-macro-function (car form))))
	(if compmac
	    (setq form (funcall compmac form nil)))))
  (setq form (macroexpand form))
  (cond
    ((atom form) form)
    ((member (car form) '(go) :test #'eq)
     form)
    ((eq (car form) 'function)
     (if (and (consp (cadr form))
	      (eq (caadr form) 'lambda))
	 `(function (lambda ,(cadadr form) . ,(macroexpand-body (cddadr form))))
	 form))
    ((member (car form) '(if progn block tagbody multiple-value-call
			  multiple-value-prog1 and or catch throw progv
			  unwind-protect locally) :test #'eq)
     (macroexpand-cdr form))
    ((member (car form) '(let let* flet labels macrolet symbol-macrolet dotimes
			  dolist handler-bind restart-bind return-from eval-when
			  the) :test #'eq)
     (let ((body (macroexpand-body (cddr form))))
       (if (eq body (cddr form))
	   form
	   (list* (car form) (cadr form) body))))
    ((member (car form) '(setq setf) :test #'eq)
     form)
    ((not (special-operator-p (car form)))
     (macroexpand-cdr form))
    (t
     form)))


(defun macroexpand-body-nondestructively (body)
  (let ((out body)
	(cons body)
	last-copied last-alloc)
    (while cons
      (let ((form (macroexpand-form-deeply (car cons))))
	(when (not (eq form (car cons)))
	  (while (not (eq last-copied cons))
	    (if last-alloc
		(progn
		  (setf (cdr last-alloc) (cons (cadr last-copied) (cddr last-copied)))
		  (setq last-alloc (cdr last-alloc) last-copied (cdr last-copied)))
		(progn
		  (setq last-alloc (cons (car body) (cdr body)))
		  (setq last-copied body out last-alloc))))
	  (setf (car last-alloc) form)))
      (setq cons (cdr cons)))
    out))



(defparameter *compile-file-truename* nil)

(defparameter *compile-file-pathname* nil)

(defparameter *compile-print* nil)

(defparameter *compile-verbose* nil)


(defun compile-file-pathname (infile &key output-file &allow-other-keys)
  (let ((dir (al:pathname-directory infile)))
    (make-pathname :directory (and dir (if (char= (elt dir 0) #\/) dir (list :relative dir)))
		   :name (pathname-name infile)
		   :type "alc")))


(defun write-preserving-similarity (obj str gensyms)
  (typecase obj
    (cons
     (write-string "(" str)
     (let ((firstobjp t))
       (do ((c obj (cdr c)))
	   (nil)
	 (unless firstobjp
	   (write-string " " str))
	 (setq firstobjp nil)
	 (setq gensyms (write-preserving-similarity (car c) str gensyms))
	 (if (not (cdr c))
	     (return nil))
	 (when (atom (cdr c))
	   (write-string " . " str)
	   (setq gensyms (write-preserving-similarity (cdr c) str gensyms))
	   (return nil)))
       (write-string ")" str)))
    (symbol
     (let ((al:*print-always-two-colons* t))
       (if (symbol-package obj)
	   (write obj :stream str)
	   (let ((ind (position obj gensyms :test 'eq)))
	     (if ind
		 (format str "#~s#" ind)
		 (progn
		   (setq gensyms (nconc gensyms (list obj)))
		   (format str "#~s=~s" (1- (length gensyms)) obj)))))))
    (function
     (format str "#.(CL:FUNCTION ~s)" (nth-value 2 (function-lambda-expression obj))))
    (package
     (format str "#.(CL:FIND-PACKAGE ~s)" (package-name obj)))
    (al:backquote
     (write-string "`" str)
     (setq gensyms (write-preserving-similarity (al:next obj) str gensyms)))
    (al:comma
     (write-string "," str)
     (setq gensyms (write-preserving-similarity (al:next obj) str gensyms)))
    (al:at
     (write-string "@" str)
     (setq gensyms (write-preserving-similarity (al:next obj) str gensyms)))
    (al:dot
     (write-string "." str)
     (setq gensyms (write-preserving-similarity (al:next obj) str gensyms)))
    (otherwise (write obj :stream str)))
  gensyms)


(defun parse-toplevel-form-at-compile-time (form)
  (if (consp form)
      (cond
	((eq (car form) 'progn)
	 (dolist (f (cdr form))
	   (parse-toplevel-form-at-compile-time f)))
	((member (car form) '(in-package) :test #'eq)
	 (eval form))
	((and
	  (eq (car form) 'eval-when)
	  (member :compile-toplevel (cadr form) :test #'eq))
	 (eval `(progn
		  ,@(cddr form)))))))


(defun compile-file (infile &key (output-file infile) (verbose *compile-verbose*) (print *compile-print*) external-format)
  (let* ((*compile-file-truename* infile)
	 (*compile-file-pathname* infile)
	 (outname (compile-file-pathname output-file))
	 (old-readtable *readtable*)
	 (old-package *package*)
	 (eofsym (gensym)))
    (unwind-protect
	 (progn
	   (if verbose
	       (format t ";;; Compiling file ~a...~%" infile))
	   (with-open-file (instr infile :direction :input)
	     (with-open-file (outstr outname :direction :output :if-exists :overwrite :if-does-not-exist :create)
	       (do nil
		   (nil)
		 (let ((obj (read instr nil eofsym)))
		   (when (eq obj eofsym)
		     (if verbose
			 (format t ";;; Compiling file ~a succeeded~%" infile))
		     (return-from compile-file (values outname nil nil)))
		   (when print
		     (format t "Compiling ")
		     (write obj)
		     (terpri))
		   (setq obj (macroexpand-form-deeply obj))
		   (parse-toplevel-form-at-compile-time obj)
		   (if print
		       (terpri))
		   (write-preserving-similarity obj outstr nil)
		   (terpri outstr)
		   (terpri outstr))))))
      (setq *readtable* old-readtable)
      (setq *package* old-package))))


(defun compile (name &optional definition)
  (if definition
      (setq definition (coerce definition 'function))
      (setq definition (if (symbolp name)
			   (or (macro-function name) (fdefinition name))
			   (fdefinition name))))
  (unless (typep definition 'compiled-function)
    (if (typep definition 'generic-function)
	(dolist (meth (al:dump-methods definition))
	  (unless (typep meth 'al:compiled-method)
	    (setf (al:function-body meth)
		  (macroexpand-body (al:function-body meth)))
	    (setf (al:function-attributes meth) '(:compiled))))
	(setf (al:function-body definition)
	      (macroexpand-body (al:function-body definition))))
    (setf (al:function-attributes definition) '(:compiled)))
  (if name
      (progn
	(if (and (symbolp name)
		 (macro-function name))
	    (setf (macro-function name) definition)
	    (setf (fdefinition name) definition))
	(values name nil nil))
      (values definition nil nil)))
