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
  (let (wholevar)
    (if (eq (car lambdal) '&whole)
	(setq wholevar (cadr lambdal) lambdal (cddr lambdal)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (progn
	 (setf (compiler-macro-function ',name)
	       (lambda (form env)
		 (let ((args form))
		   (if (eq (car form) 'funcall)
		       (setq args (cdr args)))
		   (let ,(if wholevar
			     `((,wholevar form)))
		     (al:with-macro-arguments ,lambdal
		       env args
		       . ,body)))))
	 ',name))))



(defmacro with-compilation-unit (opts &rest forms)
  `(progn
     ,@forms))



(defun macroexpand-backquote (next backquote-depth env)
  (cond
    ((typep next 'al:backquote)
     (setf (al:next next) (macroexpand-backquote (al:next next) (1+ backquote-depth) env))
     next)
    ((typep next 'al:comma)
     (if (= 1 backquote-depth)
	 (setf (al:next next) (macroexpand-form-deeply (al:next next) env))
	 (setf (al:next next) (macroexpand-backquote (al:next next) (1- backquote-depth) env)))
     next)
    ((typep next 'al:dot)
     (setf (al:next next) (macroexpand-backquote (al:next next) backquote-depth env))
     next)
    ((typep next 'al:at)
     (setf (al:next next) (macroexpand-backquote (al:next next) backquote-depth env))
     next)
    ((consp next)
     (let ((cons next))
       (while (consp cons)
	 (setf (car cons) (macroexpand-backquote (car cons) backquote-depth env))
	 (setq cons (cdr cons))))
     next)
    (t
     next)))


(defun macroexpand-body (form env)
  (let ((cons form))
    (while cons
      (setf (car cons) (macroexpand-form-deeply (car cons) env))
      (setq cons (cdr cons)))
    form))


(defun expand-compiler-macro (form)
  (if (and
       (consp form)
       (symbolp (car form)))
      (cond
	((eq (car form) 'setf)
	 (if (and
	      (consp (cadr form))
	      (symbolp (caadr form)))
	     (let ((compmac (compiler-macro-function (list 'setf (caadr form)))))
	       (if compmac
		   (setq form (funcall compmac form nil))))))
	((eq (car form) 'funcall)
	 (if (and
	      (consp (cadr form))
	      (or (eq (caadr form) 'quote)
		  (eq (caadr form) 'function)))
	     (let ((compmac (compiler-macro-function (cadadr form))))
	       (if compmac
		   (setq form (funcall compmac form nil))))))
	(t
	 (let ((compmac (compiler-macro-function (car form))))
	   (if compmac
	       (setq form (funcall compmac form nil)))))))
  form)


(defun macroexpand-form-deeply (form env)
  (if al:*expand-compiler-macros*
      (setq form (expand-compiler-macro form)))
  (setq form (macroexpand form env))
  (cond
    ((typep form 'al:backquote)
     (setf (al:next form) (macroexpand-backquote (al:next form) 1 env))
     form)
    ((atom form) form)
    ((member (car form) '(go) :test #'eq)
     form)
    ((eq (car form) 'function)
     (if (and (consp (cadr form))
	      (eq (caadr form) 'lambda))
	 (macroexpand-body (cddadr form) env))
     form)
    ((member (car form) '(if progn block tagbody multiple-value-call
			  multiple-value-prog1 and or catch throw progv
			  unwind-protect locally) :test #'eq)
     (macroexpand-body (cdr form) env)
     form)
    ((member (car form) '(let let* handler-bind restart-bind
			  al:with-macro-arguments :test #'eq))
     (dolist (f (cadr form))
       (if (and (consp f)
		(consp (cdr f)))
	   (setf (cadr f) (macroexpand-form-deeply (cadr f) env))))
     (macroexpand-body (cddr form) env)
     form)
    ((eq (car form) 'macrolet)
     (let ((i 0))
       (dolist (f (cadr form))
	 (macroexpand-body (cddr f) env)
	 (rplaca env (cons (list (car f)
				 (coerce `(lambda (form env)
					    (al:with-macro-arguments ,(cadr f)
					      env form
					      . ,(cddr f))) 'function))
			   (car env)))
	 (incf i))
       (macroexpand-body (cddr form) env)
       (rplaca env (nthcdr i (car env))))
     form)
    ((eq (car form) 'symbol-macrolet)
     (let ((i 0))
       (dolist (f (cadr form))
	 (macroexpand-body (cddr f) env)
	 (rplaca (cdr env) (cons (list (car f) (cadr f)) (cadr env)))
	 (incf i))
       (macroexpand-body (cddr form) env)
       (rplaca (cdr env) (nthcdr i (cadr env))))
     form)
    ((member (car form) '(flet labels) :test #'eq)
     (dolist (f (cadr form))
       (macroexpand-body (cddr f) env))
     (macroexpand-body (cddr form) env)
     form)
    ((member (car form) '(dotimes dolist return-from eval-when
			  the) :test #'eq)
     (macroexpand-body (cddr form) env)
     form)
    ((member (car form) '(do do* :test #'eq))
     (macroexpand-body (cdddr form) env)
     form)
    ((member (car form) '(setq setf) :test #'eq)
     (let ((cons (cddr form)))
       (while cons
	 (setf (car cons) (macroexpand-form-deeply (car cons) env))
	 (setq cons (cddr cons))))
     form)
    ((not (special-operator-p (car form)))
     (macroexpand-body (cdr form) env)
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


(defgeneric make-load-form (obj &optional env))

(defun make-load-form-saving-slots (obj &key slot-names environment)
  (typecase obj
    (structure-object
     (let (args)
       (dolist (slot (mapcar #'car (al:dump-fields obj)))
	 (setq args (list* (intern (string slot) 'keyword)
			   `(quote ,(slot-value obj slot))
			   args)))
       `(al:make-structure ',(type-of obj) . ,args)))
    (t
     (error "not yet implemented!"))))


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
     (error "function objects can't be dumped in a compiled file"))
    (package
     (format str "#.(CL:FIND-PACKAGE ~s)" (package-name obj)))
    (structure-object
     (format str "#.~s" (make-load-form obj)))
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
    ((or vector number character pathname)
     (write obj :stream str))
    (otherwise
     (error "don't know how to dump that type of object in a compiled file")))
  gensyms)


(defun parse-toplevel-form-at-compile-time (form)
  (if (consp form)
      (cond
	((eq (car form) 'progn)
	 (dolist (f (cdr form))
	   (parse-toplevel-form-at-compile-time f)))
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
		   (setq obj (macroexpand-form-deeply obj (list nil nil)))
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
		  (macroexpand-body (al:function-body meth) (list nil nil)))
	    (setf (al:function-attributes meth) '(:compiled))))
	(setf (al:function-body definition)
	      (macroexpand-body (al:function-body definition) (list nil nil))))
    (setf (al:function-attributes definition) '(:compiled)))
  (if name
      (progn
	(if (and (symbolp name)
		 (macro-function name))
	    (setf (macro-function name) definition)
	    (setf (fdefinition name) definition))
	(values name nil nil))
      (values definition nil nil)))



(defun add-to-objvector-and-get-index (obj objvec)
  (or (position obj objvec :test 'eq)
      (vector-push-extend obj objvec)))


(defmacro append-to-list (list last-cons newcar)
  (let ((newcarsym (gensym)))
    `(let ((,newcarsym ,newcar))
       (if ,list
	   (setf (cdr ,last-cons) (cons ,newcarsym nil) ,last-cons (cdr ,last-cons))
	   (setf ,list (cons ,newcarsym nil) ,last-cons ,list)))))


(defun immediatep (obj)
  (typep obj '(and (not cons) (not symbol))))


(defun generate-pseudo-bytecode-for-form (form bcode last-bc-cons objvec next-reg last-in-func-p is-funcall)
  (cond
    ((find (car form) '(if progn tagbody go block do do* dolist dotimes let let*) :test 'eq)
     (error "don't know how to compile that"))
    ((symbolp form)
     (let ((ind (add-to-objvector-and-get-index form objvec)))
       (append-to-list bcode last-bc-cons `(eval-var (reg ,next-reg) (imm ,ind))))
     (incf next-reg))
    ((consp form)
     (let ((retreg next-reg)
	   (ind (add-to-objvector-and-get-index (car form) objvec))
	   funcreg funcall-args regs-to-decrement-refcount)
       (incf next-reg)
       (append-to-list bcode last-bc-cons `(resolve-function-name (reg ,next-reg)
								  (imm ,ind)))
       (setq funcreg next-reg)
       (incf next-reg)
       (dolist (arg (cdr form))
	 (if (immediatep arg)
	     (setq funcall-args (cons `(imm ,(add-to-objvector-and-get-index arg objvec)) funcall-args))
	     (progn
	       (setq funcall-args (cons `(reg ,next-reg) funcall-args))
	       (setq regs-to-decrement-refcount (cons next-reg regs-to-decrement-refcount))
	       (multiple-value-setq (bcode last-bc-cons next-reg)
		 (generate-pseudo-bytecode-for-form arg bcode last-bc-cons objvec next-reg nil nil)))))
       (setq funcall-args (reverse funcall-args))
       (append-to-list bcode last-bc-cons `(call-function (reg ,retreg) (reg ,funcreg) . ,funcall-args))
       (dolist (reg regs-to-decrement-refcount)
	 (append-to-list bcode last-bc-cons `(decrement-refcount (reg ,reg))))))
    (t
     (if last-in-func-p
	 (let ((ind (add-to-objvector-and-get-index form objvec)))
	   (append-to-list bcode last-bc-cons `(return (imm ,ind)))))))
  (values
   bcode
   last-bc-cons
   next-reg))


(defun generate-pseudo-bytecode-for-body (body bcode last-bc-cons objvec next-reg)
  (while body
    (multiple-value-setq (bcode last-bc-cons) ;; next-reg)
      (generate-pseudo-bytecode-for-form (car body) bcode last-bc-cons objvec next-reg nil nil))
    (if (cdr body)
	(append-to-list bcode last-bc-cons '(decrement-refcount (reg 1))))
    (setq body (cdr body)))
  (setf (cdr last-bc-cons) (cons '(return (reg 1)) nil))
  (values
   bcode
   last-bc-cons
   next-reg))


(defun translate-register-or-immediate-to-bytecode (place)
  (if (eq (car place) 'reg)
      (cadr place)
      (logior (cadr place) (ash 1 31))))


(defconstant +return+ 0)
(defconstant +jump+ 1)
(defconstant +jump-if+ 2)
(defconstant +resolve-function-name+ 3)
(defconstant +resolve-function-name-in-global-env+ 4)
(defconstant +eval-var+ 5)
(defconstant +decrement-refcount+ 6)
(defconstant +call-function+ 7)

(defun translate-pseudo-bytecode-to-bytecode (bcode)
  (let ((out (make-array 512 :element-type '(unsigned-byte 32) :fill-pointer 0)))
    (dolist (instr bcode)
      (case (car instr)
	(return
	  (vector-push-extend +return+ out)
	  (vector-push-extend (translate-register-or-immediate-to-bytecode (cadr instr)) out))
	(resolve-function-name
	 (vector-push-extend +resolve-function-name+ out)
	 (vector-push-extend (translate-register-or-immediate-to-bytecode (cadr instr)) out)
	 (vector-push-extend (translate-register-or-immediate-to-bytecode (caddr instr)) out))
	(decrement-refcount
	 (vector-push-extend +decrement-refcount+ out)
	 (vector-push-extend (translate-register-or-immediate-to-bytecode (cadr instr)) out))
	(call-function
	 (vector-push-extend +call-function+ out)
	 (dolist (arg (cdr instr))
	   (vector-push-extend (translate-register-or-immediate-to-bytecode arg) out))
	 (vector-push-extend 0 out))))
    out))


(defun compile-function-to-bytecode (fun)
  (let* ((body (cddar (al:function-body fun)))
	 (objvec (make-array 16 :fill-pointer 0))
	 (bcode (translate-pseudo-bytecode-to-bytecode
		 (generate-pseudo-bytecode-for-body body nil nil objvec 1))))
    (setf (al:function-bytecode fun) bcode)
    (setf (al:function-objvector fun) objvec)
    fun))




(dolist (sym '(macroexpand-backquote macroexpand-body expand-compiler-macro
	       macroexpand-form-deeply write-preserving-similarity
	       parse-toplevel-form-at-compile-time
	       add-to-objvector-and-get-index append-to-list immediatep
	       generate-pseudo-bytecode-for-form
	       generate-pseudo-bytecode-for-body
	       translate-register-or-immediate-to-bytecode
	       translate-pseudo-bytecode-to-bytecode
	       compile-function-to-bytecode))
  (compile sym))


(dolist (sym '(compiler-macro-function define-compiler-macro
	       with-compilation-unit *compile-file-truename*
	       *compile-file-pathname* *compile-print* *compile-verbose*
	       compile-file-pathname make-load-form make-load-form-saving-slots
	       compile-file compile))
  (export sym)
  (if (fboundp sym)
      (compile sym)))
