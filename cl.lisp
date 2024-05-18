;;;;  Copyright (C) 2022-2024 Andrea G. Monaco

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




(defparameter *features* '(:common-lisp))



(defparameter *DEFAULT-PATHNAME-DEFAULTS* #p".")



(defun machine-instance nil)
(defun machine-type nil)
(defun machine-version nil)

(defun short-site-name nil)
(defun long-site-name nil)



(defparameter *modules* '("this variable is deprecated per ANSI CL"))
(defun provide (mod) "this function is deprecated per ANSI CL.  It does nothing")
(defun require (mod &optional pl) "this function is deprecated per ANSI CL.  It does nothing")



(defun describe (obj &optional str) (values))
(defun describe-object (obj str))
(defun inspect (obj))



;;; the following useless constants are just ridiculously high because lambda
;;; lists and multiple values use chained lists, so there's no hard limit

(defconstant LAMBDA-PARAMETERS-LIMIT most-positive-fixnum)
(defconstant CALL-ARGUMENTS-LIMIT most-positive-fixnum)
(defconstant MULTIPLE-VALUES-LIMIT most-positive-fixnum)



(defconstant ARRAY-RANK-LIMIT most-positive-fixnum)
(defconstant ARRAY-DIMENSION-LIMIT most-positive-fixnum)
(defconstant ARRAY-TOTAL-SIZE-LIMIT most-positive-fixnum)


(defconstant CHAR-CODE-LIMIT 4294967296)



(defconstant lambda-list-keywords '(&optional &rest &body &key &allow-other-keys &aux &whole))



(defun identity (x) x)

(defun constantly (x) (lambda (&rest r) x))



(defparameter *READ-DEFAULT-FLOAT-FORMAT* 'single-float)


(defconstant PI 3.141592653589793L0)


(defun 1+ (num)
  (+ num 1))

(defun 1- (num)
  (- num 1))

(defun minusp (num)
  (< num 0))

(defun plusp (num)
  (> num 0))

(defun abs (num)
  (if (>= num 0)
      num
      (- num)))

(defun zerop (num)
  (= num 0))

(defun signum (num)
  (if (zerop num) num (/ num (abs num))))

(defun mod (num div)
  (nth-value 1 (floor num div)))

(defun rem (num div)
  (nth-value 1 (truncate num div)))

(defun evenp (num)
  (= 0 (mod num 2)))

(defun oddp (num)
  (= 1 (mod num 2)))

(defun gcd (&rest nums)
  (let ((l (length nums)))
    (cond
      ((= l 0)
       0)
      ((= l 1)
       (abs (car nums)))
      ((> l 2)
       (apply 'gcd (gcd (car nums) (cadr nums)) (cddr nums)))
      ((or (zerop (car nums)) (zerop (cadr nums)))
       (max (abs (car nums)) (abs (cadr nums))))
      (t
       (gcd (min (abs (car nums)) (abs (cadr nums)))
	    (mod (max (abs (car nums)) (abs (cadr nums)))
		 (min (abs (car nums)) (abs (cadr nums)))))))))


(defun lcm (&rest nums)
  (let ((l (length nums)))
    (cond
      ((= l 0)
       1)
      ((= l 1)
       (abs (car nums)))
      ((> l 2)
       (apply 'lcm (lcm (car nums) (cadr nums)) (cddr nums)))
      ((or (zerop (car nums)) (zerop (cadr nums)))
       0)
      (t
       (/ (abs (* (car nums) (cadr nums))) (gcd (car nums) (cadr nums)))))))



(defun isqrt (num)
  (values (floor (sqrt num))))

(defun conjugate (num)
  (complex (realpart num) (- (imagpart num))))

(defun cis (rad)
  (complex (cos rad) (sin rad)))


(defun upgraded-complex-part-type (type)
  (cond
    ((subtypep type 'integer) 'integer)
    ((subtypep type 'ratio) 'ratio)
    ((subtypep type 'float) 'float)))



(defun logand (&rest ints)
  (lognot (apply 'logior (mapcar #'lognot ints))))

(defun logandc1 (int1 int2)
  (logand (lognot int1) int2))

(defun logandc2 (int1 int2)
  (logand int1 (lognot int2)))

(defun logeqv (&rest ints)
  (let ((l (length ints)))
    (cond
      ((= l 0) -1)
      ((= l 1) (car ints))
      ((= l 2) (lognot (logxor (car ints) (cadr ints))))
      (t (logeqv (car ints) (apply 'logeqv (cdr ints)))))))

(defun lognand (int1 int2)
  (lognot (logand int1 int2)))

(defun lognor (int1 int2)
  (lognot (logior int1 int2)))

(defun logorc1 (int1 int2)
  (logior (lognot int1) int2))

(defun logorc2 (int1 int2)
  (logior int1 (lognot int2)))

(defun logxor (&rest ints)
  (let ((l (length ints)))
    (cond
      ((= l 0) 0)
      ((= l 1) (car ints))
      ((= l 2) (logand (logior (car ints) (cadr ints)) (lognot (logand (car ints) (cadr ints)))))
      (t (logxor (car ints) (apply 'logxor (cdr ints)))))))


(defconstant boole-1 'boole-1)
(defconstant boole-2 'boole-2)
(defconstant boole-andc1 'boole-andc1)
(defconstant boole-andc2  'boole-andc2)
(defconstant boole-and 'boole-and)
(defconstant boole-c1 'boole-c1)
(defconstant boole-c2 'boole-c2)
(defconstant boole-clr 'boole-clr)
(defconstant boole-eqv 'boole-eqv)
(defconstant boole-ior 'boole-ior)
(defconstant boole-nand 'boole-nand)
(defconstant boole-nor 'boole-nor)
(defconstant boole-orc1 'boole-orc1)
(defconstant boole-orc2 'boole-orc2)
(defconstant boole-set 'boole-set)
(defconstant boole-xor 'boole-xor)

(defun boole (op int1 int2)
  (cond
    ((eq op 'boole-1) int1)
    ((eq op 'boole-2) int2)
    ((eq op 'boole-andc1) (logandc1 int1 int2))
    ((eq op 'boole-andc2) (logandc2 int1 int2))
    ((eq op 'boole-and) (logand int1 int2))
    ((eq op 'boole-c1) (lognot int1))
    ((eq op 'boole-c2) (lognot int2))
    ((eq op 'boole-clr) 0)
    ((eq op 'boole-eqv) (logeqv int1 int2))
    ((eq op 'boole-ior) (logior int1 int2))
    ((eq op 'boole-nand) (lognand int1 int2))
    ((eq op 'boole-nor) (lognor int1 int2))
    ((eq op 'boole-orc1) (logorc1 int1 int2))
    ((eq op 'boole-orc2) (logorc2 int1 int2))
    ((eq op 'boole-set) -1)
    ((eq op 'boole-xor) (logxor int1 int2))
    (t (+ ""))))


#|
(defparameter *gensym-counter* 1)

(defun gensym (&optional x)
  (let ((str (make-string-output-stream)))
    (cond
      ((typep x 'string) (write-string x str)
       (write *gensym-counter* :stream str)
       (incf *gensym-counter*))
      ((typep x 'integer) (write-string "G" str)
       (write x :stream str))
      ((null x) (write-string "G" str)
       (write *gensym-counter* :stream str)
       (incf *gensym-counter*)))
    (make-symbol (get-output-stream-string str))))
|#


(defparameter gentemp-counter 1)

(defun gentemp (&optional (pr "T") (pack *package*))
  (let ((str (make-string-output-stream)))
    (loop
     (write-string pr str)
     (write gentemp-counter :stream str)
     (incf gentemp-counter)
     (let ((s (get-output-stream-string str)))
       (if (not (find-symbol s pack))
	   (return-from gentemp (values (intern s pack))))))))



(defun first (list) (nth 0 list))
(defun second (list) (nth 1 list))
(defun third (list) (nth 2 list))
(defun fourth (list) (nth 3 list))
(defun fifth (list) (nth 4 list))
(defun sixth (list) (nth 5 list))
(defun seventh (list) (nth 6 list))
(defun eighth (list) (nth 7 list))
(defun ninth (list) (nth 8 list))
(defun tenth (list) (nth 9 list))


(defun rest (list) (cdr list))


(defun caar (list) (car (car list)))
(defun cadr (list) (nth 1 list))
(defun cdar (list) (cdr (car list)))
(defun cddr (list) (nthcdr 2 list))
(defun caaar (list) (car (car (car list))))
(defun caadr (list) (car (nth 1 list)))
(defun cadar (list) (nth 1 (car list)))
(defun caddr (list) (car (nthcdr 2 list)))
(defun cdaar (list) (cdr (car (car list))))
(defun cdadr (list) (cdr (car (cdr list))))
(defun cddar (list) (nthcdr 2 (car list)))
(defun cdddr (list) (nthcdr 3 list))
(defun caaaar (list) (car (car (car (car list)))))
(defun caaadr (list) (car (car (nth 1 list))))
(defun caadar (list) (car (nth 1 (car list))))
(defun caaddr (list) (car (nth 1 (cdr list))))
(defun cadaar (list) (nth 1 (car (car list))))
(defun cadadr (list) (nth 1 (nth 1 list)))
(defun caddar (list) (nth 1 (cdr (car list))))
(defun cadddr (list) (car (nthcdr 3 list)))
(defun cdaaar (list) (cdr (car (car (car list)))))
(defun cdaadr (list) (cdr (car (car (cdr list)))))
(defun cdadar (list) (cdr (car (cdr (car list)))))
(defun cdaddr (list) (cdr (car (nthcdr 2 list))))
(defun cddaar (list) (nthcdr 2 (car (car list))))
(defun cddadr (list) (nthcdr 2 (nth 1 list)))
(defun cdddar (list) (nthcdr 3 (car list)))
(defun cddddr (list) (nthcdr 4 list))


(defun (setf caar) (newval list)
  (let ((c (car list)))
    (rplaca c newval)
    newval))

(defun (setf cadr) (newval list)
  (let ((c (cdr list)))
    (rplaca c newval)
    newval))

(defun (setf cdar) (newval list)
  (let ((c (car list)))
    (rplacd c newval)
    newval))

(defun (setf cddr) (newval list)
  (let ((c (cdr list)))
    (rplacd c newval)
    newval))

(defun (setf caaar) (newval list)
  (let ((c (caar list)))
    (rplaca c newval)
    newval))

(defun (setf caadr) (newval list)
  (let ((c (cadr list)))
    (rplaca c newval)
    newval))

(defun (setf cadar) (newval list)
  (let ((c (cdar list)))
    (rplaca c newval)
    newval))

(defun (setf caddr) (newval list)
  (let ((c (cddr list)))
    (rplaca c newval)
    newval))

(defun (setf cdaar) (newval list)
  (let ((c (caar list)))
    (rplacd c newval)
    newval))

(defun (setf cdadr) (newval list)
  (let ((c (cadr list)))
    (rplacd c newval)
    newval))

(defun (setf cddar) (newval list)
  (let ((c (cdar list)))
    (rplacd c newval)
    newval))

(defun (setf cdddr) (newval list)
  (let ((c (cddr list)))
    (rplacd c newval)
    newval))

(defun (setf caaaar) (newval list)
  (let ((c (caaar list)))
    (rplaca c newval)
    newval))

(defun (setf caaadr) (newval list)
  (let ((c (caadr list)))
    (rplaca c newval)
    newval))

(defun (setf caadar) (newval list)
  (let ((c (cadar list)))
    (rplaca c newval)
    newval))

(defun (setf caaddr) (newval list)
  (let ((c (caddr list)))
    (rplaca c newval)
    newval))

(defun (setf cadaar) (newval list)
  (let ((c (cdaar list)))
    (rplaca c newval)
    newval))

(defun (setf cadadr) (newval list)
  (let ((c (cdadr list)))
    (rplaca c newval)
    newval))

(defun (setf caddar) (newval list)
  (let ((c (cddar list)))
    (rplaca c newval)
    newval))

(defun (setf cadddr) (newval list)
  (let ((c (cdddr list)))
    (rplaca c newval)
    newval))

(defun (setf cdaaar) (newval list)
  (let ((c (caaar list)))
    (rplacd c newval)
    newval))

(defun (setf cdaadr) (newval list)
  (let ((c (caadr list)))
    (rplacd c newval)
    newval))

(defun (setf cdadar) (newval list)
  (let ((c (cadar list)))
    (rplacd c newval)
    newval))

(defun (setf cdaddr) (newval list)
  (let ((c (caddr list)))
    (rplacd c newval)
    newval))

(defun (setf cddaar) (newval list)
  (let ((c (cdaar list)))
    (rplacd c newval)
    newval))

(defun (setf cddadr) (newval list)
  (let ((c (cdadr list)))
    (rplacd c newval)
    newval))

(defun (setf cdddar) (newval list)
  (let ((c (cddar list)))
    (rplacd c newval)
    newval))

(defun (setf cddddr) (newval list)
  (let ((c (cdddr list)))
    (rplacd c newval)
    newval))



(defun make-list (len &key initial-element)
  (let (out)
    (dotimes (i len)
      (setq out (cons initial-element out)))
    out))


(defun copy-alist (alist)
  (let (out)
    (dolist (c alist)
      (setq out (cons (cons (car c) (cdr c)) out)))
    (reverse out)))


(defun copy-tree (tree)
  (if (atom tree)
      tree
      (cons (car tree) (cdr tree))))


(defun tree-equal (t1 t2 &key test test-not)
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (if (and (atom t1) (atom t2))
	(funcall tst t1 t2)
	(if (and (consp t1) (consp t2))
	    (and
	     (tree-equal (car t1) (car t2) :test test :test-not test-not)
	     (tree-equal (cdr t1) (cdr t2) :test test :test-not test-not))))))


(defun sublis (alist tree &key key test test-not)
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql))
	(tr (copy-tree tree)))
    (dolist (c alist)
      (nsubst (cdr c) (car c) tr :key key :test tst))
    tr))


(defun nsublis (alist tree &key key test test-not)
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (dolist (c alist)
      (nsubst (cdr c) (car c) tree :key key :test tst))
    tree))



(defun endp (l) (null l))


(defun butlast (ls &optional (n 1))
  (let* ((l (length ls))
	 (last (max 0 (- l n)))
	 out)
    (dotimes (i last)
      (setq out (cons (car ls) out))
      (setq ls (cdr ls)))
    (reverse out)))


(defun nbutlast (ls &optional (n 1))
  (butlast ls n))


(defun acons (key dat list)
  (cons (cons key dat) list))


(defun pairlis (keys data &optional alist)
  (dolist (k keys)
    (setq alist (cons (cons k (car data)) alist))
    (setq data (cdr data)))
  alist)



(define-setf-expander symbol-value (sym)
  (let ((var (gensym))
	(val (gensym)))
    (values
     `(,var)
     `(,sym)
     `(,val)
     `(set ,var ,val)
     `(symbol-value ,var))))



(defmacro defsetf (accessfn secondarg &rest args)
  (if (listp secondarg)
      (let (formsymsyms storesymsyms)
	(dolist (arg secondarg)
	  (setq formsymsyms (cons (gensym) formsymsyms)))
	(setq formsymsyms (reverse formsymsyms))
	(dolist (storevar (car args))
	  (setq storesymsyms (cons (gensym) storesymsyms)))
	(setq storesymsyms (reverse storesymsyms))
	`(define-setf-expander ,accessfn ,secondarg
	   (let (,@(mapcar (lambda (s) (list s '(gensym))) formsymsyms)
		 ,@(mapcar (lambda (s) (list s '(gensym))) storesymsyms))
	     (values
	      (list ,@formsymsyms)
	      (list ,@secondarg)
	      (list ,@storesymsyms)
	      (block ,accessfn
		(let (,@(mapcar (lambda (s v) (list s v)) secondarg formsymsyms)
		      ,@(mapcar (lambda (s v) (list s v)) (car args) storesymsyms))
		  ,@(cdr args)))
	      (list ',accessfn ,@formsymsyms)))))
      (progn
	)))



(defmacro when (clause &body body)
  `(if ,clause (progn ,@body)))


(defmacro unless (clause &body body)
  `(if (not ,clause) (progn ,@body)))


(defmacro define-modify-macro (name lambda-list func &optional doc)
  (let (args)
    (dolist (l lambda-list)
      (unless (eq l '&optional)
	(if (typep l 'cons)
	    (setq args (cons (car l) args))
	    (setq args (cons l args)))))
    `(defmacro ,name (place . ,lambda-list)
       (let ((exp (multiple-value-list (get-setf-expansion place)))
	      letf)
	  (dolist (f (car exp))
	    (setq letf (cons (list f (caadr exp)) letf))
	    (setf (cadr exp) (cdadr exp)))
	  (setq letf (cons (list (caaddr exp) (list ',func (nth 4 exp) ,@args)) letf))
	  (setq letf (list 'let* (reverse letf) nil))
	  (setf (caddr letf) (nth 3 exp))
	  letf))))


(define-modify-macro incf (&optional (delta 1)) +)

(define-modify-macro decf (&optional (delta 1)) -)


(defmacro and (&rest forms)
  (if (not forms)
      t
      (let ((valsym (gensym)))
	`(let ((,valsym (multiple-value-list ,(car forms))))
           (if (car ,valsym)
               ,(if (not (cdr forms))
                    `(values-list ,valsym)
                    `(and ,@(cdr forms))))))))


(defmacro or (&rest forms)
  (if (not forms)
      nil
      (let ((valsym (gensym)))
	`(let ((,valsym (multiple-value-list ,(car forms))))
	   (if (car ,valsym)
	       ,(if (not (cdr forms))
		    `(values-list ,valsym)
		    `(car ,valsym))
               (or ,@(cdr forms)))))))


(defmacro cond (&body body)
  (when body
    (let ((first (car body))
	  (rest (cdr body))
	  (valsym (gensym)))
      `(let ((,valsym ,(car first)))
	 (if ,valsym
	     (progn ,valsym ,@(cdr first))
	     (cond ,@rest))))))



(defmacro case (keyf &rest clauses)
  (let ((keysym (gensym))
	outsym)
    `(let ((,keysym ,keyf))
       ,(dolist (clause clauses (setq outsym (cons 'cond outsym)))
	  (cond ((or (eq (car clause) t)
		     (eq (car clause) 'otherwise))
		 (setq outsym (append outsym `((t (progn ,@(cdr clause)))))))
		((typep (car clause) 'list)
		 (setq outsym (append outsym `(((member ,keysym ',(car clause))
						(progn ,@(cdr clause)))))))
		(t
		 (setq outsym (append outsym `(((eql ,keysym ',(car clause))
						(progn ,@(cdr clause))))))))))))


(defmacro ccase (keyf &rest clauses)
  (let ((keysym (gensym))
	outsym)
    `(let ((,keysym ,keyf))
       ,(dolist (clause clauses (progn
				  (setq outsym (append outsym '((t (error 'type-error)))))
				  (setq outsym (cons 'cond outsym))))
	  (cond
	    ((listp (car clause))
	     (setq outsym (append outsym `(((member ,keysym ',(car clause))
					    (progn ,@(cdr clause)))))))
	    (t
	     (setq outsym (append outsym `(((eql ,keysym ',(car clause))
					    (progn ,@(cdr clause))))))))))))


(defmacro ecase (keyf &rest clauses)
  (let ((keysym (gensym))
	outsym)
    `(let ((,keysym ,keyf))
       ,(dolist (clause clauses (progn
				  (setq outsym (append outsym '((t (error 'type-error)))))
				  (setq outsym (cons 'cond outsym))))
	  (cond
		((listp (car clause))
		 (setq outsym (append outsym `(((member ,keysym ',(car clause))
						(progn ,@(cdr clause)))))))
		(t
		 (setq outsym (append outsym `(((eql ,keysym ',(car clause))
						(progn ,@(cdr clause))))))))))))


(defmacro typecase (keyf &rest clauses)
  (let ((keysym (gensym))
	outsym)
    `(let ((,keysym ,keyf))
       ,(dolist (clause clauses (setq outsym (cons 'cond outsym)))
	  (cond ((or (eq (car clause) t)
		     (eq (car clause) 'otherwise))
		 (setq outsym (append outsym `((t (progn ,@(cdr clause)))))))
		(t
		 (setq outsym (append outsym `(((typep ,keysym ',(car clause))
						(progn ,@(cdr clause))))))))))))


(defmacro ctypecase (keyf &rest clauses)
  (let ((keysym (gensym))
	outsym)
    `(let ((,keysym ,keyf))
       ,(dolist (clause clauses (progn
				  (setq outsym (append outsym '((t (error 'type-error)))))
				  (setq outsym (cons 'cond outsym))))
	  (setq outsym (append outsym `(((typep ,keysym ',(car clause))
					 (progn ,@(cdr clause))))))))))


(defmacro etypecase (keyf &rest clauses)
  (let ((keysym (gensym))
	outsym)
    `(let ((,keysym ,keyf))
       ,(dolist (clause clauses (progn
				  (setq outsym (append outsym '((t (error 'type-error)))))
				  (setq outsym (cons 'cond outsym))))
	  (setq outsym (append outsym `(((typep ,keysym ',(car clause))
					 (progn ,@(cdr clause))))))))))


(defmacro return (&optional val)
  `(return-from nil (values-list (multiple-value-list ,val))))


(defmacro multiple-value-bind (vars valform &body forms)
  (let ((restsym (gensym)))
    `(multiple-value-call (lambda (&optional ,@vars &rest ,restsym)
			    ,@forms)
       ,valform)))


(defmacro prog (bins &body body)
  `(let ,bins
     (block nil
       (tagbody
	  ,@body))))


(defmacro prog* (bins &body body)
  `(let* ,bins
     (block nil
       (tagbody
	  ,@body))))



(defun every (pred &rest sequences)
  (let ((n (apply 'min (mapcar #'length sequences))))
    (dotimes (i n)
      (let ((args (mapcar (lambda (s) (elt s i)) sequences)))
	(if (not (apply pred args))
	    (return-from every nil)))))
  t)


(defun some (pred &rest sequences)
  (not (apply 'every (complement pred) sequences)))


(defun notany (pred &rest sequences)
  (not (apply 'some pred sequences)))


(defun notevery (pred &rest sequences)
  (not (apply 'every pred sequences)))


(defun member (obj list &key key test test-not)
  (let ((tst (or test
	      (if test-not (complement test-not))
	       #'eql)))
    (unless key
      (setq key #'identity))
    (dotimes (i (length list))
      (if (funcall tst obj (funcall key (car list)))
	  (return-from member list))
      (setq list (cdr list)))))


(defun member-if (pred list &key key)
  (unless key
    (setq key #'identity))
  (dotimes (i (length list))
    (if (funcall pred (funcall key (car list)))
	(return-from member-if list))
    (setq list (cdr list))))


(defun member-if-not (pred list &key key)
  (member-if (complement pred) list :key key))


(defun find (obj seq &key from-end test test-not (start 0) end key)
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (find-if (lambda (x) (funcall tst obj x)) seq :from-end from-end :start start :end end :key key)))


(defun find-if (pred seq &key from-end (start 0) end key)
  (unless end
    (setq end (length seq)))
  (unless key
    (setq key #'identity))
  (dotimes (i (- end start))
    (let ((j (if from-end
		 (- end i 1)
		 (+ start i))))
      (if (funcall pred (funcall key (elt seq j)))
	  (return-from find-if (elt seq j))))))


(defun find-if-not (pred seq &key from-end (start 0) end key)
  (find-if (complement pred) seq :from-end from-end :start start :end end :key key))


(defun assoc (obj alist &key key test test-not)
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (assoc-if (lambda (x) (funcall tst obj x)) alist :key key)))


(defun assoc-if (pred alist &key key)
  (unless key
    (setq key #'identity))
  (dotimes (i (length alist))
    (if (and
	 (car alist)
	 (funcall pred (funcall key (caar alist))))
	(return-from assoc-if (car alist)))
    (setq alist (cdr alist))))


(defun assoc-if-not (pred alist &key key)
  (assoc-if (complement pred) alist :key key))


(defun rassoc (obj alist &key key test test-not)
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (rassoc-if (lambda (x) (funcall tst obj x)) alist :key key)))


(defun rassoc-if (pred alist &key key)
  (unless key
    (setq key #'identity))
  (dotimes (i (length alist))
    (if (and
	 (car alist)
	 (funcall pred (funcall key (cdar alist))))
	(return-from rassoc-if (car alist)))
    (setq alist (cdr alist))))


(defun rassoc-if-not (pred alist &key key)
  (rassoc-if (complement pred) alist :key key))


(defun position (obj seq &key from-end test test-not (start 0) end key)
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (position-if (lambda (x) (funcall tst obj x)) seq :from-end from-end :start start :end end :key key)))


(defun position-if (pred seq &key from-end (start 0) end key)
  (unless end
    (setq end (length seq)))
  (unless key
    (setq key #'identity))
  (dotimes (i (- end start))
    (let ((j (if from-end
		 (- end i 1)
		 (+ start i))))
      (if (funcall pred (elt seq j))
	  (return-from position-if j)))))


(defun position-if-not (pred seq &key from-end (start 0) end key)
  (position-if (complement pred) seq :from-end from-end :start start :end end :key key))


(defun count (obj seq &key from-end (start 0) end key test test-not)
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql))
	(ret 0))
    (unless end
      (setq end (length seq)))
    (unless key
      (setq key #'identity))
    (dotimes (i (- end start))
      (let ((j (if from-end
		   (- end i 1)
		   (+ start i))))
	(if (funcall tst obj (funcall key (elt seq j)))
	    (setq ret (+ 1 ret)))))
    ret))


(defun count-if (pred seq &key from-end (start 0) end key)
  (unless end
    (setq end (length seq)))
  (unless key
    (setq key #'identity))
  (let ((ret 0))
    (dotimes (i (- end start))
      (let ((j (if from-end
		   (- end i 1)
		   (+ start i))))
	(if (funcall pred (funcall key (elt seq j)))
	    (setq ret (+ 1 ret)))))
    ret))


(defun count-if-not (pred seq &key from-end (start 0) end key)
  (count-if (complement pred) seq :from-end from-end :start start :end end :key key))


(defun remove (obj seq)
  (remove-if (lambda (ob) (eql ob obj)) seq))


(defun remove-if-not (pred seq)
  (remove-if (complement pred) seq))


(defun delete (obj seq)
  (remove-if (lambda (ob) (eql ob obj)) seq))


(defun delete-if (pred seq)
  (remove-if pred seq))


(defun delete-if-not (pred seq)
  (remove-if (complement pred) seq))


(defun remove-duplicates (seq &key from-end test test-not (start 0) end key)
  (let* ((len (length seq))
	 (n (if end (- end start) len))
	 (removed (make-array len))
	 leftnum
	 (out (if from-end (reverse seq)
		  (concatenate (if (stringp seq)
				   'string
				   (if (arrayp seq)
				       'vector
				       'list)) seq)))
	 (tst (or test
		  (if test-not (complement test-not))
		  #'eql))
	 (k (or key #'identity)))
    (dotimes (i n)
      (dotimes (l (- n i 1))
	(when (funcall tst (funcall k (elt out (+ start i)))
		       (funcall k (elt out (+ start i l 1))))
	  (setf (aref removed (+ start i)) t)
	  (return nil))))
    (setq leftnum (count-if #'not removed))
    (dotimes (i len)
      (dotimes (l (- len i))
	(unless (aref removed (+ l i))
	  (setf (elt out i) (elt out (+ l i)))
	  (setf (aref removed (+ l i)) t)
	  (return nil))))
    (if (arrayp seq)
	(adjust-array out leftnum)
	(if (consp seq)
	    (rplacd (nthcdr (1- leftnum) out) nil)))
    (if from-end
	(reverse out)
	out)))


(defun delete-duplicates (&rest args)
  (apply 'remove-duplicates args))


(defun substitute (newobj oldobj seq &key from-end test test-not (start 0) end count key)
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (substitute-if newobj (lambda (x) (funcall tst oldobj x)) seq :from-end from-end :start start :end end :count count :key key)))


(defun substitute-if (newobj pred seq &key from-end (start 0) end count key)
  (unless end
    (setq end (length seq)))
  (unless key
    (setq key #'identity))
  (let ((out (copy-seq seq)))
    (dotimes (i (- end start))
      (let ((j (if from-end
		   (- end i 1)
		   (+ start i))))
	(if (and count (= 0 count))
	    (return-from substitute-if out))
	(when (funcall pred (funcall key (elt out j)))
	  (setf (elt out j) newobj)
	  (if count
	      (decf count)))))
    out))


(defun substitute-if-not (newobj pred seq &key from-end (start 0) end count key)
  (substitute-if newobj (complement pred) seq :from-end from-end :start start :end end :count count :key key))


(defun nsubstitute (&rest args)
  (apply 'substitute args))


(defun nsubstitute-if (&rest args)
  (apply 'substitute-if args))


(defun nsubstitute-if-not (&rest args)
  (apply 'substitute-if-not args))



(defun subst (new old tree &key key test test-not)
  (nsubst new old (copy-tree tree) :key key :test test :test-not test-not))


(defun subst-if (new pred tree &key key)
  (nsubst-if new pred (copy-tree tree) :key key))


(defun subst-if-not (new pred tree &key key)
  (nsubst-if-not new pred (copy-tree tree) :key key))


(defun nsubst (new old tree &key key test test-not)
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (nsubst-if new (lambda (x) (funcall tst old x)) tree :key key)))


(defun nsubst-if (new pred tree &key key)
  (unless key
    (setq key #'identity))
  (cond
    ((funcall pred (funcall key tree))
     new)
    ((atom tree)
     tree)
    (t
     (rplaca tree (nsubst-if new pred (car tree)))
     (rplacd tree (nsubst-if new pred (cdr tree)))
     tree)))


(defun nsubst-if-not (new pred tree &key key)
  (nsubst-if new (complement pred) tree :key key))



(defun nreverse (seq)
  (reverse seq))


(defun revappend (list tail)
  (nconc (reverse list) tail))


(defun nreconc (list tail)
  (nconc (reverse list) tail))


(defun adjoin (obj list)
  (if (member obj list) list (cons obj list)))


(defun fill (seq obj &key (start 0) end)
  (dotimes (i (if end (- end start) (length seq)))
    (setf (elt seq (+ i start)) obj))
  seq)


(defun replace (seq1 seq2 &key (start1 0) end1 (start2 0) end2)
  (unless end1
    (setq end1 (length seq1)))
  (unless end2
    (setq end2 (length seq2)))
  (dotimes (i (min (- end1 start1) (- end2 start2)))
    (setf (elt seq1 (+ i start1)) (elt seq2 (+ i start2))))
  seq1)


(defmacro push (item place)
  (let ((exp (multiple-value-list (get-setf-expansion place)))
	(itemkey (gensym))
	letf)
    (dolist (f (car exp))
      (setq letf (cons (cons f (cadr exp)) letf))
      (setf (cadr exp) (cdadr exp)))
    (setq letf (reverse letf))
    `(let* ((,itemkey ,item)
	    ,@letf
	    (,(caaddr exp) (cons ,itemkey ,(nth 4 exp))))
       ,(nth 3 exp)
       ,(nth 4 exp))))


(defmacro pushnew (item place &key key test test-not)
  (let ((exp (multiple-value-list (get-setf-expansion place)))
	(blocksym (gensym))
	(itemsym (gensym))
	(keysym (gensym))
	(tstsym (gensym))
	letf)
    (dolist (f (car exp))
      (setq letf (cons (cons f (cadr exp)) letf))
      (setf (cadr exp) (cdadr exp)))
    (setq letf (reverse letf))
    `(block ,blocksym
       (let* ((,itemsym ,item)
	      ,@letf
	      (,keysym (or ,key #'identity))
	      (,tstsym (or ,test
			   (if ,test-not (complement ,test-not))
			   #'eql))
	      (,(caaddr exp) (cons ,itemsym ,(nth 4 exp))))
	 (dolist (l ,(nth 4 exp))
	   (if (funcall ,tstsym (car ,(caaddr exp)) (funcall ,keysym l))
	       (return-from ,blocksym ,(nth 4 exp))))
	 ,(nth 3 exp)))))


(defmacro pop (place)
  (let ((exp (multiple-value-list (get-setf-expansion place)))
	(carsym (gensym))
	letf)
    (dolist (f (car exp))
      (setq letf (cons (cons f (cadr exp)) letf))
      (setf (cadr exp) (cdadr exp)))
    (setq letf (reverse letf))
    `(let* (,@letf
	    (,carsym (car ,(nth 4 exp)))
	    (,(caaddr exp) (cdr ,(nth 4 exp))))
       ,(nth 3 exp)
       ,carsym)))


(defun set-difference (l1 l2 &key key test test-not)
  (unless key
    (setq key #'identity))
  (let ((out (concatenate 'list l1))
	(tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (dolist (l l2)
      (setq out (remove-if (lambda (o) (funcall tst (funcall key o)
						(funcall key l)))
			   out)))
    out))


(defun nset-difference (&rest args)
  (apply 'set-difference args))


(defun union (list-1 list-2 &key key test test-not)
  (remove-duplicates (append list-1 list-2) :key key :test test :test-not test-not))


(defun nunion (&rest args)
  (apply 'union args))


(defun intersection (list-1 list-2 &key key test test-not)
  (unless key
    (setq key #'identity))
  (let (out)
    (dolist (l list-1)
      (if (member (funcall key l) list-2 :key key :test test :test-not test-not)
	  (setq out (cons l out))))
    out))


(defun nintersection (&rest args)
  (apply 'intersection args))


(defun set-exclusive-or (list-1 list-2 &key key test test-not)
  (union (set-difference list-1 list-2 :key key :test test :test-not test-not)
	 (set-difference list-2 list-1 :key key :test test :test-not test-not)))


(defun nset-exclusive-or (&rest args)
  (apply 'set-exclusive-or args))


(defun subsetp (l1 l2 &key key test test-not)
  (unless key
    (setq key #'identity))
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (dolist (l l1)
      (unless (member (funcall key l) l2 :test tst)
	(return-from subsetp nil)))
    t))



(defun mismatch (seq1 seq2 &key from-end test test-not key (start1 0) (start2 0) end1 end2)
  (unless key
    (setq key #'identity))
  (unless end1
    (setq end1 (length seq1)))
  (unless end2
    (setq end2 (length seq2)))
  (when from-end
    (setq seq1 (reverse seq1))
    (let ((tmp end1))
      (setq end1 (- (length seq1) start1))
      (setq start1 (- (length seq1) tmp)))
    (setq seq2 (reverse seq2))
    (let ((tmp end2))
      (setq end2 (- (length seq2) start2))
      (setq start2 (- (length seq2) tmp))))
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (dotimes (i (min (- end1 start1) (- end2 start2)))
      (unless (funcall tst (funcall key (elt seq1 (+ start1 i)))
		       (funcall key (elt seq2 (+ start2 i))))
	(return-from mismatch (if from-end
				  (- (length seq1) (+ start1 i))
				  (+ start1 i)))))
    (if (/= (- end1 start1) (- end2 start2))
	(if from-end
	    (- (length seq1) (+ start1 (min (- end1 start1) (- end2 start2))))
	    (+ start1 (min (- end1 start1) (- end2 start2)))))))



(defun search (seq1 seq2 &key from-end test test-not key (start1 0) (start2 0) end1 end2)
  (unless key
    (setq key #'identity))
  (unless end1
    (setq end1 (length seq1)))
  (unless end2
    (setq end2 (length seq2)))
  (if (<= (- end1 start1) 1)
      (return-from search nil))
  (when from-end
    (setq seq1 (reverse seq1))
    (let ((tmp end1))
      (setq end1 (- (length seq1) start1))
      (setq start1 (- (length seq1) tmp)))
    (setq seq2 (reverse seq2))
    (let ((tmp end2))
      (setq end2 (- (length seq2) start2))
      (setq start2 (- (length seq2) tmp))))
  (let ((tst (or test
		 (if test-not (complement test-not))
		 #'eql)))
    (dotimes (i (+ 1 (- end2 start2 (length seq1))))
      (when (funcall tst (funcall key (elt seq1 start1))
		     (funcall key (elt seq2 (+ start2 i))))
	(dotimes (j (- end1 start1) (if from-end (return-from search (- (length seq2) i (- end1 start1)))
					(return-from search i)))
	  (unless (funcall tst (funcall key (elt seq1 (+ start1 j)))
			   (funcall key (elt seq2 (+ start2 i j))))
	    (return nil)))))))



(defun sort (seq pred &key key)
  (stable-sort seq pred :key key))


(defun stable-sort (seq pred &key key)
  (unless key
    (setq key #'identity))
  (let ((len (length seq)))
    (dotimes (i len)
      (dotimes (j (- len i 1))
	(if (funcall pred (funcall key (elt seq (+ j 1)))
		     (funcall key (elt seq j)))
	    (let ((tmp (elt seq j)))
	      (setf (elt seq j) (elt seq (+ j 1)))
	      (setf (elt seq (+ j 1)) tmp))))))
  seq)



(defun array-rank (array)
  (length (array-dimensions array)))


(defun array-dimension (array axis)
  (nth axis (array-dimensions array)))


(defun array-total-size (array)
  (apply #'* (array-dimensions array)))


(defun array-in-bounds-p (array &rest inds)
  (and
   (eql (array-rank array) (length inds))
   (every (lambda (x y) (and (>= x 0) (< x y)))
	  inds
	  (array-dimensions array))))


(defun array-element-type (array)
  (cond
    ((stringp array) 'character)
    ((bit-vector-p array) 'bit)
    (t)))


(defun upgraded-array-element-type (ts &optional env)
  (cond
    ((subtypep ts 'bit) 'bit)
    ((subtypep ts 'character) 'character)
    (t t)))


(defun adjustable-array-p (arr)
  t)



(defun get (sym ind &optional (def nil))
  (let* ((pl (symbol-plist sym))
	 (l (length pl)))
    (do ((i 0 (+ i 2)))
	((>= i l))
      (if (eq (car pl) ind)
	  (return-from get (cadr pl)))
      (setq pl (cddr pl))))
  def)


(defun get-properties (pl il)
  (let ((l (length pl)))
    (do ((i 0 (+ i 2)))
	((>= i l))
      (if (position-if (lambda (e) (eq e (car pl))) il)
	  (return-from get-properties (values (car pl) (cadr pl) pl)))
      (setq pl (cddr pl))))
  (values nil nil nil))


(defun remprop (sym ind)
  (let ((propl (symbol-plist sym))
	prevpl)
    (if (eq (car propl) ind)
	(progn
	  (setf (symbol-plist sym) (cddr propl))
	  (return-from remprop t))
	(progn
	  (setq prevpl propl)
	  (setq propl (cddr propl))
	  (do nil
	      ((not propl))
	    (when (eq (car propl) ind)
	      (rplacd (cdr prevpl) (cddr propl))
	      (return-from remprop t))
	    (setq prevpl propl)
	    (setq propl (cddr propl)))))))



(defun char (str ind)
  (aref str ind))

(defun (setf char) (newval str ind)
  (setf (aref str ind) newval))

(defun schar (str ind)
  (aref str ind))

(defun (setf schar) (newval str ind)
  (setf (aref str ind) newval))

(defun bit (arr &rest subs)
  (apply 'aref arr subs))

(defun (setf bit) (newval arr &rest subs)
  (apply #'(setf aref) newval arr subs))

(defun sbit (arr &rest subs)
  (apply 'aref arr subs))

(defun (setf sbit) (newval arr &rest subs)
  (apply #'(setf aref) newval arr subs))

(defun svref (vec ind)
  (aref vec ind))

(defun (setf svref) (newval vec ind)
  (setf (aref vec ind) newval))


(defun vector-pop (vec)
  (setf (fill-pointer vec) (1- (fill-pointer vec)))
  (aref vec (fill-pointer vec)))

(defun vector-push (newel vec)
  (unless (>= (fill-pointer vec) (array-dimension vec 0))
    (setf (aref vec (fill-pointer vec)) newel)
    (setf (fill-pointer vec) (1+ (fill-pointer vec)))
    (1- (fill-pointer vec))))

(defun vector-push-extend (newel vec &optional (ext 1))
  (if (>= (fill-pointer vec) (array-dimension vec 0))
      (adjust-array vec (+ (array-dimension vec 0) ext)))
  (vector-push newel vec))


(defun string= (str1 str2 &key (start1 0) end1 (start2 0) end2)
  (setq str1 (string str1))
  (setq str2 (string str2))
  (unless end1
    (setq end1 (length str1)))
  (unless end2
    (setq end2 (length str2)))
  (let ((l1 (- end1 start1))
	(l2 (- end2 start2)))
    (if (/= l1 l2)
	(return-from string= nil))
    (dotimes (i (min l1 l2) t)
      (unless (char= (elt str1 (+ start1 i)) (elt str2 (+ start2 i)))
	(return-from string= nil)))))


(defun string/= (str1 str2 &key (start1 0) end1 (start2 0) end2)
  (setq str1 (string str1))
  (setq str2 (string str2))
  (unless end1
    (setq end1 (length str1)))
  (unless end2
    (setq end2 (length str2)))
  (let ((l1 (- end1 start1))
	(l2 (- end2 start2)))
    (dotimes (i (min l1 l2))
      (unless (char= (elt str1 (+ start1 i)) (elt str2 (+ start2 i)))
	(return-from string/= (+ start1 i))))
    (if (/= l1 l2)
	(min l1 l2))))


(defun string< (str1 str2 &key (start1 0) end1 (start2 0) end2)
  (setq str1 (string str1))
  (setq str2 (string str2))
  (unless end1
    (setq end1 (length str1)))
  (unless end2
    (setq end2 (length str2)))
  (let ((l1 (- end1 start1))
	(l2 (- end2 start2)))
    (dotimes (i (min l1 l2))
      (if (char< (elt str1 (+ start1 i)) (elt str2 (+ start2 i)))
	  (return-from string< (+ start1 i))
	  (if (char> (elt str1 (+ start1 i)) (elt str2 (+ start2 i)))
	      (return-from string< nil))))
    (if (< l1 l2)
	end1)))


(defun string<= (str1 str2 &key (start1 0) end1 (start2 0) end2)
  (unless end1
    (setq end1 (length str1)))
  (let ((ret (funcall 'string< str1 str2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))
    (if ret
	ret
	(let ((ret (funcall 'string= str1 str2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))
	  (if ret
	      end1)))))


(defun string> (str1 str2 &key (start1 0) end1 (start2 0) end2)
  (setq str1 (string str1))
  (setq str2 (string str2))
  (unless end1
    (setq end1 (length str1)))
  (unless end2
    (setq end2 (length str2)))
  (let ((l1 (- end1 start1))
	(l2 (- end2 start2)))
    (dotimes (i (min l1 l2))
      (if (char> (elt str1 (+ start1 i)) (elt str2 (+ start2 i)))
	  (return-from string> (+ start1 i))
	  (if (char< (elt str1 (+ start1 i)) (elt str2 (+ start2 i)))
	      (return-from string> nil))))
    (if (> l1 l2)
	end2)))


(defun string>= (str1 str2 &key (start1 0) end1 (start2 0) end2)
  (unless end1
    (setq end1 (length str1)))
  (let ((ret (funcall 'string> str1 str2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))
    (if ret
	ret
	(let ((ret (funcall 'string= str1 str2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))
	  (if ret
	      end1)))))


(defun string-equal (&rest strings)
  (apply #'string= (mapcar #'string-upcase strings)))


(defun string-not-equal (&rest strings)
  (apply #'string/= (mapcar #'string-upcase strings)))


(defun string-lessp (&rest strings)
  (apply #'string< (mapcar #'string-upcase strings)))


(defun string-not-greaterp (&rest strings)
  (apply #'string<= (mapcar #'string-upcase strings)))


(defun string-greaterp (&rest strings)
  (apply #'string> (mapcar #'string-upcase strings)))


(defun string-not-lessp (&rest strings)
  (apply #'string>= (mapcar #'string-upcase strings)))


(defun char/= (&rest chars)
  (do ((c chars (cdr c)))
      ((not c) t)
    (dolist (d (cdr c))
      (if (char= (car c) d)
	  (return-from char/= nil)))))


(defun char< (&rest chars)
  (do ((c chars (cdr c)))
      ((not (cdr c)) t)
    (if (>= (char-code (car c)) (char-code (cadr c)))
	(return-from char< nil))))


(defun char<= (&rest chars)
  (do ((c chars (cdr c)))
      ((not (cdr c)) t)
    (if (> (char-code (car c)) (char-code (cadr c)))
	(return-from char<= nil))))


(defun char> (&rest chars)
  (do ((c chars (cdr c)))
      ((not (cdr c)) t)
    (if (<= (char-code (car c)) (char-code (cadr c)))
	(return-from char> nil))))


(defun char>= (&rest chars)
  (do ((c chars (cdr c)))
      ((not (cdr c)) t)
    (if (< (char-code (car c)) (char-code (cadr c)))
	(return-from char>= nil))))


(defun char-equal (&rest chars)
  (apply #'char= (mapcar #'char-upcase chars)))


(defun char-not-equal (&rest chars)
  (apply #'char/= (mapcar #'char-upcase chars)))


(defun char-lessp (&rest chars)
  (apply #'char< (mapcar #'char-upcase chars)))


(defun char-not-greaterp (&rest chars)
  (apply #'char<= (mapcar #'char-upcase chars)))


(defun char-greaterp (&rest chars)
  (apply #'char> (mapcar #'char-upcase chars)))


(defun char-not-lessp (&rest chars)
  (apply #'char>= (mapcar #'char-upcase chars)))


(defun digit-char (weight &optional (radix 10))
  (if (or (>= weight radix))
      ()
      (aref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" weight)))


(defun digit-char-p (char &optional (radix 10))
  (let ((str "00112233445566778899aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"))
    (dotimes (i (* 2 radix))
      (if (char= char (aref str i))
	  (return-from digit-char-p (values (floor i 2)))))
    nil))


(defun char-int (ch)
  (char-code ch))


(defun string-upcase (s)
  (let* ((str (string s))
	 (ret (make-string (length str))))
    (dotimes (i (length str))
      (setf (aref ret i) (char-upcase (aref str i))))
    ret))


(defun string-downcase (s)
  (let* ((str (string s))
	 (ret (make-string (length str))))
    (dotimes (i (length str))
      (setf (aref ret i) (char-downcase (aref str i))))
    ret))


(defun string-capitalize (s)
  (let* ((str (string s))
	 (ret (make-string (length str)))
	 (notword t))
    (dotimes (i (length str))
      (if (alphanumericp (aref str i))
	  (if notword
	      (progn
		(setf (aref ret i) (char-upcase (aref str i)))
		(setq notword nil))
	      (setf (aref ret i) (aref str i)))
	  (progn
	    (setq notword t)
	    (setf (aref ret i) (aref str i)))))
    ret))


(defun nstring-upcase (str)
  (dotimes (i (length str))
    (setf (aref str i) (char-upcase (aref str i))))
  str)


(defun nstring-downcase (str)
  (dotimes (i (length str))
    (setf (aref str i) (char-downcase (aref str i))))
  str)


(defun nstring-capitalize (str)
  (let ((notword t))
    (dotimes (i (length str))
      (if (alphanumericp (aref str i))
	  (when notword
	    (setf (aref str i) (char-upcase (aref str i)))
	    (setq notword nil))
	  (setq notword t))))
  str)


(defun string-left-trim (charbag s)
  (let ((str (string s)))
    (dotimes (i (length str) (subseq str 0 0))
      (if (not (position (aref str i) charbag))
	  (return-from string-left-trim (subseq str i))))))


(defun string-right-trim (charbag s)
  (let* ((str (string s))
	 (len (length str)))
    (dotimes (i len (subseq str 0 0))
      (if (not (position (aref str (- len i 1)) charbag))
	  (return-from string-right-trim (subseq str 0 (- len i)))))))


(defun string-trim (charbag s)
  (string-left-trim charbag (string-right-trim charbag s)))



(defmacro defpackage (s &rest opts)
  (let ((name (string s))
	nicks uses imports exports intern)
    (dolist (opt opts)
      (case (car opt)
	(:nicknames (setq nicks (append nicks (mapcar #'string (cdr opt)))))
	(:use (setq uses (append uses (mapcar #'find-package (cdr opt)))))
	(:import-from (setq imports (append imports (list (list (find-package (cadr opt)) (mapcar #'string (cddr opt)))))))
	(:export (setq exports (append exports (mapcar #'string (cdr opt)))))
	(:intern (setq intern (append intern (mapcar #'string (cdr opt)))))))
    `(progn
       (unless (find-package ,name)
	 (make-package ,name))
       (rename-package ,name ,name ',nicks)
       (use-package ',uses ,name)
       ,@(mapcar (lambda (l) `(import (list ,@(mapcar (lambda (sm) `(intern ,sm ,(car l))) (cadr l))) ,name)) imports)
       ,@(mapcar (lambda (sm) `(intern ,sm ,name)) intern)
       (export (list ,@(mapcar (lambda (st) `(intern ,st ,name)) exports)) ,name)
       (find-package ,name))))



(deftype signed-byte (&optional s)
  (if (eq s '*)
      'integer
      `(integer ,(- (expt 2 (1- s))) ,(1- (expt 2 (1- s))))))


(deftype unsigned-byte (&optional s)
  (if (eq s '*)
      '(integer 0 *)
      `(integer 0 ,(1- (expt 2 s)))))


(deftype mod (s)
  `(integer 0 ,(1- s)))


(deftype extended-char ()
  nil)



(defun consp (obj)
  (typep obj 'cons))

(defun atom (obj)
  (typep obj 'atom))

(defun listp (obj)
  (typep obj 'list))

(defun symbolp (obj)
  (typep obj 'symbol))

(defun keywordp (obj)
  (typep obj 'keyword))

(defun compiled-function-p (obj)
  (typep obj 'compiled-function))

(defun functionp (obj)
  (typep obj 'function))

(defun packagep (obj)
  (typep obj 'package))

(defun integerp (obj)
  (typep obj 'integer))

(defun rationalp (obj)
  (typep obj 'rational))

(defun floatp (obj)
  (typep obj 'float))

(defun complexp (obj)
  (typep obj 'complex))

(defun random-state-p (obj)
  (typep obj 'random-state))

(defun characterp (obj)
  (typep obj 'character))

(defun standard-char-p (obj)
  (typep obj 'standard-char))

(defun vectorp (obj)
  (typep obj 'vector))

(defun simple-vector-p (obj)
  (typep obj 'simple-vector))

(defun arrayp (obj)
  (typep obj 'array))

(defun sequencep (obj)
  (typep obj 'sequence))

(defun stringp (obj)
  (typep obj 'string))

(defun simple-string-p (obj)
  (typep obj 'simple-string))

(defun bit-vector-p (obj)
  (typep obj 'bit-vector))

(defun simple-bit-vector-p (obj)
  (typep obj 'simple-bit-vector))

(defun hash-table-p (obj)
  (typep obj 'hash-table))

(defun pathnamep (obj)
  (typep obj 'pathname))

(defun streamp (obj)
  (typep obj 'stream))

(defun realp (obj)
  (typep obj 'real))

(defun numberp (obj)
  (typep obj 'number))


(defmacro check-type (place type &optional str)
  `(unless (typep ,place ',type)
     (error 'type-error)))



(defmacro assert (test &optional places (conddat "Assertion does not hold") condargs)
  `(unless ,test
     (apply 'error ,conddat ,condargs)))



(defun macroexpand (form)
  (do ((ret (list form t))
       expp)
      ((not (cadr ret)) (values (car ret) expp))
    (setq ret (multiple-value-list (macroexpand-1 (car ret))))
    (if (and (not expp) (cadr ret))
	(setq expp t))))



(defun equal (x y)
  (cond
    ((numberp x) (eql x y))
    ((characterp x) (eql x y))
    ((and (stringp x) (stringp y))
     (let ((l (length x)))
       (if (= l (length y))
	   (dotimes (i l t)
	     (if (not (eql (elt x i) (elt y i)))
		 (return-from equal nil))))))
    ((and (consp x) (consp y))
     (and
      (equal (car x) (car y))
      (equal (cdr x) (cdr y))))
    (t (eq x y))))


#|(defun equalp (x y)
  (cond
    ((and (numberp x) (numberp y)) (= x y))
    ((and (characterp x) (characterp y)) (char-equal x y))
    ((and (consp x) (consp y))
     (and
      (equalp (car x) (car y))
      (equalp (cdr x) (cdr y))))
    ((and (arrayp x) (arrayp y)) (let ((dimsx (array-dimensions x))
				       (dimsy (array-dimensions y)))
				   (and
				    (= (length dimsx) (length dimsy))
				    (dolist (dimx dimsx t)
				      (unless (= dimx (car dimsy))
					 (return-from equalp nil))
				      (setq dimsy (cdr dimsy)))
				    (dotimes (i (array-total-size x) t)
				      (unless (equalp (row-major-aref x i) (row-major-aref y i))
					(return-from equalp nil))))))
    (t (eq x y))))
|#


(defun fdefinition (fname)
  (symbol-function fname))



(defun complement (func)
  (lambda (&rest args) (not (apply func args))))



(defun mapc (fun &rest lists)
  (apply #'mapcar fun lists)
  (car lists))



(defun mapcan (fun &rest lists)
  (apply #'nconc (apply 'mapcar fun lists)))



(defun maplist (fun &rest lists)
  (let (out)
    (do nil
	(nil)
      (if (member nil lists)
	  (return nil))
      (setq out (cons (apply fun lists) out))
      (setq lists (mapcar #'cdr lists)))
    (reverse out)))



(defun mapl (fun &rest lists)
  (apply #'maplist fun lists)
  (car lists))



(defun mapcon (fun &rest lists)
  (apply #'nconc (apply 'maplist fun lists)))



(defun map-into (res fun &rest seqs)
  (let ((len (apply 'min (mapcar #'length (cons res seqs)))))
    (dotimes (i len)
      (setf (elt res i) (apply fun (mapcar (lambda (s) (elt s i)) seqs))))
    res))


(defun reduce (fun seq &key key from-end (start 0) end (initial-value nil initvalprov))
  (unless end
    (setq end (length seq)))
  (unless key
    (setq key #'identity))
  (cond
    ((= (- end start) 0)
     (if initvalprov
	 initial-value
	 (funcall fun)))
    ((and (= (- end start) 1)
	  (not initvalprov))
     (funcall key (elt seq start)))
    (t
     (let (out)
       (dotimes (i (- end start))
	 (let ((j (if from-end
		      (- end i 1)
		      (+ start i))))
	   (if (= i 0)
	       (if initvalprov
		   (setq out (funcall fun initial-value (funcall key (elt seq j))))
		   (progn
		     (setq out (funcall key (elt seq j)))))
	       (if from-end
		   (setq out (funcall fun (funcall key (elt seq j)) out))
		   (setq out (funcall fun out (funcall key (elt seq j))))))))
       out))))


(defun merge (restype seq1 seq2 pred &key key)
  (unless key
    (setq key #'identity))
  (let* ((outsz (+ (length seq1) (length seq2)))
	 (out (cond
		((subtypep restype 'list)
		 (make-list outsz))
		((subtypep restype 'string)
		 (make-string outsz))
		(t
		 (make-array outsz))))
	 (j 0)
	 (k 0))
    (dotimes (i outsz)
      (if (and
	   (< j (length seq1))
	   (or
	    (>= k (length seq2))
	    (funcall pred (funcall key (elt seq1 j))
		     (funcall key (elt seq2 k)))
	    (and (not (funcall pred (funcall key (elt seq1 j))
			       (funcall key (elt seq2 k))))
		 (not (funcall pred (funcall key (elt seq2 k))
			       (funcall key (elt seq1 j)))))))
	  (progn
	    (setf (elt out i) (elt seq1 j))
	    (incf j))
	  (progn
	    (setf (elt out i) (elt seq2 k))
	    (incf k))))
    out))



(deftype logical-pathname ()
  'pathname)

(defun logical-pathname (fn)
  (pathname fn))


(defun pathname-host (fn &key case))

(defun pathname-device (fn &key case))

(defun pathname-directory (fn &key case))

(defun pathname-type (fn &key case))

(defun pathname-version (fn &key case))


(defun namestring (fn) (pathname-name fn))

(defun file-namestring (fn) (pathname-name fn))

(defun directory-namestring (fn))

(defun host-namestring (fn))

(defun enough-namestring (fn &optional d) (pathname-name fn))


(defun merge-pathnames (pn &optional defpn defv)
  (pathname pn))



(defun file-author (fn))

(defun file-write-date (fn))

(defun user-homedir-pathname (&optional h))



(defmacro with-open-file ((str fn &rest opts) &body forms)
  `(let ((,str (open ,fn ,@opts)))
     ,@forms))



(defun terpri (&optional (out *standard-output*))
  (write-char #\newline out)
  nil)


(defun write-line (string &optional (out *standard-output*))
  (write-string string out)
  (terpri out)
  string)


(defun write-sequence (seq str &key (start 0) end)
  (dotimes (i (if end (- end start) (length seq)))
    (princ (elt seq (+ i start)) str))
  seq)


(defun prin1 (obj &optional (out *standard-output*))
  (let ((*print-escape* t))
    (write obj :stream out)))


(defun princ (obj &optional (out *standard-output*))
  (let ((*print-escape* nil)
	(*print-readably* nil))
    (write obj :stream out)))


(defun print (obj &optional (out *standard-output*))
  (terpri out)
  (prin1 obj out)
  (write-char #\space out)
  obj)


(defun write-to-string (obj &rest opts &key array base case circle escape gensym
			      length level lines miser-width pprint-dispatch
			      pretty radix readably right-margin)
  (let ((out (make-string-output-stream)))
    (apply 'write obj :stream out opts)
    (get-output-stream-string out)))


(defun prin1-to-string (obj)
  (let ((out (make-string-output-stream)))
    (prin1 obj out)
    (get-output-stream-string out)))


(defun princ-to-string (obj)
  (let ((out (make-string-output-stream)))
    (princ obj out)
    (get-output-stream-string out)))


(defmacro do-all-symbols (varres &body body)
  (let ((packsym (gensym)))
    `(progn
       (dolist (,packsym (list-all-packages))
	 (do-symbols ,(list (car varres) packsym)
	   ,@body))
       (let (,(car varres))
	 ,(cadr varres)))))



(defun loop-parse-accumulation (forms)
  (let ((sym (string (car forms)))
	var
	out)
    (if (string= (string (caddr forms)) "INTO")
	(setq var (cadddr forms))
	(setq var (gensym)))
    (cond
      ((or (string= sym "COLLECT")
	   (string= sym "COLLECTING"))
       (setq out `(setq ,var (append ,var `(,,(cadr forms))))))
      ((or (string= sym "APPEND")
	   (string= sym "APPENDING"))
       (setq out `(setq ,var (append ,var ,(cadr forms)))))
      ((or (string= sym "NCONC")
	   (string= sym "NCONCING"))
       (setq out `(setq ,var (nconc ,var ,(cadr forms))))))
    (if (string= (string (caddr forms)) "INTO")
	(values out var nil (cddddr forms))
	(values out nil var (cddr forms)))))


(defun loop-parse-iteration (forms)
  (let (iter)
    (setq iter `(,(cadr forms)))
    (setq forms (cddr forms))
    (do ((f forms))
	((not f) (setq forms f))
      (if (atom (car f))
	  (let ((sym (string (car f))))
	    (cond
	      ((string= sym "FROM")
	       (if (= (length iter) 1)
		   (setq iter `(:from ,(car iter) nil 0 nil 1 1)))
	       (setf (elt iter 3) (cadr f))
	       (setq f (cddr f)))
	      ((string= sym "DOWNFROM")
	       (if (= (length iter) 1)
		   (setq iter `(:from ,(car iter) nil 0 nil 1 1)))
	       (setf (elt iter 3) (cadr f))
	       (setf (elt iter 5) -1)
	       (setq f (cddr f)))
	      ((string= sym "UPFROM")
	       (if (= (length iter) 1)
		   (setq iter `(:from ,(car iter) nil 0 nil 1 1)))
	       (setf (elt iter 3) (cadr f))
	       (setf (elt iter 5) 1)
	       (setq f (cddr f)))
	      ((string= sym "TO")
	       (if (= (length iter) 1)
		   (setq iter `(:from ,(car iter) nil 0 nil 1 1)))
	       (setf (elt iter 4) (cadr f))
	       (setq f (cddr f)))
	      ((string= sym "DOWNTO")
	       (if (= (length iter) 1)
		   (setq iter `(:from ,(car iter) nil 0 nil 1 1)))
	       (setf (elt iter 4) (cadr f))
	       (setf (elt iter 5) -1)
	       (setq f (cddr f)))
	      ((string= sym "UPTO")
               (if (= (length iter) 1)
		   (setq iter `(:from ,(car iter) nil 0 nil 1 1)))
	       (setf (elt iter 4) (cadr f))
	       (setf (elt iter 5) 1)
	       (setq f (cddr f)))
	      ((string= sym "BELOW")
	       (if (= (length iter) 1)
		   (setq iter `(:from ,(car iter) nil 0 nil 1 1)))
	       (setf (elt iter 4) (1- (cadr f)))
	       (setq f (cddr f)))
	      ((string= sym "ABOVE")
	       (if (= (length iter) 1)
		   (setq iter `(:from ,(car iter) nil 0 nil 1 1)))
	       (setf (elt iter 4) (1+ (cadr f)))
	       (setq f (cddr f)))
	      ((string= sym "BY")
	       (if (= (length iter) 1)
		   (setq iter `(:from ,(car iter) nil 0 nil 1 1)))
	       (cond
		 ((eq (car iter) :from)
		  (setf (elt iter 6) (cadr f)))
		 ((member (car iter) '(:in :on))
		  (setf (elt iter 4) (cadr f))))
	       (setq f (cddr f)))
	      ((string= sym "IN")
	       (setq iter `(:in ,(car iter) nil ,(cadr f) #'cdr))
	       (setq f (cddr f)))
	      ((string= sym "ON")
	       (setq iter `(:on ,(car iter) nil ,(cadr f) #'cdr))
	       (setq f (cddr f)))
	      ((string= sym "=")
	       (setq iter `(:eq ,(car iter) ,(cadr f) nil))
	       (setq f (cddr f)))
	      ((string= sym "THEN")
	       (setf (elt iter 3) (cadr f))
	       (setq f (cddr f)))
	      ((string= sym "ACROSS")
	       (setq iter `(:across ,(car iter) nil nil ,(cadr f)))
	       (setq f (cddr f)))
	      ((string= sym "BEING")
	       (setq iter `(:hashtable ,(car iter) nil nil nil))
	       (setq f (cdr f))
	       (if (or (string= (string (car f)) "EACH")
		       (string= (string (car f)) "THE"))
		   (setq f (cdr f)))
	       (when (or (string= (string (car f)) "HASH-VALUE")
			 (string= (string (car f)) "HASH-VALUES"))
		 (setf (elt iter 2) (elt iter 1))
		 (setf (elt iter 1) nil)
		 (setq f (cdr f)))
	       (if (or (string= (string (car f)) "HASH-KEY")
		       (string= (string (car f)) "HASH-KEYS"))
		   (setq f (cdr f)))
	       (when (or (string= (string (car f)) "IN")
			 (string= (string (car f)) "OF"))
		 (setf (elt iter 4) (cadr f))
		 (setq f (cddr f)))
	       (when (string= (string (car f)) "USING")
		 (setf f (cdr f))
		 (if (string= (string (caar f)) "HASH-VALUE")
		     (setf (elt iter 2) (cadar f))
		     (setf (elt iter 1) (cadar f)))
		 (setq f (cdr f))))
	      (t
	       (setq forms f)
	       (return nil))))
	  (progn
	    (setq forms f)
	    (return nil))))
    (if (member (car iter) '(:from :in :on :across))
	(setf (elt iter 2) (gensym)))
    (if (eq (car iter) :across)
	(setf (elt iter 3) (gensym)))
    (when (eq (car iter) :hashtable)
      (unless (elt iter 1)
	(setf (elt iter 1) (gensym)))
      (unless (elt iter 2)
	(setf (elt iter 2) (gensym)))
      (setf (elt iter 3) (gensym)))
    (values iter forms)))


(defun loop-parse-conditional (forms loopname)
  (let (out
	docl
	elsecl
	retst
	vars
	returnvar
	(sym (string (car forms))))
    (cond
      ((or (string= sym "IF")
	   (string= sym "WHEN"))
       (setq out `(if ,(cadr forms) nil))
       (setq forms (cddr forms)))
      ((string= sym "UNLESS")
       (setq out `(if (not ,(cadr forms)) nil))
       (setq forms (cddr forms))))
    (do ((f forms))
	((not f) (setq forms f))
      (if (atom (car f))
	  (let ((sym (string (car f))))
	    (cond
	      ((or (string= sym "IF")
		   (string= sym "WHEN")
		   (string= sym "UNLESS"))
	       (multiple-value-bind (res frm vs rv)
		   (loop-parse-conditional f loopname)
		 (if (= (length out) 3)
		     (setq docl (cons res docl))
		     (setq elsecl (cons res elsecl)))
		 (setq vars (append vs vars))
		 (if rv
		     (setq returnvar rv))
		 (setq f frm)))
	      ((string= sym "ELSE")
	       (setq out (append out (list nil)))
	       (setq f (cdr f)))
	      ((or (string= sym "DO")
		   (string= sym "DOING")
		   (string= sym "AND"))
	       (setq f (cdr f)))
	      ((string= sym "RETURN")
	       (setq retst `(return-from ,loopname ,(cadr f)))
	       (if (= (length out) 3)
		   (setq docl (cons retst docl))
		   (setq elsecl (cons retst elsecl)))
	       (setq f (cddr f)))
	      ((or (string= sym "COLLECT")
		   (string= sym "COLLECTING")
		   (string= sym "APPEND")
		   (string= sym "APPENDING")
		   (string= sym "NCONC")
		   (string= sym "NCONCING"))
	       (multiple-value-bind (res var1 var2 frm) (loop-parse-accumulation f)
		 (if (= (length out) 3)
		     (setq docl (cons res docl))
		     (setq elsecl (cons res elsecl)))
		 (setq vars (cons (list (or var1 var2)) vars))
		 (if var2
		     (setq returnvar var2))
		 (setq f frm)))
	      ((string= sym "END")
	       (setq forms (cdr f))
	       (return nil))
	      (t
	       (setq forms f)
	       (return nil))))
	  (progn
	    (if (= (length out) 3)
		(setq docl (cons (car f) docl))
		(setq elsecl (cons (car f) elsecl)))
	    (setq f (cdr f)))))
    (setq docl (reverse docl))
    (setq docl (cons 'progn docl))
    (setf (elt out 2) docl)
    (when (= (length out) 4)
      (setq elsecl (reverse elsecl))
      (setq elsecl (cons 'progn elsecl))
      (setf (elt out 3) elsecl))
    (values out forms vars returnvar)))


(defun flatten-tree-skipping-nils (tree)
  (let (out)
    (if (atom tree)
	(list tree)
	(do nil
	    (nil)
	  (if (null tree)
	      (return-from flatten-tree-skipping-nils out))
	  (if (or (atom tree) (car tree))
	      (setq out (append (flatten-tree-skipping-nils (if (consp tree)
								(car tree)
								tree))
				out)))
	  (if (atom tree)
	      (return-from flatten-tree-skipping-nils out))
	  (setq tree (cdr tree))))))


(defmacro loop (&body forms)
  (let (block-name
	(body-tag (gensym))
	(prologue-tag (gensym))
	(inner-block-name (gensym))
	(skip-body-sym (gensym))
	initially-forms
	finally-forms
	do-forms
	iters
	var vars parvars
	lets l
	initial-setup iteration-setup
	out o
	returnvar)
    (do ()
	((not forms))
      (let ((form (car forms)))
	(if (symbolp form)
	    (let ((sym (string form)))
	      (cond
                ((string= sym "INITIALLY")
		 (do ((f (cdr forms) (cdr f)))
		     ((atom (car f)) (setq forms f))
		   (setq initially-forms (append initially-forms `(,(car f))))))
		((string= sym "FINALLY")
		 (do ((f (cdr forms) (cdr f)))
		     ((atom (car f)) (setq forms f))
		   (setq finally-forms (append finally-forms `(,(car f))))))
		((string= sym "WITH")
		 (when parvars
		   (setq vars (cons parvars vars))
		   (setq parvars nil))
		 (setq var `(,(cadr forms) nil))
		 (if (and (symbolp (caddr forms))
			  (string= (caddr forms) "="))
		     (progn
		       (setf (cadr var) (cadddr forms))
		       (setq forms (cddddr forms)))
		     (setq forms (cddr forms)))
		 (setq parvars (cons var parvars)))
		((string= sym "AND")
		 (setq var `(,(cadr forms) nil))
		 (if (and (symbolp (caddr forms))
			  (string= (caddr forms) "="))
		     (progn
		       (setf (cadr var) (cadddr forms))
		       (setq forms (cddddr forms)))
		     (setq forms (cddr forms)))
		 (setq parvars (cons var parvars)))
		((or (string= sym "IF")
		     (string= sym "WHEN")
		     (string= sym "UNLESS"))
		 (multiple-value-bind (res frm vs rv)
		     (loop-parse-conditional forms block-name)
		   (setq do-forms (append do-forms `(,res)))
		   (setq vars (append vs vars))
		   (if rv
		       (setq returnvar rv))
		   (setq forms frm)))
		((or (string= sym "DO")
		     (string= sym "DOING"))
		 (setq forms (cdr forms)))
		((string= sym "RETURN")
		 (setq do-forms (append do-forms `((return-from ,block-name ,(cadr forms)))))
		 (setq forms (cddr forms)))
		((string= sym "NAMED")
		 (setq block-name (cadr forms))
		 (setq forms (cddr forms)))
		((or (string= sym "FOR")
		     (string= sym "AS"))
		 (multiple-value-bind (res frm) (loop-parse-iteration forms)
		   (setq iters (append iters `(,res)))
		   (setq forms frm)))
		((or (string= sym "COLLECT")
		     (string= sym "COLLECTING")
		     (string= sym "APPEND")
		     (string= sym "APPENDING")
		     (string= sym "NCONC")
		     (string= sym "NCONCING"))
		 (multiple-value-bind (res var1 var2 frm) (loop-parse-accumulation forms)
		   (setq do-forms (append do-forms `(,res)))
		   (setq vars (cons (list (or var1 var2)) vars))
		   (if var2
		       (setq returnvar var2))
		   (setq forms frm)))
		(t
		 (setq do-forms (append do-forms `(,form)))
		 (setq forms (cdr forms)))))
	    (progn
	      (setq do-forms (append do-forms `(,form)))
	      (setq forms (cdr forms))))))
    (if parvars
	(setq vars (cons parvars vars)))
    (setq vars (reverse vars))

    (setq lets `(let (,skip-body-sym) nil))
    (setq l (cddr lets))

    (dolist (v vars)
      (setf (car l) `(let ,v nil))
      (setq l (cddar l)))

    (let (vrs)
      (dolist (i iters)
	(when (eq (car i) :in)
	  (setq vrs (append (flatten-tree-skipping-nils (elt i 1)) (cons (elt i 2) vrs)))
	  (setq initial-setup `((cl-user:al-loopy-setq ,(elt i 1) (car ,(elt i 3)))
				(when (endp ,(elt i 2))
				  (setq ,skip-body-sym t)
				  (go ,prologue-tag))
				(cl-user:al-loopy-setq ,(elt i 2) ,(elt i 3))
				. ,initial-setup))
	  (setq iteration-setup `((cl-user:al-loopy-setq ,(elt i 1) (car ,(elt i 2)))
				  (if (endp ,(elt i 2)) (return-from ,inner-block-name nil))
				  (cl-user:al-loopy-setq ,(elt i 2) (funcall ,(elt i 4) ,(elt i 2)))
				  . ,iteration-setup)))
	(when (eq (car i) :on)
	  (setq vrs (append (flatten-tree-skipping-nils (elt i 1)) (cons (elt i 2) vrs)))
	  (setq initial-setup `((when (endp ,(elt i 2))
				  (setq ,skip-body-sym t)
				  (go ,prologue-tag))
				(cl-user:al-loopy-setq ,(elt i 1) ,(elt i 3))
				(cl-user:al-loopy-setq ,(elt i 2) ,(elt i 3))
				. ,initial-setup))
	  (setq iteration-setup `((if (endp ,(elt i 2)) (return-from ,inner-block-name nil))
				  (cl-user:al-loopy-setq ,(elt i 1) ,(elt i 2))
				  (cl-user:al-loopy-setq ,(elt i 2) (funcall ,(elt i 4) ,(elt i 2)))
				  . ,iteration-setup)))
	(when (eq (car i) :from)
	  (setq vrs (cons (elt i 1) vrs))
	  (setq initial-setup `(,@(if (elt i 4)
				      (list `(when ,(if (= (elt i 5) 1)
							`(> ,(elt i 1) ,(elt i 4))
							`(< ,(elt i 1) ,(elt i 4)))
					       (setq ,skip-body-sym t)
					       (go ,prologue-tag))))
				  (setq ,(elt i 1) ,(elt i 3))
				  . ,initial-setup))
	  (setq iteration-setup `(,@(if (elt i 4)
					(list `(if ,(if (= (elt i 5) 1)
							`(> ,(elt i 1) ,(elt i 4))
							`(< ,(elt i 1) ,(elt i 4)))
						   (return-from ,inner-block-name nil))))
				    (setq ,(elt i 1) (+ ,(elt i 1) ,(* (elt i 5) (elt i 6))))
				    . ,iteration-setup)))
	(when (eq (car i) :eq)
	  (setq vrs (cons (elt i 1) vrs))
	  (setq initial-setup `((cl-user:al-loopy-setq ,(elt i 1) ,(elt i 2))
				. ,initial-setup))
	  (setq iteration-setup `((cl-user:al-loopy-setq ,(elt i 1) ,(or (elt i 3) (elt i 2)))
				  . ,iteration-setup)))
	(when (eq (car i) :across)
	  (setq vrs (list* (elt i 1) (elt i 2) (elt i 3) vrs))
	  (setq initial-setup `((setq ,(elt i 1) (aref ,(elt i 3) 0))
				(setq ,(elt i 2) 0)
				(setq ,(elt i 3) ,(elt i 4))
				. ,initial-setup))
	  (setq iteration-setup `((setq ,(elt i 1) (aref ,(elt i 3) ,(elt i 2)))
				  (if (>= ,(elt i 2) (length ,(elt i 3)))
				      (return-from ,inner-block-name nil))
				  (incf ,(elt i 2))
				  . ,iteration-setup)))
	(when (eq (car i) :hashtable)
	  (setq initial-setup `((cl-user:al-loopy-setq ,(elt i 2) (cdar ,(elt i 3)))
				(cl-user:al-loopy-setq ,(elt i 1) (caar ,(elt i 3)))
				(when (endp ,(elt i 3))
				  (setq ,skip-body-sym t)
				  (go ,prologue-tag))
				(setq ,(elt i 3)
				      (let (o) (maphash (lambda (k v) (setq o (cons (cons k v) o))) ,(elt i 4)) o))
				. ,initial-setup))
	  (setq iteration-setup `((cl-user:al-loopy-setq ,(elt i 2) (cdar ,(elt i 3)))
				  (cl-user:al-loopy-setq ,(elt i 1) (caar ,(elt i 3)))
				  (if (endp ,(elt i 3)) (return-from ,inner-block-name nil))
				  (setq ,(elt i 3) (cdr ,(elt i 3)))
				  . ,iteration-setup))))

      (when vrs
	(setf (car l) `(let ,vrs nil))
	(setq l (cddar l)))

      (let* ((out `(block ,block-name
		     nil))
	     (o (cddr out)))
	(setf (car o) lets)
	(setq o l)
	(setf (car o) `(tagbody
			  (progn ,@(reverse initial-setup))
			  ,prologue-tag
			  ,@initially-forms
			  (unless ,skip-body-sym
			    (block ,inner-block-name
			      (tagbody
				 ,body-tag
				 ,@do-forms
				 (progn ,@(reverse iteration-setup))
				 (go ,body-tag))))
			  ,@finally-forms
			  ,(if returnvar
			       `(return-from ,block-name ,returnvar))))
	out))))



(defun format (out fstr &rest args)
  (let ((*standard-output* (if (not out)
			       (make-string-output-stream)
			       (if (streamp out)
				   out
				   *standard-output*)))
	in-spec at-sign colon sign num dirargs iterbegin otherargs skip-mode
	(case-conv #'identity))
    (do ((i 0 (+ 1 i)))
	((= i (length fstr)))
      (let ((ch (elt fstr i)))
	(if in-spec
	    (cond
	      ((char= ch #\@) (setq at-sign t dirargs (cons num dirargs) num 0))
	      ((char= ch #\:) (setq colon t dirargs (cons num dirargs) num 0))
	      ((digit-char-p ch) (setq num (+ (* 10 num) (digit-char-p ch))))
	      ((char= ch #\,) (setq dirargs (cons num dirargs) num 0))
	      ((char= ch #\~) (unless skip-mode (write-char #\~)) (setq in-spec nil))
	      ((char= ch #\%) (unless skip-mode (write-char #\newline)) (setq in-spec nil))
	      ((char= ch #\&) (unless skip-mode (fresh-line)) (setq in-spec nil))
	      ((char= ch #\() (setq case-conv (lambda (s) (if (or (stringp s)
								  (symbolp s)
								  (characterp s))
							      (string-downcase (string s))
							      s))
				    in-spec nil))
	      ((char= ch #\)) (setq case-conv #'identity in-spec nil))
	      ((char= ch #\{) (setq iterbegin i in-spec nil)
	       (if at-sign
		   (setq otherargs nil)
		   (setq otherargs args args (car args))))
	      ((char= ch #\}) (if args
				  (setq i iterbegin)
				  (setq iterbegin nil args (cdr otherargs)))
	       (setq in-spec nil skip-mode nil))
	      ((char= ch #\^) (unless args (setq skip-mode t)) (setq in-spec nil))
	      ((char-equal ch #\s) (unless skip-mode (prin1 (car args))) (setq args (cdr args)) (setq in-spec nil))
	      ((char-equal ch #\a) (unless skip-mode (princ (funcall case-conv (car args)))) (setq args (cdr args)) (setq in-spec nil))
	      ((find (char-downcase ch) "doxr")
	       (setq dirargs (cons num dirargs))
	       (let ((*print-base* (cond
				     ((char-equal ch #\d) 10)
				     ((char-equal ch #\o) 8)
				     ((char-equal ch #\x) 16)
				     ((char-equal ch #\r) (car dirargs))))
		     (*print-escape* nil)
		     (*print-radix* nil)
		     (*print-readably* nil))
		 (unless skip-mode
		   (write (car args)))
		 (setq args (cdr args))
		 (setq in-spec nil)))
	      (t (setq in-spec nil)))
	    (if (char= ch #\~)
		(setq in-spec t at-sign nil colon nil sign nil num 0 dirargs nil)
		(unless skip-mode
		  (write-char ch))))))
    (if (not out)
	(get-output-stream-string *standard-output*))))



(defun encode-universal-time (sec min hour d m y &optional tz)
  (+ (* (- y 1900) 31536000)
     (* 86400 (+ (1+ (floor (- y 1900 1) 4))
		 (- (1+ (floor (- y 1901) 100)))
		 (floor (- y 1601))))
     (* 86400 (nth m '(0 0 31 59 90 120 151 181 212 243 273 304 334)))
     (* (1- d) 86400)
     (* hour 3600)
     (* min 60)
     sec))



(defmacro with-standard-io-syntax (&body forms)
  `(let ((*package* (find-package 'cl-user))
	 (*print-array* t)
	 (*print-base* 10)
	 (*print-escape* t)
	 (*print-readably* t)
	 (*read-base* 10)
	 (*read-default-float-format* 'single-float)
	 (*read-suppress* nil))
     ,@forms))



(defmacro handler-case (expr &rest clauses)
  (let ((blockname (gensym))
	(valssym (gensym))
	handlers
	noerrfunc)
    (mapcar (lambda (cl)
	      (if (eq (car cl) :no-error)
		  (setq noerrfunc `(lambda ,(cadr cl) ,@(cddr cl)))
		  (setq handlers (cons `(,(car cl)
					  (lambda ,(or (cadr cl) (list (gensym))) (return-from ,blockname ,@(cddr cl))))
				       handlers))))
	    clauses)
    `(let (,valssym)
       (block ,blockname
	 (handler-bind
	     ,(reverse handlers)
	   (setq ,valssym (multiple-value-list ,expr)))
	 ,(if noerrfunc
	      `(apply ,noerrfunc ,valssym)
	      `(values-list ,valssym))))))



(defmacro restart-case (form &rest clauses)
  (let (restarts
	tail
	(blockname (gensym))
	(argssym (gensym))
	tagsyms)
    (mapcar (lambda (cl)
	      (let ((tag (gensym)))
		(setq tagsyms (cons tag tagsyms))
		(setq restarts (cons `(,(car cl) (lambda (&rest args)
						   (setq ,argssym args)
						   (go ,tag))) restarts))
		(setq tail (list* tag
				  `(return-from ,blockname
				     (apply #'(lambda ,(cadr cl) . ,(cddr cl)) ,argssym))
				  tail))))
	    clauses)
    `(let (,argssym)
       (block ,blockname
	 (tagbody
	    (restart-bind
		,(reverse restarts)
	      (return-from ,blockname ,form))
	    ,@tail)))))



(defmacro with-simple-restart ((restname frm &rest args) &rest forms)
  `(restart-case
       (progn ,@forms)
     (,restname nil (values nil t))))



(defun find-restart (id &optional cond)
  (let ((rs (compute-restarts)))
    (or (find id rs :key #'car)
	(find id rs :key #'cdr))))



(defun abort (&optional cond)
  (invoke-restart 'abort))



(defun continue (&optional cond)
  (let ((r (find-restart 'continue)))
    (if r (invoke-restart r))))




(export '(*features* *default-pathname-defaults* machine-instance machine-type
	  machine-version short-site-name long-site-name *modules* provide
	  require describe describe-object inspect lambda-parameters-limit
	  call-arguments-limit multiple-values-limit array-rank-limit
	  array-dimension-limit array-total-size-limit char-code-limit
	  lambda-list-keywords identity constantly *read-default-float-format*
	  pi 1+ 1- minusp plusp abs zerop signum mod rem evenp oddp gcd lcm
	  isqrt conjugate cis upgraded-complex-part-type logand logandc1
	  logandc2 logeqv lognand lognor logorc1 logorc2 logxor boole-1 boole-2
	  boole-andc1 boole-andc2 boole-and boole-c1 boole-c2 boole-clr
	  boole-eqv boole-ior boole-nand boole-nor boole-orc1 boole-orc2
	  boole-set boole-xor boole *gensym-counter* gensym gentemp first second
	  third fourth fifth sixth seventh eighth ninth tenth rest caar cadr
	  cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar
	  caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar
	  cdaddr cddaar cddadr cdddar cddddr make-list copy-alist copy-tree
	  tree-equal sublis nsublis endp butlast nbutlast acons pairlis defsetf
	  when unless define-modify-macro incf decf and or cond otherwise case
	  ccase ecase typecase ctypecase etypecase return multiple-value-bind
	  prog prog* every some notany notevery member member-if member-if-not
	  find find-if find-if-not assoc assoc-if assoc-if-not rassoc rassoc-if
	  rassoc-if-not position position-if position-if-not count count-if
	  count-if-not remove remove-if-not delete delete-if delete-if-not
	  remove-duplicates delete-duplicates substitute substitute-if
	  substitute-if-not nsubstitute nsubstitute-if nsubstitute-if-not subst
	  subst-if subst-if-not nsubst nsubst-if nsubst-if-not nreverse
	  revappend nreconc adjoin fill replace push pushnew pop set-difference
	  nset-difference union nunion intersection nintersection
	  set-exclusive-or nset-exclusive-or subsetp mismatch search sort
	  stable-sort array-rank array-dimension array-total-size
	  array-in-bounds-p array-element-type upgraded-array-element-type
	  adjustable-array-p get get-properties remprop char schar bit sbit
	  svref vector-pop vector-push vector-push-extend string= string/=
	  string< string<= string> string>= string-equal string-not-equal
	  string-lessp string-not-greaterp string-greaterp string-not-lessp
	  char/= char< char<= char> char>= char-equal char-not-equal char-lessp
	  char-not-greaterp char-greaterp char-not-lessp digit-char digit-char-p
	  char-int string-upcase string-downcase string-capitalize
	  nstring-upcase nstring-downcase nstring-capitalize string-left-trim
	  string-right-trim string-trim defpackage signed-byte unsigned-byte
	  extended-char consp listp symbolp keywordp compiled-function-p
	  functionp packagep integerp rationalp floatp complexp random-state-p
	  characterp standard-char-p vectorp simple-vector-p arrayp sequencep
	  stringp simple-string-p bit-vector-p simple-bit-vector-p hash-table-p
	  pathnamep streamp realp numberp check-type assert macroexpand equal
	  fdefinition complement mapc mapcan maplist mapl mapcon map-into reduce
	  merge logical-pathname pathname-host pathname-device
	  pathname-directory pathname-type pathname-version namestring
	  file-namestring directory-namestring host-namestring enough-namestring
	  merge-pathnames file-author file-write-date user-homedir-pathname
	  with-open-file terpri write-line write-sequence prin1 princ print
	  write-to-string prin1-to-string princ-to-string do-all-symbols loop
	  format encode-universal-time with-standard-io-syntax handler-case
	  restart-case with-simple-restart find-restart abort continue))



(in-package cl-user)
