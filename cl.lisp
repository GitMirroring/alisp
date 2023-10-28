;;;;  Copyright (C) 2022-2023 Andrea G. Monaco

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



(defun machine-instance nil)
(defun machine-type nil)
(defun machine-version nil)

(defun short-site-name nil)
(defun long-site-name nil)



(defun describe (obj &optional str) (values))
(defun describe-object (obj str))



;;; the following useless constants are just ridiculously high because lambda
;;; lists and multiple values use chained lists, so there's no hard limit

(defconstant LAMBDA-PARAMETERS-LIMIT most-positive-fixnum)
(defconstant CALL-ARGUMENTS-LIMIT most-positive-fixnum)
(defconstant MULTIPLE-VALUES-LIMIT most-positive-fixnum)



(defconstant ARRAY-RANK-LIMIT most-positive-fixnum)
(defconstant ARRAY-DIMENSION-LIMIT most-positive-fixnum)
(defconstant ARRAY-TOTAL-SIZE-LIMIT most-positive-fixnum)


(defconstant CHAR-CODE-LIMIT most-positive-fixnum)



(defconstant lambda-list-keywords '(&optional &rest &body &key &allow-other-keys))



(defun identity (x) x)


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

(defun isqrt (num)
  (values (floor (sqrt num))))

(defun conjugate (num)
  (complex (realpart num) (- (imagpart num))))

(defun cis (rad)
  (complex (cos rad) (sin rad)))


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



(defparameter *gensym-counter* 1)

(defun gensym (&optional x)
  (let ((str (make-string-output-stream)))
    (cond
      ((stringp x) (write-string x str)
       (write *gensym-counter* :stream str)
       (incf *gensym-counter*))
      ((integerp x) (write-string "G" str)
       (write x :stream str))
      ((null x) (write-string "G" str)
       (write *gensym-counter* :stream str)
       (incf *gensym-counter*)))
    (make-symbol (get-output-stream-string str))))


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
(defun cdar (list) (cdr (car (list))))
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


(defun make-list (len &key initial-element)
  (let (out)
    (dotimes (i len)
      (setq out (cons initial-element out)))
    out))


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



(defmacro when (clause &body body)
  `(if ,clause (progn ,@body)))


(defmacro unless (clause &body body)
  `(if (not ,clause) (progn ,@body)))


(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,place ,delta)))


(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,place ,delta)))


(defmacro and (&rest forms)
  `(let ((val (multiple-value-list ,(car forms))))
     (if ,(not forms)
         t
         (if (car val)
             (if ,(not (cdr forms))
                 (values-list val)
                 (and ,@(cdr forms)))))))


(defmacro or (&rest forms)
  `(let ((val (multiple-value-list ,(car forms))))
     (if ,(not forms)
       nil
       (if (car val)
	   (if ,(not (cdr forms))
               (values-list val)
	       (car val))
           (or ,@(cdr forms))))))


(defmacro cond (&body body)
  (let ((first (car body))
	(rest (cdr body)))
    `(if ,(car first)
	 (progn ,@(cdr first))
	 (if ',rest
	     (cond ,@rest)))))


(defmacro case (keyf &rest clauses)
  (let ((keysym (gensym))
	outsym)
    `(let ((,keysym ,keyf))
       ,(dolist (clause clauses (setq outsym (cons 'cond outsym)))
	  (cond ((or (eq (car clause) t)
		     (eq (car clause) 'otherwise))
		 (setq outsym (append outsym `((t (progn ,@(cdr clause)))))))
		((listp (car clause))
		 (setq outsym (append outsym `(((member ,keysym ',(car clause))
						(progn ,@(cdr clause)))))))
		(t
		 (setq outsym (append outsym `(((eql ,keysym ',(car clause))
						(progn ,@(cdr clause))))))))))))


(defmacro return (&optional val)
  `(return-from nil (values-list (multiple-value-list ,val))))


(defmacro multiple-value-bind (vars valform &body forms)
  (let ((restsym (gensym)))
    `(multiple-value-call (lambda (&optional ,@vars &rest ,restsym)
			    ,@forms)
       ,valform)))


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


(defun member (obj list)
  (let ((subl list))
    (dotimes (i (length list))
      (if (eql obj (car subl))
	  (return-from member subl))
      (setq subl (cdr subl)))))


(defun member-if (pred list)
  (let ((subl list))
    (dotimes (i (length list))
      (if (funcall pred (car subl))
	  (return-from member-if subl))
      (setq subl (cdr subl)))))


(defun member-if-not (pred list)
  (member-if (complement pred) list))


(defun find (obj seq)
  (find-if (lambda (x) (eql obj x)) seq))


(defun find-if (pred seq)
  (dotimes (i (length seq) nil)
    (if (funcall pred (elt seq i))
	(return-from find-if (elt seq i)))))


(defun find-if-not (pred seq)
  (find-if (complement pred) seq))


(defun assoc (obj alist)
  (assoc-if (lambda (x) (eql obj x)) alist))


(defun assoc-if (pred alist)
  (let ((subl alist))
    (dotimes (i (length alist))
      (if (funcall pred (car (car subl)))
	  (return-from assoc-if (car subl)))
      (setq subl (cdr subl)))))


(defun assoc-if-not (pred alist)
  (assoc-if (complement pred) alist))


(defun position (obj seq)
  (position-if (lambda (x) (eql obj x)) seq))


(defun position-if (pred seq)
  (dotimes (i (length seq) nil)
    (if (funcall pred (elt seq i))
	(return-from position-if i))))


(defun position-if-not (pred seq)
  (position-if (complement pred) seq))


(defun count (obj seq)
  (let ((ret 0))
    (dotimes (i (length seq))
      (if (eql (elt seq i) obj)
	  (setq ret (+ 1 ret))))
    ret))


(defun count-if (pred seq)
  (let ((ret 0))
    (dotimes (i (length seq))
      (if (funcall pred (elt seq i))
	  (setq ret (+ 1 ret))))
    ret))


(defun count-if-not (pred seq)
  (count-if (complement pred) seq))


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


(defun nreverse (seq)
  (reverse seq))


(defun adjoin (obj list)
  (if (member obj list) list (cons obj list)))


(defun fill (seq obj &key (start 0) end)
  (dotimes (i (if end (- end start) (length seq)))
    (setf (elt seq (+ i start)) obj))
  seq)


(defmacro push (item place)
  `(setf ,place (cons ,item ,place)))


(defmacro pop (place)
  `(let ((c (car ,place)))
     (setf ,place (cdr ,place))
     c))


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


(defun char (str ind)
  (aref str ind))


(defun string/= (str1 str2)
  (not (string= str1 str2)))


(defun char-equal (&rest chars)
  (apply #'char= (mapcar #'char-upcase chars)))


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

(defun vectorp (obj)
  (typep obj 'vector))

(defun arrayp (obj)
  (typep obj 'array))

(defun sequencep (obj)
  (typep obj 'sequence))

(defun stringp (obj)
  (typep obj 'string))

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
    ((and (consp x) (consp y))
     (and
      (let (l)
	(= (setq l (length x)) (length y))
	(dotimes (i l)
	  (if (equal (car x) (car y))
	      (setq x (cdr x) y (cdr y))
	      (return-from equal nil)))
	(equal x y))))
    (t (eq x y))))


(defun equalp (x y)
  (cond
    ((and (numberp x) (numberp y)) (= x y))
    ((and (characterp x) (characterp y)) (char-equal x y))
    ((and (consp x) (consp y))
     (and
      (let (l)
	(= (setq l (length x)) (length y))
	(dotimes (i l)
	  (if (equalp (car x) (car y))
	      (setq x (cdr x) y (cdr y))
	      (return-from equalp nil)))
	(equalp x y))))
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



(defun fdefinition (fname)
  (symbol-function fname))



(defun complement (func)
  (lambda (&rest args) (not (apply func args))))



(defun mapc (fun &rest lists)
  (apply #'mapcar (cons fun lists))
  (car lists))



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


(defmacro loop (&body forms)
  (let (block-name
	(tagname (gensym))
	(inner-block-name (gensym))
	initially-forms
	finally-forms
	do-forms
	vars var)
    (do ()
	((not forms))
      (let ((form (car forms)))
	(if (atom form)
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
		((or (string= sym "DO")
		     (string= sym "DOING"))
		 (do ((f (cdr forms) (cdr f)))
		     ((atom (car f)) (setq forms f))
		   (setq do-forms (append do-forms `(,(car f))))))
		((string= sym "NAMED")
		 (setq block-name (cadr forms))
		 (setq forms (cddr forms)))
		((or (string= sym "FOR")
		     (string= sym "AS"))
		 (setq var `(,(cadr forms) 0 nil 1 1))
		 (setq vars (append vars `(,var)))
		 (setq forms (cddr forms)))
		((string= sym "FROM")
		 (setf (elt var 1) (cadr forms))
		 (setq forms (cddr forms)))
		((or (string= sym "DOWNFROM"))
		 (setf (elt var 1) (cadr forms))
		 (setf (elt var 3) -1)
		 (setq forms (cddr forms)))
		((or (string= sym "UPFROM"))
		 (setf (elt var 1) (cadr forms))
		 (setf (elt var 3) 1)
		 (setq forms (cddr forms)))
		((or (string= sym "TO"))
		 (setf (elt var 2) (cadr forms))
		 (setq forms (cddr forms)))
		((or (string= sym "DOWNTO"))
		 (setf (elt var 2) (cadr forms))
		 (setf (elt var 3) -1)
		 (setq forms (cddr forms)))
		((or (string= sym "UPTO"))
		 (setf (elt var 2) (cadr forms))
		 (setf (elt var 3) 1)
		 (setq forms (cddr forms)))
		((or (string= sym "BELOW"))
		 (setf (elt var 2) (1- (cadr forms)))
		 (setq forms (cddr forms)))
		((or (string= sym "ABOVE"))
		 (setf (elt var 2) (1+ (cadr forms)))
		 (setq forms (cddr forms)))
		((or (string= sym "BY"))
		 (setf (elt var 4) (cadr forms))
		 (setq forms (cddr forms)))))
	    (progn
	      (setq do-forms (append do-forms `(,form)))
	      (setq forms (cdr forms))))))
    `(block ,block-name
       (let ,(mapcar (lambda (x) `(,(car x) ,(cadr x))) vars)
	 ,@initially-forms
	 (block ,inner-block-name
	   (tagbody
	      ,tagname
	      (if (or ,@(mapcar (lambda (x) (if (elt x 2)
						(if (= (elt x 3) 1)
						    `(> ,(elt x 0) ,(elt x 2))
						    `(< ,(elt x 0) ,(elt x 2)))
						nil)) vars))
		  (return-from ,inner-block-name nil))
	      ,@do-forms
	      (progn
		,@(mapcar (lambda (x) `(setq ,(elt x 0) (+ ,(elt x 0) ,(* (elt x 3) (elt x 4))))) vars))
	      (go ,tagname)))
	 ,@finally-forms
	 nil))))


(defun format (out fstr &rest args)
  (let (in-spec at-sign colon sign num dirargs)
    (dotimes (i (length fstr))
      (let ((ch (elt fstr i)))
	(if in-spec
	    (cond
	      ((char= ch #\@) (setq at-sign t))
	      ((char= ch #\:) (setq colon t))
	      ((char= ch #\,) (setq dirargs (cons num dirargs) num nil))
	      ((char= ch #\~) (write-char #\~) (setq in-spec nil))
	      ((char= ch #\%) (write-char #\newline) (setq in-spec nil))
	      ((char= ch #\&) (fresh-line) (setq in-spec nil))
	      ((char-equal ch #\s) (prin1 (car args)) (setq args (cdr args)) (setq in-spec nil))
	      ((char-equal ch #\a) (princ (car args)) (setq args (cdr args)) (setq in-spec nil)))
	    (if (char= ch #\~)
		(setq in-spec t at-sign nil colon nil sign nil num nil dirargs nil)
		(write-char ch)))))))



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




(export '(*features* machine-instance machine-type machine-version
	  short-site-name long-site-name describe describe-object
	  lambda-parameters-limit call-arguments-limit multiple-values-limit
	  array-rank-limit array-dimension-limit array-total-size-limit
	  char-code-limit lambda-list-keywords identity pi 1+ 1- minusp plusp
	  abs zerop signum mod rem evenp oddp isqrt conjugate cis logand
	  logandc1 logandc2 logeqv lognand lognor logorc1 logorc2 logxor boole-1
	  boole-2 boole-andc1 boole-andc2 boole-and boole-c1 boole-c2 boole-clr
	  boole-eqv boole-ior boole-nand boole-nor boole-orc1 boole-orc2
	  boole-set boole-xor boole *gensym-counter* gensym gentemp first second
	  third fourth fifth sixth seventh eighth ninth tenth rest caar cadr
	  cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar
	  caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar
	  cdaddr cddaar cddadr cdddar cddddr make-list endp butlast nbutlast
	  acons pairlis when unless incf decf and or cond otherwise case return
	  multiple-value-bind every some notany notevery member member-if
	  member-if-not find find-if find-if-not assoc assoc-if assoc-if-not
	  position position-if position-if-not count count-if count-if-not
	  remove remove-if-not delete delete-if delete-if-not nreverse adjoin
	  fill push pop array-rank array-dimension array-total-size
	  array-in-bounds-p get get-properties char string/= char-equal
	  digit-char digit-char-p char-int string-upcase string-downcase
	  string-capitalize nstring-upcase nstring-downcase nstring-capitalize
	  string-left-trim string-right-trim string-trim defpackage consp listp
	  symbolp keywordp functionp packagep integerp rationalp floatp complexp
	  random-state-p characterp vectorp arrayp sequencep stringp
	  hash-table-p pathnamep streamp realp numberp macroexpand equal equalp
	  fdefinition complement mapc terpri write-line write-sequence prin1
	  princ print loop format encode-universal-time))



(in-package cl-user)
