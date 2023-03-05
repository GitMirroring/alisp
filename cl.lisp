;;;;  Copyright (C) 2022 Andrea G. Monaco

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



;;; the following useless constants are just ridiculously high because there is no actual limit
(defconstant LAMBDA-PARAMETERS-LIMIT 4294967296)
(defconstant CALL-ARGUMENTS-LIMIT 4294967296)
(defconstant MULTIPLE-VALUES-LIMIT 4294967296)
(defconstant ARRAY-RANK-LIMIT 4294967296)
(defconstant ARRAY-DIMENSION-LIMIT 4294967296)
(defconstant ARRAY-TOTAL-SIZE-LIMIT 4294967296)


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


(defun endp (l) (null l))


(defmacro when (clause &body body)
  `(if ,clause (progn ,@body)))


(defmacro unless (clause &body body)
  `(if (not ,clause) (progn ,@body)))


(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,place ,delta)))


(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,place ,delta)))


(defmacro and (&rest forms)
  `(if ,(not forms)
       t
       (if ,(car forms)
	   (and ,@(cdr forms)))))


(defmacro or (&rest forms)
  `(if ,(not forms)
       nil
       (if ,(car forms)
	   t
	   (or ,@(cdr forms)))))


(defmacro cond (&body body)
  (let ((first (car body))
	(rest (cdr body)))
    `(if ,(car first)
	 (progn ,@(cdr first))
	 (if ',rest
	     (cond ,@rest)))))


(defmacro return (&optional val)
  `(return-from nil (values-list (multiple-value-list ,val))))


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



(defun consp (obj)
  (typep obj 'cons))

(defun listp (obj)
  (typep obj 'list))

(defun symbolp (obj)
  (typep obj 'symbol))

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

(defun pathnamep (obj)
  (typep obj 'pathname))

(defun streamp (obj)
  (typep obj 'stream))

(defun realp (obj)
  (typep obj 'real))

(defun numberp (obj)
  (typep obj 'number))



(defun equal (x y)
  (cond
    ((numberp x) (eql x y))
    ((characterp x) (eql x y))
    ((consp x) (and (equal (car x) (car y)) (equal (cdr x) (cdr y))))
    (t (eq x y))))


(defun equalp (x y)
  (cond
    ((and (numberp x) (numberp y)) (= x y))
    ((and (characterp x) (characterp y)) (char-equal x y))
    ((consp x) (and (equalp (car x) (car y)) (equalp (cdr x) (cdr y))))
    ((and (arrayp x) (arrayp y)) (and
				  (equal (array-dimensions x) (array-dimensions y))
				  (dotimes (i (array-total-size x) t)
				    (unless (equalp (row-major-aref x i) (row-major-aref y i))
				      (return-from equalp nil)))))
    (t (eq x y))))



(defun fdefinition (fname)
  (symbol-function fname))



(defun complement (func)
  (lambda (&rest args) (not (apply func args))))



(defun mapc (fun &rest lists)
  (apply #'mapcar (cons fun lists))
  (car lists))



(defun terpri ()
  (write-char #\newline)
  nil)


(defun write-line (string)
  (write-string string)
  (terpri)
  string)


(defun prin1 (obj)
  (let ((*print-escape* t))
    (write obj)))


(defun princ (obj)
  (let ((*print-escape* nil)
	(*print-readably* nil))
    (write obj)))


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
