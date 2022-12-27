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



(defun array-dimension (array axis)
  (nth axis (array-dimensions array)))


(defun char (str ind)
  (aref str ind))



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


(defun fdefinition (fname)
  (symbol-function fname))



(defun mapc (fun &rest lists)
  (apply #'mapcar (cons fun lists))
  (car lists))
