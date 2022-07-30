;;;;  Copyright (C) 2022 Andrea G. Monaco

;;;;  This file is part of al

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


(defun 1+ (num)
  (+ num 1))

(defun 1- (num)
  (- num 1))


(defmacro when (clause &rest body)
  `(if ,clause (progn ,@body)))
