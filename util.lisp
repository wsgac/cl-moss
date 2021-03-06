;;;; util.lisp
;;;;
;;;; Copyright (C) 2017 Wojciech Gac <wojciech.s.gac@gmail.com>
;;;;
;;;; This file is part of CL-MOSS.
;;;;
;;;; CL-MOSS is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; CL-MOSS is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with CL-MOSS.  If not, see <http://www.gnu.org/licenses/>.
;;;;

(in-package #:cl-moss)

;;; "util" goes here. Hacks and glory await!

(defun make-keyword (x)
  (let ((keyword (load-time-value (find-package :keyword))))
    (etypecase x
      (keyword x)
      (symbol (intern (symbol-name x) keyword))
      (string (intern (string-upcase x) keyword))
      (cons    (mapcar #'make-keyword x))
      (vector  (mapcar #'make-keyword (coerce x 'list))))))
