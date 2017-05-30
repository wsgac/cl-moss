;;;; util.lisp

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
