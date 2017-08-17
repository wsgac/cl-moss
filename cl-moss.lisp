;;;; cl-moss.lisp
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

;;; "cl-moss" goes here. Hacks and glory await!

(defparameter *allowed-languages*
  '(:c :cc :java :ml :pascal :ada :lisp :scheme :haskell
    :fortran :ascii :vhdl :perl :matlab :python :mips :prolog
    :spice :vb :csharp :modula2 :a8086 :javascript :plsql
    :verilog))

(defparameter *server-url* "moss.stanford.edu")

(defparameter *server-port* 7690)

(defparameter *default-options*
  '(("m" . 10)
    ("d" . 0)
    ("n" . 250)
    ("x" . 0)
    ("c" . "")
    ("l" . "c")))

(defclass moss ()
  ((options :initarg :options
            :initform *default-options*
            :accessor moss-options)
   (server :initarg :server
           :initform *server-url*
           :accessor moss-server)
   (port :initarg :port
         :initform *server-port*
         :accessor moss-port)
   (user-id :initarg :user-id
            :initform (error "User ID is required")
            :accessor moss-user-id)
   (files :initarg :files
          :initform nil
          :accessor moss-files)
   (base-files :initarg :base-files
               :initform nil
               :accessor moss-base-files)))

(defun moss-initialize (user-id &key server port)
  (apply 'make-instance 'moss
         `(:user-id ,user-id
                    ,@(when server `(:server ,server))
                    ,@(when port `(:port ,port)))))

(defun get-option-value (option-alist option)
  (cdr (assoc option option-alist :test #'equalp)))

(defmethod set-language ((moss moss) language)
  (let ((lang (make-keyword language)))
    (if (member lang *allowed-languages*)
        (rplacd (assoc "l" (moss-options moss) :test #'equalp)
                (string-downcase lang))
        (error "Language [~(~a~)] is not allowed." lang))))

(defmethod set-directory-mode ((moss moss) bool)
  (rplacd (assoc "d" (moss-options moss) :test #'equalp)
          (if bool 1 0)))

(defmethod set-ignore-limit ((moss moss) limit)
  (if (and (integerp limit) (> limit 1))
      (rplacd (assoc "m" (moss-options moss) :test #'equalp) limit)
      (error "Ignore limit has to be an integer greater than 1. [~a] given."
             limit)))

(defmethod set-comment-string ((moss moss) comment)
  (rplacd (assoc "c" (moss-options moss) :test #'equalp)
          (princ-to-string comment)))

(defmethod set-result-limit ((moss moss) limit)
  (if (and (integerp limit) (> limit 1))
      (rplacd (assoc "n" (moss-options moss) :test #'equalp) limit)
      (error "Result limit has to be an integer greater than 1. [~a] given."
             limit)))

(defmethod set-experimental-server ((moss moss) bool)
  (rplacd (assoc "x" (moss-options moss) :test #'equalp)
          (if bool 1 0)))

(defmethod add-base-file ((moss moss) file)
  (if (probe-file file)
      (pushnew file (moss-base-files moss) :test #'equalp)
      (error "Cannot find file [~a]" file)))

(defmethod add-file ((moss moss) file)
  (if (probe-file file)
      (pushnew file (moss-files moss) :test #'equalp)
      (error "Cannot find file [~a]" file)))

(defmethod add-by-wildcard ((moss moss) pattern)
  (loop
     with files = (directory pattern)
     for file in files
     do (add-file moss file)
     finally (return files)))

(defun upload-file (socket file lang id)
  (with-open-file (in file :direction :input)
    (let ((file-size (file-length in))
          (file-name (substitute #\_ #\- (namestring file)))
          (socket-stream (usocket::socket-stream socket)))
      (format socket-stream "file ~a ~a ~d ~a~%"
              id lang file-size file-name)
      (uiop::copy-stream-to-stream in socket-stream))))

(defmethod send ((moss moss))
  (with-slots (server port options files base-files user-id) moss
    (let* ((socket (usocket::socket-connect server port))
           (socket-stream (usocket::socket-stream socket))
           (language (get-option-value options "l")))
      #+cl-moss-debug
      (warn "Socket successfully opened on port ~d"
            (usocket::get-local-port socket))
      (format socket-stream "moss ~a~%~
                             directory ~a~%~
                             X ~a~%~
                             maxmatches ~a~%~
                             show ~a~%~
                             language ~a~%"
              user-id
              (get-option-value options "d")
              (get-option-value options "x")
              (get-option-value options "m")
              (get-option-value options "n")
              language)
      (force-output socket-stream)
      #+cl-moss-debug
      (warn "Preamble sent to MOSS server")
      ;; Language check
      (let ((in-line (read-line socket-stream)))
        (when (string-equal "no"
                            (string-trim '(#\space) in-line))
          (format socket-stream "end~%")
          (usocket::socket-close socket)
          (error "Unsupported language: [~(~a~)]" language)))
      #+cl-moss-debug
      (warn "Language check successful. Language: [~(~a~)]" language)
      (loop
         for bfile in base-files
         do (upload-file socket bfile language 0))
      #+cl-moss-debug
      (warn "Base files uploaded successfully: ~{~a~^, ~}" base-files)
      (loop
         for i from 1
         for file in files
         do (upload-file socket file language i))
      #+cl-moss-debug
      (warn "Files uploaded successfully: ~{~a~^, ~}" files)
      (format socket-stream "query 0 ~d~%" (get-option-value options "c"))
      (force-output socket-stream)
      #+cl-moss-debug
      (warn "Query successfully sent")
      (let ((response (read-line socket-stream)))
        (format socket-stream "end~%")
        (usocket::socket-close socket)
        response))))
