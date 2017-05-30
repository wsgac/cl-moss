;;;; cl-moss.asd

(asdf:defsystem #:cl-moss
  :description "Common Lisp submission mechanism for Stanford's MOSS system"
  :author "Wojciech S. Gac <wojciech.s.gac@gmail.com>"
  :license "GPLv3"
  :depends-on (#:usocket)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "cl-moss")))

