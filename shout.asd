;; vim:ft=lisp
(defpackage :shout-asd
  (:use :cl :asdf))
(in-package :shout-asd)

(defsystem shout
  :name        "shout"
  :version     "0.0.1"
  :maintainer  "James Hunt"
  :author      "James Hunt"
  :license     "MIT"

  :description      "Shout!"
  :long-description "A configurable notifications gateway server"

  :serial t
  :components ((:file "shout"))

  :depends-on (#:hunchentoot
               #:drakma
               #:cl-json
               #:daemon))
