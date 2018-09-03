;; vim:ft=lisp
(defpackage :shout-asd
  (:use :cl :asdf))
(in-package :shout-asd)

(defsystem shout
  :name        "shout"
  :version     "0.1.0"
  :maintainer  "James Hunt"
  :author      "James Hunt"
  :license     "MIT"

  :description      "Shout!"
  :long-description "A Programmable Notification Gateway"

  :serial t
  :components ((:file "packages")
               (:file "version")
               (:file "io")
               (:file "shout")
               (:file "rules")
               (:file "api")
               (:file "slack"))

  :depends-on (#:hunchentoot
               #:drakma
               #:cl-json
               #:daemon))
