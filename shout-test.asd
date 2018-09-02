;; vim:ft=lisp
(defpackage :shout-test-asd
  (:use :cl :asdf))
(in-package :shout-test-asd)

(defsystem shout-test
  :components ((:file "test/packages")
               (:file "test/rules")
               (:file "test/states"))
  :depends-on (#:shout
               #:prove))
