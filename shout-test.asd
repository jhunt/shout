;; vim:ft=lisp
(defpackage :shout-test-asd
  (:use :cl :asdf))
(in-package :shout-test-asd)

(defsystem shout-test
  :components ((:file "test/rules"))
  :depends-on (#:shout
               #:prove))
