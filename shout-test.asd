;; vim:ft=lisp
(defsystem shout-test
  :components ((:file "test/packages")
               (:file "test/rules")
               (:file "test/states"))
  :depends-on (#:shout
               #:prove))
