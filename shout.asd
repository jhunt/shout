;; vim:ft=lisp
(defsystem "shout"
  :name        "shout"
  :version     "0.0.1"
  :maintainer  "James Hunt"
  :author      "James Hunt"
  :license     "MIT"

  :description      "Shout!"
  :long-description "A configurable notifications gateway server"

  :components ((:file "packages")
               (:file "version" :depends-on ("packages"))
               (:file "shout" :depends-on ("packages"))
               (:file "rules" :depends-on ("packages"))
               (:file "api" :depends-on ("packages"))
               (:file "slack" :depends-on ("packages")))

  :depends-on (#:hunchentoot
               #:drakma
               #:cl-json
               #:daemon))
