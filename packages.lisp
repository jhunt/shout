(in-package :cl-user)

(defpackage :slack
  (:use :cl
        :drakma
        :cl-json)
  (:export :send
           :attach))

(defpackage :shout
  (:use :cl)
  (:export :shout))

(defpackage :rules
  (:use :cl)
  (:export :register-plugin
           :evaluate
           :parse))

(defpackage :api
  (:use :cl
        :hunchentoot
        :cl-json)
  (:export :run
           :scan))
