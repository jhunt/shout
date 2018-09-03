(in-package :cl-user)

(defpackage :slack
  (:use :cl
        :drakma
        :cl-json)
  (:export :send
           :attach))

(defpackage :io
  (:use :cl)
  (:export :infof
           :debugf))

(defpackage :shout
  (:use :cl :io)
  (:export :shout))

(defpackage :rules
  (:use :cl :io)
  (:export :register-plugin
           :evaluate
           :parse))

(defpackage :api
  (:use :cl :io
        :hunchentoot
        :cl-json)
  (:export :run
           :scan))
