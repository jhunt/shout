#!/usr/bin/env sbcl --script
;; shortcut for installing all the deps
(load "~/quicklisp/setup.lisp")
(ql:quickload :hunchentoot)
(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :daemon)
