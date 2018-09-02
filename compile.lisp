#!/usr/bin/sbcl --script
(load "build/quicklisp/setup.lisp")
(asdf:load-system :shout)
(sb-ext:save-lisp-and-die
  "shout"
  :compression t
  :executable  t
  :toplevel #'shout:shout)
