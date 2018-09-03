#!/usr/bin/env sbcl --script
(load "build/quicklisp/setup.lisp")
(asdf:load-system :shout)
(shout:shout)
