#!/usr/bin/env sbcl --script
(load "build/quicklisp/setup.lisp")
(require "prove")
(prove:run :shout-test :reporter :list)
