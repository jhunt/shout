#!/usr/bin/env sbcl --script
(load "build/quicklisp/setup.lisp")

;;; Enable generation of code coverage instrumentation.
(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))

;;; Reload shout-test, ensuring that it's recompiled with
;;; the new optimization policy.
(asdf:oos 'asdf:load-op :shout-test :force t)
(asdf:oos 'asdf:load-op :shout :force t)

;;; Run the test suite.
(prove:run :shout-test :reporter :list)
(sb-cover:report "coverage/")
(declaim (optimize (sb-cover:store-coverage-data 0)))
