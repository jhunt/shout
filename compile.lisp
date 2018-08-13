#!/usr/bin/sbcl --script
(require "sb-posix")
(require "asdf")
(asdf:load-system :daemon)

(load "shout.lisp")

(defun env (name default)
  (or (sb-posix:getenv name)
      default))

(sb-ext:save-lisp-and-die
  "shout"
  :compression t
  :executable  t
  :toplevel (lambda ()
              (format t " ######  ##     ##  #######  ##     ## ######## #### ~%")
              (format t "##    ## ##     ## ##     ## ##     ##    ##    #### ~%")
              (format t "##       ##     ## ##     ## ##     ##    ##    #### ~%")
              (format t " ######  ######### ##     ## ##     ##    ##     ##  ~%")
              (format t "      ## ##     ## ##     ## ##     ##    ##         ~%")
              (format t "##    ## ##     ## ##     ## ##     ##    ##    #### ~%")
              (format t " ######  ##     ##  #######   #######     ##    #### ~%")
              (format t "starting up...~%")
              (daemon:daemonize :exit-parent t
                                :pidfile (sb-posix:getenv "PID_FILE"))
              (api:run :port    (env "SHOUT_PORT"     api::*default-port*)
                       :dbfile  (env "SHOUT_DATABASE" api::*default-dbfile*))))
