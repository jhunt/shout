#!/usr/bin/sbcl --script
(require "sb-posix")
(require "asdf")
(asdf:load-system :daemon)

(load "shout.lisp")
(format t " ######  ##     ##  #######  ##     ## ######## #### ~%")
(format t "##    ## ##     ## ##     ## ##     ##    ##    #### ~%")
(format t "##       ##     ## ##     ## ##     ##    ##    #### ~%")
(format t " ######  ######### ##     ## ##     ##    ##     ##  ~%")
(format t "      ## ##     ## ##     ## ##     ##    ##         ~%")
(format t "##    ## ##     ## ##     ## ##     ##    ##    #### ~%")
(format t " ######  ##     ##  #######   #######     ##    #### ~%")
(format t "starting up...~%")

(defun env (name default)
  (or (sb-posix:getenv name)
      default))

(daemon:daemonize :exit-parent t
                  :pidfile (sb-posix:getenv "PID_FILE"))
(api:run :port    (env "SHOUT_PORT"     api::*default-port*)
         :dbfile  (env "SHOUT_DATABASE" api::*default-dbfile*))
