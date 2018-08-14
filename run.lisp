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

(if (not (env "SHOUT_IT_OUT_LOUD" nil))
  (daemon:daemonize :exit-parent t
                    :pidfile (sb-posix:getenv "PID_FILE")))
(api:run :port    (env "SHOUT_PORT"     api::*default-port*)
         :rules   (env "SHOUT_RULES"    api::*default-rules*)
         :dbfile  (env "SHOUT_DATABASE" api::*default-dbfile*))
