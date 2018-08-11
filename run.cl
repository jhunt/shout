#!/usr/bin/sbcl --script
(require "asdf")
(asdf:load-system :daemon)

(load "shout.cl")
(format t " ######  ##     ##  #######  ##     ## ######## #### ~%")
(format t "##    ## ##     ## ##     ## ##     ##    ##    #### ~%")
(format t "##       ##     ## ##     ## ##     ##    ##    #### ~%")
(format t " ######  ######### ##     ## ##     ##    ##     ##  ~%")
(format t "      ## ##     ## ##     ## ##     ##    ##         ~%")
(format t "##    ## ##     ## ##     ## ##     ##    ##    #### ~%")
(format t " ######  ##     ##  #######   #######     ##    #### ~%")
(format t "starting up...~%")

(daemon:daemonize :exit-parent t
                  :pidfile (or (sb-unix::posix-getenv "PID_FILE")
                               "/var/run/shout.pid"))
(api:run)
