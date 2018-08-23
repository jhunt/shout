#!/usr/bin/env sbcl --script
(load "build/quicklisp/setup.lisp")
(require "sb-posix")
(require "asdf")
(asdf:load-system :shout)

;;(load "shout.lisp")
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

(defun bail (msg code)
  (format t "[ERROR] ~A~%" msg)
  (sb-ext::exit :code 1))

(defun get-auth (env-var)
  (let ((auth (env env-var nil)))
    (if (null auth)
      api::*default-auth*
      (let ((idx (position #\: auth)))
        (if (null idx)
          (bail (format nil "Invalid authentication string in ~A: expecting \"username:password\"" env-var) 1)
          (cons (subseq auth 0 idx) (subseq auth (+ idx 1))))))))

(if (not (env "SHOUT_IT_OUT_LOUD" nil))
  (daemon:daemonize :exit-parent t
                    :pidfile (sb-posix:getenv "PID_FILE")))
(api:run :port    (env "SHOUT_PORT"     api::*default-port*)
         :dbfile  (env "SHOUT_DATABASE" api::*default-dbfile*)
         :ops-auth (get-auth "SHOUT_OPS_AUTH")
         :admin-auth (get-auth "SHOUT_ADMIN_AUTH"))
