(in-package :shout)

(defvar *default-daemon-mode*   t)
(defvar *default-pidfile*       #p"/var/run/shout.pid")
(defvar *default-port*          "7109")
(defvar *default-log-level*     "none")
(defvar *default-database-file* #p"/var/db/shout.db")
(defvar *default-credentials*   "shout:shout")

(defun env (name &optional default)
  (or (sb-posix:getenv name)
      default))

(defun parse-creds (creds)
  (let ((idx (position #\: creds)))
    (if (null idx)
      (error "Invalid authentication string")
      (cons (subseq creds 0 idx)
            (subseq creds (+ idx 1))))))

(defun shout (&key (daemonize         (env "SHOUT_IT_OUT_LOUD" *default-daemon-mode*))
                   (pidfile           (env "SHOUT_PIDFILE"     *default-pidfile*))
                   (port              (env "SHOUT_PORT"        *default-port*))
                   (database-file     (env "SHOUT_DATABASE"    *default-database-file*))
                   (log-level         (env "SHOUT_LOG_LEVEL"   *default-log-level*))
                   (ops-credentials   (env "SHOUT_OPS_CREDS"   (env "SHOUT_CREDS" *default-credentials*)))
                   (admin-credentials (env "SHOUT_ADMIN_CREDS" (env "SHOUT_CREDS" *default-credentials*))))

  (if (eq daemonize t)
    (daemon:daemonize :exit-parent t
                      :pidfile pidfile)
    (progn
      (format t " ######  ##     ##  #######  ##     ## ######## #### ~%")
      (format t "##    ## ##     ## ##     ## ##     ##    ##    #### ~%")
      (format t "##       ##     ## ##     ## ##     ##    ##    #### ~%")
      (format t " ######  ######### ##     ## ##     ##    ##     ##  ~%")
      (format t "      ## ##     ## ##     ## ##     ##    ##         ~%")
      (format t "##    ## ##     ## ##     ## ##     ##    ##    #### ~%")
      (format t " ######  ##     ##  #######   #######     ##    #### ~%")
      (format t "starting up...~%")))

  (setf *log-level*
        (case log-level
          (("info"  "INFO")  :info)
          (("debug" "DEBUG") :debug)
          (otherwise nil)))

  (api:run :port       port
           :dbfile     database-file
           :ops-auth   (parse-creds ops-credentials)
           :admin-auth (parse-creds admin-credentials)))
