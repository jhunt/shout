(in-package :shout)

(defvar *default-daemon-mode*   t)
(defvar *default-pidfile*       #p"/var/run/shout.pid")
(defvar *default-port*          "7109")
(defvar *default-database-file* #p"/var/db/shout.db")
(defvar *default-credentials*   "shout:shout")

(defun env (name &optional default)
  (or (sb-posix:getenv name)
      default))

(defun bail (msg code)
  (format t "[ERROR] ~A~%" msg)
  (sb-ext::exit :code 1))

(defun get-auth (var)
  (let ((auth (env var nil)))
    (if (null auth)
      *default-credentials*
      (let ((idx (position #\: auth)))
        (if (null idx)
          (bail (format nil "Invalid authentication string in ~A: expecting \"username:password\"" var) 1)
          (cons (subseq auth 0 idx) (subseq auth (+ idx 1))))))))

(defun shout (&key (daemonize         (env "SHOUT_IT_OUT_LOUD" *default-daemon-mode*))
                   (pidfile           (env "SHOUT_PIDFILE"     *default-pidfile*))
                   (port              (env "SHOUT_PORT"        *default-port*))
                   (database-file     (env "SHOUT_DATABASE"    *default-database-file*))
                   (ops-credentials   (env "SHOUT_OPS_CREDS"   (env "SHOUT_CREDS" *default-credentials*)))
                   (admin-credentials (env "SHOUT_ADMIN_CREDS" (env "SHOUT_CREDS" *default-credentials*))))

  (if daemonize
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

  (api:run :port       port
           :dbfile     database-file
           :ops-auth   ops-credentials
           :admin-auth admin-credentials))
