(load "~/quicklisp/setup.lisp")
(ql:quickload :hunchentoot)
(ql:quickload :drakma)
(ql:quickload :cl-json)

(defpackage :slack
  (:use :common-lisp
        :drakma
        :cl-json)
  (:export :send
           :attach
           :color
           :summary))

(defpackage :api
  (:use :common-lisp
        :hunchentoot
        :cl-json)
  (:export :run
           :scan))

(in-package :slack)

(defvar *default-name* "shout!bot")
(defvar *default-icon* "http://cl.ly/image/3e1h0H3H2s0P/concourse-logo.png")

(defun env (name default)
  (or (sb-unix::posix-getenv name) default))

(defun color (state)
  (if (or (equal state "fixed")
          (equal state "working"))
    "good"
    "danger"))

(defun summary (msg link)
  (let ((m (or msg "(no message provided)")))
    (if (null link)
      (format nil "~A" m)
      (format nil "~A <~A>" m link))))


(defun attach (text &key title color)
  (remove-if #'null
    (list
      (if (null title) nil `(title . ,title))
      (if (null color) nil `(color . ,color))
      `(text . ,text))))

(defun send (text &key (icon     (env "SHOUT_BOTICON" *default-icon*))
                       (username (env "SHOUT_BOTNAME" *default-name*))
                       (webhook  (env "SHOUT_WEBHOOK" ""))
                       (attachments nil))
  (if (equal webhook "")
    (error "no webhook supplied to slack:send!"))
  (drakma:http-request webhook
                       :method :post
                       :content (json:encode-json-to-string
                                  `((text . ,text)
                                    (username . ,username)
                                    (icon_url . ,icon)
                                    (attachments . ,attachments)))))

(in-package :api)

(defvar *default-port* 7109)
(defvar *default-expiry* 86400)

;; the base offset of UNIX Epoch time into LISP Universal Time
(defvar *EPOCH* (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-now ()
  (- (get-universal-time) *EPOCH*))

(defvar *states* '())

(defclass event ()
  ((message
    :initarg :message
    :accessor event-message)
   (link
    :initarg :link
    :accessor event-link)
   (ok
    :initarg :ok
    :accessor event-ok?)
   (occurred-at
    :initarg :occurred-at
    :initform (unix-now)
    :accessor event-occurred-at)
   (reported-at
    :initarg :reported-at
    :initform (unix-now)
    :accessor event-reported-at)))

(defclass state ()
  ((topic
    :initarg :name
    :accessor topic)
   (status
    :initarg :status
    :accessor status-of)
   (notified-at
    :initform nil
    :accessor last-notified-at)
   (previous-event
    :initarg :previous-event
    :initform nil
    :accessor previous-event)
   (first-event
    :initarg :first-event
    :initform nil
    :accessor first-event)
   (last-event
    :initarg :last-event
    :initform nil
    :accessor last-event)))

(defun state-is-ok? (st)
  (and st
       (last-event st)
       (event-ok? (last-event st))))

(defun notify-about-state (state event mode edge)
  (slack:send
    (format nil "~A is ~A ~A!" (topic state) mode edge)
    :attachments (list
                   (slack:attach
                     (slack:summary
                       (event-message event)
                       (event-link    event))
                     :color (slack:color edge)))))

(defun trigger-edge (state event type)
  (notify-about-state state event "now" type))

(defun transition-state (e1 e2)
  (cond ((not (event-ok? e2))
         "broken")
        ((and (not (event-ok? e1))
              (event-ok? e2))
         "fixed")
        ((and (event-ok? e2))
         "working")))

(defun ingest-event (state event)
  (let ((edge (transition-state
                (last-event state)
                event)))
    (if (not (eq (event-ok? (last-event state))
                 (event-ok? event)))
      (progn
        (trigger-edge state event edge)
        (setf (previous-event state)
              (last-event     state))))

    (setf (status-of state) edge
          (last-event state) event)
    state))

(defun find-state (topic)
  (cdr (assoc topic *states* :test #'equal)))

(defun add-state (topic event)
  (let ((state (make-instance 'state :name topic
                                     :status (if (event-ok? event) "working" "broken"))))
    (setf (last-notified-at state) (unix-now)
          (previous-event   state) nil
          (first-event      state) event
          (last-event       state) event
          *states* (acons topic state *states*))
    state))

(defun set-state (topic event)
  (let ((state (find-state topic)))
    (if state
      (ingest-event state event)
      (add-state topic event))))

(defmacro handle (url &body body)
  (let ((fn (gensym "fn")))
    `(progn
       (defun ,fn ()
         ,@body)
       (push (create-prefix-dispatcher ,url ',fn) *dispatch-table*))))

(defmacro handle-json (url &body body)
  `(handle ,url
           (setf (content-type* *reply*) "application/json")
           (format nil "~A~%" (json:encode-json-to-string
                                (progn ,@body)))))

(defun json-body ()
  (decode-json-from-string
    (raw-post-data :force-text t)))

(defun jref (o field)
  (cdr (assoc field o)))

(defun event-json (e)
  (if (null e)
    nil
    `((occurred_at . ,(event-occurred-at e))
      (reported_at . ,(event-reported-at e))
      (ok          . ,(event-ok? e))
      (message     . ,(event-message e))
      (link        . ,(event-link e)))))

(defun state-json (st)
  `((name     . ,(topic st))
    (state    . ,(status-of st))
    (previous . ,(event-json (previous-event st)))
    (first    . ,(event-json (first-event st)))
    (last     . ,(event-json (last-event st)))))

(defun event-from-json (json)
  (when json
    (make-instance 'event
                   :message     (jref json :message)
                   :ok          (jref json :ok)
                   :link        (jref json :link)
                   :occurred-at (jref json :occurred-at)
                   :reported-at (jref json :reported-at))))

(defun state-from-json (json)
  (make-instance 'state
                 :name   (jref json :name)
                 :status (jref json :status)
                 :previous-event (event-from-json (jref json :previous))
                 :first-event    (event-from-json (jref json :first))
                 :last-event     (event-from-json (jref json :last))))

(defun read-database (path)
  (let ((raw (with-open-file (in path :direction :input :if-does-not-exist nil)
               (when in
                 (json:decode-json-from-string
                   (format nil "~{~A~^, ~}"
                     (loop for line = (read-line in nil)
                           while line
                           collect line))))))
        (db '()))
    (loop for state in raw do
          (setf db (acons (jref state :name)
                          (state-from-json state)
                          db)))
    db))

(defun write-database (path db)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "~A~%" (json:encode-json-to-string
                         (mapcar #'(lambda (pair)
                                     (state-json (cdr pair))) db)))))

(defun run-api (&key (port 7109))
  ;; GET /state?topic=blah
  (handle-json "/state"
               (find-state (parameter "topic")))

   ; GET /states
  (handle-json "/states"
               (mapcar #'(lambda (a)
                           (state-json (cdr a))) *states*))
  ;; POST /events
  (handle-json "/events"
               (if (eq (request-method* *request*) :post)
                 (let ((b (json-body)))
                   (state-json
                     (set-state
                       (jref b :topic)
                       (make-instance 'event
                         :message     (jref b :message)
                         :link        (jref b :link)
                         :ok          (jref b :ok)
                         :occurred-at (jref b :ocurred-at)))))
                 `((oops . "not a POST")
                   (got . ,(request-method *request*)))))

  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun scan (expiry)
  (write-database "states.db" *states*)
  (let ((deadline (- (unix-now) expiry)))
    (labels ((f (lst)
                (if (not (null lst))
                  (let ((state (cdar lst)))
                    (if (and (not (state-is-ok? state))
                             (last-notified-at state)
                             (> deadline (last-notified-at state)))
                      (progn
                        (notify-about-state
                          state
                          (last-event state)
                          "still"
                          (status-of state))
                        (setf (last-notified-at state) (unix-now))))
                    (f (cdr lst))))))
      (f *states*))))

(defun run (&key (port *default-port*)
                 (expiry *default-expiry*))
  (setf *states* (read-database "test.db"))
  (run-api :port port)
  (loop
    (scan expiry)
    (sleep 60)))
