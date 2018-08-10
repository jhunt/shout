(load "~/quicklisp/setup.lisp")
(ql:quickload :hunchentoot)
(ql:quickload :drakma)
(ql:quickload :cl-json)

(defpackage :slack
  (:use :common-lisp
        :drakma
        :cl-json)
  (:export :send
           :attach))

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
  (message
    link
    ok
    occurred
    reported))

(defclass state ()
  (name
    state
    notified
    previous-event
    first-event
    last-event))

(defun new-event (message link ok occurred)
  (let ((e (make-instance 'event))
        (now (unix-now)))
    (setf (slot-value e 'message) message
          (slot-value e 'link) link
          (slot-value e 'ok) ok
          (slot-value e 'occurred) (or occurred now)
          (slot-value e 'reported) now)
    e))

(defun event-ok? (e)
  (slot-value e 'ok))

(defun slack-color (edge)
  (if (or (equal edge "fixed")
          (equal edge "working"))
    "good"
    "danger"))

(defun slack-summary (msg link)
  (let ((m (or msg "(no message provided)")))
    (if (null link)
      (format nil "~A" m)
      (format nil "~A <~A>" m link))))

(defun notify-about-state (state event mode edge)
  (let ((topic (slot-value state 'name)))
    (slack:send
      (format nil "~A is ~A ~A!" topic mode edge)
      :attachments (list
                     (slack:attach
                       (slack-summary
                         (slot-value event 'message)
                         (slot-value event 'link))
                        :color (slack-color edge))))))

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
                (slot-value state 'last-event)
                event)))
    (if (not (eq (event-ok? (slot-value state 'last-event))
                 (event-ok? event)))
      (progn
        (trigger-edge state event edge)
        (setf (slot-value state 'previous-event)
              (slot-value state 'last-event))))

    (setf (slot-value state 'state) edge
          (slot-value state 'last-event) event)
    state))

(defun find-state (topic)
  (cdr (assoc topic *states* :test #'equal)))

(defun add-state (topic event)
  (let ((state (make-instance 'state)))
    (setf (slot-value state 'name) topic
          (slot-value state 'notified) (unix-now)
          (slot-value state 'previous-event) nil
          (slot-value state 'first-event) event
          (slot-value state 'last-event) event
          (slot-value state 'state) (if (event-ok? event) "working" "broken")

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
    `((occurred-at . ,(slot-value e 'occurred))
      (reported-at . ,(slot-value e 'reported))
      (ok . ,(event-ok? e))
      (message . ,(slot-value e 'message))
      (link . ,(slot-value e 'link)))))

(defun state-json (st)
  `((name . ,(slot-value st 'name))
    (state . ,(slot-value st 'state))
    (previous . ,(event-json (slot-value st 'previous-event)))
    (first . ,(event-json (slot-value st 'first-event)))
    (last . ,(event-json (slot-value st 'last-event)))))

(defun run-api (&key (port 7109))
  ;; GET /state?topic=blah
  (handle-json "/state" (find-state (parameter "topic")))
  ;; GET /states
  (handle-json "/states" *states*)
  ;; POST /events
  (handle-json "/events"
               (if (eq (request-method* *request*) :post)
                 (let ((b (json-body)))
                   (state-json
                     (set-state
                       (jref b :topic)
                       (new-event (jref b :message)
                                  (jref b :link)
                                  (jref b :ok)
                                  (jref b :occurred-at)))))
                 `((oops . "not a POST")
                   (got . ,(request-method *request*)))))

  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun scan (expiry)
  (let ((deadline (- (unix-now) expiry)))
    (labels ((f (lst)
                (if (not (null lst))
                  (let ((state (cdar lst)))
                    (if (and (not (event-ok? (slot-value state 'last-event)))
                             (> deadline (slot-value state 'notified)))
                      (progn
                        (notify-about-state
                          state
                          (slot-value state 'last-event)
                          "still"
                          (slot-value state 'state))
                        (setf (slot-value state 'notified) (unix-now))))
                    (f (cdr lst))))))
      (f *states*))))

(defun run (&key (port *default-port*)
                 (expiry *default-expiry*))
  (sb-thread:make-thread
    (lambda ()
      (loop
        (scan expiry)
        (sleep 60))))
  (run-api :port port))
