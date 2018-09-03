(in-package :api)

(defvar *default-port* 7109)
(defvar *default-dbfile* #p"/var/shout.db")
(defvar *default-rules* #p"/var/shout.rules")
(defvar *default-expiry* 86400)
(defvar *default-auth* '("shout" . "shout"))
(defvar *debug-prefix* nil)

;; the base offset of UNIX Epoch time into LISP Universal Time
(defvar *EPOCH* (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-now ()
  (- (get-universal-time) *EPOCH*))

(defvar *states* '())
(defvar *rules* '())
(defvar *rules-src* "")

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
   (metadata
    :initarg :metadata
    :initform ()
    :accessor event-metadata)
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
   (last-notified-at
    :initform nil
    :accessor last-notified-at)
   (remind-every
    :initform nil
    :accessor remind-every)
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

(defun state-needs-reminder? (st)
  (and (not (state-is-ok? st))
       (remind-every st)
       (< (+ (last-notified-at st) (remind-every st))
          (unix-now))))

(defun notify-about-state (state event mode edge)
  (infof "attempting notification for ~A (~A ~A) with message ~S"
         (topic state) mode edge (event-message event))
  (let ((result (rules:evaluate *rules*
                  (pairlis
                    '(:topic :ok? :status :last-notified :message :link)
                    (list
                      (topic state)
                      (event-ok? event)
                      (format nil "~A ~A" mode edge)
                      (last-notified-at state)
                      (event-message event)
                      (if (equal (event-link event) "")
                        nil
                        (event-link event))))
                  (event-metadata event))))
    (setf (remind-every state)
          (if (and result (eq (car result) :remind))
            (cdr result)))))

(defun notify-announcement (topic event)
  (debugf "attempting notification for ~A (announcement) with message ~S"
          topic (event-message event))
  (rules:evaluate *rules*
    (pairlis
      '(:announcement? :topic :ok? :status :message :link)
      (list t topic t "worth looking into..."
            (event-message event)
            (if (equal (event-link event) "")
                nil
                (event-link event))))
    (event-metadata event)))

(defun trigger-edge (state event type)
  (notify-about-state state event "now" type))

(defun transition-state (e1 e2)
  (cond ((not (event-ok? e2))
         "broken")
        ((and (not (event-ok? e1))
              (event-ok? e2))
         "fixed")
        (t "working")))

(defun ingest-event (state event)
  (let ((edge (transition-state
                (last-event state)
                event)))
    (when (not (eq (event-ok? (last-event state))
                   (event-ok? event)))
      (trigger-edge state event edge)
      (setf (previous-event state)
            (last-event     state)))

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

(defun random-string (&optional (length 32) (alphabet "0123456789abcdefghjkmnpqrtvwxyz"))
  (let ((rs (make-random-state t)))
    (loop with s = (make-string length)
          with len = (length alphabet)
          for i below length
          do (setf (aref s i)
                   (aref alphabet (random len rs)))
          finally (return s))))

(defun debug-endpoint (url)
  (when (null *debug-prefix*)
    (setf *debug-prefix* (random-string 12)))
  (format nil "/~A~A" *debug-prefix* url))

(defmacro with-auth (auth &body body)
  (let ((got-user (gensym))
        (got-pass (gensym)))
  `(multiple-value-bind (,got-user ,got-pass)
    (hunchentoot:authorization)
    (cond ((or (null ,got-user) (null ,got-pass))
           (hunchentoot:require-authorization))
          ((or (not (equal ,got-user (car ,auth)))
               (not (equal ,got-pass (cdr ,auth))))
           (setf (return-code *reply*) 403)
           (hunchentoot:abort-request-handler))
          (t ,@body)))))

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
      (metadata    . ,(event-metadata e))
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
                   :metadata    (jref json :metadata)
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
                   (format nil "~{~A~}"
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

(defun run-api (&key (port 7109) (ops-auth *default-auth*) (admin-auth *default-auth*))
  ;; GET /info
  (handle-json "/info"
               `((version . ,*release-version*)
                 (release . ,*release-name*)))

  ;; GET /state?topic=blah
  (handle-json (debug-endpoint "/state")
               (with-auth ops-auth
                 (find-state (parameter "topic"))))

   ; GET /states
  (handle-json (debug-endpoint "/states")
               (with-auth ops-auth
                 (mapcar #'(lambda (a)
                    (state-json (cdr a))) *states*)))

  ;; POST /announce
  (handle-json "/announcements"
               (with-auth ops-auth
                 (if (eq (request-method* *request*) :post)
                   (let ((b (json-body)))
                     (notify-announcement
                       (jref b :topic)
                       (make-instance 'event
                         :message     (jref b :message)
                         :link        (jref b :link)
                         :ok          (jref b :ok)
                         :metadata    (jref b :metadata)
                         :occurred-at (jref b :occurred-at)))
                     `((ok . "Success!")))
                   `((oops . "not a POST")
                     (got . ,(request-method *request*))))))

  ;; POST /events
  (handle-json "/events"
               (with-auth ops-auth
                 (if (eq (request-method* *request*) :post)
                   (let ((b (json-body)))
                     (set-state
                       (jref b :topic)
                       (make-instance 'event
                         :message     (jref b :message)
                         :link        (jref b :link)
                         :ok          (jref b :ok)
                         :metadata    (jref b :metadata)
                         :occurred-at (jref b :occurred-at)))
                     `((ok . "Success!")))
                   `((oops . "not a POST")
                     (got . ,(request-method *request*))))))

  ;; GET/POST /rules
  (handle "/rules"
          (with-auth admin-auth
            (case (request-method* *request*)
              (:post
                (let ((rules-src (raw-post-data :force-text t)))
                  (setf *rules* (rules:parse rules-src))
                  (setf *rules-src* rules-src)))
              (:get  *rules-src*)
              (otherwise
                (setf (return-code *reply*) 400)
                (hunchentoot:abort-request-handler)))))

  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun scan (dbfile)
  (infof "writing database to file ~A" dbfile)
  (write-database dbfile *states*)

  (infof "scanning for reminders that need to be sent...")
  (loop for pair in *states*
        do (let ((state (cdr pair)))
             (when (state-needs-reminder? state)
               (notify-about-state
                 state (last-event state) "still" (status-of state))))))

(defun run (&key (port *default-port*)
                 (dbfile *default-dbfile*)
                 (expiry *default-expiry*)
                 (ops-auth *default-auth*)
                 (admin-auth *default-auth*))

  (if (stringp port)
      (setf port (parse-integer port)))

  (format t "reading database from file ~A~%" dbfile)
  (setf *states* (read-database dbfile))

  (format t "registering notification plugins...~%")
  (labels ((arg (args name)
             (nth (+ 1 (position name args)) args)))
    (rules:register-plugin
      'rules::slack
      #'(lambda (args)
          (slack:send
            (arg args :text)
            :webhook (arg args :webhook)
            :attachments (list
                           (slack:attach
                             (arg args :attach)
                             :color (arg args :color)))))))

  (format t "binding *:~A~%" port)
  (run-api :port port :ops-auth ops-auth :admin-auth admin-auth)

  (format t "debugging endpoints accessible under /~A/...~%" *debug-prefix*)
  (format t "entering upkeep thread main loop...~%")
  (loop
    (scan dbfile)
    (sleep 60)))
