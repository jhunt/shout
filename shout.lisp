(defpackage :slack
  (:use :common-lisp
        :drakma
        :cl-json)
  (:export :send
           :attach))

(defpackage :rules
  (:use :common-lisp)
  (:export :register-plugin
           :eval/rules
           :load/rules))

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

(in-package :rules)

(defvar *NOW* nil)
(defun now ()
  (or *NOW* (get-universal-time)))


(defmacro aif (test &body body)
  `(let ((it ,test))
      (if it ,@body)))

(defun weekday (&optional n)
  (nth (or n (nth-value 6 (decode-universal-time (now))))
       '(monday tuesday wednesday thursday friday
         saturday sunday)))

(defun time-of-day ()
  (multiple-value-bind
        (s m h)
        (decode-universal-time (now))
    (+ (* h 3600)
       (* m 60)
       s)))

(defun hhmm-seconds (hhmm meridian)
  (+ (* 60 (+ (mod hhmm 100)
              (* 60 (floor (/ hhmm 100)))))
     (cond ((eq meridian 'am) 0)
           ((eq meridian 'pm) 43200)
           (t (error "invalid meridian ~A" meridian)))))

(defun assocify (lst)
  (if lst
      (acons (car lst)
             (cadr lst)
             (assocify (cddr lst)))))

(defvar *environment* '())
(defvar *metadata* '())
(defvar *parameters* '())
(defvar *plugin-handlers* '())

(defun register-plugin (plugin fn)
  (setf *plugin-handlers*
        (acons (sym->key plugin) fn *plugin-handlers*)))

(defun registered-plugin? (plugin)
  (not (null (assoc (sym->key plugin) *plugin-handlers*))))

(defun dispatch-to-plugin (plugin args)
  (let ((fn (cdr (assoc (sym->key plugin) *plugin-handlers*))))
    (if fn
        (funcall fn args)
        (error "no such plugin ~A" plugin))))

(defun param (name &optional default)
  (aif (assoc name *parameters*)
    (cdr it)
    default))

(defun replace-all (subj old new)
  (with-output-to-string (out)
    (loop with part-length = (length old)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search old subj :start2 old-pos)
          do (write-string subj out
                           :start old-pos
                           :end (or pos (length subj)))
          when pos do (write-string new out)
          while pos)))

(defun interpolate (s)
  (loop for n in '(:topic :status :message :link) do
        (setf s (replace-all s (format nil "$~(~A~)" (symbol-name n)) (param n))))
  (loop for pair in *metadata* do
        (setf s (replace-all s (format nil "$[~(~A~)]" (symbol-name (car pair))) (cdr pair))))
  s)

; pre-declaration -- will be re-defined shortly.
(eval-when (:execute)
  (defun -eval/args (args)
    (declare (ignore args))))

(defun sym->key (sym)
  (find-symbol (symbol-name sym)
               (find-package "KEYWORD")))

(defun -eval/expr (expr)
  (cond ((atom expr)
         (cond ((symbolp expr)
                (param (sym->key expr)))
               ((stringp expr)
                (interpolate expr))
               (t expr)))

        ((eq (car expr) 'not)
         (not (-eval/expr (cadr expr))))

        ((eq (car expr) 'and)
         (loop for sub in (cdr expr) do
               (if (not (-eval/expr sub))
                   (return-from -eval/expr nil)))
         t)

        ((eq (car expr) 'or)
         (loop for sub in (cdr expr) do
               (if (-eval/expr sub)
                   (return-from -eval/expr t)))
         nil)

        ((eq (car expr) 'map)
           (assocify (cdr expr)))

        ((eq (car expr) 'concat)
         (format nil "~{~a~%~}"
                 (remove-if #'null (-eval/args (cdr expr)))))

        ((eq (car expr) 'value)
         (cdr (assoc (cadr expr) *environment*)))

        ((eq (car expr) 'lookup)
         (let ((map (cdr (assoc (cadr expr) *environment*))))
           (loop for var in (cddr expr) do
                 (aif (assoc var map :test #'equal)
                   (return-from -eval/expr (cdr it))))
           nil))

        ((eq (car expr) 'metadata?)
         (not (null (assoc (sym->key (cadr expr)) *metadata*))))

        ((eq (car expr) 'metadata)
         (let ((pair (assoc (sym->key (cadr expr)) *metadata*)))
           (if pair (cdr pair) "")))

        ((eq (car expr) 'if)
           (cond ((-eval/expr (cadr expr))
                  (-eval/expr (caddr expr)))
                 ((cadddr expr)
                  (-eval/expr (cadddr expr)))
                 (t nil)))

        ((eq (car expr) 'matches)
           ;; FIXME : implement cl-ppcre and regex
           nil)

        ((eq (car expr) 'is)
           (equal (param :topic)
                  (-eval/expr (cadr expr))))

        ((eq (car expr) 'on)
         (cond ((equal (cdr expr) '(weekdays))
                (find (weekday) '(monday
                                  tuesday
                                  wednesday
                                  thursday
                                  friday)))
               ((equal (cdr expr) '(weekends))
                (find (weekday) '(sunday
                                  saturday)))
               (t (find (weekday) (cdr expr)))))

        ((eq (car expr) 'from)
         (let ((start (nthcdr 1 expr))
               (stop  (nthcdr 4 expr)))
           (and (>= (time-of-day)
                    (hhmm-seconds (car start) (cadr start)))
                (<= (time-of-day)
                    (hhmm-seconds (car stop) (cadr stop))))))

        ((eq (car expr) 'after)
         (> (time-of-day)
            (hhmm-seconds (cadr expr) (caddr expr))))
        ((eq (car expr) 'before)
         (< (time-of-day)
            (hhmm-seconds (cadr expr) (caddr expr))))

        (t (error "expression syntax error ~A" (car expr)))))

(defun -eval/args (args)
  (loop for arg in args collect
        (if (keywordp arg)
            arg
            (-eval/expr arg))))

(defun -eval/body (body)
  ;; handle SLACK, EMAIL, and other handlers
  ;; properly evlis'ing the arguments
  (loop for call in body do
        (cond ((atom call)
               (error "invalid WHEN body (~A is not a list)" call))
              ((not (registered-plugin? (car call)))
               (error "no plugin ~A registered..." (car call)))
              (t (dispatch-to-plugin (car call)
                                     (-eval/args (cdr call))))))
  t)

(defun -eval/when (clause)
  (if (or (eq (car clause) '*)
          (-eval/expr `(and ,@(car clause))))
      (-eval/body (cdr clause))))

(defun -eval/for (form)
  (if (or (null form)
          (and (atom form)
               (not (eq form '*))))
      (error "invalid form for FOR"))
  (when (or (eq (car form) '*)
            (-eval/expr (if (atom (car form))
                        `(is ,(car form))
                        (car form))))
    (loop for clause in (cdr form) do
          (if (not (eq (car clause) 'when))
              (error "unexpected ~A clause in FOR (should have been a WHEN)" (car clause)))
          (when (-eval/when (cdr clause))
                (return-from -eval/for t))))
  nil)

(defun -eval/set (form)
  (if (not (eq (length form) 2))
      (error "bad SET form"))
  (setf *environment* (acons (car form)
                             (-eval/expr (cadr form))
                             *environment*)))

(defun eval/rules (rules params metadata)
  (let ((*environment* '())
        (*parameters* params)
        (*metadata* metadata))
    (loop for rule in rules do
          (case (car rule)
            (for (-eval/for (cdr rule)))
            (set (-eval/set (cdr rule)))
            (otherwise (error "unexpected top-level form (~A ...)" (car rule)))))
    'all-done))

(defun load/rules (str)
  (let ((*package* (find-package "RULES")))
    (read-from-string str)))

(in-package :api)

(defvar *default-port* 7109)
(defvar *default-dbfile* #p"/var/shout.db")
(defvar *default-rules* #p"/var/shout.rules")
(defvar *default-expiry* 86400)
(defvar *default-auth* '("shout" . "shout"))

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
  (rules:eval/rules *rules*
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
    (event-metadata event)))

(defun notify-announcement (topic event)
  (rules:eval/rules *rules*
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
        ((and (event-ok? e2))
         "working")))

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
  ;; GET /state?topic=blah
  (handle-json "/state"
               (with-auth ops-auth
                 (find-state (parameter "topic"))))

   ; GET /states
  (handle-json "/states"
               (with-auth ops-auth
                 (mapcar #'(lambda (a)
                    (state-json (cdr a))) *states*)))
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
                  (setf *rules* (rules:load/rules rules-src))
                  (setf *rules-src* rules-src)))
              (:get  *rules-src*)
              (otherwise
                (setf (return-code *reply*) 400)
                (hunchentoot:abort-request-handler)))))

  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun scan (expiry dbfile)
  (write-database dbfile *states*)
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

  (format t "entering upkeep thread main loop...~%")
  (loop
    (scan expiry dbfile)
    (sleep 60)))
