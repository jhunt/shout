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
