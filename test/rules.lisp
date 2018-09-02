(in-package :shout-test)

(defvar *no-params*   '())
(defvar *no-metadata* '())

(defvar *args* '())
(rules::register-plugin
  'made-it
  #'(lambda (args)
      (setf *args* args)))

(defun triggers (what &optional msg)
  (is *args* (list what)
      (or msg
          (format nil "Should have triggered '~A'" what))))

(defun parse (form)
  (cond ((symbolp form) (symbol-name form))
        ((atom form) (format nil "~S" form))
        (t (format nil "(~{~A ~})" (mapcar #'parse form)))))

(defun try (rules &key params metadata)
  (rules:eval/rules
    (rules:load/rules (parse rules))
    (or params *no-params*)
    (or metadata *no-metadata*)))

(plan nil)
(subtest "Plugin Registration"
  (ok (rules::registered-plugin? 'made-it)
      "'made-it (calling package) should be registered")
  (ok (rules::registered-plugin? :made-it)
      ":made-it (KEYWORD package) should be registered")
  (ok (not (rules::registered-plugin? 'not-a-thing))
      "'not-a-thing shoult not be registered"))

(subtest "Evaluation"
  (try `((for *
           (when *
             (made-it "here")))))
  (triggers "here" "FOR * / WHEN * always fires")


  (try `((for *
           (when *
             (made-it "no further than here"))
           (when (on weekdays)
             (made-it "to the weekday!"))
           (when (on weekends)
             (made-it "to the weekday!")))))
  (triggers "no further than here"
      "Only the first matching WHEN should fire")

  (try `((for "topic"
           (when * (made-it "to the topic")))
             (for "off-topic"
           (when * (made-it "wrong"))))
       :params (pairlis '(:topic) '("topic")))
  (triggers "to the topic"
      "FOR should match topic expicitly")

  (try `((for (or (is "topic")
                  (is "something else"))
           (when * (made-it "to the topic")))
         (for "off-topic"
           (when * (made-it "wrong"))))
       :params (pairlis '(:topic) '("topic")))
  (triggers "to the topic"
      "FOR should match topic in an expression tree")

  (try `((for *
           (when ((metadata? super-important))
             (made-it "shouldn't make it..."))
           (when ((metadata? escalate))
             (made-it (metadata escalate)))
           (when * (made-it "normal"))))
       :metadata (pairlis '(:escalate) '("escalated")))
  (triggers "escalated" "Metadata evaluates properly")

  (try `((set msg "a message")
         (for *
           (when *
             (made-it (value msg))))))
  (triggers "a message" "Variables should work")

  (try `((set hooks (map "default"  "the default"
                         "override" "something"))
         (for *
           (when *
             (made-it (lookup hooks "override" "default"))))))
  (triggers "something" "Map lookups with variable keys should work")

  (try `((set hooks (map "this" "to that"))
         (for *
           (when ((lookup hooks "x" "y" "z"))
             (made-it "to the wrong place"))
           (when *
             (made-it (lookup hooks "this"))))))
  (triggers "to that" "Map lookups work in conditionals and value positions")

  (let ((ruleset
          `((for *
              (when ((or a b))
                (made-it "a or b was true"))
              (when *
                (made-it "neither were true"))))))
    (try ruleset :params (pairlis '(:a :b) '(t t)))
    (triggers "a or b was true" "(OR t t) is true")

    (try ruleset :params (pairlis '(:a :b) '(t nil)))
    (triggers "a or b was true" "(OR t nil) is true")

    (try ruleset :params (pairlis '(:a :b) '(nil t)))
    (triggers "a or b was true" "(OR nil t) is true")

    (try ruleset :params (pairlis '(:a :b) '(nil nil)))
    (triggers "neither were true" "(OR nil nil) is false"))

  (let ((ruleset
          `((for *
              (when ((not a))
                (made-it "(NOT a) was true"))
              (when *
                (made-it "(NOT a) was false"))))))
    (try ruleset :params (pairlis '(:a) '(t)))
    (triggers "(NOT a) was false")

    (try ruleset :params (pairlis '(:a) '(nil)))
    (triggers "(NOT a) was true"))

  (let ((ruleset
          `((for *
              (when *
                (made-it (if test "consequence" "alternate")))))))
    (try ruleset :params (pairlis '(:test) '(t)))
    (triggers "consequence"
              "(IF t ...) should trigger the consequent")
    (try ruleset :params (pairlis '(:test) '(nil)))
    (triggers "alternate"
              "(IF nil ...) should trigger the alternate"))

  (let ((ruleset
          `((for *
              (when *
                (made-it (if test "consequence")))))))
    (try ruleset :params (pairlis '(:test) '(t)))
    (triggers "consequence"
              "(IF t ...) should trigger the consequent")
    (try ruleset :params (pairlis '(:test) '(nil)))
    (triggers nil "A nil alternate for (IF ...) is ok"))

  (try `((set var "variable")
         (for *
           (when *
             (made-it (concat "this is a" (value var)))))))
  (triggers
    (format nil "this is a~%variable~%")
    "Concat combines strings mapped with trailing newlines")

  (try `((for *
           (when *
             (made-it message))))
       :params (pairlis '(:message) '("a message")))
  (triggers "a message"
    "Symbols evaluate to the named parameter")

  (try `((for *
           (when *
             (made-it nil)))))
  (triggers nil "NIL evaluates to nil")

  (try `((for *
           (when *
             (made-it "this is a $message"))))
       :params (pairlis '(:message) '("parameter")))
  (triggers "this is a parameter"
    "String interpolation of parameters works")

  (try `((for *
           (when *
             (made-it "this is $[meta]"))))
       :metadata (pairlis '(:meta) '("metadata")))
  (triggers "this is metadata"
    "String interpolation of metadata works"))

(subtest "Time-based predicates"
  (let ((rules::*NOW* (encode-universal-time 0 14 2 29 8 1997 4)))
    ; It's August 29th, 2:14am Eastern (a Friday)
    ; SkyNet has become self-aware.
    ; How do we notify about that?

    (try `((for *
             (when ((on weekends))
               (made-it "not yet..."))
             (when ((on weekdays))
               (made-it "skynet has become self-aware"))
             (when * (made-it "wrong")))))
    (triggers "skynet has become self-aware")

    (try `((for *
             (when ((on friday))
               (made-it "skynet has become self-aware"))
             (when * (made-it "wrong")))))
    (triggers "skynet has become self-aware")

    (try `((for *
             (when ((after 0800 am))
               (made-it "too late"))
             (when * (made-it "correct")))))
    (triggers "correct")

    (try `((for *
             (when ((before 0800 am))
               (made-it "on time"))
             (when * (made-it "wrong")))))
    (triggers "on time")

    (try `((for *
             (when ((after 0100 pm))
               (made-it "too late"))
             (when * (made-it "on time")))))
    (triggers "on time")

    (try `((for *
             (when ((from 0100 am to 0300 am))
               (made-it "on time"))
             (when * (made-it "wrong")))))
    (triggers "on time")

    (ok t))
  (ok t))

(subtest "Syntax errors"
  (defun is-syntax-error (form &key params metadata)
    (is-error (try form :params params :metadata metadata)
              'simple-error))

  (is-syntax-error `((set 42)))
  (is-syntax-error `((set var)))
  (is-syntax-error `((foo bar)))
  (is-syntax-error `((for * (foo bar))))
  (is-syntax-error `((for * (when * bar))))
  (is-syntax-error `((for * (when * (foo bar)))))
  (is-syntax-error `((for * (when ((foo bar)) (made-it "fail")))))
  (is-syntax-error `((for * (when ((after 0800 bc)) (made-it "fail")))))
  (ok t))

(finalize)
