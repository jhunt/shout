(defpackage :shout-test
  (:use :cl :prove))
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

(plan nil)
(subtest "Plugin Registration"
  (ok (rules::registered-plugin? 'made-it)
      "'made-it (calling package) should be registered")
  (ok (rules::registered-plugin? :made-it)
      ":made-it (KEYWORD package) should be registered")
  (ok (not (rules::registered-plugin? 'not-a-thing))
      "'not-a-thing shoult not be registered"))

(subtest "Evaluation"
  (rules:eval/rules
    (rules:load/rules
      (parse `((for *
                  (when *
                    (made-it "here"))))))
    *no-params*
    *no-metadata*)
  (triggers "here" "FOR * / WHEN * always fires")


  (rules:eval/rules
    (rules:load/rules
      (parse `((for *
                  (when *
                    (made-it "no further than here"))
                  (when (on weekdays)
                    (made-it "to the weekday!"))
                  (when (on weekends)
                    (made-it "to the weekday!"))))))
    *no-params*
    *no-metadata*)
  (triggers "no further than here"
      "Only the first matching WHEN should fire")

  (rules:eval/rules
    (rules:load/rules
      (parse `((for "topic"
                  (when * (made-it "to the topic")))
               (for "off-topic"
                  (when * (made-it "wrong"))))))
    (pairlis '(:topic) '("topic"))
    *no-metadata*)
  (triggers "to the topic"
      "FOR should match topic expicitly")

  (rules:eval/rules
    (rules:load/rules
      (parse `((for (or (is "topic")
                        (is "something else"))
                  (when * (made-it "to the topic")))
               (for "off-topic"
                  (when * (made-it "wrong"))))))
    (pairlis '(:topic) '("topic"))
    *no-metadata*)
  (triggers "to the topic"
      "FOR should match topic in an expression tree")

  (rules:eval/rules
    (rules:load/rules
      (parse `((for *
                 (when ((metadata? super-important))
                   (made-it "shouldn't make it..."))
                 (when ((metadata? escalate))
                   (made-it (metadata escalate)))
                 (when * (made-it "normal"))))))
    *no-params*
    (pairlis '(:escalate) '("escalated")))
  (triggers "escalated" "Metadata evaluates properly")

  (rules:eval/rules
    (rules:load/rules
      (parse `((set msg "a message")
               (for *
                  (when *
                    (made-it (value msg)))))))
    *no-params*
    *no-metadata*)
  (triggers "a message" "Variables should work")

  (rules:eval/rules
    (rules:load/rules
      (parse `((set hooks (map "default"  "the default"
                               "override" "something"))
               (for *
                  (when *
                    (made-it (lookup hooks "override" "default")))))))
    *no-params*
    *no-metadata*)
  (triggers "something" "Map lookups with variable keys should work")

  (rules:eval/rules
    (rules:load/rules
      (parse `((set hooks (map "this" "to that"))
               (for *
                  (when ((lookup hooks "x" "y" "z"))
                    (made-it "to the wrong place"))
                  (when *
                    (made-it (lookup hooks "this")))))))
    *no-params*
    *no-metadata*)
  (triggers "to that" "Map lookups work in conditionals and value positions")

  (let ((ruleset
          (rules:load/rules
            (parse
              `((for *
                  (when ((or a b))
                    (made-it "a or b was true"))
                  (when *
                    (made-it "neither were true"))))))))
    (rules:eval/rules
      ruleset
      (pairlis '(:a :b) '(t t))
      *no-metadata*)
    (triggers "a or b was true" "(OR t t) is true")

    (rules:eval/rules
      ruleset
      (pairlis '(:a :b) '(t nil))
      *no-metadata*)
    (triggers "a or b was true" "(OR t nil) is true")

    (rules:eval/rules
      ruleset
      (pairlis '(:a :b) '(nil t))
      *no-metadata*)
    (triggers "a or b was true" "(OR nil t) is true")

    (rules:eval/rules
      ruleset
      (pairlis '(:a :b) '(nil nil))
      *no-metadata*)
    (triggers "neither were true" "(OR nil nil) is false"))

  (let ((ruleset
          (rules:load/rules
            (parse
              `((for *
                  (when ((not a))
                    (made-it "(NOT a) was true"))
                  (when *
                    (made-it "(NOT a) was false"))))))))
    (rules:eval/rules
      ruleset
      (pairlis '(:a) '(t))
      *no-metadata*)
    (triggers "(NOT a) was false")

    (rules:eval/rules
      ruleset
      (pairlis '(:a) '(nil))
      *no-metadata*)
    (triggers "(NOT a) was true"))

  (let ((ruleset
          (rules:load/rules
            (parse
              `((for *
                  (when *
                    (made-it (if test "consequence" "alternate")))))))))
    (rules:eval/rules
      ruleset
      (pairlis '(:test) '(t))
      *no-metadata*)
    (triggers "consequence"
              "(IF t ...) should trigger the consequent")
    (rules:eval/rules
      ruleset
      (pairlis '(:test) '(nil))
      *no-metadata*)
    (triggers "alternate"
              "(IF nil ...) should trigger the alternate"))

  (let ((ruleset
          (rules:load/rules
            (parse
              `((for *
                  (when *
                    (made-it (if test "consequence")))))))))
    (rules:eval/rules
      ruleset
      (pairlis '(:test) '(t))
      *no-metadata*)
    (triggers "consequence"
              "(IF t ...) should trigger the consequent")
    (rules:eval/rules
      ruleset
      (pairlis '(:test) '(nil))
      *no-metadata*)
    (triggers nil "A nil alternate for (IF ...) is ok"))

  (rules:eval/rules
    (rules:load/rules
      (parse `((set var "variable")
               (for *
                  (when *
                    (made-it (concat "this is a" (value var))))))))
    *no-params*
    *no-metadata*)
  (triggers
    (format nil "this is a~%variable~%")
    "Concat combines strings mapped with trailing newlines")

  (rules:eval/rules
    (rules:load/rules
      (parse `((for *
                  (when *
                    (made-it message))))))
    (pairlis '(:message) '("a message"))
    *no-metadata*)
  (triggers "a message"
    "Symbols evaluate to the named parameter")

  (rules:eval/rules
    (rules:load/rules
      (parse `((for *
                  (when *
                    (made-it nil))))))
    *no-params*
    *no-metadata*)
  (triggers nil "NIL evaluates to nil")

  (rules:eval/rules
    (rules:load/rules
      (parse `((for *
                  (when *
                    (made-it "this is a $message"))))))
    (pairlis '(:message) '("parameter"))
    *no-metadata*)
  (triggers "this is a parameter"
    "String interpolation of parameters works")

  (rules:eval/rules
    (rules:load/rules
      (parse `((for *
                  (when *
                    (made-it "this is $[meta]"))))))
    *no-params*
    (pairlis '(:meta) '("metadata")))
  (triggers "this is metadata"
    "String interpolation of metadata works"))

(subtest "Time-based predicates"
  (let ((rules::*NOW* (encode-universal-time 0 14 2 29 8 1997 4)))
    ; It's August 29th, 2:14am Eastern (a Friday)
    ; SkyNet has become self-aware.
    ; How do we notify about that?

    (rules:eval/rules
      (rules:load/rules
        (parse `((for *
                   (when ((on weekends))
                     (made-it "not yet..."))
                   (when ((on weekdays))
                     (made-it "skynet has become self-aware"))
                   (when * (made-it "wrong"))))))
      *no-params*
      *no-metadata*)
    (triggers "skynet has become self-aware")

    (rules:eval/rules
      (rules:load/rules
        (parse `((for *
                   (when ((on friday))
                     (made-it "skynet has become self-aware"))
                   (when * (made-it "wrong"))))))
      *no-params*
      *no-metadata*)
    (triggers "skynet has become self-aware")

    (rules:eval/rules
      (rules:load/rules
        (parse `((for *
                   (when ((after 0800 am))
                     (made-it "too late"))
                   (when * (made-it "correct"))))))
      *no-params*
      *no-metadata*)
    (triggers "correct")

    (rules:eval/rules
      (rules:load/rules
        (parse `((for *
                   (when ((before 0800 am))
                     (made-it "on time"))
                   (when * (made-it "wrong"))))))
      *no-params*
      *no-metadata*)
    (triggers "on time")

    (rules:eval/rules
      (rules:load/rules
        (parse `((for *
                   (when ((after 0100 pm))
                     (made-it "too late"))
                   (when * (made-it "on time"))))))
      *no-params*
      *no-metadata*)
    (triggers "on time")

    (rules:eval/rules
      (rules:load/rules
        (parse `((for *
                   (when ((from 0100 am to 0300 am))
                     (made-it "on time"))
                   (when * (made-it "wrong"))))))
      *no-params*
      *no-metadata*)
    (triggers "on time")


    (ok t))
  (ok t))

(subtest "Syntax errors"
  (is-error (rules:eval/rules
              (rules:load/rules
                (parse `((set 42))))
              *no-params*
              *no-metadata*) 'simple-error)
  (is-error (rules:eval/rules
              (rules:load/rules
                (parse `((set var))))
              *no-params*
              *no-metadata*) 'simple-error)
  (is-error (rules:eval/rules
              (rules:load/rules
                (parse `((foo bar))))
              *no-params*
              *no-metadata*) 'simple-error)
  (is-error (rules:eval/rules
              (rules:load/rules
                (parse `((for * (foo bar)))))
              *no-params*
              *no-metadata*) 'simple-error)
  (is-error (rules:eval/rules
              (rules:load/rules
                (parse `((for * (when * bar)))))
              *no-params*
              *no-metadata*) 'simple-error)
  (is-error (rules:eval/rules
              (rules:load/rules
                (parse `((for * (when * (foo bar))))))
              *no-params*
              *no-metadata*) 'simple-error)
  (is-error (rules:eval/rules
              (rules:load/rules
                (parse `((for * (when ((foo bar)) (made-it "fail"))))))
              *no-params*
              *no-metadata*) 'simple-error)
  (is-error (rules:eval/rules
              (rules:load/rules
                (parse `((for * (when ((after 0800 bc)) (made-it "fail"))))))
              *no-params*
              *no-metadata*) 'simple-error)
  (ok t))

(finalize)
