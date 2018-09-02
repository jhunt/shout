(in-package :shout-test)

(plan nil)
(subtest "State introspection"
  (ok (not (api::state-is-ok? nil))
      "The nil state should not be ok")

  (ok (not (api::state-is-ok?
             (make-instance 'api::state)))
      "The empty state should not be ok")

  (ok (not (api::state-is-ok?
             (make-instance 'api::state
                            :last-event (make-instance 'api::event
                                                       :ok nil))))
      "A state with a non-ok last-event should not be ok")

  (ok (api::state-is-ok?
        (make-instance 'api::state
                       :last-event (make-instance 'api::event
                                                  :ok t)))
      "A state with an ok last-event should be ok"))

(subtest "State transitions"
  (ok (null (api::find-state "test-topic"))
      "Should start out with no 'test-topic' state")

  (api::set-state "test-topic"
                  (make-instance 'api::event
                                 :ok t
                                 :message "everything starts out fine"
                                 :link "http://ci.example.com/build/1"))

  (let ((st (api::find-state "test-topic")))
    (ok (not (null st)) "Should now have a 'test-topic' state")
    (ok (api::state-is-ok? st) "State should be ok")
    (is (api::status-of st) "working" "State should be 'working'"))

  (api::set-state "test-topic"
                  (make-instance 'api::event
                                 :ok nil
                                 :message "something broke"
                                 :link "http://ci.example.com/build/2"))

  (let ((st (api::find-state "test-topic")))
    (ok (not (api::state-is-ok? st))
        "State should have transitioned to broken")
    (is (api::status-of st) "broken"
        "State status should now be 'broken'"))

  (api::set-state "test-topic"
                  (make-instance 'api::event
                                 :ok t
                                 :message "it got fixed"
                                 :link "http://ci.example.com/build/3"))

  (let ((st (api::find-state "test-topic")))
    (ok (api::state-is-ok? st)
        "State should have transitioned to fixed")
    (is (api::status-of st) "fixed"
        "State status should now be 'fixed'"))

  (api::set-state "test-topic"
                  (make-instance 'api::event
                                 :ok t
                                 :message "it is still working"
                                 :link "http://ci.example.com/build/4"))

  (let ((st (api::find-state "test-topic")))
    (ok (api::state-is-ok? st)
        "State should still be ok")
    (is (api::status-of st) "working"
        "State status should now be 'working'"))

  (api::set-state "another-topic"
                  (make-instance 'api::event
                                 :ok nil
                                 :message "initially broken"))
  (let ((st (api::find-state "another-topic")))
    (ok (not (api::state-is-ok? st))
        "State should be not ok")
    (is (api::status-of st) "broken"
        "State status should initially be 'broken'")))

(finalize)
