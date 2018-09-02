(in-package :shout-test)

(defvar *topic* "test-topic")
(defvar *build-id* 0)

(defun transition (ok? &key status)
  (incf *build-id*)
  (api::set-state *topic*
    (make-instance 'api::event
                   :ok ok?
                   :message (if ok? "SUCCESS!"
                                    "FAILED!")
                   :link (format nil "http://ci/build/~d" *build-id*)))
  (let ((st (api::find-state *topic*)))
    (ok (not (null st))
        (format nil "Should have a ~A topic" *topic*))
    (if ok?
      (ok      (api::state-is-ok? st) "State should be ok")
      (ok (not (api::state-is-ok? st)) "State should not be ok"))
    (when status
      (is (api::status-of st) status
          (format nil "State should be '~A' now" status)))))


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
  (ok (null (api::find-state *topic*))
      "Should start out with no 'test-topic' state")

  (transition t   :status "working")
  (transition nil :status "broken")
  (transition t   :status "fixed")
  (transition t   :status "working")

  (setf *topic* "another-topic")
  (transition nil :status "broken")
  (transition t   :status "fixed")
  (transition t   :status "working"))

(finalize)
