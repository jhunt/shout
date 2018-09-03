(in-package :io)

(defvar *log-level* nil)

(defun prepend (prefix str)
  (format nil "~~&[~A]: ~A~%" prefix str))

(defun debugf (fmt &rest args)
  (if (or (eq *log-level* :debug)
          (eq *log-level* :info))
    (apply #'format (append (list *error-output*
                                  (prepend "DEBUG" fmt))
                            args))))

(defun infof (fmt &rest args)
  (if (eq *log-level* :info)
    (apply #'format (append (list *error-output*
                                  (prepend "INFO" fmt))
                            args))))
