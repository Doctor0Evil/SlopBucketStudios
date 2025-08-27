;;;; trigger.animation.lisp
;;;; SlopBucketStudios - Event Trigger/Animation Dispatcher
;;;; (c) SlopBucketStudios Collective

(defpackage :slopbucket.triggers
  (:use :cl)
  (:export :register-trigger
           :dispatch-trigger
           :*trigger-registry*))

(in-package :slopbucket.triggers)

(defparameter *trigger-registry* (make-hash-table :test 'equal)
  "Registry mapping string triggers to animation keywords.")

(defun log-trigger (filename trigger animation)
  "Logs triggers firing into specified log file."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~A [TRIGGER]: '~A' => ~A~%"
            (get-universal-time) trigger animation)))

(defun register-trigger (trigger animation-key)
  "Register a trigger phrase → animation keyword mapping."
  (setf (gethash trigger *trigger-registry*) animation-key)
  (format t "[REGISTER] Trigger '~A' maps to animation '~A'.~%"
          trigger animation-key))

(defun dispatch-trigger (trigger)
  "Dispatches the trigger, firing associated animation payload."
  (let ((animation (gethash trigger *trigger-registry*)))
    (if animation
        (progn
          (log-trigger "trigger.log" trigger animation)
          (format t "[DISPATCH] Triggered '~A' → Animation '~A'~%" trigger animation)
          animation)
        (progn
          (format t "[NO MATCH] No animation registered for trigger '~A'.~%" trigger)
          :none))))
