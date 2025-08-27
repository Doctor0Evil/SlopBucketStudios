;;;; layer-compatible.lisp
;;;; SlopBucketStudios - Interoperable Layer Checker
;;;; (c) SlopBucketStudios Collective

(defpackage :slopbucket.layers
  (:use :cl)
  (:export :layer-compatible-function
           :*last-layer-check*))

(in-package :slopbucket.layers)

(defparameter *last-layer-check* nil
  "Holds status of the last compatibility test.")

(defun log-debug (filename message)
  "Append debugging/warning messages into a log file."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~A [DEBUG/WARN]: ~A~%" (get-universal-time) message))
  (format t "[WARN] ~A~%" message))

(defun layer-compatible-function (is-flexible?)
  "Check if layer compatibility is flexible and interoperable."
  (cond
    ((eq is-flexible? t)
     (setf *last-layer-check* 'success)
     (format t "[LAYER] Interoperability check passed. Restarting loop...~%")
     "system.interoperable.layer")
    (t
     (setf *last-layer-check* 'error)
     (log-debug "debug.log.trace" "Layer not flexible - returning error state.")
     "return_error")))

(defun restart-loop-if-success (result)
  "Handles return values and controls whether loop resumes."
  (if (string= result "system.interoperable.layer")
      (progn
        (format t "[SYSTEM] Success! Returning to main loop.~%")
        'restart.loop)
      (progn
        (format t "[SYSTEM] Error encountered. Containment active.~%")
        'hold.loop)))
