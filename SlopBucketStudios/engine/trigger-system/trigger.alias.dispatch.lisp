;;;; trigger.alias.dispatch.lisp
;;;; SlopBucketStudios - Synonym/Alias Trigger System
;;;; (c) SlopBucketStudios Collective

(defpackage :slopbucket.triggers.ext
  (:use :cl :slopbucket.triggers)
  (:export :register-trigger-alias
           :dispatch-trigger-extended
           :*trigger-aliases*))

(in-package :slopbucket.triggers.ext)

(defparameter *trigger-aliases* (make-hash-table :test 'equal)
  "Registry mapping alias strings to canonical triggers.")

(defun log-system-event (filename event trigger)
  "Logs event firing into both console + system log file."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~A [EVENT.TRIGGER]: ~A => ~A~%"
            (get-universal-time) event trigger))
  (format t "[WARN:EVENT] ~A triggered → '~A'~%" event trigger))

(defun register-trigger-alias (alias canonical)
  "Map an alias term to a canonical trigger phrase."
  (setf (gethash alias *trigger-aliases*) canonical)
  (format t "[REGISTER] Alias '~A' now redirects to trigger '~A'.~%"
          alias canonical))

(defun resolve-canonical-trigger (trigger)
  "Resolve aliases to their canonical trigger if present."
  (gethash trigger *trigger-aliases* trigger))

(defun dispatch-trigger-extended (trigger)
  "Dispatch with alias resolution, logging to system logs."
  (let* ((canonical (resolve-canonical-trigger trigger))
         (animation (gethash canonical slopbucket.triggers::*trigger-registry*)))
    (if animation
        (progn
          (log-system-event "sys.log.event.triggers" "event.trigger" canonical)
          (log-trigger "trigger.log" canonical animation)
          (format t "[DISPATCH] Alias/Trigger '~A' → '~A' → Animation '~A'~%"
                  trigger canonical animation)
          animation)
        (progn
          (format t "[NO MATCH] Nothing mapped for '~A'.~%" trigger)
          :none))))
