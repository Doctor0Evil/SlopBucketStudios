;;;; File: see.u.later/bye.falicia/organizational/threat_containment.lisp
(defpackage :slopbucket.containment
  (:use :cl :cl-user)
  (:export :contain-threat))
(in-package :slopbucket.containment)
(defparameter *threat-log* nil)
(defun log-threat (event details)
  (push (list (get-universal-time) event details) *threat-log*)
  (format t "CONTAINMENT-LOG ~A : ~A~%" event details))
(defun contain-threat (target reason)
  (log-threat "BEGIN_CONTAINMENT" (list target reason))
  ;; Segment asset: dog-NPC, rogue NPC, errant user, breached server
  (case target
    (:dog-npc   (format t "Dog NPC isolated. Aggression state locked.~%"))
    (:npc       (format t "NPC flagged. Quarantined in sandbox.~%"))
    (:server    (format t "Server segment isolated. Firewall raised.~%"))
    (otherwise  (format t "Unknown target. Logging threat, no action.~%")))
  (log-threat "END_CONTAINMENT" target)
  'contained)
;;;; Usage:
;;; (slopbucket.containment:contain-threat :npc "Detected unauthorized action")
;;; (slopbucket.containment:contain-threat :server "Suspicious traffic pattern")
