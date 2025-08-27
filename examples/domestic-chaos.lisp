;;;; domestic-chaos.lisp
;;;; SlopBucketStudios - Household Chaos Event Trigger
;;;; (c) SlopBucketStudios Collective

(defpackage :slopbucket.domestic
  (:use :cl :slopbucket.triggers.ext)
  (:export :register-domestic-chaos
           :trigger-domestic-chaos))

(in-package :slopbucket.domestic)

(defun register-domestic-chaos ()
  "Registers household comedy/chaos events under trigger system."
  ;; Define core event trigger
  (register-trigger
   "Wakeup!.small-surprise.bed-turd.found.dog.mom.revenge.son's-mischief"
   'anim.mom.revenge.sequence)

  ;; Aliases for flexibility
  (register-trigger-alias "dog.turd.prank"
                          "Wakeup!.small-surprise.bed-turd.found.dog.mom.revenge.son's-mischief")
  (register-trigger-alias "revenge.mom.morning"
                          "Wakeup!.small-surprise.bed-turd.found.dog.mom.revenge.son's-mischief")

  (format t "[REGISTER:DCHAOS] Event family set up for mom revenge-turd prank.~%"))

(defun trigger-domestic-chaos (event-id)
  "Dispatch domestic chaos events."
  (dispatch-trigger-extended event-id))
