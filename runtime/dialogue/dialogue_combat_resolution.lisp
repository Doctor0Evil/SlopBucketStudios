;;; ================================================================
;;; DIALOGUE-COMBAT RESOLUTION MODULE
;;; ================================================================
(defpackage :dialogue-combat
  (:use :cl)
  (:export :resolve-outcome))
(in-package :dialogue-combat)

(defparameter *good-outcome* "of course i did it!")
(defparameter *bad-outcome* "wasn't.me")

(defun roll-chance () 
  "Return random float between 0.0–1.0"
  (random 1.0))

(defun resolve-outcome (&key (chance-to-fight 0.15) (chance-to-blame 0.5))
  "Resolves NPC/player interaction over 'blame'.
   - chance-to-blame → likelihood outcome = good vs. bad.
   - chance-to-fight → likelihood of combat if bad outcome occurs.
   Returns keyword event tags for ZDRE engine."
  (let* ((blame-roll (roll-chance))
         (fight-roll (roll-chance))
         (outcome nil)
         (event nil))
    (if (< blame-roll chance-to-blame)
        (progn
          (setf outcome *good-outcome*)
          (setf event :resolved.good))
        (progn
          (setf outcome *bad-outcome*)
          ;; nested: chance to trigger rare combat
          (if (< fight-roll chance-to-fight)
              (setf event :combat.initiated.rare)
              (setf event :npc.react.not-my-problem))))
    ;; Print debug log
    (format t "~%[DIALOGUE] Outcome=~A" outcome)
    (format t "~%[RESULT] Event triggered=~A" event)
    (list :outcome outcome :event event)))
;;; ================================================================
;;; github-file-destination: runtime/dialogue/dialogue_combat_resolution.lisp
;;; ================================================================
