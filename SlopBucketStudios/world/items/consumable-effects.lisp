;;;; consumable-effects.lisp
;;;; SlopBucketStudios - Item Effects + Stat Mods + Animation Events
;;;; (c) SlopBucketStudios Collective

(defpackage :slopbucket.effects
  (:use :cl :slopbucket.items)
  (:export :register-player
           :apply-item-effects
           :clear-all-effects
           :define-event-type
           :switch-objects
           :*player-stats*
           :*effect-registry*
           :*event-types*))

(in-package :slopbucket.effects)

;;;; PLAYER REGISTRY + BASE STATS
(defparameter *player-stats*
  '((strength . 10)
    (accuracy . 10)
    (rage     . 0))
  "Represents simple player stats.")

(defun register-player (&key (strength 10) (accuracy 10) (rage 0))
  (setf *player-stats* `((strength . ,strength)
                         (accuracy . ,accuracy)
                         (rage . ,rage)))
  (format t "[INIT] Player registered with stats: ~A~%" *player-stats*))

(defun update-stat (key delta)
  "Modify player stat by delta."
  (let ((val (cdr (assoc key *player-stats*))))
    (setf (cdr (assoc key *player-stats*)) (+ val delta))
    (format t "[STAT] ~A changed by ~A → ~A~%" key delta (cdr (assoc key *player-stats*)))))

;;;; ITEM EFFECT REGISTRY
(defparameter *effect-registry* (make-hash-table :test 'equal)
  "Maps item IDs to a list of stat modifications.")

(defun apply-item-effects (id)
  "Applies registered effects for item consumption to player stats."
  (let ((effects (gethash id *effect-registry*)))
    (dolist (eff effects)
      (destructuring-bind (stat delta) eff
        (update-stat stat delta)))
    (if effects
        (format t "[EFFECTS] Applied effects for item '~A': ~A~%" id effects)
        (format t "[NO EFFECTS] '~A' has no registered stat modifications.~%" id))))

(defun clear-all-effects ()
  "Reset player stats back to a basic default. Simulates End.if.game reset."
  (setf *player-stats* '((strength . 10)
                         (accuracy . 10)
                         (rage . 0)))
  (format t "[RESET] Player stats reset to defaults.~%"))

;;;; EVENT TYPES + SPRITE SWITCH SYSTEM
(defparameter *event-types* (make-hash-table :test 'equal)
  "Dictionary of event types (animation/sprite) keyed by name.")

(defun define-event-type (event-name animation)
  "Define a sprite/animation event that can be triggered during gameplay."
  (setf (gethash event-name *event-types*) animation)
  (format t "[EVENT.DEFINE] '~A' → Animation/Sprite '~A'.~%" event-name animation))

(defun switch-objects (event-name)
  "Switch objects/animations based on current event type."
  (let ((anim (gethash event-name *event-types*)))
    (if anim
        (format t "[SWITCH] Event '~A' triggered: Switching to animation '~A'.~%" event-name anim)
        (format t "[NO.EVENT] Unknown event '~A'.~%" event-name))))
