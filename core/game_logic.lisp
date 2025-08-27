;;; File: core/game_logic.lisp
(defpackage :core.game-logic
  (:use :cl :alexandria)
  (:export :play-dice-game
           :route-dot-command
           :register-dot-combo
           :run-word-twist))

(in-package :core.game-logic)

(defvar *dot-combo-table* (make-hash-table :test 'equal))
(defvar *event-log* nil)

;; --- Dice Roll Core ---
(defun roll-d6 () (+ 1 (random 6)))
(defun roll-dice (n) (loop repeat n collect (roll-d6)))

;; --- Dot-Command Routing (Word-Twist System) ---
(defun register-dot-combo (dot-key effect-func)
  "Register a new dot macro (scene/event/dialogue-twist)."
  (setf (gethash dot-key *dot-combo-table*) effect-func))

(defun route-dot-command (dot-command context)
  "Main entry: run and log a dot-combo macro, or return fallback."
  (let ((f (gethash dot-command *dot-combo-table*)))
    (push (list dot-command context (if f :success :fallback) (get-universal-time)) *event-log*)
    (if f (funcall f context)
        (format nil "[Unmapped dot-command: ~A]" dot-command))))

;; -- Example: Dice Game Infused with Word-Twist/Scene Mechanics --
(defun run-word-twist (dice-ctx user-str)
  "If user gave valid dot-command, run its effect in the dice scene."
  (cond
    ((string= user-str "fuck.you")
     (route-dot-command "fuck.you" dice-ctx))
    ((string= user-str "snake.eyes")
     (route-dot-command "snake.eyes" dice-ctx))
    ((string= user-str "vac.bs")
     (route-dot-command "vac.bs" dice-ctx))
    (t "[No word-twist triggered]")))

(defun play-dice-game (&optional (user-str ""))
  "Roll dice, dynamically trigger replayable, eventful dot-macro scenes."
  (let* ((dice (roll-dice 2))
         (outcome (cond
                    ((equal dice '(1 1)) 'snake.eyes)
                    ((equal dice '(6 6)) 'double.six)
                    ((> (car dice) (cadr dice)) 'over.under)
                    (t 'under.over))))
    (format t "~%Dice rolled: ~A | Outcome: ~A~%" dice outcome)
    (let ((scene-res (run-word-twist dice user-str)))
      (when (stringp scene-res) (format t "Twist: ~A~%" scene-res)))))

;; --- Register Core Word-Twist Combos (Expanding On Demand) ---
(register-dot-combo "fuck.you"
  (lambda (ctx)
    "Animosity escalates: NPC's mood shifts, scene tension spikes, fight chance rises."))

(register-dot-combo "snake.eyes"
  (lambda (ctx)
    "Bad luck: Gator-corpse bonus! Random sewer-creature event erupts, mocking banter."))

(register-dot-combo "vac.bs"
  (lambda (ctx)
    "Comic relief: Player groans about work-vacation, bystanders cheer, random street insult triggers."))

(register-dot-combo "double.six"
  (lambda (ctx)
    "Jackpot! Instant lucky dialogue, rare ally, or scene windfall initiated."))

(register-dot-combo "over.under"
  (lambda (ctx)
    "Your luck outpaces the other, triggering new bravado banter and minor stat buff."))
