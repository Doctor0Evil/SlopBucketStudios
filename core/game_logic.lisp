;;; File: core/game_logic.lisp
(defpackage :core.game-logic
  (:use :cl :alexandria)
  (:export :play-dice-game :word-twist-result :register-twist-macro))

(in-package :core.game-logic)

;; --- Dice Utilities ---
(defun roll-die (&optional (sides 6))
  "Rolls a die with N sides."
  (+ 1 (random sides)))

(defun roll-dice (&optional (n 2) (sides 6))
  "Rolls N dice with S sides."
  (loop repeat n collect (roll-die sides)))

;; --- Dynamic Dot-Command (Twist) Mapping ---
(defvar *twist-macro-table* (make-hash-table :test 'equal))

(defun register-twist-macro (dot-key func)
  "Register new word-twisting/dot-syntax macro for runtime extension."
  (setf (gethash dot-key *twist-macro-table*) func))

(defun word-twist-result (dot-cmd context)
  "Given a 'dot' command, return custom logic/scene/dialogue twist."
  (let ((f (gethash dot-cmd *twist-macro-table*)))
    (if f
        (funcall f context)
        (format nil "[No twist mapped to: ~A]" dot-cmd))))

;; --- Dice Game Core - Infused with Twist System ---
(defun play-dice-game ()
  "Runs a replayable dice game session; allows dynamic 'dot' command injection."
  (let* ((dice (roll-dice 2))
         (twist (if (> (car dice) (cadr dice)) "over.under" "under.over"))
         (outcome (cond
                    ((equal dice '(6 6)) "twist.lucky.double-six")
                    ((equal dice '(1 1)) "twist.snake-eyes")
                    (t twist))))
    (format t "~%You rolled: ~A~%Result: ~A~%Twist Effect: ~A~%"
            dice outcome (word-twist-result outcome dice))))

;; --- Register Example Word-Twist Macros ---
(register-twist-macro "twist.lucky.double-six"
  (lambda (context)
    "JACKPOT! Dialogue unlocks 'congratulate.winner' + special scene triggered."))

(register-twist-macro "twist.snake-eyes"
  (lambda (context)
    "Snake Eyes! Rival insults you ('fuck.you'), bar brawl nearly triggers!"))

(register-twist-macro "over.under"
  (lambda (dice)
    (if (> (car dice) (cadr dice))
        "Player dominates, unlocks 'dominance.scene.trigger'"
        "Result misrouted; logic error.")))

(register-twist-macro "under.over"
  (lambda (dice)
    (if (< (car dice) (cadr dice))
        "NPC taunts: 'better luck next time!', minor scuffle dialogue spun."
        "Result misrouted; logic error.")))
GitHub destination: core/game_logic.lisp
