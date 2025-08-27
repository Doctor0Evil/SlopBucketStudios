;;; ================================================================
;;; COMPOUNDED INTERNAL LOGIC – CHARACTER CREATION MODULE (Weighted)
;;; ================================================================
(defpackage :wastepunk-character
  (:use :cl)
  (:export :create-character))
(in-package :wastepunk-character)

(defparameter *wastepunk-names*
  '("Skagjaw" "Rustbiter" "Hollow-Grit" "Fizzlegut" "Charcoal-Mary"
    "Bent-Clutch" "Spindle-Eyes" "Gutspike" "Sootlord" "Junkyard-Jo"))

(defparameter *wastepunk-quirks-serious*
  '("has a thousand-yard-stare during combat"
    "counts every bullet whispering prayers"
    "believes their shadow is conspiring against them"
    "never speaks unless blood has spilled"
    "uses scars as maps to forgotten places"))

(defparameter *wastepunk-quirks-absurd*
  '("always mutters inappropriate jokes mid-fight"
    "collects bottle caps but only the bent ones"
    "refuses to fight unless wearing mismatched shoes"
    "believes fire is the solution to all problems"
    "laughs hysterically when shot at"
    "tries to barter combat damage as 'small favors'"
    "insults enemies with surprisingly good poetry"))

(defun roll-dice (sides)
  (+ 1 (random sides)))

(defun generate-stats ()
  (list
   (cons :strength (roll-dice 10))
   (cons :perception (roll-dice 10))
   (cons :endurance (roll-dice 10))
   (cons :charisma (roll-dice 10))
   (cons :intelligence (roll-dice 10))
   (cons :agility (roll-dice 10))
   (cons :luck (roll-dice 10))))

(defun random-name ()
  (nth (random (length *wastepunk-names*)) *wastepunk-names*))

(defun weighted-quirk (humor-level)
  "Pick quirk based on humor-level (0 serious → 1 absurd)."
  (if (< (random 1.0) humor-level)
      (nth (random (length *wastepunk-quirks-absurd*)) *wastepunk-quirks-absurd*)
      (nth (random (length *wastepunk-quirks-serious*)) *wastepunk-quirks-serious*)))

(defun create-character (&key (theme "wastepunk")
                              (humor-level 0.5)
                              (dep-ref "post.war.savage.raider"))
  "Creates a randomized character for combat simulation."
  (let* ((cname (random-name))
         (cstats (generate-stats))
         (cquirk (weighted-quirk humor-level)))
    (format t "~%[CHARACTER CREATED @ theme ~A]" theme)
    (format t "~% Name: ~A" cname)
    (format t "~% Archetype Reference: ~A" dep-ref)
    (format t "~% Humor-Level (~,2f) → Quirk: ~A" humor-level cquirk)
    (format t "~% Primary Stats: ")
    (dolist (st cstats) (format t " ~A=~A" (car st) (cdr st)))
    (list :name cname :archetype dep-ref :quirk cquirk :stats cstats)))
;;; ================================================================
;;; github-file-destination: runtime/character/wastepunk_character_weighted.lisp
;;; ================================================================
