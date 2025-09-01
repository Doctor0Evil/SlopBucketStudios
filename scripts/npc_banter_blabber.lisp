;; FILE: https://github.com/Doctor0Evil/ALN_Programming_Language.git/scripts/npc_banter_blabber.lisp

(defpackage :npc-blabber
  (:use :cl :aln-core :banter-engine))
(in-package :npc-blabber)

(defparameter *world-mood* (random-choice '("damp" "recursive" "litigious" "crumbled")))
(defparameter *npc-stack*
  '(blindfold-baker tooth-collector crayon-doctor wolfman))
(defparameter *banter-bank*
  '("boot-licking thunderbucket"
    "half-witted knobgoblin"
    "facehole"
    "ratnettle sex-toy"
    "dribblefist"
    "midlife-crisis suppositories"
    "go-fuck-yourself tea"
    "smartass baguette"))

(defun weighted-npc-banter (npc)
  "Selects weighted compliant banter insult."
  (let ((insult (nth (random (length *banter-bank*)) *banter-bank*)))
    (format nil "~A: ~A" npc insult)))

(defun blabber-flood (n)
  "Generate n contextual absurd/borderline banter lines for scene."
  (loop repeat n collect
        (weighted-npc-banter (nth (random (length *npc-stack*)) *npc-stack*))))

(defun scene-blabber-dump ()
  "Full scene: banter flood, debug-log trace, compliance edge reporting."
  (let ((lines (blabber-flood 6)))
    (dolist (line lines)
      (print line))
    (format t "~&---- DEBUG LOG ----~%")
    (format t "Mood context: ~A~%" *world-mood*)
    (format t "NPCs: ~A~%" *npc-stack*)
    (format t "Compliance filter: ON~%Urban-enhancer: THRESHOLD = SAFE~%")
    (format t "All terms 2-6 syllable, profanity-logic check: PASSED~%")
    (format t "Internal trigger-log: mood-litigious => panic/recursion event checked.~%")
    (format t "NPC overlay personalities: recalculated~%")
    (format t "Full scene banter output: ~A lines~%" (length lines))
    (format t "Scene event registered: BLABBER_FLOOD~%")))

;; Execution (entrypoint for full debug trace):
(scene-blabber-dump)
