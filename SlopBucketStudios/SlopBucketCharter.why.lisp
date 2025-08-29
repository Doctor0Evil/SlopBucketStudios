;; SlopBucket Studios Chaos NPC: Grease Fry Pamphlet Machine
(defpackage :slopbucket-greasefry
  (:use :cl)
  (:export :run-greasefry-cycle :inject-greasefry-npc))

(in-package :slopbucket-greasefry)

;;--------------------------------
;; NPC Core State Vars
;;--------------------------------
(defparameter *npc-list* nil)
(defparameter *fcc-pamphlets* '("FCC says F*** YOU!" "Eat my grease fry!" "Pamphlet? Shit on that, pal!" "Official fuck-you!"))
(defparameter *greasefry-quirks* '("flapping-dick" "pamphlet-bomb" "birdshit-targeting" "grease-splatter"))
(defparameter *debug-log* nil)

;;--------------------------------
;; Logging Utility
;;--------------------------------
(defun log-greasefry (tag msg &rest args)
  (let ((entry (format nil "~A: ~A" (string-upcase (symbol-name tag)) (apply #'format nil msg args))))
    (push entry *debug-log*)
    (format t "~A~%" entry)))

;;--------------------------------
;; Pamphlet Handler Action
;;--------------------------------
(defun hand-out-pamphlet (npc)
  (let ((pamphlet (nth (random (length *fcc-pamphlets*)) *fcc-pamphlets*)))
    (log-greasefry 'npc "~A HANDS OUT: ~A" (getf npc :name) pamphlet)
    (pamphlet)))

;;--------------------------------
;; Squawk + Shit Attack
;;--------------------------------
(defun npc-squawk-shit (npc)
  (let ((insult (nth (random 4)
    '("You smell like expired lard!"
      "Squawk! FCC can gobble my chassis!"
      "Here's your pamphlet—eat it, fool!"
      "Cawwwk! Droppin' bombs! *flaps dick*"))))
    (log-greasefry 'squawk "~A SCREAMS: ~A" (getf npc :name) insult)
    insult))

;;--------------------------------
;; NPC Injection + Main Event Loop
;;--------------------------------
(defun inject-greasefry-npc ()
  (let ((npc (list :name (format nil "GreaseFry-~D" (random 9000))
                   :quirk (nth (random (length *greasefry-quirks*)) *greasefry-quirks*)
                   :madness (random 100))))
    (push npc *npc-list*)
    npc))

(defun run-greasefry-cycle ()
  (log-greasefry 'init "GREASEFRY SESSION STARTED")
  (let ((npc (inject-greasefry-npc)))
    (dotimes (i 8)
      (hand-out-pamphlet npc)
      (npc-squawk-shit npc))
    (log-greasefry 'finish "SESSION END. ~A pamphlets, ~A shit-bombs dropped."
                   (length *fcc-pamphlets*) 8)
    (reverse *debug-log*)))

;; To run: (run-greasefry-cycle)
