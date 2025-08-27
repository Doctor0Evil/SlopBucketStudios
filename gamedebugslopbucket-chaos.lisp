;; SlopBucket Studios Daily Splatter
(defpackage slopbucket-chaos
  (:use :cl)
  (:export :run-daily-splatter))

(in-package :slopbucket-chaos)

(defparameter *dog-state* 'chewing-toy)
(defparameter *dog-aggression* 20)
(defparameter *dog-mood* (random-choice '(neutral excited feral hilarious)))
(defparameter *player-influence* 0)
(defparameter *world-mood* (random-choice '(freefall nonsense feral litigious orgy)))
(defparameter *event-log* nil)
(defparameter *ram-limit* (random-choice '(4 8 16 64)))

(defun random-npc (n)
  (loop for i below n
        collect (make-npc :name (format nil "NPC-~A" (1+ i))
                          :quirk (random-choice '(cat-raft-captain smoking-wolfman bootlicker baker-fleshfan))
                          :mood (random 100)
                          :aggression (random 100)
                          :favorite-action (random-choice '(complain hustle scam haunt howl)))))

(defun simulate-dog-event ()
  (let ((result (random-choice
                 '("The dog gnaws a sandal, barnacle crunch! Aggression +5, calm +10."
                   "Dog dreams of licking his balls, humanity frowns upon greatness."
                   "Dog narrows eyes, plots revenge collar for next smoke blown."))))
    (push result *event-log*)
    (log-guts 'dog result)))

(defun run-daily-splatter ()
  (log-guts 'init "SLOPBUCKET CHAOS YER GUTS ON THE FLOOR")
  (log-guts 'status (format nil "Dog State ~A Dog Aggression ~A Dog Mood ~A"
                           *dog-state* *dog-aggression* *dog-mood*))
  (simulate-dog-event)
  (inject-chaos-npc)
  (log-guts 'world (format nil "World Mood ~A RAM Limit ~A MB" *world-mood* *ram-limit*))
  (dolist (evt *event-log*)
    (log-guts 'event evt))
  (log-guts 'finish "END OF DAILY SLOP SUBMIT TO SLOPBUCKETSTUDIOS.BLOG FOR MAXIMUM ABSURDITY.")
  (values))

;; Usage:
;; (slopbucket-chaos:run-daily-splatter)
