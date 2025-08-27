;;; SlopBucketStudios / Lisp Engine
;;; World-Builder: Cyberpunk Post-War Junkie Scavenger Setting
;;; File: worlds/perplexity-cyberpunk-postwar.lisp

(defpackage :slopbucket.cyberpunk
  (:use :cl :alexandria)
  (:export :spawn-setting
           :generate-event
           :junkie-dialogue))

(in-package :slopbucket.cyberpunk)

;; Core world parameters
(defparameter *ruins-locations*
  '("cracked metro tunnels"
    "burnt-out skyscraper husks"
    "neon-stained alleyways"
    "collapsed freeway overpass"
    "scavenger market built of car-doors"))

(defparameter *npc-archetypes*
  '("slutty-bitch in a shaved wig"
    "cracked-out ruins-raider"
    "chrome-teethed ex-gang banger"
    "junk-preacher screaming prophecy"
    "ex-merc with missing digits"))

(defparameter *ambient-vibes*
  '("smell of melted circuitry"
    "piss-soaked neon fog"
    "broken synthwave echoing in static"
    "corpse-rats feasting in rhythm"
    "sirens bleeding into gunfire"))

;; Random helper
(defun rnd (list)
  (nth (random (length list)) list))

;; World Builder Functions
(defun spawn-setting ()
  "Returns a description of a random ruin-street backdrop."
  (format nil "~A lit with ~A, haunted by ~A."
          (rnd *ruins-locations*)
          (rnd *ambient-vibes*)
          (rnd *npc-archetypes*)))

(defun generate-event ()
  "Random conflict/drama event matching tone."
  (let ((actor1 (rnd *npc-archetypes*))
        (actor2 (rnd *npc-archetypes*)))
    (format nil "~A just smashed ~A's teeth across the pavement - crowd chants 'epic-necksnapping-buffet!'"
            actor1 actor2)))

(defun junkie-dialogue ()
  "Return raw scavenger banter, crackling cyberpunk nonsense."
  (format nil "«Listen up, fucker—~A lurks near ~A. Step wrong, and it's *game over*.»"
          (rnd *npc-archetypes*)
          (rnd *ruins-locations*)))
