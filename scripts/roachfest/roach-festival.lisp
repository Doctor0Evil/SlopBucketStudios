;;;; ============================================================
;;;;  SlopBucketStudios – "Roach.Festival.lisp"
;;;;  The Official Simulation of Roach Gut Terror at the Diner.
;;;; ============================================================

(defpackage :roach-festival
  (:use :cl :cl-ppcre))

(in-package :roach-festival)

;;; GLOBALS
(defparameter *cigarette-butt-count* 0)
(defparameter *roach-health* 5)
(defparameter *alarm-triggered* nil)
(defparameter *stomp-sequence-played* nil)

(defun lick-cigarette-butt ()
  "The roach greedily licks the cigarette butt like a maniac."
  (incf *cigarette-butt-count*)
  (format t "~&Roach licks cigarette butt [count=~A]... disgusting.~%" *cigarette-butt-count*)
  (when (> *cigarette-butt-count* 3)
    (panic-and-fly)))

(defun panic-and-fly ()
  "Roach panics, grows wings, flies at your face. The diner erupts in screams!"
  (setf *alarm-triggered* t)
  (format t "~&!!! ALARM TRIGGERED !!!~%")
  (format t "The roach ~A and flies straight for your eyeballs!~%" (random-roach-taunt))
  (format t "--- Diners are SCREAMING & running for the door! ---~%"))

(defun random-roach-taunt ()
  "Return a random, rude roach insult."
  (nth (random 5)
       '("spits tar in your soup!"
         "wiggles its greasy antennas mockingly!"
         "demands a light for another cig!"
         "laughs in insectoid Morse code!"
         "licks its own crack—taunting you!")))

(defun stomp-sequence ()
  "Player unleashes BOOT OF DESTINY!"
  (setf *stomp-sequence-played* t)
  (decf *roach-health* 5)
  (format t "~&>> BOOT crashes DOWN! CRUNCH! EXPLOSION of vile roach guts! <<~%")
  (roachgut-animation))

(defun roachgut-animation ()
  "Make intestines twitch and glitch like a proper 'roachgut.hiccups'."
  (dotimes (i 7)
    (format t "roachgut.hiccup ~A: legs twitch.exe~%" (1+ i)))
  (format t "~&The diner returns to silence... except the smell.~%"))

(defun roach-festival-demo ()
  "Simulate the entire cursed diner event."
  (format t "~&Welcome to SlopBucket Diner: Roach Festival!~%")
  (lick-cigarette-butt)
  (lick-cigarette-butt)
  (lick-cigarette-butt)
  ;; 4th lick triggers chaos
  (lick-cigarette-butt)
  (when *alarm-triggered*
    (format t "~&~%ROACH FLIGHT SEQUENCE INITIATED!~%"))
  (sleep 1)
  (stomp-sequence))

;;;; ============================================================
;;;; Run the demo at the REPL using:
;;;; > (roach-festival:roach-festival-demo)
;;;; ============================================================
