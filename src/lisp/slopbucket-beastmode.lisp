;; ============================================
;; SLOPBUCKET STUDIOS: Beastmode Narrative Engine
;; ============================================

;; Debug-level gore flooding toggle
(defparameter *debug-guts* t)

(defun debug-log (msg &rest args)
  (when *debug-guts*
    (format t "~&[SLOPBUCKET.DEBUG] " )
    (apply #'format t msg args)
    (terpri)))

;; World Entities =================================================
(defstruct (character (:print-function print-character))
  name
  quirks
  malfunction)

(defun print-character (c stream depth)
  (declare (ignore depth))
  (format stream "<~a: Quirks=~a Malfunction=~a>"
          (character-name c)
          (character-quirks c)
          (character-malfunction c)))

;; Narration ======================================================
(defun random-glitch ()
  (nth (random 5)
       '("::SYSTEM.GLITCH:: vomiting sparkles!"
         "::internal.cpu meltdown!::"
         "::fisting the urinal server::"
         "::Reality collapses into ASCII art::"
         "::Overclocked hamster wheel explodes::")))

(defun narrate-chaos (character action)
  (debug-log "Narrating chaos for ~a doing ~a"
             (character-name character)
             action)
  (format t "~%~a attempts to ~a, but instead ~a~%"
          (character-name character)
          action
          (if (zerop (random 2))
              (character-malfunction character)
              (random-glitch))))

;; BeastMode Toggle ==============================================
(defun unleash-beast-mode (characters actions iterations)
  (debug-log "Beast Mode engaged with ~a characters, ~a actions, ~a iterations"
             (length characters)
             (length actions)
             iterations)
  (dotimes (i iterations)
    (let ((c (nth (random (length characters)) characters))
          (a (nth (random (length actions)) actions)))
      (narrate-chaos c a))))

;; Content Feed ===================================================
(defun daily-slopbucket-show ()
  (let ((cast (list
                (make-character :name "S.Barks!"
                                :quirks "Cybernetic Great Dane"
                                :malfunction "malfunctions violently, handing out ass-beatings in public")
                (make-character :name "Captain Cupcakes"
                                :quirks "Overclocked sugar demon"
                                :malfunction "reboots into disco mode uncontrollably")
                (make-character :name "Trashcan R2.DOOM"
                                :quirks "shit-talking server bin"
                                :malfunction "spills user guts onto the floor")))

        (actions '("give a TED talk"
                   "hack the main urinal.server"
                   "challenge Mike Tyson in church"
                   "lick their own balls in public"
                   "start a war over expired burritos")))

    (debug-log "Launching daily Slopbucket content feed…")
    (unleash-beast-mode cast actions 8)))

;; ===============================================================
;; To run the madness:
;; (daily-slopbucket-show)
;;
;; Debug spew will flood, characters malfunction,
;; and "guts" of absurd narrative will splatter on screen.
;; ===============================================================
