(defparameter *medicine-count* 1)
(defun check-meds ()
  (if (<= *medicine-count* 0)
      (progn
        (log-guts 'npc "WARNING: Out of medicine! Cognitive integrity dropping fast.")
        (trigger-chaos-event 'hallucination))
      (progn
        (decf *medicine-count*)
        (log-guts 'npc "Medicine consumed. Mild side effects: bitter mood, leg cramp, enlightenment."))))
