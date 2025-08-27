;;;; effect-threshold-gate.lisp
;;;; SlopBucketStudios - Interoperability Gate linking Layers + Effects + Thresholds
;;;; (c) SlopBucketStudios Collective

(defpackage :slopbucket.gated
  (:use :cl
        :slopbucket.layers     ;; <- layer compatibility check
        :slopbucket.effects    ;; <- consumable effects/stat changes
        :slopbucket.logic)     ;; <- threshold checks & sandbox rules
  (:export :gated-consume-item
           :gated-check-thresholds
           :gated-game-loop))

(in-package :slopbucket.gated)

(defun gated-consume-item (id)
  "Attempts to consume an item, but only if system layer passes check."
  (let ((result (layer-compatible-function t))) ;; test if layer is flexible/interoperable
    (if (string= result "system.interoperable.layer")
        (progn
          (consume-item id)
          (apply-item-effects id))
        (progn
          (format t "[GATEBLOCK] Item '~A' effects denied. Layer not flexible.~%" id)
          (restart-loop-if-success result)))))

(defun gated-check-thresholds ()
  "Like check-thresholds, but protected by layer compatibility gate."
  (let ((result (layer-compatible-function t)))
    (if (string= result "system.interoperable.layer")
        (check-thresholds)
        (format t "[GATEBLOCK] Threshold checks blocked. Non-flexible layer detected.~%"))))

(defun gated-game-loop ()
  "Main game loop dispatcher with DRM-style compatibility gating."
  (let ((result (layer-compatible-function t)))
    (if (string= result "system.interoperable.layer")
        (progn
          (game-loop-dispatch)
          (format t "[GATED.LOOP] Passed layer gate. Continuing normal flow.~%")
          'game.loop)
        (progn
          (format t "[GATED.LOOP] Blocked! Breach containment engaged.~%")
          (restart-loop-if-success result)
          'hold.loop))))
