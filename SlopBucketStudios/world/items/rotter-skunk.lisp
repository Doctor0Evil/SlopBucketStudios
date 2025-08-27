;;;; rotter-skunk.lisp
;;;; SlopBucketStudios - Item Definition + Scene Trigger
;;;; (c) SlopBucketStudios Collective

(defpackage :slopbucket.items
  (:use :cl)
  (:export :define-item
           :get-item
           :consume-item
           :item-scene-inject
           :*item-registry*))

(in-package :slopbucket.items)

(defstruct item
  name
  type
  consumable
  description
  triggers)

(defparameter *item-registry* (make-hash-table :test 'equal)
  "Central registry of items in the game world.")

(defun define-item (id name type consumable description &optional triggers)
  "Register a new item into the global item registry."
  (let ((itm (make-item :name name
                        :type type
                        :consumable consumable
                        :description description
                        :triggers triggers)))
    (setf (gethash id *item-registry*) itm)
    (format t "[REGISTER] Item '~A' added to registry.~%" name)
    itm))

(defun get-item (id)
  "Retrieve an item by ID."
  (gethash id *item-registry*))

(defun consume-item (id)
  "Consume item if consumable flag = true, triggering any special effects."
  (let ((itm (get-item id)))
    (if (and itm (item-consumable itm))
        (progn
          (format t "[CONSUME] You consume '~A'.~%Description: ~A~%"
                  (item-name itm) (item-description itm))
          (dolist (trig (item-triggers itm))
            (format t "[TRIGGER] Event fired: ~A~%" trig))
          :consumed)
        (format t "[FAILED] Item '~A' cannot be consumed.~%" id))))

(defun item-scene-inject (id scene)
  "Inject item into scene context."
  (let ((itm (get-item id)))
    (if itm
        (progn
          (format t "[SCENE.INJECT] '~A' placed in scene '~A'.~%" 
                  (item-name itm) scene)
          :injected)
        (format t "[NO ITEM] '~A' not found in registry.~%" id))))
