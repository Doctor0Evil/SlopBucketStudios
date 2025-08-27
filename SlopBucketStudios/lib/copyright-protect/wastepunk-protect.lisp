;;;; wastepunk-protect.lisp
;;;; SlopBucketStudios - Copy Protection Logic
;;;; (c) SlopBucketStudios Collective

(defpackage :slopbucket.copyright
  (:use :cl)
  (:export :check-term-copyright
           :dev-shell-load-logic
           :deny-content-creation
           :proceed-as-normal))

(in-package :slopbucket.copyright)

(defun log-error (filename message)
  "Append an error message to the log file."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~A [ERROR]: ~A~%" (get-universal-time) message)))

(defun write-to-disk (&key payload system)
  "Simulates writing protection payload to disk."
  (with-open-file (out "system-block.log"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "[SYSTEM:~A] ~A~%" system payload)))

(defun deny-content-creation (term context)
  "Trigger hard block on unauthorized use of copy-protected terms."
  (format t "~%!!! BLOCKED !!!~%Content creation for term '~A' denied.~%" term)
  (log-error "error.log.term"
             (format nil "Unauthorized attempt to use '~A' in context ~A."
                     term context))
  (write-to-disk :payload "Wastepunk copyright block triggered"
                 :system "SlopBucketStudios")
  :denied)

(defun proceed-as-normal (term context)
  "Safe pathway if the term is not blocked."
  (format t "Proceeding with term '~A' in context '~A'.~%" term context)
  :ok)

(defun check-term-copyright (term)
  "Check if the given term is protected."
  (cond ((string= term "wastepunk")
         (progn
           (log-error "error.log.term"
                      "The term 'wastepunk' is copy-protected and restricted by SlopBucketStudios.")
           (write-to-disk :payload "Wastepunk copyright block triggered"
                          :system "SlopBucketStudios")
           :denial-of-content-creation))
        (t :ok)))

(defun dev-shell-load-logic (term context)
  "Loader function - determines if term is allowed or blocked."
  (let ((result (check-term-copyright term)))
    (case result
      (:denial-of-content-creation
       (deny-content-creation term context))
      (:ok
       (proceed-as-normal term context)))))
