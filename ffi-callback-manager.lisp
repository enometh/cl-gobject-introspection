;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Wed Sep 18 06:21:10 2019 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; ;madhu 250327 (resourced from girlib ffi-callback-manager)

(in-package "CL-USER")
(defpackage "CFFI-CALLBACK-MANAGER"
  (:use "CL")
  (:export
   "CALLBACK-MANAGER"
   "REGISTER-CALLBACK"
   "UNREGISTER-CALLBACK"
   "FIND-CALLBACK"
   "WITH-REGISTERED-CALLBACK"
   "FUNCALL-OBJECT-CALLBACK"
   "FREE-FUNCALL-OBJECT-CALLBACK"))

(in-package "CFFI-CALLBACK-MANAGER")

(declaim (optimize (speed 0) (safety 1) (debug 3)))

(defstruct (callback-manager (:constructor %make-callback-manager))
  (lock (bordeaux-threads:make-lock "callback-manager-lock"))
  (queue (make-array 0 :adjustable t :fill-pointer t))
  (free-list nil))

(defvar *callback-manager* (%make-callback-manager))

;; REGISTER-CALLBACK Allocate a tag and return the CFFI:POINTER
;; of its location. This tag identifies the FUNCTION.  The location
;; pointer can be used to lookup the function via FIND-CALLBACK.
;; FIND-CALLBACK is intended to be used within a CFFI:DEFCALLBACK with
;; the location pointer being passed in as user-data. Once the lisp
;; FUNCTION is retrieved it can be called within the CFFI:DEFCALLBACK
;; form.

(defun register-callback (function)
  "Registers a lisp object with the CALLBACK-MANAGER. Returns a foreign
pointer which is the address of an integer that identifies the object
in the CALLBACK-MANAGER."
  (with-slots (queue free-list lock) *callback-manager*
    (bordeaux-threads:with-lock-held (lock)
      (loop for i from 0
	    for x across queue
	    if (eql x function)
	    do (return-from register-callback
		 (values (cffi:foreign-alloc :int :initial-element i)
			 nil)))
      (let* ((index (pop free-list)))
	(if index
	    (setf (elt queue index) function)
	    (progn (setq index (length queue))
		   (assert (= index (vector-push-extend function queue)))))
	(values
	 (cffi:foreign-alloc :int :initial-element index)
	 t)))))

(defun unregister-callback (loc)
  (with-slots (lock queue free-list) *callback-manager*
    (bordeaux-threads:with-lock-held (lock)
      (let ((index (cffi:mem-ref loc :int)))
	(when (< index (length queue))
	  (assert (not (find index free-list)))
	  (push index free-list)
	  (setf (elt queue index) nil)
	  (cffi:foreign-free loc))))))

(defun find-callback (loc)
  "Returns the lisp object registered with REGISTER-CALLBACK"
  (with-slots (lock queue free-list) *callback-manager*
    (bordeaux-threads:with-lock-held (lock)
      (let ((index (cffi:mem-ref loc :int)))
	(cond ((< index (length queue))
	       (assert (not (find index free-list)))
	       (elt queue index))
	      (t nil))))))

(defmacro with-registered-callback ((loc-var) function &body body)
  `(let ((,loc-var (register-callback ,function)))
     (unwind-protect (progn ,@body)
       (unregister-callback ,loc-var))))

(defun clear-callback-manager ()
  (with-slots (queue free-list) *callback-manager*
    (cffi:with-foreign-object (loc :int)
      (loop for i from 0
	    for x across queue
	    unless (find i free-list)
	    do (setf (cffi:mem-ref loc :int) i)
	    (unregister-callback loc)))
    (setq free-list nil)))

#+nil
(clear-callback-manager)

#+nil ;; bootstrap
(mapcar (lambda (x &aux s) (when (setq s (find-symbol x :gir-lib)))
			     (unintern s :gir-lib))
	'("FIND-CALLBACK" "WITH-REGISTERED-CALLBACK" "FUNCALL-OBJECT-ASYNC-READY-CALLBACK"))

