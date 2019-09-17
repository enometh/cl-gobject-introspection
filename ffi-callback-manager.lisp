(in-package "GIR-LIB")

(defstruct (callback-manager (:constructor %make-callback-manager))
  (lock (bordeaux-threads:make-lock "callback-manager-lock"))
  (queue (make-array 0 :adjustable t :fill-pointer t))
  (free-list nil))

(defvar *callback-manager* (%make-callback-manager))

;; REGISTER-CALLBACK Allocate a tag which and return the CFFI:POINTER
;; of its location. This tag identifies the FUNCTION.  The location
;; pointer can be used to lookup the function via FIND-CALLBACK.
;; FIND-CALLBACK is intended to be used within a CFFI:DEFCALLBACK with
;; the location pointer being passed in as user-data. Once the lisp
;; FUNCTION is retrieved it can be called within the CFFI:DEFCALLBACK
;; form.

(defun register-callback (function)
  (with-slots (queue free-list lock) *callback-manager*
    (bordeaux-threads:with-lock-held (lock)
      (let* ((index (pop free-list)))
	(if index
	    (setf (elt queue index) function)
	    (progn (setq index (length queue))
		   (assert (= index (vector-push-extend function queue)))))
	(cffi:foreign-alloc :int :initial-element index)))))

(defun unregister-callback (loc)
  (with-slots (lock queue free-list) *callback-manager*
    (let ((index (cffi:mem-ref loc :int)))
      (bordeaux-threads:with-lock-held (lock)
	(push index free-list)
	(setf (elt queue index) nil)))
    (cffi:foreign-free loc)))

(defun find-callback (loc)
  (with-slots (lock queue free-list) *callback-manager*
    (let ((index (cffi:mem-ref loc :int)))
      (bordeaux-threads:with-lock-held (lock)
	(elt queue index)))))

(defmacro with-registered-callback ((loc-var) function &body body)
  `(let ((,loc-var (register-callback ,function)))
     (unwind-protect (progn ,@body)
       (unregister-callback ,loc-var))))
