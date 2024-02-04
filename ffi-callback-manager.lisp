(in-package "GIR-LIB")

(declaim (optimize (speed 0) (safety 1) (debug 3)))

(defstruct (callback-manager (:constructor %make-callback-manager))
  (lock (make-lock "callback-manager-lock"))
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
    (with-lock-held (lock)
      (let* ((index (pop free-list)))
	(if index
	    (setf (elt queue index) function)
	    (progn (setq index (length queue))
		   (assert (= index (vector-push-extend function queue)))))
	(cffi:foreign-alloc :int :initial-element index)))))

(defun unregister-callback (loc)
  (with-slots (lock queue free-list) *callback-manager*
    (let ((index (cffi:mem-ref loc :int)))
      (with-lock-held (lock)
	(push index free-list)
	(setf (elt queue index) nil)))
    (cffi:foreign-free loc)))

(defun find-callback (loc)
  "Returns the lisp object registered with REGISTER-CALLBACK"
  (with-slots (lock queue free-list) *callback-manager*
    (declare (ignorable free-list))
    (let ((index (cffi:mem-ref loc :int)))
      (with-lock-held (lock)
	(elt queue index)))))

(defmacro with-registered-callback ((loc-var) function &body body)
  `(let ((,loc-var (register-callback ,function)))
     (unwind-protect (progn ,@body)
       (unregister-callback ,loc-var))))

;;Define XXX-MAP-CALLBACK MAP-XXX-1 and (MAP-XXX result-type function
;;cffi-ptr)
(defmacro define-foreach-callback-mapper (c-func-name c-type-name)
  (let ((cffi-callback-name (intern (concatenate 'string (string-upcase c-type-name) "-MAP-CALLBACK")))
	(ptr-var-name (intern (concatenate 'string (string-upcase c-type-name) "-PTR")))
	(helper-function-name (intern (concatenate 'string "MAP-" (string-upcase c-type-name) "-1")))
	(map-function-name  (intern (concatenate 'string "MAP-" (string-upcase c-type-name)))))
    `(progn
       (cffi:defcallback ,CFFI-CALLBACK-NAME :void
	   ((data :pointer) (user-data :pointer))
	 (let ((thunk (find-callback user-data)))
	   (with-simple-restart (skip-execution ,(format nil "Skip Executing one ~A" c-func-name))
	     (funcall thunk data))))

       (defun ,HELPER-FUNCTION-NAME (,PTR-VAR-NAME function)
	 (with-registered-callback (loc) function
	   (cffi:foreign-funcall
	    ,c-func-name
	    :pointer ,PTR-VAR-NAME
	    :pointer (CFFI-SYS:%CALLBACK ',CFFI-CALLBACK-NAME)
	    :pointer loc)))

       (defun ,MAP-FUNCTION-NAME (result-type function ,PTR-VAR-NAME)
	 (let* ((ret nil)
		(func (if result-type
			  #'(lambda (x) (push (funcall function x) ret))
			  function)))
	   (,HELPER-FUNCTION-NAME ,PTR-VAR-NAME func)
	   (if result-type (coerce (nreverse ret) result-type)))))))

#+nil
(define-foreach-callback-mapper "g_slist_foreach" "SLIST")


;; provide an interface similar to 1) cl-gobject-introspection-wrapper
;; glib and 2) cl-cffi-gtk's stable pointer

(cffi:defcallback funcall-object-callback :bool ((user-data :pointer))
  (with-simple-restart (cont "Skip Error during Execution")
    (funcall (find-callback user-data))))

(cffi:defcallback free-funcall-object-callback :void ((user-data :pointer))
  (unregister-callback user-data))


(defun idle-add (function)
  (let ((loc (register-callback function)))
    (gir:invoke (*glib* "idle_add") 200
		(cffi:callback funcall-object-callback)
		loc
		(cffi:callback free-funcall-object-callback))))

#+nil
(idle-add (lambda () (warn "BOO")))

(defmacro block-idle-add (&body forms)
  `(let ((.main-loop. (gir:invoke (*glib* "MainLoop" "new") nil nil))
	 (.result.))
     (idle-add (lambda ()
		 (setq .result. (multiple-value-list (progn ,@forms)))
		 (gir:invoke (.main-loop. "quit"))))
     (gir:invoke (.main-loop. "run"))
     (values-list .result.)))

#+nil
(block-idle-add
  (sleep 2)
  (format t "depth=~D OK~%" (gir:invoke (*glib* "main_depth")))
  (values 1 2 3))

(export '(idle-add block-idle-add))



;;; handle patterns that use GAsynReadyCallback by using the user-data
;;; field to indicate a registered thunk. The thunk will be called
;;; with 2 parameters, the source object and the GAsyncResult (no
;;; third user-data parameter).

(cffi:defcallback funcall-object-async-ready-callback :void
    ((SOURCE-OBJECT :POINTER) (RES :POINTER) (USER-DATA :POINTER))
  "ARGS:  SOURCE-OBJECT Object. RES AsyncResult."
  (let* ((thunk (find-callback user-data))
	 (source (UNLESS (CFFI-SYS:NULL-POINTER-P SOURCE-OBJECT)
		   (GIR::GOBJECT (GIR:GTYPE SOURCE-OBJECT)
				 SOURCE-OBJECT)))
	 (async-result (UNLESS (CFFI-SYS:NULL-POINTER-P RES)
			 (GIR::GOBJECT (GIR:GTYPE RES)
				       RES))))
    (with-simple-restart (skip "Skip Error")
      (assert (functionp thunk))
      (funcall thunk source async-result))))

;; unused
#+nil
(cffi:defcallback free-funcall-object-async-ready-callback :void
    ((user-data :pointer))
  (unregister-callback user-data))



;;; example
#+nil
(defun get-file-contents (path)
  (let* ((file (gir:invoke (*gio* "File" "new_for_path") path))
	 (contents)
	 (main-loop (gir:invoke (*glib* "MainLoop" "new") nil nil)))
    (flet ((finish (source async-result)
	     (multiple-value-bind (ret contents-1 etag-out)
		 (gir:invoke (source "load_contents_finish") async-result)
	       (declare (ignorable ret etag-out))
	       (setq contents contents-1)
	       (gir:invoke (main-loop "quit")))))
      (with-registered-callback (loc) #'finish
	(gir:invoke (file "load_contents_async")
	  nil
	  (cffi:callback funcall-object-async-ready-callback)
	  loc)
	(gir:invoke (main-loop "run"))
	contents))))

#+nil
(get-file-contents "/etc/passwd")
