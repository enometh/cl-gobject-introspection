(in-package :gir)

;;(declaim (optimize (debug 3) (speed 0)))

(cffi:defcfun g-closure-add-finalize-notifier :void
  (closure :pointer) (data :pointer) (func :pointer))

(cffi:defcfun g-closure-new-simple :pointer (sizeof :int) (data :pointer))

(cffi:defcfun g-cclosure-new :pointer (sizeof :int) (data :pointer) (destroycb :pointer))
(cffi:defcfun g-cclosure-new-swap :pointer (sizeof :int) (data :pointer) (destroycb :pointer))

(cffi:defcfun g-closure-set-marshal :void (closure :pointer) (marshal :pointer))

(cffi:defcfun g-signal-connect-closure :ulong
  (instance :pointer)
  (detailed-signal :string)
  (closure :pointer)
  (after :boolean))

;; Define the closure struct to compute its size
(cffi:defcstruct g-closure
  (flags :unsigned-int)
  (marshal :pointer)
  (data :pointer)
  (notifiers :pointer))

(defvar *objects* (make-hash-table))

(defvar *user-data* nil)

(cffi:defcallback marshal :void ((closure :pointer)
                                 (return :pointer)
                                 (n-values :int)
                                 (params :pointer)
                                 (hint :pointer)
                                 (data :pointer))
  (declare (ignore hint data))
  (let ((lisp-func (gethash (cffi:pointer-address closure) *objects*))
	(*user-data* (cffi:foreign-slot-value closure '(:struct g-closure)
					      'data))
	(lisp-params
         (loop
            :for i :below n-values
            :collect 
            (let* ((gvalue (cffi:mem-aptr
			    params
			    '(:struct g-value-struct)
			    i))
		   (val (gvalue->lisp/free gvalue (gvalue-gtype gvalue)
					   :no-free t)))
	      (if (typep val 'object-instance)
		  (object-setup-gc val :nothing)
		  val)))))
    (let ((res (apply lisp-func lisp-params)))
      (unless (cffi:null-pointer-p return)
        (set-value! return (gvalue-gtype return) res)))))

(cffi:defcallback free-closure :void ((data :pointer) (closure :pointer))
  (declare (ignore data))
  (when (not (cffi:null-pointer-p closure))
    (remhash (cffi:pointer-address closure) *objects*)))

(defun make-closure (func &optional (data-ptr (cffi:null-pointer)))
  (let* ((g-closure-size (cffi:foreign-type-size '(:struct g-closure)))
         (closure-ptr (g-closure-new-simple
                       g-closure-size data-ptr))) ;; sizeof(GClosure) = 16
    (setf (gethash (cffi:pointer-address closure-ptr) *objects*) func)
    (g-closure-set-marshal closure-ptr (cffi:callback marshal))
    (g-closure-add-finalize-notifier closure-ptr
                                     (cffi:null-pointer)
                                     (cffi:callback free-closure))
    closure-ptr))

(defun c-func (value)
  (labels ((to-ptr (str)
             (declare (type string str))
             (cffi:foreign-symbol-pointer (substitute #\_ #\- str))))
    (etypecase value
      (string (to-ptr value))
      (keyword (to-ptr (string-downcase value)))
      (cffi:foreign-pointer value)
      (null (cffi:null-pointer)))))

(defun connect (g-object signal c-handler &key after swapped data)
  (let* ((object-ptr (if (typep g-object 'object-instance)
                         (this-of g-object)
                         g-object))
         (str-signal (string-downcase signal))
	 (data-ptr (if data
		       (if (typep data 'object-instance)
			   (this-of data)
			   data)
		       (cffi:null-pointer)))
         (c-handler (cond 
                      ((and (symbolp c-handler) (fboundp c-handler))
                       (symbol-function c-handler))
                      ((functionp c-handler) c-handler)
                      (t (c-func c-handler))))
         (flags (+ (if after 1 0) (if swapped 2 0)))
         (handler-id
          (typecase c-handler
            (function (g-signal-connect-closure 
                       object-ptr str-signal
                       (make-closure c-handler data-ptr) ; XXX swapped ignored
		       after))
            (t (g-signal-connect-data object-ptr
                                      str-signal 
                                      c-handler
				      data-ptr
				      (cffi:null-pointer)
                                      flags)))))
    handler-id))

(cffi:defcfun g-signal-handler-disconnect :void (instance :pointer) (id :ulong))
(defun disconnect (g-object id)
  (let* ((object-ptr (if (typep g-object 'object-instance)
                         (this-of g-object)
                         g-object)))
    (g-signal-handler-disconnect object-ptr id)))

(defun signal-lookup (signal-name object)
  "Return the signal id of the signal-name registered with object."
  (let* ((gtype (gtype (this-of object))))
    (eval `(cffi:foreign-funcall "g_signal_lookup"
				 :string ,signal-name
				 :long-long ,gtype
				 :int))))

(defun signal-handler-lookup (signal-name object)
  "Return the handler id of the signal handler registered for the
given signal with the object."
  (let ((id (signal-lookup signal-name object)))
    (cffi:foreign-funcall
     "g_signal_handler_find"
     :pointer (this-of object) ;instance
     :int (load-time-value
	   (nget (require-namespace "GObject")
		 "SignalMatchType" :id))   ;type
     :int id
     :int 0					;gquark
     :pointer (cffi:null-pointer)
     :pointer (cffi:null-pointer)
     :pointer (cffi:null-pointer)
     :int)))