(in-package "GIR-LIB")
(export '(create-signal emit-signal))

(defun create-signal (&key itype name flags return-type param-types accumulator accu-data)
  "Returns the ID. Should be done in a class initializer"
  (check-type itype number)
  (check-type name string)
  (assert (gir:invoke (*gobject* "signal_is_valid_name") name))
  (let ((null-ptr (cffi:null-pointer)) ret param-types-ptr)
    (unwind-protect
	 (let* ((accumulator-ptr
		 (cond (accumulator (assert (cffi:pointerp accumulator))
				    accumulator)
		       (t null-ptr)))
		(accu-data-ptr
		 (cond (accu-data (assert (cffi:pointerp accu-data)) accu-data)
		       (t null-ptr)))
		(rtype (etypecase return-type
			 (null gir::+g-type-none+)
			 (number return-type)
			 (keyword (let ((tmp (gir::%gtype return-type)))
				    (assert (numberp tmp))
				    tmp))))
		(n-params (length param-types)))
	   (when (> n-params 0)
	     (setq param-types-ptr
		   (cffi:foreign-alloc :ulong :initial-element 0
				       :count n-params))
	     (loop for i from 0
		   for arg-type in param-types
		   do (let ((tmp (etypecase arg-type
				   (keyword (gir::%gtype arg-type))
				   (number arg-type))))
			(check-type tmp number)
			(setf (cffi:mem-aref param-types-ptr :ulong i)
			      tmp))))
	   (setq ret
		 (cffi:foreign-funcall "g_signal_newv"
				       :string name
				       :ulong itype
				       :int (or flags 0)
				       :pointer null-ptr ;class closure
				       :pointer accumulator-ptr
				       :pointer accu-data-ptr
				       :pointer null-ptr ; c_marshaller, no
				       :ulong rtype
				       :uint n-params
				       :pointer (or param-types-ptr null-ptr)
				       :uint)))
      (g-message "created signal ~S" ret)
      (when param-types-ptr
	(cffi:foreign-free param-types-ptr)))
    ret))

;; TODO cannot call (gir:invoke (*gobject* "signal_emitv") (list* obj
;; args) sig-id, ret) because lisp always segfaults on some gvalue
;; manipulation bug

(defun emit-signal (instance signal-name-or-id &rest args)
  "Emit a signal on instance in the most inefficient way. If a numeric
id is passed for the signal detail is always set to 0."
  (check-type instance gir::object-instance)
  (let* ((i-type (gir::gtype-of (gir:gtype-of instance)))
	 (det-id 0)
	 (sig-id (etypecase signal-name-or-id
		   (number signal-name-or-id)
		   (string
		    (multiple-value-bind (ret signal-id detail-id)
			(gir:invoke (*gobject* "signal_parse_name")
				    signal-name-or-id
				    i-type
				    t)
		      (assert ret nil
			  "Could not look up signal ~S" signal-name-or-id)
		      (setq det-id detail-id)
		      signal-id)))))
    (multiple-value-bind (ignore sigdet)
	(gir:invoke (*gobject* "signal_query") sig-id)
      (declare (ignore ignore))
      (let* ((nparams (gir:field sigdet "n_params"))
	     (ret-type (gir:field sigdet "return_type"))
	     (arg-types (gir:field sigdet "param_types"))
	     (len (+ 1 1 nparams))
	     (storage ;; ret-val + instance + params
	      (cffi:foreign-alloc '(:struct gir::g-value-struct) :count len)))
	(cffi:foreign-funcall
	 "memset" :pointer storage
	 :int 0
	 :int (* len (cffi:foreign-type-size '(:struct gir::g-value-struct))))
	(unless (eql ret-type gir::+g-type-none+)
	  (gir::g-value-init
	   (cffi:mem-aptr storage '(:struct gir::g-value-struct)  0)
	   ret-type))
	(gir::g-value-init
	 (cffi:mem-aptr storage '(:struct gir::g-value-struct)  1)
	 i-type)
	(gir::gvalue-set
	 (cffi:mem-aptr storage '(:struct gir::g-value-struct)  1)
	 (this-of instance))
	(loop for i from 2 repeat nparams
	      for arg in args
	      for gtype in arg-types
	      do
	      (gir::g-value-init
	       (cffi:mem-aptr storage '(:struct gir::g-value-struct)  i)
	       gtype)
	      (gir::gvalue-set
	       (cffi:mem-aptr storage '(:struct gir::g-value-struct)  i)
	       arg))
	(cffi:foreign-funcall
	 "g_signal_emitv"
	 :pointer (cffi:mem-aptr storage '(:struct gir::g-value-struct) 1)
	 :uint sig-id
	 :int32 det-id
	 :pointer (if (eql ret-type gir::+g-type-none+)
		      (cffi:null-pointer)
		      (cffi:mem-aptr storage '(:struct gir::g-value-struct) 0))
	 :void)
	(let ((ret
	       (if (eql ret-type gir::+g-type-none+)
		   nil
		   (gir::gvalue-get
		    (cffi:mem-aptr storage '(:struct gir::g-value-struct) 0)))))
	  (gir::g-value-unset
	   (cffi:mem-aptr storage '(:struct gir::g-value-struct) 1))
	  (loop for i from 2 repeat nparams
		do
		(gir::g-value-unset
		 (cffi:mem-aptr storage '(:struct gir::g-value-struct) i)))
	  (cffi:foreign-free storage)
	  ret)))))


#||
;; example from pygobject/guide/api/signals.html

(defvar $myclass-obj
  (make-instance 'gir:gobject-subclassable-info
    :gir-name "MyClass"
    :gir-parent (gir:nget *gobject* "Object")
    ;; :vfunc-overrides '("get_property" "set_property")
    ))

(eval (gir:%gobject-subclassable-define-cstructs  $myclass-obj))
(eval (gir:%gobject-subclassable-define-init-callbacks $myclass-obj))

(defvar $test-signal-specs
  (list :name "test"
	:flags (logior (gir:nget *gobject* "SignalFlags" :run-last))
	:return-type (gir:%gtype :boolean)
	:param-types (list (gir::%gtype :object))
	:accumulator (cffi:null-pointer) ; (cffi:foreign-symbol-pointer "g_signal_accumulator_true_handled")
	:accu-data (cffi:null-pointer)))

(defvar $i-type (gir::%gobject-subclassable-register-type-dynamic $myclass-obj))
(defvar $test-obj (gir::gobject-new $i-type))

(defvar $sig-id
  (apply #'create-signal (append (list :itype $i-type) $test-signal-specs)))

(assert
    (equal $sig-id (gir:invoke (*gobject* "signal_lookup")
			       "test"
			       (gir::gtype-of $test-obj))))

(cffi:defcallback handle-test-cb :boolean
    ((obj :pointer)
     (arg :pointer)
     (user-data :pointer))
  (g-message "handling signal test: ~S"
	     (list obj arg user-data))
  t)

(defvar $hid (gir:connect $test-obj "test" (cffi:callback handle-test-cb)))

(defvar $v1 (gir::build-struct-ptr (gir:nget *gobject* "Value")
				      (gir::make-gvalue 80 $test-obj)))

(defvar $dummy-object (gir::gobject-new 80))

(defvar $a1 (gir::build-struct-ptr (gir:nget *gobject* "Value")
				   (gir::make-gvalue 80 $dummy-object)))


(defvar $retp (cffi:foreign-alloc :boolean))

;; calling g_signal_emit through cffi works
#+nil
(cffi:foreign-funcall
 "g_signal_emit"
 :pointer (this-of $test-obj)		;instance
 :uint $sig-id ;signal-id
 :int32 0			;gquark detail
 :pointer (this-of $dummy-object)
 :pointer $retp
 :void)

#+nil
(cffi:mem-ref $retp :boolean)


;; calling g_signal_emitv through cffi works
(defvar $instance-and-params
  (cffi:foreign-alloc '(:struct gir::g-value-struct) :count 2))

(cffi:foreign-funcall "memset"
		      :pointer $instance-and-params
		      :int 0
		      :int (* 2 (cffi:foreign-type-size '(:struct gir::g-value-struct))))

(progn
(gir::g-value-init
 (cffi:mem-aptr $instance-and-params '(:struct gir::g-value-struct)  0)
 80)
(gir::g-value-set-object
 (cffi:mem-aptr $instance-and-params '(:struct gir::g-value-struct)  0)
 (this-of $test-obj))
(gir::g-value-init
 (cffi:mem-aptr $instance-and-params '(:struct gir::g-value-struct)  1)
 80)
(gir::g-value-set-object
 (cffi:mem-aptr $instance-and-params '(:struct gir::g-value-struct)  1)
 (this-of $dummy-object)))

(defvar $ret
  (gir::build-struct-ptr (gir:nget *gobject* "Value")
			 (gir::make-gvalue (gir::%gtype :boolean) t)))

#+nil
(cffi:foreign-funcall
 "g_signal_emitv"
 :pointer $instance-and-params
 :uint $sig-id ;signal-id
 :int32 0			;gquark detail
 :pointer (this-of $ret)
 :void)

(= (gir:field $ret "g_type") 20)
(gir:invoke ($ret "get_boolean"))


#+nil
(gir:get-callable-desc *gobject* "signal_emitv")
;; wrong g-i! return_value is both in in-args and out-args

;; calling signal_emitv through gi fails
#+nil
(with-gtk-thread
(multiple-value-setq ($ret1 $ret2)
  (gir:invoke (*gobject* "signal_emitv")
	      (list $v1  $a1)
	      $sig-id
	      0 ;;detail
	      $ret)))



(emit-signal $test-obj "test" $dummy-object)

(defun handle-noarg-signal (&rest args)
  (g-message "Handling noarg_signal ~S" args)
  t)

(create-signal :itype $i-type :name "noarg_signal")
(gir:connect $test-obj "noarg_signal" #'handle-noarg-signal)
(emit-signal $test-obj "noarg_signal")
(gir:invoke (*gobject* "signal_lookup") "noarg_signal" $i-type)


;;; ----------------------------------------------------------------------
;;;
;;; properties
;;;

(defvar $test-value 0)

(defun my-class-set-property-lisp (test-obj prop-id valu param-spec)
  (g-message "test-obj-setter-lisp ~S" (list test-obj prop-id valu param-spec))
  (assert (= (gir:field (gir::build-object-ptr (gir:nget *gobject* "ParamSpec")
					       (this-of param-spec))
			"value_type")
	     (gir::%gtype :int)))
  (setq $test-value (gir:invoke (valu "get_int"))))

(defun my-class-get-property-lisp (test-obj prop-id valu param-spec)
  (g-message "test-obj-getter-lisp ~S" (list test-obj prop-id valu param-spec))
  #+nil
  (assert (= (gir:field (gir::build-object-ptr (gir:nget *gobject* "ParamSpec")
					       (this-of param-spec))
			"value_type")
	     (gir::%gtype :int)))
  #+nil
  (cffi:foreign-funcall "g_value_set_int"
			:pointer (this-of valu)
			:int $test-value
			:void)
  (gir:invoke (valu "set_int") $test-value))


(progn
(eval (gir::%generate-vfunc-callback-form $myclass-obj "get_property"))
(eval (gir::%generate-vfunc-callback-form $myclass-obj "set_property"))
)

(eval
 `(let ((my-class-class (this-of (this-of (gir::type-class-peek $i-type)))))
    ,(gir::%generate-class-init-override-1 $myclass-obj "get_property")
    ,(gir::%generate-class-init-override-1 $myclass-obj "set_property")))

(defvar $object-klass  (gir::build-struct-ptr
			(gir:nget *gobject* "ObjectClass")
			(this-of (gir::type-class-peek $i-type))
			))

#+nil
(defvar $ptr-1
      (cffi:foreign-funcall "g_param_spec_int"
			    :string "test"			;name
			    :string "test"			;nick
			    :string "test blurb"		;blurb
			    :int 0				;min
			    :int 1024			;max
			    :int 42			;default_value
			    :int (logior			;flags
				  (gir:nget *gobject* "ParamFlags" :readwrite))
			    :pointer
	      ))

#+nil
(gir:get-callable-desc  *gobject* "param_spec_int")

(defvar $prop1-param-spec
  (gir:invoke (*gobject* "param_spec_int")
	      "test" "test" "test blurb"
	      0 1024 42
	      (gir:nget *gobject* "ParamFlags" :readwrite))
  #+nil
  (gir::build-object-ptr (gir:nget *gobject* "ParamSpec") $ptr-1))

(gir:invoke ($object-klass "install_property")
	    1
	    $prop1-param-spec
	    )

(gir:property $test-obj  "test")

(setf (gir:property $test-obj "test") 14)
||#



;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defvar *destroyable-types* nil)

(defun has-destroy-signal (obj)
  (some (lambda (type) (typep obj type)) *destroyable-types*))

(defun register-destroyable-type (gtype)
  (if (not (gir:invoke (*gobject* "type_is_a") gtype (gir:%gtype :object)))
      (error "gtype ~D not a GObject subclass" gtype))
  (if (not (gir:invoke (*gobject* "signal-lookup") "destroy" gtype))
      (error "gtype ~D does nor have a destroy signal" gtype))
  (pushnew *destroyable-types* gtype))

(defstruct (signal-tracker (:constructor %make-signal-tracker))
  owner-destroy-id owner map)

(defun make-signal-tracker (owner)
  (let ((ret (%make-signal-tracker :owner owner :map (make-hash-table))))
    (with-slots (owner owner-destroy-id map) ret
      (when (has-destroy-signal owner)
	(setq owner-destroy-id (gir:connect owner "destroy" (lambda () (clrhash map)) :after t))))))


;; implict signal-manager map of gobject-addresses -> signal-tracker objects
(defvar *signal-trackers* (make-hash-table))

(defun signal-manager-get-signal-tracker (obj)
  (or (gethash (cffi:pointer-address (gir:this-of obj))
	       *signal-trackers*)
      (setf (gethash (cffi:pointer-address (gir:this-of obj)) *signal-trackers*)
	    (make-signal-tracker obj))))

(defun signal-manager-maybe-get-signal-tracker (obj)
  (or (gethash (cffi:pointer-address (gir:this-of obj))
	       *signal-trackers*)))

(defun signal-manager-remove-signal-tracker (obj)
  (remhash  (cffi:pointer-address (gir:this-of obj)) *signal-trackers*))

(defun shutdown-signal-manager ()
  (maphash (lambda (key tracker)
	     (declare (ignore key))
	     (signal-tracker-destroy tracker))
	   *signal-trackers*)
  (signal-tracker-clear *signal-trackers*))

(defstruct signal-data
  owner-signals			    ;list of handler ids
  destroy-id)			    ;destroy handler id of tracked obj

(defun signal-tracker-get-signal-data (self obj)
  (with-slots (map) self
    (or (gethash (cffi:pointer-address (gir:this-of obj)) map)
	(setf (gethash  (cffi:pointer-address (gir:this-of obj)) map)
	      (make-signal-data :owner-signals nil :destroy-id 0)))))

(defun signal-tracker-remove-tracker (self)
  (with-slots (owner-destroy-id owner) self
    (if owner-destroy-id
	(signal-tracker-disconnect-signal self owner owner-destroy-id))
    (signal-manager-remove-signal-tracker owner)
    (setq owner-destroy-id nil)
    (setq owner nil)))

(defun signal-tracker-track (self obj handlerids)
  (if (has-destroy-signal obj)
      (signal-tracker-track-destroy self obj))
  (let ((data (signal-tracker-get-signal-data self obj)))
    (setf (signal-data-owner-signals data)
	  (nconc (signal-data-owner-signals data) handlerids))))

(defun signal-tracker-disconnect-signal (self obj id)
  (declare (ignore self))
  (gir:disconnect obj id))

(defun signal-tracker-untrack (self obj)
  (with-slots (owner map) self
    (remhash (cffi:pointer-address (gir:this-of obj)) map)
    (let ((signal-data (signal-tracker-get-signal-data self obj)))
      (with-slots (owner-signals destroy-id) signal-data
	(loop for id in owner-signals do
	      (gir:disconnect owner id))
	(if destroy-id
	    (gir:disconnect obj destroy-id))
	(if (zerop (hash-table-count map))
	    (signal-tracker-remove-tracker self))))))

(defun signal-tracker-clear (self)
  (maphash (lambda (key obj)
	     (declare (ignore key))
	     (signal-tracker-untrack self obj))
	   (signal-tracker-map self)))

(defun signal-tracker-track-destroy (self obj)
  (let ((signal-data (signal-tracker-get-signal-data self obj)))
    (unless (zerop (signal-data-destroy-id signal-data))
      (setf (signal-data-destroy-id signal-data)
	    (gir:connect obj "destroy"
			 (lambda () (signal-tracker-untrack self obj)))))))

(defun signal-tracker-destroy (self)
  (signal-tracker-clear self)
  (signal-tracker-remove-tracker self))

;; global
(defun connect-object (this-obj obj signal-name handler &key after)
  (let* ((signal-tracker (signal-manager-get-signal-tracker this-obj))
	 (id (gir:connect this-obj signal-name handler :after after)))
    (signal-tracker-track signal-tracker obj (list id))))

(defun disconnect-object (this-obj obj)
  (let ((signal-tracker (signal-manager-maybe-get-signal-tracker this-obj)))
    (when signal-tracker
      (signal-tracker-untrack signal-tracker obj))))
