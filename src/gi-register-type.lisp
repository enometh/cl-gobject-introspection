(in-package "GIR")

(cffi:defcstruct GTypeInfo
  ;; interface types, classed types, instantiated types
  (class-size :uint16)
  (base-init :pointer)			;GBaseInitFunc
  (base-finalize :pointer)		;GBaseFinalizeFunc
  ;; interface types, classed types, instantiated types
  (class-init :pointer)			;GClassInitFunc
  (class-finalize :pointer)		;GClassFinalizeFunc
  (class-data :pointer)
  ;; instantiated types
  (instance-size :uint16)
  (n-preallocs :uint16)
  (instance-init :pointer)		;GInstanceInitFunc
  ;;  value handling
  (value-table :pointer)		;GTypeValueTable*
  )

#+nil
(cffi:foreign-type-size '(:struct gtypeinfo))	;72

(defvar *registered-types* (make-hash-table :test #'equal))

(defun register-type (type-name parent-gtype type-flags &key force
		      instance-struct-name class-struct-name
		      class-size base-init base-finalize
		      class-init class-finalize class-data
		      instance-size n-preallocs instance-init
		      value-table
		      )
  (let ((ret (gethash type-name *registered-types*)))
    (and ret (not force) (return-from register-type ret)))
  (check-type parent-gtype integer)	;ulong

  #+nil
  (unless force				; doesn't work
    (assert (invoke (*gobject* "type_class_peek") parent-gtype) nil
	"Parent gtype not defined.")
    (assert (zerop (invoke (*gobject* "type_from_name") type-name)) nil
	"Typename already registered."))

  (unless instance-struct-name
    (setq instance-struct-name (intern (string type-name))))

  (unless class-struct-name
    (setq class-struct-name (intern
			     (concatenate 'string (string instance-struct-name)
					  "-CLASS"))))
  (unless class-size
    (setq class-size (cffi:foreign-type-size
		      (list :struct class-struct-name))))

  (unless instance-size
    (setq instance-size (cffi:foreign-type-size
			 (list :struct instance-struct-name))))

  (unless type-flags (setq type-flags 0))
  (unless n-preallocs (setq n-preallocs 0))

  (cffi:with-foreign-object (type-info '(:struct gtypeinfo))
    (cffi:foreign-funcall "memset" :pointer type-info
			  :int 0
			  :int (cffi:foreign-type-size '(:struct gtypeinfo)))

    ;; initialize the other fields of GTypeInfo
    (macrolet ((foo (field)
		 `(when ,field
		    (setf (cffi:foreign-slot-value type-info
						   '(:struct gtypeinfo)
						   ',field)
			  ,field))))
      (macrolet ((bar (&rest fields)
		   `(progn ,@(loop for field in fields
				   collect `(foo ,field)))))
	(bar
      	 class-size base-init base-finalize
	 class-init class-finalize class-data
	 instance-size n-preallocs instance-init
	 value-table)))

    (let ((gtype (cffi:foreign-funcall
		  "g_type_register_static"
		  ;; parent-type
		  :ulong parent-gtype
		  ;; type-name
		  :string type-name
		  ;; type-info
		  :pointer type-info
		  ;; type-flags
		  :int type-flags
		  ;; return-type
		  :ulong
		  )))
      (assert (not (zerop gtype)) nil "Failed to register type")
      (setf (gethash type-name *registered-types*) gtype)
      gtype)))


(export '(*registered-types* register-type))