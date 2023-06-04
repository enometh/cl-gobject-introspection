(in-package "GIR")

(cffi:defcfun g-type-name :string (gtype :ulong))

(defun gtype-of (obj)
  (etypecase obj
    (integer obj)
    (object-instance (gtype (this-of obj)))
    (object-info
     (eval `(cffi:foreign-funcall ,(coerce (object-info-get-type-init obj)
					   'base-string)
				  :ulong)))
    (object-class (gtype-of (info-of obj)))
    (interface-desc (gtype-of (info-of obj)))
    (interface-info
     (registered-type-info-get-g-type obj))))

;;(invoke (*gobject* "type_interfaces") gtype)
(defun g-type-interfaces (gtype)
  (cffi:with-foreign-objects ((n :int))
    (let* ((ptr (cffi:foreign-funcall "g_type_interfaces"
				      :ulong gtype
				      :pointer n
				      :pointer))
	   (ret (loop for i below (cffi:mem-ref n :int)
		      collect (cffi:mem-aref ptr :ulong i))))
      #-(or ecl mkcl)
      (cffi:foreign-free ptr)
      ret)))

;; an alternative to object-class-find-function-info
(defun find-method-recursive (object-gtype cname)
  "Search for method CNAME in the object, it's interfaces,
it's parent and parent's interfaces.  Return the function-info.
Second return value is the object-info or interface-info of the object
or interface which implements the method."
  (labels ((rec (object-gtype cname)
	     (let ((object-info (repository-find-by-gtype nil object-gtype)))
	       (when object-info
		 (multiple-value-bind (info implementor)
		     (object-info-find-method-using-interfaces object-info
							       cname)
		   (if info
		       (values info implementor)
		       (let ((parent-gtype (g-type-parent object-gtype)))
			 (unless (zerop parent-gtype)
			   (rec parent-gtype cname)))))))))
    (rec object-gtype cname)))

;; an alternative to object-class-find-vfunc-info
(defun find-vfunc-recursive (object-gtype cname)
  "Search for vfunc CNAME in the object, it's interfaces,
it's parent and parent's interfaces.  Return the function-info.
Second return value is the object-info or interface-info of the object
or interface which implements the method."
  (labels ((rec (object-gtype cname)
	     (let ((object-info (repository-find-by-gtype nil object-gtype)))
	       (when object-info
		 (multiple-value-bind (info implementor)
		     (object-info-find-vfunc-using-interfaces object-info
							      cname)
		   (if info
		       (values info implementor)
		       (let ((parent-gtype (g-type-parent object-gtype)))
			 (unless (zerop parent-gtype)
			   (rec parent-gtype cname)))))))))
    (rec object-gtype cname)))

(defmethod find-method-recursive-2 (gtype cname)
  (let ((parent-gtype gtype) parent-info info)
    (loop (cond ((zerop parent-gtype) (return))
		((and (setq parent-info
			    (repository-find-by-gtype nil parent-gtype))
		      (setq info (object-class-find-method-function-info
				  (find-build-interface parent-info)
				  cname)))
		 (return))
		(t (setq parent-gtype (g-type-parent parent-gtype)))))
    (and info (values info parent-gtype))))

(defmethod find-vfunc-recursive-2 (gtype cname)
  (let ((parent-gtype gtype) parent-info info)
    (loop (cond ((zerop parent-gtype) (return))
		((and (setq parent-info
			    (repository-find-by-gtype nil parent-gtype))
		      (setq info (object-class-find-vfunc-info
				  (find-build-interface parent-info)
				  cname)))
		 (return))
		(t (setq parent-gtype (g-type-parent parent-gtype)))))
    (and info (values info parent-gtype))))

(defun find-vfunc-offset-recursive (gtype cname)
  (multiple-value-bind (function-info implementor-info)
      (find-vfunc-recursive gtype cname)
    (declare (ignorable function-info))
    (when implementor-info
      (let* ((struct-info (object-info-get-class-struct implementor-info))
	     (struct-class (build-interface struct-info)))
	(find-class-struct-offset struct-class cname)))))

(defun %gobject (ptr)
  (let ((gtype (cffi:mem-ref (cffi:mem-ref ptr :pointer) :ulong)))
    (gobject gtype ptr)))

(defun fields (obj)
  (let ((fields (list-fields-desc
		 (etypecase obj
		   (object-instance (gir-class-of obj))
		   (struct-instance  (struct-class-of obj))))))
    (when fields
      (loop for field in fields
	    collect (list field
			  (field obj (slot-value field 'gir::name)))))))

(defun list-props (obj &optional (object-class (gir-class-of obj)))
  (loop for desc in  (list-properties-desc object-class)
	 for name = (name-of desc)
	 collect (cons name (property obj name))))

(defun g-type-parents (gtype &optional (depth 0))
  (let ((parent-gtype (g-type-parent gtype)))
    (if (zerop parent-gtype)
	nil
	(cons parent-gtype (g-type-parents parent-gtype (1+ depth))))))

(defun list-all-props (obj)
  (let* ((gtype (gtype-of obj)))
    (append (list-props obj)
	    (loop for intf-gtype in (g-type-interfaces gtype)
		  append (list-props obj (find-build-interface  (repository-find-by-gtype nil intf-gtype))))
	    (loop for parent-gtype in (g-type-parents gtype)
		  append (list-props obj (find-build-interface (repository-find-by-gtype nil parent-gtype)))))))

(defun list-property-names (object-class)
  (mapcar 'name-of (list-properties-desc object-class)))

(export '(type-class-peek get-vfunc-function-pointer))
(defun type-class-peek (gtype)
  (or (invoke ((require-namespace "GObject") "type_class_peek") gtype)
      (invoke ((require-namespace "GObject") "type_class_ref") gtype)))

(defun get-vfunc-function-pointer (object-class cname)
  (cffi:mem-ref
   (this-of (type-class-peek (gtype-of object-class)))
   :pointer
   (find-vfunc-offset-recursive (gtype-of object-class) cname)))

(defun get-private-ptr (obj-instance &optional (type-class nil type-class-supplied-p))
  (let* ((obj-ptr (this-of obj-instance))
	 (type-class-ptr (cond ((not type-class-supplied-p)
				(cffi:mem-ref obj-ptr :pointer))
			       ((typep type-class 'cffi:foreign-pointer)
				type-class)
			       (t	;fixme call g-type-class-peek
				(gir::this-of
				 (type-class-peek (gtype-of type-class)))))))
    (cffi:inc-pointer obj-ptr
		      (cffi:foreign-funcall "g_type_class_get_instance_private_offset"
			:pointer type-class-ptr
			:int))))

(export '(get-private-ptr))
