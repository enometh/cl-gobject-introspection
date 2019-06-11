(in-package "GIR")

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
