(in-package "GIR")

(cffi::defcstruct g-type-query ; "GTypeQuery"
  (type :ulong)
  (type-name :string)
  (class-size :unsigned-int)
  (instance-size :unsigned-int))

(defun g-type-query (object query-type)
  "Object can be a gtype (ulong), an object-info or an
object-class. query-type is a keyword designating the field of
interest in the g-type-query struct. (If it is nil, just return the
gtype.)"
  (let* ((info (etypecase object
		 (integer (repository-find-by-gtype nil object))
		 (registered-type-info object)
		 (gir::object-class (gir::info-of object))))
	 (gtype (registered-type-info-get-g-type info)))
    (if (not query-type)
	gtype
	(let ((slot-name (ecase query-type
			   ((nil) nil)
			   (:ulong 'type)
			   (:name 'type-name)
			   (:class-size 'class-size)
			   (:instance-size 'instance-size))))
	  (cffi:with-foreign-objects ((a '(:struct g-type-query)))
	    (cffi:foreign-funcall "g_type_query"
				  :ulong gtype
				  :pointer a
				  :void)
	    (cffi:foreign-slot-value a '(:struct g-type-query) slot-name))))))

(export '(g-type-query))