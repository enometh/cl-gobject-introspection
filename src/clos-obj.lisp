(in-package "GIR")
(export '(%ensure-class %define-methods))

(eval-when (load eval compile)
(defclass instance-wrapper-class (standard-class) ())
(eval-1 (%build-validate-superclass-forms 'instance-wrapper-class)))

#||
(defclass instance-wrapper-object (standard-object) ())

;; ensure all instance-wrapper-class classes inherit from
;; instance-wrapper-object
(defmethod initialize-instance :around
    ((class instance-wrapper-class) &rest initargs
     &key direct-superclasses &allow-other-keys)
  (declare (dynamic-extent initargs))
  (if (loop for direct-superclass in direct-superclasses
            thereis (ignore-errors
                      (subtypep direct-superclass
                                (find-class 'instance-wrapper-object))))
      (call-next-method)
      (apply #'call-next-method
             class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'instance-wrapper-object)))
             initargs)))

(defmethod reinitialize-instance :around
    ((class instance-wrapper-class) &rest initargs
     &key (direct-superclasses '() direct-superclasses-p)
     &allow-other-keys)
  (declare (dynamic-extent initargs))
  (if (or (not direct-superclasses-p)
          (loop for direct-superclass in direct-superclasses
                thereis (ignore-errors
                          (subtypep direct-superclass
                                    (find-class 'instance-wrapper-object)))))
      (call-next-method)
      (apply #'call-next-method
             class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'instance-wrapper-object)))
             initargs)))
||#

(defclass struct-instance-wrapper (struct-instance)
  ()
  (:metaclass instance-wrapper-class))

(defclass object-instance-wrapper (object-instance)
  ()
  (:metaclass instance-wrapper-class))

(defclass interface-base-class ()
  ()
  (:metaclass instance-wrapper-class))

(dolist (class '(object-instance-wrapper
		 struct-instance-wrapper
		 interface-base-class))
  (eval-1 (%build-validate-superclass-forms class)))

(defvar *class-to-object-class-hash* (make-hash-table))

(defun %string-name (info)
  (let* ((thing-name (info-get-name info))
	 (namespace-name (info-get-namespace info))
	 (string-name (concatenate 'string namespace-name "." thing-name)))
    string-name))

;; return a symbol denoting the class-name. recursively define classes
;; for thing. thing is an object-class or a struct-class or an
;; interface-desc. The defined class is an instance of the
;; instance-wrapper-class metaclass

(defun %ensure-class (thing &key force-p)
  (check-type thing (or interface-desc object-class struct-class))
  (let* ((info (info-of thing))
	 (string-name (%string-name info))
	 (name (intern-1 string-name *package*))
	 (ret (find-class name nil)))
    (when (and ret force-p)
      ;;(format t "<<<~S~&" `(setf ,ret nil))
      ;;(setf (gethash name *class-to-object-class-hash*) nil)
      (setf (find-class name) nil)
      (setq ret nil))
    (unless ret
      (let ((superclasses
	     (etypecase thing
	       (interface-desc '(interface-base-class))
	       (struct-class '(struct-instance-wrapper))
	       (object-class
		(let* ((intfs (mapcar 'build-interface
				      (interface-infos-of thing)))
		       (gir-parent (parent-of thing))
		       (parent (if gir-parent
				   (%ensure-class gir-parent :force-p force-p)
				   'object-instance-wrapper)))
		  (setf (gethash name *class-to-object-class-hash*) thing)
		  (cons parent (mapcar #'(lambda (x)
					   (%ensure-class x :force-p force-p))
				       intfs)))))))
	(setq ret (eval-1 `(defclass ,name ,superclasses
			     ((thing :initform  '(:thing ,thing)))
			     (:metaclass instance-wrapper-class))))))
    (class-name ret)))


(defun %argument-type (type-desc)
  (etypecase type-desc
    (symbol (ecase type-desc
	      (string type-desc)
	      (integer type-desc)
	      ((:pointer boolean real) t)
	      ))
    ((or struct-class object-class interface-desc)
     (%ensure-class type-desc))
    (enum-desc t)
    (cons (ecase (car type-desc)
	    ((sequence) 'sequence)
	    ((or) t)))
    ))

(defun %build-method-argument (variable-desc)
  (list (intern-1 (name-of variable-desc) *package*)
	(%argument-type (type-desc-of variable-desc))))

(defun %build-method-arguments (callable-desc)
  (mapcar #'%build-method-argument (arguments-desc-of callable-desc)))

(defun %build-method-form (thing callable-desc &optional constructor-p)
  (let* ((info (slot-value callable-desc 'info))
	 (string-name (%string-name info))
	 (name (intern-1 string-name *package*))
	 (args (%build-method-arguments callable-desc))
	 (self  (%ensure-class thing))
	 (doc (prin1-to-string callable-desc)))
    ;; constructor methods syntax:
    ;; (<constructor-name> '<class-name> ...args)
    (if constructor-p
	`(defmethod ,name ((self (eql ',self)) ,@args)
	   ,doc
	   (let ((object-class (gethash self *class-to-object-class-hash*)))
	     (change-class
	      (gir:invoke (object-class ,string-name) ,@(mapcar #'car args))
	      self)))
	`(defmethod ,name ((self ,self) ,@args)
	   ,doc
	   (gir:invoke (self ,string-name) ,@(mapcar #'car args))))))

;; ;madhu 210718 this doesn't "scale" at all. it might be possible to
;; salvage ensure-class though, and have a means to define foreign
;; gobject subclasses
(defun %define-methods (thing)
  (%ensure-class thing)
  (when (typep thing 'object-class)
    (when (parent-of thing)
      (%define-methods (parent-of thing)))
    (dolist (interface (mapcar #'build-interface (interface-infos-of thing)))
      (%define-methods interface))
    (dolist (constructor (list-constructors-desc thing))
      (eval-1 (%build-method-form thing constructor t)))
  (dolist (method (list-methods-desc thing))
    (eval-1 (%build-method-form thing method nil)))))

#||
(mapcar 'shadow '(close atom map remove))
(import 'gir-test::*gtk*)
(%ensure-class (nget gir-test::*gtk* "Window") :force-p t)
*class-to-object-class-hash*
(mapcar  (lambda (callable-desc)
	   (%build-method-form (nget *gtk* "Window") callable-desc))
	 (list-methods-desc (nget *gtk* "Window")))
(mapcar  (lambda (callable-desc)
	   (%build-method-form (nget *gtk* "Window") callable-desc t))
	 (list-constructors-desc (nget *gtk* "Window")))
(%define-methods (nget *gtk* "Window"))
(setq $win (new 'window (nget *gtk* "WindowType" :toplevel)))
(gir-lib:with-gtk-thread (show-all $win))
||#


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(export 'gobject-new)

(defun gobject-new (object-gtype &rest key-val-pairs)
  (let* ((n-properties (/ (length key-val-pairs) 2))
	 (names (progn (assert (integerp n-properties))
		       (cffi:foreign-alloc :string :count n-properties)))
	 (values
	  (cffi:foreign-alloc '(:struct g-value-struct) :count n-properties))
	 (object-class-ptr
	  (let ((object-class-ptr
		 (cffi:foreign-funcall "g_type_class_peek" :ulong object-gtype
				       :pointer)))
	    (if (cffi:null-pointer-p object-class-ptr)
		 (cffi:foreign-funcall "g_type_class_ref"
				       :ulong object-gtype
				       :pointer)
		 object-class-ptr)))
	 ret)
    (format *standard-output* "CTVM: G-OBJECT-NEW: OBJECT-CLASS-PTR=~S~&" object-class-ptr)
    (finish-output *standard-output*)
    (assert (not (cffi:null-pointer-p object-class-ptr)))
    (loop for i from 0
	  for (prop-name lisp-val) on key-val-pairs by #'cddr
	  for g-param-spec = (g-object-class-find-property
			      object-class-ptr prop-name)
	  for prop-gtype = (cffi:foreign-slot-value
			    g-param-spec
			    '(:struct g-param-spec) 'spec-type)
	  for gvalue-ptr = (make-gvalue prop-gtype lisp-val)
	  do
	  (format *standard-output* "CTVM: G-OBJECT-NEW: SETTING PROPERTY NUMBER ~D~&" i)
	  (finish-output *standard-output*)

	  (setf (cffi:mem-aref names :string i)
		prop-name)
	  (setf (cffi:mem-aref values '(:struct g-value-struct) i)
		(cffi:mem-ref		;ugh
		 gvalue-ptr '(:struct g-value-struct)))
	  (cffi:foreign-free gvalue-ptr)
	  finally
	  (format *standard-output* "CTVM: G-OBJECT-NEW: CALLING NEW_WITH_PROPERTIES ~D ~S~&" n-properties (list names values))
	  (finish-output *standard-output*)
	  (setq ret (cffi:foreign-funcall
			"g_object_new_with_properties"
		      :ulong object-gtype
		      :uint n-properties
		      :pointer names
		      :pointer values
		      :pointer)))
    (format *standard-output* "CTVM: G-OBJECT-NEW: GOT ~S~&" ret)
    (finish-output *standard-output*)
    (cffi:foreign-free names)
    (cffi:foreign-free values)
    (format *standard-output* "CTVM: G-OBJECT-NEW: CALLING GOBJECT ~S~&" ret)
    (finish-output *standard-output*)
    (let ((objptr (gobject object-gtype ret)))
      (format *standard-output* "CTVM: G-OBJECT-NEW: GOBJECT RETURNED ~S~&"
	      objptr)
      (finish-output *standard-output*)
      objptr)))

#||
(list-property-names (nget gir-test::*gio* "Application"))
(setq $a0 (gobject-new (gtype-of (nget gir-test::*gio* "Application"))
		       "application-id"  "org.robolove.dummy"
		       "flags" 0))
(list-all-props $a0)
(%ensure-class (nget gir-test::*gio* "Application") :force-p t)
(gethash 'Application *class-to-object-class-hash*)
(setq $a1 (change-class $a0 'application))
(eql $a1 $a0)
(list-all-props $a1)
||#