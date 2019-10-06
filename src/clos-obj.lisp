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

;; return a symbol denoting the class-name. recursively define classes
;; for thing. thing is an object-class or a struct-class or an
;; interface-desc. The defined class is an instance of the
;; instance-wrapper-class metaclass

(defun %ensure-class (thing &key force-p)
  (check-type thing (or interface-desc object-class struct-class))
  (let* ((info (info-of thing))
	 (string-name (info-get-name info))
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
	 (string-name (info-get-name info))
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
(%ensure-class (nget *gtk* "Window") :force-p t)
(%define-methods (nget *gtk* "Window"))
(setq $win (new 'window (nget *gtk* "WindowType" :toplevel)))
(gir-lib:with-gtk-thread (show-all $win))
||#

