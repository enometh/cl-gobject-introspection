(in-package :gir)

(eval-when (load eval compile)
(shadow '("UNION") :gir))

(defstruct
    (union
      (:constructor make-union (class this)))
  class
  this)

(defclass union-instance ()
  ((class :initarg :class :reader union-class-of)
   (this :initarg :this :accessor this-of)))

(defmethod print-object ((object union-instance) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((info (info-of (union-class-of object))))
      (format stream "~A.~A" (info-get-namespace info)
	      (info-get-name info)))))

(defclass union-class ()
  ((info :initarg :info :reader info-of)
   signals				; XXX REMOVE (also for struct)
   (fields-dict :reader fields-dict-of)
   (constructor-cache :reader constructor-cache-of) ;XXX REMOVE
   (method-cache :reader method-cache-of)))

(defmethod print-object ((union-class union-class) s)
  (format s "#U<~a>" (info-get-name (info-of union-class))))

(defmethod shared-initialize :after ((union-class union-class) slot-names
				     &key)
  (declare (ignore slot-names))
  (with-slots (info signals fields-dict constructor-cache method-cache)
      union-class
    (setf signals (list nil)
	  fields-dict (iter (for field-info :in (union-info-get-fields info))
			    (collect (cons (info-get-name field-info) field-info)))
	  constructor-cache (make-hash-table :test #'equal)
	  method-cache (make-hash-table :test #'equal))))

(defmethod build-interface ((info union-info))
  (make-instance 'union-class :info info))

(defun %allocate-union (union-class)
  (let* ((info (info-of union-class))
	 (size (union-info-get-size info)))
    (cffi:foreign-alloc :int8 :initial-element 0 :count size)))

(defun allocate-union (union-class)
  (build-union-ptr union-class (%allocate-union union-class)))

(defun union-class-get-constructor-info (union-class name)
  (let* ((info (info-of union-class))
	 (function-info (union-info-find-method info (c-name name))))
    (if (and function-info
	     (constructor? (function-info-get-flags function-info)))
	function-info
	(error "Bad FFI constructor name ~a" name))))

(defun union-class-build-constructor (union-class name)
  (build-function (union-class-get-constructor-info union-class name)
		  :return-interface (info-of union-class)))

(defun union-class-get-method-info (union-class name)
  (let* ((info (info-of union-class))
	 (function-info (union-info-find-method info (c-name name)))
	 (flags (if function-info (function-info-get-flags function-info))))
    (if (and function-info (method? flags))
	function-info
	(error "Bad FFI method name ~a" name))))

(defun union-class-build-method (union-class name)
  (build-function (union-class-get-method-info union-class name)))

(defun build-union-ptr (union-class this)
  (make-instance 'union-instance :class union-class :this this))

(defun union-class-find-field (union-class name)
  (let ((fields-dict (fields-dict-of union-class)))
    (cdr (or (assoc (c-name name) fields-dict :test #'string=)
	     (error "Bad FFI field name ~a" name)))))

(defmethod nsget ((union-class union-class) name)
  (let* ((constructor-cache (constructor-cache-of union-class))
	 (cname (c-name name)))
    (ensure-gethash cname constructor-cache
		    (union-class-build-constructor union-class cname))))

(defmethod field ((union union-instance) name)
  (let* ((union-class (union-class-of union))
	 (field-info (union-class-find-field union-class name)))
    (gir.field:get (this-of union) field-info)))

(defmethod set-field! ((union union-instance) name value)
  (let* ((union-class (union-class-of union))
	 (field-info (union-class-find-field union-class name)))
    (gir.field:set (this-of union) field-info value))
  value)

;; defined in struct.lisp
;;(defun (setf field) (value union name)
;;  (set-field! union name value))

(defun free-union (union)
  (let ((this (this-of union)))
    (if (cffi:null-pointer-p this)
	(error "Double free")
	(progn (cffi:foreign-free this)
	       (setf (this-of union) (cffi:null-pointer))))))

(defmethod nsget ((union union-instance) name)
  (let* ((union-class (union-class-of union))
	 (method-cache (method-cache-of union-class))
	 (cname (c-name name))
	 (method (ensure-gethash cname method-cache
				 (union-class-build-method union-class cname))))
    (lambda (&rest args)
      (apply method (cons (this-of union) args)))))

(defmethod nsget-desc ((union-class union-class) name)
  (build-callable-desc (union-class-get-constructor-info union-class name)
		       :return-interface (info-of union-class)))

(defmethod list-fields-desc ((union-class union-class))
  (let ((fields-dict (fields-dict-of union-class)))
    (iter (for (name . field-info) :in fields-dict)
	  (collect (build-variable-desc name (field-info-get-type field-info))))))

(defmethod get-field-desc ((union-class union-class) name)
  (let* ((cname (c-name name))
	 (field-info (union-class-find-field union-class cname)))
    (build-variable-desc cname (field-info-get-type field-info))))

(defmethod list-methods-desc ((union-class union-class))
  (let ((info (info-of union-class)))
    (iter (for method-info :in (union-info-get-methods info))
	  (when (method? (function-info-get-flags method-info))
	    (collect (build-callable-desc method-info))))))

(defmethod get-method-desc ((union-class union-class) name)
  (build-callable-desc (union-class-get-method-info union-class
						     (c-name name))))

(defmethod list-constructors-desc ((union-class union-class))
  (let ((info (info-of union-class)))
    (iter (for method-info :in (union-info-get-methods info))
	  (when (constructor? (function-info-get-flags method-info))
	    (collect (build-callable-desc method-info))))))

(defmethod get-constructor-desc ((union-class union-class) name)
  (when-let ((info (union-class-get-constructor-info union-class name)))
    (build-callable-desc info  :return-interface (info-of union-class))))

#||
(defvar $c-mutex (cffi:foreign-funcall "g_mutex_new" :pointer))
(defvar $Mutex (gir::build-union-ptr (gir:nget *glib* "Mutex") $c-mutex))
(gir:invoke ($mutex "init"))
||#
