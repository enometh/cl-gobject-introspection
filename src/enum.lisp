(in-package :gir)

(cffi:defcfun g-value-info-get-value :int64 (info info-ffi))

(defclass enum-desc ()
  ((name :reader name-of)
   (values-dict :reader values-of)
   methods-dict))

(defmethod print-object ((enum-desc enum-desc) s)
  (format s "#E<~a>" (name-of enum-desc)))

(defmethod shared-initialize :after ((enum-desc enum-desc) slot-names
				     &key enum-info)
  (declare (ignore slot-names))
 (when enum-info
  (with-slots (name values-dict methods-dict)
      enum-desc
    (setf name
	  (info-get-name enum-info)
	  values-dict
	  (iter (for i below (g-enum-info-get-n-values enum-info))
		(let ((value-info (g-enum-info-get-value enum-info i)))
		  (collect (cons (info-get-name value-info)
				 (g-value-info-get-value value-info)))))
	  methods-dict
	  (iter (for i below (g-enum-info-get-n-methods enum-info))
		(let ((func-info (g-enum-info-get-method enum-info i)))
                  (collect (cons (info-get-name func-info)
                                 (build-function func-info)))))))))

(defmethod build-interface-desc ((enum-info enum-info))
  (make-instance 'enum-desc :enum-info enum-info))

(defmethod nslist-desc ((enum-desc enum-desc))
  (values-of enum-desc))

(defun build-enum (enum-desc)
  (with-slots (values-dict methods-dict)
      enum-desc
    (lambda (name)
      (let ((name* (c-name name)))
        (if (keywordp name)
            (cdr (or (assoc name* values-dict :test #'string=)
                     (error "Should be FFI enum value name: ~a" name)))
            (cdr (or (assoc name* methods-dict :test #'string=)
		     (error "Should be FFI method name: ~a" name))))))))

(defmethod build-interface ((info enum-info))
  (build-enum (build-interface-desc info)))

;; should be enum-value-to-strings?
(defun enum-value-to-string (namespace enum-name value)
  (let (list)
    (or (car (rassoc value (setq list (nlist-desc namespace enum-name))))
	(loop for (name . target) in list
	      if (not (zerop (logand value target))) collect name))))

(define-compiler-macro nget (&whole form namespace &rest names)
  (declare (ignorable namespace))
  (if (keywordp (cadr names))
      `(load-time-value (funcall (nsget ,namespace ,(car names)) ,(cadr names)))
      form))

#+nil
(funcall (compiler-macro-function 'nget)
	 '(nget gir-lib:*gtk* "WindowType" :toplevel)
         nil)

(defun %define-cffi-enum (cffi-enum-name-and-options namespace enum-name)
  `(cffi:defcenum ,cffi-enum-name-and-options
     ,@(loop for (c-name . number) in (gir::values-of (gir::nget-desc namespace enum-name))
	     collect (list (intern (substitute #\- #\_ (string-upcase c-name)) :keyword)
			   number))))

#+nil
(%define-cffi-enum 'option-arg gir-test::*glib* "OptionArg")
