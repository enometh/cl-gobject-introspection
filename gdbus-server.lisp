;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package "GDBUS")

;;; ----------------------------------------------------------------------
;;;
;;; SERVICE SPECIFICATION IN LISP
;;;

;; method-handlers = alist (METHOD-NAME . LISP-FUNCTION)
;; property-handlers = alist (PROPERTY-NAME . LISP-FUNCTION)
;; notifiers = alist (SIGNAL-NAME . LISP-FUNCTION)
;;
;; when LISP-FUNCTION is called its first argument is a DBUS-SERVICE
;; object. Remaining arguments are according to the method or signal
;; signatures.
;;
;; property-handlers should also define a setter (SETF LISP-FUNCTION)
;; which would be called by HANDLE-SET-PROPERTY.

(defclass dbus-service (dbus-interface-info-mixin)
  ((regid :initform nil)
   (method-handlers :initform nil :initarg :method-handlers)
   (property-handlers :initform nil :initarg :property-handlers)
   (notifiers :initform nil :initarg :notifiers)))

(defun lookup-lisp-function (dbus-obj slot-name name)
  "SLOT is one of METHOD-HANDLERS PROPERTY-HANDLERS or NOTIFIERS."
  (cdr (assoc name (slot-value dbus-obj slot-name) :test #'equal)))

(defsetf lookup-lisp-function (dbus-obj slot-name name) (new-value)
  `(let ((elt (assoc ,name (slot-value ,dbus-obj ,slot-name) :test #'equal)))
     (cond (elt (if (eql ,new-value :delete)
		    (setf (slot-value ,dbus-obj ,slot-name)
			  (delete elt (slot-value ,dbus-obj ,slot-name)))
		    (setf (cdr elt) ,new-value)))
	   (t (setf (slot-value ,dbus-obj ,slot-name)
		    (cons ,name ,new-value))))))

(defun format-simple-condition (c)
  (check-type c simple-condition)
  (apply #'format nil
	 (simple-condition-format-control c)
	 (simple-condition-format-arguments c)))

(define-condition error-during-method-invocation (simple-error)
  ((error-domain :initarg :error-domain :reader error-domain :initform (invoke (*gio* "dbus_error_quark")) :type integer)
   (error-code  :initarg :error-code :reader error-code :initform (nget *gio* "DBusError" :failed) :type integer))
  (:report (lambda (self stream)
	     (format stream "~A:~A:~A" (error-domain self) (error-code self)
		     (format-simple-condition self)))))

(defun handle-method-call
    (bus sender object-path intf-name method-name parameters invocation)
  (format t "ss-handle-method-call ~S.~&" (list bus sender object-path intf-name method-name parameters invocation))
  (assert invocation nil "Expected invocation.")
  (let ((dbus-obj (lookup object-path intf-name)))
    (assert dbus-obj nil "Could not lookup object for given object-path and interface-name.")
    (multiple-value-bind (in-args-signatures out-args-signatures)
	(get-method-signature dbus-obj method-name)
      (let ((lisp-function
	     (lookup-lisp-function dbus-obj 'method-handlers method-name))
	    in-args out-args)
	(assert lisp-function nil "Could not lookup lisp method handler.")
	(assert (fdefinition lisp-function) nil "No binding for lisp method handler.")
	(setq in-args (convert-from-gvariant parameters in-args-signatures))
	(handler-case (progn
			(setq out-args
			      (multiple-value-list
			       (apply lisp-function dbus-obj in-args)))
			(gir:invoke (invocation "return_value")
				    (gir:convert-to-gvariant
				     out-args
				     out-args-signatures)))
	  (error-during-method-invocation (c)
	    (invoke (invocation "return_error_literal")
		    (error-domain c)
		    (error-code c)
		    (format-simple-condition c))))))))

(defun handle-get-property (bus sender object-path intf-name property-name)
  (format t "ss-handle-get-property ~S.~&" (list bus sender object-path intf-name property-name))
  (let ((dbus-obj (lookup object-path intf-name)))
    (assert dbus-obj nil "Could not lookup object for given object-path and interface-name.")
    (handler-case
	(let ((lisp-function
	       (lookup-lisp-function dbus-obj 'property-handlers property-name))
	      property-info property-signature out-args)
	  (assert lisp-function nil "Could not look up property reader.")
	  (assert (fdefinition  lisp-function) nil "property reader not bound.")
	  (with-slots (interface-info) dbus-obj
	    (setq property-info
		  (invoke (interface-info "lookup_property") property-name))
	    (assert property-info nil "Could not lookup gir property info.")
	    (setq property-signature
		  (get-property-signature dbus-obj property-name))
	    (assert property-signature nil "No signature in property info.")
	    (setq out-args (convert-to-gvariant
			    (funcall lisp-function dbus-obj)
			    property-signature))
	    (format t "XXX funcall over out-args=~S" out-args)
	    out-args))
      (error (c)
	(format t "Error encountered during handle-get-property: ")
	(typecase c
	  (simple-condition
	   (format t "~A~&" (format-simple-condition c)))
	  (t (format t "~S~&" c)))
	(cffi:null-pointer)))))

(defun handle-set-property (bus sender object-path intf-name property-name value)
  (format t "ss-handle-get-property ~S.~&" (list bus sender object-path intf-name property-name value))
  (let ((dbus-obj (lookup object-path intf-name)))
    (assert dbus-obj nil "Could not lookup object for given object-path and interface-name.")
    (handler-case
	(let ((lisp-function
	       (lookup-lisp-function dbus-obj 'property-handlers property-name))
	      property-info property-signature in-args)
	  (assert lisp-function nil "Could not look up property accessor.")
	  (assert (funcall 'fdefinition `(setf ,lisp-function)) nil
	      "Property accessor not bound.")
	  (with-slots (interface-info) dbus-obj
	    (setq property-info
		  (invoke (interface-info "lookup_property")
			  property-name))
	    (assert property-info nil "Could not lookup gir property info.")
	    (setq property-signature
		  (get-property-signature dbus-obj property-name))
	    (assert property-signature nil "No signature in property info.")
	    (setq in-args (convert-from-gvariant value property-signature))
	    (funcall (funcall 'fdefinition `(setf ,lisp-function))
		     in-args dbus-obj)
	    t))
      (error (c)
	(format t "Error encountered during handle-set-property: ")
	(typecase c
	  (simple-condition
	   (format t "~A~&" (format-simple-condition c)))
	  (t (format t "~S~&" c)))
	nil))))

(defun emit-signal (dbus-obj signal-name)
  (let ((lisp-function (lookup-lisp-function dbus-obj 'notifiers signal-name)))
    (assert lisp-function nil "Couldn't find notifier for signal.")
    (assert (fdefinition lisp-function) nil "Notifier for signal not fbound.")
    (with-slots (bus interface-info object-path interface-name) dbus-obj
      (let ((signal-info (invoke (interface-info "lookup_signal")
				 signal-name))
	    signature out-args out-variant-tuple)
	(assert signal-info nil "Signal not found.")
	(setq signature (get-signal-signature dbus-obj signal-name))
	(setq out-args
	      (multiple-value-list (funcall lisp-function dbus-obj)))
	(setq out-variant-tuple
	      (convert-to-gvariant out-args signature))
	(gir:invoke (bus "emit_signal")
		    nil
		    object-path
		    interface-name
		    signal-name
		    out-variant-tuple)))))


;;; ----------------------------------------------------------------------
;;;
;;; OBJECT REGISTRATION
;;;

(defvar *dbus-registered-objects* nil)

(defun lookup (object-path interface-name)
  (loop for a in *dbus-registered-objects*
	if (match-object-path-and-interface-name a object-path interface-name)
	return a))

(defun unregister (dbus-obj)
  (with-slots (bus regid) dbus-obj
    (when regid
      (invoke (bus "unregister_object") regid)
      (setq regid nil)))
  (setq *dbus-registered-objects* (delete dbus-obj *dbus-registered-objects*))
  dbus-obj)

(defun register (dbus-obj)
  (assert (not (find dbus-obj *dbus-registered-objects* :test #'dbus-interface-mixin-equal))
      nil "An object with the given object-path and interface-name is already registered.")
  (with-slots (bus bus-name object-path interface-info regid) dbus-obj
    (declare (ignorable bus-name))
    (and (slot-boundp dbus-obj 'regid)
	 regid
	 (error "Already registered regid."))
    (setq regid
	  (gir:invoke (bus "register_object")
		      object-path interface-info
		      (gir::make-closure (lambda (&rest args)
					   (apply 'handle-method-call args)))
		      (gir::make-closure (lambda (&rest args)
					   (apply 'handle-get-property args)))
		      (gir::make-closure (lambda (&rest args)
					   (apply 'handle-set-property args)))))
    (format t "rccservice: registered ~D: ~S.~&" regid dbus-obj)
    (assert (plusp regid) nil "failed to register.")
    (assert (not (find dbus-obj *dbus-registered-objects*)) nil "sanity")
    (push dbus-obj *dbus-registered-objects*)))
