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

(defun emit-signal-dbus (dbus-obj signal-name)
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


;;; ----------------------------------------------------------------------
;;;
;;; madhu 200913  - gdbus-eval-server.lisp
;;;

(defvar $eval-server-introspection-xml
  "<node>
<interface name=\"code.girlib.EvalServer\">
<method name=\"Eval\">
<arg name=\"input\" type=\"s\" direction=\"in\"/>
<arg name=\"success\" type=\"b\" direction=\"out\"/>
<arg name=\"result\" type=\"s\" direction=\"out\"/>
<arg name=\"output\" type=\"s\" direction=\"out\"/>
<arg name=\"error\" type=\"s\" direction=\"out\"/>
</method>
</interface>
</node>")

#+nil
(setq $node-info (dbus-node-info-from-xml $eval-server-introspection-xml))

;; from slynk.lisp
(defmacro without-printing-errors ((&key object stream
                                        (msg "<<error printing object>>"))
                                  &body body)
  ;; JT: Careful when calling this, make sure STREAM, if provided, is
  ;; a symbol that alwyas designates a non-nil stream.  See gh#287.
  "Catches errors during evaluation of BODY and prints MSG instead."
  `(handler-case (progn ,@body)
     (serious-condition ()
       ,(cond ((and stream object)
               (let ((gstream (gensym "STREAM+")))
                 `(let ((,gstream ,stream))
                    (print-unreadable-object (,object ,gstream :type t
                                                      :identity t)
                      (write-string ,msg ,gstream)))))
              (stream
               `(write-string ,msg ,stream))
              (object
               `(with-output-to-string (s)
                  (print-unreadable-object (,object s :type t :identity t)
                    (write-string ,msg  s))))
              (t msg)))))

(defun eval-server-eval (self input)
  (declare (ignore self))
  (let ((error-output (make-array 0 :element-type 'character
				  :fill-pointer t
				  :adjustable t))
	(standard-output  (make-array 0 :element-type 'character
			     :fill-pointer t
			     :adjustable t))
	(form nil)
	(values nil)
	(result nil)
	(read-successful-p nil)
	(eval-successful-p nil)
	(print-successful-p nil))
  (with-output-to-string (*standard-output* standard-output)
    (with-output-to-string (*error-output* error-output)
      (handler-case (progn (setq form (read-from-string input))
			   (setq read-successful-p t))
	(error (e)
	  (format *error-output*
		  "EVAL-SERVER: ERROR READING INPUT: ~S ~:* ~A~&" e)))
      (when read-successful-p
	(handler-case (progn (setq values (multiple-value-list (eval form)))
			     (setq eval-successful-p t))
	  (error (e)
	    (format *error-output*
		    "EVAL-SERVER: ERROR EVALING FORM: ~S ~:* ~A~&" e))))
      (when eval-successful-p
	(handler-case
	    (let ((*print-readably* nil))
	      (setq result
		    (cond ((null values) "; No value")
			  (t
			   (with-output-to-string (s)
			     (let ((strings
				    (loop for v in values
					  collect
					  (without-printing-errors
					      (:object v :stream s)
					    (prin1-to-string v)))))
			       (if (some #'(lambda (s) (find #\Newline s))
					 strings)
				   (format s "~{~a~^~%~}" strings)
				   (format s "~{~a~^, ~}" strings)))))))
	      (setq print-successful-p t))
	  (error (e)
	    (format *error-output*
		    "EVAL-SERVER: PRINTING RESULT: ~S ~:* ~A~&" e))))
      (values print-successful-p
	      (or result "")
	      standard-output
	      error-output
	      #+nil
	      (get-output-stream-string *standard-output*)
	      #+nil
	      (get-output-stream-string *error-output*))))))


#||
(defclass eval-server (dbus-service)
  ()
  (:default-initargs
   :object-path "/code/girlib/EvalServer"
   :bus-name "code.girlib.EvalServer"
   :interface-name "code.girlib.EvalServer"
   :node-info $eval-server-introspection-xml
   :method-handlers
   `(("Eval" . eval-server-eval))))

(setq $eval-server (make-instance 'eval-server))
(bus-name (slot-value $eval-server 'bus-name) :own)
(register $eval-server)
gdbus call --session --dest "code.girlib.EvalServer" --object-path "/code/girlib/EvalServer" --method "code.girlib.EvalServer.Eval" '(+ 2 3)'
(unregister $eval-server)
(bus-name (slot-value $eval-server 'bus-name) :unown)
||#
