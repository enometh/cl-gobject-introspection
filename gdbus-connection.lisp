;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package "GDBUS")


;;; ----------------------------------------------------------------------
;;;
;;; SINGLETON CONNECTIONS
;;;

(defvar *dbus-system-connection* nil "GDBusConnection to the system bus")
(defvar *dbus-session-connection* nil "GDBusConnection to the system bus")

(defun dbus-init-bus (&optional bus)
  "Establish the connection to D-Bus. BUS can be either :SYSTEM or
:SESSION. Defaults to :SESSION."
  (let* ((bus (ecase bus
		((:session :system) bus)
		((nil) :session)))
	 (bus-connection-var (ecase bus
			       (:session  '*dbus-session-connection*)
			       (:system '*dbus-system-connection*)))
	 (connection nil))
    (unless (setq connection (symbol-value bus-connection-var))
      (setq connection
	    (set bus-connection-var
		 (invoke (*gio* "bus_get_sync")
			 (nget *gio* "BusType" bus) nil)))
      (setf (property connection "exit-on-close") nil))
    (assert (not (invoke (connection "is_closed"))))
    (values connection
	    (invoke (connection "get_unique_name")))))

(defun dbus-system-bus ()
  (or *dbus-system-connection* (dbus-init-bus :system)))

(defun dbus-session-bus ()
  (or *dbus-session-connection* (dbus-init-bus :session)))

#+nil
(dbus-init-bus)

;; NO CLOSE METHOD - once a connection is closed it remains closed
;; until the process quits.
;;
;; (gir:get-method-desc (gir:nget *gio* "DBusConnection") "close")
;;

#+BOGUS
(cffi:defcallback on-connection-close :void
    ((source-object :pointer)		;GObject*
     (result :pointer)			;GAsyncResult*
     (user-data :pointer)		;gpointer
     )
  (warn "Closed DBUS Connection: source result user-dataa=~S"
	(list source-object result user-data)))

#+BOGUS
(defun dbus-close-bus (&optional bus-type)
  (let* ((bus-type (ecase bus-type
		((:session :system) bus-type)
		((nil) :session)))
	 (bus-connection-var (ecase bus-type
			       (:session '*dbus-session-connection*)
			       (:system '*dbus-system-connection*)))
	 (bus (symbol-value bus-connection-var)))
    (when bus
      (prog1 (gir:invoke (bus "close") nil (cffi:callback on-connection-close) (cffi:null-pointer))
	(set bus-connection-var nil)))))

#+nil
(dbus-close-bus :session)


;;; ----------------------------------------------------------------------
;;;
;;; BUS names
;;;

(defvar *dbus-registered-bus-names* nil
  "Alist of (service-name . owner-id) of service names owned on D-Bus.")

(defun on-name-acquired  (bus name)
  (format t "XXCB on_name_acquired: ~S.~&" (list bus name)))

(defun on-name-lost (bus name)
  (format t "XXCB on_name_lost: ~S.~&" (list bus name)))

(defun bus-name (bus-name action &key
		 re-register
		 (bus (dbus-session-bus))
		 (flags (nget *gio* "BusNameOwnerFlags" :none))
		 (on-name-acquired #'on-name-acquired)
		 (on-name-lost #'on-name-lost))
  "Own and unown bus names."
  (let* ((elt (assoc bus-name *dbus-registered-bus-names* :test #'equal))
	 (owner-id (cdr elt)) start stop)
    (ecase action
      ((:acquire :register :own) (setq start t) (if re-register (setq stop t)))
      ((:unregister :unown :relinquish) (setq start nil) (setq stop t))
      (:re-register (setq start t) (setq stop t))
      (:list (return-from bus-name (mapcar 'car *dbus-registered-bus-names*))))
    (when stop
      (when owner-id
	(invoke (*gio* "bus_unown_name") owner-id)
	(setq owner-id nil)
	(unless start
	  (when elt
	    (setq *dbus-registered-bus-names*
		  (delete elt *dbus-registered-bus-names*)))
	  (setq elt nil))))
    (when start
      (unless owner-id
	(setq owner-id (invoke (*gio* "bus_own_name_on_connection")
			       bus  bus-name flags
			       (if on-name-acquired
				   (gir::make-closure on-name-acquired)
				   (cffi:null-pointer))
			       (if on-name-lost
				   (gir::make-closure on-name-lost)
				   (cffi:null-pointer))))
	(cond (elt (setf (cdr elt) owner-id))
	      (t (setq elt (cons bus-name owner-id))
		 (setq *dbus-registered-bus-names*
		       (cons elt *dbus-registered-bus-names*))))))))

#+nil
(bus-name "org.gtk.test" :acquire)


;;; ----------------------------------------------------------------------
;;;
;;; ERROR NAMES
;;;

(defvar *dbus-error-domains* nil
  "Alist of (ERROR-DOMAIN-NAME ERROR-DOMAIN-QUARK).")

;; (nget *gio* "dbus_error_register_error_domain") does not work for
;; some reason on CCL even after extending (setf (gir:field))
;; g_dbus_error_register_error_domain calls g_quark_from_static_string
;; which required the error-domain-name to be present for the lifetime
;; of the program. this works in ECL.
#+nil
(defun register-error-domain (error-domain-name
			      &rest error-code-error-string-pairs)
  (let ((seq (loop for (error-code error-string)
		   on error-code-error-string-pairs
		   by #'cddr collect
		   (let ((struct (allocate-struct
				  (nget *gio* "DBusErrorEntry"))))
		     (setf (field struct "error_code") error-code)
		     (setf (field struct "dbus_error_name") error-string)
		     struct)))
	ret)
    (cffi:with-foreign-object (quark-volatile :int32)
      (setf (cffi:mem-ref quark-volatile :int32) 0)
      (invoke (*gio* "dbus_error_register_error_domain")
	      error-domain-name quark-volatile seq)
      (setq ret (cffi:mem-ref quark-volatile :int32)))
    (mapcar (lambda (x)
	      (cffi:foreign-free
	       (cffi:mem-ref (this-of x) :pointer
			     (gir::field-offset x "dbus_error_name"))))
	    seq)
    ret))

(defun error-domain-quark (error-domain)
  "May return 0 if the quark does not exist"
  (etypecase error-domain
    (integer error-domain)
    (string (invoke (*glib* "quark_try_string") error-domain))))

#||
(error-domain-quark "foo.bar")
(register-error-domain "foo.bar" 0 "error0" 1 "error1")
(error-domain-quark "foo.bar")		;fail on CCL
||#

(defun make-error-domain (error-domain-name &aux elt)
  "Creates GQuark for ERROR-DOMAIN-NAME if it does not exist and add it
to *dbus-error-domains*. Returns the GQuark."
  (unless (setq elt (assoc error-domain-name *dbus-error-domains*
			   :test #'equal))
    (setq *dbus-error-domains*
	  (cons (setq elt (cons (invoke (*glib* "quark_from_string")
					error-domain-name)
				error-domain-name))
		*dbus-error-domains*)))
  (cdr elt))

(defun register-error-name (error-domain error-code error-name)
  (invoke (*gio* "dbus_error_register_error")
	  (error-domain-quark error-domain)
	  error-code
	  error-name))

(defun unregister-error-name (error-domain error-code error-name)
  (invoke (*gio* "dbus_error_unregister_error")
	  (error-domain-quark error-domain)
	  error-code
	  error-name))

(defun do-error-domain (error-domain action &rest error-code-error-string-pairs)
  (values
   (let ((ret1 (error-domain-quark error-domain))) (if (zerop ret1) nil) ret1)
   (loop for (error-code error-string)
	 on error-code-error-string-pairs
	 by #'cddr collect
	 (funcall (ecase action
		    (:register #'register-error-name)
		    (:unregister #'unregister-error-name))
		  error-domain error-code error-string))))

