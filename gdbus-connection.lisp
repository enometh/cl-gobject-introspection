;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package "GDBUS")


;;; ----------------------------------------------------------------------
;;;
;;; SINGLETON CONNECTIONS
;;;

(defvar *dbus-system-connection* nil "GDBusConnection to the system bus.")
(defvar *dbus-session-connection* nil "GDBusConnection to the session bus.")

(defun dbus-init-bus (&optional bus-type)
  "Ensure an open connection to D-Bus. BUS-TYPE can be either :SYSTEM or
:SESSION. Defaults to :SESSION."
  (let* ((bus-type (ecase bus-type
		     ((:session :system) bus-type)
		     ((nil) :session)))
	 (bus-connection-var (ecase bus-type
			       (:session '*dbus-session-connection*)
			       (:system '*dbus-system-connection*)))
	 (connection nil))
    (unless (and (setq connection (symbol-value bus-connection-var))
		 (not (invoke (connection "is_closed")))
		 (progn (format t "dbus_init_bus ~A: trying to get a new connection~&" bus-type)
			t))
      (let ((address (invoke (*gio* "dbus_address_get_for_bus_sync")
			     (nget *gio* "BusType" bus-type)
			     nil)))
	(format t "dbus_init_bus: connecting to ~A~&" address)
	(setq connection
	      (set bus-connection-var
		   (invoke (*gio* "DBusConnection" "new_for_address_sync")
			   address
			   (logior (nget *gio* "DBusConnectionFlags" :authentication-client)
				   (nget *gio* "DBusConnectionFlags" :message-bus-connection))
			   nil
			   nil)))
	(setf (property connection "exit-on-close") nil)))
    (assert (not (invoke (connection "is_closed"))))
    (values connection
	    (invoke (connection "get_unique_name")))))

(defun dbus-system-bus ()
  (or *dbus-system-connection* (dbus-init-bus :system)))

(defun dbus-session-bus ()
  (or *dbus-session-connection* (dbus-init-bus :session)))

;;
;; dbus-close-bus - implemented asynchronously
;;

#+nil
(dbus-init-bus)

#+nil
(get-callback-desc *gio* "AsyncReadyCallback")

#+nil
(gir::generate-cffi-defcallback
 (info-of (nget *gio* "AsyncReadyCallback"))
 'on-connection-close)

#+nil
(get-method-desc (nget *gio* "DBusConnection") "close_finish")

(CFFI:DEFCALLBACK ON-CONNECTION-CLOSE-CALLBACK
    :VOID
    ((SOURCE-OBJECT :POINTER)
     (RES :POINTER)
     (USER-DATA :POINTER))
  "ARGS:  SOURCE-OBJECT Object. RES AsyncResult."
  (ON-CONNECTION-CLOSE
   (GIR::GOBJECT (GTYPE SOURCE-OBJECT) SOURCE-OBJECT)
   (GIR::GOBJECT (GTYPE RES) RES) USER-DATA))

(defun on-connection-close (connection async-result user-data)
  (let ((result (invoke (connection "close_finish") async-result)))
    (format t "closed DBUS Connection: (source-object result user-data)=~S"
	    (list connection result user-data))))

(defun dbus-close-bus (&optional bus-type)
  (let* ((bus-type (ecase bus-type
		     ((:session :system) bus-type)
		     ((nil) :session)))
	 (bus-connection-var (ecase bus-type
			       (:session '*dbus-session-connection*)
			       (:system '*dbus-system-connection*)))
	 (connection (symbol-value bus-connection-var)))
    (when connection
      (prog1 (gir:invoke (connection "close")
			 nil
			 (cffi:callback on-connection-close-callback)
			 (cffi:null-pointer))
	#+nil
	(set bus-connection-var nil)))))

#+nil
(dbus-close-bus :session)

#+nil
(invoke ((dbus-session-bus) "is_closed"))

#+nil
(defun pdlsym (cname library-path &key elf
	       (pid (gir-lib::getpid)))
  (check-type elf elf:elf)
  ;; read-elf is expensive so better have the caller supply it
  (unless elf (setq elf (elf:read-elf library-path)))
  (let ((base-address
	 (with-open-file (stream (format nil "/proc/~D/maps" pid))
	   (loop for line = (read-line stream nil)
		 while line do
		 (when (search library-path line :from-end t)
		   (return
		     (parse-integer line :end (position #\- line)
				    :radix 16))))))
	(elf-symbol (elf:named-symbol elf cname)))
    (assert elf-symbol)
    (assert base-address)
    (cffi:make-pointer (+ base-address (elf:value elf-symbol)))))

#||
(require 'elf)				;eschulte's
(check-type $f elf:elf)
(time (setq $f (elf:read-elf "/usr/lib64/libgio-2.0.so.0")))
;real time : 48.048 secs
;run time  : 45.603 secs
;gc count  : 557 times
;consed    : 43588343296 bytes
(setq $ptr (pdlsym "_g_bus_forget_singleton"
		   "/usr/lib64/libgio-2.0.so.0.6800.0"
		   :elf $f))
(setq $bus
      (invoke (*gio* "bus_get_sync")  (nget *gio* "BusType" :session) nil))
(invoke ($bus "is_closed"))
(invoke ($bus "close_sync") nil)
(invoke ($bus "is_closed"))
(cffi:foreign-funcall-pointer $ptr ()
			      :int (gir:nget *gio* "BusType" :session)
			      :void)
(setq $bus
      (invoke (*gio* "bus_get_sync")  (nget *gio* "BusType" :session) nil))
(invoke ($bus "is_closed"))
||#

(defun dbus-forget-singleton (&optional bus-type)
  (let* ((bus-type (ecase bus-type
		     ((:session :system) bus-type)
		     ((nil) :session)))
	 (bus-connection-var (ecase bus-type
			       (:session '*dbus-session-connection*)
			       (:system '*dbus-system-connection*)))
	 (connection (symbol-value bus-connection-var)))
    (when connection
      (assert (invoke (connection "is_closed"))
	  nil
	  "Close the connection first.")
      (set bus-connection-var nil))))

#+nil
(progn
(invoke ((dbus-session-bus) "is_closed")) ; T
(dbus-forget-singleton)
(invoke ((dbus-session-bus) "is_closed")) ;NIL
)


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

