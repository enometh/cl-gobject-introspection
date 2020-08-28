;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package "GDBUS")


;;; ----------------------------------------------------------------------
;;;
;;; DBUS CLIENT
;;;

(defclass dbus-proxy (dbus-interface-info-mixin)
  ((this :reader this-of)))

(defmethod initialize-instance :after ((self dbus-proxy) &key
				       (dbus-proxy-flags
					(nget *gio* "DBusProxyFlags" :none))
				       cancellable)
  (with-slots (this bus bus-name object-path interface-info interface-name) self
    (check-type dbus-proxy-flags integer)
    (setq this (invoke (*gio* "DBusProxy" "new_sync")
		       bus dbus-proxy-flags
		       interface-info bus-name object-path interface-name
		       cancellable))))

(defun make-dbus-proxy-for-interface (dbus-base-object interface-name)
  (with-slots (bus bus-name object-path node-info) dbus-base-object
    (make-instance 'dbus-proxy
      :bus bus :bus-name bus-name :object-path object-path
      :interface-name interface-name
      :node-info node-info)))

(defun proxy-call-sync (dbus-proxy method-name args &key (timeout-msec 15000) cancellable (call-flags (nget *gio* "DBusCallFlags" :none)))
  (multiple-value-bind (in-args-sigs out-args-sigs)
      (get-method-signature dbus-proxy method-name)
    (assert (= (length in-args-sigs) (length args)) nil "nargs Mismatch.")
    (let* ((params (when in-args-sigs
		     (convert-to-gvariant args in-args-sigs)))
	   (retval (invoke ((this-of dbus-proxy) "call_sync")
			   method-name
			   params
			   call-flags
			   timeout-msec
			   cancellable
			   )))
      (when out-args-sigs
	(values-list (convert-from-gvariant retval out-args-sigs))))))

(defun generate-defun-proxy-call-sync (dbus-proxy method-name name)
  (multiple-value-bind (in-args-names out-args-names)
      (get-method-signature dbus-proxy method-name :names-p t)
    (declare (ignore out-args-names))
    (multiple-value-bind (in-args-sigs out-args-sigs)
	(get-method-signature dbus-proxy method-name)
      (let ((param-names (loop for name in in-args-names
			       collect (intern (string-upcase name)))))
	`(defun ,name (dbus-proxy-object ,@param-names
		      &key (timeout-msec 15000) cancellable
		      (call-flags (nget *gio* "DBusCallFlags" :none)))
	   (let* ((params (convert-to-gvariant (list ,@param-names) ',in-args-sigs))
		  (retval (invoke ((this-of dbus-proxy-object) "call_sync")
				  ,method-name
				  params
				  call-flags
				  timeout-msec
				  cancellable
				  )))
	     (when ,out-args-sigs
	       (values-list (convert-from-gvariant retval ,out-args-sigs)))))))))


;;; ----------------------------------------------------------------------
;;;
;;; CACHED-PROPERTIES
;;;

(defun get-cached-property (dbus-proxy property-name)
  (convert-from-gvariant
   (invoke ((this-of dbus-proxy) "get_cached_property")
	   property-name)
   (get-property-signature dbus-proxy property-name)))

(defun set-cached-property (new-value dbus-proxy property-name)
  (invoke ((this-of dbus-proxy) "set_cached_property")
	  property-name
	  (convert-to-gvariant
	   new-value
	   (get-property-signature dbus-proxy property-name))))


;;; ----------------------------------------------------------------------
;;;
;;; SUBSCRIBING TO SIGNALS
;;;

(defstruct (signal-matcher (:type list))
  object-path interface-name signal-name
  bus-name  signature lisp-function bus id)

(defvar *subscribed* nil
  "List of SIGNAL-MATCHER")

(defun lookup-subscribed (object-path interface-name signal-name)
  (loop with args = (list object-path interface-name signal-name)
	for match in *subscribed*
	if (= (mismatch args match :test #'equal) 3) collect match))

(defun handle-signal-callback (bus bus-name object-path interface-name signal-name params user-data)
  (declare (ignorable bus bus-name user-data))
  #+nil
  (warn "<[~S]>" (list bus bus-name object-path interface-name signal-name params user-data))
  (dolist (match (lookup-subscribed object-path interface-name signal-name))
    (let ((lisp-function (signal-matcher-lisp-function match))
	  (signature (signal-matcher-signature match)))
      (when lisp-function
	(apply lisp-function
	       (and signature
		    (convert-from-gvariant
		     (gir::build-struct-ptr (nget *glib* "Variant")
					    params)
		     signature)))))))

(cffi:defcallback  default-handle-signal-callback :void
    ((bus :pointer) (bus-name :string) (object-path :string) (interface-name :string) (signal-name :string) (params :pointer) (user-data :pointer))
  (handle-signal-callback bus bus-name object-path interface-name signal-name params user-data))

(defun do-subscription (dbus-proxy action &key
			(flags (nget *gio* "DBusSignalFlags" :none))
			(bus-name nil bus-name-supplied-p)
			(interface-name nil interface-name-supplied-p)
			signal-name
			(object-path nil object-path-supplied-p)
			(callback (cffi:callback default-handle-signal-callback))
			(user-data (cffi:null-pointer))
			(user-data-free-func (cffi:null-pointer))
			allow-dups
			lisp-function
			signature)
  (with-slots ((bus-name-a bus-name)
	       (interface-name-a interface-name)
	       (object-path-a object-path)
	       bus)
      dbus-proxy
    (let* ((bus-name (if bus-name-supplied-p bus-name bus-name-a))
	   (interface-name (if interface-name-supplied-p
			       interface-name
			       interface-name-a))
	   (object-path (if object-path-supplied-p
			    object-path
			    object-path-a))
	   (matches (lookup-subscribed object-path interface-name signal-name)))
      (ecase action
	(:list (return-from do-subscription matches))
	(:unsubscribe
	 (dolist (match matches)
	   (invoke ((signal-matcher-bus match) "signal_unsubscribe")
		   (signal-matcher-id match)))
	 (setq *subscribed*
	       (delete-if (lambda (x) (find x matches)) *subscribed*)))
	(:subscribe
	 (unless allow-dups
	   (assert (not (lookup-subscribed object-path interface-name signal-name)) nil "duplicate exists"))
	 (when lisp-function
	   (unless signature
	     (when signal-name
	       (setq signature (get-signal-signature dbus-proxy signal-name))))
	   (assert signature
	       nil "cannot call a lisp callback-function without a signature"))
	 (let ((signal-id (invoke (bus "signal_subscribe")
				  bus-name
				  interface-name
				  signal-name
				  object-path
				  nil
				  flags
				  callback
				  user-data
				  user-data-free-func)))
	   (push (make-signal-matcher :bus-name bus-name
				      :object-path object-path
				      :interface-name interface-name
				      :signal-name signal-name
				      :signature signature
				      :lisp-function lisp-function
				      :bus bus
				      :id signal-id)
		 *subscribed*)))))))

#||
(setq $a (make-instance 'dbus-base-object
	   :bus (dbus-session-bus)
	   :bus-name "uk.me.doof.Cowsay"
	   ))
(get-interface-names $a)
(setq $p (make-dbus-proxy-for-interface $a "uk.me.doof.Cowsay"))
(get-interface-info-as-xml $p)
(get-method-names $p)
(get-method-signature $p "ShowCow")
(get-method-signature $p "ShowCow" :names-p t)
(proxy-call-sync $p "ShowCow" '("foo"))
(eval (generate-defun-proxy-call-sync $p "ShowCow" 'show-cow))
(show-cow $p "foo")
(get-cached-property $f "version")
(set-cached-property 2 $f "version")

(nget *gio* "bus_get")
(list-class-functions-desc (nget *gio* "DBusConnection"))
(get-callable-desc *gio* "bus_get")
(nget *gio* "DBusSignalCallback")
(get-callable-desc *gio*  "DBusSignalCallback")
(nget *gio* "AsyncReadyCallback")
(nget *gio* "Cancellable")
(gir::get-callback-desc *gio* "DBusSignalCallback")
(get-method-desc (nget *gio* "DBusConnection") "signal_unsubscribe")
(nget *glib* "DestroyNotify")
(mismatch '("1" "2" "3" "4") '("1" "2" "3") :test #'equal)
(mismatch '("1" "2" "3") '("1" "2" "3" "4") :test #'equal)
(get-callable-desc *gio* "DBusSignalCallback")
(get-method-desc (nget *gio* "DBusConnection") "signal_subscribe")

(setq $a (make-instance 'dbus-proxy
	   :bus (dbus-session-bus)
	   :bus-name "net.Corp.MyApp"
	   :object-path "/net/corp/myapp"
	   :interface-name "net.Corp.MyApp.Frobber"
	   ))
(get-signal-signature $a "Notification")
*subscribed*
(do-subscription $a :subscribe :signal-name "Notification")
(do-subscription $a :unsubscribe :signal-name "Notification")


(lookup-subscribed "/net/corp/myapp" "net.Corp.MyApp.Frobber" "Notification")
(do-subscription $a :list :signal-name "Notification")
(setf (signal-matcher-lisp-function (car *subscribed*))
      (lambda (blob-bytes height messages)
	(format t "~S~&"  (list blob-bytes height messages))))
(setf (signal-matcher-signature (car *subscribed*))
       '("ay" "i" "as"))

(nget *glib* "quark_from_static_string")
(cffi:foreign-funcall 

(invoke (*glib* "quark_from_static_string") "foo")

||#
