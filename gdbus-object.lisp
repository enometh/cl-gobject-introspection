;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package "GDBUS")


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defclass dbus-base-object ()
  ((bus :initarg :bus :initform (dbus-session-bus))
   (bus-name :initarg :bus-name)
   (object-path :initarg :object-path :initform nil)
   (node-info :initarg :node-info :initform nil)))

(defun dump-slots (obj slots stream)
  (loop for x on slots for slot = (car x)
	do (format stream ":~A ~S~@[ ~]" slot (slot-value obj slot) (cdr x))))

(defmethod print-object ((self dbus-base-object) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (dump-slots self '(bus-name object-path) stream)))

(defmethod initialize-instance :after ((self dbus-base-object) &key)
  (with-slots (bus bus-name object-path node-info) self
    (setq bus-name (auto-bus-name bus-name))
    (setq object-path (auto-object-path bus-name object-path))
    (assert bus nil "DBus Connection not initialized.")
    (assert bus-name nil "DBus Connection not initialized.")
    (assert object-path nil "DBus object-path not initialized.")
    (cond ((stringp node-info)
	   (setq node-info (dbus-node-info-from-xml node-info)))
	  ((null node-info)
	   (setq node-info
		 (dbus-node-info-from-xml
		  (dbus-get-introspection-xml bus bus-name object-path))))
	  ((eql (gir::struct-class-of node-info) (nget *gio* "DBusNodeInfo"))
	   t)
	  (t (assert node-info nil "DBus node-info not initialized")))))

(defun prefixp (prefix sequence &key (test #'equalp) (start1 0) (start2 0) end1 end2 &aux idx)
  "Begins with."
  (or (null (setq idx (mismatch prefix sequence :test test
				:start1 start1 :start2 start2 :end1 end1 :end2 end2)))
      (>= idx (length prefix))))


(defmethod get-interface-names ((self dbus-base-object) &key (filter "org.freedesktop.DBus"))
  (with-slots (node-info) self
    (loop for intf in (field node-info "interfaces")
	  for name = (field intf "name")
	  unless (prefixp filter name) collect name)))

(defclass dbus-interface-info-mixin (dbus-base-object)
  ((interface-info :initarg :interface-info :initform nil)
   (interface-name :initarg :interface-name)))

(defmethod initialize-instance :after ((self dbus-interface-info-mixin) &key)
  (with-slots (interface-info node-info interface-name bus-name) self
    (unless (and (slot-boundp self 'interface-name) interface-name)
      (setq interface-name bus-name))
    (cond (interface-info
	   (assert (not node-info) nil "DBusNodeInfo and DBusInterfaceInfo both supplied.")
	   (assert (eql (gir::struct-class-of interface-info)
			(nget *gio* "DBusInterfaceInfo"))
	       nil "DBus InterfaceInfo should be a gobject"))
	  (t (setq interface-info
		   (invoke (node-info "lookup_interface") interface-name))
	     (assert interface-info nil "DBus InterfaceInfo not found: ~A"
	       interface-name)))))

(defmethod print-object ((self dbus-interface-info-mixin) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (dump-slots self '(bus-name object-path interface-name) stream)))

(defun match-object-path-and-interface-name
    (dbus-object object-path interface-name)
  (with-slots ((object-path-a object-path)
	       (interface-name-a interface-name))
      dbus-object
    (and (equal object-path-a object-path)
	 (equal interface-name-a interface-name))))

(defun dbus-interface-mixin-equal (a b)
  (with-slots ((interface-name-a interface-name)
	       (object-path-a object-path))
      a
    (match-object-path-and-interface-name b object-path-a interface-name-a)))

(defun get-interface-info-as-xml (dbus-obj)
  (with-slots (interface-info) dbus-obj
    (with-struct (string-builder (nget *glib* "String"))
      (invoke (interface-info "generate_xml") 4 string-builder)
      (field string-builder "str"))))

(defun get-method-names (dbus-obj)
  (with-slots (interface-info interface-name) dbus-obj
    (values
     (mapcar (lambda (method-info)
	      (field method-info "name"))
	     (field interface-info "methods"))
     interface-name)))

(defun get-method-signature (dbus-obj method-name &key names-p)
  (with-slots (interface-info) dbus-obj
    (let* ((method-info (invoke (interface-info "lookup_method")
				method-name))
	   (in-args (field method-info "in_args"))
	   (out-args (field method-info "out_args"))
	   (field-name (if names-p "name" "signature")))
      (values (mapcar (lambda (arg) (field arg field-name)) in-args)
	      (mapcar (lambda (arg) (field arg field-name)) out-args)))))

(defun get-property-names (dbus-obj)
  (with-slots (interface-info interface-name) dbus-obj
    (values
     (mapcar (lambda (property-info)
	      (field property-info "name"))
	     (field interface-info "properties"))
     interface-name)))

(defun get-property-signature (dbus-obj property-name)
  (with-slots (interface-info) dbus-obj
    (let* ((property-info (invoke (interface-info "lookup_property")
				  property-name)))
      (field property-info "signature"))))

(defun get-signal-names (dbus-obj)
  (with-slots (interface-info interface-name) dbus-obj
    (values
     (mapcar (lambda (method-info)
	      (field method-info "name"))
	     (field interface-info "signals"))
     interface-name)))

(defun get-signal-signature (dbus-obj signal-name &key names-p)
  (with-slots (interface-info) dbus-obj
    (let* ((signal-info (invoke (interface-info "lookup_signal")
				signal-name))
	   (args (field signal-info "args"))
	   (field-name (if names-p "name" "signature")))
      (mapcar (lambda (arg) (field arg field-name)) args))))


#||
(defclass foo () ((bar :initarg :bar)))
(defmethod initialize-instance :after ((self foo) &key (bar 10)) (warn "bar ~S" bar) (unless (slot-boundp self 'bar) (setf (slot-value self 'bar) bar)))
(defclass bar (foo) ((xyz :initarg :xyz)))
(defmethod initialize-instance :after ((self bar) &key (bar 100) (xyz 20)) (warn "xyz ~S" xyz))
(make-instance 'bar)
(property (dbus-session-bus) "unique-name")
(property (this-of $f) "g-name-owner")

(defvar $f (make-instance 'dbus-proxy
	     :bus-name "org.freedesktop.portal.Desktop"
	     :object-path "/org/freedesktop/portal/desktop"
	     :interface-name "org.freedesktop.portal.Background"))
(get-method-names $f)
(get-property-names $f)
(get-property-signature $f "version")

(defvar $f (make-instance 'dbus-proxy
	     :bus-name "org.gtk.vfs.Daemon"
	     :object-path "/org/gtk/vfs/mounttracker"
	     :interface-name "org.gtk.vfs.MountTracker"))
(get-signal-names $f)
(get-signal-signature $f "Mounted")
||#
