;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: [Sun Nov 26 14:10:37 2017 +0530] <enometh@meer.net>
;;;   Bugs-To: enometh@meer.net
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2017 Madhu.  All Rights Reserved.
;;;
(in-package "GIR-LIB")


;;; ----------------------------------------------------------------------
;;;
;;; SINGLETON CONNECTION
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
		 (gir:invoke (*gio* "bus_get_sync")
			     (gir::nget *gio* "BusType" bus)
			     nil)))
      (setf (gir:property connection "exit-on-close") nil))
    (assert (not (gir:invoke (connection "is_closed"))))
    (values connection
	    (gir:invoke (connection "get_unique_name")))))

(defun dbus-system-bus ()
  (or *dbus-system-connection* (dbus-init-bus :system)))

(defun dbus-session-bus ()
  (or *dbus-session-connection* (dbus-init-bus :session)))

#+nil
(dbus-init-bus)
