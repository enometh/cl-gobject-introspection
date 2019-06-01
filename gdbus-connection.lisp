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

(defvar *connection* nil "GDBusConnection")

(defun dbus-init-bus (&optional bus)
  "Establish the connection to D-Bus. BUS can be either :SYSTEM or
:SESSION. Defaults to :SESSION."
  (unless *connection*
    (setq *connection*
	  (gir:invoke (*gio* "bus_get_sync")
		      (gir::nget *gio* "BusType" (ecase bus
						   ((:session :system) bus)
						   ((nil) :session)))
		      nil))
    (setf (gir:property *connection* "exit-on-close") nil))
  (assert (not (gir:invoke (*connection* "is_closed"))))
  (values *connection*
	  (gir:invoke (*connection* "get_unique_name"))))

#+nil
(dbus-init-bus)
