;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <2019-06-01 16:10:35 IST>
;;;   Touched: Fri May 31 21:31:23 2019 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2019 Madhu.  All Rights Reserved.
;;;
(in-package "CL-USER")

;;#+ccl (shadowing-import '(gir::name-of) "CL-USER")

(defpackage "GIR-LIB"
  (:use "CL")
  (:export
   "*GIO*" "*GLIB*" "*GOBJECT*"
   #-no-gtk "*GTK*" #-no-gtk "*GDK*" #-no-gtk "*CAIRO*"
   "*GI-REPOSITORY*"

   ;; gtk-thread
   "GTK-ENQUEUE" "WITH-GTK-THREAD" "APPLY-IN-GTK-THREAD"
   "START-GTK-THREAD"

   ;; gdbus-connection
   "DBUS-INIT-BUS" "*CONNECTION*"

   ;; ffi-callback-manager
   "FIND-CALLBACK" "WITH-REGISTERED-CALLBACK"

   ;; gslist
   "MAP-SLIST" "PREPEND-TO-SLIST" "SLIST-FREE" "SLIST-FREE-FULL"
   "SLIST-LENGTH"
   ))

(in-package "GIR-LIB")

(progn
(defvar *gio*  (gir:require-namespace "Gio"))
(defvar *glib* (gir:require-namespace "GLib"))
(defvar *gobject* (gir:require-namespace "GObject")))

#||
#-no-gtk
(defvar *gtk* (gir:ffi "Gtk" #+lispworks6 "2.0" #+wk "3.0")))
#-no-gtk
(defvar *gdk* (gir:ffi "Gdk" #+lispworks6 "2.0" #+wk "3.0"))
#-no-gtk
(defvar *cairo* (gir:require-namespace "cairo"))
||#

(defun featurep (x) (find x *features*))

(defvar *gtk* (load-time-value
	       (unless (featurep :no-gtk)
		 (apply #'gir:require-namespace
			"Gtk"
			(if (featurep :wk) (list "3.0"))))))

(defvar *gdk* (load-time-value
	       (unless (featurep :no-gtk)
		 (apply #'gir:require-namespace
			"Gdk"
			(if (featurep :wk) (list "3.0"))))))

(defvar *cairo* (load-time-value
		 (unless (featurep :no-gtk)
		   (apply #'gir:require-namespace "cairo" nil))))


#+nil
(import '(*gio* *glib* *gobject* *gtk* *gdk*) "CL-USER")

(defvar *gi-repository* (gir::ffi "GIRepository" "2.0"))

(defpackage "GIR-TEST"
  (:use "CL" "GIR" "GIR-LIB")
  (:export))