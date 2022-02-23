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
  (:import-from "ENSURE-CTF" "ENSURE-CTF")
  (:import-from "GIR" "THIS-OF")
  #+mkcl
  (:shadowing-import-from "TRIVIAL-GRAY-STREAMS"
   "CLOSE" "OPEN-STREAM-P" "STREAM-ELEMENT-TYPE")
  (:export
   "*GIO*" "*GLIB*" "*GOBJECT*"
   #-no-gtk "*GTK*" #-no-gtk "*GDK*" #-no-gtk "*CAIRO*"
   #-no-gtk "*GDK-X11*"
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

   "THIS-OF"

   ;;settings
   "WITH-SETTINGS-SCHEMA-SOURCE"
   "SETTINGS-SOURCE-NEW-FROM-DIRECTORY"
   "SETTINGS-LIST-SCHEMAS"
   "SETTINGS-SCHEMA-LOOKUP"
   "SETTINGS-LOOKUP"
   "SETTINGS-VALUE"
   "SETTINGS-SCHEMA-RELOCATABLE-P"
   "SETTINGS-SCHEMA-NON-RELOCATABLE-PATH"
   "SETTINGS-SCHEMA-LIST-KEYS"
   "SETTINGS-SCHEMA-KEY-LOOKUP"
   "SETTINGS-SCHEMAS-MAP"
   "SETTINGS-SCHEMAS-MAP-KEYS"
   "SETTINGS-SCHEMA-GET-RANGE-FOR-KEY"
   "SETTINGS-SCHEMA-GET-SIG-FOR-KEY"
   "SETTINGS-SCHEMA-GET-DEFAULT-VALUE-FOR-KEY"
   ))

(in-package "GIR-LIB")

(progn
(defvar *gio* (load-time-value (gir:require-namespace "Gio")))
(defvar *glib* (load-time-value (gir:require-namespace "GLib")))
(defvar *gobject* (load-time-value (gir:require-namespace "GObject"))))

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


(defvar *gdk-x11* (load-time-value
		   (unless (featurep :no-gtk)
		     (apply #'gir:require-namespace
			    "GdkX11"
			    (if (featurep :wk) (list "3.0"))))))


#+nil
(import '(*gio* *glib* *gobject* *gtk* *gdk* *gdk-x11*) "CL-USER")

(defvar *gi-repository*
  (load-time-value (gir:require-namespace "GIRepository" "2.0")))

(defpackage "GIR-TEST"
  (:use "CL" "GIR" "GIR-LIB")
  (:export))