;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Feb 14 02:41:46 PM IST 2022 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2023 Madhu.  All Rights Reserved.
;;;
(in-package "GIR-LIB")

(eval-when (load eval compile)
  (assert (not (featurep :no-gtk))))

(defvar *wk*
  (load-time-value
   (apply #'gir:require-namespace
	  (if (featurep :wk)
	      "WebKit2"
	      "WebKit")
	  (list
	   (if (featurep :wk)
	       "4.1"			; libsoup3 gtk3
	       "6.0"			; libsoup3 gtk4
	       )))))

(defvar *wkext*
  (load-time-value
   (apply #'gir:require-namespace
	  (if (featurep :wk)
	      "WebKit2WebExtension"
	      "WebKitWebExtension")
	  (list
	   (if (featurep :wk)
	       "4.1"
	       "6.0")))))

(defvar *jsc*
  (load-time-value
   (apply #'gir:require-namespace
	  "JavaScriptCore"
	  (list
	   (if (featurep :wk)
	       "4.1"
	       "6.0")))))

(export '(*wk* *wkext* *jsc*))

#||
(gir-lib:list->strings
 (gir:invoke (*gi-repository* "Repository" "get_search_path")))
((gir:invoke (*gi-repository* "Repository" "prepend_search_path")
  "/12/build/webkitgtk/build.amd64/girepository-1.0")
((gir:invoke (*gi-repository* "Repository" "prepend_library_path")
  "/12/build/webkitgtk/build.amd64/lib")
(require 'girlib-wk)
||#