;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: [Sun Nov 26 14:10:37 2017 +0530] <enometh@meer.net>
;;;   Bugs-To: enometh@meer.net
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2017-2020 Madhu.  All Rights Reserved.
;;;
(in-package "CL-USER")
(defpackage "GDBUS"
  (:use "CL" "GIR" "GIR-LIB")
  (:export
   ;; dbus-connection
   "DBUS-SESSION-BUS"
   "DBUS-SYSTEM-BUS"
   "DBUS-CLOSE-BUS"
   "DBUS-FORGET-SINGLETON"
   "BUS-NAME"

   "ERROR-DOMAIN-QUARK"
   "MAKE-ERROR-DOMAIN"
   "DO-ERROR-DOMAIN"

   ;; dbus-introspect
   "DBUS-NODE-INFO-FROM-XML"
   "DBUS-NODE-INFO-TO-XML"
   "DBUS-GET-INTROSPECTION-XML"

   ;; dbus-object
   "DBUS-BASE-OBJECT"
   "GET-INTERFACE-NAMES"
   "GET-INTERFACE-INFO-AS-XML"
   "GET-METHOD-NAMES"
   "GET-METHOD-SIGNATURE"
   "GET-PROPERTY-NAMES"
   "GET-PROPERTY-SIGNATURE"
   "GET-SIGNAL-NAMES"
   "GET-SIGNAL-SIGNATURE"

   ;; dbus-proxy
   "DBUS-PROXY"
   "MAKE-DBUS-PROXY-FOR-INTERFACE"
   "PROXY-CALL-SYNC"
   "GENERATE-DEFUN-PROXY-CALL-SYNC"

   ;; dbus-server
   "DBUS-SERVICE"
   "EMIT-SIGNAL"
   "REGISTER"
   "UNREGISTER"
   ))
(in-package "GDBUS")
