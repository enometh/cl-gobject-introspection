;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Feb 06 15:20:02 2023 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2023 Madhu.  All Rights Reserved.
;;;

;; a regressive wrapper around g_unix_signal_add_full using
;; ffi-callback-manager. only a few signals are supported by
;; glib. maintain only one handler for each signal number, and the
;; handler is called with a single argument, the signal number.

(in-package "GIR-LIB")

(defvar *unix-signal-handlers* (make-hash-table)
  "key SIGNUM value list of GSource tags returned by GLib")

(defun unregister-unix-signal-handler (signum)
  (let ((tag (gethash signum *unix-signal-handlers*)))
    (when tag
      (warn "gir-lib: Unregistering handler for signal ~D" signum)
      (gir:invoke (*glib* "source_remove") tag))
    (remhash signum *unix-signal-handlers*)))

(defun register-unix-signal-handler (signum function)
  "FUNCTION is called with one argument the signal number
SIGNUM when the program is signalled. Replaces an existing function
resgistered for SIGNUM."
  (unregister-unix-signal-handler signum)
  (flet ((source-func ()
	   (funcall function signum)		;lame
	   t))				;G_SOURCE_CONTINUE
    (let* ((loc (register-callback #'source-func))
	   (tag (gir:invoke (*glib* "unix_signal_add")
		  0					  ;priority
		  signum				  ;sigusr1
		  (cffi:callback funcall-object-callback) ;source-function
		  loc					  ;user-data
		  (cffi:callback free-funcall-object-callback ;destroy-notify
					 ))))
      (cond ((zerop tag)
	     (warn "gir-lib: Unable to register a handler for signal ~D" signum)
	     (unregister-callback loc))
	    (t (setf (gethash signum *unix-signal-handlers*) tag))))))

#||
;;(gir:nget-desc *glib* "unix_signal_add")
;; => #F<unix_signal_add(#V<priority: INTEGER> #V<signum: INTEGER>
;;                    #V<handler: POINTER> #V<user_data: POINTER>
;;                    #V<notify: POINTER>): (#V<RETURN-VALUE: INTEGER>)>

(register-unix-signal-handler 10 (lambda (n) (g-message "Ignoring signal ~D" n)))
(unregister-unix-signal-handler 10)
||#