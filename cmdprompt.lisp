;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Sep 10 08:04:33 2020 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2020-2024 Madhu.  All Rights Reserved.
;;;
(in-package "GIR-LIB")

(eval-when (load eval compile)
(export '(get-xdisplay get-window-id get-root-window-xid cmdprompt)))

(defun get-xdisplay ()
  #+wk
  (cffi:foreign-funcall "gdk_x11_get_default_xdisplay" :pointer)
  #-wk
  (cffi:foreign-funcall "gdk_x11_display_get_xdisplay"
    :pointer (cffi:foreign-funcall "gdk_display_get_default" :pointer)
    :pointer))

(defun get-window-xid (window)
  #+wk ;; GdkX11Window.get_xid GtkWidget.get_toplevel
  (let* ((top-level (gir:invoke (window "get_toplevel")))
	 (gdk-window (gir:invoke (top-level "get_window")))
	 (gdk-x11-window (gir:build-object-ptr
			  (gir:nget *gdk-x11* "X11Window")
			  (gir:this-of gdk-window))))
    (gir:invoke (gdk-x11-window "get_xid")))
  #-wk ;; GdkX11Surface.get_xid GtkNativeWidget.get_window
  (let* ((surface (gir:invoke (window "get_surface")))
	 (x11-surface (gir:build-object-ptr (gir:nget *gdk-x11* "X11Surface")
					    surface)))
    (gir:invoke (x11-surface "get_xid"))))

(defun get-root-window-xid ()
  #+wk
  (gir:invoke (*gdk-x11* "x11_get_default_root_xwindow"))
  #-wk
  (let* ((display (gir:invoke ((gir:nget *gdk* "Display") "get_default")))
	 (x11-display (gir:build-object-ptr (gir:nget *gdk-x11* "X11Display")
					(this-of display))))
    (gir:invoke (x11-display "get_xrootwindow"))))

#+nil
(progn (defvar $subprocess) (defvar $ins) (defvar $outs))

(defun cmdprompt (&key choices (xid (get-root-window-xid)) (key #'car)
		  (dmenu-cmd-line-args
		   `("dmenu" "-l" "10" "-p" "M-x" ,@(and xid `("-w" ,xid)))))
  "CHOICES is a sequence of CMD-NAME strings, or a sequence of items,
each of which produce a CMD-NAME string on application of
KEY. Presents a dmenu of choices and returns the string chosen by the
user.  XID can be nil or the X11 Window ID of the window in which
dmenu is embedded."
  (when xid
    (unless (stringp xid)
      (setq xid (write-to-string xid))))
  (when choices
    (let* ((subprocess
 	    (gir:invoke (*gio* "Subprocess" "new")
	      dmenu-cmd-line-args
	      (logior (gir:nget *gio* "SubprocessFlags" :stdin-pipe)
		      (gir:nget *gio* "SubprocessFlags" :stdout-pipe)
		      (gir:nget *gio* "SubprocessFlags" :inherit_fds)
		      (gir:nget *gio* "SubprocessFlags" :stderr-silence))))
	   (input (make-instance 'gio-input-stream :gio-input-stream
				 (gir:invoke (subprocess "get_stdout_pipe"))))
	   (output (make-instance 'gio-output-stream :gio-output-stream
				  (gir:invoke (subprocess "get_stdin_pipe"))))
	   #+(and nil (or ecl mkcl))
	   (pid (parse-integer (invoke (subprocess "get_identifier")))))
      #+nil
      (setq $subprocess subprocess $ins input $outs output)
      (flet ((dump1 (choice)
	       (let ((cmd-name (if (stringp choice)
				   choice
				   (funcall key choice))))
		 (write-line cmd-name output))))
	(map nil #'dump1 choices))
      (close output)			; reader will see EOF
      (finish-output output)
      #+(and nil (or ecl mkcl))
      (si::waitpid pid t)
      #-(and nil (or ecl mkcl))
      (gir:invoke (subprocess "wait") nil)
      (let (ret)
	(unwind-protect (handler-case
			    (setq ret (read-line input))
			  (end-of-file (c) (declare (ignore c))))
	  (close input))
	ret))))

#+nil
(cmdprompt :choices
	   '(("1" . 1) ("2") ("3") ("4"))
	   :xid nil)
