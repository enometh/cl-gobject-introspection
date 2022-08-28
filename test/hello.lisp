(cl:defpackage #:gir-test
  (:use #:cl))
(in-package #:gir-test)

(defvar *gtk* (gir:ffi "Gtk" #-wk "4.0" #+wk "3.0"))

#-wk
(defvar *glib* (gir:require-namespace "GLib"))
#-wk
(defvar *done* nil)

(cffi:defcallback hello :void ((btn-ptr :pointer))
  (let ((button (gir::build-object-ptr (gir:nget *gtk* "Button") btn-ptr)))
    (setf (gir:property button 'label) "OK"))
  (format t "Hello, pressed~%"))

(defun main ()
  (gir:invoke (*gtk* 'init) #+wk nil)
  (let ((window (gir:invoke (*gtk* "Window" 'new)
		             #+wk
			    (gir:nget *gtk* "WindowType" :toplevel)))
        (button (gir:invoke (*gtk* "Button" 'new-with-label) "Hello, world!"))
	#-wk
	(context (gir:invoke (*glib* "main_context_default")))
	)
    #+wk
    (gir::g-signal-connect-data (gir::this-of window)
                                "destroy"
                                (cffi:foreign-symbol-pointer "gtk_main_quit")
                                (cffi:null-pointer)
                                (cffi:null-pointer)
                                0)

    #-wk
    (gir:connect window "unrealize"
		 (lambda (&rest args)
		   (setq *done* t)
		   (gir:invoke (context "wakeup"))))

    ;; (gir::g-signal-connect-data (gir::this-of button)
    ;;                             "clicked"
    ;;                             (cffi:callback hello)
    ;;                             (cffi:null-pointer)
    ;;                             (cffi:null-pointer)
    ;;                             0)

    (gir:connect button :clicked
                 (lambda (button)
                   (setf (gir:property button 'label) "OK")
		   (gir:invoke (window "close"))))

    (gir:invoke (window #+wk 'add #-wk "set_child") button)
    (gir:invoke (window #+wk 'show-all #-wk "show"))

    #+wk
    (gir:invoke (*gtk* 'main))
    #-wk
    (loop initially  (setq *done* nil) while (not *done*)
	  do (gir:invoke (context "iteration") t))))

