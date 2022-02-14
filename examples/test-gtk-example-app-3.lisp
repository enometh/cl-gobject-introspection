(defpackage "TEST-GTK-EXAMPLE-APP-3"
  (:use "CL" "GIR-LIB" "GIR" "GTK-APP"))
(in-package "TEST-GTK-EXAMPLE-APP-3")

(defun default-ui-file-path (source)
  (make-pathname :type "ui"
		 :name (pathname-name source)
		 :defaults source))

(defparameter $builder-file
  (let ((path (or *compile-file-pathname* *load-pathname*)))
    (assert path)
    (default-ui-file-path path)))

(warn "builder-file set to ~S" $builder-file)

(defclass test-gtk-example-app-3
    (gtk-app:gtk-application-builder-mixin ;first!!
     gtk-app:gtk-application-mixin)
  ()
  (:default-initargs
   :ui-path $builder-file
   :application-id "org.gtk.example.test-gtk-example-app-3"
   :main-window-id "window"
   :main-window-gir-name "ApplicationWindow"))

(defun close-request-cb (window)
  (format t "close-request-cb ~S~&" window)
  #+nil
  (invoke (window "destroy")))

#+nil
(gir:generate-cffi-defcallback (info-of (get-signal-desc (nget gir-test::*gtk* "Window") "close-request")) 'close-request-cb)

(CFFI:DEFCALLBACK CLOSE-REQUEST-CB-CALLBACK
    :BOOLEAN
    ((SELF :POINTER) (DATA :POINTER))
  "ARGS:  <SELF>"
  (DECLARE (IGNORABLE DATA))
  (CLOSE-REQUEST-CB
   (UNLESS (CFFI-SYS:NULL-POINTER-P SELF)
     (GIR::GOBJECT (GTYPE SELF) SELF))))

;;additional window setup
(defmethod activate-with-builder :after ((self test-gtk-example-app-3)
					 builder &key)
  (let ((quit-button (builder-get-object builder "quit" "Button")))
    (gir:connect quit-button "clicked"
		 (lambda (button)
		   (declare (ignorable button))
		   (format t "clicked-callback~&")
		   (quit self)))))

#+nil
(defvar $app-3 nil)

#+nil
(gir-lib:with-gtk-thread
  (setq $app-3
	(make-instance 'test-gtk-example-app-3
	  :application-id "org.gtk.example.test-gtk-example-app-3"
	  :callback-symbol-alist
	  (list (list
		 ;; name in .ui
		 "close_request_cb"
		 ;; cffi-foreign-pointer
		 (cffi:callback close-request-cb-callback))))))

#+nil
(gtk-app:run-safe $app-3)

#+nil
(gtk-app:quit $app-3)

#+nil
(gtk-app::really-quit $app-3)
