(in-package "GTK-APP")

;;; gtk-app.lisp - framework for constructing a GtkApplication.  Your
;;; class should inherit gtk-application-mixin.  Define an ACTIVATE
;;; method to set up the main-window in response to the "activate"
;;; signal. You should supply an application-id (for the dbus name)
;;; when creating an instance.
;;;
;;; To use GtkBuilder also inherit gtk-application-builder-mixin (make
;;; sure it appears earlier in the class hierarchy than
;;; gtk-application-mixin).  Define an ACTIVATE-WITH-BUILDER method to
;;; set up the main window, other ui components, and callbacks.
;;; Supply :ui-path :main-window-id and :main-window-gir-name when
;;; creating an instance. (the latter two must match the values in the
;;; XML file)


(defvar *gtk-applications* (make-hash-table :test #'equal)
  "Hashtable of application-id => lisp application object.")

(defclass gtk-application-mixin ()
  ((app)
   (application-id :initarg :application-id)
   (container-view)
   (activate-id)
   (status)))

(defmethod activate ((self gtk-application-mixin))
  (error "define (activate gtk-application-mixin) to initialize
container-view"))

(defmethod activate :around ((self gtk-application-mixin))
  (when (next-method-p) (call-next-method))
  (with-slots (container-view) self
    (gir:invoke (container-view "show_all"))))

(defmethod run ((self gtk-application-mixin))
  (with-slots (app status) self
    (with-gtk-thread
      (let ((errorp t))
	(unwind-protect (progn (setq status (gir:invoke (app "run") nil))
			       (setq errorp nil))
	  (when errorp
	    (let ((id (gir:invoke (app "get_application_id"))))
	      (quit self)
	      (remhash id *gtk-applications*))))))))

(defmethod quit ((self gtk-application-mixin))
  (with-slots (app status) self
    (with-gtk-thread
      (gir:invoke (app "quit")))))

(defun gtk-application-activate-callback (app)
  (format t "my gtk application activate callback~&")
  (let ((self (let ((id (gir:invoke (app "get_application_id"))))
	 (gethash id *gtk-applications*))))
    (activate self)))

(defmethod initialize-instance :after ((self gtk-application-mixin) &key)
  (with-slots (application-id app activate-id) self
    (check-type application-id string  "must supply application-id")
    (assert (not (gethash application-id *gtk-applications*)))
    (setf (gethash application-id *gtk-applications*) self)
    (setq app
	  (gir:invoke ( *gtk* "Application" "new" )
		      application-id
		      ;;(gir:nget *gio* "ApplicationFlags" :flags_none)
		      (gir:nget *gio* "ApplicationFlags" :non_unique)
		      ))
    (setq activate-id (gir:connect app "activate"
				   'gtk-application-activate-callback))))

(defun builder-get-object (builder widget-id widget-gir-name)
  (let ((obj (gir:invoke (builder "get_object") widget-id)))
    (let ((ptr (gir::this-of obj)))
      (gir::build-object-ptr (gir:nget *gtk* widget-gir-name) ptr))))

(defmethod activate-with-builder ((self gtk-application-mixin) builder &key
				  (main-window-id "window")
				  (main-window-gir-name "Window"))
  "Default method looks for a GtkWindow with id \"window\" to set as
the top-level window for the application."
  (with-slots (container-view) self
    (setq container-view (builder-get-object builder
					  main-window-id
					  main-window-gir-name))))

(defclass gtk-application-builder-mixin ()
  ((builder-ui-path :initarg :ui-path)
   (main-window-id :initform "window" :initarg :main-window-id)
   (main-window-gir-name :initform "Window" :initarg :main-window-gir-name)))

(defmethod activate ((self gtk-application-builder-mixin))
  "calls activate-with-builder, which override"
  (with-slots (app builder-ui-path container-view
		   main-window-id main-window-gir-name)
      self
    (let ((builder (gir:invoke (*gtk* "Builder" "new"))))
      (gir:invoke (builder "add_from_file")
	      (namestring (truename builder-ui-path)))
      (activate-with-builder self builder
			     :main-window-id main-window-id
			     :main-window-gir-name main-window-gir-name))))

;;#+nil
(gir-lib:start-gtk-thread)

;;first!!
#+nil
(defclass example-3 (gtk-application-builder-mixin
		     gtk-application-mixin)
  ()
  (:default-initargs
   :ui-path (truename "~/h/gtk-extern/gtk-examples/builder/example-2.ui")
   :application-id "org.gtk.example3"
   :main-window-id "window"
   :main-window-gir-name "ApplicationWindow"))

#+nil
(remhash "org.gtk.example3" *gtk-applications*)

#+nil
(defvar $app (make-instance 'example-3))

#+nil
(user::undefmethod activate :after ((self example-3)))

;;additional window setup
#+nil
(defmethod activate :after ((self example-3))
  (with-slots (container-view) self
    (gir:invoke ( container-view "set_title" ) "My Window")
    (gir:invoke ( container-view "set_default_size" ) 600 400)))

#+nil
(run $app)

#+nil
(quit $app)
