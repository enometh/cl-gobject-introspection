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
  ;; direct method must set container-view
  (when (next-method-p) (call-next-method))
  (with-slots (app container-view) self
    ;; do this anyway: in case the GtkApplicationWindow is created
    ;; without an "application" property
    (gir:invoke (app "add_window") container-view)
    (gir:invoke (container-view "show_all"))))


;; call g_application_run which is documented to take control of the
;; main context. This call blocks. When called within gtk-thread-main
;; the thunk won't return until the run loop exits.

(defmethod run ((self gtk-application-mixin))
  (with-slots (app status) self
    (with-gtk-thread
      (let ((errorp t))
	(unwind-protect (progn (setq status (gir:invoke (app "run") nil))
			       (setq errorp nil))
	  (when errorp
	    (format t "error running ~S:~&" self))
	  (let ((id (gir:invoke (app "get_application_id"))))
	    (format t "removing id=~S~&" id)
	    (remhash id *gtk-applications*)))))))

;; calling g_application_quit without destroying the window leaves the
;; window open and it leaves a dbus object for the open window. Our
;; quit method will just close the toplevel window and hope that it
;; tears down the application.

(defmethod quit ((self gtk-application-mixin))
  (with-slots ((win container-view) app) self
    (with-gtk-thread
      (let ((errorp t))
	(unwind-protect (progn (when (slot-boundp self 'container-view)
				 (gir:invoke (win "close")))
			       (setq errorp nil))
	  (when errorp
	    (format t "error quitting ~S:~&" self))
	  (when nil
	    ;; ;madhu 190731 this is required to relinquish the dbus name
	    (format t "unref ~S ~S~&" self (gir::this-of app))
	    (gir::g-object-unref (gir::this-of app))))))))

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
		      (logior
		       (gir:nget *gio* "ApplicationFlags" :flags_none)
		       (gir:nget *gio* "ApplicationFlags" :non_unique))
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

(start-gtk-thread)

;;;
;;;
;;;

(defclass example-1 (gtk-application-mixin)
  ()
  (:default-initargs
   :application-id "org.gtk.Example1"))

(defmethod activate ((app example-1))
  (with-slots ((win container-view) app) app
    (setq win (gir:invoke (*gtk* "ApplicationWindow" "new") app))
    (gir:invoke ( win "set_title" ) "Example1")
    (gir:invoke ( win "set_default_size" ) 600 400)))

#+nil
(defvar $app-1 nil)

#+nil
(with-gtk-thread (setq $app-1  (make-instance 'example-1)))

#+nil
(run $app-1)

#+nil
(quit $app-1)

#||
(gir:invoke ((slot-value $app-1 'app) "get_dbus_object_path"))
(gir:invoke ((slot-value $app-1 'app) "get_dbus_connection"))
(remhash "org.gtk.Example1" *gtk-applications*)
(gir:invoke ((slot-value $app-1 'app) "get_is_registered"))
(gir:invoke ((slot-value $app-1 'app) "get_is_remote"))
(gir:invoke ((slot-value $app-1 'app) "get_application_id"))
(gir:invoke ((slot-value $app-1 'container-view) "present"))
(gir:invoke ((slot-value $app-1 'app) "quit"))
(gir::g-object-unref (gir::this-of (slot-value $app-1 'app)))

||#



;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defclass gtk-application-builder-mixin ()
  ((builder-ui-path :initarg :ui-path :initform nil)
   (builder-ui-string :initarg :ui-string :initform nil)
   (main-window-id :initform "window" :initarg :main-window-id)
   (main-window-gir-name :initform "Window" :initarg :main-window-gir-name)))

(defmethod activate ((self gtk-application-builder-mixin))
  "calls activate-with-builder, which override"
  (with-slots (app builder-ui-path builder-ui-string container-view
		   main-window-id main-window-gir-name)
      self
    (let ((builder (gir:invoke (*gtk* "Builder" "new"))))
      (or (and builder-ui-path
	       (gir:invoke (builder "add_from_file")
			   (namestring (truename builder-ui-path))))
	  (and builder-ui-string
	       (gir:invoke (builder "add_from_string")
			   builder-ui-string
			   (length builder-ui-string)))
	  (error "builder-ui not supplied with either :ui-path or :ui-string"))
      (activate-with-builder self builder
			     :main-window-id main-window-id
			     :main-window-gir-name main-window-gir-name))))

;;#+nil
(gir-lib:start-gtk-thread)

(defvar +example-3-ui+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<interface>
  <requires lib=\"gtk+\" version=\"3.20\"/>
  <object class=\"GtkApplicationWindow\" id=\"window\">
    <property name=\"can_focus\">False</property>
    <child>
      <object class=\"GtkBox\">
        <property name=\"visible\">True</property>
        <property name=\"can_focus\">False</property>
        <property name=\"orientation\">vertical</property>
        <child>
          <object id=\"quit\" class=\"GtkButton\">
            <property name=\"visible\">True</property>
            <property name=\"label\">Quit</property>
          </object>
        </child>
     </object>
    </child>
  </object>
</interface>
")

(defclass example-3 (gtk-application-builder-mixin ;first!!
		     gtk-application-mixin)
  ()
  (:default-initargs
   :ui-string +example-3-ui+
   :application-id "org.gtk.example3"
   :main-window-id "window"
   :main-window-gir-name "ApplicationWindow"))

;;additional window setup
(defmethod activate-with-builder :after ((self example-3) builder &key)
  (let ((quit-button (builder-get-object builder "quit" "Button")))
    (gir:connect quit-button "clicked" (lambda (button)
				     (declare (ignorable button))
				     (quit self))))
  (with-slots (container-view) self
    (gir:invoke ( container-view "set_title" ) "Example-3")
    (gir:invoke ( container-view "set_default_size" ) 600 400)))


#+nil
(defvar $app-3 (make-instance 'example-3))

#+nil
(run $app-3)

#+nil
(quit $app-3)


#||
(gir:get-method-desc (gir:nget *glib* "SList") "length")
(setq $sl (gir::build-struct-ptr (gir:nget *glib* "SList") $l))
(gir:invoke ($sl "length"))

(gir:get-method-desc (gir:nget *gio* "Application") "get_dbus_connection")
(gir:get-method-desc (gir:nget *gio* "Application") "get_dbus_object_path")
(gir:get-method-desc (gir:nget *gtk* "Builder") "add_from_string")
(gir:invoke ((slot-value $app 'app) "get_is_registered"))
(gir:invoke ((slot-value $app 'app) "get_is_remote"))
(gir:invoke ((slot-value $app 'app) "get_dbus_object_path"))
(setq $l (gir:invoke ((slot-value $app 'app) "get_windows")))
(gir::g-object-unref (gir::this-of (slot-value $app 'app)))
(with-gtk-thread (let ((app (make-instance 'example-3)))
		   (run app)))
(activate $app)
(user::undefmethod activate :after ((self example-3)))
(remhash "org.gtk.example3" *gtk-applications*)
(with-gtk-thread (setq $app (make-instance 'example-3)))
||#

