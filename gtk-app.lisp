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

(defclass titled-object ()
  ((title :initarg :title :initform nil :accessor titled-object-title)))

(defclass element ()
  ((interface :initform nil :accessor element-interface)
   (best-height :initarg :best-height :initform nil)
   (best-width :initarg :best-width :initform nil)))

(defclass simple-pane (element)
  ((container-view :initform nil)))

;; subclasses of simple-pane should set container-view (which is the
;; GtkWidget GOBject) in the initialize-instance :before method. The
;; following :after method defaults to initializing it to a GtkWindow
;; if a subclass has not initialized it.

(defmethod initialize-instance :after ((simple-pane-1 simple-pane) &key)
  (with-slots (container-view best-height best-width) simple-pane-1
    (unless container-view
      (setq container-view
	    (gir:invoke (*gtk* "Window" "new")
			#+wk
			(gir:nget *gtk* "WindowType" :toplevel))))
    (when (and best-width best-width)
      (gir:invoke ( container-view "set_default_size" )
		  best-width best-height))))

(defclass interface (titled-object simple-pane) ())

(defmethod initialize-instance :after ((intf-1 interface) &key)
  (with-slots (container-view title) intf-1
    (when (stringp title)
      (gir:invoke (container-view "set_title") title))))

(defmethod interface-display ((intf-1 interface))
  (with-slots (container-view) intf-1
    #+(and clisp (not mt))
    (gir-lib::clisp-single-thread-register-destroy-handler container-view)
    #-wk
    (progn
      (gir:invoke (container-view "unminimize"))
      (gir:invoke (container-view "present_with_time")
	      (- (get-universal-time) cl-user:+unix-epoch+)))
    #+wk
    (gir:invoke (container-view "show_all"))
    #+(and clisp (not mt))
    (gir-lib::clisp-single-thread-run-loop container-view)))

(defun display (intf-1)
  (with-gtk-thread (interface-display intf-1)))

(defmethod make-container ((simple-pane-1 simple-pane) &rest interface-args)
  (let ((container (apply #'make-instance 'interface interface-args)))
    (with-slots (container-view) container
      (check-type container-view gir::object-instance)
      (let ((obj-gtype (gir::gtype-of container-view)))
	(assert (list (gir:invoke (*gobject* "type_is_a")
				  obj-gtype
				  (gir::gtype-of (gir:nget *gtk* "Window"))))))
      (gir:invoke (container-view #+wk "add"
				  #-wk "set_child")
		  (slot-value simple-pane-1 'container-view)))
    container))

(defun contain (element &rest interface-args &key title &allow-other-keys)
  (declare (ignorable title))
  (let ((container (element-interface element)))
    (unless container
      (setq container (apply #'make-container element interface-args))
      (setf (element-interface element) container)
      (with-slots (container-view) container
	#+(and clisp (not mt))
	(gir-lib::clisp-single-thread-register-destroy-handler container-view)
	(gir:connect container-view "destroy"
		     (lambda (window)
		       (declare (ignorable window))
		       (setf (element-interface element) nil)
		       #+(and clisp (not mt))
		       (gir-lib::clisp-single-thread-run-loop window)))))
    (display container)
    element))

(defclass title-pane (titled-object simple-pane)
  ((text :initarg :text :accessor title-pane-text :initform nil)))

(defmethod initialize-instance :before ((title-pane-1 title-pane) &key text)
  (with-slots (container-view) title-pane-1
    (unless (slot-boundp title-pane-1 'container-view)
      (setq container-view
	    (gir:invoke (*gtk* "Label" "new") text)))))

#+nil
(defclass hello-world (interface)
  ()
  (:default-initargs
   :best-height 400
   :best-width 600
   :title "Example-0"))

#+nil
(gir-lib:start-gtk-thread)

#+nil					; in a multi-threaded lisp:
(display (make-instance 'hello-world))

#+nil
(contain (setq $a (make-instance 'title-pane :text "foo bar"))
	 :title "Title Pane Container"
	 :best-height 200
	 :best-width 300)

#||
(gir:enum-value-to-string *gtk*  "SizeRequestMode" (gir:invoke ((slot-value $a 'container-view) "get_request_mode")))
(gir:invoke ((slot-value $a 'container-view) "get_size_request"))
(gir:invoke ((slot-value $a 'container-view) "get_width"))
||#


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defvar *gtk-applications* (make-hash-table :test #'equal)
  "Hashtable of application-id => lisp application object.")

(defclass gtk-application-mixin (interface)
  ((app)
   (application-id :initarg :application-id)
   (activate-id)
   (status)))

;; ACTIVATE SHOULD NOT BE CALLED DIRECTLY
(defmethod activate ((self gtk-application-mixin))
  "Default direct method should set the container view"
  (with-slots ((win container-view) app) self
    (setq win (gir:gobject-new (gir:gtype-of
				(gir:nget *gtk* "ApplicationWindow"))
			       "application"
			       (gir::this-of app)))))

(defmethod activate :around ((self gtk-application-mixin))
  ;; direct method must set container-view
  (when (next-method-p) (call-next-method))
  (with-slots (app container-view) self
    ;; do this anyway: in case the GtkApplicationWindow is created
    ;; without an "application" property
    (gir:invoke (app "add_window") container-view))
  (interface-display self))


;; call g_application_run which is documented to take control of the
;; main context. This call blocks. When called within gtk-thread-main
;; the thunk won't return until the run loop exits.

(defmethod run ((self gtk-application-mixin))
  "This calls g_application_run which usually puts lisp in an unusable
state. See RUN-SAFE instead."
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
  "This closes the application window. See Also: REALLY-QUIT"
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
    (with-simple-restart (replace "Replace it in the table")
      (assert (not (gethash application-id *gtk-applications*))))
    (setf (gethash application-id *gtk-applications*) self)
    (setq app
	  (gir:invoke ( *gtk* "Application" "new" )
		      application-id
		      (logior
		       (gir:nget *gio* "ApplicationFlags" :flags_none)
		       (gir:nget *gio* "ApplicationFlags" :replace)
		       (gir:nget *gio* "ApplicationFlags" :allow-replacement)
		       (gir:nget *gio* "ApplicationFlags" :non-unique))
		      ))
    (setq activate-id (gir:connect app "activate"
				   'gtk-application-activate-callback))))

(defmethod really-quit ((self gtk-application-mixin))
  "GApplication is said to be useless after invoking this. Should also
unregister from D-Bus. But it doesn't. g_application_quit merely quits
the main loop of run if it is running and the state machine is already
wedged."
  (with-slots (app) self
    (let ((id (gir:invoke (app "get_application_id"))))
      (gir:invoke (app "quit"))
      (gir::g-object-unref (gir::this-of app))
      (remhash id *gtk-applications*))))

(defmethod register ((self gtk-application-mixin))
  (with-slots (app) self
    (or (gir:invoke (app "get_is_registered"))
	(gir:invoke (app "register") nil))))

(defmethod run-safe ((self gtk-application-mixin))
  "Invoke REGISTER and ACTIVATE"
  (gir-lib:with-gtk-thread
    (unwind-protect
	 (with-slots (app) self
	   (cond ((register self)
		  (format t "ARRANGING TO CALL ACTIVATE~&")
		  (gir:invoke (app "activate")))
		 (t (format t "RUN-SAFE: not registered~&"))))
	   #+nil
      (format t "error run-safe ~S:~&" self))))

(defun builder-get-object (builder widget-id &optional widget-gir-name)
  "If WIDGET-GIR-NAME is given use old logic to construct an object in
the Gtk namespace with that name. Otherwise construct a gobject which
may not be in g-i repository but which may have been registered
outside g-i repository."
  (check-type widget-id string)
  (let ((obj (gir:invoke (builder "get_object") widget-id)))
    (let ((ptr (gir::this-of obj)))
      (if (stringp widget-gir-name)
	  (gir::build-object-ptr (gir:nget *gtk* widget-gir-name) ptr)
	  (gir::%gobject ptr)))))

(defmethod activate-with-builder ((self gtk-application-mixin) builder &key
				  (main-window-id "window"
						  main-window-id-supplied-p)
				  (main-window-gir-name
				   "ApplicationWindow"
				   main-window-gir-name-supplied-p)
				  key-val-props)
  "Default values of parameters look for a \"GtkApplicationWindow\" with
id \"window\" to set as the top-level window for the application.

If the MAIN-WINDOW-ID is specified through a builder spec then
MAIN-WINDOW-GIR-NAME is looked up in the Gtk namespace.

But we need not require that object be specified through the builder.
In that case pass a value of NIL for MAIN-WINDOW-ID and pass in either
the GType or the global type name of the required object through
for MAIN-WINDOW-GIR-NAME.

If a wrong type name lisp is supplied *LISP* *WILL* *ABORT*.
"
  (assert main-window-id-supplied-p)
  (assert main-window-gir-name-supplied-p)
  (check-type main-window-id (or null string))
  (with-slots (container-view) self
    (setq container-view
	  (if main-window-id
	      (builder-get-object builder
				  main-window-id
				  main-window-gir-name)
	      (let ((gtype
		     (etypecase main-window-gir-name
		       (integer main-window-gir-name)
		       (string
			(cffi:foreign-funcall "g_type_from_name"
					      :string main-window-gir-name
					      :ulong)))))
		(apply #'gir:gobject-new gtype key-val-props))))))

(start-gtk-thread)

;;;
;;;
;;;

(defclass example-1 (gtk-application-mixin)
  ()
  (:default-initargs
   :application-id "org.gtk.Example1"))

(defmethod activate :after  ((app example-1))
  (with-slots ((win container-view) app) app
    (gir:invoke ( win "set_title" ) "Example1")
    (gir:invoke ( win "set_default_size" ) 600 400)))

#+nil
(defvar $app-1 nil)

#+nil
(with-gtk-thread (setq $app-1 (make-instance 'example-1)))

#+nil
(run-safe $app-1)

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

;; callback-symbol-alist is a list of (string_name callback-symbol) where
;; `string_name' appears in the UI XML and will resolve to a function
;; which is handled by the function `callback-symbol'
;;
;; use (gir:generate-cffi-defcallback SINGAL-INFO NAME) to generate a
;; callback function NAME-CALLBACK and (defun NAME) to implement the
;; callback.
;;
;; when gtk builder is initialized it can expect to process a
;; <signal name=\"clicked\" handler=\"example_4_clicked_cb\"/>
;;
;; provided that the following has happened
;;
;; (gir:generate-cffi-defcallback
;; (gir:info-of (gir:get-signal-desc (gir:nget  *gtk* "Button") "clicked"))
;;  'example-4-clicked-cb)
;;
;; (defun example-4-clicked-cb (button) ... )
;;
;; and the alist has an entry
;; '("exmaple_4_clicked_cb" example-4-clicked-cb-callback)

(defun update-builder-scope (scope callback-symbol-alist)
  "scope is a GtkBuilderScope"
  (loop for (callback-name callback-symbol) in callback-symbol-alist
	  for cffi-callback-address =
	  (if (cffi:pointerp callback-symbol)
	      callback-symbol
	      (let (ret)
		(assert (symbolp callback-symbol))
		(setq ret (cffi:get-callback callback-symbol))
		(assert ret)
		ret))
	  do
	  (gir:invoke (scope "add_callback_symbol")
		      callback-name cffi-callback-address)))

;; builder-ui-path and builder-ui-string can be lists of paths and
;; strings the ACTIVATE method will add all of them.

(defclass gtk-application-builder-mixin ()
  ((builder-ui-path :initarg :ui-path :initform nil)
   (builder-ui-string :initarg :ui-string :initform nil)
   (main-window-id :initform "window" :initarg :main-window-id)
   (main-window-gir-name :initform "Window" :initarg :main-window-gir-name)
   (callback-symbol-alist :initform nil :initarg :callback-symbol-alist)))

(defmethod activate ((self gtk-application-builder-mixin))
  "calls activate-with-builder, which override"
  (with-slots (app builder-ui-path builder-ui-string container-view
		   main-window-id main-window-gir-name
		   callback-symbol-alist)
      self
    (let ((builder (gir:invoke (*gtk* "Builder" "new"))))
      (when callback-symbol-alist
	(update-builder-scope (gir:property builder "scope")
			      callback-symbol-alist))
      (when builder-ui-path
	(dolist (path (if (consp builder-ui-path)
			  builder-ui-path
			  (list builder-ui-path)))
	  (gir:invoke (builder "add_from_file")
		      (namestring (truename path)))))
      (when builder-ui-string
	(dolist (string (if (consp builder-ui-string)
			    builder-ui-string
			    (list builder-ui-string)))
	  (gir:invoke (builder "add_from_string")
		      string  (length string))))
      (unless (or builder-ui-path builder-ui-string)
	(warn "builder-ui not supplied via either :ui-path or :ui-string"))
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
(defvar $app-3 nil)

#+nil
(with-gtk-thread (setq $app-3 (make-instance 'example-3)))

#+nil
(run-safe $app-3)

#+nil
(quit $app-3)


#||
(user::string->file +example-3-ui+ "/dev/shm/1.ui")
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


(defvar +example-4-ui+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<interface>
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
            <signal name=\"clicked\" handler=\"example_4_clicked_cb\"/>
          </object>
        </child>
     </object>
    </child>
  </object>
</interface>
")

(defvar $app-4 nil)

(defun example-4-clicked-cb (button)
  (declare (ignore button))
  (format t "example-4-clicked-cb: quitting.~&")
  (when $app-4 (quit $app-4)))

#+nil
(gir:generate-cffi-defcallback
 (gir:info-of (gir:get-signal-desc (gir:nget  *gtk* "Button") "clicked"))
 'example-4-clicked-cb)

(CFFI:DEFCALLBACK EXAMPLE4-CLICKED-CB-CALLBACK
    :VOID
    ((SELF :POINTER) (DATA :POINTER))
  "ARGS:  <SELF>"
  (DECLARE (IGNORABLE DATA))
  (EXAMPLE-4-CLICKED-CB
   (UNLESS (CFFI-SYS:NULL-POINTER-P SELF)
     (GIR::GOBJECT (GIR:GTYPE SELF) SELF))))

(defclass example-4 (gtk-application-builder-mixin ;first!!
		     gtk-application-mixin)
  ()
  (:default-initargs
   :ui-string +example-4-ui+
   :title "Example 4"
   :application-id "org.gtk.example4"
   :main-window-id "window"
   :callback-symbol-alist
   `(("example_4_clicked_cb" example4-clicked-cb-callback))
   :main-window-gir-name "ApplicationWindow"))

#+nil
(defvar $app-4 nil)

#+nil
(with-gtk-thread (setq $app-4 (make-instance 'example-4)))

#+nil
(run-safe $app-4)

#+nil
(quit $app-4)
