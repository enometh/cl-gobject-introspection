(defpackage "EXAMPLE-TEMPLATE-GTK-5"
  (:use "CL" "GIR-LIB" "GIR" "GTK-APP"))
(in-package "EXAMPLE-TEMPLATE-GTK-5")

(defvar +example-5-ui+ (concatenate 'string
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<interface>
  <template class=\"Example5AppWindow\" parent=\"GtkApplicationWindow\">
    <property name=\"title\" translatable=\"yes\">Example Application</property>
    <property name=\"default-width\">600</property>
    <property name=\"default-height\">400</property>"
#+wk
"
    <signal name=\"destroy\" handler=\"example5_close_request_cb\" swapped=\"no\"/>"
#-wk
"
    <signal name=\"close-request\" handler=\"example5_close_request_cb\" swapped=\"no\"/>"
"
    <child>
      <object class=\"GtkBox\" id=\"content_box\">
        <property name=\"orientation\">vertical</property>
        <child>
          <object class=\"GtkStack\" id=\"stack\"/>
        </child>
      </object>
    </child>
  </template>
</interface>
"))

(defun example-5-close-request-cb (window)
  (format t "close-request-cb ~S~&" window)
  #+nil
  (gir:invoke (window "destroy"))
  nil)

#+nil
(gir:generate-cffi-defcallback (gir:info-of (gir:get-signal-desc (gir:nget gir-test::*gtk* "Window") "destroy")) 'example-5-close-request-cb)

#+wk
(CFFI:DEFCALLBACK EXAMPLE-5-CLOSE-REQUEST-CB-CALLBACK :VOID
    ((SELF :POINTER) (DATA :POINTER))
  "ARGS:  <SELF>"
  (DECLARE (IGNORABLE DATA))
  (AND (FBOUNDP 'EXAMPLE-5-CLOSE-REQUEST-CB)
       (EXAMPLE-5-CLOSE-REQUEST-CB
	(UNLESS (CFFI-SYS:NULL-POINTER-P SELF)
	  (GIR::GOBJECT (GTYPE SELF) SELF)))))

#+nil
(gir:generate-cffi-defcallback (gir:info-of (gir:get-signal-desc (gir:nget gir-test::*gtk* "Window") "close-request")) 'example-5-close-request-cb)

#-wk
(CFFI:DEFCALLBACK EXAMPLE-5-CLOSE-REQUEST-CB-CALLBACK
    :BOOLEAN
    ((SELF :POINTER) (DATA :POINTER))
  "ARGS:  <SELF>"
  (DECLARE (IGNORABLE DATA))
  (EXAMPLE-5-CLOSE-REQUEST-CB
   (UNLESS (CFFI-SYS:NULL-POINTER-P SELF)
     (GIR::GOBJECT (GIR:GTYPE SELF) SELF))))

(defvar $Example-5-App-Window-subclassable-info-obj
  (make-instance 'gir:gobject-subclassable-info
	    :gir-name "Example5AppWindow"
	    :gir-parent (gir:nget gir-test::*gtk* "ApplicationWindow")))


(eval (gir::%gobject-subclassable-define-cstructs
       $Example-5-App-Window-subclassable-info-obj))

(eval
 (gir::%gobject-subclassable-define-init-callbacks
  $Example-5-App-Window-subclassable-info-obj))

(defun EXAMPLE5-APP-WINDOW-INIT-LISP (win)
  (cffi:foreign-funcall "gtk_widget_init_template" :pointer win :void))

(defun EXAMPLE5-APP-WINDOW-CLASS-INIT-LISP (klass)
  (let ((gbytes (gir:invoke ((gir:invoke (*glib* "String" "new")

					 +example-5-ui+)
			     "free_to_bytes")))
	(callback-symbol-alist
	 '(("example5_close_request_cb"
	    EXAMPLE-5-CLOSE-REQUEST-CB-CALLBACK)))
	#-wk
	(scope (gir:invoke (gir-test::*gtk* "BuilderCScope" "new"))))

    (cffi:foreign-funcall "gtk_widget_class_set_template"
			  :pointer klass
			  :pointer (this-of gbytes)
			  :void)

    #-wk
    (cffi:foreign-funcall "gtk_widget_class_set_template_scope"
			  :pointer klass
			  :pointer (this-of scope)
			  :void)

    #-wk
    (update-builder-scope scope callback-symbol-alist)

    #+wk
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
	  (cffi:foreign-funcall "gtk_widget_class_bind_template_callback_full"
	    :pointer klass
	    :string callback-name
	    :pointer cffi-callback-address
	    :void))))

(gir::%gobject-subclassable-register-type-dynamic
 $Example-5-App-Window-subclassable-info-obj)

(defclass example-5
    (gtk-app:gtk-application-builder-mixin ;first!!
     gtk-app:gtk-application-mixin)
  ()
  (:default-initargs
   :application-id "org.gtk.example5"
   :main-window-id nil			;<-- important
   :main-window-gir-name (gethash "Example5AppWindow" gir:*registered-types*)))

(defvar $app-5 nil)

#+nil
(with-gtk-thread (setq $app-5 (make-instance 'example-5)))

#+nil
(run-safe $app-5)

#+nil
(register $app-5)

#+nil
(quit $app-5)