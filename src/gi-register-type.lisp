(in-package "GIR")

(export '(*registered-types* register-type
	  gobject-subclassable-info
	  ;; XXX the following functions are a semi-private interface
	  ;; of helpers to define and register new gobjects
	  %gobject-subclassable-register-type-static
	  %gobject-subclassable-register-type-dynamic
	  %gobject-subclassable-define-cstructs
	  %gobject-subclassable-define-init-callbacks
	  ))

(cffi:defcstruct GTypeInfo
  ;; interface types, classed types, instantiated types
  (class-size :uint16)
  (base-init :pointer)			;GBaseInitFunc
  (base-finalize :pointer)		;GBaseFinalizeFunc
  ;; interface types, classed types, instantiated types
  (class-init :pointer)			;GClassInitFunc
  (class-finalize :pointer)		;GClassFinalizeFunc
  (class-data :pointer)
  ;; instantiated types
  (instance-size :uint16)
  (n-preallocs :uint16)
  (instance-init :pointer)		;GInstanceInitFunc
  ;;  value handling
  (value-table :pointer)		;GTypeValueTable*
  )

#+nil
(cffi:foreign-type-size '(:struct gtypeinfo))	;72

(defvar *registered-types* (make-hash-table :test #'equal))

;; BOOTSTRAP NOTE! This has to be initialized further down, after
;; defining our default type-loading module object and registering a
;; static type for it.
(defvar *default-type-loading-module* nil ;see INIT-DEFERRED
  "CFFI pointer to a GTypeModule.")

;; Register-type can register static types and dynamic types (using a
;; given GTypeModule object or using our default type-loading module
;; defined below

(defun register-type (type-name parent-gtype type-flags &key force
		      (type :static)
		      ;; for :type :dynamic
		      (type-module *default-type-loading-module*)
		      instance-struct-name class-struct-name
		      class-size base-init base-finalize
		      class-init class-finalize class-data
		      instance-size n-preallocs instance-init
		      value-table
		      )
  "If :type is :static, register a type with g_type_register_static.
If :type is :dynamic expect a GTypeModule pointer :type-module to
handle dynamic type registration.  In this case register the type with
g_type_module_register (which in turn calls g_type_register_dynamic.)"

    (when (and (eql type :dynamic)
	       (null *default-type-loading-module*))
	(init-deferred)
	(setq type-module *default-type-loading-module*))

  (let ((ret (gethash type-name *registered-types*)))
    (and ret
	 (not force)
	 (eql type :static)
	 (return-from register-type (values ret t))))
  (check-type parent-gtype integer)	;ulong

  #+nil
  (unless force				; doesn't work
    (assert (invoke (*gobject* "type_class_peek") parent-gtype) nil
	"Parent gtype not defined.")
    (assert (zerop (invoke (*gobject* "type_from_name") type-name)) nil
	"Typename already registered."))

  (unless instance-struct-name
    (setq instance-struct-name (intern (string type-name))))

  (unless class-struct-name
    (setq class-struct-name (intern
			     (concatenate 'string (string instance-struct-name)
					  "-CLASS"))))
  (unless class-size
    (setq class-size (cffi:foreign-type-size
		      (list :struct class-struct-name))))

  (unless instance-size
    (setq instance-size (cffi:foreign-type-size
			 (list :struct instance-struct-name))))

  (unless type-flags (setq type-flags 0))
  (unless n-preallocs (setq n-preallocs 0))

  (cffi:with-foreign-object (type-info '(:struct gtypeinfo))
    (cffi:foreign-funcall "memset" :pointer type-info
			  :int 0
			  :int (cffi:foreign-type-size '(:struct gtypeinfo)))

    ;; initialize the other fields of GTypeInfo
    (macrolet ((foo (field)
		 `(when ,field
		    (setf (cffi:foreign-slot-value type-info
						   '(:struct gtypeinfo)
						   ',field)
			  ,field))))
      (macrolet ((bar (&rest fields)
		   `(progn ,@(loop for field in fields
				   collect `(foo ,field)))))
	(bar
      	 class-size base-init base-finalize
	 class-init class-finalize class-data
	 instance-size n-preallocs instance-init
	 value-table)))

    (let ((gtype
	   (ecase type
	     (:static
	      (cffi:foreign-funcall
	       "g_type_register_static"
	       ;; parent-type
	       :ulong parent-gtype
	       ;; type-name
	       :string type-name
	       ;; type-info
	       :pointer type-info
	       ;; type-flags
	       :int type-flags
	       ;; return-type
	       :ulong
	       ))
	     (:dynamic
	      (assert (cffi:pointerp type-module))
	      (cffi:foreign-funcall
	       "g_type_module_register_type"
	       :pointer type-module
	       ;; parent
	       :ulong parent-gtype
	       ;; type-name
	       :string type-name
	       ;; type-info
	       :pointer type-info
	       ;; type-flags
	       :int type-flags
	       ;; return-type
	       :ulong)))))
      (assert (not (zerop gtype)) nil "Failed to register type")
      (let ((orig (gethash type-name *registered-types*)))
	(when orig
	  (if (/= orig gtype)
	      (format t "REGISTER-TYPE: ~A  orig = ~A new = ~A~&"
		      type-name orig gtype))))
      (setf (gethash type-name *registered-types*) gtype)
      gtype)))

;;;
;;; %GOBJECT-SUBCLASSABLE-INFO - collect information about gobject
;;; %types to help defining them
;;;

(defclass gobject-subclassable-info ()
  ((gir-parent :initarg :gir-parent)	;object-class
   (parent-instance-cstruct)
   (parent-class-cstruct)
   ;; each cstruct-slots-spec becomes the body of the defcstruct: so
   ;; it is a list (field-name cffi-field-type)+
   (instance-cstruct-slots-spec :initarg :instance-cstruct-slots-spec
				:initform nil)
   (class-cstruct-slots-spec :initarg :class-cstruct-slots-spec
			     :initform nil)
   (gir-name :initarg :gir-name)

   ;; if decamel-p is t then gir-name and class-name used for the
   ;; instance and class C structs are transformed to lisp style names.
   ;; (The struct names for the parent instance and class C structs are still
   ;; camel case)
   (decamel-p :initarg :decamel-p :initform nil)

   ;; vfunc-overrides - a list of cnames which are overriden.  the
   ;; user agrees to define lisp functions with the name
   ;; <GIR-NAME>-<VFUNC-NAME>-LISP. which have the same signature as
   ;; the vfunc (the C signature which is not the same as what
   ;; `get-vfunc-desc' gives.) we then generate defcallback forms
   ;; which will in turn call these user defined functions, and
   ;; register those callbacks in the generated class-init.
   (vfuncs :initarg :vfunc-overrides :initform nil)

))

;; For the first example of using REGISTER-TYPE we implement a
;; GTypeModule and register it *statically*.  We can then use that
;; module to implement the registration of dynamic types with
;; REGISTER-TYPE.

(defvar +default-type-loading-module-name+ "LispModule")

(defun make-default-dynamic-type-loading-module-prototype ()
  (make-instance 'gobject-subclassable-info
    :gir-name +default-type-loading-module-name+
    :gir-parent (let ((*override-cache* nil))
		  (nget (require-namespace "GObject") "TypeModule"))))

#+nil  ;DEFERRED
(defvar +default-type-loading-module-prototype+
  (make-default-dynamic-type-loading-module-prototype))

;;; %GOBJECT-SUBCLASSABLE helpers to define opaque C structures and
;;; init callbacks and register the type.

(defun decamel (string)
  (if (symbolp string) (setq string (symbol-name string)))
  (string-upcase
   (with-output-to-string (stream)
     (let ((i 0))
       (loop (cond ((< i (length string))
		    (cond ((upper-case-p (elt string i))
			   (cond ((= i 0) t)
				 (t (write-char #\- stream)))
			   (write-char (char-downcase (elt string i)) stream))
			  (t (write-char (elt string i) stream)))
		    (incf i))
		   (t (return))))))))

(defun %gobject-subclassable-get-name (obj what &optional fname)
  "Interns names in the current *package*!!"
  (with-slots ((parent-obj gir-parent) gir-name decamel-p) obj
    (flet ((maybe-decamel (x) (if decamel-p (decamel x) x)))
      (let* ((info (info-of parent-obj))
	     (parent-name (info-get-name info))
	     (namespace (info-get-namespace info))
	     (parent-string (concatenate 'string namespace parent-name))
	     (parent-class-string (concatenate 'string parent-string "Class"))
	     (instance-string gir-name)
	     (class-string (concatenate 'string instance-string "Class")))
	(ecase what
	  (:parent-instance-struct (intern parent-string  *package*))
	  (:parent-class-struct (intern parent-class-string *package*))
	  (:instance-struct (intern (maybe-decamel instance-string) *package*))
	  (:class-struct (intern (maybe-decamel class-string) *package*))
	  (:instance-init-callback
	   (intern (concatenate 'string (decamel instance-string) "-INIT")
		   *package*))
	  (:class-init-callback
	   (intern (concatenate 'string (decamel class-string) "-INIT")
		   *package*))
	  (:instance-arg (intern (decamel instance-string) *package*))
	  (:class-arg (intern (decamel class-string) *package*))
	  (:instance-string instance-string)
	  (:class-string class-string)
	  ((:func :method :vfunc)
	   (intern (concatenate 'string (decamel instance-string)
				"-"
				(string-upcase (substitute #\- #\_ fname)))
		   *package*)))))))

(defun %gobject-subclassable-define-cstructs (obj)
  "Return a lisp form which defines opaque cffi defstructs for the
parent's instance structure, the parent's class structure, the
objects's instance structure and object's class structure for this
subclassable"
  (with-slots ((parent-obj gir-parent)
	       instance-cstruct-slots-spec
	       class-cstruct-slots-spec)
      obj
    (let* ((parent-instance-struct-name
	    (%gobject-subclassable-get-name obj :parent-instance-struct))
	   (parent-class-struct-name
	    (%gobject-subclassable-get-name obj :parent-class-struct))
	   (instance-struct-name
	    (%gobject-subclassable-get-name obj :instance-struct))
	   (class-struct-name
	    (%gobject-subclassable-get-name obj :class-struct)))
      `(progn (cffi:defcstruct
		  (,parent-instance-struct-name
		   :size ,(g-type-query parent-obj :instance-size)))
	      (cffi:defcstruct
		  (,parent-class-struct-name
		   :size ,(g-type-query parent-obj :class-size)))
	      (cffi:defcstruct ,instance-struct-name
		(parent (:struct ,parent-instance-struct-name))
		,@instance-cstruct-slots-spec)
	      (cffi:defcstruct ,class-struct-name
		(parent (:struct ,parent-class-struct-name))
		,@class-cstruct-slots-spec)
	      T))))

(defun %gobject-subclassable-define-init-callback-form (obj)
  (let* ((callback-name (%gobject-subclassable-get-name obj :instance-init-callback))

	 (arg-name (%gobject-subclassable-get-name obj :instance-arg))
	 (gir-name (%gobject-subclassable-get-name obj :instance-string))
	 (lisp-function-name
	  (intern (concatenate 'string (symbol-name callback-name) "-LISP")
		  *package*)))
    `(cffi:defcallback ,callback-name :void ((,arg-name :pointer))
       (format t ,(format nil "~A (~A*): ~~A~~%" callback-name gir-name)
	       (list ,arg-name))
       (when (fboundp ',lisp-function-name)
	 (,lisp-function-name ,arg-name)))))

(defun %gobject-subclassable-define-class-init-callback-form (obj)
  (let* ((callback-name (%gobject-subclassable-get-name obj :class-init-callback))

	 (arg-name (%gobject-subclassable-get-name obj :class-arg))
	 (gir-name (%gobject-subclassable-get-name obj :class-string))
	 (vfunc-init-forms (with-slots (vfuncs) obj
			     (when vfuncs
			       (mapcar (lambda (x) (%generate-class-init-override obj x))
				       vfuncs))))
	 (lisp-function-name
	  (intern (concatenate 'string (symbol-name callback-name) "-LISP")
		  *package*)))
    `(cffi:defcallback ,callback-name :void ((,arg-name :pointer))
       (format t ,(format nil "~A (~A*): ~~A~~%" callback-name gir-name)
	       (list ,arg-name))
       ,@vfunc-init-forms
       (when (fboundp ',lisp-function-name)
	 (,lisp-function-name ,arg-name)))))

(defun %gobject-subclassable-define-init-callbacks (obj)
  "Return a lisp form which defines stub cffi callbacks for the class
init function and the instance init function for the subclassable"
  `(progn ,@(list
	     (%gobject-subclassable-define-init-callback-form obj)
	     (%gobject-subclassable-define-class-init-callback-form obj))))

(defun %gobject-subclassable-register-type-static (obj)
  (with-slots ((parent-obj gir-parent) gir-name) obj
    (register-type
     gir-name
     (g-type-query parent-obj nil)
     0
     :type :static
     :instance-struct-name
     (%gobject-subclassable-get-name obj :instance-struct)
     :class-struct-name
     (%gobject-subclassable-get-name obj :class-struct)
     :instance-init
     (eval `(cffi:callback ,(%gobject-subclassable-get-name
			     obj :instance-init-callback)))
     :class-init
     (eval `(cffi:callback ,(%gobject-subclassable-get-name
			     obj  :class-init-callback))))))

;;; Realise our default type loading module:

;;  Define the structs
#+nil					;DEFERRED
(eval
 (%gobject-subclassable-define-cstructs
  +default-type-loading-module-prototype+))

;; Implement the GTypeModule virtual function methods for our type
;; module loader.  We are not loading shared libraries -- this is a
;; dummy implementation.
;;
(cffi:defcallback default-type-loading-module-load :boolean
    ((module :pointer))
  (format t "~A: load ~A: return T~%" +default-type-loading-module-name+ module)
  t)

(cffi:defcallback default-type-loading-module-unload :void
    ((module :pointer))
  (format t "~A: unload ~A~%" +default-type-loading-module-name+ module))

;; implement the init callbacks:
#+nil
(eval
 (%gobject-subclassable-define-init-callbacks
  +default-type-loading-module-prototype+))

;; implement the instance init callback
(CFFI:DEFCALLBACK LISP-MODULE-INIT :VOID ((LISP-MODULE :POINTER))
  (FORMAT T "LISP-MODULE-INIT (LispModule*): ~A~%" (LIST LISP-MODULE)))

;; the class init callback needs to override `load' and `unload'
;; virtual methods of the GTypeModule struct. However since glib fails
;; at finding the correct offsets within the class struct i.e.

#+nil
(progn
(multiple-value-setq ($fi_l $oi_l)
  (gir:object-info-find-vfunc-using-interfaces
   (gir::info-of (nget gir-test::*gobject* "TypeModule"))
   "load"))
(gir::info-get-name $oi_l)
(gir::info-get-namespace $oi_l)
(gir::vfunc-info-get-offset $fi_l)	;65535 fail!
(gir::vfunc-info-get-address $fi_l (gtype-of $oi_l)) ; NULL ptr FAIL
;; if that worked we could subtract the address from the following to
;; get the offset
(cffi:pointer-address
 (gir::this-of (invoke (gir-lib::*gobject* "type_class_peek") (gir::gtype-of (nget gir-lib::*gobject* "TypeModule")))))
(function-info-get-vfunc $fi_l)
)

;; we instead need to go the c2ffi route and extract the offsets of
;; the `load' and `unload' fields in GTypeModule's class struct.

#+nil
(progn
(require 'jsown)
(user::lc "cl/c2ffi-query-struct")
(use-package :c2ffi-query-struct)
(time (c2ffi-ensure-parsed-defs
       "exampleapp"
       :header "/usr/include/gtk-3.0/gtk/gtk.h"
;;       :arch "i686-pc-linux-gnu"
       :pkgs "gtk+-3.0"))
(c2ffi-query-struct "_GTypeModule" "name" "bit-offset") ;((384))
(c2ffi-struct-alias "GTypeModuleClass")
(c2ffi-query-struct "_GTypeModuleClass" "load" "bit-offset") ;((1088))
(c2ffi-query-struct "_GTypeModuleClass" "unload" "bit-offset") ;((1152))
#+(and linux x86-64)
(progn
(defvar +type-module-class-offset-load+ (/ 1088 8))
(defvar +type-module-class-offset-unload+ (/ 1152 8)))
#+(and linux x86)
(progn
(defvar +type-module-class-offset-load+ (/ 544 8))
(defvar +type-module-class-offset-unload+ (/ 576 8)))
)

;; or the simple solution that we had all along: If we know the name
;; of the struct-class and get a struct-class object.
(defun find-class-struct-offset (struct-class cname)
  (field-info-get-offset
   (struct-class-find-field struct-class cname)))

#+nil
(find-class-struct-offset (nget gir-test::*gobject* "TypeModuleClass") "load")

(defvar +type-module-class-offset-load+
 (load-time-value
  (find-class-struct-offset (nget (require-namespace "GObject") "TypeModuleClass")
			    "load")))

(defvar +type-module-class-offset-unload+
 (load-time-value
  (find-class-struct-offset (nget (require-namespace "GObject") "TypeModuleClass")
			    "unload")))

;; and now that we have the offsets we can implement the class init
;; callback:
(CFFI:DEFCALLBACK LISP-MODULE-CLASS-INIT :VOID ((LISP-MODULE-CLASS :POINTER))
  (FORMAT T "LISP-MODULE-CLASS-INIT (LispModuleClass*): ~A~%" (LIST LISP-MODULE-CLASS))
  (setf (cffi:mem-ref LISP-MODULE-CLASS :pointer
		      +type-module-class-offset-load+)
	(cffi:callback default-type-loading-module-load))
  (setf (cffi:mem-ref LISP-MODULE-CLASS :pointer
		      +type-module-class-offset-unload+)
	(cffi:callback default-type-loading-module-unload)))

;; register our type loading module
#+NIL 					;DEFERRED
(%gobject-subclassable-register-type-static
 +default-type-loading-module-prototype+)

;; gtype of our type loading module
#+nil					;DEFERRED
(defvar +default-type-loading-module-type+
  (gethash +default-type-loading-module-name+ *registered-types*))

#+nil
(g-type-name +default-type-loading-module-type+)

#||
(defvar $module (gobject +default-type-loading-module-type+
			 *default-type-loading-module*))
(cffi:mem-ref *default-type-loading-module* :string (/ 384 8))
(invoke ($module "set_name") "lisp-module")
||#

;; Initialize *default-type-loading-module*
#+NIL					;DEFERRED
(when (null *default-type-loading-module*)
  (setq *default-type-loading-module*
	(cffi:foreign-funcall "g_object_new"
			      :ulong +default-type-loading-module-type+
			      :pointer (cffi:null-pointer)
			      :pointer)))

;; use it in the helper function to register a dynamic type.
(defun %gobject-subclassable-register-type-dynamic
    (obj &optional (type-module *default-type-loading-module*))
  (with-slots ((parent-obj gir-parent) gir-name) obj
    (register-type
     gir-name
     (g-type-query parent-obj nil)
     0
     :type :dynamic
     :type-module type-module

     :instance-struct-name
     (%gobject-subclassable-get-name obj :instance-struct)
     :class-struct-name
     (%gobject-subclassable-get-name obj :class-struct)
     :instance-init
     (eval `(cffi:callback ,(%gobject-subclassable-get-name
			     obj :instance-init-callback)))
     :class-init
     (eval `(cffi:callback ,(%gobject-subclassable-get-name
			     obj  :class-init-callback))))))

(defun init-deferred ()
  (if *default-type-loading-module*
      (cerror "continue" "*default-type-loading-module* already defined"))

  (let ((+default-type-loading-module-prototype+
	  (make-default-dynamic-type-loading-module-prototype))
	(*package* (find-package "GIR")))
    (eval
     (%gobject-subclassable-define-cstructs
      +default-type-loading-module-prototype+))
    (%gobject-subclassable-register-type-static
     +default-type-loading-module-prototype+)
    (let ((+default-type-loading-module-type+
	    (gethash +default-type-loading-module-name+ *registered-types*)))
      (setq *default-type-loading-module*
	    (cffi:foreign-funcall "g_object_new"
				  :ulong +default-type-loading-module-type+
				  :pointer (cffi:null-pointer)
				  :pointer)))))

;; Example
#+nil
(progn
(defvar $g0 (make-instance 'gobject-subclassable-info
	      :gir-name "MyWindow"
	      :gir-parent (nget gir-test::*gtk* "Window")))
(eval (%gobject-subclassable-define-cstructs $g0))
(eval (%gobject-subclassable-define-init-callbacks $g0))
(%gobject-subclassable-register-type-dynamic $g0)
(gethash "MyWindow" *registered-types*)
(defvar $g0-win-ptr
  (cffi:foreign-funcall
   "g_object_new"
   :ulong (gethash "MyWindow" gir::*registered-types*)
   :pointer (cffi:null-pointer)
   :pointer))
(defvar $g0-win (gir::gobject (gethash "MyWindow" gir::*registered-types*)
			      $g0-win-ptr))
(gir-test::with-gtk-thread
  (gir:invoke ($g0-win "show"))))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defun %generate-vfunc-callback-form (obj vfunc-name)
  "Emit forms for a lisp defmethod and a corresponding cffi:callback
which calls a lisp function."
  (let* ((callback-name (%gobject-subclassable-get-name obj :vfunc (concatenate 'string vfunc-name "_callback")))
	 (lisp-function-name (%gobject-subclassable-get-name obj :vfunc(concatenate 'string vfunc-name "_lisp")))
	 (vfunc-info
	  (with-slots ((parent-obj gir-parent)) obj
	    (object-class-find-vfunc-info parent-obj vfunc-name))))
    (generate-cffi-defcallback vfunc-info lisp-function-name callback-name)))

(defun %gobject-subclassable-define-vfunc-callbacks (obj)
  (with-slots (vfuncs) obj
    (when vfuncs
      `(progn ,@(mapcar (lambda (vfunc)
			  (%generate-vfunc-callback-form obj vfunc))
		       vfuncs)))))

(defun %generate-class-init-override (obj vfunc-name)
  (let ((klass (%gobject-subclassable-get-name obj :class-arg))
	(g-object-class
	 (with-slots  ((object-class gir-parent)) obj
	   `(nget (require-namespace ,(info-get-namespace (info-of object-class)))
		  ,(info-get-name (info-of object-class)))))
	(callback-name (%gobject-subclassable-get-name obj :vfunc (concatenate 'string  vfunc-name "-callback"))))
    `(setf (cffi:mem-ref ,klass :pointer
			 (find-vfunc-offset-recursive
			  (gtype-of ,g-object-class)
			  ,vfunc-name))
	   (cffi:callback ,callback-name))))
