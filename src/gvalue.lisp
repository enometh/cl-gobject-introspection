(in-package :gir)

;; GType

(cffi:defcfun g-type-fundamental :ssize (id :ssize))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +g-type-invalid+   (ash  0 2))
  (defconstant +g-type-none+      (ash  1 2))
  (defconstant +g-type-interface+ (ash  2 2))
  (defconstant +g-type-char+      (ash  3 2))
  (defconstant +g-type-uchar+     (ash  4 2))
  (defconstant +g-type-boolean+   (ash  5 2))
  (defconstant +g-type-int+       (ash  6 2))
  (defconstant +g-type-uint+      (ash  7 2))
  (defconstant +g-type-long+      (ash  8 2))
  (defconstant +g-type-ulong+     (ash  9 2))
  (defconstant +g-type-int64+     (ash 10 2))
  (defconstant +g-type-uint64+    (ash 11 2))
  (defconstant +g-type-enum+      (ash 12 2))
  (defconstant +g-type-flags+     (ash 13 2))
  (defconstant +g-type-float+     (ash 14 2))
  (defconstant +g-type-double+    (ash 15 2))
  (defconstant +g-type-string+    (ash 16 2))
  (defconstant +g-type-pointer+   (ash 17 2))
  (defconstant +g-type-boxed+     (ash 18 2))
  (defconstant +g-type-param+     (ash 19 2))
  (defconstant +g-type-object+    (ash 20 2))
  (defconstant +g-type-variant+   (ash 21 2)))

;; GValue

(cffi:defcunion g-value-data
  (v-int :int)
  (v-uint :uint)
  (v-long :long)
  (v-ulong :ulong)
  (v-int64 :int64)
  (v-uint64 :uint64)
  (v-float :float)
  (v-double :double)
  (v-pointer :pointer))

(cffi:defcstruct g-value-struct
  "GValue struct"
  (g-type :ssize)
  (data (:union g-value-data) :count 2))

(cffi:define-foreign-type pvariant ()
  ()
  (:documentation "pointer to GVariant")
  (:actual-type :pointer)
  (:simple-parser pvariant))

(defmethod cffi:translate-to-foreign (object (type pvariant))
  (this-of object))

(defmethod cffi:translate-from-foreign (pointer (type pvariant))
  (gobject +g-type-variant+ pointer))

(cffi:defcfun g-value-init :pointer (value :pointer) (gtype :ssize))
(cffi:defcfun g-value-unset :void (value :pointer))

(cffi:defcfun g-value-get-boolean :boolean (g-value :pointer))
(cffi:defcfun g-value-get-schar   :char    (g-value :pointer))
(cffi:defcfun g-value-get-uchar   :uchar   (g-value :pointer))
(cffi:defcfun g-value-get-int     :int     (g-value :pointer))
(cffi:defcfun g-value-get-uint    :uint    (g-value :pointer))
(cffi:defcfun g-value-get-long    :long    (g-value :pointer))
(cffi:defcfun g-value-get-ulong   :ulong   (g-value :pointer))
(cffi:defcfun g-value-get-int64   :int64   (g-value :pointer))
(cffi:defcfun g-value-get-uint64  :uint64  (g-value :pointer))
(cffi:defcfun g-value-get-float   :float   (g-value :pointer))
(cffi:defcfun g-value-get-double  :double  (g-value :pointer))
(cffi:defcfun g-value-get-enum    :int     (g-value :pointer))
(cffi:defcfun g-value-get-flags   :uint    (g-value :pointer))
(cffi:defcfun g-value-get-string  :string  (g-value :pointer))
(cffi:defcfun g-value-get-param   :pointer (g-value :pointer))
(cffi:defcfun g-value-get-boxed   :pointer (g-value :pointer))
(cffi:defcfun g-value-get-pointer :pointer (g-value :pointer))
(cffi:defcfun g-value-get-object  pobject  (g-value :pointer))
(cffi:defcfun g-value-get-variant pvariant (g-value :pointer))

(cffi:defcfun g-value-set-boolean :void (g-value :pointer) (val :boolean))
(cffi:defcfun g-value-set-schar   :void (g-value :pointer) (val :char))
(cffi:defcfun g-value-set-uchar   :void (g-value :pointer) (val :uchar))
(cffi:defcfun g-value-set-int     :void (g-value :pointer) (val :int))
(cffi:defcfun g-value-set-uint    :void (g-value :pointer) (val :uint))
(cffi:defcfun g-value-set-long    :void (g-value :pointer) (val :long))
(cffi:defcfun g-value-set-ulong   :void (g-value :pointer) (val :ulong))
(cffi:defcfun g-value-set-int64   :void (g-value :pointer) (val :int64))
(cffi:defcfun g-value-set-uint64  :void (g-value :pointer) (val :uint64))
(cffi:defcfun g-value-set-float   :void (g-value :pointer) (val :float))
(cffi:defcfun g-value-set-double  :void (g-value :pointer) (val :double))
(cffi:defcfun g-value-set-enum    :void (g-value :pointer) (val :int))
(cffi:defcfun g-value-set-flags   :void (g-value :pointer) (val :uint))
(cffi:defcfun g-value-set-string  :void (g-value :pointer) (val :string))
(cffi:defcfun g-value-set-param   :void (g-value :pointer) (val :pointer))
(cffi:defcfun g-value-set-boxed   :void (g-value :pointer) (val :pointer))
(cffi:defcfun g-value-set-pointer :void (g-value :pointer) (val :pointer))

;; REVISIT g-value-get-object autoconverts to gobject but if we want
;; to get the pointer?
(cffi:defcfun g-value-set-object  :void (g-value :pointer) (val pobject))
(cffi:defcfun g-value-set-variant :void (g-value :pointer) (val pvariant))

;; REVISIT document exactly where these get used.
;madhu 230115 gone
;;(defun g-value-get-interface (g-value) (g-value-get-object g-value))
;;(defun g-value-set-interface (g-value val) (g-value-set-object g-value val))

(defun make-gvalue (gtype)
  (let ((ptr (cffi:foreign-alloc '(:struct g-value-struct))))
    (setf (cffi:foreign-slot-value ptr '(:struct g-value-struct) 'g-type) 0)
    (g-value-init ptr gtype)
    ptr))

(defun gvalue-free (gvalue)
  (g-value-unset gvalue)
  (cffi:foreign-free gvalue))

(defun gvalue-gtype (gvalue)
  (cffi:foreign-slot-value gvalue '(:struct g-value-struct) 'g-type))

;;(defvar +g-type-none+ (ash 1 2))	; (%gtype :none) or (%gtype void)
(defvar +g-type-value+ (cffi:foreign-funcall "g_value_get_type" :ulong))


;; restored
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +fundamental-g-types+
    '(:invalid :void :interface :char :uchar :boolean
      :int :uint :long :ulong :int64 :uint64
      :enum :flags :float :double :string
      :pointer :boxed :param :object :variant)))

(defun %gtype (x)
  "Convert between keywords and (integer) gtypes. If the argument is a
 keyword recognized in +FUNDAMENTAL-G-TYPES+ the corresponding integer
 gtype is returned. If the argument is an integer recognized as a
 gtype correspoding in +FUNDAMENTAL-G-TYPES+ denoting this gtype is
 returned."
  (etypecase x
    (number (or (loop for i from 0 for type-name in +fundamental-g-types+
		      when (>= i 2)
			if (= x (* i 4)) return type-name)
		(and (= x +g-type-value+) :value)
		(and (= x +g-type-none+) :none)
		x))
    (keyword (or (loop for i from 0 for type-name in +fundamental-g-types+
		       when (>= i 2)
			 if (eq x type-name) return (* i 4))
		 (and (eql x :value) +g-type-value+)
		 (and (eql x :none)  +g-type-none+)
		 x))))

(defun ffi-enum (value gtype)
  (declare (ignore gtype))
  "Maybe later it will convert list of symbols to integer"
  value)

(defun gvalue-get (gvalue)
  (let ((type (g-type-fundamental (gvalue-gtype gvalue))))
    (cond
      ((eq type +g-type-interface+) (g-value-get-object  gvalue))
      ((eq type +g-type-char+)      (g-value-get-schar   gvalue))
      ((eq type +g-type-uchar+)     (g-value-get-uchar   gvalue))
      ((eq type +g-type-boolean+)   (g-value-get-boolean gvalue))
      ((eq type +g-type-int+)       (g-value-get-int     gvalue))
      ((eq type +g-type-uint+)      (g-value-get-uint    gvalue))
      ((eq type +g-type-long+)      (g-value-get-long    gvalue))
      ((eq type +g-type-ulong+)     (g-value-get-ulong   gvalue))
      ((eq type +g-type-int64+)     (g-value-get-int64   gvalue))
      ((eq type +g-type-uint64+)    (g-value-get-uint64  gvalue))
      ((eq type +g-type-enum+)      (g-value-get-enum    gvalue))
      ((eq type +g-type-flags+)     (g-value-get-flags   gvalue))
      ((eq type +g-type-float+)     (g-value-get-float   gvalue))
      ((eq type +g-type-double+)    (g-value-get-double  gvalue))
      ((eq type +g-type-string+)    (g-value-get-string  gvalue))
      ((eq type +g-type-pointer+)   (g-value-get-pointer gvalue))
      ((eq type +g-type-boxed+)     (g-value-get-boxed   gvalue))
      ((eq type +g-type-param+)     (g-value-get-param   gvalue))
      ((eq type +g-type-object+)    (g-value-get-object  gvalue))
      ((eq type +g-type-variant+)   (g-value-get-variant gvalue)))))

(defun gvalue-set (gvalue value)
  (let ((type (g-type-fundamental (gvalue-gtype gvalue))))
    (cond
      ((eq type +g-type-interface+) (g-value-set-object  gvalue value))
      ((eq type +g-type-char+)      (g-value-set-schar   gvalue value))
      ((eq type +g-type-uchar+)     (g-value-set-uchar   gvalue value))
      ((eq type +g-type-boolean+)   (g-value-set-boolean gvalue value))
      ((eq type +g-type-int+)       (g-value-set-int     gvalue (round value)))
      ((eq type +g-type-uint+)      (g-value-set-uint    gvalue (round value)))
      ((eq type +g-type-long+)      (g-value-set-long    gvalue (round value)))
      ((eq type +g-type-ulong+)     (g-value-set-ulong   gvalue (round value)))
      ((eq type +g-type-int64+)     (g-value-set-int64   gvalue (round value)))
      ((eq type +g-type-uint64+)    (g-value-set-uint64  gvalue (round value)))
      ((eq type +g-type-enum+)      (g-value-set-enum    gvalue (ffi-enum value type)))
      ((eq type +g-type-flags+)     (g-value-set-flags   gvalue (ffi-enum value type)))
      ((eq type +g-type-float+)     (g-value-set-float   gvalue (coerce value 'single-float)))
      ((eq type +g-type-double+)    (g-value-set-double  gvalue (coerce value 'double-float)))
      ((eq type +g-type-string+)    (g-value-set-string  gvalue value))
      ((eq type +g-type-pointer+)   (g-value-set-pointer gvalue value))
      ((eq type +g-type-boxed+)     (g-value-set-boxed   gvalue value))
      ((eq type +g-type-param+)     (g-value-set-param   gvalue value))
      ((eq type +g-type-object+)    (g-value-set-object  gvalue value))
      ((eq type +g-type-variant+)   (g-value-set-variant gvalue value)))))

#||
(defmethod gtype-from-instance ((object object-instance))
  (let* ((object-class (gir-class-of object))
	 (object-class-info (info-of object-class))
	 (type-init-function
	  (object-info-get-type-init object-class-info)))
    (eval `(cffi:foreign-funcall (coerce ,type-init-function 'base-string)
				 :ulong))))
||#
