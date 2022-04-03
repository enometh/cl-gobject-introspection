(defpackage #:gir.field
  (:use #:cl)
  (:export #:get #:set)
  (:shadow #:get #:set))

(in-package #:gir.field)

(cffi:defcfun g-field-info-get-field 
    :boolean (field gir::info-ffi) (obj :pointer) (value :pointer))
(cffi:defcfun g-field-info-set-field 
    :boolean (field gir::info-ffi) (obj :pointer) (value :pointer))
(cffi:defcfun g-field-info-get-offset
    :int (field gir::info-ffi))
(cffi:defcfun g-field-info-get-flags
    :int (field gir::info-ffi))

(defun maybe-kludge-array-length (ptr field giarg-type class)
  (let* ((typeinfo (gir:field-info-get-type field))
	 (array-len-pos (gir::get-array-length typeinfo))
	 (info (gir::info-of class)))
    (when array-len-pos
      (let* ((len (get ptr
		       (etypecase class
			 (gir::struct-class
			  (gir::g-struct-info-get-field info array-len-pos))
			 (gir::object-class
			  (gir::g-object-info-get-field info array-len-pos))
			 (gir::union-class
			  (gir::g-union-info-get-field info array-len-pos)))
		       class))
	     (c-array-type (gir::pointed-type-of
			    (gir::contained-type-of giarg-type))))
	(setf (gir::length-of c-array-type) len)))))

(defun get (ptr field &optional class)
  (cffi:with-foreign-object (giarg '(:union gir:argument))
    (unless (g-field-info-get-field field ptr giarg)
      (error "FFI get field failed: ~a" (gir:info-get-name field)))
    (let ((giarg-type (gir::build-argument-type (gir:field-info-get-type field) :nothing)))
      (maybe-kludge-array-length ptr field giarg-type class)
      (gir::mem-get giarg giarg-type))))

(defun set (ptr field value &optional class)
 (if (typep value 'string) ;; the string has to be freed explicitly
			   ;; with (cffi:foreign-free (cffi:mem-ref
			   ;; (this-of struct-instance) :pointer
			   ;; (field-offset struct-instance
			   ;; field-name))
     (setf (cffi:mem-ref ptr :string (g-field-info-get-offset field))
	   value)
  (cffi:with-foreign-object (giarg '(:union gir:argument))
    (let ((giarg-type (gir::build-argument-type (gir:field-info-get-type field) :nothing)))
      (maybe-kludge-array-length ptr field giarg-type class)
      (gir::mem-set giarg value giarg-type)
      ;; this will still fail. see g_field_info_set_field doc
      (unless (g-field-info-set-field field ptr giarg)
	(error "FFI set field failed: ~a" (gir:info-get-name field)))))))
