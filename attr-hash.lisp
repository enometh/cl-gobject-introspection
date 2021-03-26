;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Touched: Wed Sun Jul 26 08:09:41 2020 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2020 Madhu.  All Rights Reserved.
;;;
;;; ATTR-HASH - wrap glib Attribute HashTable (string -> string)
;;;
(in-package "GIR-LIB")
(eval-when (load eval compile)
(import '(gir:this-of))
(export '(make-attr-hash set-attr-hash rem-attr-hash get-attr-hash
	  set-attr-hash-from-alist attr-hash-to-alist)))

(defun %make-attr-hash ()
  "make a glib attr hash table. Keys and values are strings."
  (cffi:foreign-funcall "g_hash_table_new"
			:pointer (cffi:foreign-symbol-pointer "g_str_hash")
			:pointer (cffi:foreign-symbol-pointer "g_str_equal")
			:pointer))

(defun %map-attr-hash (function glib-hash-table)
  "Call FUNCTION (KEY VAL) on the raw pointers in the glib attr hash
table."
  (gir:with-struct (iter (gir:nget *glib* "HashTableIter"))
    (gir:invoke (iter "init") glib-hash-table)
    (loop (multiple-value-bind (ret key val) (gir:invoke (iter "next"))
	    (when (null ret) (return))
	    (funcall function key val)))))


(defun %put-attr-hash (glib-hash-table key val)
  "put KEY VAL lisp strings into glib attr hash table. Returns T if
the key did not exist yet."
  (let ((actual-key (etypecase key
		      (string (cffi:convert-to-foreign key :string))
		      (cffi:foreign-pointer key)))
	(actual-val (etypecase val
		      (string (cffi:convert-to-foreign val :string))
		      (cffi:foreign-pointer val))))
    (gir:invoke (*glib* "hash_table_insert")
		glib-hash-table actual-key actual-val)))

(defun %get-attr-hash (glib-hash-table key &optional default)
  "lookup KEY lisp string in glib attr hash table. Returns DEFAULT if
key is not found. Second return value is T if a key was found."
  (let ((lookup-key (etypecase key
		      (string (cffi:convert-to-foreign key :string))
		      (cffi:foreign-pointer key))))
    ;; TODO MAKE SURE CL-GIR finalizes ORIG-KEY ORIG-VAL pointers
    (multiple-value-bind (foundp orig-key orig-val)
	(gir:invoke (*glib* "hash_table_lookup_extended")
		    glib-hash-table lookup-key)
      (declare (ignore orig-key))
      (values (if foundp (cffi:convert-from-foreign orig-val :string) default)
	      foundp))))

(defun %rem-attr-hash (glib-hash-table key)
  "Remove KEY and value from glib attr hash table. return T if the key
was found and removed."
  (let ((lookup-key (etypecase key
		      (string (cffi:convert-to-foreign key :string))
		      (cffi:foreign-pointer key))))
    (gir:invoke (*glib* "hash_table_remove") glib-hash-table lookup-key)))

(defun %puthash-gvalue (glib-hash-table key val &optional val-type)
  "KEY is always a string. VAL is converted to a gvalue. VAL-TYPE can
 be omitted only if VAL is a string."
  (when (stringp val)
    (unless val-type
      (setq val-type :string))
    (assert (eql val-type :string)))
  (gir:invoke (*glib* "hash_table_insert")
	      glib-hash-table
	      (cffi:convert-to-foreign key :string)
	      (gir::make-gvalue (gir::%gtype val-type) val)))

(defun %attr-hash-from-alist (alist &optional (glib-hash-table (%make-attr-hash)))
  (loop for (k . v) in alist
	do
	(check-type k string)
	(check-type v string)
	(%put-attr-hash glib-hash-table k v))
  glib-hash-table)

(defun %attr-hash-to-alist (glib-hash-table)
  (let (ret)
    (%map-attr-hash (lambda (k v)
		      (push (cons (cffi:convert-from-foreign k :string)
				  (cffi:convert-from-foreign v :string))
			    ret))
		    glib-hash-table)
    ret))

(defclass glib-hashtable-wrapper ()
  ((this :type cffi:foreign-pointer :reader this-of :initarg :ptr)))

(defmethod initialize-instance :after
    ((self glib-hashtable-wrapper) &key
     (hash-function (cffi:foreign-symbol-pointer "g_str_hash"))
     (equal-function (cffi:foreign-symbol-pointer "g_str_equal"))
     alist)
  (check-type hash-function cffi:foreign-pointer)
  (check-type equal-function cffi:foreign-pointer)
  (with-slots (this) self
    (unless (and (slot-boundp self 'this) this)
      (setq this (cffi:foreign-funcall "g_hash_table_new"
				       :pointer hash-function
				       :pointer equal-function
				       :pointer))
      (let ((a (cffi:pointer-address this)))
	(tg:finalize self
		     (lambda ()
		       (cffi:foreign-funcall "g_hash_table_unref"
					     :pointer (cffi:make-pointer a)
					     :void)))))
    (check-type this cffi:foreign-pointer)
    (when alist
      (%attr-hash-from-alist alist this))))


(defun make-attr-hash (&rest rest &key ptr alist)
  "If PTR is supplied it must be a pointer to a GHashTable. Otherwise an
empty GHashtable is created. If ALIST is supplied the key-value pairs
are added to the wrapper object."
  (declare (ignorable ptr alist))
  (apply #'make-instance 'glib-hashtable-wrapper rest))

(defmethod get-attr-hash ((self glib-hashtable-wrapper) (key string) &optional default)
  (%get-attr-hash (this-of self) key default))

(defmethod (setf get-attr-hash) (new-value (self glib-hashtable-wrapper) (key string) &optional default)
  (declare (ignore default))
  (check-type new-value string)
  (%put-attr-hash (this-of self) key new-value))

(defmethod rem-attr-hash ((key string) (self glib-hashtable-wrapper))
  (%rem-attr-hash (this-of self) key))

(defmethod attr-hash-to-alist ((self glib-hashtable-wrapper))
  (%attr-hash-to-alist (this-of self)))

(defmethod set-attr-hash-from-alist ((self glib-hashtable-wrapper) alist)
  (%attr-hash-from-alist alist (this-of self)))


#||
(defvar $a (make-attr-hash))
(setf (get-attr-hash $a "foo") "bar")
(defvar $attrhash (%make-attr-hash))
(%puthash-gvalue $attrhash1 "key0" "value0")
(%put-attr-hash $attrhash "key0" "value0")
(%put-attr-hash $attrhash "key1" "value1")
(%attr-hash-to-alist $attrhash)
||#