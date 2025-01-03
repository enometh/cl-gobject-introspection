;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Jan 03 12:28:09 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
(in-package "GDK-KEY")

;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defvar $gdk-keysyms (make-hash-table :test #'equal))

(defun read-gdk-keysyms (file)
  (let ((ht $gdk-keysyms))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
	    while line
	    do (multiple-value-bind (beg end begs ends)
		   (cl-ppcre:scan "^#define (GDK_KEY_[^ ]+) (0x[0-9a-f]+)$"
				  line)
		 (when beg
		   (let ((key (subseq line (elt begs 0) (elt ends 0)))
			 (ret (subseq line (elt begs 1) (elt ends 1))))
		      (assert (cl-user::prefixp "0x" ret))
		      (setf (gethash key  ht)
			    (parse-integer (subseq ret 2) :radix 16)))))))
    ht))


#||
(setq $s "#define GDK_KEY_Next_VMode 0x1008fe22")
(multiple-value-bind (beg end begs ends)
    (cl-ppcre:scan "^#define (GDK_KEY_[^ ]+) (0x[0-9a-f]+)$" $s)
  (list beg end begs ends))
(clrhash $gdk-keysyms)
(read-gdk-keysyms (mk::system-relative-pathname :cl-gdk-key
						"data/gdkkeysyms.h"))
(gethash "GDK_KEY_Next_VMode" $gdk-keysyms)
(=(get-gdk-keysym "BackSpace")65288)
||#

(defun get-gdk-keysym (name &key ((:gdk-keysyms $gdk-keysyms) $gdk-keysyms))
  "NAME is a string designator for one of the constants typically found
in gdk/gdkkeysyms.h. Return the integer value associated with NAME."
  (let ((name (string name)))
    (unless (cl-user::prefixp "GDK_KEY_" name)
      (setq name (concatenate 'string "GDK_KEY_" name)))
    (gethash name $gdk-keysyms)))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defvar $keynames (make-hash-table :test #'equal))
(defvar $keys-by-keyval (make-hash-table :test #'equal))
(defvar $keys-by-name (make-hash-table :test #'equal))

(defun read-keynames-table (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil) for i from 0
	  while line do
	  (let* ((p (position #\Space line))
		 (code (subseq line 0 p))
		 (name (subseq line (1+ p)))
		 (keyval (progn (assert (user::prefixp "0x" code))
				(parse-integer (subseq code 2) :radix 16))))
	    (setf (gethash name $keynames) name)
	    (setf (gethash keyval $keys-by-keyval) name)
	    (setf (gethash name $keys-by-name) keyval))))))


#+nil
(read-keynames-table
 (mk::system-relative-pathname :cl-gdk-key
			       "data/keynames.txt"))

(defun gdk-keyval-name (keyval)
  "/**
 * gdk_keyval_name:
 * @keyval: a key value
 *
 * Converts a key value into a symbolic name.
 *
 * The names are the same as those in the
 * `gdk/gdkkeysyms.h` header file
 * but without the leading “GDK_KEY_”.
 *
 * Returns: (nullable) (transfer none): a string containing the name
 *   of the key
 */"

  (or
   #|/* Check for directly encoded 24-bit UCS characters: */|#
   (if (= (logand keyval #xff000000)
	  #x01000000)
       (format nil "U+'~4,'0X" (logand keyval #x00ffffff)))
   (gethash keyval $keys-by-keyval)
   (format nil "#~x" keyval)))



(defun gdk-key-init-tables ()
  (read-keynames-table
   (mk::system-relative-pathname :cl-gdk-key
				 "data/keynames.txt"))
  (read-gdk-keysyms
   (mk::system-relative-pathname :cl-gdk-key
				 "data/gdkkeysyms.h")))