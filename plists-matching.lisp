;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Sep 8 07:00:12 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;;;
;;; plists-matching: functions to match plists and manage the matching
;;; process.
;;;
;;; ;madhu 240908 - packaged as part of GIRLIB, but suitable for
;;; individual use see consumer surfprop.lisp in girlib.

(in-package "CL-USER")

(defpackage "PLISTS-MATCHING"
  (:use "CL")
  (:export
   "PLIST-EMPTY-P"
   "PLISTS-MATCH-P"
   "MAKE-PLISTS-MATCH-P-FN"
   "FIND-PLISTS-MATCHING"
   "INSTALL-PLIST"
   "UNINSTALL-PLIST"))
(in-package "PLISTS-MATCHING")

(defun plist-empty-p (plist exclude-keys)
  (zerop (loop for (k1 _v1 . _rest1) on plist by #'cddr
	       unless (find k1 exclude-keys)
	       count 1)))

#+nil
(plist-empty-p '(:cmd 1) '(:cmd))

(defun plists-match-p (p1 p2 &key (test #'eql) exclude-keys require-all-p)
  "Return T if the keys of plist P1 (except those in EXCLUDE-KEYS) that
are in plist P2 have the same value under TEST. TEST is applied to the
values of the keys to see if they are the same.  If REQUIRE-ALL-P is
non-NIL all keys in P1 (excluding those in EXCLUDE-KEYS) must be found
in P2. Returns T if p1 and p2 are both empty after excluding
EXCLUDE-KEYS."
  (check-type p1 cons)
  (check-type p2 cons)
  (loop for (k1 v1 . _rest1) on p1 by #'cddr
	with count = 0
	unless (find k1 exclude-keys)
	do (let* ((v2 (getf p2 k1 '+unbound+))
		  (v2-found-p (not (eql v2 '+unbound+))))
	     (cond ((and require-all-p (not v2-found-p)) (return nil))
		   ((funcall test v1 v2) (incf count))
		   (t (return nil))))
	finally  (if (zerop count)
		     (return (values (plist-empty-p p2 exclude-keys) t))
		     (return t))))


#||
(plists-match-p '(:a 1 :b 2 :cmd 12) '(:a 1 :c 2 :cmd 'barf))
(plists-match-p '(:a 1 :b 2) '(:a 1 :c 2 :cmd 'barf))
(plists-match-p '(:a 1 :b 2) '(:a 1 :c 2 :b 3))
(plists-match-p '(:b 2) '(:a 1 :b 2))
(plists-match-p '(:b 2) '(:b 1) :exclude-keys '(:b))
(plists-match-p '(:b 2 :a 1) '(:b 1 :a 1 :c 2) :exclude-keys '(:b :a))
(plists-match-p '(:b 2 :c 3) '(:a 1 :b 2))
(plists-match-p '(:b 2 :c 3) '(:a 1 :b 2) :exclude-keys '(:c))
(plists-match-p '(:b 2 :c 3) '(:a 1 :b 2) :exclude-keys '(:c) :require-all-p t)
(plists-match-p '(:b 2 :c 3) '(:a 1 :b 2))
||#

(defun make-plists-match-p-fn (exclude-keys test require-all-p)
  (lambda (a b)
    (plists-match-p a b :test test :exclude-keys exclude-keys
		    :require-all-p require-all-p)))

(defun find-plists-matching (plist plists-list exclude-keys test require-all-p)
  (loop for head = plists-list then (cdr found)
	for found = (member plist head
			    :test (make-plists-match-p-fn exclude-keys test
							  require-all-p))
	if found collect (car found)
	unless head do (loop-finish)))

(defun uninstall-plist (plist plists-list-var exclude-keys test require-all-p)
  (check-type plists-list-var symbol)
  (let ((fn (make-plists-match-p-fn exclude-keys test require-all-p)))
    (flet ((f (x) (funcall fn plist x)))
      (set plists-list-var (remove-if #'f (symbol-value plists-list-var))))))

(defun install-plist (plist plists-list-var exclude-keys test require-all-p)
  (check-type plists-list-var symbol)
  (let* ((plists-list (symbol-value plists-list-var))
	 (found (member plist plists-list
			:test (make-plists-match-p-fn exclude-keys test require-all-p))))
    (cond ((not found)
	   (setf (symbol-value plists-list-var)
		 (cons plist plists-list)))
	  (t (loop for k in exclude-keys do
		   (loop for (a b . rest) on plist by #'cddr
			 when (eql a k)
			 do
			 (setf (getf (car found) k) b)
			 #+nil
			 (if (null b)
			     (remf (car found) k)
			     (setf (getf (car found) k) b))
			 (loop for (c _d . _rest1) on rest by #'cddr
			       do (assert (not (eql c k))
				      nil "duplicate excluded keys"))))
	     (assert (not (find-plists-matching plist (cdr found) exclude-keys test require-all-p))
		 nil "Duplicates found")
	     ))))
