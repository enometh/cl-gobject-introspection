;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <2018-07-21 16:39:36 IST>
;;;   Touched: [Thu Apr 26 22:30:09 2018 +0530] <enometh@meer.net>
;;;   Bugs-To: enometh@meer.net
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2018 Madhu.  All Rights Reserved.
;;;
;;; inifile.lisp - useless for desktop spec files because multiple
;;; keys are not merged and the last key wins.

(in-package "GIR-LIB")

(eval-when (load eval compile)
(export '(ini-file ini-file-lookup key-file-of
	  ini-file-get-groups
	  ini-file-get-group-keys)))

(defvar +key-file-flags+
  (logior (load-time-value (gir:nget *glib* "KeyFileFlags" :none))
	  #+nil
	  (load-time-value(gir:nget *glib* "KeyFileFlags" :keep-translations))))

(defun key-file-from-file (kf path)
  (gir:invoke (kf "load_from_file")
	      (namestring (truename path)) +key-file-flags+))

(defun key-file-from-data (kf string)
  (gir:invoke (kf "load_from_data")
	      string (length string) +key-file-flags+))

(defun key-file-parse-to-alist (kf)
  (let (alist)
    (multiple-value-bind (groups) (gir:invoke (kf "get_groups"))
      (dolist (group groups)
	(let ((elem (cons group nil)))
	  (push elem alist)
	  (multiple-value-bind (keys) (gir:invoke (kf "get_keys") group)
	    (dolist (key keys)
	      (push (cons key (gir:invoke (kf "get_value") group key))
		    (cdr elem)))
	    (setf (cdr elem) (nreverse (cdr elem)))))))
    (setq alist (nreverse alist))))

(defun key-file-lookup-cached (alist named-group named-key &key error
			       &aux group-found)
  (if (null named-group) (return-from key-file-lookup-cached alist))
  (loop for (group . elem) in alist do
	(cond ((equal group named-group)
	       (setq group-found t)
	       (cond ((null named-key)
		      (return-from key-file-lookup-cached elem))
		     (t (loop for (key . val) in elem do
			      (cond ((equal named-key key)
				     (return-from key-file-lookup-cached
				       val)))))))))
  (when error
    (if group-found
	(error "Key ~A not found" named-key)
	(error "Group ~A not found" named-group))))

(defclass ini-file ()
  ((kf :initform nil :reader key-file-of)
   (cached :initform  nil)))

(defun ini-file-initialize-from-path (self path)
  (with-slots (kf cached) self
    (key-file-from-file kf path)
    (setq cached (key-file-parse-to-alist kf))))

(defun ini-file-initialize-from-data (self data)
  (with-slots (kf cached) self
    (key-file-from-data kf data)
    (setq cached (key-file-parse-to-alist kf))))

(defmethod initialize-instance :after ((self ini-file) &key path data)
  (let (kf)
    (unwind-protect
	 (setq kf (setf (slot-value self 'kf)
			(gir:invoke (*glib* "KeyFile" 'new))))
      (when kf
	(tg:finalize self (lambda () (gir:invoke (kf "unref"))))))
    (when path (ini-file-initialize-from-path self path))
    (when data
      (if path
	  (warn "initialize-instance ini-file: ignoring supplied :data and using :path")
	  (ini-file-initialize-from-data self data)))))

(defun ini-file-get-groups (self)
  (gir:invoke ((key-file-of self) "get_groups")))

(defun ini-file-get-group-keys (self group)
  (gir:invoke ((key-file-of self) "get_keys") group))

(defun ini-file-lookup-cached (self &optional named-group named-key &key error)
  "always looks up string values from a cached alist"
  (with-slots (kf cached) self
    (key-file-lookup-cached (or cached (setq cached
					     (key-file-parse-to-alist kf)))
			    named-group named-key
			    :error error)))

(defun ini-file-lookup (self &optional named-group named-key &key (error t)
			(type "get_value" type-supplied-p) cached locale)
  "If NAMED-GROUP and NAMED-KEY are both supplied, lookup the value
via the Glib C method with c-name TYPE. In this case LOCALE must be
supplied if TYPE is `get_locale_string' or `get_locale_string_list'
Otherwise parse the keyfile to a cached alist of strings and return
selected element(s)."
  (when (or (not named-key) (not named-group) cached)
    (when type-supplied-p
      (warn "ini-file-lookup: looking at the cache. ignoring supplied ~A" type))
    (if (eq cached :recache) (setf (slot-value self 'cached) nil))
    (return-from ini-file-lookup
      (ini-file-lookup-cached self named-group named-key :error error)))
  (with-slots (kf) self
    (when (null error)
      (if (not (gir:invoke (kf "has_group") named-group))
	  (return-from ini-file-lookup nil))
      (cffi:with-foreign-object (err :pointer)
	(setf (cffi:mem-ref err :pointer) (cffi:null-pointer))
	(let ((ret (cffi:foreign-funcall "g_key_file_has_key"
					 :pointer (this-of kf)
					 :string named-group
					 :string named-key
					 :pointer err
					 :boolean)))
	  (cond ((cffi:null-pointer-p (cffi:mem-ref err :pointer))
		 (unless ret (return-from ini-file-lookup nil)))
		(t (unwind-protect (error "wtf is this API")
		     (cffi:foreign-funcall "g_error_free" :pointer err :void))
		   (return-from ini-file-lookup nil))))))
    (if locale
	(gir:invoke (kf type) named-group named-key locale)
	(gir:invoke (kf type) named-group named-key))))

(defsetf ini-file-lookup (self &optional named-group named-key &key (error t)
			  (type "set_value" type-supplied-p) cached
			  (locale nil locale-supplied-p))
    (data)
  "NAMED-GROUP and NAMED-KEY MUST be supplied. LOCALE must be supplied
if TYPE is `set_locale_string' or `set_locale_string_list'."
  (declare (ignorable cached error type-supplied-p)
	   (optimize (speed 3))
	   (optimize (safety 1)))
 `(with-slots (gir-lib::kf) ,self
     ,(if locale-supplied-p
	  `(gir:invoke (gir-lib::kf ,type) ,named-group ,named-key
		       ,locale ,data)
	  `(gir:invoke (gir-lib::kf ,type) ,named-group ,named-key ,data))))


#||
(defvar $i (make-instance 'ini-file :data #1="[First Group]
Name=Key File Example\\tthis value shows\\nescaping
Welcome=Hello
Welcome[de]=Guten Tag
[Second Group]
Welcome=To Chicago
Welcome[au]=Gday
Numbers=2;20;-200;0"))

(ini-file-lookup $i "Second Group" "Welcome" :cached :recache :locale "au" :type "get_locale_string_list")
(setf (gir-lib::ini-file-lookup $i "Second Group" "Numbers" :type "set_integer_list")
      '(10 20 300))
(ini-file-lookup $i "Second Group")
(ini-file-lookup $i "Second Group" nil :cached :recache)
(gir:invoke ((key-file-of $i) "set_comment") nil nil "BARF")
(gir:invoke ((key-file-of $i) "save_to_file") "/dev/shm/1.ini")
||#
