;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <2020-09-18 08:59:45 IST>
;;;   Touched: Thu Sep 10 08:04:33 2020 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2020 Madhu.  All Rights Reserved.
;;;
(defpackage "XLIB-XPROP"
  (:use "CL"))
(in-package "XLIB-XPROP")

(export '(get-xdisplay-ptr atom-name get-string-atom atom-prop-op
	  list-window-properties get-window-property
	  change-window-property
	  property-event))

(defun get-xdisplay-ptr ()
  #+wk
  (and (cffi:foreign-symbol-pointer "gdk_x11_get_default_xdisplay")
       (cffi:foreign-funcall "gdk_x11_get_default_xdisplay" :pointer))
  #-wk
  (cffi:foreign-funcall "gdk_x11_display_get_xdisplay"
    :pointer (cffi:foreign-funcall "gdk_display_get_default" :pointer)
    :pointer))

(defun intern-atom (atom-name &key (only-if-exists t) xdisplay-ptr)
  (assert (cffi:pointerp xdisplay-ptr))
  (cffi:foreign-funcall "XInternAtom" :pointer xdisplay-ptr
			:string atom-name
			:bool only-if-exists
			:unsigned-long))

(defun ensure-atom (atom-name &key xdisplay-ptr)
  (etypecase atom-name
    (integer atom-name)
    (string (intern-atom atom-name :xdisplay-ptr xdisplay-ptr :only-if-exists nil))))

(defun atom-name (atom &key xdisplay-ptr)
  (cffi:foreign-funcall "XGetAtomName"
			:pointer xdisplay-ptr
			:unsigned-long atom
			:string))

(defun getatomprop (xdisplay-ptr xid prop)
  (cffi:with-foreign-objects ((realtypep :unsigned-long)
			      (formatp :int)
			      (remainp :unsigned-long)
			      (nitemsp :unsigned-long)
			      (p :pointer))
    (let ((ret
	   (cffi:foreign-funcall "XGetWindowProperty"
				 :pointer xdisplay-ptr
				 :unsigned-long xid
				 :unsigned-long (ensure-atom prop :xdisplay-ptr xdisplay-ptr) ; prop Atom
				 :long 0	  ;long_offset long
				 :long 8192	  ;long_length long
				 :boolean nil	  ;del-p Bool
				 :unsigned-long 4 ;XA_ATOM type
				 :pointer realtypep ;:actual_type_return Atom*
				 :pointer formatp ;:actual_format_return int*
				 :pointer nitemsp ;nitems_return unsigned-long*
				 :pointer remainp ;bytes_after_return unsigned-long*
				 :pointer p
				 :int)))
      (cond ((zerop ret)		;Success
	     (let ((atom (cffi:mem-ref (cffi:mem-ref p  :pointer) :unsigned-long)))
	       (unless (cffi:null-pointer-p p)
		 (cffi:foreign-funcall "XFree" :pointer (cffi:mem-ref p :pointer) :int))
	       atom))
	    (t (values nil nil))))))

#||
(gir-lib:start-gtk-thread)
(setq $xid #x8004b5)
(setq $xdisplay-ptr (get-xdisplay-ptr))
(getatomprop $xdisplay-ptr $xid "XdndAware")
(ensure-atom "_NET_WM_STATE" :xdisplay-ptr $xdisplay-ptr)
||#

(defun atom-for-string-type (string-type &key xdisplay-ptr)
  (ecase string-type
    (:string ;(intern-atom "STRING" :xdisplay-ptr xdisplay-ptr)
     31)
    (:utf8-string
     (intern-atom "UTF8_STRING" :xdisplay-ptr xdisplay-ptr))))

(defun encoding-for-string-type (string-type)
  (ecase string-type
    (:string :latin-1)
    (:utf8-string :utf-8)))

(defun get-string-atom (atom-name &key xdisplay-ptr xid
			(string-type :string)
			((:encoding cffi:*default-foreign-encoding*)
			 (encoding-for-string-type string-type))
			delete-p)
  "String"
  (assert (cffi:pointerp xdisplay-ptr))
  (check-type xid (integer 1))
  (cffi:foreign-funcall "XSync" :pointer xdisplay-ptr :boolean nil :int)
  (cffi:with-foreign-objects ((idummy :int)
			      (adummy :unsigned-long)
			      (ldummy :long)
			      (p :pointer))
    (let ((ret
	   (cffi:foreign-funcall "XGetWindowProperty" :pointer xdisplay-ptr
				 :unsigned-long xid
				 :unsigned-long ;property atom
				 (ensure-atom atom-name :xdisplay-ptr xdisplay-ptr)
				 :long 0	   ;long_offset
				 :long 8192	   ;BUFSIZ long_length
				 :boolean delete-p ;delete
				 :unsigned-long	   ;req_type
				 (atom-for-string-type string-type
						       :xdisplay-ptr xdisplay-ptr)
				 :pointer adummy ;actual_type_return atom
				 :pointer idummy ;actual_format_return int
				 :pointer ldummy ;nitems_return long
				 :pointer ldummy ;bytes_after_return long
				 :pointer p ;prop_return unsigned char**
				 :int)))
      (unless (zerop ret)
	(return-from get-string-atom nil)))
    (unless (cffi:null-pointer-p p)
      (prog1 (cffi:convert-from-foreign (cffi:mem-ref p :pointer) :string)
	(cffi:foreign-funcall "XFree"
			      :pointer (cffi:mem-ref p :pointer) :int)))))

(defun set-string-atom (atom-name data &key xdisplay-ptr xid
			(string-type :string)
			((:encoding cffi:*default-foreign-encoding*)
			 (encoding-for-string-type string-type)))
  "String"
  (assert (cffi:pointerp xdisplay-ptr))
  (check-type xid (integer 1))
  ;; XChangeProperty always returns 1
  (cffi:foreign-funcall "XChangeProperty"
			:pointer xdisplay-ptr
			:unsigned-long xid
			:unsigned-long
			(ensure-atom atom-name :xdisplay-ptr xdisplay-ptr)
			:unsigned-long
			(atom-for-string-type string-type
					      :xdisplay-ptr xdisplay-ptr)
			:int 8		;type
			:int 0		;PropModeReplace
			:string data
			:int (1+ (length data))
			:int)
  (cffi:foreign-funcall "XSync" :pointer xdisplay-ptr :boolean nil :int)
  data)

;; fails in clozure with
;; The value #:STRING-TYPE is not of the expected type (MEMBER :STRING :UTF8-STRING).
#-clozure
(defsetf get-string-atom (atom-name &key xdisplay-ptr xid
			  (string-type :string)
			  ((:encoding cffi:*default-foreign-encoding*)
			   (encoding-for-string-type string-type)))
    (data)
  `(set-string-atom ,atom-name ,data
		    :xdisplay-ptr ,xdisplay-ptr
		    :xid ,xid :string-type ,string-type
		    :encoding ,cffi:*default-foreign-encoding*))

#+clozure
(defsetf get-string-atom (atom-name &key xdisplay-ptr xid
			  (string-type :string) encoding)
    (data)
  `(set-string-atom ,atom-name ,data
		    :xdisplay-ptr ,xdisplay-ptr
		    :xid ,xid :string-type ,string-type
		    :encoding (or ,encoding
				  (encoding-for-string-type ,string-type))))

#||
(set-string-atom "FOOBAR" "FOOBAR" :xdisplay-ptr $xdisplay-ptr :xid $xid)
(setf (get-string-atom "FOOBAR" :xdisplay-ptr $xdisplay-ptr :xid $xid)
      "123")
||#

(defun atompropop (xid atom-name value mode &key xdisplay-ptr)
  "mode = 0: has, mode=1, add, mode=2 delete.
get mode=3 - val is the index of the property to retrieve"
  (let (n found (nfound 0))
    (cffi:with-foreign-objects ((realtypep :unsigned-long)
				(formatp :int)
				(np :unsigned-long)
				(extrap :unsigned-long)
				(p :pointer))
      (let ((ret
	     (cffi:foreign-funcall "XGetWindowProperty"
				   :pointer xdisplay-ptr
				   :unsigned-long xid
				   :unsigned-long ;property Atom
				   (ensure-atom atom-name :xdisplay-ptr xdisplay-ptr)
				   :long 0    ;long_offset long
				   :long 8192 ;BUFSIZ long_length long
				   :boolean nil	  ;delete Bool
				   :unsigned-long ;req_type Atom
				   4 ;(intern-atom "ATOM" :xdisplay-ptr xdisplay-ptr)
				   :pointer realtypep ;actual_type_return Atom*
				   :pointer formatp ;actual_format_return int*
				   :pointer np ;nitems_return unsigned-long*
				   :pointer extrap ;bytes_after_return unsigned-long*
				   :pointer p ;prop_return unsigned char**
				   :int)))
	(unless (zerop ret)
	  (return-from atompropop nil)))
      (cond ((cffi:null-pointer-p p)
	     (if (member mode '(:has :delete))
		 (return-from atompropop nil))))
      (cond ((= (setq n (cffi:mem-ref np :unsigned-long)) 0)
	     (cond ((or (eql mode :has) (eql mode :delete) (eql mode :get))
		    (cffi:foreign-funcall "XFree" :pointer (cffi:mem-ref p :pointer) :int)
		    (return-from atompropop nil)))))
      (cond ((eql mode :get)
	     (let ((ret (etypecase value
			  (integer
			   (if (< value n)
			       (cffi:mem-aref (cffi:mem-ref p :pointer) :unsigned-long value)))
			  (symbol
			   (ecase value
			     ((nil :none) nil)
			     ((:n :length) n)
			     ((t :all)
			      (loop for i below n
				    collect (cffi:mem-aref (cffi:mem-ref p :pointer) :unsigned-long i))))))))
	       (cffi:foreign-funcall "XFree" :pointer (cffi:mem-ref p :pointer) :int)
	       (return-from atompropop ret))))
      (loop for i below n
	    if (= value (cffi:mem-aref (cffi:mem-ref p :pointer) :unsigned-long i))
	    do (progn (setq found t)
		      (incf nfound)
		      (when (eql mode :has)
			(cffi:foreign-funcall "XFree" :pointer (cffi:mem-ref p :pointer) :int)
			(return-from atompropop t))))
      (when (eql mode :has)
	(cffi:foreign-funcall "XFree" :pointer (cffi:mem-ref p :pointer) :int)
	(return-from atompropop (if (not found) nil t)))
      (when (and (eql mode :delete) (not found))
	(cffi:foreign-funcall "XFree" :pointer (cffi:mem-ref p :pointer) :int)
	(return-from atompropop t))
      (when (and (eql mode :add) found)
	(cffi:foreign-funcall "XFree" :pointer (cffi:mem-ref p :pointer) :int)
	(return-from atompropop t))
      (let ((newsize (if (eql mode :add)
			 (1+ n)
			 (- n nfound)))
	    (j 0))
	(cffi:with-foreign-object (ret :unsigned-long newsize)
	  (if (not (or (eql mode :delete) (zerop newsize)))
	      (assert (not (cffi:null-pointer-p ret))))
	  (loop for i below n
		if (or (eql mode :add)
		       (and (eql mode :delete)
			    (/= (cffi:mem-aref p :unsigned-long i)
				value)))
		do (progn (setf (cffi:mem-aref ret :unsigned-long j)
				(cffi:mem-aref p :unsigned-long i))
			  (incf j)))
	  (if (eql mode :add)
	      (progn (setf (cffi:mem-aref ret j) value)
		     (incf j)))
	  (assert (/= j newsize))
	  (let ((retval
		 (cffi:foreign-funcall "XChangeProperty"
				       :pointer xdisplay-ptr
				       :unsigned-long xid
				       :unsigned-long
				       (ensure-atom atom-name
						    :xdisplay-ptr xdisplay-ptr)
				       :unsigned-long
				       4       ;XA_ATOM type
				       :int 32 ;type
				       :int 0  ;PropModeReplace
				       :pointer ret
				       :int j
				       :int)))
	    (if (and (eql mode :add) (= newsize 1))
		(assert (= (cffi:mem-aref ret :unsigned-long 0)
			   value)))
	    (if (not (cffi:null-pointer-p ret))
		(cffi:foreign-funcall "XFree" :pointer (cffi:mem-ref ret :pointer) :int))
	    (cffi:foreign-funcall "XFree" :pointer (cffi:mem-ref p :pointer) :int)
	    retval))))))

#||
(mapcar (lambda (x) (atom-name x :xdisplay-ptr $xdisplay-ptr))
	(atompropop $xid "WM_PROTOCOLS"
		    :all
		    :get
		    :xdisplay-ptr $xdisplay-ptr))
||#


;;; XLib interface
(defun list-window-properties (xid &key xdisplay-ptr)
  (cffi:with-foreign-object (n :int)
    (let ((p (cffi:foreign-funcall "XListProperties"
				   :pointer xdisplay-ptr
				   :unsigned-long xid
				   :pointer n
				   :pointer)))
      (unless (cffi:null-pointer-p p)
	(prog1 (loop for i below (cffi:mem-ref n :int)
		     collect (atom-name (cffi:mem-aref p :unsigned-long i)
					:xdisplay-ptr xdisplay-ptr))
	  (cffi:foreign-funcall "XFree" :pointer p :int))))))


(defun get-window-property (xid prop &key xdisplay-ptr (type 0) (start 0) end delete-p)
  (cffi:with-foreign-objects ((realtypep :unsigned-long)
			      (formatp :int)
			      (remainp :unsigned-long)
			      (nitemsp :unsigned-long)
			      (p :pointer))
    (let ((ret (cffi:foreign-funcall "XGetWindowProperty"
				     :pointer xdisplay-ptr
				     :unsigned-long xid
				     :unsigned-long (ensure-atom prop :xdisplay-ptr xdisplay-ptr) ; prop Atom
				     :long start ;long_offset long
				     :long (- (or end 8192) start) ;long_length long
				     :boolean delete-p ;Bool
				     :unsigned-long    ;type Atom
				     (ensure-atom type :xdisplay-ptr xdisplay-ptr)
				     :pointer realtypep ;:actual_type_return Atom*
				     :pointer formatp ;:actual_format_return int*
				     :pointer nitemsp ;nitems_return unsigned-long*
				     :pointer remainp ;bytes_after_return unsigned-long*
				     :pointer p
				     :int)))
      (cond ((zerop ret)		;Success
	     (let* ((reply-type (cffi:mem-ref realtypep :unsigned-long))
		    (reply-format (cffi:mem-ref formatp :int))
		    (bytes-after (cffi:mem-ref remainp :unsigned-long))
		    (nitems (cffi:mem-ref nitemsp :unsigned-long))
		    (cffi-elem-type (case reply-format
				      (8 :unsigned-char)
				      (16 :unsigned-short)
				      (32 :unsigned-long)))
		    (ret (ecase reply-format
			   ((0) nil)
			   ((8 16 32)
			    (loop for i below nitems collect
				  (cffi:mem-aref (cffi:mem-ref p :pointer)
						 cffi-elem-type
						 i))))))
	       (unless (cffi:null-pointer-p p)
		 (cffi:foreign-funcall "XFree" :pointer
				       (cffi:mem-ref p :pointer) :int))
	       (values ret
		       (and (plusp reply-type)
			    (atom-name reply-type :xdisplay-ptr xdisplay-ptr))
		       reply-format
		       bytes-after
		       cffi-elem-type)))))))

(defun change-window-property (xid prop data type format &key
			       xdisplay-ptr (mode :replace) (start 0) end)
  (let* ((prop-mode (ecase mode
		      (:replace 0)	;PropModeReplace
		      (:prepend 1)
		      (:append 2)))
	 (data-seq (etypecase data
		     (atom (unless (zerop start) (setq start 0))
			   (if (and end (not (= end 1))) (setq end 1))
			   (list data))
		     (sequence (subseq data start end))))
	 (nitems (- (or end (length data-seq)) start))
	 (cffi-elem-type (ecase format
			   (8 :unsigned-char)
			   (16 :unsigned-short)
			   (32 :unsigned-long)))
	 (size (* nitems (cffi:foreign-type-size cffi-elem-type))))
    (cffi:with-foreign-pointer (p size)
      (let ((i 0))
	(map nil (lambda (x)
		   (setf (cffi:mem-aref p cffi-elem-type i) x)
		   (incf i))
	     data-seq))
      (prog1
	  (cffi:foreign-funcall "XChangeProperty"
				:pointer xdisplay-ptr
				:unsigned-long xid
				:unsigned-long
				(ensure-atom prop :xdisplay-ptr xdisplay-ptr)
				:unsigned-long
				(ensure-atom type :xdisplay-ptr xdisplay-ptr)
				:int format
				:int prop-mode
				:pointer p
				:int nitems
				:int)
	(cffi:foreign-funcall "XSync" :pointer xdisplay-ptr :boolean nil :int)))))

#||
(get-window-property  $xid "_MOTIF_WM_HINTS" :xdisplay-ptr $xdisplay-ptr)
;; show decorations
(change-window-property $xid "_MOTIF_WM_HINTS" '(2 0 1 0 0) "CARDINAL" 32
			:xdisplay-ptr $xdisplay-ptr)
;; hide decorations
(change-window-property $xid "_MOTIF_WM_HINTS" '(2 0 2 0 0) "CARDINAL" 32
			:xdisplay-ptr $xdisplay-ptr)
||#



;;; ----------------------------------------------------------------------
;;;
;;;
;;;

#||
#define PropertyNotify		28
typedef struct {
	int type;
	unsigned long serial;	/* # of last request processed by server */
	Bool send_event;	/* true if this came from a SendEvent request */
	Display *display;	/* Display the event was read from */
	Window window;
	Atom atom;
	Time time;
	int state;		/* NewValue, Deleted */
} XPropertyEvent;
#define PropertyNewValue	0
#define PropertyDelete		1
||#

(cffi:defcstruct property-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (window-id :unsigned-long)
  (atom :unsigned-long)
  (time :unsigned-long)
  (state :int))

#+nil
(= (cffi:foreign-type-size '(:struct property-event)) 64)

