;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: [Sun Nov 26 14:10:37 2017 +0530] <enometh@meer.net>
;;;   Bugs-To: enometh@meer.net
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2017-2020 Madhu.  All Rights Reserved.
;;;
;;; ;madhu 200521 - rewrite using the glib algorithm. dont use
;;; hashtables use a distinct types to represent dictionary entries,
;;; arrays and tuples when parsing gvariant type signatures.

(in-package "GIR")

;; ;madhu 200521 fixme avoid defining *glib* as a variable
;;(define-symbol-macro *glib* (require-namespace "GLib" "2.0"))
(defvar *glib* (require-namespace "GLib" "2.0"))

(defstruct dict-entry k v)

;; code from GLIB
;; Return (values RET end-position child-depth) or NIL
(defun variant-type-string-scan-internal (string &optional (start 0) (end (length string)) (depth-limit 65))
  (prog ((max-depth 0) end1 child-depth ret1 ret c d)
     (assert string)
     (if (= start end) (return nil))
     (ecase (setq c (elt string (prog1 start (incf start))))
       (#\(
	(loop while (or (= start end) (not (eql (elt string start) #\))))
	      do (if (zerop depth-limit) (return nil))
		 (multiple-value-setq (ret1 end1 child-depth)
		   (variant-type-string-scan-internal string start end (1- depth-limit)))
		 (if (not ret1) (return nil))
		 (assert (listp ret))
		 (push ret1 ret)
		 (setq start end1)
		 (setq max-depth (max max-depth (1+ child-depth))))
	(setq ret (nreverse ret))
	(incf start))
       (#\{
	(if (= depth-limit 0) (return nil))
	(if (= start end) (return nil))
	(if (not (find (setq d (elt string (prog1 start (incf start)))) "bynqihuxtdsog?"))
	    (return nil))
	(multiple-value-setq (ret1 end1 child-depth)
	  (variant-type-string-scan-internal string start end (1- depth-limit)))
	(unless ret1 (return nil))
	(setq start end1)
	(if (= start end) (return nil))
	(if (not (eql (elt string (prog1 start (incf start))) #\})) (return nil))
	(assert (null ret))
	(setq ret (make-dict-entry :k (string d) :v ret1))
	(setq max-depth (max max-depth (1+ child-depth))))
       ((#\m #\a)
	(if (= depth-limit 0) (return nil))
	(multiple-value-setq (ret1 end1 child-depth)
	  (variant-type-string-scan-internal string start end (1- depth-limit)))
	(if (not ret1) (return nil))
	(assert (null ret))		;FIXME
	(setq ret (vector ret1))
	(setq start end1)
	(setq max-depth (max max-depth (1+ child-depth))))
       ((#\b #\y #\n #\q #\i #\u #\x #\t #\d #\s #\o #\g #\v #\r #\* #\? #\h)
	(assert (null ret))
	(setq ret (string c))
	(setq max-depth (max max-depth 1)))
       (t (return nil)))
     (return (values ret start max-depth))))

#||
(variant-type-string-scan-internal "aa{ss}") ; #(#(#S(DICT-ENTRY :K "s" :V "s"))), 6, 4
(variant-type-string-scan-internal "as") ; #("s"), 2, 2
(variant-type-string-scan-internal "{s(u)}" 0 6) ; #S(DICT-ENTRY :K "s" :V ("u")), 6, 3
(variant-type-string-scan-internal "(su)" 0 4) ; ("s" "u"), 4, 2
(variant-type-string-scan-internal "a{ss}") ; #(#S(DICT-ENTRY :K "s" :V "s")), 5, 3
(variant-type-string-scan-internal "(asas)") ; (#("s") #("s")), 6, 3
(variant-type-string-scan-internal "(a{ss})") ; (#(#S(DICT-ENTRY :K "s" :V "s"))), 7, 4
||#

(defun unparse-sig (sig)
  (etypecase sig
    (null "()")
    (string sig)
    (array
     (assert (= 1 (length sig)))
     (concatenate 'string "a" (unparse-sig (elt sig 0))))
    (dict-entry (concatenate 'string "{" (unparse-sig (dict-entry-k sig))
			    (unparse-sig (dict-entry-v sig)) "}"))
    (cons
     (with-output-to-string (stream)
       (write-char #\( stream)
       (loop for x in sig
	     do (write-string (unparse-sig x) stream))
       (write-char #\) stream)))))


#||
(unparse-sig #("s")) ; "as"
(unparse-sig #(#S(dict-entry :k "s" :v "s"))) ; "a{ss}"
||#

(defun struct-instance-is-gvariant (arg)
  (and (typep arg 'struct-instance)
       (equal (struct-class-of arg) (nget *glib* "Variant"))))

;; INCOMPLETE! can't handle most numbers
(defun unparse-to-sig (arg)
  (etypecase arg
    (string "s")
    (boolean "b")
    (array (concatenate 'string "a" (unparse-to-sig (elt arg 0))))
    (dict-entry (concatenate 'string "{" (unparse-to-sig (dict-entry-k arg))
			     (unparse-to-sig (dict-entry-v arg)) "}"))
    (cons (with-output-to-string (stream)
	    (write-char #\( stream)
	    (map nil (lambda (x) (write-string (unparse-to-sig x) stream)) arg)
	    (write-char #\) stream)))
    ((integer 0 #.(expt 2 32)) "u")
    ((integer #.(- (expt 2 31)) #.(expt 2 31)) "i") ; or h?
    ((satisfies struct-instance-is-gvariant) "v")
    (float "d")))

#||
(unparse-to-sig #(1 2 3)) ; "au"
(unparse-to-sig #(1 2 -3)) ; "au"
(unparse-to-sig #(#s(dict-entry :k 2 :v "foo"))) ; "a{us}"
||#

(defun convert-to-gvariant (arg sig &optional (depth 0))
  (if (zerop depth) ;; (assert (stringp sig))
      (cond ((stringp sig) t)
	    (t (setq sig (unparse-sig sig)))))
  (let ((parsed-sig (if (zerop depth) (variant-type-string-scan-internal sig) sig)))
    (etypecase parsed-sig
      (string
       (assert (= (length parsed-sig) 1))
       (invoke (*glib* "Variant"
		       (cond ((string-equal sig "s") "new_string")
			     ((string-equal sig "b") "new_boolean")
			     ((string-equal sig "y") "new_byte")
			     ((string-equal sig "s") "new_string")
			     ((string-equal sig "n") "new_int16")
			     ((string-equal sig "q") "new_uint16")
			     ((or (string-equal sig "i")
				  (string-equal sig "h"))
			      "new_int32")
			     ((string-equal sig "u") "new_uint32")
			     ((string-equal sig "x") "new_int64")
			     ((string-equal sig "t") "new_uint64")
			     ((string-equal sig "d") "new_double")
			     ((string-equal sig "v") "new_variant")
			     ((string-equal sig "o") "new_object_path")
			     (t (error "Unknown signature ~S" sig))))
	       arg))
      (array
       (assert (=  (length parsed-sig) 1))
       (assert (typep arg 'sequence))
       (invoke (*glib* "Variant" "new_array")
	       (invoke (*glib* "VariantType" "new")
			   (unparse-sig (elt parsed-sig 0)))
	       (map 'list (lambda (x)
			    (convert-to-gvariant x  (elt parsed-sig 0) (1+ depth)))
		    arg)))
      (dict-entry
       ;; handle arg as (k v) or (k . v) or #S(dict-entry :k k :v v)
       (multiple-value-bind (k v)
	   (etypecase arg
	     (dict-entry (values (dict-entry-k arg) (dict-entry-v arg)))
	     (sequence
	      (ecase (length arg)
		(2 (values (elt arg 0) (elt arg 1)))
		(1 (values (car arg) (cdr arg))))))
	 (invoke (*glib* "Variant" "new_dict_entry")
		     (convert-to-gvariant k (dict-entry-k parsed-sig) (1+ depth))
		     (convert-to-gvariant v (dict-entry-v parsed-sig) (1+ depth)))))
      (cons				; tuple
       (invoke (*glib* "Variant" "new_tuple")
		   (loop for arg in arg
			 for sig in parsed-sig
			 collect (convert-to-gvariant arg sig (1+ depth))))))))

(defun convert-from-gvariant (gvariant &optional sig (depth 0))
  (when (zerop depth)
    (if sig
	(let ((s (invoke (gvariant "get_type_string"))))
	  (cond ((equalp sig s) t)
		((equalp (unparse-sig sig) s) (setq sig s))
		(t (error "gvariant sig=~S but get_type_string=~S" sig s))))
	(setq sig (invoke (gvariant "get_type_string")))))
  (let ((parsed-sig (if (zerop depth) (variant-type-string-scan-internal sig) sig)))
    (etypecase parsed-sig
      (null nil)
      (string
       (assert (= (length parsed-sig) 1))
       (invoke (gvariant (cond ((or (string-equal sig "s")
				    (string-equal sig "g"))
				"get_string")
			       ((string-equal sig "b") "get_boolean")
			       ((string-equal sig "y") "get_byte")
			       ((string-equal sig "s") "get_string")
			       ((string-equal sig "n") "get_int16")
			       ((string-equal sig "q") "get_uint16")
			       ((or (string-equal sig "i")
				    (string-equal sig "h"))
				"get_int32")
			       ((string-equal sig "u") "get_uint32")
			       ((string-equal sig "x") "get_int64")
			       ((string-equal sig "t") "get_uint64")
			       ((string-equal sig "d") "get_double")
			       ((string-equal sig "v") "get_variant")
			       ((string-equal sig "o") "get_string")
			       (t (error "Unknown signature ~S." sig))))))
      (array
       (assert (=  (length parsed-sig) 1))
       (let ((array-element-type-sig (elt parsed-sig 0))
	     (nargs (invoke (gvariant "n_children"))))
	 (funcall
	  #'make-array nargs :initial-contents
	  (loop for i below nargs
		for gvariant-elem = (invoke (gvariant "get_child_value") i)
		collect
		(convert-from-gvariant gvariant-elem array-element-type-sig (1+ depth))))))
      (dict-entry
       (let* ((kv (invoke (gvariant "get_child_value") 0))
	      (vv (invoke (gvariant "get_child_value") 1))
	      (k (convert-from-gvariant kv (dict-entry-k parsed-sig) (1+ depth)))
	      (v (convert-from-gvariant vv (dict-entry-v parsed-sig) (1+ depth))))
	 (make-dict-entry :k k :v v)))
      (cons
       (let ((nargs (invoke (gvariant "n_children"))))
	 (loop for i below nargs
	       for sig in parsed-sig
	       for gvariant-elem = (invoke (gvariant "get_child_value") i)
	       collect (convert-from-gvariant gvariant-elem sig (1+ depth))))))))

(defun parse-signature (str &key (start 0) (end (length str)))
  "Return a list of single complete types. Tuples are returned in sub
lists."
  (let (ret1 ret end1 depth (i start))
    (declare (ignorable depth))
    (loop (multiple-value-setq (ret1 end1 depth)
	    (variant-type-string-scan-internal str i end))
	  (assert ret1)
	  (push ret1 ret)
	  (setq i (1+ end1))
	  (if (>= i end) (return (nreverse ret))))))

#||
(parse-signature "a{o(oayay)}")
(parse-signature "(as)a{o(oayay)}")
(parse-signature "{o(oayay)}")
||#

(defun print-arg (arg &optional (stream *standard-output*) (depth 0))
  (flet  ((format (stream control &rest format-args)
	    (loop for i below depth do (apply 'cl:format stream (list "  ")))
	    (apply 'cl:format stream control format-args)))
    (etypecase arg
      ((and array (not string))
       (format stream "array [~&")
       (map nil (lambda (x)
		  (print-arg x stream (1+ depth))
		  #+nil(terpri stream))
	    arg)
       (format stream "]~&"))
      (dict-entry
       (format stream "dict entry(~&")
       (print-arg (dict-entry-k arg) stream (1+ depth)) ; prints newline
       (print-arg (dict-entry-v arg) stream (1+ depth))
       (format stream ")~&"))
      (cons (format t "(")
       (map nil (lambda (x)
		  (print-arg x stream (1+ depth))
		  #+nil(terpri stream))
	    arg)
       (format stream ")~&"))
      (t (format stream "~S~&" arg)))))

#+nil
(with-output-to-string (*standard-output*)
  (print-arg #(1 2 #S(dict-entry :k "key" :v "val") 3)))