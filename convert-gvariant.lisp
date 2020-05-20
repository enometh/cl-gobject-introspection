;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: [Sun Nov 26 14:10:37 2017 +0530] <enometh@meer.net>
;;;   Bugs-To: enometh@meer.net
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2017 Madhu.  All Rights Reserved.
;;;
;;; from 1.l
;;;
;;; recursive un/marshalling
;;;
;;; gvariant arrays of dict entries are converted to/from lisp
;;; hash-tables

(in-package "GIR-LIB")

(defun parse-single-complete-type (str &key (start 0))
  (let ((i start))
    (cond ((find (elt str i) "ybnqiuxtdsogv")
	   (values (string (elt str i)) (1+ i)))
	  ((eql (elt str i) #\()	; handle nesting
	   (cond ((eql (elt str (1+ i)) #\()
		  (multiple-value-bind (typ2 idx2)
		      (parse-single-complete-type str :start (1+ i))
		    (values (list typ2) (1+ idx2))))
		 (t (let ((p (position #\) str :start (1+ i))))
		      (values (subseq str i (1+ p)) (1+ p))))))
	  ((eql (elt str i) #\a)
	   (if (eql (elt str (1+ i)) #\{) ;; a{..} is a full complete type
	       (let (typ1 index1 typ2 index2)
		 (declare (ignorable typ1 typ2))
		 (multiple-value-setq (typ1 index1)
		   (parse-single-complete-type str :start (+ 2 i)))
		 (multiple-value-setq (typ2 index2)
		   (parse-single-complete-type str :start index1))
		 (assert (eql (elt str index2) #\}))
		 (values (subseq str i (1+ index2)) (1+ index2)))
	       (multiple-value-bind (typ index)
		   (parse-single-complete-type str :start (1+ i))
		 (declare (ignore typ))
		 (values (subseq str i index) index))))
	  (t (error "parsing single complete type: ~S ~S" str (subseq str start))))))

(defun parse-signature (str &key (start 0) (end (length str)))
  "Return a list of single complete types. Tuples are returned in sub
lists."
  (let ((ret nil) (i start))
    (loop (cond ((= i end) (return (nreverse ret)))
		(t (multiple-value-bind (sig idx)
		       (parse-single-complete-type str :start i)
		     (push (cond ((eql (elt sig 0) #\()
				  (parse-signature sig :start 1
						   :end (1- (length sig))))
				 (t sig))
			   ret)
		     (assert (> idx i))
		     (assert (<= idx end))
		     (setq i idx)))))))

(defun parse-dict-entry (signature &key (start 0) (end (length signature)))
  ;; parse the dictionary entry fragment {ss} => ("s" "s")
  (assert (eql (elt signature start) #\{))
  (assert (eql (elt signature (1- end)) #\}))
  (parse-signature signature :start (1+ start) :end (1- end)))

(defun convert-arg-to-gvariant (arg sig)
  (if (> (length sig) 1) ; (and (typep arg 'sequence) (not (typep arg 'string)))
      (let* ((parsed (parse-signature sig))
	     (aggregate-sig (car parsed)))
	(assert (null (cddr parsed)))
	(if (consp aggregate-sig)
	    (progn (assert (eql (elt sig 0) #\())
		   (args->gvariant-tuple arg aggregate-sig))
	    (progn (assert (eql (elt aggregate-sig 0) #\a))
		   (args->gvariant-array arg (subseq aggregate-sig 1)))))
      (gir:invoke (*glib* "Variant"
			  (cond ((string-equal sig "s") "new_string")
				((string-equal sig "b") "new_boolean")
				((string-equal sig "y") "new_byte")
				((string-equal sig "s") "new_string")
				((string-equal sig "n") "new_int16")
				((string-equal sig "q") "new_uint16")
				((string-equal sig "i") "new_int32")
				((string-equal sig "u") "new_uint32")
				((string-equal sig "x") "new_int64")
				((string-equal sig "t") "new_uint64")
				((string-equal sig "d") "new_double")
				((string-equal sig "v") "new_variant")
				(t (error "Unknown signature ~S" sig))))
		  arg)))

(defun convert-gvariant-to-arg (gvariant &optional sig)
  (if sig
      (assert (equal sig (gir:invoke (gvariant "get_type_string"))))
      (setq sig (gir:invoke (gvariant "get_type_string"))))
  (if (> (length sig) 1)
      (let* ((parsed (parse-signature sig))
	     (aggregate-sig (car parsed)))
	(assert (null (cddr parsed)))
	(if (consp aggregate-sig)	; tuple
	    (progn (assert (or (eql (elt sig 0) #\()  (eql (elt sig 0) #\{)))
		   (gvariant-tuple->args gvariant aggregate-sig))
	    (progn (assert (eql (elt aggregate-sig 0) #\a))
		   (gvariant-array->args gvariant (subseq aggregate-sig 1)))))
      (gir:invoke (gvariant (cond ((string-equal sig "s") "get_string")
				  ((string-equal sig "b") "get_boolean")
				  ((string-equal sig "y") "get_byte")
				  ((string-equal sig "s") "get_string")
				  ((string-equal sig "n") "get_int16")
				  ((string-equal sig "q") "get_uint16")
				  ((string-equal sig "i") "get_int32")
				  ((string-equal sig "u") "get_uint32")
				  ((string-equal sig "x") "get_int64")
				  ((string-equal sig "t") "get_uint64")
				  ((string-equal sig "d") "get_double")
				  ((string-equal sig "v") "get_variant")
				  (t (error "Unknown signature ~S." sig)))))))

(defun args->gvariant-tuple (args arg-signatures)
  (gir:invoke (*glib* "Variant" "new_tuple")
	      (loop for arg in args
		    for sig in arg-signatures
		    collect (convert-arg-to-gvariant arg sig))))

(defun gvariant-tuple->args (gvariant-tuple arg-signatures)
  (let ((nargs (gir:invoke (gvariant-tuple "n_children"))))
    (loop for i below nargs
	  for sig in arg-signatures
	  for gvariant = (gir:invoke (gvariant-tuple "get_child_value") i)
	  collect (convert-gvariant-to-arg gvariant sig))))

(defun args->gvariant-array (seq array-element-type-sig &aux ret)
  (if (eql (elt array-element-type-sig 0) #\{) ; special case
      (let ((tuple-sig (parse-dict-entry array-element-type-sig)))
	(maphash (lambda (k v)
		   (push
		    (gir:invoke (*glib* "Variant" "new_dict_entry")
				(convert-arg-to-gvariant k (first tuple-sig))
				(convert-arg-to-gvariant v (second tuple-sig)))
		    ret))
		 seq)
	(setq ret (nreverse ret)))
      (setq ret
	    (map 'list (lambda (x)
			 (convert-arg-to-gvariant x array-element-type-sig))
		 seq)))
  (gir:invoke (*glib* "Variant" "new_array")
	      (gir:invoke (*glib* "VariantType" "new") array-element-type-sig)
	      ret))

(defun gvariant-array->args (gvariant-array array-element-type-sig)
  (let ((nargs (gir:invoke (gvariant-array "n_children"))))
    (if (eql (elt array-element-type-sig 0) #\{) ;special case
	(let ((ret (make-hash-table :test #'equal))
	      (tuple-sig (parse-dict-entry array-element-type-sig)))
	  (loop for i below nargs
		for gvariant-dict-entry =
		(gir:invoke (gvariant-array "get_child_value") i)
		for kv = (gir:invoke (gvariant-dict-entry "get_child_value") 0)
		for vv = (gir:invoke (gvariant-dict-entry "get_child_value") 1)
		for k = (convert-gvariant-to-arg kv (first tuple-sig))
		for v = (convert-gvariant-to-arg vv (second tuple-sig))
		do (setf (gethash k ret) v))
	  ret)
	(loop for i below nargs
	      for gvariant = (gir:invoke (gvariant-array "get_child_value") i)
	      collect
	      (convert-gvariant-to-arg gvariant array-element-type-sig)))))


#||
(parse-signature "a{o(oayay)}")
(parse-signature "(as)a{o(oayay)}")
(parse-dict-entry "{o(oayay)}")
||#



(defun print-arg (arg &optional (depth 0))
  (flet  ((format (stream control &rest format-args)
	    (loop for i below depth do (apply 'cl:format t (list "  ")))
	    (apply 'cl:format stream control format-args)))
    (etypecase arg
      (list (format t "(")
	    (mapcar (lambda (x) (print-arg x (1+ depth)) (terpri)) arg)
	    (format t ")~&"))
      (hash-table
       (format t "array [~&")
       (maphash (lambda (k v)
		  (format t "dict entry(~&")
		  (print-arg k (1+ depth)) ; prints newline
		  (print-arg v (1+ depth))
		  (format t ")~&"))
		arg)
       (format t "]~&"))
      (gir::struct-instance
       (if (equal (gir::struct-class-of arg) (gir:nget *glib* "Variant"))
	   (let ((fmt (gir:invoke (arg "get_type_string"))))
	     (format t "variant ~S " fmt)
	     (print-arg (convert-gvariant-to-arg
					arg
					fmt)))
	   (format t "<<~S>>~&" arg)))
      (t (format t "~S~&" arg)))))
