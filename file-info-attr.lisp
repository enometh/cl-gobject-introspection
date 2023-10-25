;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Jun 21 17:15:06 2019 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2023 Madhu.  All Rights Reserved.
;;;
;;; GIO FILE-INFO and FILE-ATTRIBUTES
;;;
;;; programming pattern: informative functions typically return a
;;; second value, a gobject which is passed into it (via a keyword
;;; object) or which was created (if not)

(in-package "GIR-LIB")

(eval-when (load eval compile)
(defvar +file-info-attribute-type-map+
  (loop for (str . val ) in (gir:nlist-desc *gio*  "FileAttributeType")
	collect (cons (intern (string-upcase str) "KEYWORD") val))
  "\((:invalid . 0) (:string . 1) (:byte_string . 2) (:boolean . 3) (:uint32 . 4)
    \(:int32 . 5) (:uint64 . 6) (:int64 . 7) (:object . 8) (:stringv . 9))"))

(defun convert-file-info-attribute-type (type)
  (check-type type (or number keyword))
  (macrolet ((grab-key (x)
	       `(ecase ,x
		  ,@(loop for (k . v) in +file-info-attribute-type-map+
			  collect (list k v))))
	     (grab-num (x)
	       `(ecase ,x
		  ,@(loop for (k . v) in +file-info-attribute-type-map+
			  collect (list v k)))))
    (etypecase type
      (number (grab-num type))
      (keyword (grab-key type)))))

(defun file-info-for-path (path &key
			   (attributes "*")
			   (file-query-info-flags
			    (gir:nget *gio* "FileQueryInfoFlags" :nofollow-symlinks))
			   (gfile
			    (gir:invoke (*gio* "file_new_for_path") path)))
  (values
   (gir:invoke (gfile "query_info")
     attributes
     file-query-info-flags
     nil)
   gfile))

#||
(zerop (convert-file-info-attribute-type :invalid))
(eq :invalid (convert-file-info-attribute-type 0))
(convert-file-info-attribute-type :barf)
(multiple-value-setq ($fi $f) (file-info-for-path "/etc/passwd"))
(multiple-value-setq ($fi $f) (file-info-for-path "/etc/passwd" ))
(gir:invoke ($f "get_path"))
||#

(defun file-info-get-attribute-type (file-info-obj attribname)
  "second return value is a keyword describing the type."
  (let ((file-info-attribute-type
	 (gir:invoke (file-info-obj "get_attribute_type") attribname)))
    (values file-info-attribute-type
	    (convert-file-info-attribute-type file-info-attribute-type))))

#||
(file-info-get-attribute-type $fi "standard::name")
;; => 2, :BYTE_STRING
||#

(defun file-info-has-attribute (file-info-obj attribname)
  (gir:invoke (file-info-obj "has_attribute") attribname))

(defun file-info-has-namespace (file-info-obj attribname)
  (gir:invoke (file-info-obj "has_namespace") attribname))

(defun file-info-get-attribute (file-info-obj attribname &optional (file-info-attribute-type (gir:invoke (file-info-obj "get_attribute_type") attribname)))
  (unless (numberp file-info-attribute-type)
    (setq file-info-attribute-type
	  (convert-file-info-attribute-type file-info-attribute-type)))
  (ecase file-info-attribute-type
    (#.(gir:nget *gio* "FileAttributeType" :invalid)
       ;;indicates an invalid or uninitalized type.
       (error "Invalid Attribute ~A for ~A" attribname file-info-obj))
    (#.(gir:nget *gio* "FileAttributeType" :string)
       ;;a null terminated UTF8 string.
       (gir:invoke (file-info-obj "get_attribute_string") attribname))
    (#.(gir:nget *gio* "FileAttributeType" :byte-string)
       ;; a zero terminated string of non-zero bytes.
       (gir:invoke (file-info-obj "get_attribute_byte_string") attribname))
    (#.(gir:nget *gio* "FileAttributeType" :boolean)
       ;;a boolean value.
       (gir:invoke (file-info-obj "get_attribute_boolean") attribname))
    (#.(gir:nget *gio* "FileAttributeType" :uint32)
       ;;an unsigned 4-byte/32-bit integer.
       (gir:invoke (file-info-obj "get_attribute_uint32") attribname))
    (#.(gir:nget *gio* "FileAttributeType" :int32)
       (gir:invoke (file-info-obj "get_attribute_int32") attribname))
    (#.(gir:nget *gio* "FileAttributeType" :uint64)
       ;;an unsigned 8-byte/64-bit integer.
       (gir:invoke (file-info-obj "get_attribute_uint64") attribname))
    (#.(gir:nget *gio* "FileAttributeType" :int64)
       ;;a signed 8-byte/64-bit integer.
       (gir:invoke (file-info-obj "get_attribute_int64") attribname))
    (#.(gir:nget *gio* "FileAttributeType" :object)
       ;;a GObject
       (gir:invoke (file-info-obj "get_attribute_object") attribname))
    (#.(gir:nget *gio* "FileAttributeType" :stringv)
       ;;a NULL terminated char **. Since 2.22
       (gir:invoke (file-info-obj "get_attribute_stringv") attribname))))

#+nil
(gir:get-method-desc (gir:nget *gio* "FileInfo") "set_attribute")

(defun file-info-set-attribute (file-info-obj attribname file-info-attribute-type value)
  "To unset an attribute call it with FILE-INFO-ATTRIBUTE-TYPE 0 and VALUE nil.
If Value is a CFFI pointer, calls g_file_info_set_attribute, otherwise calls
an appropriate g_file_info_set_attribute_* function."
  (unless (numberp file-info-attribute-type)
    (setq file-info-attribute-type
	  (convert-file-info-attribute-type file-info-attribute-type)))
  (if (or (cffi:pointerp value) (zerop file-info-attribute-type))
      (gir:invoke (file-info-obj "set_attribute")
	attribname
	file-info-attribute-type
	(or value (cffi:null-pointer)))
      (ecase file-info-attribute-type
	(#.(gir:nget *gio* "FileAttributeType" :string)
	   ;;a null terminated UTF8 string.
	   (gir:invoke (file-info-obj "set_attribute_string") attribname value))
	(#.(gir:nget *gio* "FileAttributeType" :byte-string)
	   ;; a zero terminated string of non-zero bytes.
	   (gir:invoke (file-info-obj "set_attribute_byte_string") attribname value))
	(#.(gir:nget *gio* "FileAttributeType" :boolean)
	   ;;a boolean value.
	   (gir:invoke (file-info-obj "set_attribute_boolean") attribname value))
	(#.(gir:nget *gio* "FileAttributeType" :uint32)
	   ;;an unsigned 4-byte/32-bit integer.
	   (gir:invoke (file-info-obj "set_attribute_uint32") attribname value))
	(#.(gir:nget *gio* "FileAttributeType" :int32)
	   (gir:invoke (file-info-obj "set_attribute_int32") attribname value))
	(#.(gir:nget *gio* "FileAttributeType" :uint64)
	   ;;an unsigned 8-byte/64-bit integer.
	   (gir:invoke (file-info-obj "set_attribute_uint64") attribname value))
	(#.(gir:nget *gio* "FileAttributeType" :int64)
	   ;;a signed 8-byte/64-bit integer.
	   (gir:invoke (file-info-obj "set_attribute_int64") attribname value))
	(#.(gir:nget *gio* "FileAttributeType" :object)
	   ;;a GObject
	   (gir:invoke (file-info-obj "set_attribute_object") attribname value))
	(#.(gir:nget *gio* "FileAttributeType" :stringv)
	   ;;a NULL terminated char **. Since 2.22
	   (gir:invoke (file-info-obj "set_attribute_stringv") attribname value)))))

(defsetf file-info-get-attribute file-info-set-attribute)

(defun file-info-get-attributes-list (file-info-obj &optional namespace)
  (gir:invoke (file-info-obj "list_attributes") namespace))

#+nil
(file-info-get-attributes-list $fi)

(defun file-info-ls-attributes (file-info-obj &optional namespace)
  "Returns an alist"
  (mapcar (lambda (attribname)
	    (cons attribname (file-info-get-attribute file-info-obj attribname)))
	  (file-info-get-attributes-list file-info-obj namespace)))

#+nil
(defvar +default-attributes+
  "metadata::*,standard::*,access::*,time::modified,unix::mode")

#||
From g_file_query_info:

The attributes value is a string that specifies the file attributes that should be gathered. It is not an error if it's not possible to read a particular requested attribute from a file - it just won't be set. attributes should be a comma-separated list of attributes or attribute wildcards. The wildcard "*" means all attributes, and a wildcard like "standard::*" means all attributes in the standard namespace. An example attribute query be "standard::*,owner::user". The standard attributes are available as defines, like G_FILE_ATTRIBUTE_STANDARD_NAME.

From g_file_attribute_matcher_new

The attributes string should be formatted with specific keys separated from namespaces with a double colon. Several "namespace::key" strings may be concatenated with a single comma (e.g. "standard::type,standard::is-hidden"). The wildcard "*" may be used to match all keys and namespaces, or "namespace::*" will match all keys in a given namespace.

Examples of file attribute matcher strings and results

"*": matches all attributes.

"standard::is-hidden": matches only the key is-hidden in the standard namespace.

"standard::type,unix::*": matches the type key in the standard namespace and all keys in the unix namespace.

To change the actual attributes of a file, you should then set the attribute in the GFileInfo and call g_file_set_attributes_from_info() or g_file_set_attributes_async() on a GFile.

However, not all attributes can be changed in the file. For instance, the actual size of a file cannot be changed via g_file_info_set_size(). You may call g_file_query_settable_attributes() and g_file_query_writable_namespaces() to discover the settable attributes of a particular file at runtime.
||#

(defun ls-attributes (path &key (attributes "*") namespace
		      (file-query-info-flags
		       (gir:nget *gio* "FileQueryInfoFlags" :nofollow-symlinks))
		      (gfile (gir:invoke (*gio* "file_new_for_path") path))
		      file-info &aux gfile1)
  "Unless file-info is supplied, freshly queries information and returns
the file-info object as the second value."
  (unless file-info
    (multiple-value-setq (file-info gfile1)
      (file-info-for-path path :attributes attributes
			  :file-query-info-flags file-query-info-flags
			  :gfile gfile))
    (assert (eql gfile1 gfile)))
    (values
     (file-info-ls-attributes file-info namespace)
     file-info
     gfile)))

(defun file-info-remove-attribute (file-info-obj attribname)
  (gir:invoke (file-info-obj "remove_attribute") attribname))

(defun get-attribute-infos-from-attribute-info-list (attribute-info-list-obj)
  (let ((ptr (gir::this-of (gir:field attribute-info-list-obj "infos")))
	(sz (gir:struct-info-get-size (gir::info-of (gir:nget *gio* "FileAttributeInfo")))))
    (loop for i below (gir:field attribute-info-list-obj "n_infos") collect
	  (gir::build-struct-ptr (gir:nget *gio* "FileAttributeInfo")
				 (cffi:inc-pointer ptr (* sz i))))))

(defun match-attribute-info-name (string attribute-info-list-obj &optional
				  (attribute-infos
				   (get-attribute-infos-from-attribute-info-list
				    attribute-info-list-obj)))
  (some (lambda (s) (equal string s))
	(mapcar (lambda (attribute-info)
		  (gir:field attribute-info "name"))
		attribute-infos)))

(defun check-file-set-attributes-from-info (gfile file-info-obj)
  "Check if the attributes in file-info-obj can be set on gfile. This is
a runtime check. We skip writeable namespaces. The results are not
reliable."
  (let ((settable-attribute-info-list (gir:invoke (gfile "query_settable_attributes") nil))
	(writeable-namespaces
	 (mapcar (lambda (attribute-info)
		   (gir:field attribute-info "name"))
		 (get-attribute-infos-from-attribute-info-list
		  (gir:invoke (gfile "query_writable_namespaces") nil)))))
    (loop for attribname in (file-info-get-attributes-list file-info-obj)
	  for p = (search "::" attribname)
	  unless (or (some (lambda (x)
			     (user::prefixp attribname x :end1 p))
			   writeable-namespaces)
		     (match-attribute-info-name attribname
						settable-attribute-info-list))
	  return (values nil attribname)
	  finally (return t))))

#||
(setq $ail (gir:invoke ($f "query_settable_attributes") nil))
(mapcar 'gir:fields (get-attribute-infos-from-attribute-info-list $ail))
(match-attribute-info-name "time::modified" $ail)
||#


(defun file-has-writable-namespace (gfile namespace &optional
				    (attribute-info-list
				     (gir:invoke (gfile "query_writable_namespaces") nil)))
  (match-attribute-info-name namespace attribute-info-list))

(defun file-has-settable-attribute (gfile attribname &optional
				    (attribute-info-list
				     (gir:invoke (gfile "query_settable_attributes") nil)))
  (match-attribute-info-name attribname attribute-info-list))

(defun file-set-attributes-from-info (gfile file-info-obj &key
				      (file-query-info-flags
				       (gir:nget *gio* "FileQueryInfoFlags" :nofollow-symlinks))
				      (check t))
  (when check
    ;; XXX xattr::* are settable, but the check
    ;; fails. standard::symlink-target is settable but using it
    ;; deletes the file.
    (multiple-value-bind (ok fail)
	(check-file-set-attributes-from-info gfile file-info-obj)
      (unless ok
	(error "attribute ~a is not settable for file ~a"
	       fail (gir:invoke (gfile "get_path"))))))
  (gir:invoke (gfile "set_attributes_from_info")
    file-info-obj
    (or file-query-info-flags
	)
    nil))


#+nil
(gir:nlist-desc *gio* "FileAttributeInfoFlags")
;; => (("none" . 0) ("copy_with_file" . 1) ("copy_when_moved" . 2))

#+nil
(gir:nget *gio* "FileAttributeMatcher")

#||
from gattribute
File attributes in GIO consist of a list of key-value pairs.

Keys are strings that contain a key namespace and a key name, separated by a colon, e.g. "namespace::keyname". Namespaces are included to sort key-value pairs by namespaces for relevance. Keys can be retrieved using wildcards, e.g. "standard::*" will return all of the keys in the "standard" namespace.

[...]

Note that there are no predefined keys in the "xattr" and "xattr-sys" namespaces. Keys for the "xattr" namespace are constructed by stripping away the "user." prefix from the extended user attribute, and prepending "xattr::". Keys for the "xattr-sys" namespace are constructed by concatenating "xattr-sys::" with the extended attribute name. All extended attribute values are returned as hex-encoded strings in which bytes outside the ASCII range are encoded as escape sequences of the form \xnn where nn is a 2-digit hexadecimal number.
||#


(defun escape-byte-string (str)
  "from glib"
  (flet ((valid-char (c)
	   (and (>= c 32) (<= c 126) (not (eql c #.(char-code #\\))))))
    (let* ((byte-str (if (< (length str) 1)
			 (return-from escape-byte-string "")
			 (if (typep (elt str 0) '(unsigned-byte 8))
			     str
			     (babel:string-to-octets str))))
	   (hex-digits "0123456789abcdef")
	   (len (length byte-str))
	   (num-invalid
	    (loop for i below len for c across byte-str
		  if (not (valid-char c))
		  count 1)))
      (if (zerop num-invalid)
	  str
	  (let ((val (make-string (+ len (* num-invalid 3))))
		(p 0))
	    (loop for i below len
		  for c = (elt byte-str i)
		  do (assert (< c 256))
		  do (cond ((valid-char c)
			    (setf (elt val p) (code-char c))
			    (incf p))
			   (t (setf (elt val p) #\\)
			      (incf p)
			      (setf (elt val p) #\x)
			      (incf p)
			      (setf (elt val p)
				    (elt hex-digits (logand #xf (ash c -4))))
			      (incf p)
			      (setf (elt val p)
				    (elt hex-digits (logand #xf c)))
			      (incf p))))
	    val)))))

#+nil
(unescape-byte-string
 (escape-byte-string "foo
bar"))


(defun unescape-byte-string (string &key (start 0) end return-type)
  ;;madhu 210813
  (let* (babel-used
	 (byte-str (if (< (length string) 1)
		       (return-from unescape-byte-string string)
		       (if (typep (elt string 0) '(unsigned-byte 8))
			   string
			   (progn
			     (setq babel-used t)
			     (babel:string-to-octets string :start start
						     :end end)))))
	 (beg (if babel-used 0 start))
	 (end (if babel-used (length byte-str) (or end (length string))))
	 (len (- end beg))
	 (out (make-array len :element-type '(unsigned-byte 8)
			  :initial-element 0
			  :fill-pointer t))
	 i a b
	 (p 0))
    (loop (cond ((and (< beg end)
		      (setq i (search #(#.(char-code #\\) #.(char-code #\x))
				      byte-str :start2 beg :end2 end))
		      (<= (+ i 4) end)
		      (eql (elt byte-str (+ i 0)) #.(char-code #\\))
		      (eql (elt byte-str (+ i 1)) #.(char-code #\x))
		      (setq a (digit-char-p (code-char (elt byte-str (+ i 2))) 16))
		      (setq b (digit-char-p (code-char (elt byte-str (+ i 3))) 16)))
		 (replace out byte-str :start1 p :start2 beg :end2 i)
		 (incf p (- i beg))
		 (setf (elt out p) (+ (* a 16)
				      (* b 1)))
		 (incf p)
		 (setq beg (+ i 4)  i nil))
		(i (replace out byte-str :start1 p :start2 beg :end2 (+ i 1))
		   (incf p (- (+ i 1) beg))
		   (setq beg (+ i 1) i nil))
		(t (replace out byte-str :start1 p :start2 beg :end2 end)
		   (incf p (- end beg))
		   (return t))))
    (setf (fill-pointer out) p)
    (if (eql return-type 'vector)
	(values out p)
	(babel:octets-to-string out :end p))))
