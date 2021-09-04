;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri May 24 16:40:41 2019 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2019-2021 Madhu.  All Rights Reserved.
;;;
;;; GSETTINGS
;;;
(in-package "GIR-LIB")

(defvar *settings-schema-source*
  (gir:invoke (*gio* "settings_schema_source_get_default")))

(defun settings-source-new-from-directory (directory &key parent (trusted t))
  (gir:invoke (*gio* "SettingsSchemaSource" "new_from_directory")
	      directory
	      (or parent *settings-schema-source*)
	      trusted))

(defmacro with-settings-schema-source (settings-schema-source &body body)
  `(let ((*settings-schema-source* ,settings-schema-source))
     ,@body))

(defun settings-list-schemas (&key ((:settings-schema-source *settings-schema-source*) *settings-schema-source*) (relocatable t) non-relocatable (recursive t))
  (multiple-value-bind (dummy non-reloc reloc)
      (gir:invoke (*settings-schema-source* "list_schemas") recursive)
    (declare (ignore dummy))
    (cond ((and non-relocatable relocatable)
	   (append non-reloc reloc))
	  (relocatable reloc)
	  (non-relocatable non-reloc)
	  (t (values reloc non-reloc)))))

(defun gir-check-name (obj name &optional namespace-name)
  "Check if OBJ has NAME in g-i."
  (let ((obj (if (slot-exists-p obj 'gir::info)
		 ;; obj is an interface-desc, object-class or
		 ;; struct-class
		 obj
		 ;; obj is an object-instance or struct-instance
		 (slot-value obj 'gir::class))))
    (let ((info (slot-value obj 'gir::info)))
      (or (and (equalp name (gir:info-get-name info))
	       (or (null namespace-name)
		   (equalp namespace-name
			   (gir:info-get-namespace info))))
	  (error "~S is not a ~@[~A:~]~A" obj namespace-name name)))))

(defun settings-schema-lookup (schema-id &key (recursive-p t) ((:settings-schema-source *settings-schema-source*) *settings-schema-source*) if-does-not-exist)
  "Return a GSettingsSchema object or NIL \(unless IF-DOES-NOT-EXIST is
:ERROR)"
  (etypecase schema-id
    (string
     (let ((ret (gir:invoke (*settings-schema-source* "lookup") schema-id
			    recursive-p)))
       (or ret
	   (case if-does-not-exist
	     (:error (error "~S: no such schema" schema-id))))))
    ;; the other clauses are for convenience: (from ensure-schema)
    (gir::object-instance
     (gir-check-name schema-id "Settings" "Gio")
     (gir::build-struct-ptr (gir:nget *gio* "SettingsSchema")
			    (gir:property schema-id "settings-schema")))
    (gir::struct-instance
     (gir-check-name schema-id "SettingsSchema" "Gio")
     schema-id)))

(defun settings-schema-non-relocatable-path (settings-schema)
  (gir:invoke ((settings-schema-lookup settings-schema
				       :if-does-not-exist :error)
	       "get_path")))

(defun settings-schema-relocatable-p (settings-schema)
  "Schemas may be single-instance or relocatable.
Single-instance schemas correspond to exactly one set of keys in the backend database: The path is returned as the second return value.

Relocatable schemas can be referenced by other schemas and can therefore describe multiple sets of keys at different locations. For relocatable schemas, The path is null"
  (let ((p (gir:invoke ((settings-schema-lookup settings-schema
						:if-does-not-exist :error)
			"get_path"))))
    (if p (values nil p) t)))

(defun settings-lookup (schema-id &optional path &key ((:settings-schema-source *settings-schema-source*) *settings-schema-source*) settings-backend if-does-not-exist (recursive-p t))
  "Return a GSettings object. or NIL \(unless IF-DOES-NOT-EXIST is :ERROR)"
  (etypecase schema-id
    (string
     (let ((schema
	    (settings-schema-lookup schema-id :recursive-p recursive-p
				    :if-does-not-exist if-does-not-exist)))
       (when schema
	 (when (and (settings-schema-relocatable-p schema)
		    (not path))
	   (error "Cannot create a settings object for relocatable schema ~S without supplying a path" schema-id))
	 (gir:invoke (*gio* "Settings" "new_full")
		     schema settings-backend path))))
    ;; convenience clauses - from (ensure-settings)
    (gir::object-instance
     (gir-check-name schema-id "Settings" "Gio")
     schema-id)
    (gir::struct-instance
     (let ((schema schema-id))
       (gir-check-name schema "SettingsSchema" "Gio")
       (when (and (settings-schema-relocatable-p schema)
		  (not path))
	 (error "Cannot create a settings object for relocatable schema ~S without supplying a path" schema-id))
       (gir:invoke (*gio* "Settings" "new_full")
		   schema settings-backend path)))))

(defun settings-schema-has-key (schema-id key)
  (gir:invoke ((settings-schema-lookup schema-id :if-does-not-exist :error)
	       "has_key")
	      key))

(defun settings-schema-list-keys (schema-id)
  (gir:invoke ((settings-schema-lookup schema-id :if-does-not-exist :error)
	       "list_keys")))

(defun settings-schema-key-lookup (settings-schema key &key if-does-not-exist)
  "Return a GSettingSchemaKey object. If settings-schema does not exist,
an error is raised. Returns NIL if the key does not exist unless
IF-DOES-NOT-EXIST is :ERROR"
  (let ((schema (settings-schema-lookup settings-schema :if-does-not-exist :error)))
    (cond ((gir:invoke (schema "has_key") key)
	   (gir:invoke (schema "get_key") key))
	  ((or (null if-does-not-exist)
	       (eq if-does-not-exist :soft))
	   nil)
	  (t (error "No such key: ~S: in settings schema ~S." key
		    settings-schema)))))

(defun settings-schema-get-default-value-for-key (settings-schema key)
  (let* ((schema-key
	  (settings-schema-key-lookup settings-schema key
				      :if-does-not-exist :error)))
    (gir:convert-from-gvariant (gir:invoke (schema-key "get_default_value")))))

(defun settings-schema-get-range-for-key (settings-schema key)
  "Second return value is a string that describes the first return value
as described below -

`type' - a gvariant type-string denoting - values for this key are of
that type.
`enum' - an array of allowed values for this key. (exhaustive)
`flags' - an array of values that may appear zero or one times in an array
to be used as the value for this key.
`range' - a list of (MIN MAX) values for this key."
  (let ((schema-key (settings-schema-key-lookup settings-schema key :if-does-not-exist :error)))
    (destructuring-bind (string-desc range-gvariant-value)
	(gir:convert-from-gvariant (gir:invoke (schema-key "get_range"))
				   "(sv)")
      (values
       (cond ((string-equal "type" string-desc)
	     ;; variant contains an empty array the type of which
	      ;; specifies the allowed type for the values.
	      (let* ((gvtypeobj (gir:invoke (range-gvariant-value "get_type")))
		     (sig (gir:invoke (gvtypeobj "dup_string"))))
		(assert (eql (elt sig 0) #\a))
		(subseq sig 1)))
	    ((string-equal "enum" string-desc)
	     ;; variant contains an array enumerating the possible
	     ;; values. no other values are valid.
	     (gir:convert-from-gvariant range-gvariant-value))
	    ((string-equal "flags" string-desc)
	     ;; variant contains an array each item of which is a
	     ;; value that may appear zero or one times in an array
	     ;; to be used as the value for this key.
	     (gir:convert-from-gvariant range-gvariant-value))
	    ((string-equal "range" string-desc)
	     ;; list of (min max)
	     (gir:convert-from-gvariant  range-gvariant-value)
	     ))
       string-desc))))

(defun settings-schema-get-sig-for-key (settings-schema key)
  (let* ((schema-key
	  (settings-schema-key-lookup settings-schema key
				      :if-does-not-exist :error))
	 (type1 (gir:invoke (schema-key "get_value_type"))) ;GVariantType
	 (sig1 (gir:invoke (type1 "dup_string"))))
    sig1))

;; returns a lisp object after autoconverting gvariant
(defun %settings-value (settings-obj key)
  (assert (settings-schema-has-key settings-obj key))
  (let* ((ret (gir:invoke (settings-obj "get_value") key))
	 (sig (gir:invoke (ret "get_type_string")))
	 ;;check-optional
	 (settings-schema (settings-schema-lookup settings-obj))
	 (sig1 (settings-schema-get-sig-for-key settings-schema key)))
    ;;check-optional
    (assert (equal sig sig1) nil "sig=~S sig1=~S" sig sig1)
    (gir:convert-from-gvariant ret sig)))

;; new-value is autoconverted to a gvariant according to the type
;; signature for key
(defsetf %settings-value (settings-obj key) (new-value)
  `(progn
     (let* ((settings-schema (settings-schema-lookup ,settings-obj :if-does-not-exist :error))
	    (schema-key (settings-schema-key-lookup settings-schema ,key :if-does-not-exist :error))
	    (sig (settings-schema-get-sig-for-key settings-schema ,key))
	    (gvariant-value (gir:convert-to-gvariant ,new-value sig)))
       (assert (gir:invoke (schema-key "range_check") gvariant-value))
       (gir:invoke (,settings-obj "set_value") ,key gvariant-value))
     ,new-value))

(defun settings-value (settings key &optional path)
  (%settings-value
   (settings-lookup settings path :if-does-not-exist :error)
   key))

(defsetf settings-value (settings key &optional path) (new-value)
  `(let ((settings-obj
	  (settings-lookup ,settings ,path :if-does-not-exist :error)))
     (setf (%settings-value settings-obj ,key) ,new-value)))

(defun settings-schemas-map (function)
  "Map FUNCTION over all non-relocatable settings-schemas. FUNCTION is
called with one argument - the settings-schema object"
  (loop for schema-name in (settings-list-schemas :relocatable nil :non-relocatable t :recursive t)
	do (funcall function
		    (settings-schema-lookup schema-name :recursive-p t :if-does-not-exist :error))))

(defun settings-schemas-map-keys (function)
  "Map FUNCTION over all non-relocatable settings-schemas. FUNCTION is
called with two arguments - the settings-schema object and the
settings-schema-key object"
  (settings-schemas-map
   (lambda (settings-schema)
     (loop for key-name in (cdr (settings-schema-list-keys settings-schema))
	   for schema-key = (settings-schema-key-lookup settings-schema key-name :if-does-not-exist :error)
	   do (funcall function settings-schema schema-key)))))

(defun gsettings-list-recursively ()
  (settings-schemas-map-keys
   (lambda (settings-schema schema-key)
     (let ((key (gir:invoke (schema-key "get_name"))))
       (format t "~A ~A ~A~&"
	       (gir:invoke (settings-schema "get_id"))
	       key
	       (settings-value settings-schema key))))))

#||
(in-package "GIR-TEST")
(setq $easy-src
      (settings-source-new-from-directory
       "/home/madhu/extern/EXT-GSE/EasyScreenCast/schemas/"))

(setq $easy-schema
      (with-settings-schema-source $easy-src
	(settings-schema-lookup "org.gnome.shell.extensions.EasyScreenCast")))

(setq $easy-path
      (settings-schema-non-relocatable-path $easy-schema))

(setq $easy (settings-lookup $easy-schema $easy-path))
(settings-value $easy "verbose-debug")

(setf (settings-value $easy "verbose-debug") nil)
(setf (settings-value $easy "verbose-debug") t)


(setq $media-keys-settings
      (settings-lookup "org.gnome.settings-daemon.plugins.media-keys"))
(settings-schema-non-relocatable-path $media-keys-settings)
(settings-schema-relocatable-p $media-keys-settings)
(invoke ($media-keys-settings "is_writable") "custom-keybindings")
(settings-schema-list-keys $media-keys-settings)
(invoke ($media-keys-settings "list_children"))
(property $media-keys-settings "path")
(settings-value $media-keys-settings "custom-keybindings")
(setq $cbschema
      (settings-schema-lookup "org.gnome.settings-daemon.plugins.media-keys.custom-keybinding"))

(settings-schema-relocatable-p $cbschema)
(setq $s (settings-lookup
	  "org.gnome.settings-daemon.plugins.media-keys.custom-keybinding"
	  (setq $p "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/")))

(property $s "path")
(settings-schema-non-relocatable-path $s)
(settings-schema-relocatable-p $s)
(settings-schema-list-keys $s)
(settings-value $s "command")
(settings-value $s "name")
(settings-value $s "binding")
(settings-value "org.gnome.settings-daemon.plugins.media-keys.custom-keybinding" "command" $p)
(gir-lib::gsettings-list-recursively)
(settings-schema-get-range-for-key "org.gnome.desktop.background" "picture-options")
(setf (settings-value
        "org.gnome.desktop.background" "picture-options")
      "wallpaper")

||#
