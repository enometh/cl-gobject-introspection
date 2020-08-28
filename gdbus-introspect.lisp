;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package "GDBUS")

;;;
;;; auto_names.py
;;;
;;;  If you start the bus name with "." (".NetworkManager"),
;;;  "org.freedesktop" will become automatically prepended. If you
;;;  specify a relative object path (without the leading "/"), the bus
;;;  name transformed to a path format will get prepended
;;;  ("/org/freedesktop/NetworkManager/"). If you don't specify the
;;;  object path at all, the transformed bus name will be used
;;;  automatically ("/org/freedesktop/NetworkManager").

;;;
(defun auto-bus-name (bus-name)
  (when (eql (char bus-name 0) #\.)	; default namespace
    (setq bus-name (concatenate 'string "org.freedesktop" bus-name)))
  (assert (invoke (*gio* "dbus_is_name") bus-name) nil
      "Invalid bus name")
  bus-name)

#+nil
(auto-bus-name ".IBus")

(defun auto-object-path (bus-name &optional object-path)
  (when (null object-path)
    (setq object-path (concatenate 'string "/"
				   (substitute #\/ #\. bus-name))))
  (when (not (eql (char object-path 0) #\/))
    (setq object-path (concatenate 'string "/"
				   (substitute #\/ #\. bus-name)  "/"
				   object-path)))
  object-path)

#+nil
(auto-object-path "org.freedesktop.DBus" "/foo")

(defun dbus-get-introspection-xml (bus bus-name &optional object-path &key (timeoutsec 15))
  "Return the introspection XML as a DBusNodeInfo D-Bus introspection
data structure"
  (setq bus-name (auto-bus-name bus-name))
  (setq object-path (auto-object-path bus-name object-path))
  (let ((ret-variant
	 (invoke (bus "call_sync")
		 bus-name object-path "org.freedesktop.DBus.Introspectable"
		 "Introspect"
		 nil
		 (invoke (*glib* "VariantType" "new") "(s)")
		 (nget *gio* "DBusCallFlags" :none)
		 (* timeoutsec 1000)
		 nil)))
    (car (convert-from-gvariant ret-variant))))

(defun dbus-node-info-from-xml (xml)
  (invoke (*gio* "DBusNodeInfo" "new_for_xml") xml))

(defun dbus-node-info-to-xml (dbus-node-info)
  (with-struct (string-builder (nget *glib* "String"))
    (invoke (dbus-node-info "generate_xml") 4 string-builder)
    (field string-builder "str")))

