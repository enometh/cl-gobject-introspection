(in-package "GIR-LIB")

;; Use get-clipboard on a GtkWidget to get a GdkClipboard on Gtk4 and
;; a GtkClipboard on gtk3.

(export '(%get-clipboard
	  %clipboard-set
	  %clipboard-read-text-sync
	  %clipboard-empty-p))

(defun %get-clipboard (wid &optional (selection-type :clipboard))
  (ecase selection-type
    (:clipboard
     (gir:invoke (wid "get_clipboard")
       #+wk (gir:invoke (*gdk* "atom_intern") "CLIPBOARD" t)))
    (:primary
     #+wk
     (gir:invoke (wid "get_clipboard")
       (gir:invoke (*gdk* "atom_intern") "PRIMARY" t))
     #-wk
     (let ((display (gir:invoke (wid "get_display"))))
       (gir:invoke (display "get_primary_clipboard"))))))

(defun %clipboard-set (clipboard text)
  #+wk
  (gir:invoke (clipboard "set_text") text (length text))
  #-wk
  (gir:with-struct (var (gir:nget *gobject* "Value"))
    (gir:invoke (var "init") (gir::%gtype :string))
    (gir:invoke (var "set_string") text)
    (gir:invoke (clipboard "set") var)))

(CFFI:DEFCALLBACK HANDLE-CLIPBOARD-TEXT-RECEIVED-CALLBACK
    :VOID
    ((CLIPBOARD :POINTER) (TEXT :POINTER) (USER-DATA :POINTER))
  "ARGS:  CLIPBOARD Clipboard."
  (let ((thunk (find-callback user-data)))
    (with-simple-restart (skip-execution "Skip executing clipboard-text-received callback")
      (funcall thunk
	       (UNLESS (CFFI-SYS:NULL-POINTER-P CLIPBOARD)
		 (GIR::GOBJECT (gir::GTYPE CLIPBOARD) CLIPBOARD))
	       (cffi:foreign-string-to-lisp TEXT)))))

(defun %clipboard-read-text-sync (clipboard)
  (let ((main-loop (gir:invoke (*glib* "MainLoop" "new") nil nil))
	(text nil))
    (with-registered-callback (loc)
	#+wk #'(lambda (clipboard text1)
		 (declare (ignore clipboard))
		 (setq text text1)
		 (gir:invoke (main-loop "quit")))
	#-wk #'(lambda (source async-result)
		 (setq text (gir:invoke (source "read_text_finish") async-result))
		 (gir:invoke (main-loop "quit")))
	#+wk
	(gir:invoke (clipboard "request_text")
	  (cffi:callback HANDLE-CLIPBOARD-TEXT-RECEIVED-CALLBACK)
	  loc)
	#-wk
	(gir:invoke (clipboard "read_text_async")
	  (cffi:null-pointer)
	  (cffi:callback gir-lib::funcall-object-async-ready-callback)
	  loc)
	(gir:invoke (main-loop "run"))
	text)))

(defun %clipboard-empty-p (clipboard)
  (let (#-wk (formats (gir:invoke (clipboard "get_formats"))))
    (cond (#+wk
	   (gir:invoke (clipboard "wait_is_text_available"))
	   #-wk
	   (gir:invoke (formats "contain_gtype") (gir::%gtype :string))
	   nil)
	  (t t))))
