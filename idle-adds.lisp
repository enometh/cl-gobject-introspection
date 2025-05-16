(in-package "GIR-LIB")

(eval-when (load eval compile)
(import (mapcar (lambda (x)
		  (let ((sym (find-symbol x "CFFI-CALLBACK-MANAGER")))
		    (assert sym)
		    sym))
		#1='("CALLBACK-MANAGER"
		  "REGISTER-CALLBACK"
		  "UNREGISTER-CALLBACK"
		  "FIND-CALLBACK"
		  "WITH-REGISTERED-CALLBACK"
		  "FUNCALL-OBJECT-CALLBACK"
		  "FREE-FUNCALL-OBJECT-CALLBACK"))
	"GIR-LIB")
(export (mapcar (lambda (x) (find-symbol x "GIR-LIB")) #1#) "GIR-LIB"))


;; provide an interface similar to 1) cl-gobject-introspection-wrapper
;; glib and 2) cl-cffi-gtk's stable pointer

(cffi:defcallback funcall-object-callback :bool ((user-data :pointer))
  (with-simple-restart (cont "Skip Error during Execution")
    (funcall (find-callback user-data))))

(cffi:defcallback free-funcall-object-callback :void ((user-data :pointer))
  (unregister-callback user-data))


(defun idle-add (function)
  (let ((loc (register-callback function)))
    (gir:invoke (*glib* "idle_add") 200
		(cffi:callback funcall-object-callback)
		loc
		(cffi:callback free-funcall-object-callback))))

#+nil
(idle-add (lambda () (warn "BOO")))

(defmacro block-idle-add (&body forms)
  `(let ((.main-loop. (gir:invoke (*glib* "MainLoop" "new") nil nil))
	 (.result.))
     (idle-add (lambda ()
		 (setq .result. (multiple-value-list (progn ,@forms)))
		 (gir:invoke (.main-loop. "quit"))))
     (gir:invoke (.main-loop. "run"))
     (values-list .result.)))

#+nil
(block-idle-add
  (sleep 2)
  (format t "depth=~D OK~%" (gir:invoke (*glib* "main_depth")))
  (values 1 2 3))

(export '(idle-add block-idle-add))



;;; handle patterns that use GAsynReadyCallback by using the user-data
;;; field to indicate a registered thunk. The thunk will be called
;;; with 2 parameters, the source object and the GAsyncResult (no
;;; third user-data parameter).

(cffi:defcallback funcall-object-async-ready-callback :void
    ((SOURCE-OBJECT :POINTER) (RES :POINTER) (USER-DATA :POINTER))
  "ARGS:  SOURCE-OBJECT Object. RES AsyncResult."
  (let* ((thunk (find-callback user-data))
	 (source (UNLESS (CFFI-SYS:NULL-POINTER-P SOURCE-OBJECT)
		   (GIR::GOBJECT (GIR:GTYPE SOURCE-OBJECT)
				 SOURCE-OBJECT)))
	 (async-result (UNLESS (CFFI-SYS:NULL-POINTER-P RES)
			 (GIR::GOBJECT (GIR:GTYPE RES)
				       RES))))
    (with-simple-restart (skip "Skip Error")
      (assert (functionp thunk))
      (funcall thunk source async-result))))

;; unused
#+nil
(cffi:defcallback free-funcall-object-async-ready-callback :void
    ((user-data :pointer))
  (unregister-callback user-data))



;;; example
#+nil
(defun get-file-contents (path)
  (let* ((file (gir:invoke (*gio* "File" "new_for_path") path))
	 (contents)
	 (main-loop (gir:invoke (*glib* "MainLoop" "new") nil nil)))
    (flet ((finish (source async-result)
	     (multiple-value-bind (ret contents-1 etag-out)
		 (gir:invoke (source "load_contents_finish") async-result)
	       (declare (ignorable ret etag-out))
	       (setq contents contents-1)
	       (gir:invoke (main-loop "quit")))))
      (with-registered-callback (loc) #'finish
	(gir:invoke (file "load_contents_async")
	  nil
	  (cffi:callback funcall-object-async-ready-callback)
	  loc)
	(gir:invoke (main-loop "run"))
	contents))))

#+nil
(get-file-contents "/etc/passwd")



;; ;madhu 250515 - hacky
(defun call-with-async-ready-callback
    (obj method-name &key
     args
     (finisher-method-name
      (concatenate 'string method-name "_finish"))
     (finisher-thunk (lambda (&rest args) (car args))))
  "method_name is the string name of the async function, typically ends
in \"_async\".  finisher-method-name is the name of the async ready
callback receiver. specify it it isn't \"method_name_async\".  Returns
the results of calling finisher-thunk on the results of calling the
finisher-method-name asynchronously.  This function is synchronous and
should be wrapped in a block-idle-add if it should run on the main
thread.

args should be a list. if non-NIL the args are spliced before the
cancellable parameter in the call to method-name.
"
  (let ((ret nil)
	(main-loop (gir:invoke (*glib* "MainLoop" "new") nil nil)))
    (flet ((finish (source async-result)
	     (setq ret
		   (multiple-value-call finisher-thunk
		     (gir:invoke (source finisher-method-name) async-result)))
	     (gir:invoke (main-loop "quit"))))
      (gir-lib::with-registered-callback (loc) #'finish
	(apply (gir:nget obj method-name)
	       (append args
		       (list nil
			     (cffi:callback funcall-object-async-ready-callback)
			     loc)))
	(gir:invoke (main-loop "run"))
	ret))))

(export 'call-with-async-ready-callback)

;; example
#+nil
(let* ((file (gir:invoke (*gio* "File" "new_for_path") "/etc/passwd")))
   (call-with-async-ready-callback
    file "load_contents_async"
    :finisher-method-name "load_contents_finish"
    :finisher-thunk
    #'(lambda (ret contents etags-1)
	(declare (ignore ret etags-1))
	(coerce
	 (map 'list 'code-char contents)
	 'string))))
