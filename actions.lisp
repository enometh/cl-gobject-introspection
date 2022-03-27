(in-package "GIR-LIB")

(export '(generate-action-activate-defcallback
	  action-group-interpret-cmdline))

(defun generate-action-activate-defcallback
    (lisp-function-name gvariant-parameter-type-string
     &optional(cffi-callback-name
	       (intern (concatenate 'string (symbol-name lisp-function-name)
				    "-CALLBACK")
		       *package*)))
  "Define a CFFI defcallack which calls LISP-FUNCTION-NAME with a
primary SimpleAction object and parameters translated according to
GVARIANT-PARAMETER-TYPE-STRING which should be NIL if the action
doesn't accept any parameters."
  (check-type lisp-function-name symbol)
  (check-type gvariant-parameter-type-string string)
  `(cffi:defcallback ,cffi-callback-name :void
       ((action :pointer)
	(param :pointer))
     (when (fboundp ',lisp-function-name)
       (apply ',lisp-function-name
	      (gir::build-object-ptr (gir:nget *gio* "SimpleAction") action)
	      ,@(when gvariant-parameter-type-string
		  `((list
		     (gir:convert-from-gvariant
		      (gir::build-struct-ptr (gir:nget *glib* "Variant") param)
		      ,gvariant-parameter-type-string))))))))

(defun action-group-interpret-cmdline (action-group line)
  "LINE is of the form \"ACTION-NAME [ARGS]\" where ARGS is the standard
gvariant string representation of the parameters according to the
parameter type of ACTION-NAME. If the action is found, it is activated
with args."
  (let ((whitespace #(#\Space #\Tab)))
    (let* ((pos (position-if (lambda (c) (find c whitespace)) line))
	   (name (string-trim whitespace (subseq line 0 pos)))
	   (act (gir:invoke (action-group "lookup_action") name))
	   (args (and act pos (string-trim whitespace (subseq line (1+ pos)))))
	   (param (and args (gir:invoke
			     (*glib* "variant_parse")
			     (gir:invoke (act "get_parameter_type"))
			     args nil nil))))
      (cond (act (gir:invoke (action-group "activate_action") name param))
	    (t(warn "action ~A not found in ~A" name action-group))))))


#||
(defun hello-cb (action seq)
  (format t "hello-cb ~S: ~{=~A~=^ ~}." action (coerce seq 'list)))
(eval (generate-action-activate-defcallback 'hello-cb "as"))
(setq $m1 (gir:invoke (*gio* "SimpleActionGroup" "new")))
(setq $h1 (gir:invoke (*gio* "SimpleAction" "new")
		      "hello"
		      (gir:invoke (*glib* "VariantType" "new") "as")))
(gir:connect $h1 "activate" (cffi:callback hello-cb-callback))
(gir:invoke ($m1 "add_action") $h1)
(action-group-interpret-cmdline $m1 "hello ['foo','bar','car']")
||#
