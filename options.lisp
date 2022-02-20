(in-package "GIR-LIB")

(export '(option-context-new
	  option-entry-new option-entry-arg-data
	  defoptionvar
	  make-option-entry-rec
	  find-option-entry-rec
	  fetch-option-entry-rec-value
	  ))

(defun option-context-new (desc)
  (check-type desc string)
  (let (addr)
    (let ((ret
	   (gir::build-struct-ptr
	    (gir:nget *glib* "OptionContext")
	    (cffi:foreign-funcall "g_option_context_new" :string desc :pointer))))
      ;;  avoid exiting lisp when calling OptionContext.parse
      (gir:invoke (ret "set_help_enabled") nil)
      (setq addr (cffi:pointer-address (gir:this-of ret)))
      (tg:finalize ret
		   (lambda ()
		     (cffi:foreign-funcall "g_option_context_free"
					   :pointer (cffi:make-pointer addr)
					   :void)))
      ret)))


#+nil
(gir::%define-cffi-enum 'option-arg *glib* "OptionArg")

(CFFI:DEFCENUM OPTION-ARG
               (:NONE 0)
               (:STRING 1)
               (:INT 2)
               (:CALLBACK 3)
               (:FILENAME 4)
               (:STRING-ARRAY 5)
               (:FILENAME-ARRAY 6)
               (:DOUBLE 7)
               (:INT64 8))

(cffi:defcstruct option-entry
  (long-name :string)
  (short-name :char)
  (flags :int)
  (arg option-arg)
  (arg-data :pointer)
  (description :string)
  (arg-description :string))

#+nil
(= (gir::struct-info-get-size(gir::info-of (gir:nget *glib* "OptionEntry")))
   (cffi:foreign-type-size '(:struct option-entry)))

(defun option-entry-arg-data (option-entry)
  (etypecase option-entry
    (cffi:foreign-pointer (cffi:foreign-slot-value option-entry '(:struct option-entry) 'arg-data))
    (gir::struct-instance (option-entry-arg-data (this-of option-entry)))))

(defun set-option-entry-arg-data (option-entry new-pointer-value)
  (etypecase option-entry
    (cffi:foreign-pointer (setf (cffi:foreign-slot-value option-entry '(:struct option-entry) 'arg-data)
				new-pointer-value))
    (gir::struct-instance (set-option-entry-arg-data (this-of option-entry) new-pointer-value))))

(defsetf option-entry-arg-data (option-entry) (new-pointer-value)
  `(set-option-entry-arg-data ,option-entry ,new-pointer-value))

(defun option-entry-new (&key long-name short-name flags arg arg-data description arg-description)
  (let ((ret (gir::allocate-struct (gir:nget *glib* "OptionEntry"))))
    (setf (gir:field ret "long_name") (or long-name ""))
    (setf (gir:field ret "short_name")
	  (etypecase short-name
	    (null 0)
	    (character (char-code short-name))
	    (number short-name)))
    (setf (gir:field ret "flags") (or flags 0))
    (setf (gir:field ret "arg") (or arg (gir:nget *glib* "OptionArg" :none)))
    ;; cannot setf the pointer field for some reason
    ;; (setf (gir:field ret "arg_data") (or arg-data (cffi:null-pointer)))
    (set-option-entry-arg-data ret (or arg-data (cffi:null-pointer)))
    ;; an empty arg_description should be 0 which corresponds a lisp
    ;; string NIL. It cannot be "".  however again we cannot set the
    ;; field to NIL. so go through cffi again
    (if description
	(setf (gir:field ret "description") description)
	(setf (cffi:foreign-slot-value (gir::this-of ret) '(:struct option-entry) 'description)
	      (cffi:null-pointer)))
    (if arg-description
	(setf (gir:field ret "arg_description") arg-description)
	(setf (cffi:foreign-slot-value (gir::this-of ret) '(:struct option-entry) 'arg-description)
	      (cffi:null-pointer)))
    ret))


#||
(gir:list-fields-desc (gir::nget *glib* "OptionEntry"))
(setq $a (gir::allocate-struct (gir:nget *glib* "OptionEntry")))
;;fails (setf (gir::field $a "arg_data") (cffi:null-pointer))
(setf (cffi:foreign-slot-value  (gir::this-of $a) '(:struct option-entry) 'arg-description) "foo")
(gir:field $a "arg_description")
||#

(defmacro defoptionvar (name cffi-type &optional initial-value doc)
  "Define NAME via DEFVAR to point to foreign storage of type
CFFI-TYPE. Initial value is a lisp value of the appropriate type. DOC
if supplied is a string and is pretty useless. NAME is suitable to be
supplied as the arg-data parameter to OPTION-ENTRY-NEW.

Normally this would not be used. Instead make-option-entry-rec will
allocate foreign storage for the option entry.
"
  `(progn
     (defvar ,name (cffi:foreign-alloc ,cffi-type)
       ,@(and doc `(,doc)))
     ,@(and initial-value
	    `((setf (cffi:mem-ref ,name ,cffi-type) ,initial-value)))))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defun plist-sans-keys (plist &rest keys) ; <3247672165664225@naggum.no>
  (loop with sans for tail = (nth-value 2 (get-properties plist keys))
	unless tail return (nreconc sans plist) do
	(loop until (eq plist tail) do
	      (push (pop plist) sans)
	      (push (pop plist) sans))
	(setq plist (cddr plist))))


(defvar +option-arg-cffi-type-alist+
  '((:none . :boolean)
    (:string . :string)
    (:int . :int)
    (:filename . :string)
    (:string-array . :pointer)
    (:double . :double)
    (:callback . :pointer)
    (:filename-array . :pointer))
  "Map an (nget *glib* \"OptionArg\" type) argtype to a CFFI type. If
 arg type is G_OPTION_ARG_STRING or G_OPTION_ARG_FILENAME, the
 location will contain a newly allocated string if the option was
 given. That string needs to be freed by the callee using
 g_free(). Likewise if arg type is G_OPTION_ARG_STRING_ARRAY or
 G_OPTION_ARG_FILENAME_ARRAY, the data should be freed using
 g_strfreev().")

(defstruct (option-entry-rec (:constructor %make-option-entry-rec))
  arg-type				; a keyword
  option-entry
  storage-location			; we manage the alloction
  initial-value				; keep around for debugging
  )

(defun %make-option-entry-rec-finalizer (rec)
  (let (pointer-addr pointer-arg-type)
    (with-slots (arg-type storage-location) rec
      (when storage-location
	(assert (cffi:pointerp storage-location))
	(assert (not (cffi:null-pointer-p storage-location)))
	;; special case arg-type = :callback
	(unless (eq arg-type :callback)
	  (setq pointer-addr (cffi:pointer-address storage-location)))
	(setq pointer-arg-type arg-type)))
    (lambda ()
      (when pointer-addr
	(let ((storage-location (cffi:make-pointer pointer-addr)))
	  (case pointer-arg-type
	    ((:string :filename)
	     (gir:invoke (*glib* "free")
			 (cffi:mem-ref storage-location :pointer)))
	    ((:string-array :filename-array)
	     (gir:invoke (*glib* "strfreev")
			 (cffi:mem-ref storage-location :pointer))))
	  (cffi:foreign-free storage-location))))))

;; special case arg-type = :callback
(defun %make-option-entry-rec--assign-initial-value
    (arg-type cffi-type storage-location initial-value)
  (ecase arg-type
    (:none
     (check-type initial-value boolean)
     (assert (eq cffi-type :boolean))
     (setf (cffi:mem-ref storage-location :boolean) initial-value))
    (:int
     (assert (eq cffi-type :int))
     (setf (cffi:mem-ref storage-location cffi-type)
	   (if initial-value
	       (progn (check-type initial-value integer)
		      initial-value)
	       0)))
    (:double
     (assert (eq cffi-type :double))
     (setf (cffi:mem-ref storage-location cffi-type)
	   (if initial-value
	       (progn (check-type initial-value float)
		      initial-value)
	       0)))
    ((:string :filename)
     (assert (eq cffi-type :string))
     (setf (cffi:mem-ref storage-location :string)
	   (if initial-value
	       (progn (check-type initial-value string)
		      initial-value)
	       (cffi:null-pointer))))
    ((:string-array :filename-array)
     (assert (eq cffi-type :pointer))
     (setf (cffi:mem-ref storage-location :pointer)
	   (if initial-value
	       (progn (assert (cffi:pointerp initial-value))
		      (assert (cffi:null-pointer-p initial-value))
		      initial-value)
	       (cffi:null-pointer))))))



(defun make-option-entry-rec (&rest rest &key
			      long-name short-name flags arg-type description arg-description
			      storage-location-supplied
			      initial-value)
  "If arg-type is :callback initial-value should be a cffi pointer to
the GOptionArgFunc callback function. Otherwise a foreign storage
location is allocated and initialized for the option-entry arg-data
field.

If the parameter storage-location-supplied is given, no allocation is
made or managed but the pointer at the given location is used
blindly. It is not initialized with the initial-value. This argument
this can be used to share locations defined by defoptionvar.
"
  (declare (ignorable long-name short-name flags arg-type description arg-description))
  (check-type arg-type (member :none :int :double :string :filename :string-array :filename-array :callback))
  (let ((cffi-type (cdr (assoc arg-type +option-arg-cffi-type-alist+)))
	(arg (gir:nget *glib* "OptionArg" arg-type))
	(ret (%make-option-entry-rec :arg-type arg-type :initial-value initial-value)))
    (with-slots (option-entry storage-location) ret
      (cond ((eq arg-type :callback)
	     (assert (cffi:pointerp initial-value))
	     (assert (not (cffi:null-pointer-p initial-value)))
	     (setq storage-location initial-value))
	    (storage-location-supplied
	     (assert (cffi:pointerp storage-location-supplied))
	     (setq storage-location storage-location-supplied))
	    (t
	     (setq storage-location (cffi:foreign-alloc cffi-type))
	     (%make-option-entry-rec--assign-initial-value
	      arg-type cffi-type storage-location initial-value)))
      (setq option-entry
	    (apply #'option-entry-new
		   (append (list :arg arg)
			   (list :arg-data storage-location)
			   (plist-sans-keys rest
					    :arg-type
					    :initial-value
					    :storage-location-supplied
					    )))))
    (unless storage-location-supplied
      ;; finalizer assumes there is no mutation after construction.
      (tg:finalize ret (%make-option-entry-rec-finalizer ret)))
    ret))

(defun find-option-entry-rec (option-entry-recs long-name)
  (find long-name option-entry-recs
	:key #'(lambda (rec)
		 (gir:field (option-entry-rec-option-entry rec)
			    "long_name"))
	:test #'equal))

(defun fetch-option-entry-rec-value (option-entry-recs long-name)
  (let ((rec (find-option-entry-rec option-entry-recs long-name)))
    (assert rec nil
	"no entry with name ~S in the supplied list of option-entry-recs."
      long-name)
    (with-slots (arg-type storage-location) rec
      (ecase arg-type
	(:none (cffi:mem-ref storage-location :boolean))
	(:int (cffi:mem-ref storage-location :int))
	(:double (cffi:mem-ref storage-location :double))
	((:string :filename) (cffi:mem-ref storage-location :string))
	(:callback storage-location)
	((:string-array :filename-array)
	 (let ((n (gir:invoke (*glib* "strv_length") storage-location)))
	   (loop for i below n
		 collect (cffi:mem-aref storage-location :string i))))))))


#||
(defvar $option-entry-recs
  (list
   ;;{ "repeats", 'r', 0, OptionArg.INT, ref repeats, "Average over N repetitions", "N" }, ;
   (make-option-entry-rec :long-name "repeats"
			  :short-name #\r
			  :flags 0
			  :arg-type :int
			  :initial-value 2
			  :description "Average over N repetitions"
			  :arg-description "N")
   ;;{ "max-size", 'm', 0, OptionArg.INT, ref max_size, "Test up to 2^M items", "M" }, ;
   (make-option-entry-rec :long-name "max-size"
			  :short-name #\m
			  :flags 0
			  :arg-type :int
			  :initial-value 10
			  :description "Test up to 2^M Items"
			  :arg-description "M")
   ;;{ "verbose", 'v', 0, OptionArg.NONE, ref verbose, "Be verbose", null }, ;
   (make-option-entry-rec :long-name "verbose"
			  :short-name #\v
			  :flags 0
			  :arg-type :none
			  :initial-value nil
			  :description "Be verbose"
			  :arg-description nil)
   ;;{ "beep", 'b', 0, OptionArg.NONE, ref beep, "Beep when done", null }, ;
   (make-option-entry-rec :long-name "beep"
			  :short-name #\b
			  :flags 0
			  :arg-type :none
			  :description "Beep when done"
			  :arg-description nil)
   ;;{ "rand", 0, 0, OptionArg.NONE, ref randomize, "Randomize the data", null }, ;
   (make-option-entry-rec :long-name "rand"
			  :short-name 0
			  :flags 0
			  :arg-type :none
			  :description "Randomize the data"
			  :arg-description nil)
   ;;{ "mutter-plugin", 0, 0, OptionArg.STRING, ref plugin_name, "plugin", "PLUGIN" }, ;
   (make-option-entry-rec :long-name "mutter-plugin"
			  :short-name 0
			  :flags 0
			  :arg-type :string
			  :initial-value "libmdefault"
			  :description "plugin"
			  :arg-description "PLUGIN")))

(find-option-entry-rec $option-entry-recs "beep")
(fetch-option-entry-rec-value $option-entry-recs "mutter-plugin")

(defvar $option-ctx (option-context-new "TestOptions"))
(gir:invoke ($option-ctx "add_main_entries")
	    (mapcar #'option-entry-rec-option-entry
		    $option-entry-recs)
	    nil)

(gir:invoke ($option-ctx "get_help_enabled"))
(gir:invoke ($option-ctx "get_help") t nil)

(setq $g (gir:invoke ($option-ctx "get_main_group")))
(pprint (gir:invoke ($option-ctx "get_help") t $g))

(gir:invoke ($option-ctx "parse") '("argv0" "--verbose"  "--beep" "--mutter-plugin" "libbarf"))
(fetch-option-entry-rec-value $option-entry-recs "mutter-plugin")
(fetch-option-entry-rec-value $option-entry-recs "beep")

(gir:nget *glib* "OptionArgFunc")
(setq $beep-ent (find-option-entry-rec $option-entry-recs "beep"))
(cffi:mem-ref (option-entry-rec-storage-location $beep-ent) :boolean)
(gir:field (option-entry-rec-option-entry $beep-ent) "short_name")
(gir:field (option-entry-rec-option-entry
	    (find-option-entry-rec $option-entry-recs "rand"))
	   "short_name")

(gir:get-callable-desc *glib* "OptionArgFunc")
;; => #F<OptionArgFunc(#V<option_name: STRING> #V<value: STRING> #V<data: POINTER>): (#V<RETURN-VALUE: BOOLEAN>)>

(defun parse-no-beep (option-name value data)
  (warn "parse-no-beep: ~S" (list option-name value data))
  (let ((beep-ent (find-option-entry-rec $option-entry-recs "beep")))
    (when beep-ent
      (setf (cffi:mem-ref (option-entry-rec-storage-location beep-ent) :boolean)
	    nil)))
  t)

(eval (gir:generate-cffi-defcallback
       (gir::info-of (gir:get-callable-desc *glib* "OptionArgFunc"))
       'parse-no-beep))

(setq $nobeep-ent
        (make-option-entry-rec :long-name "no-beep"
			       :short-name 0
			       :flags (logior (gir:nget *glib* "OptionFlags" :no-arg))
			       :arg-type :callback
			       :initial-value (cffi:callback parse-no-beep-callback)
			       :description "Don't Beep when done"
			       :arg-description nil))

(gir:invoke ($option-ctx "add_main_entries")
	    (mapcar #'option-entry-rec-option-entry
		    (list $nobeep-ent))
	    nil)

(gir:invoke ($option-ctx "parse") '("argv0" "--verbose"  "--no-beep" "--mutter-plugin" "libbarf"))
(fetch-option-entry-rec-value $option-entry-recs "beep")

(defoptionvar $bleep-loc :boolean t)

(setq $bleep-ent
      (make-option-entry-rec :long-name "bleep"
			     :short-name 0
			     :flags (logior (gir:nget *glib* "OptionFlags" :none))
			     :storage-location-supplied $bleep-loc
			     :arg-type :none
			     :description "Bleep"
			     :arg-description nil))

(setq $no-bleep-ent
      (make-option-entry-rec :long-name "no-bleep"
			     :short-name 0
			     :flags (logior (gir:nget *glib* "OptionFlags" :none)
					    (gir:nget *glib* "OptionFlags" :reverse))
			     :storage-location-supplied $bleep-loc
			     :arg-type :none
			     :description "Bleep"
			     :arg-description nil))

(gir:invoke ($option-ctx "add_main_entries")
	    (mapcar #'option-entry-rec-option-entry
		    (list $bleep-ent $no-bleep-ent))
	    nil)

(gir:get-method-desc (gir:nget *glib* "OptionContext") "parse_strv")

(gir:invoke ($option-ctx "parse") '("argv0" "--bleep"))
(gir:invoke ($option-ctx "parse") '("argv0" "--no-bleep"))
(cffi:mem-ref $bleep-loc :boolean)

;; goodbye world. - if this works lisp exits.
(gir:invoke ($option-ctx "parse") '("argv0" "--help"))
||#