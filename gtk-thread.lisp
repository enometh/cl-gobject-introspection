(in-package "GIR-LIB")

(ensure-ctf :no-gtk :wk)

;; GTK+, however, is not thread safe. You should only use GTK+ and GDK
;; from the thread gtk_init() and gtk_main() were called on. This is
;; usually referred to as the “main thread”.
;;
;; Signals on GTK+ and GDK types, as well as non-signal callbacks, are
;; emitted in the main thread.

;; dont even create a mainloop. just call the default main context
;; iteration in its own thread.


(defun make-lock (name)
  (#+(or mkcl ecl sbcl) bordeaux-threads:make-recursive-lock
     #-(or mkcl ecl sbcl)
     bordeaux-threads:make-lock
     name))

(defmacro with-lock-held ((place) &body body)
  `(#+(or mkcl ecl sbcl)
      bordeaux-threads:with-recursive-lock-held
      #-(or mkcl ecl sbcl)
      bordeaux-threads:with-lock-held
      (,place)
      ,@body))

(defstruct (gtk-main (:constructor %make-gtk-main))
  (thread nil)
  (queue (make-array 0 :adjustable t :fill-pointer t)) ;task-queue
  (source-id nil)     ;glib main-loop source to process the task-queue
  (lock (make-lock "gtk-task-queue-lock"))
  (free-list nil))		  ; indices of removed thunks in queue

(defvar *gtk-main* (%make-gtk-main))

(defun unix-errno ()
  (cffi:mem-ref (cffi:foreign-funcall "__errno_location" :pointer) :int))

(cffi:defcfun strerror :string (errno :int))

(defvar *perror-signals-cerror* nil)

(defun perror (format-string &rest format-args)
  (let* ((errno (unix-errno))
	 (str (format nil "~?: ~D: ~A~&" format-string format-args errno
		      (strerror errno))))
    (if *perror-signals-cerror*
	(cerror "Continue" str)
	(write-string str *error-output*))))

(defvar +NR_gettid+ (or #+(and linux x86-64) 186 ;asm/unistd_64.h
			#+(and linux x86) 224	 ;asm/unistd_32.h
			))

(defun gettid ()
  (let ((tid (cffi:foreign-funcall "syscall" :long +NR_gettid+ :long)))
    (if (< tid 0)
	(progn (perror "gettid failed") nil)
	tid)))

(cffi:defcfun ("getpid" getpid) :int)

(defvar $tv (load-time-value (gir::allocate-struct (gir:nget *glib* "TimeVal"))))

(defun format-log (stream format-string &rest format-args)
  (gir:invoke (*glib* "get_current_time") $tv)
  (format stream "~A [~6D][~6D]]: ~?"
	  (gir:invoke ($tv "to_iso8601"))
	  (getpid) (gettid) format-string format-args))

#+nil
(format-log *error-output* "~A" "foo")

(defvar *serial* 0)

(cffi:defcallback process-gtk-main-task-queue :boolean ((user-data :pointer))
  (with-slots (queue lock free-list) *gtk-main*
    (let (index thunk)
      (with-lock-held (lock)
	 (setq index (cffi:mem-ref user-data :int))
	 (setq thunk (elt queue index))
	 (push index free-list)		; delete it
	 (setf (elt queue index) nil))
      (with-simple-restart (skip-execution "Skip Executing this function")
	(format-log t "START-EXECUTION THUNK ~A.~&" *serial*)
	(funcall thunk)
	(format-log t "FINISHED-EXECUTION THUNK ~A.~&" *serial*)
	(incf *serial*))))
  (cffi:foreign-free user-data)
  nil)

(eval-when (load eval compile)
(cffi:define-foreign-library (libX11)
  (:unix "libX11.so"))
(cffi:load-foreign-library 'libX11))

(eval-when (load eval compile)
(defun x11-init-threads ()
  (cffi:load-foreign-library 'libX11)
  (cffi:foreign-funcall "XInitThreads" :int)))

(defvar *gtk-main-kill-switch* nil)

#-no-gtk
(when (or (= (gir:invoke (*gtk* "get_major_version")) 4)
	  (and (= (gir:invoke (*gtk* "get_major_version")) 3)
	       (> (gir:invoke (*gtk* "get_minor_version")) 90)))
  (pushnew :gtk4 *features*))

(defun featurep (x)
  (when (or (keywordp x)
	    (setq x (find-symbol (symbol-name x) :keyword)))
    (find x *features*)))

#-no-gtk
(defun init-gtk ()
  (x11-init-threads)
  (if (featurep :gtk4)
      (gir:invoke (*gtk* "init"))
      (gir:invoke (*gtk* "init") nil)))

(defun run-gtk-main ()
  "Enter the GTK main loop."
  (with-simple-restart (cont "CONT")
    (assert (zerop   #-no-gtk
		     (if (featurep :gtk4)
			 (gir:invoke (*glib* "main_depth"))
			 (gir:invoke (*gtk* "main_level")))
		     #+no-gtk
		     (gir:invoke (*glib* "main_depth")))))
  #-no-gtk
  (init-gtk)
  ;; todo avoid call to maincontext
  #+mkcl
  (progn (si::disable-fpe t)
	 (format t "MKCL: all enabled FPE: ~S~&"  (si::all-enabled-fpe)))
  #+ecl
  (progn (ext:trap-fpe t nil)
	 (format t "ECL: all enabled FPE: ~S~&"  (ext:trap-fpe 'last nil)))
  (prog ((default-context (gir:invoke (*glib* "main_context_default"))))
   loop
     (cond (*gtk-main-kill-switch* (return))
	   (t (unwind-protect
		   (with-simple-restart (recover "Ignore Error in Main Context Iteration")
		     (gir:invoke (default-context "iteration") t))
		(go loop)))))
  (format t "Impossible! Leaving eternal damnation~&"))

#+nil
(gir:invoke ((gir:invoke (*glib* "main_context_default")) "iteration") nil)


(defun run-one-main-context-iteration ( &optional block)
  (prog ((default-context (gir:invoke (*glib* "main_context_default"))))
     (cond (*gtk-main-kill-switch* (return))
	   (t (unwind-protect
		   (with-simple-restart (recover "Ignore Error in Main Context Iteration")
		     (gir:invoke (default-context "iteration") block)))))))

#+nil
(run-one-main-context-iteration t)

(defun start-gtk-thread ()
  #-no-gtk
  (unless (find :bordeaux-threads *features*)
    (init-gtk))
  (when (find :bordeaux-threads *features*)
    (with-slots (thread queue free-list) *gtk-main*
      (cond ((and thread (bordeaux-threads:thread-alive-p thread))
	       (format t "start-gtk-thread: already running~&"))
	      (t (let ((nthunks (- (length queue) (length free-list))))
		   (when (> nthunks 0)
		     (format t "start-gtk-thread: blowing off ~D thunks~&"
			     nthunks)
		     (fill queue nil)
		     (setf (fill-pointer queue) 0)
		     (setq free-list nil)))
		 (setq thread
		       (bordeaux-threads:make-thread
			#'run-gtk-main
			:name "GTK-Main-Thread")))))))

#+nil
(with-slots (lock thread) *gtk-main*
  (with-lock-held (lock)
  (bordeaux-threads:destroy-thread thread)))

;; for single-threaded lisps: (and clisp (not mt))
(defvar *gtk-main-single-threaded-kill-switch* nil)

(defun clisp-single-thread-check-runnable ()
  (when *gtk-main-single-threaded-kill-switch*
    (error "refusing to run when kill-switch is active")))

(defun clisp-single-thread-register-destroy-handler (window)
  (gir:connect window
	       #+wk "destroy"
	       #-wk "unrealize"
	       #'(lambda (window)
		   (warn "clisp-single-thread: set destroy-callback for ~S" window)
		   (setq *gtk-main-single-threaded-kill-switch* t))))

(defun clisp-single-thread-run-loop (window)
  (let ((default-context (gir:invoke (*glib* "main_context_default"))))
    (loop (cond (*gtk-main-kill-switch* (return))
		(t (unwind-protect
		   (with-simple-restart (recover "Ignore Error in Main Context Iteration")
		     (gir:invoke (default-context "iteration") t))))))
    (format t "clisp-single-thread: loop exited on window ~S ~&" window)
    (setq *gtk-main-single-threaded-kill-switch* nil)))

(defun run-clisp-window (win)
  (clisp-single-thread-check-runnable)
  (clisp-single-thread-register-destroy-handler win)
  #-wk
  (progn
    (gir:invoke (win "unminimize"))
    (gir:invoke (win "present_with_time")
		(- (get-universal-time) cl-user:+unix-epoch+)))
  #+wk
  (gir:invoke (win "show_all"))
  (clisp-single-thread-run-loop win))

#+nil
(loop while  (gir:invoke (*gtk* "events_pending"))
      count 1
      do (gir:invoke (*gtk* "main_iteration")))

;; execute thunk in the default main context
(defun gtk-enqueue (thunk)
  (check-type thunk function)
  (with-simple-restart (recover "Recover from GTK-ENQUEUE")
  (if (find :bordeaux-threads *features*)
      (with-slots (lock queue free-list) *gtk-main*
	(with-lock-held (lock)
	  (let ((index (pop free-list)))
	    (if index
		(setf (elt queue index) thunk)
		(progn
		  (setq index (length queue))
		  (assert (= index (vector-push-extend thunk queue)))))
	    (let ((loc (cffi:foreign-alloc :int :initial-element index))
		  (default-context
		   (gir:invoke (*glib* "main_context_default"))))
	      (gir:invoke (default-context "invoke_full")
			  0		;priority
			  (cffi:callback process-gtk-main-task-queue)
			  loc		      ;user-data
			  (cffi:null-pointer) ;notifier
			  )))))
      (funcall thunk))))

(defun apply-in-gtk-thread (function &rest args)
  (gtk-enqueue (lambda () (apply function args))))

(defmacro with-gtk-thread (&body body)
  `(gtk-enqueue #'(lambda () ,@body)))

;;; MAIN
#+nil
(start-gtk-thread)

#+nil
(progn (gtk-enqueue (lambda () (format t "OK1~%")))
       (gtk-enqueue (lambda () (format t "OK2~%")))
       (gtk-enqueue (lambda () (format t "OK3~%"))))

#+nil
(with-gtk-thread			;bogus
  (gir:invoke (*gtk* "main_quit")))

#+nil
(with-gtk-thread
  (format t "main depth=~D~&" (gir:invoke (*glib* "main_depth"))))

#+nil
(format t "main level=~D~&" (gir:invoke (*gtk* "main_level")))

#+nil
(bordeaux-threads:thread-alive-p (gtk-main-thread *gtk-main*))

#+nil
(gtk-main-queue *gtk-main*)

#+nil
(setq *gtk-main-kill-switch* t)

#+nil
(setq *gtk-main-kill-switch* nil)

#+nil
(gtk-main-lock *gtk-main*)

(defmacro with-gtk-thread-sync (&body body)
  "Try to Evaluate body in the default main-context and return the
results."
  `(let ((.main-loop. (gir:invoke (*glib* "MainLoop" "new") nil nil))
	 (.result.))
     (gtk-enqueue (lambda ()
		    (setq .result. (multiple-value-list (progn ,@body)))
		    (gir:invoke (.main-loop. "quit"))))
     (gir:invoke (.main-loop. "run"))
     (values-list .result.)))

#+nil
(with-gtk-thread-sync (sleep 2) (format t "depth=~D OK~%" (gir:invoke  (*glib* "main_depth"))) (values 1 2 3))
