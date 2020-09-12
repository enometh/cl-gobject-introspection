(in-package "GIR-LIB")

(export '(g-debug g-message g-info g-warning g-critical g-error))
(export '(unix-errno make-g-file-error signal-gerror))
(export '(unix-open))

(defconstant +null-pointer+ (cffi:null-pointer))

(defmacro define-g-loggers ()
  `(progn
     ,@(loop for name in '(g-debug g-message g-info
			   g-warning g-critical g-error)
	     for log-level-name in '(:level-debug :level-message
				     :level-info :level-warning
				     :level-critical :level-error)
	     for log-level = (gir:nget *glib* "LogLevelFlags" log-level-name)
	     collect
	     `(defun ,name (fmt-control &rest fmt-args)
		(gir:invoke (*glib* "log_default_handler")
			nil ,log-level
			(apply #'format nil fmt-control fmt-args)
			+null-pointer+)))
     nil))

(define-g-loggers)


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun unix-errno ()
  (cffi:mem-ref (cffi:foreign-funcall "__errno_location" :pointer) :int))

(cffi:defcfun (unix-open "open") :int
  (filename :string)
  (flags :int)
  (mode :int))

(defvar +O_RDWR+  #o00000002)

(defun make-g-file-error (filename saved-errno &optional fmt-string &rest fmt-args)
  (gir:invoke (*glib* "Error" "new_literal")
	      (gir:invoke (*glib* "file_error_quark"))
	      (gir:invoke (*glib* "file_error_from_errno") saved-errno)
	      (apply #'format nil "~A: ~A~@[: ~@?~]"
		     (gir:invoke (*glib* "filename_display_name") filename)
		     (gir:invoke (*glib* "strerror") saved-errno)
		     fmt-string
		     fmt-args)))


(defun signal-gerror (gerror)
  (gir::with-gerror err (setf (cffi:mem-ref err :pointer) (this-of gerror))))

#+nil
(signal-gerror (make-g-file-error "/etc/passwd" 13 "~A" 42))

