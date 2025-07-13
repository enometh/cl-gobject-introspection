;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sat Sep 12 22:10:16 2020 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(in-package "GIR-LIB")

(defvar *default-appname*
  (let ((ptr (cffi:foreign-symbol-pointer "stashed_appname"))) ;mkclplug
    (if ptr
	(cffi:mem-ref ptr :string)
	"default")))

(defun fifo-path (name)
  (concatenate 'string
	       (gir:invoke (*glib* "get_user_runtime_dir"))
	       "/"
	       (progn (assert (and (stringp *default-appname*)
				   (> (length *default-appname*) 0)))
		      *default-appname*)
	       (let ((suffix (gir:invoke (*glib* "getenv") "SUFFIX")))
		 (if suffix
		     (concatenate 'string "." suffix "/")
		     "/"))
	       name))

(defun make-fifo (fifo-path)
  (ensure-directories-exist fifo-path)
  (let ((f (gir:invoke (*gio* "file_new_for_path") fifo-path))
	(expected-perms #o600))
    (cond ((gir:invoke (f "query_exists") nil)
	   (assert (= (gir:invoke (f "query_file_type") 1 nil)
		      (gir:nget *gio* "FileType" :special))
	       nil
	       "~A exists and NOT A PIPE." fifo-path)
	   (let ((info (gir:invoke (f "query_info") "unix" 1 nil)) perms)
	     (with-simple-restart (cont "Cont")
	       (assert (= expected-perms (setq perms (logand #o777 (gir:invoke (info "get_attribute_uint32") "unix::mode"))))
		   nil "~A exists but perms are ~o, NOT ~o."
		 fifo-path
		 perms expected-perms))))
	  (t (let ((ret (cffi:foreign-funcall "mkfifo" :string fifo-path
					      :int expected-perms :int)))
	       (if (zerop ret)
		   (g-info "Created fifo at ~A" fifo-path)
		   (signal-gerror
		    (make-g-file-error fifo-path (unix-errno)))))))
    fifo-path))

;; crude lockless leak protection
(defvar *opened-fds* nil)

#+nil
(mapcar #'%close-fd *opened-fds*)

(defun %close-fd (fd)
  (prog1 (cffi:foreign-funcall "close" :int fd :int)
    (setq *opened-fds* (delete fd *opened-fds*))))

(defun %open-fifo (fifo-path mode)
  "open non-block"
  (let ((fd (cffi:foreign-funcall "open" :string fifo-path
				  :int  (ecase mode
					  ;;  +o_rdonly+ hangs
					  (:server +o_rdwr+)
					  (:client +o_wronly+)
					  (:probe (logior +o_wronly+
							  +o_nonblock+)))
				  :int)))
    (if (> fd 0)
	(pushnew fd *opened-fds*))
    fd))

(defun is-fifo-running (fifo-path)
  (let ((fd (%open-fifo fifo-path :probe)))
    (when (> fd 0)
      (%close-fd fd)
      t)))

#+nil
(is-fifo-running (fifo-path "main"))

(defmacro with-open-fifo ((fd-var fifo-path mode) &body body)
  (let ((fifo-path-var (gensym "FIFO-PATH-"))
	(mode-var (gensym "MODE-")))
    `(let* ((,fifo-path-var ,fifo-path)
	    (,mode-var ,mode)
	    (,fd-var (%open-fifo ,fifo-path-var ,mode-var)))
       (unwind-protect
	    (progn
	      (cond ((= ,fd-var -1)
		     (signal-gerror
		      (make-g-file-error
		       ,fifo-path-var (unix-errno)
		       "failed to open ~a as ~a"
		       ,fifo-path-var ,mode-var)))
		    (t  t))
	      ,@body)
	 (%close-fd ,fd-var)))))

(defun send-to-fifo (fifo-path string)
  (with-open-fifo (fd fifo-path :client)
    (let ((str (concatenate 'string
			    ;;string
			    (gir:invoke (*glib* "strescape") string "")
			    (string #\Newline))))
      (cffi:with-foreign-string ((buf sz) str)
	(assert (<= sz +PIPE-BUF+) nil "string too big")
	(let ((ret (cffi:foreign-funcall "write" :int fd
					 :pointer buf
					 :int sz
					 :int)))
	  (assert (= ret sz) nil "Failed to send"))))))

#||
;; cat $XDG_RUNTIME_DIR/default/main
(make-fifo (fifo-path "main"))
(send-to-fifo (fifo-path "main") "foo bar")
||#

;;; ----------------------------------------------------------------------
;;;
;;;
;;;
#+nil
(clrhash *fifo-listeners*)

(defvar *fifo-listeners* (make-hash-table :test #'eql)
  "Hash Table of IOContext -> fifo-listener")


(defstruct fifo-listener
  path handler-fn main-ctx main-loop io-channel fd event-id watch loc)

(cffi:defcallback ipcwatch-echo-callback :boolean
    ((source :pointer) (io-condition :int) (user-data :pointer))
  (g-message "ipcwatch-echo-callback")
  (assert
   (gir:invoke ((fifo-listener-main-ctx (gethash source *fifo-listeners*)) "is_owner")))

  (let* ((io-channel-ptr source)
	 (thunk (cffi-callback-manager:find-callback user-data))
	 (should-quit nil))
    (cond ((= io-condition
	      #.(gir:invoke (*glib* "IOCondition") :in))
	   (g-message "ipcwatch-echo-callback: calling read_line")
	   (cffi:with-foreign-pointer (line 1)
	     (let* ((status (cffi:foreign-funcall "g_io_channel_read_line"
			      :pointer io-channel-ptr
			      :pointer line
			      :pointer (cffi:null-pointer)
			      :pointer (cffi:null-pointer)
			      :pointer (cffi:null-pointer)
			      :int)))
	       	   (g-message "ipcwatch-echo-callback: read_line status=~S" status)
	       (cond ((= status 1)
		      (with-simple-restart
			  (skip-execution "Skip Executing in ipcwatch-echo-callback")
			(funcall thunk
				 (gir:invoke (*glib* "strcompress")
				   (cffi:mem-ref line :string)))))
		     (t (g-warning "ipcwatch read-line: invalid iostatus ~A."
				   status)
			(setq should-quit t))))))
	  (t (g-warning "ipcwatch-echo-callback: spurious io-condition ~A" io-condition)
	     (setq should-quit t)))
    (not should-quit)))

(defun clear-fifo-listener ($listener)
  (when (fifo-listener-handler-fn $listener)
    (setf (fifo-listener-handler-fn $listener) nil))
  (when (fifo-listener-main-ctx $listener)
    (gir:invoke ((fifo-listener-main-ctx $listener) "unref"))
    (setf (fifo-listener-main-ctx $listener) nil))
  (when (fifo-listener-main-loop $listener)
    (gir:invoke ((fifo-listener-main-loop $listener) "unref"))
    (setf (fifo-listener-main-loop $listener) nil))
  (when (fifo-listener-watch $listener)
    (unless (gir:invoke ((fifo-listener-watch $listener) "is_destroyed"))
      (gir:invoke ((fifo-listener-watch $listener) "destroy")))
    (setf (fifo-listener-watch $listener) nil))
  (when (fifo-listener-io-channel $listener)
    (remhash (gir:this-of (fifo-listener-io-channel $listener))
	     *fifo-listeners*)
    #+nil
    (gir:invoke ((fifo-listener-io-channel $listener) "shutdown") nil)
    (setf (fifo-listener-io-channel $listener) nil))
  (when (fifo-listener-main-ctx $listener)
    (gir:invoke ((fifo-listener-main-ctx $listener) "unref"))
    (setf (fifo-listener-main-ctx $listener) nil))
  (when (fifo-listener-loc $listener)
    (cffi-callback-manager:unregister-callback (fifo-listener-loc $listener))
    (setf (fifo-listener-loc $listener) nil))
  #+nil
  (when (fifo-listener-fd $listener)
    (%close-fd (fifo-listener-fd $listener))
    (setf (fifo-listener-fd $listener) nil))
  #+nil
  (when (fifo-listener-event-id $listener)
    (gir:invoke (*glib* "source_remove") (fifo-listener-event-id $listener))
    (setf (fifo-listener-event-id $listener) nil))
  (when (fifo-listener-path $listener)
    (setf (fifo-listener-path $listener) nil)))


#||
(setq $fifo-path (fifo-path "main"))
(setq $handler-fn 'ipcgencb)
||#

(defun run-fifo-listener ($listener $fifo-path $handler-fn)
  (setf (fifo-listener-path $listener) $fifo-path)
  (setf (fifo-listener-handler-fn $listener) $handler-fn)
  (setf (fifo-listener-main-ctx $listener)
	(gir:invoke (*glib* "MainContext" "new")))
  (setf (fifo-listener-main-loop $listener)
	(gir:invoke (*glib* "MainLoop" "new")
	  (fifo-listener-main-ctx $listener)
	  nil))
  (gir:invoke ((fifo-listener-main-ctx $listener) "push_thread_default"))
  (with-open-fifo (fd (fifo-listener-path $listener) :server)
    (setf (fifo-listener-fd $listener) fd)
    (with-registered-callback (loc) (fifo-listener-handler-fn $listener)
      (setf (fifo-listener-loc $listener) loc)
      (setf (fifo-listener-io-channel $listener)
	    (gir:invoke (*glib* "IOChannel" "unix_new") fd))
      (setf (gethash (gir:this-of (fifo-listener-io-channel $listener))
		     *fifo-listeners*)
	    $listener)
      (setf (fifo-listener-watch $listener)
	    (gir:invoke (*glib* "io_create_watch")
	      (fifo-listener-io-channel $listener)
	      #.(gir:invoke (*glib* "IOCondition") :in)))
      (gir:invoke ((fifo-listener-io-channel $listener) "unref"))
      (gir:invoke ((fifo-listener-watch $listener) "set_callback")
	(cffi:callback ipcwatch-echo-callback)
	loc
	(cffi:null-pointer))
      (setf (fifo-listener-event-id $listener)
	    (gir:invoke ((fifo-listener-watch $listener) "attach")
	      (fifo-listener-main-ctx $listener)))
      (gir:invoke ((fifo-listener-watch $listener) "unref"))
      (gir:invoke ((fifo-listener-main-loop $listener) "run"))))
  (gir:invoke ((fifo-listener-main-ctx $listener) "pop_thread_default")))


(defun ipcgencb (line)
  (g-message "handling line1: ~A" line))


#||
(make-fifo (fifo-path "main"))
(is-fifo-running (fifo-path "main"))

(setq $listener (make-fifo-listener))
(clear-fifo-listener $listener)

*fifo-listeners*
(run-fifo-listener $listener (fifo-path "main") 'ipcgencb)
(%close-fd 17)
(setq $thread (bt:make-thread
	       (lambda ()
		 (run-fifo-listener $listener (fifo-path "main") 'ipcgencb))
	       :name "fifo listener for main"))

(gir:invoke ((fifo-listener-main-loop $listener)
	     "quit"))
(ipcgencb "foo")

(gir:invoke ($io-channel "flush"))
(gir:invoke ((fifo-listener-watch $listener) "is_destroyed"))
(mapcar 'gir:this-of (list
 (gir:invoke ((fifo-listener-watch $listener) "get_context"))
 (fifo-listener-main-ctx  $listener)
  (gir:invoke (*glib* "main_context_default"))))
(gir:invoke ($io-channel "get_buffer_condition"))
(gir:list-methods-desc(gir:nget *glib* "MainContext"))
(setq $fifo-path (fifo-path "main"))
||#


#+nil
    (when should-quit
      (gir:invoke ((fifo-listener-main-loop
		    (gethash io-channel-ptr *fifo-listeners*))
		   "quit")))
