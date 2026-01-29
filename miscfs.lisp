(in-package "GIR-LIB")
(export '(ftrunc))

#||
;; bits/fcntl-linux.h
(defvar +O_CREAT+  #o00000100)
(defvar +O_EXCL+  #o00000200)
(defvar +O_NOCTTY+  #o00000400)
(defvar +O_TRUNC+  #o00001000)
;; open/fcntl
(defvar +O_APPEND+  #o00002000)
;;(defvar +O_NONBLOCK+ #o00004000)
(defvar +O_NONDELAY+ #o00004000)
(defvar +O_SYNC+ #o04010000)
(defvar +O_FSYNC+ #o04010000)
(defvar +O_ASYNC+ #o00020000)
(defvar +O_LARGEFILE+ #o00100000)
(defvar +O_DIRECTORY+ #o00200000)
(defvar +O_NOFOLLOW+ #o00400000)
(defvar +O_CLOEXEC+ #o02000000)
(defvar +O_DIRECT+ #o00040000)
(defvar +O_NOATIME+ #o01000000)
(defvar +O_PATH+ #o10000000)
(defvar +O_DSYNC+ #o00010000)
;;(defvar +PIPE-BUF+ 512)
;;(defvar +S_IFMT+ #o0170000)
||#

;; madhu 251002
(defun ftrunc (file &optional (len 0))
  (labels ((unix-errno ()
	     (cffi:mem-ref (cffi:foreign-funcall "__errno_location" :pointer)
			   :int))
	   (perror ()
	     (cffi:foreign-funcall "strerror" :int (unix-errno) :string)))
    (let (fd ret)
      (unwind-protect
	   (progn
	     (unless (plusp (setq fd (cffi:foreign-funcall "open" :string file :int (logior +O_WRONLY+ #+nil +O_CREAT+) :int)))
	       (error "ftrunc: error opening ~S: ~A" file (perror)))
	     (unless (zerop (setq ret (cffi:foreign-funcall "ftruncate" :int fd
							    :int len :int)))
	       (error "ftrunc: error truncating ~S: ~A" file (perror))))
	(when (and fd (plusp fd))
	  (let ((ret (cffi:foreign-funcall "close" :int fd :int)))
	    (unless (zerop ret)
	      (warn "ftrunc: ignoring error on close ~A ~A: ~A"
		    file fd (perror))))))
      ret)))

#+nil
(ftrunc "/tmp/fnas")
