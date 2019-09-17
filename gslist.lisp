(in-package "GIR-LIB")

(cffi:defcallback slist-map-callback :void
    ((data :pointer) (user-data :pointer))
  (let ((thunk (find-callback user-data)))
    (with-simple-restart (skip-execution "Skip Executing slist-map-callback")
      (funcall thunk data))))

(defun map-slist-1 (slist-ptr function)
  (with-registered-callback (loc) function
    (cffi:foreign-funcall
     "g_slist_foreach"
     :pointer slist-ptr
     :pointer (cffi:callback slist-map-callback)
     :pointer loc)))

(defun map-slist (result-type function slist-ptr)
  (let* ((ret nil)
	 (func (if result-type
		   #'(lambda (x) (push (funcall function x) ret))
		   function)))
    (map-slist-1 slist-ptr func)
    (if result-type (coerce (nreverse ret) result-type))))

(defun prepend-to-slist (slist-var data-ptr)
  (setf (symbol-value slist-var)
	(cffi:foreign-funcall "g_slist_prepend"
			      :pointer (symbol-value slist-var)
			      :pointer data-ptr
			      :pointer)))

(defun slist-free (slist-ptr)
  (cffi:foreign-funcall "g_slist_free" :pointer slist-ptr :void))

(defun slist-free-full (slist-ptr &optional
			(g-destroy-func-ptr
			 (cffi:foreign-symbol-pointer "g_free")))
  (cffi:foreign-funcall "g_slist_free_full"
			:pointer slist-ptr
			:pointer g-destroy-func-ptr
			:void))

(defun slist->strings (slist-ptr)
  (let (ret)
    (map-slist-1 slist-ptr (lambda (ptr)
			     (push (cffi:foreign-string-to-lisp ptr) ret)))
    (nreverse ret)))

(defun slist-length (slist-ptr)
  (cffi:foreign-funcall "g_slist_length" :pointer slist-ptr :int))

#+nil
(defvar $slist (cffi:null-pointer))

#+nil
(progn
  (prepend-to-slist '$slist (cffi:foreign-string-alloc "last"))
  (prepend-to-slist '$slist (cffi:foreign-string-alloc "first")))

#+nil
(slist->strings $slist)

#+nil
(slist-free-full $slist)

#+nil
(map-slist 'list #'cffi:foreign-string-to-lisp  $slist)

#||
(cffi:foreign-string-to-lisp
 (cffi:foreign-funcall "g_slist_nth_data" :pointer $slist
		       :int 3
		       :pointer))
cffi::*default-foreign-encoding*
(setq $s (cffi:foreign-string-alloc "last"))
(cffi:foreign-string-to-lisp $s)
(cffi:defcstruct gslist
  (data :pointer)
  (next :pointer))

||#
