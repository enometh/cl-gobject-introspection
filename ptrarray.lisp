(in-package "GIR-LIB")
(export '(ptr-array-length ptr-array-index ptr-array->strings ptr-array->objects))

(cffi:defcstruct gptrarray
  (pdata :pointer)
  (len :uint))


(defun ptr-array-length (gptrarray-ptr)
  (cffi:with-foreign-slots ((len) gptrarray-ptr (:struct gptrarray))
    len))

;; #define g_ptr_array_index(array,index_) ((array)->pdata)[index_]
(defun ptr-array-index (gptrarray-ptr index)
  (cffi:with-foreign-slots ((len pdata) gptrarray-ptr (:struct gptrarray))
    (assert (>= index 0))
    (assert (< index len))
    (cffi:mem-ref pdata :pointer index)))

(defun map-ptr-array-1 (gptrarray-ptr function)
  (cffi:with-foreign-slots ((len pdata) gptrarray-ptr (:struct gptrarray))
    (loop for i below len
	  do (funcall function (cffi:mem-ref pdata :pointer i)))))

(defun ptr-array->strings (gptrarray-ptr)
  (let (ret)
    (map-ptr-array-1 gptrarray-ptr (lambda (ptr)
				     (push (cffi:foreign-string-to-lisp ptr)
					   ret)))
    (nreverse ret)))

(defun ptr-array->objects (gptrarray-ptr &optional object-class)
  (let (ret)
    (map-ptr-array-1 gptrarray-ptr
		     (lambda (ptr)
		       (push (ptr->object ptr object-class) ret)))
    (nreverse ret)))
