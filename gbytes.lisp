(in-package "GIR-LIB")

(export '(gbytes-to-string))

(defun gbytes-to-string (gbytes &key encoding)
  (cffi:with-foreign-object (size :int)
    (let ((ret (cffi:foreign-funcall "g_bytes_get_data" :pointer (gir:this-of gbytes)
				     :pointer size
				     :pointer)))
      (cffi:foreign-string-to-lisp ret :count (cffi:mem-ref size :int)
				   :encoding (or encoding
						 cffi:*default-foreign-encoding*)))))
