(in-package "GIR-LIB")
(export '(map-list list->strings list->objects))

(define-foreach-callback-mapper "g_list_foreach" "LIST")

(defun list->strings (list-ptr)
  (let (ret)
    (map-list-1 list-ptr (lambda (ptr)
			   (push (cffi:foreign-string-to-lisp ptr) ret)))
    (nreverse ret)))

(defun list->objects (list-ptr &optional object-class)
  (let (ret)
    (map-list-1 list-ptr
		(lambda (ptr)
		  (push (ptr->object ptr object-class) ret)))
    (nreverse ret)))