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
		  (push (if object-class
			    (etypecase object-class
			      (gir::struct-class
			       (gir::build-struct-ptr object-class ptr))
			      (gir::object-class
			       (gir::build-object-ptr object-class ptr)))
			    ;; assume it is a gobject and crash to ldb
			    (gir::gobject
			     (cffi:mem-ref (cffi:mem-ref ptr :pointer) :ulong)
			     ptr))
			ret)))
    (nreverse ret)))