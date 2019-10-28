(in-package "GIR-LIB")
(export '(map-list list->string list->objects))

(define-foreach-callback-mapper "g_list_foreach" "LIST")

(defun list->strings (list-ptr)
  (let (ret)
    (map-list-1 list-ptr (lambda (ptr)
			   (push (cffi:foreign-string-to-lisp ptr) ret)))
    (nreverse ret)))

(defun list->objects (list-ptr object-class)
  (let (ret)
    (map-list-1 list-ptr (lambda (ptr)
			   (push (gir::build-object-ptr object-class ptr) ret)))
    (nreverse ret)))