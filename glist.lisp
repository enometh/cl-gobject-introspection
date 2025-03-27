(in-package "GIR-LIB")
(export '(map-list list->strings list->objects
	  define-foreach-callback-mapper))

;;Define XXX-MAP-CALLBACK MAP-XXX-1 and (MAP-XXX result-type function
;;cffi-ptr)
(defmacro define-foreach-callback-mapper (c-func-name c-type-name)
  (let ((cffi-callback-name (intern (concatenate 'string (string-upcase c-type-name) "-MAP-CALLBACK")))
	(ptr-var-name (intern (concatenate 'string (string-upcase c-type-name) "-PTR")))
	(helper-function-name (intern (concatenate 'string "MAP-" (string-upcase c-type-name) "-1")))
	(map-function-name  (intern (concatenate 'string "MAP-" (string-upcase c-type-name)))))
    `(progn
       (cffi:defcallback ,CFFI-CALLBACK-NAME :void
	   ((data :pointer) (user-data :pointer))
	 (let ((thunk (find-callback user-data)))
	   (with-simple-restart (skip-execution ,(format nil "Skip Executing one ~A" c-func-name))
	     (funcall thunk data))))

       (defun ,HELPER-FUNCTION-NAME (,PTR-VAR-NAME function)
	 (with-registered-callback (loc) function
	   (cffi:foreign-funcall
	    ,c-func-name
	    :pointer ,PTR-VAR-NAME
	    :pointer (CFFI-SYS:%CALLBACK ',CFFI-CALLBACK-NAME)
	    :pointer loc)))

       (defun ,MAP-FUNCTION-NAME (result-type function ,PTR-VAR-NAME)
	 (let* ((ret nil)
		(func (if result-type
			  #'(lambda (x) (push (funcall function x) ret))
			  function)))
	   (,HELPER-FUNCTION-NAME ,PTR-VAR-NAME func)
	   (if result-type (coerce (nreverse ret) result-type)))))))

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