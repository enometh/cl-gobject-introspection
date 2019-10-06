(in-package "GIR")

(defun eval-1 (expr)
  (format t "EVAL-1: ===> ~S~&" expr)
  (with-simple-restart (skip "Skip this form")
    (eval expr)))

(defun intern-1 (string package)
  (let ((symbol-name (string-upcase (substitute #\- #\_ string))))
    (or (find-symbol symbol-name package)
	(progn (format t "INTERN-1: ~A -> ~A~&" string symbol-name)
	       (intern symbol-name  package)))))

(defun %build-validate-superclass-forms (class)
  (let ((sym (or  #+(or lispworks ecl clisp) 'clos:validate-superclass
		  #+(or allegro cmu) 'mop:validate-superclass
		  #+(or clozure) 'ccl:validate-superclass
		  #+(or mkcl) 'gir::validate-superclass ;BOGUS
		  (error "No validate-superclass"))))
  `(progn (defmethod ,sym ((class ,class) (super standard-class))
	    t)
	  (defmethod ,sym ((class standard-class) (super ,class))
	    t))))
