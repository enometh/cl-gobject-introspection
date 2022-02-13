(in-package "GIR-LIB")

(export '(log-with-timings call-g-message-with-timings))

(defun call-g-message-with-timings (fmt-control fmt-args thunk)
  (let ((start (gir:invoke (*glib* "get_monotonic_time"))))
    (g-message "~? ..."  fmt-control fmt-args)
    (funcall thunk)
    (let ((stop (gir:invoke (*glib* "get_monotonic_time"))))
      (g-message "... Done! in ~As."(/ (- stop start) 1e6)))))

;; FIXME
(defmacro log-with-timings ((fmt-control &rest fmt-args) &body body)
  `(let ((thunk (lambda () ,@body)))
     (call-g-message-with-timings ,fmt-control ,fmt-args thunk)))

#+nil
(log-with-timings ("Sleeping  sec") (sleep 2))