(in-package "GIR-LIB")

(defvar *soup* (gir:require-namespace "Soup" "3.0"))
(export '(*soup* map-soup-headers fetch-uri-as-string))

(cffi:defcallback soup-message-headers-map-callback :void
    ((name :string)
     (value :string)
     (user-data :pointer))
  (let ((thunk (cffi-callback-manager:find-callback user-data)))
    (with-simple-restart (skip-execution
                          "Skip Executing one soup_message_headers_foreach")
      (funcall thunk name value))))

(defun map-soup-message-headers-1 (soup-message-headers function)
  (cffi-callback-manager:with-registered-callback (loc) function
    (gir:invoke (soup-message-headers "foreach")
      (cffi-sys:%callback 'soup-message-headers-map-callback)
      loc)))

(defun map-soup-message-headers (result-type function soup-message-headers)
  (let* ((ret nil)
         (func (if result-type
		   #'(lambda (k v) (push (funcall function k v) ret))
		   function)))
    (map-soup-message-headers-1 soup-message-headers func)
    (if result-type (coerce (nreverse ret) result-type))))

(defun fetch-uri-as-string (uri &key (meth "GET") encoding)
  (let* ((m (gir:invoke (*soup* "Message" "new") meth uri))
	 (s (gir:invoke (*soup* "Session" "new")))
	 (b (gir:invoke (s "send_and_read") m nil)))
    (gbytes-to-string b :encoding encoding)))

