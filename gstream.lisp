(in-package "GIR-LIB")

(eval-when (load eval compile)
(export '(gio-input-stream gio-output-stream)))

(defclass gio-input-stream
    (trivial-gray-streams:fundamental-binary-input-stream
     trivial-gray-streams:fundamental-character-input-stream)
  ((gio-input-stream :initarg :gio-input-stream)))

(defmethod close ((stream gio-input-stream) &key abort)
  (declare (ignore abort))
  (with-slots (gio-input-stream) stream
    (gir:invoke (gio-input-stream "close") nil)))

(defmethod open-stream-p ((stream gio-input-stream))
  (with-slots (gio-input-stream) stream
    (not (gir:invoke (gio-input-stream "is_closed")))))

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream gio-input-stream) seq start end &key)
  (with-slots (gio-input-stream) stream
    (let* ((required (- end start))
	   (gbytes (gir:invoke (gio-input-stream "read_bytes")
			       required nil)))
      (loop (multiple-value-bind (data size)
		(gir:invoke (gbytes "unref_to_data"))
	      (replace seq data :start1 start :end1 end :end2 size)
	      (cond ((= size (- end start)) (return required))
		    ((< size (- end start))
		     (incf start size)
		     (setq gbytes (gir:invoke (gio-input-stream "read_bytes")
					      (- end start) nil)))
		    (t (error "read"))))))))

(defmethod stream-element-type ((stream gio-input-stream))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-char ((stream gio-input-stream))
  (let ((seq (make-array 1 :element-type '(unsigned-byte 8))));ugh!
    (trivial-gray-streams:stream-read-sequence stream seq 0 1)
    (code-char (aref seq 0))))

(defmethod trivial-gray-streams:stream-read-byte ((stream gio-input-stream))
  (let ((seq (make-array 1 :element-type '(unsigned-byte 8))));ugh!
    (trivial-gray-streams:stream-read-sequence stream seq 0 1)
    (aref seq 0)))



;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defclass gio-output-stream
    (trivial-gray-streams::fundamental-binary-output-stream
     trivial-gray-streams:fundamental-character-output-stream)
  ((gio-output-stream :initarg :gio-output-stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream gio-output-stream))
  nil)

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream gio-output-stream) seq start end &key)
  (with-slots (gio-output-stream) stream
    (let* ((gbytes0 (gir:invoke (*glib* "Bytes" "new") (subseq seq start end)))
	   (gbytes gbytes0))
      (loop (let ((size (gir:invoke (gio-output-stream "write_bytes")
				    gbytes nil)))
	      (cond ((= size (- end start))
		     (gir:invoke (gbytes0 "unref"))
		     (return t))
		    ((< size (- end start))
		     (setq gbytes (gir:invoke (gbytes "new_from_bytes")
					      size (- end size))))
		    (t (error "write"))))))))

(defmethod stream-element-type ((stream gio-output-stream))
  '(unsigned-byte 8))

(defmethod close ((stream gio-output-stream) &key abort)
  (declare (ignore abort))
  (with-slots (gio-output-stream) stream
    (gir:invoke (gio-output-stream "close") nil)))

(defmethod open-stream-p ((stream gio-output-stream))
  (with-slots (gio-output-stream) stream
    (not (gir:invoke (gio-output-stream "is_closed")))))

(defmethod trivial-gray-streams:stream-write-char
    ((stream gio-output-stream) char)
  (let ((seq (make-array 1 :element-type '(unsigned-byte 8)
			 :initial-element (char-code char))))
    (trivial-gray-streams:stream-write-sequence stream seq 0 1)))

(defmethod trivial-gray-streams:stream-write-byte
    ((stream gio-output-stream) byte)
  (let ((seq (make-array 1 :element-type '(unsigned-byte 8)
			 :initial-element byte)))
    (trivial-gray-streams:stream-write-sequence stream seq 0 1)))

;; lispworks uses stream-element-type to collect the output of
;; read-char so we have to supply our own method
(defmethod trivial-gray-streams:stream-read-line ((stream gio-input-stream))
  (let ((result (make-array 1 :element-type (if (subtypep
						 (stream-element-type stream)
						 'unsigned-byte)
						'character
						(stream-element-type stream))
			    :adjustable t
			    :fill-pointer 0)))
    (loop for byte = (trivial-gray-streams:stream-read-byte stream)
          while (and byte (/= byte 10))
          do (vector-push-extend (code-char byte) result)
          finally (return (values (subseq result 0) (not byte))))))