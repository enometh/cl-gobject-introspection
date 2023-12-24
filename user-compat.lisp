(in-package "CL-USER")
;; functions from cmucl-init.lisp which may not be loaded.

(eval-when (load eval compile)
(unless (boundp '+unix-epoch+)
  (defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0)))
(export '(+unix-epoch+)))

(eval-when (load eval compile)
(unless (fboundp 'prefixp)
(defun prefixp (prefix sequence &key (test #'equalp) (start1 0) (start2 0) end1 end2 &aux idx)
  "Begins with."
  (or (null (setq idx (mismatch prefix sequence :test test
				:start1 start1 :start2 start2 :end1 end1 :end2 end2)))
      (>= idx (length prefix))))
(export '(prefixp))))
