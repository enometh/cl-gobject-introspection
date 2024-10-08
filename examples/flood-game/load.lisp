(in-package :cl-user)
#+nil
(ql:quickload '("cffi"
                "cffi-objects"
                "cl-gobject-introspection"
                ;; Accessible Cairo API through gobject introspection
                ;; is quite poor
                "cl-cairo2"))
(require 'cl-cairo2)
(require 'cffi-objects)
(let ((*default-pathname-defaults*
        (merge-pathnames #P"examples/flood-game/"
			 #+(and (not asdf) mk-defsystem)
			 (make::system-relative-pathname :cl-gir "")
			 #+asdf
                         (asdf:system-source-directory "cl-gobject-introspection"))))
  (load #P"./src/package.lisp")
  (load #P"./src/gui.lisp")
  (load #P"./src/flood-game.lisp"))

(in-package :flood-game-example)

(defparameter *image*
  (copy-image #2A((0 3 0 2 0 3 1 3 2 0 3 0)
                  (2 3 0 2 3 1 3 0 2 0 2 1)
                  (0 2 3 3 2 1 2 3 3 0 2 0)
                  (2 1 1 0 1 1 3 2 1 2 0 0)
                  (1 2 2 1 2 2 3 0 1 0 0 1)
                  (0 0 2 2 3 1 2 2 3 3 3 1)
                  (2 0 1 3 0 2 0 1 0 3 1 3)
                  (3 1 2 3 1 3 2 0 2 3 3 1)
                  (2 0 3 2 0 1 2 0 3 3 0 2)
                  (2 2 1 0 2 2 3 0 1 0 2 3)
                  (3 3 2 1 0 0 0 2 2 0 1 1)
                  (1 1 2 3 3 2 2 1 0 3 1 2))))


#+nil
(visualize-solution-in-gtk-window
 '(3 2 1 2 0 1 3 2 3 0 2 1 0 3 2))

(require 'bordeaux-threads)
(defvar $t1
  (bordeaux-threads:make-thread
   (lambda ()
     (gtk-window-with-cairo-painting 'render-image-in-cairo-context))
   :name "Flood Game GUI"))

(defvar $t2
  (bordeaux-threads:make-thread
   (lambda ()
     (visualize-solution-in-gtk-window
      '(3 2 1 2 0 1 3 2 3 0 2 1 0 3 2)))
   :name "Flood Visualize-Solution"))


#+nil
(gtk-window-with-cairo-painting #'render-image-in-cairo-context)


;; (gtk-window-with-cairo-painting 'render-image-in-cairo-context)
;; or without blocking REPL with slime-eval-defun or:
;; (bordeaux-threads:make-thread
;;  (lambda ()
;;    (gtk-window-with-cairo-painting 'render-image-in-cairo-context))
;;  :name "Flood Game GUI")
;;
;; timeout which connected to GTK thread works and GUI is redrawing
;; for each 0.3 sec. So you can patch *image* or
;; render-image-in-cairo-context and see results in window with little
;; delay. Take notice this feature doesn't work in case:
;;
;; (gtk-window-with-cairo-painting #'render-image-in-cairo-context)

