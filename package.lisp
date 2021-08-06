(in-package "CL-USER")

(defpackage "GTK-APP"
  (:use "CL" "GIR-LIB")
  (:export
   ;; gtk-app
   "GTK-APPLICATION-MIXIN" "ACTIVATE" "RUN" "QUIT" "BUILDER-GET-OBJECT"
   "ACTIVATE-WITH-BUILDER" "GTK-APPLICATION-BUILDER-MIXIN" "CONTAINER-VIEW"
   "UPDATE-BUILDER-SCOPE"
   "REGISTER" "REALLY-QUIT" "RUN-SAFE"
   ))
