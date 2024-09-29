;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Tue Jan  4 00:19:50 AM IST 2022 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2022 Madhu.  All Rights Reserved.
;;;
(in-package "GIR-LIB")

;;; lisp implementation of (suckless) surf's dmenu-based prompting,
;;; ipc using X11 XProps, with surf-omnihist to manage history.
;;;
;;; depends on functions defined in cmdprompt and xlib-props. On gtk3
;;; processx uses gdk_window_add_filter to filter property events (see
;;; processx-win-add).  On gtk4 hook in to the "xevent" signal on
;;; GdkX11Display to filter property events (see connect-xevents).
;;; both use the event controller key framework for processing
;;; keypress events, which can be used to bind keys to a top level
;;; window.
;;;
(eval-when (load eval compile)
  (use-package "PLISTS-MATCHING"))

(defvar *processx-property-changed-hook* nil
  "Entries are plists, each plist can contain the keys :display-ptr,
:window-id, :atom and :cmd. CMD should be a function which handles a
property change event. it takes the these keys as keyword arguments to
and returns T if the event is handled.

When processx receives a property changed event which matches an
entry, command is called with the plist. If it returns T the event is
handled.")

(defun handle-property-change (&key display-ptr window-id atom)
  (format t "Atom ~A (~D) changed display ~a window ~a to ~a~&"
	  (xlib-xprop:atom-name atom :xdisplay-ptr display-ptr) atom
	  display-ptr window-id
	  (xlib-xprop:get-string-atom atom :xdisplay-ptr display-ptr
				      :xid window-id))
  ;; return t to handle
  t)

(defun foo-eql (a b)
  (cond ((and (cffi:pointerp a) (cffi:pointerp b))
	 (cffi:pointer-eq a b))
	(t (eql a b))))

#+nil
(funcall (make-plists-match-p-fn '(:cmd) #'foo-eql t)
	 '(:cmd nil) '(:cmd t))

(defun add-to-processx-hook (spec)
  (check-type spec cons)
  (install-plist spec '*processx-property-changed-hook* '(:cmd) #'foo-eql t))

(defun delete-from-processx-hook (spec)
  (check-type spec cons)
  (uninstall-plist spec '*processx-property-changed-hook* '(:cmd) #'foo-eql t))


#+nil
(add-to-processx-hook (list :cmd 'handle-property-change))

#||
(pop *processx-property-changed-hook*)
(add-to-processx-hook (list :cmd #'handle-property-change))
(add-to-processx-hook (list :cmd (lambda  (&key display-ptr window-id atom)
				   (warn "FOO!"))
			    :atom 861))
(delete-from-processx-hook '(:cmd nil :atom 861))
(delete-from-processx-hook '(:cmd nil))

(delete-from-processx-hook (list :cmd #'handle-property-change))
*processx-property-changed-hook*
(add-to-processx-hook (list :cmd nil :atom 861))
(trace plists-match-p)
(untrace)
||#

(defun run-processx-hook (&rest args &key display-ptr window-id atom &allow-other-keys &aux ret cmd-1)
  (declare (ignorable window-id display-ptr atom))
  (dolist (target *processx-property-changed-hook*)
    (multiple-value-bind (matches-p empty-p)
	(plists-match-p target args :test #'foo-eql :exclude-keys '(:cmd) :require-all-p nil)
      (when (and (or matches-p empty-p)
		 (if (setq cmd-1 (getf target :cmd))
		     (setq ret (apply cmd-1 args))
		     t))
	(when ret (return ret))))))

(defun processx (xevent-ptr event-ptr data)
  (declare (ignorable event-ptr data))
  (cond
    ((= (cffi:mem-ref xevent-ptr :int) 28) ;;PropertyNotify (X.h)
     (cffi:with-foreign-slots
	 (((atom xlib-xprop::atom)
	   (state xlib-xprop::state)
	   (display-ptr xlib-xprop::display-ptr)
	   (window-id xlib-xprop::window-id))
	  xevent-ptr
	  (:struct xlib-xprop:property-event))
       (cond ((and (= state 0) ;;PropertyNewValue (X.h)
		   (run-processx-hook
		    :display-ptr display-ptr
		    :window-id window-id
		    :atom atom))
	      #+wk
	      (gir:nget *gdk* "FilterReturn" :remove)
	      #-wk
	      t)
	     (t #+wk(gir:nget *gdk* "FilterReturn" :continue)
		#-wk nil))))
    (t #+wk (gir:nget *gdk* "FilterReturn" :continue)
       #-wk nil)))

#+wk
(unless (ignore-errors (cffi:callback processx-callback))
  (eval
   (gir:generate-cffi-defcallback
    (gir:info-of (gir:nget *gdk* "FilterFunc"))
    'processx 'processx-callback)))

#+wk
(defun processx-win-add ($win)
  (cffi:foreign-funcall "gdk_window_add_filter"
    :pointer (this-of (gir:invoke ($win "get_window")))
    :pointer (cffi:callback processx-callback)
    :pointer (cffi:null-pointer)
    :void))

#+wk
(defun processx-win-remove ($win)
  (cffi:foreign-funcall "gdk_window_remove_filter"
  :pointer  (this-of (gir:invoke ($win "get_window")))
  :pointer (cffi:callback processx-callback)
  :pointer (cffi:null-pointer)
  :void))

(defun make-win ()
  (gir:invoke (*gtk* "Window" "new")
    #+wk
    (gir:nget *gtk* "WindowType" :toplevel)))

(defun win-get-events ($win)
  #+wk
  (gir:enum-value-to-string *gdk* "EventMask"
			  (gir:invoke ($win "get_events"))))

(defun win-add-keypress-events ($win)
  #+wk
  (gir:invoke ($win "add_events")
    (logior
     (gir:nget *gdk* "EventMask" :key-press-mask)
     (gir:nget *gdk* "EventMask" :key-release-mask))))




#+nil
(progn
(defvar $win (make-win))
(win-add-keypress-events $win)
(win-get-events $win)
(with-gtk-thread (gir:invoke ($win "show")))
(processx-win-add $win))


#||
*processx-property-changed-hook*
(processx-win-remove $win)
(with-gtk-thread (gir:invoke ($win "hide")))
(win-get-events $win)
(setq $xdisplay-ptr (get-xdisplay))
(xlib-xprop::intern-atom "FOOBAR" :xdisplay-ptr $xdisplay-ptr
			 :only-if-exists nil)
(setf (xlib-xprop:get-string-atom "FOOBAR"
				  :xdisplay-ptr $xdisplay-ptr
				  :xid (get-window-xid $win))
      "FOOBAR1")
||#


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
#||
(gir:get-signal-desc (gir:nget *gtk* "EventControllerKey") "key-pressed")
(gir:nlist-desc *gdk* "ModifierType")
||#

(defun handle-key-press (&key keyval keycode state &allow-other-keys)
  (format t "key pressed: keyval ~A (~A) keycode ~A state ~D (~A)~%"
	  keyval (gir:invoke (*gdk* "keyval_name") keyval)
	  keycode state
	  (gir:enum-value-to-string *gdk* "ModifierType" state))
  nil)

#+nil
(handle-key-press :keyval 120 :keycode 53 :state 8)

(defvar *global-keymap* nil
  "Entries are plists. Each plist can contain the keys :keyval, :keycode
:state and :cmd.  CMD is a function that takes the same keys as
keyword arguments to handle the keystoke represented by these keys and
should return T if it handles it (and stop further searching).")

(defun add-to-global-keymap (spec)
  (install-plist spec '*global-keymap* '(:cmd) #'equal t))

(defun delete-from-global-keymap (spec)
  (uninstall-plist spec '*global-keymap* '(:cmd) #'equal t))

#+nil
(add-to-global-keymap (list :cmd #'handle-key-press))

#+nil
*global-keymap*

#+nil
(plists-match-p ;;(car *processx-property-changed-hook*)
		(list :CMD 'HANDLE-SURF-GO :ATOM 679)
		(list :keyval 120 :keycode 53 :state 8) :require-all-p nil)

(defun run-keymap-hook (&rest args &key keyval keycode state &allow-other-keys &aux ret cmd-1)
  (declare (ignorable keyval keycode state))
  (dolist (entry *global-keymap*)
    (multiple-value-bind (match-p empty-p)
	(plists-match-p entry args :test #'equal :exclude-keys '(:cmd) :require-all-p nil)
      (when (and (or match-p empty-p)
		 (if (setq cmd-1 (getf entry :cmd))
		     (setq ret (apply cmd-1 args)) t))
	(when ret (return ret))))))

#+nil
(run-keymap-hook :keyval 120 :keycode 53 :state 8)

(defun key-pressed-cb (evt-ctlr keyval keycode state) ;return t if handled
  (declare (ignorable evt-ctlr))
  (run-keymap-hook :keyval keyval :keycode keycode :state state))

(unless (ignore-errors (cffi:callback key-pressed-cb-callback))
(eval (gir:generate-cffi-defcallback
       (gir:info-of (gir:get-signal-desc
		     (gir:nget *gtk* "EventControllerKey") "key-pressed"))
       'key-pressed-cb)))

(defvar $evtc1 nil)
(defvar $keypcb-id nil)

(defun connect-global-keymap ($win)
  (unless $evtc1
    (setq $evtc1 (gir:invoke (*gtk* "EventControllerKey" "new") #+wk $win)))
  #-wk
  (gir:invoke ($win "add_controller") $evtc1)
  (unless $keypcb-id
    (setq $keypcb-id
	  (gir:connect $evtc1 "key-pressed"
		       (cffi:callback key-pressed-cb-callback)))))

#+nil
(connect-global-keymap $win)

(defun disconnect-global-keymap ($win &key (delete-event-controller t))
  (when (and $evtc1 $keypcb-id)
    (prog1
	(gir:disconnect $evtc1 $keypcb-id)
      (setq $keypcb-id nil)
      #-wk
      (gir:invoke ($win "_controller") $evtc1)
      (when delete-event-controller
	(setq $evtc1 nil)))))

#+nil
(disconnect-global-keymap $win)

#||
*global-keymap*
(find-plists-matching (list :keyval 32) *global-keymap* '(:cmd) #'eql nil)

(add-to-global-keymap
 (list :keyval (gir:invoke (*gdk* "keyval_from_name") "space")
       :state (gir:nget *gdk* "ModifierType" :mod1-mask)
       :cmd (lambda (&rest keys)
	      (warn "BARF"))))

(delete-from-global-keymap '(:state 8))
*global-keymap*
(gir:invoke (*gdk* "keyval_from_name") "space")
(gir:invoke (*gdk* "keyval_from_name") "Hagul Sios")
(gir:nget *gdk* "Keymap")
(cffi:foreign-symbol-pointer "gdk_display_map_keycode")
(cffi:foreign-symbol-pointer "gdk_display_map_keyval")
(pop *global-keymap*)
||#


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun spawnv (args)
  (let ((sp (gir:invoke (*gio* "Subprocess" "new")  args
			(gir:nget *gio* "SubprocessFlags" :none))))
    (gir:invoke (sp "wait_check") nil)))

(defun make-setprop-cmdargs (readprop setprop prompt)
  (declare (special $win))
  (list "/bin/sh"
	"-c"
	"omnihist-surf goto \"$0\" \"$1\" \"$2\" \"\$3\""
	(prin1-to-string (get-window-xid $win))
	readprop
	setprop
	prompt))

(defun setprop (readprop setprop prompt)
  (spawnv (make-setprop-cmdargs readprop setprop prompt)))


(defun run-surf-go (&rest args)
  (declare (ignore args))
  (setprop "_SURF_URI" "_SURF_GO" "Go: ")
  t)

(defun handle-surf-go (&key $xdisplay-ptr window-id atom)
  (if (eql atom (xlib-xprop::intern-atom "_SURF_GO" :xdisplay-ptr $xdisplay-ptr
					 :only-if-exists nil))
      (warn "handle-surf-go: ~S"
	    (xlib-xprop::get-string-atom atom :xdisplay-ptr $xdisplay-ptr
					 :xid window-id
					 :string-type :utf8-string))))

#+nil
(xlib-xprop::get-string-atom "_SURF_GO" :xdisplay-ptr $xdisplay-ptr
			     :xid (get-window-xid $win)
			     :string-type :utf8-string)

#+nil
(add-to-processx-hook
 (list :cmd 'handle-surf-go
       :atom (xlib-xprop::intern-atom "_SURF_GO"
				      :xdisplay-ptr $xdisplay-ptr
				      :only-if-exists nil)))

#||
*processx-property-changed-hook*
(cl-user::setenv "HISTFN" "/dev/shm/surf-2/history")
(delete-from-processx-hook (list :atom 679))
(add-to-global-keymap
 (list :cmd 'run-surf-go
       :keyval (gir:invoke (*gdk* "keyval_from_name") "g")
       :state (gir:nget *gdk* "ModifierType" :control-mask)))
*global-keymap*
(delete-from-global-keymap '(:keyval 103 :state 4))
(delete-from-global-keymap (list :cmd #'handle-key-press))
(find (list :cmd #'handle-key-press) *global-keymap*
      :test (make-plists-match-p-fn '(:cmd) #'eql t))

(setq $f (make-plists-match-p-fn '(:cmd) #'eql t nil))
(funcall $f   (list :cmd #'handle-key-press)   (list :cmd #'handle-key-press))
(funcall $f   (list :cmd #'handle-key-press)   (cadr *global-keymap*))
*processx-property-changed-hook*
(delete-from-processx-hook (list :atom 861))
||#


;;; ----------------------------------------------------------------------
;;;
;;;;madhu 210321 gtk4
;;;

#+nil
(setq $win (make-win))

#+nil
(gir:invoke ($win "show"))

#+nil
(defvar $gdk-display (gir:invoke ($win "get_display")))

#+nil
(defvar $gdk-x11-display
  (gir:build-object-ptr (gir:nget *gdk-x11* "X11Display")
			(gir:this-of $gdk-display)))


#+nil
(gir:get-signal-desc (gir:nget *gdk-x11* "X11Display") "xevent")

#-wk
(defun x-event-handler-4 (self event)
  (declare (ignorable self))
  (processx event (cffi:null-pointer) (cffi:null-pointer)))

#-wk
(unless (ignore-errors (cffi:callback x-event-handler-4-callback))
(eval
 (gir:generate-cffi-defcallback
  (gir:info-of (gir:get-signal-desc (gir:nget *gdk-x11* "X11Display") "xevent"))
  'x-event-handler-4)))

#-wk
(defvar $xhid nil)

#-wk
(defun connect-xevents ($gdk-x11-display)
  (unless $xhid
    (setq $xhid
	  (gir:connect $gdk-x11-display
		       "xevent"
		       (cffi:callback x-event-handler-4-callback)))))

#-wk
(defun disconnect-xevents ($gdk-x11-display)
  (when $xhid
    (prog1 (gir:disconnect $gdk-x11-display $xhid)
      (setq $xhid nil))))


#+nil
(connect-xevents $gdk-x11-display)

#+nil
(disconnect-xevents $gdk-x11-display)

#||
(gir:invoke ($evtc1 "reset"))
(gir:invoke ($evtc1 "get_widget"))
||#