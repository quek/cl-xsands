(defpackage cl-xsands
  (:use :cl))

(in-package :cl-xsands)

(defconstant +space-code+ 65)
(defconstant +shift-code+ 50)
(defconstant +tab-code+ 23)

(defvar *display*)
(defvar *window*)

(defun select-input ()
  (setf (xlib:window-event-mask *window*)
        (xlib:make-event-mask :focus-change)))

(defun off-space-auto-repeat ()
  (xlib:change-keyboard-control *display*
                                :key +space-code+
                                :auto-repeat-mode :off)
  (xlib:display-force-output *display*))

(defun default-space-auto-repeat ()
  (xlib:change-keyboard-control *display*
                                :key +space-code+
                                :auto-repeat-mode :default)
  (xlib:display-force-output *display*))

(defun grab-keyboard ()
  (xlib:grab-keyboard *window* :owner-p t
                      :sync-pointer-p nil :sync-keyboard-p nil)
  (xlib:display-force-output *display*))

(defun ungrab-keyboard ()
  (xlib:ungrab-keyboard *display*)
  (xlib:display-force-output *display*))

(defun grab-space ()
  (xlib:grab-key *window* +space-code+ :modifiers :any)
  (xlib:display-force-output *display*))

(defun ungrab-space ()
  (xlib:ungrab-key *window* +space-code+ :modifiers :any)
  (xlib:display-force-output *display*))

(defun emit (code)
  (xtest:fake-key-event *display* code t)
  (xtest:fake-key-event *display* code nil)
  (xlib:display-force-output *display*))

(defun init ()
  (setf *display* (xlib:open-default-display)
        *window* (xlib:input-focus *display*))
  (xlib:set-input-focus *display* *window* :parent)
  (select-input)
  (xlib:set-input-focus *display* :pointer-root :none)
  (xlib:set-input-focus *display* *window* :none)
  (off-space-auto-repeat)
  (xlib:display-force-output *display*))


(defmacro p (&rest args)
  (declare (ignorable args))
  #+nil
  `(progn
     (format t "~a~%" (get-internal-real-time))
     ,@(mapcar (lambda (x)
                 `(format t "~a => ~a~&" ',x ,x))
               args)
     (terpri)
     (force-output)))

(let (key-press-p)
  (defun proc ()
    (xlib:event-cond
     (*display*)
     (:focus-out () t
                 (p :focus-out)
                 (ungrab-space)
                 (setf *window* (loop thereis (ignore-errors
                                                (xlib:input-focus *display*))
                                      do (sleep 0.01)))
                 (unless (member *window* '(:none :pointer-root))
                   (select-input)
                   (grab-space)))
     (:key-release (code) (= code +space-code+)
                   (p :key-release-space)
                   (ungrab-keyboard)
                   (if key-press-p
                       (setf key-press-p nil)
                     (progn
                       (ungrab-space)
                       (emit +space-code+)
                       (grab-space))))
     (:key-press (code) t
                 (cond ((= code +space-code+)
                        (p :key-press-space)
                        (grab-keyboard))
                       (t
                        (p :key-press3 code)
                        (setf key-press-p t)
                        (ungrab-keyboard)
                        (xtest:fake-key-event *display* +shift-code+ t)
                        ;; いったんキーリリースが必要みたい。
                        (xtest:fake-key-event *display* code nil)
                        (xtest:fake-key-event *display* code t)
                        (xtest:fake-key-event *display* code nil)
                        (xtest:fake-key-event *display* +shift-code+ nil)
                        (grab-keyboard)))))
    (xlib:display-force-output *display*)))

(defun main ()
  (init)
  (loop (ignore-errors (funcall 'proc))))
;;(sb-thread:make-thread 'main)

#|
(xlib:process-event *display*
                    :timeout 0
                    :handler (lambda (&rest args) (print args)))

(xlib:event-listen *display*)

(let ((keycode 38))
  (xtest:fake-key-event *display* +shift-code+ t)
  (xtest:fake-key-event *display* keycode t)
  (xtest:fake-key-event *display* keycode nil)
  (xtest:fake-key-event *display* +shift-code+ nil)
  (xlib:display-force-output *display*))

(xlib:keysym->keycodes *display* xlib::left-shift-keysym)
|#