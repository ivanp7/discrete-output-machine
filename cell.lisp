;;;; cell.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:discrete-output-machine)

(defparameter *default-cell-data* ())
(defparameter *default-cell-x* 0)
(defparameter *default-cell-y* 0)
(defparameter *default-cell-chr* #\?)
(defparameter *default-cell-fg* 7)
(defparameter *default-cell-bg* 1)
(defparameter *default-cell-visibility* nil)
(defparameter *default-cell-buffer* nil)

(es:define-structure cell
  :parameters (id &key (data *default-cell-data*) 
                  (x *default-cell-x*) (y *default-cell-y*) 
                  (chr *default-cell-chr*)
                  (fg *default-cell-fg*) (bg *default-cell-bg*) 
                  (visibility *default-cell-visibility*) 
                  (buffer *default-cell-buffer*))
  :bindings (data-changed-p (new-x x) (new-y y) (new-chr chr) 
             (new-fg fg) (new-bg bg) (new-visibility visibility) 
             (new-buffer buffer))
  :init-forms ((setf buffer nil))
  :getters (id data x y chr fg bg visibility buffer
            (needs-unregistration-p ()
              (and buffer (null new-buffer)))
            (needs-registration-p ()
              (and new-buffer (null buffer)))
            (needs-clearing-p ()
              (and buffer visibility (or (not new-visibility)
                                         (null new-buffer)
                                         (/= new-x x) (/= new-y y))))
            (needs-drawing-p ()
              (and new-buffer new-visibility 
                   (or (not visibility) (null buffer) 
                       (/= new-x x) (/= new-y y) (char/= new-chr chr) 
                       (/= new-fg fg) (/= new-bg bg) data-changed-p)))
            (moved-p ()
              (and buffer new-buffer (or (/= new-x x) (/= new-y y))))
            (update ()
              (setf data-changed-p nil x new-x y new-y 
                    chr new-chr fg new-fg bg new-bg visibility new-visibility
                    buffer new-buffer)))
  :setters ((data ()
              (setf data-changed-p t
                    data es:value))
            (x ()
              (setf new-x (if (and visibility buffer) 
                            es:value (setf x es:value))))
            (y ()
              (setf new-y (if (and visibility buffer) 
                            es:value (setf y es:value))))
            (chr ()
              (setf new-chr (if (and visibility buffer) 
                              es:value (setf chr es:value))))
            (fg ()
              (when (and (>= es:value 0) (< es:value 256))
                (setf new-fg (if (and visibility buffer) 
                               es:value (setf fg es:value)))))
            (bg ()
              (when (and (>= es:value 0) (< es:value 256))
                (setf new-bg (if (and visibility buffer) 
                               es:value (setf bg es:value)))))
            (visibility ()
              (setf new-visibility (if buffer
                                     es:value (setf visibility es:value))))
            (buffer ()
              (if es:value
                (if (null buffer)
                  (progn
                    (when (and new-buffer (not (eq new-buffer es:value)))
                      (funcall new-buffer :unregister-cell es:self))
                    (unless (eq es:value new-buffer)
                      (funcall es:value :register-cell es:self))
                    (setf new-buffer es:value))
                  buffer)
                (when new-buffer
                  (funcall new-buffer :unregister-cell es:self)
                  (setf new-buffer nil)))))
  :post-forms ((when new-buffer
                 (funcall new-buffer :register-cell es:self))))

