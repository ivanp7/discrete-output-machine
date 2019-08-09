;;;; discrete-output-machine.lisp
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
  (:parameters (id &key (data *default-cell-data*) 
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
               (when (>= es:value 0)
                 (setf new-x (if (and visibility buffer) 
                               es:value (setf x es:value)))))
             (y ()
               (when (>= es:value 0)
                 (setf new-y (if (and visibility buffer) 
                               es:value (setf y es:value)))))
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
                  (funcall new-buffer :register-cell es:self)))))

;;;----------------------------------------------------------------------------

(defmacro make-pos (x y)
  `(cons ,x ,y))

(defmacro pos-x (pos)
  `(car ,pos))

(defmacro pos-y (pos)
  `(cdr ,pos))

;;;----------------------------------------------------------------------------

(defun occupant-find-top (table pos priority-fn)
  (reduce (lambda (&optional c1 c2)
            (if (and c1 c2 (cell-visibility c1) (cell-visibility c2))
              (if (funcall priority-fn c1 c2) 
                c2 
                c1)
              (if (and c2 (cell-visibility c2))
                c2
                (when (and c1 (cell-visibility c1))
                  c1)))) 
          (gethash pos table)))

;;;----------------------------------------------------------------------------

(defparameter *default-buffer-stream* *standard-output*)
(defparameter *default-buffer-priority-fn* 
  (constantly nil)) ; latest added cell is at the top
(defparameter *default-buffer-blank-fn* 
  (lambda (x y)
    (declare (ignore x y))
    (values #\Space 7 0))) ; (values chr fg bg)
(defparameter *default-buffer-displ-x* 0)
(defparameter *default-buffer-displ-y* 0)
(defparameter *default-buffer-size-x* 80)
(defparameter *default-buffer-size-y* 24)

(es:define-structure buffer
  (:parameters (&key (stream *default-buffer-stream*)
                     (cell-priority-fn *default-buffer-priority-fn*)
                     (blank-fn *default-buffer-blank-fn*)
                     (displ-x *default-buffer-displ-x*)
                     (displ-y *default-buffer-displ-y*)
                     (size-x *default-buffer-size-x*)
                     (size-y *default-buffer-size-y*))
   :bindings ((pos-old (make-pos 0 0)) (pos (make-pos 0 0)) 
              (id-table (make-hash-table :test 'equal)) 
              (pos-occ-table (make-hash-table :test 'equal)) 
              (pos-redraw-table (make-hash-table :test 'equal)) 
              full-redraw-p cell-priority-fn-changed-p blank-fn-changed-p 
              (new-displ-x displ-x) (new-displ-y displ-y) 
              (new-size-x size-x) (new-size-y))
   :getters (stream cell-priority-fn blank-fn displ-x displ-y size-x size-y
             (cells ()
               (alexandria:hash-table-values id-table))
             (refresh ()
               (setf full-redraw-p t))
             (redraw ()
               (when (or full-redraw-p blank-fn-changed-p 
                         (/= new-displ-x displ-x) (/= new-displ-y displ-y) 
                         (/= new-size-x size-x) (/= new-size-y size-y))
                 (setf full-redraw-p nil blank-fn-changed-p nil
                       displ-x new-displ-x displ-y new-displ-y
                       size-x new-size-x size-y new-size-y)
                 (dotimes (y size-y)
                   (setf (pos-y pos) y)
                   (dotimes (x size-x)
                     (setf (pos-x pos) x 
                           (gethash pos pos-redraw-table) t))))
               (maphash 
                 (lambda (id cell)
                   (let ((needs-unregistration-p 
                           (cell-needs-unregistration-p cell))
                         (needs-registration-p 
                           (cell-needs-registration-p cell))
                         (needs-clearing-p (cell-needs-clearing-p cell))
                         (needs-drawing-p (cell-needs-drawing-p cell))
                         (moved-p (cell-moved-p cell)))
                     (setf (pos-x pos-old) (cell-x cell) 
                           (pos-y pos-old) (cell-y cell))
                     (cell-update cell)
                     (setf (pos-x pos) (cell-x cell) 
                           (pos-y pos) (cell-y cell))
                     (when needs-unregistration-p
                       (remhash id id-table)
                       #1=(alexandria:deletef (gethash pos-old pos-occ-table)
                                              cell :test #'eq))
                     (when needs-registration-p
                       #2=(push cell (gethash pos pos-occ-table)))
                     (when needs-clearing-p
                       (when (and (< (pos-x pos-old) size-x)
                                  (< (pos-y pos-old) size-y))
                         (setf (gethash pos-old pos-redraw-table) t)))
                     (when needs-drawing-p
                       (when (and (< (pos-x pos) size-x)
                                  (< (pos-y pos) size-y))
                         (setf (gethash pos pos-redraw-table) t)))
                     (when moved-p
                       #1#
                       #2#)))
                 id-table)
               (alexandria:maphash-keys 
                 (lambda (pos)
                   (alexandria:if-let ((cell (occupant-find-top 
                                               pos-occ-table pos 
                                               cell-priority-fn)))
                     (put-character 
                       stream (+ displ-x (pos-x pos)) (+ displ-y (pos-y pos)) 
                       (cell-fg cell) (cell-bg cell) (cell-chr cell))
                     (multiple-value-bind (chr fg bg) 
                         (multiple-value-call blank-fn (pos-x pos) (pos-y pos))
                       (put-character
                         stream (+ displ-x (pos-x pos)) (+ displ-y (pos-y pos)) 
                         fg bg chr)))
                   (remhash pos pos-redraw-table))
                 pos-redraw-table)
               (force-output stream)
               t))
   :setters ((cell-priority-fn ()
               (setf cell-priority-fn-changed-p t
                     cell-priority-fn es:value))
             (blank-fn ()
               (setf blank-fn-changed-p t
                     blank-fn es:value))
             (displ-x ()
               (when (>= es:value 0)
                 (setf new-displ-x es:value)))
             (displ-y ()
               (when (>= es:value 0)
                 (setf new-displ-y es:value)))
             (size-x ()
               (when (>= es:value 0)
                 (setf new-size-x es:value)))
             (size-y ()
               (when (>= es:value 0)
                 (setf new-size-y es:value)))
             (register-cell ()
               (setf (gethash (cell-id es:value) id-table) es:value))
             (unregister-cell ()
               (remhash (cell-id es:value) id-table)))))
 
