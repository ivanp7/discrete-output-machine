;;;; buffer.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:discrete-output-machine)

(defmacro make-position (x y)
  `(cons ,x ,y))

(defmacro position-x (pos)
  `(car ,pos))

(defmacro position-y (pos)
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
(defparameter *default-buffer-screen-displ-x* 0)
(defparameter *default-buffer-screen-displ-y* 0)
(defparameter *default-buffer-screen-size-x* 80)
(defparameter *default-buffer-screen-size-y* 24)

(es:define-structure buffer
  :parameters (&key (stream *default-buffer-stream*)
                    (cell-priority-fn *default-buffer-priority-fn*)
                    (blank-fn *default-buffer-blank-fn*)
                    (displ-x *default-buffer-displ-x*)
                    (displ-y *default-buffer-displ-y*)
                    (screen-displ-x *default-buffer-screen-displ-x*)
                    (screen-displ-y *default-buffer-screen-displ-y*)
                    (screen-size-x *default-buffer-screen-size-x*)
                    (screen-size-y *default-buffer-screen-size-y*))
  :body-macros ((pos-on-screen-p (pos)
                  `(and (>= (position-x ,pos) displ-x)
                        (>= (position-y ,pos) displ-y)
                        (< (position-x ,pos) (+ displ-x screen-size-x))
                        (< (position-y ,pos) (+ displ-y screen-size-y)))))
  :bindings ((pos-old (make-position 0 0)) (pos (make-position 0 0)) 
             (id-table (make-hash-table :test 'equal)) 
             (pos-occ-table (make-hash-table :test 'equal)) 
             (pos-redraw-table (make-hash-table :test 'equal)) 
             full-redraw-p cell-priority-fn-changed-p blank-fn-changed-p 
             (new-displ-x displ-x) 
             (new-displ-y displ-y) 
             (new-screen-displ-x screen-displ-x) 
             (new-screen-displ-y screen-displ-y) 
             (new-screen-size-x screen-size-x) (new-screen-size-y))
  :getters (stream cell-priority-fn blank-fn displ-x displ-y
            screen-displ-x screen-displ-y screen-size-x screen-size-y
            (cells ()
              (alexandria:hash-table-values id-table))
            (refresh ()
              (setf full-redraw-p t))
            (redraw ()
              (when (or full-redraw-p blank-fn-changed-p 
                        (/= new-displ-x displ-x)
                        (/= new-displ-y displ-y)
                        (/= new-screen-displ-x screen-displ-x) 
                        (/= new-screen-displ-y screen-displ-y) 
                        (/= new-screen-size-x screen-size-x) 
                        (/= new-screen-size-y screen-size-y))
                (setf full-redraw-p nil blank-fn-changed-p nil
                      displ-x new-displ-x displ-y new-displ-y
                      screen-displ-x new-screen-displ-x 
                      screen-displ-y new-screen-displ-y
                      screen-size-x new-screen-size-x 
                      screen-size-y new-screen-size-y)
                (loop :for y 
                      :from displ-y :below (+ displ-y screen-size-y)
                      :do
                      (setf (position-y pos) y)
                      (loop :for x 
                            :from displ-x :below (+ displ-x screen-size-x)
                            :do
                            (setf (position-x pos) x 
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
                    (setf (position-x pos-old) (cell-x cell) 
                          (position-y pos-old) (cell-y cell))
                    (cell-update cell)
                    (setf (position-x pos) (cell-x cell) 
                          (position-y pos) (cell-y cell))
                    (when needs-unregistration-p
                      (remhash id id-table)
                      #1=(alexandria:deletef (gethash pos-old pos-occ-table)
                                             cell :test #'eq))
                    (when needs-registration-p
                      #2=(push cell (gethash pos pos-occ-table)))
                    (when needs-clearing-p
                      (when (pos-on-screen-p pos-old)
                        (setf (gethash pos-old pos-redraw-table) t)))
                    (when needs-drawing-p
                      (when (pos-on-screen-p pos)
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
                      stream 
                      (+ screen-displ-x (- (position-x pos) displ-x)) 
                      (+ screen-displ-y (- (position-y pos) displ-y)) 
                      (cell-fg cell) (cell-bg cell) (cell-chr cell))
                    (multiple-value-bind (chr fg bg) 
                        (multiple-value-call blank-fn 
                          (position-x pos) (position-y pos))
                      (put-character
                        stream 
                        (+ screen-displ-x (- (position-x pos) displ-x)) 
                        (+ screen-displ-y (- (position-y pos) displ-y)) 
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
              (setf new-displ-x es:value))
            (displ-y ()
              (setf new-displ-y es:value))
            (screen-displ-x ()
              (when (>= es:value 0)
                (setf new-screen-displ-x es:value)))
            (screen-displ-y ()
              (when (>= es:value 0)
                (setf new-screen-displ-y es:value)))
            (screen-size-x ()
              (when (>= es:value 0)
                (setf new-screen-size-x es:value)))
            (screen-size-y ()
              (when (>= es:value 0)
                (setf new-screen-size-y es:value)))
            (register-cell ()
              (setf (gethash (cell-id es:value) id-table) es:value))
            (unregister-cell ()
              (remhash (cell-id es:value) id-table))))
 
