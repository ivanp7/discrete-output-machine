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

(defun set-position (pos x y)
  (setf (position-x pos) x
        (position-y pos) y)
  t)

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

(defparameter *default-stream* *standard-output*)
(defparameter *default-stream-lock* (bt:make-lock))
(defparameter *default-area-pos-x* 0)
(defparameter *default-area-pos-y* 0)
(defparameter *default-area-size-x* 80)
(defparameter *default-area-size-y* 24)

(defparameter *default-priority-fn* 
  (constantly nil)) ; latest added cell is at the top
(defparameter *default-blank-fn* 
  (lambda (x y)
    (declare (ignore x y))
    (values #\Space 7 nil))) ; (values chr fg bg)
(defparameter *default-displ-x* 0)
(defparameter *default-displ-y* 0)

(cl-mas:define-entity buffer
    (&key (stream *default-stream*) (stream-lock *default-stream-lock*)
          (area-pos-x *default-area-pos-x*) 
          (area-pos-y *default-area-pos-y*)
          (area-size-x *default-area-size-x*) 
          (area-size-y *default-area-size-y*)
          (priority-fn *default-priority-fn*) 
          (blank-fn *default-blank-fn*)
          (displ-x *default-displ-x*) (displ-y *default-displ-y*)
     &aux (pos (make-position 0 0)) (pos-old (make-position 0 0))
          (cell-table (make-hash-table :test 'eq)) 
          (occ-table (make-hash-table :test 'equal))
          (flag-table (make-hash-table :test 'equal)) full-redraw-flag
          (new-displ-x displ-x) (new-displ-y displ-y))
    (:declarations
      ((type stream stream)
       (type bt:lock stream-lock)
       (type (integer 0 *) area-pos-x area-pos-y)
       (type (integer 1 *) area-size-x area-size-y)
       (type (function (cell cell) boolean) priority-fn)
       (type (function (fixnum fixnum) (values character color color)))
       (type fixnum displ-x displ-y new-displ-x new-displ-y)
       (type (cons fixnum fixnum) pos pos-old)
       (type hash-table cell-table occ-table flag-table)
       (type boolean full-redraw-flag)))

  ((stream ()) (values stream stream-lock))

  ((area-pos-x () :reads (area)) area-pos-x)
  ((area-pos-y () :reads (area)) area-pos-y)
  ((area-size-x () :reads (area)) area-size-x)
  ((area-size-y () :reads (area)) area-size-y)
  ((set-area (&key pos-x pos-y size-x size-y) :writes (area full-redraw-flag))
   (let ((pos-x (or pos-x area-pos-x)) (pos-y (or pos-y area-pos-y))
         (size-x (or size-x area-size-x)) (size-y (or size-y area-size-y))) 
     (setf area-pos-x pos-x area-pos-y pos-y
           area-size-x size-x area-size-y size-y
           full-redraw-flag t))
   t)

  priority-fn blank-fn
  (((setf priority-fn) (value) :writes (priority-fn full-redraw-flag))
   (setf full-redraw-flag t
         priority-fn value))
  (((setf blank-fn) (value) :writes (blank-fn full-redraw-flag))
   (setf full-redraw-flag t
         blank-fn value))

  displ-x displ-y
  (((setf displ-x) (value) :writes (new-displ-x))
   (setf new-displ-x value))
  (((setf displ-y) (value) :writes (new-displ-y))
   (setf new-displ-y value))
  ((set-displ (x-value y-value) :writes (new-displ-x new-displ-y))
   (setf new-displ-x x-value
         new-displ-y y-value)
   t)

  ((number-of-cells () :reads (cell-table))
   (hash-table-count cell-table))
  ((map-cells (fn) :reads (cell-table))
   (alexandria:maphash-keys fn cell-table))

  ((pos-in-area-p (pos) :reads (area displ-x displ-y) :visibility :private)
   (and (>= (position-x pos) displ-x)
        (>= (position-y pos) displ-y)
        (< (position-x pos) (+ displ-x area-size-x))
        (< (position-y pos) (+ displ-y area-size-y))))
  ((flag-position (pos) :writes (flag-table) :calls (pos-in-area-p) 
                  :visibility :private)
   (when (pos-in-area-p pos)
     (setf (gethash pos flag-table) t)))

  ((push-cell (cell) :writes (cell-table occ-table) 
                     :calls (flag-position) :visibility :private)
   (set-position pos (coord-x (cell-coord cell)) (coord-y (cell-coord cell)))
   (push cell (gethash pos occ-table))
   (setf (gethash cell cell-table) t)
   (flag-position pos)
   t)
  ((pop-cell (cell) :writes (cell-table occ-table) 
                    :calls (flag-position) :visibility :private)
   (set-position pos-old (coord-x (cell-coord cell)) 
                 (coord-y (cell-coord cell)))
   (unless (alexandria:deletef (gethash pos-old occ-table) cell :test #'eq)
     (remhash pos-old occ-table))
   (remhash cell cell-table)
   (flag-position pos-old)
   t)

  ((redraw (&optional full) :writes (displ-x displ-y occ-table flag-table
                                     full-redraw-flag) 
           :reads (area priority-fn blank-fn new-displ-x new-displ-y 
                   cell-table) :calls (pos-in-area-p flag-position))
   (setf full-redraw-flag (or full full-redraw-flag
                              (/= displ-x new-displ-x) 
                              (/= displ-y new-displ-y))
         displ-x new-displ-x displ-y new-displ-y)
   (when full-redraw-flag
     (clrhash flag-table)
     (dotimes (y area-size-y)
       (dotimes (x area-size-x)
         (set-position pos (+ displ-x x) (+ displ-y y))
         (flag-position pos)))
     (setf full-redraw-flag nil))

   (alexandria:maphash-keys
     (lambda (cell)
       (let ((needs-clearing-p (cell-needs-clearing-p cell))
             (needs-drawing-p (cell-needs-drawing-p cell))
             (moved-p (cell-moved-p cell)))
         (set-position pos-old (coord-x (cell-coord cell)) 
                       (coord-y (cell-coord cell)))
         (cell-trigger cell t)
         (set-position pos (coord-x (cell-coord cell)) 
                       (coord-y (cell-coord cell)))
         (when needs-clearing-p
           (flag-position pos-old))
         (when needs-drawing-p
           (flag-position pos))
         (when moved-p
           (unless (alexandria:deletef (gethash pos-old occ-table) cell 
                                       :test #'eq)
             (remhash pos-old occ-table))
           (push cell (gethash pos occ-table)))))
     cell-table)

   (bt:with-lock-held (stream-lock)
     (alexandria:maphash-keys
       (lambda (pos)
         (when (pos-in-area-p pos)
           (let (chr fg bg)
             (alexandria:if-let ((cell (occupant-find-top occ-table pos 
                                                          priority-fn)))
               (setf chr (colchar-chr (cell-colchar cell))
                     fg (colchar-fg (cell-colchar cell))
                     bg (colchar-bg (cell-colchar cell)))
               (multiple-value-bind (chr-value fg-value bg-value)
                   (funcall blank-fn (position-x pos) (position-y pos))
                 (setf chr chr-value fg fg-value bg bg-value)))
             (put-character stream
                            (+ area-pos-x (- (position-x pos) displ-x)) 
                            (+ area-pos-y (- (position-y pos) displ-y)) 
                            fg bg chr))))
       flag-table))
   (clrhash flag-table)
   t))

