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

(defparameter *default-stream* *standard-output*)
(defparameter *default-stream-lock* (bt:make-lock))
(defparameter *default-area-displ-x* 0)
(defparameter *default-area-displ-y* 0)
(defparameter *default-area-size-x* 80)
(defparameter *default-area-size-y* 24)

(defparameter *default-priority-fn* 
  (constantly nil)) ; latest added cell is at the top
(defparameter *default-blank-fn* 
  (lambda (x y)
    (declare (ignore x y))
    (values #\Space 7 0))) ; (values chr fg bg) or nil
(defparameter *default-displ-x* 0)
(defparameter *default-displ-y* 0)

(cl-mas:define-synchronized-entity buffer
    (&key (stream *default-stream*) (stream-lock *default-stream-lock*)
          (area-displ-x *default-area-displ-x*) 
          (area-displ-y *default-area-displ-y*)
          (area-size-x *default-area-size-x*) 
          (area-size-y *default-area-size-y*)
          (priority-fn *default-priority-fn*) (blank-fn *default-blank-fn*)
          (displ-x *default-displ-x*) (displ-y *default-displ-y*)
     &aux (pos (make-position 0 0)) (pos-old (make-position 0 0))
          (id-table (make-hash-table :test 'equal)) 
          (pos-table (make-hash-table :test 'equal)))
    (:declarations
      ((type stream stream) (type bt:lock stream-lock)
       (type (integer 0 *) area-displ-x area-displ-y)
       (type (integer 1 *) area-size-x area-size-y)
       (type (function ((or null cell) (or null cell)) (or null cell)) 
             priority-fn)
       (type (function (fixnum fixnum) (or (values character color color)
                                           null)) blank-fn)
       (type fixnum displ-x displ-y)
       (type (cons fixnum fixnum) pos pos-old)
       (type hash-table id-table pos-table)))

  stream stream-lock area-displ-x area-displ-y area-size-x area-size-y
  priority-fn blank-fn displ-x displ-y

  ((set-area (&key adx ady asx asy) :writes (area-displ-x area-displ-y
                                             area-size-x area-size-y)
                   :calls (redraw))
   (when adx (setf area-displ-x adx))
   (when ady (setf area-displ-y ady))
   (when asx (setf area-size-x asx))
   (when asy (setf area-size-y asy))
   (redraw)
   t)

  (((setf priority-fn) (value) :writes (priority-fn) :calls (redraw))
   (prog1 (setf priority-fn value)
     (redraw)))
  (((setf blank-fn) (value) :writes (blank-fn) :calls (redraw))
   (prog1 (setf blank-fn value)
     (redraw)))
  (((setf displ-x) (value) :writes (displ-x) :calls (redraw))
   (prog1 (setf displ-x value)
     (redraw)))
  (((setf displ-y) (value) :writes (displ-y) :calls (redraw))
   (prog1 (setf displ-y value)
     (redraw)))
  ((set-displ (x-value y-value) :writes (displ-x displ-y) :calls (redraw))
   (setf displ-x x-value
         displ-y y-value)
   (redraw)
   t)

  ((push-cell (cell)))
  ((pop-cell (cell)))
  ((redraw-cell (cell)))
  ((redraw ())))















(es:define-structure buffer
  :body-macros ((pos-on-area-p (pos)
                  `(and (>= (position-x ,pos) displ-x)
                        (>= (position-y ,pos) displ-y)
                        (< (position-x ,pos) (+ displ-x area-size-x))
                        (< (position-y ,pos) (+ displ-y area-size-y)))))
  :bindings ((pos-old (make-position 0 0)) (pos (make-position 0 0)) 
             (id-table (make-hash-table :test 'equal)) 
             (pos-table (make-hash-table :test 'equal)) 
             (pos-redraw-table (make-hash-table :test 'equal)) 
             full-redraw-p priority-fn-changed-p blank-fn-changed-p 
             (new-displ-x displ-x) 
             (new-displ-y displ-y) 
             (new-area-displ-x area-displ-x) 
             (new-area-displ-y area-displ-y) 
             (new-area-size-x area-size-x) (new-area-size-y))
  :getters (stream priority-fn blank-fn displ-x displ-y
            area-displ-x area-displ-y area-size-x area-size-y
            (cells ()
              (alexandria:hash-table-values id-table))
            (refresh ()
              (setf full-redraw-p t))
            (redraw ()
              (when (or full-redraw-p blank-fn-changed-p 
                        (/= new-displ-x displ-x)
                        (/= new-displ-y displ-y)
                        (/= new-area-displ-x area-displ-x) 
                        (/= new-area-displ-y area-displ-y) 
                        (/= new-area-size-x area-size-x) 
                        (/= new-area-size-y area-size-y))
                (setf full-redraw-p nil blank-fn-changed-p nil
                      displ-x new-displ-x displ-y new-displ-y
                      area-displ-x new-area-displ-x 
                      area-displ-y new-area-displ-y
                      area-size-x new-area-size-x 
                      area-size-y new-area-size-y)
                (loop :for y 
                      :from displ-y :below (+ displ-y area-size-y)
                      :do
                      (setf (position-y pos) y)
                      (loop :for x 
                            :from displ-x :below (+ displ-x area-size-x)
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
                      #1=(alexandria:deletef (gethash pos-old pos-table)
                                             cell :test #'eq))
                    (when needs-registration-p
                      #2=(push cell (gethash pos pos-table)))
                    (when needs-clearing-p
                      (when (pos-on-area-p pos-old)
                        (setf (gethash pos-old pos-redraw-table) t)))
                    (when needs-drawing-p
                      (when (pos-on-area-p pos)
                        (setf (gethash pos pos-redraw-table) t)))
                    (when moved-p
                      #1#
                      #2#)))
                id-table)
              (alexandria:maphash-keys 
                (lambda (pos)
                  (alexandria:if-let ((cell (occupant-find-top 
                                              pos-table pos 
                                              priority-fn)))
                    (put-character 
                      stream 
                      (+ area-displ-x (- (position-x pos) displ-x)) 
                      (+ area-displ-y (- (position-y pos) displ-y)) 
                      (cell-fg cell) (cell-bg cell) (cell-chr cell))
                    (multiple-value-bind (chr fg bg) 
                        (multiple-value-call blank-fn 
                          (position-x pos) (position-y pos))
                      (put-character
                        stream 
                        (+ area-displ-x (- (position-x pos) displ-x)) 
                        (+ area-displ-y (- (position-y pos) displ-y)) 
                        fg bg chr)))
                  (remhash pos pos-redraw-table))
                pos-redraw-table)
              (force-output stream)
              t))
  :setters ((priority-fn ()
              (setf priority-fn-changed-p t
                    priority-fn es:value))
            (blank-fn ()
              (setf blank-fn-changed-p t
                    blank-fn es:value))
            (displ-x ()
              (setf new-displ-x es:value))
            (displ-y ()
              (setf new-displ-y es:value))
            (area-displ-x ()
              (when (>= es:value 0)
                (setf new-area-displ-x es:value)))
            (area-displ-y ()
              (when (>= es:value 0)
                (setf new-area-displ-y es:value)))
            (area-size-x ()
              (when (>= es:value 0)
                (setf new-area-size-x es:value)))
            (area-size-y ()
              (when (>= es:value 0)
                (setf new-area-size-y es:value)))
            (register-cell ()
              (setf (gethash (cell-id es:value) id-table) es:value))
            (unregister-cell ()
              (remhash (cell-id es:value) id-table))))
 
