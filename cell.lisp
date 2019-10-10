;;;; cell.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:discrete-output-machine)

(cl-mas:define-entity coord
    (x y &aux (new-x x) (new-y y) owners)
    (:declarations
      ((type fixnum x y new-x new-y)
       (type list owners)))
  
  x y new-x new-y
  (((setf x) (value) :writes (new-x))
   (setf new-x value))
  (((setf y) (value) :writes (new-y))
   (setf new-y value))
  ((setv (x-value y-value) :writes (new-x new-y))
   (setf new-x x-value new-y y-value)
   t)

  ((changed-p () :reads (x y new-x new-y))
   (or (not (eql x new-x)) (not (eql y new-y))))
  ((trigger (&optional force) :writes (x y) :reads (new-x new-y owners))
   (when (or (null owners) force)
     (setf x new-x y new-y)
     t))

  ((push-owner (owner) :writes (owners))
   (push owner owners)
   t)
  ((pop-owner (owner) :writes (owners))
   (alexandria:deletef owners owner :test #'eq)
   t)
  ((owned-p () :reads (owners))
   (not (null owners))))

(deftype color () '(or null (integer 0 255)))

(defparameter *default-chr* #\█)
(defparameter *default-fg* 7)
(defparameter *default-bg* nil)

(cl-mas:define-entity colchar
    (&optional (chr *default-chr*) (fg *default-fg*) (bg *default-bg*) 
     &aux (new-chr chr) (new-fg fg) (new-bg bg) owners)
    (:declarations
      ((type character chr new-chr)
       (type color fg bg new-fg new-bg)))

  chr fg bg new-chr new-fg new-bg
  (((setf chr) (value) :writes (new-chr))
   (setf new-chr value))
  (((setf fg) (value) :writes (new-fg))
   (setf new-fg value))
  (((setf bg) (value) :writes (new-bg))
   (setf new-bg value))
  ((setv (chr-value fg-value bg-value) :writes (new-chr new-fg new-bg))
   (setf new-chr chr-value new-fg fg-value new-bg bg-value)
   t)

  ((changed-p () :reads (chr fg bg new-chr new-fg new-bg))
   (or (char/= chr new-chr) (not (eql fg new-fg)) (not (eql bg new-bg))))
  ((trigger (&optional force) :writes (chr fg bg) 
            :reads (new-chr new-fg new-bg))
   (when (or (null owners) force)
     (setf chr new-chr fg new-fg bg new-bg)
     t))

  ((push-owner (owner) :writes (owners))
   (push owner owners)
   t)
  ((pop-owner (owner) :writes (owners))
   (alexandria:deletef owners owner :test #'eq)
   t)
  ((owned-p () :reads (owners))
   (not (null owners))))

(defparameter *default-visibility* t)
(defparameter *default-buffer* nil)

(cl-mas:define-entity cell
    (role coord &optional (colchar (make-colchar)) 
     (visibility *default-visibility*) (buffer *default-buffer*)
     &aux (new-visibility visibility))
    (:declarations
      ((type coord coord)
       (type colchar colchar)
       (type boolean visibility new-visibility))
     :initialization
      ((coord-push-owner coord cl-mas:self)
       (colchar-push-owner colchar cl-mas:self)))

  role coord colchar visibility buffer
  (setf role)
  (((setf coord) (value) :writes (coord) :reads (buffer))
   (unless buffer
     (coord-pop-owner coord cl-mas:self)
     (prog1 (setf coord value)
       (coord-push-owner coord cl-mas:self))))
  (((setf colchar) (value) :writes (colchar) :reads (buffer))
   (unless buffer
     (colchar-pop-owner colchar cl-mas:self)
     (prog1 (setf colchar value)
       (colchar-push-owner colchar cl-mas:self))))
  (((setf visibility) (value) :writes (new-visibility))
   (setf new-visibility value))
  (((setf buffer) (value) :writes (buffer))
   (when buffer
     (funcall buffer :pop-cell cl-mas:self))
   (trigger t)
   (prog1 (setf buffer value)
     (funcall buffer :push-cell cl-mas:self)))

  ((changed-p () :reads (coord colchar visibility new-visibility))
   (or (not (eql visibility new-visibility)) 
       (coord-changed-p coord) (colchar-changed-p colchar)))
  ((trigger (&optional force) :writes (coord colchar visibility)
            :reads (new-visibility))
   (when (or (null buffer) force)
     (coord-trigger force)
     (colchar-trigger force)
     (setf visibility new-visibility)
     t))

  ((needs-clearing-p () :reads (coord visibility new-visibility))
   (and visibility (or (not new-visibility) (coord-changed-p coord))))
  ((needs-drawing-p () :reads (coord colchar visibility new-visibility))
   (and new-visibility (or (not visibility) (coord-changed-p coord) 
                           (colchar-changed-p colchar))))
  ((moved-p () :reads (coord))
   (coord-changed-p coord)))

