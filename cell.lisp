;;;; cell.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:discrete-output-machine)

;;; Coordinates

(cl-se:define-synchronized-entity coord (x y &aux (old-x x) (old-y y) 
                                                  (trigger-flag t) owners)
    (:declarations ((type fixnum x y old-x old-y) 
                    (type boolean trigger-flag) 
                    (type list owners)))

  x y
  ((old-x () :reads (x))
   old-x)
  ((old-y () :reads (y))
   old-y)

  (((setf x) (value) :writes (x) :calls (trigger-owners))
   (setf old-x x x value)
   (when (/= x old-x) 
     (trigger-owners))
   x)
  (((setf y) (value) :writes (y) :calls (trigger-owners))
   (setf old-y y y value)
   (when (/= y old-y) 
     (trigger-owners))
   y)

  ((setv (x-value y-value) :writes (x y) :calls (trigger-owners))
   (setf old-x x x x-value 
         old-y y y y-value)
   (when (or (/= x old-x) (/= y old-y))
     (trigger-owners))
   t)

  (((setf trigger-flag) (value) :writes (trigger-flag))
   (setf trigger-flag value))
  ((trigger-owners () :reads (trigger-flag owners) :visibility :private)
   (when trigger-flag
     (dolist (owner owners)
       ;; use funcall form because the accessor macro isn't defined yet
       (funcall owner +no-value+ :trigger)))) 
  ((push-owner (owner) :writes (owners))
   (pushnew owner owners :test #'eq))
  ((pop-owner (owner) :writes (owners))
   (alexandria:deletef owners owner :test #'eq)))

;;;----------------------------------------------------------------------------
;;; Colored character

(deftype color () '(integer 0 255))

(defparameter *default-chr* #\█)
(defparameter *default-fg* 7)
(defparameter *default-bg* 0)
(defparameter *default-visibility* t)

(cl-se:define-synchronized-entity colchar 
    (&optional (chr *default-chr*) (fg *default-fg*) (bg *default-bg*)
               (visibility *default-visibility*) 
     &aux (old-visibility visibility) (trigger-flag t) owners)
    (:declarations
      ((type character chr)
       (type color fg bg)
       (type boolean visibility old-visibility trigger-flag)
       (type list owners)))

  chr fg bg visibility
  ((old-visibility () :reads (visibility))
   old-visibility)

  (((setf chr) (value) :writes (chr) :reads (visibility) 
               :calls (trigger-owners))
   (setf chr 
         (prog1 value
           (when (and visibility (char/= chr value)) 
             (trigger-owners)))))
  (((setf fg) (value) :writes (fg) :reads (visibility) 
              :calls (trigger-owners))
   (setf fg 
         (prog1 value
           (when (and visibility (/= fg value)) 
             (trigger-owners)))))
  (((setf bg) (value) :writes (bg) :reads (visibility) 
              :calls (trigger-owners))
   (setf bg 
         (prog1 value
           (when (and visibility (/= bg value)) 
             (trigger-owners)))))
  (((setf visibility) (value) :writes (visibility) :calls (trigger-owners))
   (setf old-visibility visibility visibility value) 
   (unless (eql visibility old-visibility) 
     (trigger-owners))
   visibility)

  ((setv (chr-value fg-value bg-value) 
         :writes (chr fg bg) :reads (visibility) :calls (trigger-owners))
   (when (or (char/= chr chr-value) (/= fg fg-value) (/= bg bg-value))
     (setf chr chr-value fg fg-value bg bg-value)
     (when visibility
       (trigger-owners)))
   t)

  (((setf trigger-flag) (value) :writes (trigger-flag))
   (setf trigger-flag value))
  ((trigger-owners () :reads (trigger-flag owners) :visibility :private)
   (when trigger-flag
     (dolist (owner owners)
       ;; use funcall form because the accessor macro isn't defined yet
       (funcall owner +no-value+ :trigger)))) 
  ((push-owner (owner) :writes (owners))
   (pushnew owner owners :test #'eq))
  ((pop-owner (owner) :writes (owners))
   (alexandria:deletef owners owner :test #'eq)))

;;;----------------------------------------------------------------------------
;;; Cell

(defparameter *default-buffer* nil)

(cl-se:define-synchronized-entity cell 
    (coord &optional (colchar (make-colchar)) (buffer *default-buffer*))
    (:declarations
      ((type coord coord) (type colchar colchar)
       (type (or null buffer) buffer))
      :initialization
      ((coord-push-owner coord self)
       (colchar-push-owner colchar self)
       (when buffer
         (funcall buffer :push-cell +no-value+ self))))

  coord colchar buffer
  (((setf coord) (value) :writes (coord) :reads (buffer))
   (coord-pop-owner coord self)
   (prog1 (setf coord value)
     (coord-push-owner coord self)
     (when buffer
       (funcall buffer :redraw-cell +no-value+ self))))
  (((setf colchar) (value) :writes (colchar) :reads (buffer))
   (colchar-pop-owner colchar self)
   (prog1 (setf colchar value)
     (colchar-push-owner colchar self)
     (when buffer
       (funcall buffer :redraw-cell +no-value+ self))))

  ((setv (x-value y-value chr-value fg-value bg-value) 
         :writes (coord colchar) :reads (buffer))
   (setf (coord-trigger-flag coord) nil
         (colchar-trigger-flag colchar) nil)
   (coord-setv coord x-value y-value)
   (colchar-setv colchar chr-value fg-value bg-value)
   (setf (coord-trigger-flag coord) t
         (colchar-trigger-flag colchar) t)
   (when buffer
     (funcall buffer :redraw-cell +no-value+ self))
   t)

  (((setf buffer) (value) :writes (buffer))
   (when buffer
     (funcall buffer :pop-cell +no-value+ self))
   (prog1 (setf buffer value)
     (when buffer
       (funcall buffer :push-cell +no-value+ self))))

  ((trigger () :reads (buffer))
   (when buffer
     (funcall buffer :redraw-cell +no-value+ self)))

  ((moved-p () :reads (coord))
   (or (/= (coord-x coord) (coord-old-x coord))
       (/= (coord-y coord) (coord-old-y coord))))
  ((needs-clearing-p () :reads (coord colchar))
   (and (colchar-old-visibility colchar)
        (or (not (colchar-visibility colchar)) (moved-p))))
  ((needs-drawing-p () :reads (coord colchar))
   (and (colchar-visibility colchar)
        (or (not (colchar-old-visibility colchar)) (moved-p)))))

(defmacro cell-x (c)
  `(coord-x (cell-coord ,c)))

(defmacro cell-y (c)
  `(coord-y (cell-coord ,c)))

(defmacro cell-old-x (c)
  `(coord-old-x (cell-coord ,c)))

(defmacro cell-old-y (c)
  `(coord-old-y (cell-coord ,c)))

(defmacro cell-chr (c)
  `(colchar-chr (cell-colchar ,c)))

(defmacro cell-fg (c)
  `(colchar-fg (cell-colchar ,c)))

(defmacro cell-bg (c)
  `(colchar-bg (cell-colchar ,c)))

(defmacro cell-visibility (c)
  `(colchar-visibility (cell-colchar ,c)))

(defmacro cell-old-visibility (c)
  `(colchar-old-visibility (cell-colchar ,c)))

