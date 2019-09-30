;;;; display-agent.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:discrete-output-machine)

(defparameter *default-stream* *standard-output*)
(defparameter *default-area-displ-x* 0)
(defparameter *default-area-displ-y* 0)
(defparameter *default-area-size-x* 80)
(defparameter *default-area-size-y* 24)

(defparameter *default-priority-fn* 
  (constantly nil)) ; latest added cell is at the top
(defparameter *default-blank-fn* 
  (lambda (x y)
    (declare (ignore x y))
    (values #\Space 0 0))) ; (values chr fg bg)
(defparameter *default-coord-displ-x* 0)
(defparameter *default-coord-displ-y* 0)

(cl-mas:define-agent display-agent 
    (&key (stream *default-stream*) 
          (area-displ-x *default-area-displ-x*)
          (area-displ-y *default-area-displ-y*)
          (area-size-x *default-area-size-x*)
          (area-size-y *default-area-size-y*)

          (priority-fn *default-priority-fn*)
          (blank-fn *default-blank-fn*)
          (coord-displ-x *default-coord-displ-x*)
          (coord-displ-y *default-coord-displ-y*))
    (:declarations
      ((type stream stream)
       (type (integer 0 *) area-displ-x area-displ-y)
       (type (integer 1 *) area-size-x area-size-y)
       (type (function (cell cell) boolean) priority-fn)
       (type (function (fixnum fixnum) (values character color color)) 
             blank-fn)
       (type fixnum coord-displ-x coord-displ-y))
     :body
      ())

  stream area-displ-x area-displ-y area-size-x area-size-y
  priority-fn blank-fn coord-displ-x coord-displ-y

  )
