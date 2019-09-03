;;;; multicell.lisp

(in-package #:discrete-output-machine)

(defparameter *default-multicell-data-fn* (constantly nil))
(defparameter *default-multicell-size-x* 2)
(defparameter *default-multicell-size-y* 1)
(defparameter *default-multicell-x* 0)
(defparameter *default-multicell-y* 0)
(defparameter *default-multicell-visibility* nil)

(es:define-structure multicell
  :parameters (id &key (data-fn *default-multicell-data-fn*)
                  (size-x *default-multicell-size-x*) 
                  (size-y *default-multicell-size-y*)
                  (x *default-multicell-x*) (y *default-multicell-y*)
                  (visibility *default-multicell-visibility*)
                  (buffer *default-cell-buffer*))
  :body-macros ((do-cells (&body body)
                  `(dotimes (i size-y)
                     (dotimes (j size-x)
                       (symbol-macrolet ((cell (aref cell-array j i)))
                         ,@body)))))
  :bindings ((cell-array (make-array `(,size-y ,size-x)
                                     :element-type '(or null cell)
                                     :initial-element nil)))
  :init-forms ((do-cells
                 (setf cell (make-cell (list* 'multicell id j i) 
                                       :data (funcall data-fn j i)
                                       :x (+ x j) :y (+ y i)
                                       :visibility visibility
                                       :buffer buffer))))
  :getters (id size-x size-y x y visibility buffer
            (data (lx ly)
              (cell-data (aref cell-array ly lx)))
            (chr (lx ly)
              (cell-chr (aref cell-array ly lx)))
            (fg (lx ly)
              (cell-fg (aref cell-array ly lx)))
            (bg (lx ly)
              (cell-bg (aref cell-array ly lx)))
            (acquire-cells-locks ()
              (do-cells
                (cell-acquire-lock cell))
              t)
            (release-cells-locks ()
              (do-cells
                (cell-release-lock cell))
              t))
  :setters ((x ()
              (prog1 (setf x es:value)
                (do-cells
                  (setf (cell-x cell) (+ x j)))))
            (y ()
              (prog1 (setf y es:value)
                (do-cells
                  (setf (cell-y cell) (+ y i)))))
            (visibility ()
              (prog1 (setf visibility es:value)
                (do-cells
                  (setf (cell-visibility cell) visibility))))
            (buffer ()
              (prog1 (setf buffer es:value)
                (do-cells
                  (setf (cell-buffer cell) buffer))))
            (data (lx ly)
              (setf (cell-data (aref cell-array ly lx)) es:value))
            (chr (lx ly)
              (setf (cell-chr (aref cell-array ly lx)) es:value))
            (fg (lx ly)
              (setf (cell-fg (aref cell-array ly lx)) es:value))
            (bg (lx ly)
              (setf (cell-bg (aref cell-array ly lx)) es:value))))

(defmacro with-multicell-cells-locks-held ((multicell) &body body)
  (alexandria:once-only (multicell)
    `(progn
       (multicell-acquire-cells-locks ,multicell)
       ,@body
       (multicell-release-cells-locks ,multicell))))

