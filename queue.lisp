;;;; queue.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:workers-2d)

(defstruct queue
  (list nil :type list)
  (last nil :type list)
  (empty-value nil)
  (lock (bt:make-lock) :type bt:lock)
  (condition-variable (bt:make-condition-variable)))

(defmacro queue-head (queue)
  `(car (queue-list ,queue)))

(defmacro queue-tail (queue)
  `(car (queue-last ,queue)))

(defun queue-empty-p (queue)
  (null (queue-list queue)))

(defun queue-push (queue &rest elements)
  (bt:with-lock-held ((queue-lock queue))
    (let ((new-last (last elements)))
      (if (queue-empty-p queue)
        (setf (queue-head queue) elements)
        (setf (rest (queue-last queue)) elements))
      (setf (queue-last queue) new-last))
    (let ((tail (queue-tail queue))) 
      (bt:condition-notify (queue-condition-variable queue))
      tail)))

(defun queue-pop (queue &key (wait t))
  (bt:with-lock-held ((queue-lock queue))
    (when (and wait (queue-empty-p queue))
      (bt:condition-wait (queue-condition-variable queue) (queue-lock queue)))
    (if (queue-empty-p queue)
      (queue-empty-value queue)
      (prog1 (pop (queue-list queue))
        (when (queue-empty-p queue)
          (setf (queue-last queue) nil))))))

