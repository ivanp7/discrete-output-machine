;;;; queue.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:workers-2d)

(defstruct queue
  (list nil :type list)
  (last nil :type list)
  (lock (bt:make-lock) :type bt:lock)
  (condition-variable (bt:make-condition-variable)))

(defmacro queue-head (queue)
  `(car (queue-list ,queue)))

(defmacro queue-tail (queue)
  `(car (queue-last ,queue)))

(defun queue-empty-p (queue)
  (null (queue-list queue)))

(defun queue-notify (queue)
  (bt:condition-notify (queue-condition-variable queue)))

(defun queue-push (queue element &rest more-elements)
  (bt:with-lock-held ((queue-lock queue))
    (let ((new-last (cons element nil)))
      (if (queue-empty-p queue)
        (setf (queue-head queue) new-last
              (queue-last queue) new-last)
        (setf (cdr (queue-last queue)) new-last
              (queue-last queue) new-last))
      (when more-elements
        (setf (cdr (queue-last queue)) more-elements 
              (queue-last queue) (last more-elements))))
    (queue-notify queue)
    (queue-tail queue)))

(defun queue-pop (queue &key (wait-if-empty t) value-if-empty)
  (bt:with-lock-held ((queue-lock queue))
    (when (and (queue-empty-p queue) wait-if-empty)
      (bt:condition-wait (queue-condition-variable queue)
                         (queue-lock queue)))
    (if (queue-empty-p queue)
      value-if-empty
      (let ((value (queue-head queue)))
        (setf (queue-list queue) (cdr (queue-list queue)))
        (when (queue-empty-p queue)
          (setf (queue-last queue) nil))
        value))))

