;;;; fsm.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:workers-2d)

(deftype fsm () `(function (keyword &optional *) *))

(defmacro make-finite-state-machine ((initial-state &key info 
                                                    (state-type 'keyword))
                                     &body state-transfer)
  `(let ((state ,initial-state) next-state old-state (info ,info))
     (declare (type ,state-type state next-state old-state))
     (lambda (op &optional input-data)
       (declare (type keyword op))
       (ecase op
         (:get state)
         (:set (setf next-state (the ,state-type input-data)))
         (:calc
           (let ((info info))
             ,@state-transfer)
           next-state)
         (:advance
           (setf old-state state 
                 state next-state 
                 next-state nil)
           t)
         (:state-changed-p (not (null next-state)))
         (:info info)
         (:set-info 
           (setf info input-data)
           t)))))

(defmacro fsm-state (fsm)
  `(funcall ,fsm :get))

(defsetf fsm-state (fsm) (new-state)
  `(funcall ,fsm :set ,new-state))

(defmacro calc-next-fsm-state (fsm &optional input-data)
  `(funcall ,fsm :calc ,input-data))

(defmacro advance-fsm (fsm)
  `(funcall ,fsm :advance))

(defmacro fsm-state-changed-p (fsm)
  `(funcall ,fsm :state-changed-p))

(defmacro fsm-info (fsm)
  `(funcall ,fsm :info))

(defsetf fsm-info (fsm) (new-info)
  `(funcall ,fsm :set-info ,new-info))

(defun fsm-info-add (fsm key value)
  (push (cons key value) (fsm-info fsm))
  value)

(defun fsm-info-del (fsm key)
  (if (eql key (car (first (fsm-info fsm))))
    (setf (fsm-info fsm) (rest (fsm-info fsm)))
    (loop :for rst :on (fsm-info fsm) :do
          (when (eql key (car (second rst)))
            (setf (rest rst) (rest (rest rst)))
            (return)))))

(defun fsm-info-get (fsm key &optional default)
  (alexandria:if-let ((c (assoc key (fsm-info fsm))))
    (cdr c)
    default))

(defsetf fsm-info-get (fsm key) (new-value)
  `(alexandria:if-let ((c (assoc ,key (fsm-info ,fsm)))) 
     (setf (cdr c) ,new-value)
     (fsm-info-add ,fsm ,key ,new-value)))

;;;----------------------------------------------------------------------------

(defmacro make-fsm-table ()
  `(make-hash-table :test 'equal))

(defmacro fsm-table-entry (table key)
  (alexandria:once-only (table key)
    `(gethash ,key ,table)))

(defsetf fsm-table-entry (table key) (new-fsm)
  `(setf (gethash ,key ,table) ,new-fsm))

(defmacro fsm-table-add-entry (table key fsm)
  `(setf (fsm-table-entry ,table ,key) ,fsm))

(defmacro fsm-table-del-entry (table key)
  (alexandria:once-only (table key)
    `(remhash ,key ,table)))

(defmacro advance-fsm-table (table &body post-advance-forms)
  (alexandria:once-only (table)
    `(maphash (lambda (key fsm)
                (declare (ignorable key))
                (advance-fsm fsm)
                ,@post-advance-forms)
              ,table)))

