;;;; fsm.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:workers-2d)

(deftype fsm () `(function (keyword &optional *) *))

(defmacro make-finite-state-machine ((initial-state &key info 
                                                    (state-type 'keyword))
                                     &body state-transfer)
  `(let ((state ,initial-state) next-state old-state (info ,info))
     (declare (type ,state-type state)
              (type (or null ,state-type) next-state old-state))
     (lambda (op &optional input-data)
       (declare (type keyword op))
       (ecase op
         (:get state)
         (:set (setf next-state (the (or null ,state-type) input-data)))
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
  key)

(defun fsm-info-del (fsm key)
  (alexandria:deletef (fsm-info fsm) key :key #'car :test #'eql)
  key)

(defun fsm-info-get (fsm key &optional default)
  (alexandria:if-let ((c (assoc key (fsm-info fsm))))
    (cdr c)
    default))

(defsetf fsm-info-get (fsm key &optional default) (new-value)
  `(locally
     (declare (ignore ,default))
     (alexandria:if-let ((c (assoc ,key (fsm-info ,fsm)))) 
       (setf (cdr c) ,new-value)
       (progn
         (fsm-info-add ,fsm ,key ,new-value)
         ,new-value))))

;;;----------------------------------------------------------------------------

(defstruct fsm-table
  (itself (make-hash-table :test 'equal) :type hash-table)
  (addition-queue (make-queue) :type queue)
  (deletion-queue (make-queue) :type queue))

(defmacro fsm-key (fsm)
  `(fsm-info-get ,fsm :table-key (error "No table key assigned to a FSM")))

(defmacro fsm-table-add-entry-now (table fsm)
  (alexandria:once-only (table fsm)
    `(setf (gethash (fsm-key ,fsm) (fsm-table-itself ,table)) ,fsm)))

(defmacro fsm-table-del-entry-now (table key)
  (alexandria:once-only (table key)
    `(remhash ,key (fsm-table-itself ,table))))

(defun fsm-table-add-entry (table fsm)
  (queue-push (fsm-table-addition-queue table) fsm))

(defun fsm-table-del-entry (table key)
  (queue-push (fsm-table-deletion-queue table) key))

(defmacro fsm-table-entry (table key)
  (alexandria:once-only (table key)
    `(gethash ,key (fsm-table-itself ,table))))

(defsetf fsm-table-entry (table key) (new-fsm)
  `(progn
     (fsm-table-del-entry ,table ,key)
     (fsm-table-add-entry ,table ,new-fsm)))

(defun advance-fsm-table (table &key (del-fn (constantly nil))
                                (add-fn (constantly nil))
                                (advance-fn (constantly nil)))
  (consume-queue (fsm-table-deletion-queue table)
                 (lambda (key)
                   (alexandria:if-let ((fsm (fsm-table-entry table key)))
                     (progn
                       (funcall del-fn key)
                       (fsm-table-del-entry-now table key)))))
  (consume-queue (fsm-table-addition-queue table)
                 (lambda (fsm)
                   (funcall add-fn fsm)
                   (fsm-table-add-entry-now table fsm)))
  (maphash (lambda (key fsm)
             (advance-fsm fsm)
             (funcall advance-fn key fsm))
           (fsm-table-itself table))
  t)

