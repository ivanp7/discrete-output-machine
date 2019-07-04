;;;; package.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(defpackage #:discrete-output-machine
  (:use #:cl)
  (:nicknames #:dom)
  (:export :queue :make-queue queue-empty-value queue-head queue-tail
           :queue-empty-p :queue-push :queue-pop :consume-queue
           :fsm :make-finite-state-machine :fsm-state :calc-next-fsm-state
           :advance-fsm :fsm-state-changed-p :fsm-info :fsm-info-add
           :fsm-info-del :fsm-info-get :fsm-key 
           :fsm-table :make-fsm-table :fsm-table-add-entry 
           :fsm-table-del-entry :fsm-table-entry :advance-fsm-table
           :colored-character :make-colored-character :colored-character-value
           :colored-character-foreground :colored-character-background
           :*blank-char* :*default-char* 
           :coord :make-coord :coord-x :coord-y
           :output-queue :make-output-queue :output-queue-screen-size
           :output-queue-loop :output-queue-stop-loop 
           :output-queue-push-position :output-queue-push-character
           :output-queue-push-positioned-character
           :output-buffer :make-output-buffer output-buffer-fsm-priority-fn
           :initialize-cell-fsm :finalize-cell-fsm :fsm-visual-fn
           :output-buffer-top-cell-occupant 
           :output-buffer-do-for-each-cell-occupant 
           :output-buffer-cell :output-buffer-add-cell :output-buffer-del-cell
           :output-buffer-cell-coord :advance-output-buffer))

