;;;; server.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:workers-2d)

(defconstant +local-host+ "localhost")

(defun start-output-server (port &optional (stream *standard-output*))
  (multiple-value-bind (queue loop-fn stop-fn) (make-output-queue stream)
    (let ((server-thread 
            (usocket:socket-server
              +local-host+ port
              (lambda (stream)
                (declare (type stream stream))
                (let ((event-fn (compile nil (read stream))))
                  ))
              :in-new-thread t 
              :element-type 'character
              :multi-threading t))
          (display-loop-thread (bt:make-thread loop-fn :name "Display loop")))
      (lambda ()
        (funcall stop-fn)
        (bt:destroy-thread server-thread)))))

