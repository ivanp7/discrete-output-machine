;;;; discrete-output-machine.asd
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(asdf:defsystem #:discrete-output-machine
  :description "Finite state machine table for output"
  :author "Ivan Podmazov"
  :license  "GNU/GPLv2"
  :version "0.0.1"
  :depends-on (#:alexandria #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "queue" :depends-on ("package"))
               (:file "fsm" :depends-on ("queue"))
               (:file "output" :depends-on ("queue" "fsm"))))

