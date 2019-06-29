;;;; workers-2d.asd
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(asdf:defsystem #:workers-2d
  :description "2D workers model for multiagent system testing"
  :author "Ivan Podmazov"
  :license  "GNU/GPLv2"
  :version "0.0.1"
  :depends-on (#:alexandria #:bordeaux-threads #:usocket #:trivial-signal)
  :serial t
  :components ((:file "package")
               (:file "fsm" :depends-on ("package"))
               (:file "queue" :depends-on ("package"))
               (:file "output" :depends-on ("queue" "fsm"))))

