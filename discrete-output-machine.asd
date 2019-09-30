;;;; discrete-output-machine.asd
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(asdf:defsystem #:discrete-output-machine
  :description "Facilities to make creation of 2D text visualization easy."
  :author "Ivan Podmazov"
  :license  "MIT"
  :version "1.0.0"
  :depends-on (#:alexandria #:cl-multiagent-system)
  :serial t
  :components ((:file "package")
               (:file "terminal-io" :depends-on ("package"))
               (:file "cell" :depends-on ("package"))
               (:file "display-agent" :depends-on ("cell" "terminal-io"))))

