;;;; discrete-output-machine.asd
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(asdf:defsystem #:discrete-output-machine
  :description "Abstraction layer to make creation of terminal apps easy."
  :author "Ivan Podmazov"
  :license  "GNU/GPLv2"
  :version "0.0.1"
  :depends-on (#:alexandria #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "terminal-io" :depends-on ("package"))
               (:file "discrete-output-machine" :depends-on ("terminal-io"))))

