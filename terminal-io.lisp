;;;; terminal-io.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:discrete-output-machine)

(defmacro format/ansi-sequence (stream control-string &rest args)
  `(format ,stream ,(format nil "~C[~A" #\Escape control-string) ,@args))

(defun format/initialize (stream)
  (format/ansi-sequence "?1049h")
  (format/ansi-sequence "?25l")
  (format/ansi-sequence "2J"))

(defun format/finalize (stream)
  (format/ansi-sequence "2J")
  (format/ansi-sequence "?25h")
  (format/ansi-sequence "?1049l"))

(defun format/position+color (stream x y fg bg)
  (when (and (>= x 0) (>= y 0) (>= fg 0) (< fg 256) (>= bg 0) (< bg 256))
    (format/ansi-sequence stream "~A;~AH" (1+ y) (1+ x))
    (format/ansi-sequence stream "38;5;~Am" fg)
    (format/ansi-sequence stream "48;5;~Am" bg)))

(defun format/position+color+character (stream x y fg bg chr)
  (format/position+color stream x y fg bg)
  (format stream "~C" chr))

