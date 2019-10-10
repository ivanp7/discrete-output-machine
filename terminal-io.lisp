;;;; terminal-io.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:discrete-output-machine)

(defmacro format/ansi-sequence (stream control-string &rest args)
  `(format ,stream ,(format nil "~C[~A" #\Escape control-string) ,@args))

(defun hide-cursor (stream)
  (format/ansi-sequence stream "?25l"))

(defun show-cursor (stream)
  (format/ansi-sequence stream "?25h"))

(defun enable-alternative-screen (stream)
  (format/ansi-sequence stream "?1049h"))

(defun disable-alternative-screen (stream)
  (format/ansi-sequence stream "?1049l"))

(defun clear-screen (stream)
  (format/ansi-sequence stream "2J"))

(defun go-to (stream x y)
  (when (and (>= x 0) (>= y 0))
    (format/ansi-sequence stream "~A;~AH" (1+ y) (1+ x))))

(defun set-color (stream fg bg)
  (cond
    ((and fg bg)
     (format/ansi-sequence stream "38;5;~Am" fg)
     (format/ansi-sequence stream "48;5;~Am" bg))
    (fg
     (format/ansi-sequence stream "0")
     (format/ansi-sequence stream "38;5;~Am" fg))
    (bg
     (format/ansi-sequence stream "0")
     (format/ansi-sequence stream "48;5;~Am" bg))
    (t
     (format/ansi-sequence stream "0"))))

(defun put-character (stream x y fg bg chr)
  (go-to stream x y)
  (set-color stream fg bg)
  (format stream "~C" chr))

