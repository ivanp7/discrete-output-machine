;;;; output.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:workers-2d)

(defparameter *stream* *standard-output*)

(defmacro force-write (control-string &rest args)
  `(progn
     (format *stream* ,control-string ,@args)
     (force-output *stream*)))

(defmacro force-write-ansi-seq (control-string &rest args)
  `(force-write ,(format nil "~C[~A" #\Escape control-string) ,@args))

(defun initialize-output ()
  (force-write-ansi-seq "?1049h")
  (force-write-ansi-seq "?25l")
  (force-write-ansi-seq "2J"))

(defun finalize-output ()
  (force-write-ansi-seq "?1049l")
  (force-write-ansi-seq "?25h"))

(defstruct buffer-character
  (itself #\Space :type character)
  (foreground 0 :type fixnum)
  (background 0 :type fixnum))

(defstruct buffer-cell
  (character (make-buffer-character) :type buffer-character)
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defun write-cell (cell)
  (force-write-ansi-seq "~A;~AH" 
                        (1+ (buffer-cell-y cell)) (1+ (buffer-cell-x cell)))
  (force-write-ansi-seq "38;5;~Am" (buffer-character-foreground 
                                     (buffer-cell-character cell)))
  (force-write-ansi-seq "48;5;~Am" (buffer-character-background 
                                     (buffer-cell-character cell)))
  (force-write "~C" (buffer-character-itself 
                      (buffer-cell-character cell))))

(defparameter *output-stop-flag* nil)
(defparameter *buffer-queue* (make-queue))

(defun output-queue ()
  (initialize-output)
  (loop
    (when *output-stop-flag* (return))
    (write-cell (queue-pop *buffer-queue*)))
  (finalize-output))

