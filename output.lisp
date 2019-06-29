;;;; output.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:workers-2d)

(defmacro format-ansi-seq (stream control-string &rest args)
  `(format ,stream ,(format nil "~C[~A" #\Escape control-string) ,@args))

(defmacro initialize-output (stream)
  (alexandria:once-only (stream)
    `(progn 
       (format-ansi-seq ,stream "?1049h")
       (format-ansi-seq ,stream "?25l")
       (format-ansi-seq ,stream "2J")
       (force-output ,stream))))

(defmacro finalize-output (stream)
  (alexandria:once-only (stream)
    `(progn
       (format-ansi-seq ,stream "?1049l")
       (format-ansi-seq ,stream "?25h")
       (force-output ,stream))))

(defstruct colored-character
  (value #\Space :type character)
  (foreground 0 :type fixnum)
  (background 0 :type fixnum))

(defparameter *blank-char* (make-colored-character))
(defparameter *default-char* 
  (make-colored-character #\? :foreground 9 :background 16))

(defmacro make-coord (x y)
  `(cons ,x ,y))

(defmacro coord-x (coord)
  `(car ,coord))

(defmacro coord-y (coord)
  `(cdr ,coord))

(defmacro write-colored-character (stream coord cc)
  (alexandria:once-only (stream coord cc)
    `(when ,cc
       (format-ansi-seq ,stream "~A;~AH" 
                        (1+ (coord-y ,coord)) (1+ (coord-x ,coord)))
       (format-ansi-seq ,stream "38;5;~Am" (colored-character-foreground ,cc))
       (format-ansi-seq ,stream "48;5;~Am" (colored-character-background ,cc))
       (format ,stream "~C" (colored-character-value ,cc))
       (force-output ,stream))))

(deftype output-queue () '(cons queue queue))

(defun make-output-queue (&optional (stream *standard-output*))
  (let ((queue (cons (make-queue) (make-queue))) stop-flag)
    (values queue
            (lambda ()
              (initialize-output stream)
              (loop
                (when stop-flag (return))
                (write-colored-character stream 
                                         (queue-pop (car queue)) 
                                         (queue-pop (cdr queue))))
              (finalize-output stream)
              t)
            (lambda ()
              (setf stop-flag t)
              (queue-push (car queue))
              (queue-push (cdr queue))
              t))))

(defun output-character (output-queue coord cc)
  (queue-push (car output-queue) coord)
  (queue-push (cdr output-queue) cc)
  t)

;;;----------------------------------------------------------------------------

(defstruct output-buffer
  (fsm-table (make-fsm-table) :type fsm-table)
  (cell-occupants-table (make-hash-table :test 'equal) :type hash-table)
  (cell-priority-fn (constantly nil) :type (function (fsm fsm) boolean))
  (queue (error "Output queue must be assigned to a new output buffer")
         :type (or null output-queue)))

(defmacro output-buffer-coord-occupants (buffer coord)
  (alexandria:once-only (buffer coord)
    `(gethash ,coord (output-buffer-cell-occupants-table ,buffer))))

(defsetf output-buffer-coord-occupants (buffer coord) (new-list)
  `(setf (gethash ,coord (output-buffer-cell-occupants-table ,buffer)) 
         ,new-list))

(defmacro output-buffer-add-coord-occupant (buffer coord fsm)
  (alexandria:once-only (buffer coord fsm)
    `(push ,fsm (output-buffer-coord-occupants ,buffer ,coord))))

(defmacro output-buffer-del-coord-occupant (buffer coord fsm)
  (alexandria:once-only (buffer coord fsm)
    `(alexandria:deletef (output-buffer-coord-occupants ,buffer ,coord)
                         ,fsm :test #'eq)))

(defun find-top-occupant (buffer coord)
  (reduce (lambda (fsm1 fsm2)
            (if (funcall (output-buffer-cell-priority-fn buffer) fsm1 fsm2) 
              fsm2 fsm1))
          (output-buffer-coord-occupants buffer coord)))

(defmacro top-occupant-p (buffer coord occupant)
  `(eq (find-top-occupant ,buffer ,coord) ,occupant))

(defun output-buffer-visualize-coord (buffer coord)
  (output-character 
    (output-buffer-queue buffer) coord
    (alexandria:if-let ((fsm (find-top-occupant buffer coord)))
      (alexandria:if-let ((visual-fn (fsm-info-get fsm :visual-fn)))
        (funcall visual-fn fsm)
        *default-char*)
      *blank-char*)))

(defmacro fsm-coord (fsm)
  `(fsm-info-get ,fsm :coord))

(defun output-buffer-visualize-fsm (buffer fsm)
  (let ((coord (fsm-coord fsm)))
    (when (top-occupant-p buffer coord fsm)
      (output-character 
        (output-buffer-queue buffer) coord
        (alexandria:if-let ((visual-fn (fsm-info-get fsm :visual-fn)))
          (funcall visual-fn fsm)
          *default-char*)))))

(defmacro output-buffer-cell-fsm (buffer key)
  `(fsm-table-entry (output-buffer-fsm-table ,buffer) ,key))

(defun output-buffer-add-cell (buffer key fsm coord)
  (setf (fsm-coord fsm) coord)
  (fsm-table-add-entry (output-buffer-fsm-table buffer) key fsm)
  (output-buffer-add-coord-occupant buffer coord fsm)
  (output-buffer-visualize-fsm buffer fsm)
  t)

(defun output-buffer-del-cell (buffer key)
  (let ((fsm (output-buffer-cell-fsm buffer key)))
    (when fsm
      (let ((coord (fsm-coord fsm)))
        (output-buffer-del-coord-occupant buffer coord fsm)
        (output-buffer-visualize-coord buffer coord))
      (fsm-table-del-entry (output-buffer-fsm-table buffer) key)
      (fsm-info-del fsm :coord)
      t)))

(defsetf output-buffer-cell-fsm (buffer key) (new-fsm)
  `(alexandria:if-let ((fsm (output-buffer-cell-fsm ,buffer ,key)))
     (let ((coord (fsm-coord fsm)))
       (output-buffer-del-coord-occupant ,buffer coord fsm)
       (fsm-info-del fsm :coord)
       (setf (fsm-coord ,new-fsm) coord)
       (setf (fsm-table-entry (output-buffer-fsm-table ,buffer) ,key) ,new-fsm)
       (output-buffer-add-coord-occupant ,buffer coord ,new-fsm)
       (output-buffer-visualize-coord ,buffer coord)
       ,new-fsm)
     (error "No cell with key ~A is in the buffer" key)))

(defun output-buffer-move-cell (buffer key new-coord)
  (let ((fsm (output-buffer-cell-fsm buffer key)))
    (when fsm
      (let ((coord (fsm-coord fsm)))
        (when (equal coord new-coord)
          (return-from output-buffer-move-cell t))
        (output-buffer-del-coord-occupant buffer coord fsm)
        (output-buffer-visualize-coord buffer coord))
      (setf (fsm-info-get fsm :coord) new-coord)
      (output-buffer-add-coord-occupant buffer coord fsm)
      (output-buffer-visualize-fsm buffer fsm)
      t)))

(defun advance-output-buffer (buffer)
  (advance-fsm-table (output-buffer-fsm-table buffer)
    (when (fsm-state-changed-p fsm)
      (output-buffer-visualize-fsm buffer fsm))))

