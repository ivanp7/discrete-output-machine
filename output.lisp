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

(deftype coord () '(cons fixnum fixnum))

(defmacro make-coord (x y)
  `(cons ,x ,y))

(defmacro coord-x (coord)
  `(car ,coord))

(defmacro coord-y (coord)
  `(cdr ,coord))

(defun write-colored-character (stream coord cchar)
  (when (and coord cchar (>= (coord-x coord) 0) (>= (coord-y coord) 0))
    (format-ansi-seq stream "~A;~AH" 
                     (1+ (coord-y coord)) (1+ (coord-x coord)))
    (format-ansi-seq stream "38;5;~Am" (colored-character-foreground cchar))
    (format-ansi-seq stream "48;5;~Am" (colored-character-background cchar))
    (format stream "~C" (colored-character-value cchar))
    (force-output stream)))

;;;----------------------------------------------------------------------------

(defstruct output-queue
  (screen-size (make-coord 80 24) :type coord)
  (ss-lock (bt:make-lock) :type bt:lock)
  (stream *standard-output* :type stream)
  (coords (make-queue) :type queue)
  (cchars (make-queue) :type queue)
  (lock (bt:make-lock) :type bt:lock)
  (stop-flag t :type boolean))

(defun output-queue-get-screen-size (output-queue)
  (bt:with-lock-held ((output-queue-ss-lock output-queue))
    (output-queue-screen-size output-queue)))

(defun output-queue-set-screen-size (output-queue new-size)
  (bt:with-lock-held ((output-queue-ss-lock output-queue))
    (setf (output-queue-screen-size output-queue) new-size)))

(defun output-queue-loop (output-queue)
  (when (output-queue-stop-flag output-queue)
    (setf (output-queue-stop-flag output-queue) nil)
    (let ((stream (output-queue-stream output-queue)))
      (initialize-output stream)
      (loop
        (when (output-queue-stop-flag output-queue) 
          (return))
        (let ((coord (queue-pop (output-queue-coords output-queue)))
              (cchar (queue-pop (output-queue-cchars output-queue)))
              (screen-size (output-queue-get-screen-size output-queue))) 
          (when (and (< (coord-x coord) (coord-x screen-size))
                     (< (coord-y coord) (coord-y screen-size))) 
            (write-colored-character stream coord cchar))))
      (finalize-output stream)
      t)))

(defun output-queue-stop-loop (output-queue)
  (setf (output-queue-stop-flag output-queue) t)
  (queue-push (output-queue-coords output-queue)) 
  (queue-push (output-queue-cchars output-queue))
  t)

(defun output-queue-push-position (output-queue coord)
  (queue-push (output-queue-coords output-queue) coord) 
  t)

(defun output-queue-push-character (output-queue cchar)
  (queue-push (output-queue-cchars output-queue) cchar) 
  t)

(defun output-queue-push-positioned-character (output-queue coord cchar)
  (bt:with-lock-held ((output-queue-lock output-queue))
    (queue-push (output-queue-coords output-queue) coord) 
    (queue-push (output-queue-cchars output-queue) cchar))
  t)

;;;----------------------------------------------------------------------------

(defstruct cell-occupants
  (list nil :type list)
  (lock (bt:make-lock) :type bt:lock))

(defun cell-occupants-add (occupants fsm)
  (bt:with-lock-held ((cell-occupants-lock occupants))
    (push fsm (cell-occupants-list occupants))))

(defun cell-occupants-del (occupants fsm)
  (bt:with-lock-held ((cell-occupants-lock occupants))
    (alexandria:deletef (cell-occupants-list occupants) fsm :test #'eq)))

(defun cell-occupants-find-top (occupants priority-fn)
  (bt:with-lock-held ((cell-occupants-lock occupants))
    (reduce (lambda (fsm1 fsm2)
              (if (funcall priority-fn fsm1 fsm2) fsm2 fsm1))
            (cell-occupants-list occupants))))

(defun cell-occupants-do-for-each (occupants fn)
  (bt:with-lock-held ((cell-occupants-lock occupants))
    (dolist (fsm (cell-occupants-list occupants))
      (funcall fn fsm))))

;;;----------------------------------------------------------------------------

(defstruct output-buffer
  (key-table (make-fsm-table) :type fsm-table)
  (coord-table (make-hash-table :test 'equal) :type hash-table)
  (coord-table-lock (bt:make-lock) :type bt:lock)
  (fsm-priority-fn (constantly nil) :type (function (fsm fsm) boolean))
  (queue (make-output-queue) :type output-queue))

(defun initialize-cell-fsm (fsm initial-coord visual-fn)
  (fsm-info-add fsm :coord initial-coord)
  (fsm-indo-add fsm :new-coord nil)
  (fsm-info-add fsm :visual-fn visual-fn)
  t)

(defun finalize-cell-fsm (fsm)
  (fsm-info-del fsm :coord)
  (fsm-info-del fsm :new-coord)
  (fsm-info-del fsm :visual-fn)
  t)

(defmacro fsm-coord (fsm)
  `(fsm-info-get ,fsm :coord))

(defmacro fsm-new-coord (fsm)
  `(fsm-info-get ,fsm :new-coord))

(defmacro fsm-visual-fn (fsm)
  `(fsm-info-get ,fsm :visual-fn))

(defun output-buffer-cell-occupants (buffer coord)
  (bt:with-lock-held ((output-buffer-coord-table-lock buffer))
    (alexandria:ensure-gethash coord (output-buffer-coord-table buffer)
                               (make-cell-occupants))))

(defmacro output-buffer-top-cell-occupant (buffer coord)
  (alexandria:once-only (buffer) 
    `(cell-occupants-find-top (output-buffer-cell-occupants ,buffer ,coord)
                              (output-buffer-fsm-priority-fn ,buffer))))

(defmacro output-buffer-do-for-each-cell-occupant (buffer coord fn)
  `(cell-occupants-do-for-each (output-buffer-cell-occupants ,buffer ,coord)
                               ,fn))

(defun output-buffer-visualize-coord (buffer coord)
  (output-queue-push-positioned-character
    (output-buffer-queue buffer) coord
    (alexandria:if-let ((fsm (output-buffer-top-cell-occupant buffer coord)))
      (alexandria:if-let ((visual-fn (fsm-visual-fn fsm)))
        (funcall visual-fn fsm)
        *default-char*)
      *blank-char*)))

(defun output-buffer-visualize-fsm (buffer fsm)
  (let ((coord (fsm-coord fsm)))
    (when (eq fsm (output-buffer-top-cell-occupant buffer coord))
      (output-queue-push-positioned-character
        (output-buffer-queue buffer) coord
        (alexandria:if-let ((visual-fn (fsm-visual-fn fsm)))
          (funcall visual-fn fsm)
          *default-char*)))))

(defmacro output-buffer-cell (buffer key)
  `(fsm-table-entry (output-buffer-key-table ,buffer) ,key))

(defun output-buffer-add-cell (buffer fsm)
  (fsm-table-add-entry (output-buffer-key-table buffer) fsm)
  t)

(defun output-buffer-del-cell (buffer key)
  (alexandria:if-let ((fsm (output-buffer-cell buffer key)))
    (progn
      (fsm-table-del-entry (output-buffer-key-table buffer) key)
      t)))

(defun output-buffer-cell-coord (buffer key)
  (fsm-coord (output-buffer-cell buffer key)))

(defun output-buffer-move-cell (buffer key new-coord)
  (alexandria:if-let ((fsm (output-buffer-cell buffer key)))
    (let ((coord (fsm-coord fsm)))
      (unless (equal coord new-coord)
        (setf (fsm-new-coord fsm) new-coord)))))

(defun advance-output-buffer (buffer)
  (advance-fsm-table 
    (output-buffer-key-table buffer)
    :del-fn (lambda (key)
              (alexandria:if-let ((fsm (output-buffer-cell buffer key)))
                (let ((coord (fsm-coord fsm)))
                  (cell-occupants-del 
                    (output-buffer-cell-occupants buffer coord) fsm)
                  (output-buffer-visualize-coord buffer coord))))
    :add-fn (lambda (fsm)
              (cell-occupants-add
                (output-buffer-cell-occupants buffer (fsm-coord fsm)) fsm)
              (output-buffer-visualize-fsm buffer fsm))
    :advance-fn (lambda (key fsm)
                  (let (update-needed (coord (fsm-coord fsm))) 
                    (when (fsm-new-coord fsm)
                      (setf (fsm-coord fsm) (fsm-new-coord fsm)
                            (fsm-new-coord fsm) nil
                            update-needed t)
                      (output-buffer-visualize-coord buffer coord))
                    (when (fsm-state-changed-p fsm)
                      (setf update-needed t))
                    (when update-needed
                      (output-buffer-visualize-fsm buffer fsm))))))

