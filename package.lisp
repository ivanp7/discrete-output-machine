;;;; package.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(defpackage #:discrete-output-machine
  (:use #:cl)
  (:nicknames #:dom)
  (:export :format/ansi-sequence :format/initialize :format/finalize
           :format/position+color :format/position+color+character
           :*default-cell-x* :*default-cell-y* 
           :*default-cell-chr* :*default-cell-fg* :*default-cell-bg* 
           :*default-cell-visibility* :*default-cell-buffer*
           :make-cell :cell-id :cell-metadata 
           :cell-x :cell-y :cell-chr :cell-fg :cell-bg 
           :cell-visibility :cell-buffer
           :*default-buffer-stream* :*default-buffer-cell-priority-fn* 
           :*default-buffer-blank-fn* 
           :*default-buffer-displ-x* :*default-buffer-displ-y*
           :*default-buffer-size-x* :*default-buffer-size-y*
           :make-buffer :buffer-stream :buffer-cell-priority-fn 
           :buffer-blank-fn
           :buffer-displ-x :buffer-displ-y :buffer-size-x :buffer-size-y
           :buffer-cells :buffer-refresh :buffer-redraw))

