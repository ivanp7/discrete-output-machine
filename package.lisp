;;;; package.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(defpackage #:discrete-output-machine
  (:use #:cl)
  (:nicknames #:dom)
  (:export :format/ansi-sequence :hide-cursor :show-cursor
           :enable-alternative-screen :disable-alternative-screen
           :clear-screen :go-to :set-color :put-character

           :coord :make-coord :coord-x :coord-y :coord-new-x :coord-new-y
           :coord-setv :coord-changed-p :coord-trigger :coord-owned-p

           :color :*default-chr* :*default-fg* :*default-bg*

           :colchar :make-colchar :colchar-chr :colchar-fg :colchar-bg
           :colchar-new-chr :colchar-new-fg :colchar-new-bg
           :colchar-setv :colchar-changed-p :colchar-trigger :colchar-owned-p

           :*default-visibility* :*default-buffer*

           :cell :make-cell :cell-role :cell-coord :cell-colchar 
           :cell-visibility :cell-buffer :cell-changed-p :cell-trigger

           :*default-stream* :*default-stream-lock* 
           :*default-area-pos-x* :*default-area-pos-y*
           :*default-area-size-x* :*default-area-size-y*
           :*default-priority-fn* :*default-blank-fn* 
           :*default-displ-x* :*default-displ-y*

           :buffer :make-buffer :buffer-stream 
           :buffer-area-pos-x :buffer-area-pos-y 
           :buffer-area-size-x :buffer-area-size-y :buffer-set-area
           :buffer-priority-fn :buffer-blank-fn 
           :buffer-displ-x :buffer-displ-y :buffer-set-displ
           :buffer-number-of-cells :buffer-map-cells :buffer-redraw))

