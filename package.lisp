;;;; package.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(defpackage #:discrete-output-machine
  (:use #:cl)
  (:nicknames #:dom)
  (:export :format/ansi-sequence :hide-cursor :show-cursor
           :enable-alternative-screen :disable-alternative-screen
           :clear-screen :go-to :set-color :put-character
           :*default-cell-data* :*default-cell-x* :*default-cell-y* 
           :*default-cell-chr* :*default-cell-fg* :*default-cell-bg* 
           :*default-cell-visibility* :*default-cell-buffer*
           :cell :make-cell :cell-id :cell-data 
           :cell-x :cell-y :cell-chr :cell-fg :cell-bg 
           :cell-visibility :cell-buffer
           :cell-acquire-lock :cell-release-lock :with-cell-lock-held
           :*default-multicell-data-fn* 
           :*default-multicell-size-x* :*default-multicell-size-y*
           :*default-multicell-x* :*default-multicell-y*
           :*default-multicell-visibility*
           :multicell :make-multicell :multicell-id 
           :multicell-size-x :multicell-size-y :multicell-x :multicell-y 
           :multicell-visibility :multicell-buffer :multicell-data 
           :multicell-chr :multicell-fg :multicell-bg 
           :multicell-acquire-cells-locks :multicell-release-cells-locks
           :with-multicell-cells-locks-held
           :make-position :position-x :position-y
           :*default-buffer-stream* :*default-buffer-cell-priority-fn* 
           :*default-buffer-blank-fn* 
           :*default-buffer-displ-x* :*default-buffer-displ-y*
           :*default-buffer-screen-displ-x* :*default-buffer-screen-displ-y*
           :*default-buffer-screen-size-x* :*default-buffer-screen-size-y*
           :buffer :make-buffer :buffer-stream :buffer-cell-priority-fn 
           :buffer-blank-fn :buffer-displ-x :buffer-displ-y 
           :buffer-screen-displ-x :buffer-screen-displ-y 
           :buffer-screen-size-x :buffer-screen-size-y 
           :buffer-cells :buffer-refresh :buffer-redraw
           :buffer-acquire-lock :buffer-release-lock :with-buffer-lock-held))

