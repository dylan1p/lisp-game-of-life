
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "lispbuilder-sdl"))

(defconstant window-height 600)
(defconstant window-width 800)
(defconstant board-width 100)
(defconstant board-height 100)
(defvar cell-size 10)

(defvar *board*
  (make-array (list board-width board-height) :initial-element nil))

;; Logic

(defun step-board ()
  "Compute the next board according to the game of life rules"
  ;; write me!

  )

(defun empty-board ()
  (dotimes (x board-width)
    (dotimes (y board-height)
      (setf (aref *board* x y) nil))))

(defun random-board ()
  "Generate a random board"
  (dotimes (x board-width)
    (dotimes (y board-height)
      (setf (aref *board* x y)
            (if (= (random 2) 1)
                t
                nil)))))

;; Graphical
(defun draw-cell (x y)
  (sdl:draw-box-* (* cell-size x) (* cell-size y) cell-size cell-size :color sdl:*green*))

(defun draw ()
  (sdl:clear-display sdl:*black*)
  (dotimes (x board-width)
    (dotimes (y board-height)
      (when (aref *board* x y)
        (draw-cell x y)))))

(defun run-and-wait ()
  "Start the game"
  (sdl:with-init ()
    (sdl:window window-width window-height)
    (setf (sdl:frame-rate) 1)
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle () (step-board) (draw) (sdl:update-display))
      (:video-expose-event () (sdl:update-display)))))


(defun run ()
  ;; Star the game in a new thread, so we have the REPL available for us.
  (sb-thread:make-thread #'run-and-wait :name "game loop"))
