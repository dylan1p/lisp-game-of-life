
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

(defun initial-board ()
  (empty-board)
  (setf (aref *board* 69 10) t)
  (setf (aref *board* 70 10) t)
  (setf (aref *board* 70 11) t)
  (setf (aref *board* 71 11) t)
  (setf (aref *board* 72 10) t)
  (setf (aref *board* 73 10) t))

(defun glider ()
  (empty-board)
  (setf (aref *board* 10 10) t
        (aref *board* 10 11) t
        (aref *board* 10 12) t
        (aref *board* 11 12) t
        (aref *board* 12 11) t))

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

(defun check-out-of-bounds (x y)
  "Check if a cell is out of bounds"
  (not (and (<= 0 x (1- board-width))
            (<= 0 y (1- board-height)))))

(defun has-left-neighbour (x y)
  "Check if cell has left neighbour"
  (if (not (check-out-of-bounds  (1- x) y))
      (aref *board* (1- x) y)))

(defun has-right-neighbour (x y)
  "Check if cell has right neighbour"
  (if (not (check-out-of-bounds  (1+ x) y))
      (aref *board* (1+ x) y)))

(defun has-top-neighbour (x y)
  "Check if cell has top neighbour"
  (if (not (check-out-of-bounds  x (1+ y)))
      (aref *board* x (1+ y))))

(defun has-bottom-neighbour (x y)
  "Check if cell has bottom neighbour"
  (if (not (check-out-of-bounds  x (1- y)))
      (aref *board* x (1- y))))

(defun number-of-neighbours (x y)
  "Will return the total number of active bordering cells."
  (let ((counter 0))
    (cond ((aref *board* x y)
           (when (has-left-neighbour x y) (setq counter(+ counter 1)))
           (when (has-right-neighbour x y) (setq counter(+ counter 1)))
           (when (has-top-neighbour x y) (setq counter(+ counter 1)))
           (when (has-bottom-neighbour x y) (setq counter(+ counter 1)))
           ))
    counter))
;; Rule 1 Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;; Rule 2 Any live cell with two or three live neighbours lives on to the next generation.
;; Rule 3 Any live cell with more than three live neighbours dies, as if by over-population.
;; Rule 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

(defun dead-or-alive (x y)
  "Use rules to set state of cells"
  (let ((alive (aref *board* x y)))
    (if (< (number-of-neighbours x y) 2) (setq alive nil))
    (if (> (number-of-neighbours x y) 3) (setq alive nil))
    (if (= (number-of-neighbours x y) 2) (setq alive t))
    (if (= (number-of-neighbours x y) 3) (setq alive t))
    alive))

(defun next-board ()
  "Generate next board"
  (dotimes (x board-width)
    (dotimes (y board-height)
      (setf (aref *board* x y) 
            (dead-or-alive x y)
  ))))


;;   (when ((< (number-of-neighbours x y) 2) nil)
;; )
    ;; ((> (number-of-neighbours x y) 3) (setf (aref *board* x y) nil))
    ;; ((= (number-of-neighbours x y) 3) (setf (aref *board* x y) nil))
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
      (initial-board)
      (:quit-event () t)
      (:idle () (next-board) (draw) (sdl:update-display))
      (:video-expose-event () (sdl:update-display)))))


(defun run ()
  ;; Star the game in a new thread, so we have the REPL available for us.
  (initial-board)
  (sb-thread:make-thread #'run-and-wait :name "game loop"))
