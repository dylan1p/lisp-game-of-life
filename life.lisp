
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

(defun gun ()
  (setf (aref *board* 1 5) t)
  (setf (aref *board* 1 6) t)
  (setf (aref *board* 2 5) t)
  (setf (aref *board* 2 6) t)
  (setf (aref *board* 11 5) t)
  (setf (aref *board* 11 6) t)
  (setf (aref *board* 11 7) t)
  (setf (aref *board* 12 4) t)
  (setf (aref *board* 12 8) t)
  (setf (aref *board* 13 3) t)
  (setf (aref *board* 13 9) t)
  (setf (aref *board* 14 3) t)
  (setf (aref *board* 14 9) t)
  (setf (aref *board* 15 6) t)
  (setf (aref *board* 16 4) t)
  (setf (aref *board* 16 8) t)
  (setf (aref *board* 17 5) t)
  (setf (aref *board* 17 6) t)
  (setf (aref *board* 17 7) t)
  (setf (aref *board* 18 6) t)
  (setf (aref *board* 21 3) t)
  (setf (aref *board* 21 4) t)
  (setf (aref *board* 21 5) t)
  (setf (aref *board* 22 3) t)
  (setf (aref *board* 22 4) t)
  (setf (aref *board* 22 5) t)
  (setf (aref *board* 23 2) t)
  (setf (aref *board* 23 6) t)
  (setf (aref *board* 25 1) t)
  (setf (aref *board* 25 2) t)
  (setf (aref *board* 25 6) t)
  (setf (aref *board* 25 7) t)
  (setf (aref *board* 35 3) t)
  (setf (aref *board* 35 4) t)
  (setf (aref *board* 36 3) t)
  (setf (aref *board* 36 4) t)
  (setf (aref *board* 35 22) t)
  (setf (aref *board* 35 23) t)
  (setf (aref *board* 35 25) t)
  (setf (aref *board* 36 22) t)
  (setf (aref *board* 36 23) t)
  (setf (aref *board* 36 25) t)
  (setf (aref *board* 36 26) t)
  (setf (aref *board* 36 27) t)
  (setf (aref *board* 37 28) t)
  (setf (aref *board* 38 22) t)
  (setf (aref *board* 38 23) t)
  (setf (aref *board* 38 25) t)
  (setf (aref *board* 38 26) t)
  (setf (aref *board* 38 27) t)
  (setf (aref *board* 39 23) t)
  (setf (aref *board* 39 25) t)
  (setf (aref *board* 40 23) t)
  (setf (aref *board* 40 25) t)
  (setf (aref *board* 41 24) t))



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


(defun has-neighbour (x y dx dy)
  "Check if cell has neighbour"
  (if (not (check-out-of-bounds  (+ x dx) (+ y dy)))
      (aref *board* (+ x dx) (+ y dy))))


(defun number-of-neighbours (x y)
  "Will return the total number of active bordering cells."
  (loop
     for (dx dy)
     in '((-1 +1) (+0 +1) (+1 +1)
          (-1 +0)         (+1 +0)
          (-1 -1) (+0 -1) (+1 -1))
     count (has-neighbour x y dx dy)))

(defun alive (x y)
  "Use rules to set state of cells"
  (let ((alive (aref *board* x y)))
    (if alive
        (<= 2 (number-of-neighbours x y) 3)
        (= (number-of-neighbours x y) 3))))

(defun next-board ()
  "Generate next board"
  (let ((next-board (make-array (list board-width board-height) :initial-element nil)))
    (dotimes (x board-width)
      (dotimes (y board-height)
        (setf (aref next-board x y) (alive x y))))
    (setf *board* next-board)
    nil))

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
    (setf (sdl:frame-rate) 60)
    (sdl:update-display)
    (sdl:with-events ()
      (initial-board)
      (:quit-event () t)
      (:idle () (next-board) (draw) (sdl:update-display))
      (:video-expose-event () (sdl:update-display)))))


(defun run ()
  ;; Star the game in a new thread, so we have the REPL available for us.
  (glider)
  (sb-thread:make-thread #'run-and-wait :name "game loop"))
