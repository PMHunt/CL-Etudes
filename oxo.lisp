;;; spec -  play noughts and crosses against a human opponent

(defun make-board ()
  "create values for an empty board"
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
" 1 = nought, 10 = cross, 0 = empty"
  (cond
    ((equal v 1) "O")
    ((equal v 10) "X" )
    (t " ")))

(defun print-row (x y z)
  (format t "~& ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&-----------")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&-----------")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%"))

(defun make-move (player pos board)
  "set a board cell to player's symbol and return mutated board"
  (setf (nth pos board) player)
  board)

(setf *computer* 10)
(setf *opponent* 1)
