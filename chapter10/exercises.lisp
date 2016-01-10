(defvar *total-glasses*)
(setf *total-glasses* 0)

(defun sell2 (n)
  "Ye Olde Lemonade Stand: Sales by the Glass."
  (incf *total-glasses* n)
  (format t "~&That make ~S glasses so far today" *total-glasses*))


(defvar *friends*)
(defvar *met-more*)
(setf *friends* nil)
(setf *met-more* 0)

(defun meet2 (person)
  (cond ((equal person (car *friends*))
	 (incf *met-more* 1)
	 'we-just-met)
	((member person *friends*)
	 (incf *met-more* 1)
	 'we-know-each-other)
	(t (push person *friends*)
	   'pleased-to-meet-you)))

(defun forget (person)
  (cond ((equal person (car *friends*))
	 (pop *friends*))
	(t 'person-is-not-a-friend)))

;; Tic-tac-toe

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  (cond ((equal v 1) "0")
	((equal v 10) "X")
	(t " ")))

(defun print-row (x y z)
  (format t "~&   ~A | ~A | ~A"
	  (convert-to-letter x)
	  (convert-to-letter y)
	  (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -------------")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -------------")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(defun sum-triplet (board triplet)
  (+ (nth (car triplet) board)
     (nth (cadr triplet) board)
     (nth (caddr triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
	      (sum-triplet board triplet))
	  *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
	(member (* 3 *opponent*) sums))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
		     (<= 1 pos 9)))
	   (format t "~&Invalid input.")
	   (read-a-legal-move board))
	  ((not (zerop (nth pos board)))
	   (format t "~&That space is already occupied")
	   (read-a-legal-move board))
	  (t pos)))) 

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
	pos
	(pick-random-empty-position board))))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "random move"))

(defun choose-best-move (board)
  (random-move-strategy board))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
	 (pos (car best-move))
	 (strategy (cadr best-move))
	 (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A" strategy)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&I win!"))
	  ((board-full-p new-board) (format t "~&Tie game!"))
	  (t (opponent-move new-board)))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
	 (new-board (make-move *opponent* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&You win!"))
	  ((board-full-p new-board) (format t "~&Tie game"))
	  (t (computer-move new-board)))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defvar b)
(defvar *computer*)
(defvar *opponent*)
(defvar *triplets*)

(setf b (make-board))
(setf *computer* 10)
(setf *opponent* 1)
(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)
	(1 4 7) (2 5 8) (3 6 9)
	(1 5 9) (3 5 7)))
