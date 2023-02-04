;;;; TictacToe.lisp

;; Variables for the player names
(defparameter *playerNameO* "")
(defparameter *playerNameX* "")

;; Creation of a new playing fiel
(defun newBoard ()
 (loop for i from 1 to 9
        collect i))

;; Output of the playing field
(defun printBoard (board)
  (format t "|---|---|---|~%")
  (format t "~{| ~a ~}|~%" (subseq board 0 3))
  (format t "|-----------|~%")
  (format t "~{| ~a ~}|~%" (subseq board 3 6))
  (format t "|-----------|~%")
  (format t "~{| ~a ~}|~%" (subseq board 6 9))
  (format t "|---|---|---|~%")  
  )

;; Check if there are three symbols in a line (victory condition)
(defun checkLine (symbol line)
  (if (string-equal line (concatenate 'string symbol symbol symbol))
    1
    0))

;; Concatenation of numbers (or other elements)
(defun concat( list )
  (format nil "~{~a~}" list))

;; Checking whether the game was won for the respective player
(defun gameFinished (board playerName symbol)  
(loop for victoryPossibility from 0 to 7 do   
        (let ((victory 0 )))  
        (case victoryPossibility 
            ;; First row
            (0 (setf victory (checkLine symbol (concat  (list (nth   0 board) (nth  1 board) (nth  2 board))))))
            ;; Second row
            (1 (setf victory (checkLine symbol (concat (list (nth  3 board) (nth  4 board) (nth  5 board))))))
            ;; Third row
            (2 (setf victory (checkLine symbol (concat (list (nth  6 board) (nth  7 board) (nth  8 board))))))
            ;; First column
            (3 (setf victory (checkLine symbol (concat (list (nth  0 board)  (nth 3 board) (nth  6 board))))))
            ;; Second column
            (4 (setf victory (checkLine symbol (concat (list (nth 1 board) (nth 4 board) (nth  7 board))))))
            ;; Third column
            (5 (setf victory (checkLine symbol (concat (list (nth 2 board) (nth 5 board) (nth 8 board))))))
            ;; Diagonal top left to bottom right
            (6 (setf victory (checkLine symbol (concat (list (nth 0 board) (nth 4 board ) (nth  8 board))))))
            ;; Diagonal top left to bottom right
            (7 (setf victory (checkLine symbol (concat (list(nth 2 board) (nth 4 board ) (nth  6 board)))))))          
            (if (= victory 1)  
             (progn 
                (format t "~A hat gewonnen! ~%" playerName) 
                (return t)))
            ))     

;; Setting a move
(defun makeMove (board playerName symbol)
 (format t "~A geben Sie bitte ein Feld an!.~%" playerName) 
  (setf (nth (- (read) 1) board) symbol))

;; Gameplay
(defun playGame (board)
 (printBoard board)
  (loop until (or (gameFinished board *playerNameX* "X") (gameFinished board *playerNameO* "O")) do  
    (defvar i 0)
    (if (evenp i)
        (makeMove board *playerNameO* "O")
        (makeMove board *playerNameX* "X") 
    )   
    (printBoard board)
    (if (= i 8) 
    (progn 
      (format t "Unentschieden!~%") 
      (return t)
      ))
       (setq i (+ 1 i))     
    )
)     

;; Gamebegin
(defun welcome ()
  (format t "Willkommen zu Tic Tac Toe!~%")
  (format t "Bitte geben Sie den Namen für den Spieler ein, welcher die O's verwenden soll.~%")
  (setf *playerNameO* (read))
  (format t "Bitte geben Sie den Namen für den Spieler ein, welcher die X's verwenden soll.~%")
  (setf *playerNameX* (read))
  (playGame (newBoard))
)

(welcome)
