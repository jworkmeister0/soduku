(defun parse (line)
	(with-input-from-string (s line)
    	;loop until a null value is reached (aka the end)
    	(loop for num = (read s nil nil)
       	;while there are numbers to collect, collect numbers
      	 	while num
       		collect num)))

(defun read-file (filename)
	;Open the given file as a stream
	(with-open-file (stream filename)
		;loop while the next char in stream isn't null
    	(loop while (peek-char nil stream nil nil)
    		;parse and collect the line if it isn't null
    		collect (parse(read-line stream nil)))))

(defun list-to-2d-array (list)
  ;notice that make-array is fed the dimensions of the list
                    ;x width    y width
  (make-array (list (length list) (length (first list)))
	      ;fill it with the values in list
	      :initial-contents list))

;Read the file, convert to 2d Array, save as board
(defvar board (list-to-2d-array (read-file "puzzle.txt")))

(defun print-board (board)
	(dotimes (row 9)
		(if (eq 0 (mod row 3))
		  (format t "~%+---------+---------+---------+~%")
		  (format t "~%"))
		(dotimes (col 9)
			(if (eq 0 (mod col 3))
		  		(format t "|"))
			(format t " ~A " (aref board row col)))
		(format t "|"))
	(format t "~%+---------+---------+---------+~%~%"))


;showing the original board
(progn (format t "~% ORIGINAL BOARD:") (print-board board))

;This is where the magic happens. "guess" is a recursive function
;that does the backtracking and guessing
(defun guess (board x y)
	;when x+1=9
	(when (= 9 (incf x))
		;if we get here, the sudoku is solved and we print and leave
		(when (= 9 (incf y)) (print-board board) (return-from guess))
		;set x==0 aka start of next line
		(setf x 0))

	;if current location isn't 0
	(if (/= (aref board y x) 0)
		;make a guess at current location
		(guess board x y)
    	;test i, incrementing each time
		(loop for i from 1 to 9
			;check i @ x,y and set i if valid
       	   	do (and
					;if check returns nil, we skip to "finally" block
					(check i y x)
 					(progn
    		           	(setf (aref board y x) i)
						;keep guessing
        	        	(and (guess board x y) (return t)) )
				)
			;if the loop gets here, check returned nil
			finally (progn
						;discard i and try other values
                 		(setf (aref board y x) 0)
                 		(return nil)  )
		)
	)
)


(defun check (num y x)
	"returns nil if the value is unsuitable"
	(let((r (* (truncate (/ y 3)) 3))
		 (c (* (truncate (/ x 3)) 3)))
		;return true if the loop finishes without hitting a same value
    	(dotimes (i 9 t)
		;check row, col and box for equal values
    		(and (or
					(= num (aref board y i))
           		 	(= num (aref board i x))
            		(= num (aref board (+ r (mod i 3)) (+ c (truncate (/ i 3))))
					)
			)
		   ;if an equal value is found, return nil
           (return nil))
		)
	))


;the -1 is because the guess method increments x on initialization
(and (or (guess board -1 0)) (print-board board))
