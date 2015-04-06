(defvar size 9)
(defvar box 3)
(defconstant empty 0)
;These functions deal with getting the puzzle
;from a file and putting the into a 2d array.

;The puzzle is represented by 81 chars separated
;by whitespace and new lines. Empty boxes are 0's

;Due to the extreme amount of effort it took to get
;the file i/o done, don't expect error checking.
;This is probably as good as it's going to get


;Line parser. This is needed to collect the numbers 
;from a given line into the list
(defun parse-line (line)
  (with-input-from-string (s line)
    ;loop until a null value is reached (aka the end)
    (loop for num = (read s nil nil)
       ;collect numbers only
       while num
       collect num)))

(defun read-file (filename)
  ;Open the given file as a stream
  (with-open-file (stream filename)
    ;loop while the next char in stream isn't null
    (loop while (peek-char nil stream nil nil)
       ;parse and collect the line if it isn't null
       collect (parse-line (read-line stream nil)))))
 
(defun list-to-2d-array (list)
  ;notice that make-array is fed the dimensions of the list
                    ;col width    row width
  (make-array (list (length list) (length (first list)))
	      ;fill it with the values in list
	      :initial-contents list))

;Read the file, convert to 2d Array, save as board
(defvar board (list-to-2d-array (read-file "puzzle.txt")))

(defun digits-in-box (board x y)
  (loop
   with x0 = (* 3 (truncate x 3))
   with y0 = (* 3 (truncate y 3))
   with x1 = (+ x0 2)
   with y1 = (+ y0 2)
   for x from x0 to x1
   append (loop for y from y0 to y1
                for digit = (aref board y x)
                when (/= digit 0) collect digit)))

(defun digits-in-row (board y)
  (loop for x from 0 below 9
        for digit = (aref board y x)
        when (/= digit 0) collect digit))

(defun digits-in-column (board x)
  (loop for y from 0 below 9 for digit = (aref board y x)
        when (/= digit 0) collect digit))
