(uiop:define-package advent.day4
  (:use :cl)
  (:import-from :alexandria))

(in-package :advent.day4)

(defun load-word-search (path)
  "Reads word search into a 2d array"
  (with-open-file (input path)
    (let* ((contents (loop for line = (read-line input nil)
			   while line
			   collect (coerce line 'list)))
	   (num-rows (length contents))
	   (num-cols (length (first contents))))
      (make-array (list num-rows num-cols)
		  :element-type 'standard-char
		  :initial-contents contents))))

(defun get-location (array row col)
  (when (array-in-bounds-p array row col)
    (aref array row col)))

(defun ms? (ch)
  (or (char= ch #\M) (char= ch #\S)))
  
(defun xmas? (word-search a-row a-col)
  (let* ((top-left (get-location word-search (- a-row 1) (- a-col 1)))
	 (top-right (get-location word-search (- a-row 1) (+ a-col 1)))
	 (bottom-left (get-location word-search (+ a-row 1) (- a-col 1)))
	 (bottom-right (get-location word-search (+ a-row 1) (+ a-col 1))))

    ;; get-location returns nil if location is invalid so we check if they had values
    ;; before comparing characters
    (and top-left 
	 top-right
	 bottom-left
	 bottom-right
         ;; Make sure all of them are either m or s
	 (ms? top-left)
	 (ms? top-right)
	 (ms? bottom-left)
	 (ms? bottom-right)
	 ;; Make sure they are not equal
	 ;; since at this point they are either M or S being not equal means
	 ;; being a valid X-MAS
	 (char/= top-left bottom-right)
	 (char/= top-right bottom-left))))
  
(defun count-xmas (word-search &optional (*found* *found*))
  ;; Searchs for the remaining letters 

  ;; Loop throw every position in word search 
  (let ((count 0))
    (dotimes (row (array-dimension word-search 0) count)
      (dotimes (col (array-dimension word-search 1))
	(when (and (char= (aref word-search row col) #\A) (xmas? word-search row col))
	  (incf count))))))

(count-xmas (load-word-search #p"~/advent/src/inputs/day-4.txt"))
