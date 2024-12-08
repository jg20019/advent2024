(uiop:define-package advent.day4
  (:use :cl))

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


(defvar *found* nil)

(defstruct point row col)

(defun same-point? (p1 p2)
  (and (= (point-row p1) (point-row p2))
       (= (point-col p1) (point-col p2))))

(defstruct xmas-location x m a s)

(defun same-location? (a b)
  (and (same-point? (xmas-location-x a)
		    (xmas-location-x b))
       (same-point? (xmas-location-s a)
		    (xmas-location-s b))))

(defun directions ()
  (let ((results nil))
    (dolist (row (list -1 0 1) results)
      (dolist (col (list -1 0 1))
	(unless (and (zerop row) (zerop col))
	  (push (list col row) results))))))
  
(defun count-xmas (word-search &optional (*found* *found*))
  ;; Searchs for the remaining letters 
  (labels ((find-xmas (letters r c locations dr dc)
	     (cond ((null letters)
		    (let ((locations (nreverse locations)))
		      (pushnew (make-xmas-location :x (first locations)
						   :m (second locations)
						   :a (third locations)
						   :s (fourth locations)) *found*)))
		   ((not (array-in-bounds-p word-search r c)) nil)
		   ((char= (aref word-search r c) (first letters))
		    (find-xmas (rest letters) (+ r dr) (+ c dc)
			       (cons (make-point :col c :row r) locations)
			       dr dc))
		   (t nil))))

    ;; Loop throw every position in word search 
    (dotimes (row (array-dimension word-search 0))
      (dotimes (col (array-dimension word-search 1))
	(when (char= (aref word-search row col) #\X)
	  (loop for (dc dr) in (directions)
		do (find-xmas (list #\M #\A #\S)
			      (+ row dr) (+ col dc)
			      (list (make-point :col col :row row))
			      dr dc)))))
    ;; After checking every location, count matches
    (values (length *found*) *found*)))

(count-xmas (load-word-search #p"~/advent/src/inputs/day-4-test.txt"))

(defun show-answers (word-search found)
  "Copies values in found from word-search into a new array
   showing what words were found."
  (let ((arr (make-array (array-dimensions word-search)
			 :initial-element #\.)))
    (dolist (match found arr)
      (let* ((x (xmas-location-x match))
	     (m (xmas-location-m match))
	     (a (xmas-location-a match))
	     (s (xmas-location-s match))
	     (x-row (point-row x))
	     (x-col (point-col x))
	     (m-row (point-row m))
	     (m-col (point-col m))
	     (a-row (point-row a))
	     (a-col (point-col a))
	     (s-row (point-row s))
	     (s-col (point-col s)))
	(setf (aref arr x-row x-col) #\X
	      (aref arr m-row m-col) #\M
	      (aref arr a-row a-col) #\A
	      (aref arr s-row s-col) #\S)))
    (dotimes (row (array-dimension arr 0))
      (dotimes (col (array-dimension arr 1))
	(format t "~a" (aref arr row col)))
      (format t "~%"))))
	

(let ((word-search (load-word-search #p"~/advent/src/inputs/day-4.txt")))
  (multiple-value-bind (c found) (count-xmas word-search) 
    (format t "Found: ~a matches.~%" c)))

