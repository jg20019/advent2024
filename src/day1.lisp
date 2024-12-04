(uiop:define-package advent.day1
  (:use #:cl))
(in-package #:advent.day1)

(defun numbers (line)
  (mapcar #'parse-integer (str:words line)))

(defun process-line (line)
  (values-list (numbers line)))

(defun sum (numbers)
  (reduce #'+ numbers))

(defun abs-diff (x y)
  (if (> x y)
      (- x y)
      (- y x)))

(defun sum-distance (path)
  (let ((a-col nil)
	(b-col nil))
    (with-open-file (f path)
      (loop for line = (read-line f nil nil)
	    while line
	    do (progn (multiple-value-bind (a b) (process-line line)
			(push a a-col)
			(push b b-col))))
      (sum (mapcar #'abs-diff (sort a-col #'<) (sort b-col #'<))))))
  
(defun sum-similarity (path)
  (let ((a-col nil)
	(counts (make-hash-table :test #'equal)))
    (with-open-file (f path)
      (loop for line = (read-line f nil nil)
	    while line
	    do (progn (multiple-value-bind (a b) (process-line line)
			(push a a-col)
			(incf (gethash b counts 0)))))
      (sum (mapcar (lambda (x)
		     (* x (gethash x counts 0))) a-col)))))
