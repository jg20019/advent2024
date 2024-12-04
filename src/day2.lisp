(uiop:define-package advent.day2
  (:use #:cl))

(in-package #:advent.day2)

(defun diff (x y)
  (abs (- x y)))

(defun numbers (string)
  (mapcar #'parse-integer (str:words string)))

(defun safe? (report)
  "Are levels monotonic and changing at acceptable levels?"
  (let ((min-change 1)
	(max-change 3))
    (and (or (apply #'< report) (apply #'> report)) 
	 (every #'(lambda (diff)
		    (<= min-change diff max-change))
		(mapcar #'diff report (rest report))))))

(defun count-safe-reports (path &optional (fn #'safe?))
  (let* ((lines (uiop:read-file-lines path)))
    (count-if fn lines :key #'numbers)))

(defun report-combinations (report)
  "List of reports with one of each level removed with the original report"
  (cons report
	(loop for i in report
	      collecting (remove i report :count 1))))
