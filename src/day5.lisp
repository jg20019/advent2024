(uiop:define-package advent.day5
  (:use :cl))

(in-package :advent.day5)

(defun numbers (strings)
  (mapcar #'parse-integer strings))

(defun parse-rule (line)
  (numbers (str:split "|" line)))

(defun parse-pages (line)
  (numbers (str:split "," line)))

(defun parse-rules (input)
  (let ((rules (make-hash-table :test 'equal)))
    (loop for line = (read-line input nil)
	  while (and line (not (str:blank? line)))
	  do (progn (let* ((rule (numbers (parse-rule line)))
			   (before (first rule))
			   (after (second rule)))
		      (push after (gethash before rules (list))))))
    rules))

(defun is-valid? (rules pages)
  (member (second pages) (gethash (first pages) rules)))
  
(defun valid-pages? (rules pages)
  (every #'(lambda (pages)
	     (is-valid? rules pages))
	 (mapcar #'list pages (rest pages))))
	 

(defun parse-valid-pages (rules input)
  (let ((valid-pages nil))
    (loop for line = (read-line input nil)
	  while line
	  do (progn (let ((pages (parse-pages line)))
		      (when (valid-pages? rules pages)
			(push pages valid-pages)))))
    (nreverse valid-pages)))

(defun middle (list)
  (nth (truncate (length list) 2) list))

(defun load-input (path)
  (with-open-file (input path)
    (let* ((rules (parse-rules input))
	   (pages (parse-valid-pages rules input))
	   (middle-page (mapcar #'middle pages)))
      (reduce #'+ middle-page))))

(load-input #p"~/advent/src/inputs/day-5.txt")
