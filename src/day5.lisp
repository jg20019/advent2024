(uiop:define-package advent.day5
  (:use :cl))

(in-package :advent.day5)

(defun numbers (strings)
  (mapcar #'parse-integer strings))

(defun parse-rule (line)
  (numbers (str:split "|" line)))

(defun parse-pages (input)
  (loop for line = (read-line input nil)
	while line
	collect (numbers (str:split "," line))))

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

(defun swap (list x y)
  (let ((tmp (nth x list)))
    (setf (nth x list) (nth y list)
	  (nth y list) tmp)
    list))

(defun fix-pages (rules pages)
  (sort pages #'(lambda (a b) (is-valid? rules (list a b)))))

(defun middle (list)
  (nth (truncate (length list) 2) list))

(defun load-input (path)
  (with-open-file (input path)
    (let* ((rules (parse-rules input))
	   (pages (parse-pages input))
	   (invalid-pages (remove-if (lambda (pages) (valid-pages? rules pages)) pages))
	   (fixed-pages (mapcar #'(lambda (pages) (fix-pages rules pages)) invalid-pages))
	   (middle-page (mapcar #'middle fixed-pages)))
      (reduce #'+ middle-page))))

(load-input #p"~/advent/src/inputs/day-5.txt")
