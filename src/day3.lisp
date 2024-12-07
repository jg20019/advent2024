(uiop:define-package advent.day3
  (:use :cl)
  (:import-from :alexandria :if-let :when-let))

(in-package :advent.day3)

(defvar *test-input* "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(defvar *test-extended-input* "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defun peek (input cursor)
  "Returns character at cursor"
  (unless (>= cursor (length input))
    (aref input cursor)))

(defun advance (cursor)   
  (+ cursor 1))

(defun match (input cursor expected)
  "Advance cursor by length of match when input matches expected."
  (let ((start cursor)
	(end (+ cursor (length expected))))
    (when (and (<= end (length input)) (string= (subseq input start end) expected))
      end)))

(defun parse-number (input cursor)
  "Returns (values number cursor) or nil when no number was found."
  (let ((lookahead (peek input cursor)))
    (if (and lookahead (digit-char-p lookahead)) 
	(let* ((num-digits 0)
	       (digits (loop for ch = (peek input cursor)
			     while (and ch (digit-char-p ch) (< num-digits 3))
			     collect ch
			     do (progn (incf cursor)
				       (incf num-digits)))))
	  (values (parse-integer (concatenate 'string digits)) cursor))
	(values nil cursor))))

(defun parse-mulop (input cursor)
  "Returns (values (:mulop X Y) cursor) when it could parse mulop otherse (values nil :cursor"
  (multiple-value-bind (op next) (when-let ((cursor (match input cursor "mul(")))
				     (multiple-value-bind (X cursor) (parse-number input cursor)
				       (when X
					 (when-let ((cursor (match input cursor ",")))
					   (multiple-value-bind (Y cursor) (parse-number input cursor)
					     (when Y
					       (when-let ((cursor (match input cursor ")")))
						 (values (list :mul X Y) cursor))))))))
    (if op
	(values op next)
	(values nil cursor))))

(defun parse-do (input cursor)
  "Returns (values (:do) cursor) when it could parse do otherwise (values nil cursor)"
  (if-let ((c (match input cursor "do()")))
    (values (list :do) c)
    (values nil cursor)))

(defun parse-dont (input cursor)
  "Returns (values (:dont) cursor) when it could parse do otherwise (values nil cursor)"
  (if-let ((c (match input cursor "don't()")))
    (values (list :dont) c)
    (values nil cursor)))

(defun parse-input (input &optional (cursor 0) (operators nil))
  "Parse input returning recognized mulops"
  (let ((lookahead (peek input cursor)))
    (cond ((null lookahead) (nreverse operators))
	  ((char= lookahead #\m) (multiple-value-bind (op cursor) (parse-mulop input cursor)
				   (if op
				       (parse-input input cursor (cons op operators))
				       (parse-input input (advance cursor) operators))))
	  ((char= lookahead #\d) (multiple-value-bind (op cursor) (parse-do input cursor)
				   (if op
				       (parse-input input cursor (cons op operators))
				       (multiple-value-bind (op cursor) (parse-dont input cursor)
					 (if op
					     (parse-input input cursor (cons op operators))
					     (parse-input input (advance cursor) operators))))))
	  (t (parse-input input (advance cursor) operators)))))


(defun evaluate-operators (operators &optional (enabled t) (result 0))
  (cond ((null operators) result)
	(t (let ((operator (first operators))
		 (rest-operators (rest operators)))
	     (case (first operator)
	       (:do (evaluate-operators rest-operators t result))
	       (:dont (evaluate-operators rest-operators nil result))
	       (:mul (evaluate-operators rest-operators enabled (if enabled
								    (+ result (* (second operator)
										 (third operator)))
								    result))))))))

;; (evaluate-operators (parse-input (uiop:read-file-string #p"~/advent/src/inputs/day-3.txt")))
