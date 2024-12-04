(defsystem "advent"
  :version "0.0.1"
  :author "John Gibson"
  :license ""
  :depends-on (:str)
  :components ((:module "src"
                :components
                ((:file "day1")
		 (:file "day2"))))
  :description "Advent of Code 2024"
  :in-order-to ((test-op (test-op "advent/tests"))))

(defsystem "advent/tests"
  :author ""
  :license ""
  :depends-on ("advent"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for advent"
  :perform (test-op (op c) (symbol-call :rove :run c)))
