(in-package :fkpos-tests)

(def-suite all-tests
  :description "The master test suite of FKPOS.")

(in-suite all-tests)

(defun test-fkpos () (run! 'all-tests))

(test dummy-tests
      (is (= 4 (+ 2 2))))
