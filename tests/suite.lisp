(in-package #:cl-koza-3.tests)

(def-suite cl-koza-3-tests
  :description "Test suite for cl-koza-3")

(defun run-tests ()
  (fiveam:run! 'cl-koza-3-tests))
