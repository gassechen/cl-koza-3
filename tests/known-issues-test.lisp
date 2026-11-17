(in-package #:cl-koza-3.tests)

(in-suite cl-koza-3-tests)

(test maximum-generations-zero-rejected
  (signals error
    (cl-koza-3.kernel::validate-run-arguments
     #'identity 1.0 0 4)))

(test random-integer-zero-signals-error
  (signals error (random-integer 0)))
