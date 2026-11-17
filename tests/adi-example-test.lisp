(in-package #:cl-koza-3.tests)

(in-suite cl-koza-3-tests)

(test adi-sequence-sum-known-program
  (let (first-result second-result output)
    (setf output
          (capture-output
           (multiple-value-setq (first-result second-result)
             (cl-koza-3.examples.adi-sequence-sum:verify-known-program))))
    (is (not (null first-result)))
    (is (not (null second-result)))
    (is (search "standardized-fitness=0" output))
    (is (search "hits=6" output))))

(test adi-sequence-sum-run
  (let ((*parallel-evaluation-p* nil)
        (*parallel-generation-p* nil)
        (population nil))
    (capture-output
     (setf population
           (cl-koza-3.examples.adi-sequence-sum:run 2 12 1.0)))
    (is (= 12 (length population)))
    (is (loop for index below (length population)
              for program = (individual-program (aref population index))
               always (and (= 1 (adf-program-adi-count program))
                           (= 2 (length (adf-program-branch-info program))))))))

(test adi-architecture-altering-run
  (let ((*parallel-evaluation-p* nil)
        (*parallel-generation-p* nil)
        (population nil))
    (capture-output
     (setf population
           (cl-koza-3.examples.adi-architecture-altering:run 2 8 1.0)))
    (is (= 8 (length population)))
    (is (loop for index below (length population)
              always (adf-program-p
                      (individual-program (aref population index)))))))
