(in-package #:cl-koza-3.tests)

(in-suite cl-koza-3-tests)

(test auxiliary-branches-known-program
  (let (first-result second-result output)
    (setf output
          (capture-output
           (multiple-value-setq (first-result second-result)
             (cl-koza-3.examples.auxiliary-branches:verify-known-program))))
    (is (not (null first-result)))
    (is (not (null second-result)))
    (is (search "standardized-fitness=0" output))
    (is (search "hits=1" output))))

(test auxiliary-branches-run
  (let ((*parallel-evaluation-p* nil)
        (*parallel-generation-p* nil)
        (population nil))
    (capture-output
     (setf population
           (cl-koza-3.examples.auxiliary-branches:run 2 8 1.0)))
    (is (= 8 (length population)))
    (is (loop for index below (length population)
              for program = (individual-program (aref population index))
              always (and (= 1 (adf-program-adl-count program))
                          (= 1 (adf-program-adr-count program))
                          (= 1 (adf-program-ads-count program))
                          (= 4 (length (adf-program-branch-info program))))))))
