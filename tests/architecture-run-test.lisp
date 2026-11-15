(in-package #:cl-koza-3.tests)

(in-suite cl-koza-3-tests)

(defun make-one-adf-program ()
  (let* ((adf-info (make-test-branch-info
                    :adf0 :adf 2 '(:arg0 :arg1)
                    :function-set '(and or)
                    :function-argument-map '(2 2)
                    :terminal-set '(:arg0 :arg1)
                    :allowed-branch-indices '()))
         (rpb-info (make-test-branch-info
                    :rpb :rpb 0 nil
                    :function-set '(and or adf0)
                    :function-argument-map '(2 2 2)
                    :terminal-set '(:d0 :d1)
                    :allowed-branch-indices '(0))))
    (make-adf-program
     :adfs '((and arg0 arg1))
     :rpb '(or (adf0 d0 d1) (and d1 d0))
     :branch-info (vector adf-info rpb-info))))

(defun make-breeding-population (program size)
  (let ((population (make-array size)))
    (dotimes (index size)
      (setf (aref population index)
            (make-individual :program program
                             :standardized-fitness 1
                             :normalized-fitness (/ 1.0 size))))
    population))

(defun call-architecture-breeder (population)
  (let ((new-programs (make-array (length population)))
        (*seed* 12345.0d0)
        (*method-of-selection* :tournament)
        (*tournament-size* 2)
        (*fitness-proportionate-reproduction-fraction* 0.0)
        (*crossover-at-any-point-fraction* 0.0)
        (*crossover-at-function-point-fraction* 0.0))
    (values
     (cl-koza-3.architecture::breed-architecture-altering-population
      population new-programs nil nil)
     new-programs)))

(test architecture-breeding-applies-branch-duplication
  (let* ((program (make-one-adf-program))
         (population (make-breeding-population program 4))
         (*branch-duplication-fraction* 1.0))
    (call-architecture-breeder population)
    (dotimes (index (length population))
      (let* ((individual (aref population index))
             (new-program (individual-program individual)))
        (is (= 2 (adf-program-branch-count new-program)))
        (is (eq :adf1
                (branch-info-name
                 (program-branch-info new-program :adf1))))
        (is (eq :rpb
                (branch-info-name
                 (program-branch-info new-program :rpb))))))
    (is (= 1 (adf-program-branch-count program)))))

(test architecture-breeding-reproduces-when-operation-inapplicable
  (let* ((rpb-info (make-test-branch-info
                    :rpb :rpb 0 nil
                    :function-set '(and or)
                    :function-argument-map '(2 2)
                    :terminal-set '(:d0 :d1)))
         (program (make-adf-program
                   :adfs nil
                   :rpb '(and d0 d1)
                   :branch-info (vector rpb-info)))
         (population (make-breeding-population program 3))
         (*branch-duplication-fraction* 1.0))
    (call-architecture-breeder population)
    (dotimes (index (length population))
      (let ((new-program
              (individual-program (aref population index))))
        (is (= 0 (adf-program-branch-count new-program)))
        (is (equal '(and d0 d1) (adf-program-rpb new-program)))))))

(defun define-architecture-run-parameters ()
  (setf *number-of-fitness-cases* 32)
  (setf *max-depth-for-new-individuals* 5)
  (setf *max-depth-for-new-subtrees-in-mutants* 4)
  (setf *max-depth-for-individuals-after-crossover* 17)
  (setf *fitness-proportionate-reproduction-fraction* 0.0)
  (setf *crossover-at-any-point-fraction* 0.0)
  (setf *crossover-at-function-point-fraction* 0.0)
  (setf *branch-duplication-fraction* 0.0)
  (setf *argument-duplication-fraction* 0.0)
  (setf *branch-deletion-fraction* 0.0)
  (setf *argument-deletion-fraction* 0.0)
  (setf *branch-creation-fraction* 1.0)
  (setf *argument-creation-fraction* 0.0)
  (setf *branch-creation-designation-probability* 0.0)
  (setf *method-of-selection* :tournament)
  (setf *tournament-size* 2)
  (setf *method-of-generation* :ramped-half-and-half)
  (values))

(defun architecture-run-problem ()
  (multiple-value-bind (adf-specs
                        rpb-spec
                        fitness-cases-creator
                        fitness-function
                        parameter-definer
                        termination-predicate)
      (cl-koza-3.examples.even-5-parity-architecture-altering:even-5-parity-architecture-altering)
    (declare (ignore parameter-definer))
    (values adf-specs
            rpb-spec
            fitness-cases-creator
            fitness-function
            'define-architecture-run-parameters
            termination-predicate)))

(test run-architecture-altering-system-grows-branches
  (let ((*parallel-evaluation-p* nil)
        (*parallel-generation-p* nil)
        (*seed* 12345.0d0)
        (*verbose* nil))
    (multiple-value-bind (population fitness-cases)
        (run-architecture-altering-system
         'architecture-run-problem 1.0 3 4)
      (declare (ignore fitness-cases))
      (is (= 4 (length population)))
      (dotimes (index (length population))
        (is (= 3
               (adf-program-branch-count
                (individual-program (aref population index)))))))))
