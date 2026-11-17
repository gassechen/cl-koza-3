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

(defun make-island-program (index)
  (make-adf-program
   :adfs nil
   :rpb (ecase index
          (0 '(and d0 d1))
          (1 '(or d0 d1))
          (2 '(not d0))
          (3 '(nand d0 d1)))
   :branch-info
   (vector
    (make-test-branch-info
     :rpb :rpb 0 nil
     :function-set '(and or not nand)
     :function-argument-map '(2 2 1 2)
     :terminal-set '(:d0 :d1)))))

(defun make-island-demes ()
  (let ((demes (make-array 4)))
    (dotimes (deme-index 4)
      (let ((population (make-array 5)))
        (dotimes (rank 5)
          (setf (aref population rank)
                (make-individual
                 :program (make-island-program deme-index)
                 :standardized-fitness (1+ rank)
                 :hits (- 5 rank))))
        (cl-koza-3.kernel::normalize-fitness-of-population population)
        (cl-koza-3.kernel::sort-population-by-fitness population)
        (setf (aref demes deme-index) population)))
    demes))

(test migrate-demes-replaces-worst-with-torus-neighbor-best
  (let ((*migration-rate* 0.2)
        (demes (make-island-demes)))
    (cl-koza-3.kernel::migrate-demes demes)
    (let ((programs
            (loop for rank below 5
                  collect (adf-program-rpb
                           (individual-program
                            (aref (aref demes 0) rank))))))
      (is (= 3 (count '(and d0 d1) programs :test #'equal)))
      (is (= 1 (count '(or d0 d1) programs :test #'equal)))
      (is (= 1 (count '(not d0) programs :test #'equal))))))

(test validate-deme-parameters-rejects-invalid-island-settings
  (let ((*number-of-demes* 4)
        (*migration-rate* 0.0))
    (signals error
      (cl-koza-3.kernel::validate-deme-parameters 10)))
  (let ((*number-of-demes* 3)
        (*migration-rate* 0.1)
        (*deme-topology* :torus))
    (signals error
      (cl-koza-3.kernel::validate-deme-parameters 30))))

(test run-architecture-altering-system-with-demes
  (let ((*parallel-evaluation-p* nil)
        (*parallel-generation-p* nil)
        (*seed* 12345.0d0)
        (*verbose* nil)
        (*number-of-demes* 4)
        (*migration-rate* 0.2))
    (multiple-value-bind (population fitness-cases)
        (run-architecture-altering-system
         'architecture-run-problem 1.0 2 20)
      (declare (ignore fitness-cases))
      (is (= 20 (length population)))
      (dotimes (index (length population))
        (is (= 2
               (adf-program-branch-count
                (individual-program (aref population index)))))))))

(test architecture-breeding-applies-adi-creation
  (let* ((program (make-one-adf-program))
         (population (make-breeding-population program 4))
         (*branch-duplication-fraction* 0.0)
         (*argument-duplication-fraction* 0.0)
         (*branch-deletion-fraction* 0.0)
         (*argument-deletion-fraction* 0.0)
         (*branch-creation-fraction* 0.0)
         (*argument-creation-fraction* 0.0)
         (*adi-creation-fraction* 1.0)
         (*adi-deletion-fraction* 0.0))
    (call-architecture-breeder population)
    (dotimes (index (length population))
      (let ((new-program
              (individual-program (aref population index))))
        (is (= 1 (adf-program-adi-count new-program)))
        (is (member :adi0 (branch-names new-program)))))))

(test architecture-breeding-applies-adi-deletion
  (let* ((program (make-adi-architecture-program))
         (population (make-breeding-population program 4))
         (*branch-duplication-fraction* 0.0)
         (*argument-duplication-fraction* 0.0)
         (*branch-deletion-fraction* 0.0)
         (*argument-deletion-fraction* 0.0)
         (*branch-creation-fraction* 0.0)
         (*argument-creation-fraction* 0.0)
         (*adi-creation-fraction* 0.0)
         (*adi-deletion-fraction* 1.0))
    (call-architecture-breeder population)
    (dotimes (index (length population))
      (let ((new-program
              (individual-program (aref population index))))
        (is (zerop (adf-program-adi-count new-program)))
        (is (not (member :adi0 (branch-names new-program))))))))

(defun define-adi-architecture-run-parameters ()
  (setf *number-of-fitness-cases* 6)
  (setf *max-depth-for-new-individuals* 5)
  (setf *max-depth-for-new-subtrees-in-mutants* 4)
  (setf *max-depth-for-individuals-after-crossover* 17)
  (setf *method-of-selection* :tournament)
  (setf *tournament-size* 2)
  (setf *method-of-generation* :ramped-half-and-half)
  (setf *fitness-proportionate-reproduction-fraction* 0.0)
  (setf *crossover-at-any-point-fraction* 0.0)
  (setf *crossover-at-function-point-fraction* 0.0)
  (setf *branch-duplication-fraction* 0.0)
  (setf *argument-duplication-fraction* 0.0)
  (setf *branch-deletion-fraction* 0.0)
  (setf *argument-deletion-fraction* 0.0)
  (setf *branch-creation-fraction* 0.0)
  (setf *argument-creation-fraction* 0.0)
  (setf *adi-deletion-fraction* 0.0)
  (setf *adi-creation-fraction* 1.0)
  (values))

(defun define-adi-deletion-architecture-run-parameters ()
  (define-adi-architecture-run-parameters)
  (setf *adi-deletion-fraction* 1.0)
  (setf *adi-creation-fraction* 0.0)
  (values))

(defun adi-architecture-run-problem ()
  (multiple-value-bind (adf-specs
                        adi-specs
                        rpb-spec
                        fitness-cases-creator
                        fitness-function
                        parameter-definer
                        termination-predicate)
      (cl-koza-3.examples.adi-sequence-sum:adi-sequence-sum)
    (declare (ignore parameter-definer))
    (values adf-specs
            adi-specs
            rpb-spec
            fitness-cases-creator
            fitness-function
            'define-adi-architecture-run-parameters
            termination-predicate)))

(defun adi-deletion-architecture-run-problem ()
  (multiple-value-bind (adf-specs
                        adi-specs
                        rpb-spec
                        fitness-cases-creator
                        fitness-function
                        parameter-definer
                        termination-predicate)
      (cl-koza-3.examples.adi-sequence-sum:adi-sequence-sum)
    (declare (ignore parameter-definer))
    (values adf-specs
            adi-specs
            rpb-spec
            fitness-cases-creator
            fitness-function
            'define-adi-deletion-architecture-run-parameters
            termination-predicate)))

(test run-adi-architecture-altering-system-creates-adis
  (let ((*parallel-evaluation-p* nil)
        (*parallel-generation-p* nil)
        (*seed* 12345.0d0)
        (*verbose* nil))
    (multiple-value-bind (population fitness-cases)
        (run-adi-architecture-altering-system
         'adi-architecture-run-problem 1.0 2 4)
      (declare (ignore fitness-cases))
      (is (= 4 (length population)))
      (dotimes (index (length population))
        (is (= 2
               (adf-program-adi-count
                (individual-program (aref population index)))))))))

(test run-adi-architecture-altering-system-deletes-adis
  (let ((*parallel-evaluation-p* nil)
        (*parallel-generation-p* nil)
        (*seed* 12345.0d0)
        (*verbose* nil))
    (multiple-value-bind (population fitness-cases)
        (run-adi-architecture-altering-system
         'adi-deletion-architecture-run-problem 1.0 2 4)
      (declare (ignore fitness-cases))
      (is (= 4 (length population)))
      (dotimes (index (length population))
        (is (zerop
             (adf-program-adi-count
              (individual-program (aref population index)))))))))
