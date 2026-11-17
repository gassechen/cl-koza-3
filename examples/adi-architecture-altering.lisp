(defpackage #:cl-koza-3.examples.adi-architecture-altering
  (:use #:cl
        #:cl-koza-3)
  (:export
   #:adi-architecture-altering
   #:define-parameters-for-adi-architecture-altering
   #:run
   #:run-example))

(in-package #:cl-koza-3.examples.adi-architecture-altering)

(defun define-parameters-for-adi-architecture-altering ()
  (setf *number-of-fitness-cases* 6)
  (setf *max-depth-for-new-individuals* 5)
  (setf *max-depth-for-new-subtrees-in-mutants* 4)
  (setf *max-depth-for-individuals-after-crossover* 17)
  (setf *fitness-proportionate-reproduction-fraction* 0.10)
  (setf *crossover-at-any-point-fraction* 0.50)
  (setf *crossover-at-function-point-fraction* 0.24)
  (setf *branch-duplication-fraction* 0.04)
  (setf *argument-duplication-fraction* 0.04)
  (setf *branch-deletion-fraction* 0.005)
  (setf *argument-deletion-fraction* 0.005)
  (setf *branch-creation-fraction* 0.04)
  (setf *argument-creation-fraction* 0.0)
  (setf *adi-creation-fraction* 0.02)
  (setf *adi-deletion-fraction* 0.01)
  (setf *method-of-selection* :tournament)
  (setf *tournament-size* 3)
  (setf *method-of-generation* :ramped-half-and-half)
  (values))

(defun adi-architecture-altering ()
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
            'define-parameters-for-adi-architecture-altering
            termination-predicate)))

(defun run (maximum-generations
            size-of-population
            seed
            &key verbose)
  (let ((*verbose* verbose))
    (run-adi-architecture-altering-system
     'adi-architecture-altering
     seed
     maximum-generations
     size-of-population)))

(defun run-example (&key
                      (maximum-generations 10)
                      (size-of-population 100)
                      (seed 1.0)
                      (verbose nil))
  (run maximum-generations size-of-population seed :verbose verbose))
