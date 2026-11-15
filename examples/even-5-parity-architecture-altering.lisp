(defpackage #:cl-koza-3.examples.even-5-parity-architecture-altering
  (:use #:cl
        #:cl-koza-3)
  (:export
   #:even-5-parity-architecture-altering
   #:run
   #:run-example))

(in-package #:cl-koza-3.examples.even-5-parity-architecture-altering)

(defvar d0)
(defvar d1)
(defvar d2)
(defvar d3)
(defvar d4)
(defvar arg0)
(defvar arg1)

(defun nand (a b)
  (not (and a b)))

(defun nor (a b)
  (not (or a b)))

(defun odd-parity (&rest args)
  (let ((result nil))
    (dolist (value args result)
      (when value
        (setf result (not result))))))

(defun define-function-set-for-adf0 ()
  (values '(and or nand nor)
          '(2 2 2 2)))

(defun define-function-set-for-rpb ()
  (values '(and or nand nor adf0)
          '(2 2 2 2 2)))

(defun define-terminal-set-for-adf0 ()
  (values '(arg0 arg1)))

(defun define-terminal-set-for-rpb ()
  (values '(d4 d3 d2 d1 d0)))

(defun make-adf0-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-adf0
   :terminal-set-creator 'define-terminal-set-for-adf0
   :arity 2))

(defun make-rpb-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-rpb
   :terminal-set-creator 'define-terminal-set-for-rpb
   :arity 0))

(defstruct even-5-parity-fitness-case
  d0
  d1
  d2
  d3
  d4
  target)

(defun define-fitness-cases-for-even-5-parity ()
  (let ((fitness-cases (make-array *number-of-fitness-cases*))
        (index 0))
    (dolist (d4 '(t nil))
      (dolist (d3 '(t nil))
        (dolist (d2 '(t nil))
          (dolist (d1 '(t nil))
            (dolist (d0 '(t nil))
              (let ((fitness-case (make-even-5-parity-fitness-case)))
                (setf (even-5-parity-fitness-case-d0 fitness-case) d0)
                (setf (even-5-parity-fitness-case-d1 fitness-case) d1)
                (setf (even-5-parity-fitness-case-d2 fitness-case) d2)
                (setf (even-5-parity-fitness-case-d3 fitness-case) d3)
                (setf (even-5-parity-fitness-case-d4 fitness-case) d4)
                (setf (even-5-parity-fitness-case-target fitness-case)
                      (not (odd-parity d4 d3 d2 d1 d0)))
                (setf (aref fitness-cases index) fitness-case)
                (incf index)))))))
    (values fitness-cases)))

(defun evaluate-standardized-fitness-for-even-5-parity (program fitness-cases)
  (let ((raw-fitness 0.0)
        (hits 0))
    (dotimes (index *number-of-fitness-cases*)
      (let* ((fitness-case (aref fitness-cases index))
             (value-from-program
               (evaluate-adf-program
                program
                :environment
                (list (cons 'd0 (even-5-parity-fitness-case-d0 fitness-case))
                      (cons 'd1 (even-5-parity-fitness-case-d1 fitness-case))
                      (cons 'd2 (even-5-parity-fitness-case-d2 fitness-case))
                      (cons 'd3 (even-5-parity-fitness-case-d3 fitness-case))
                      (cons 'd4 (even-5-parity-fitness-case-d4 fitness-case)))))
             (target-value (even-5-parity-fitness-case-target fitness-case))
             (match-found (eq target-value value-from-program)))
        (incf raw-fitness (if match-found 1.0 0.0))
        (when match-found
          (incf hits))))
    (values (- *number-of-fitness-cases* raw-fitness)
            hits)))

(defun define-parameters-for-even-5-parity ()
  (setf *number-of-fitness-cases* 32)
  (setf *max-depth-for-new-individuals* 5)
  (setf *max-depth-for-new-subtrees-in-mutants* 4)
  (setf *max-depth-for-individuals-after-crossover* 17)
  (setf *fitness-proportionate-reproduction-fraction* 0.10)
  (setf *crossover-at-any-point-fraction* 0.74)
  (setf *crossover-at-function-point-fraction* 0.0)
  (setf *branch-duplication-fraction* 0.05)
  (setf *argument-duplication-fraction* 0.05)
  (setf *branch-deletion-fraction* 0.005)
  (setf *argument-deletion-fraction* 0.005)
  (setf *branch-creation-fraction* 0.05)
  (setf *argument-creation-fraction* 0.0)
  (setf *branch-creation-designation-probability* 0.5)
  (setf *method-of-selection* :tournament)
  (setf *tournament-size* 7)
  (setf *method-of-generation* :ramped-half-and-half)
  (values))

(defun define-termination-criterion-for-even-5-parity
    (current-generation
     maximum-generations
     best-standardized-fitness
     best-hits)
  (declare (ignore best-standardized-fitness))
  (values (or (>= current-generation maximum-generations)
              (>= best-hits *number-of-fitness-cases*))))

(defun even-5-parity-architecture-altering ()
  (values (list (make-adf0-branch-spec))
          (make-rpb-branch-spec)
          'define-fitness-cases-for-even-5-parity
          'evaluate-standardized-fitness-for-even-5-parity
          'define-parameters-for-even-5-parity
          'define-termination-criterion-for-even-5-parity))

(defun run (maximum-generations
            size-of-population
            seed
            &key
              verbose
              (number-of-demes *number-of-demes*)
              (migration-rate *migration-rate*))
  (let ((*verbose* verbose)
        (*number-of-demes* number-of-demes)
        (*migration-rate* migration-rate))
    (run-architecture-altering-system
     'even-5-parity-architecture-altering
     seed
     maximum-generations
     size-of-population)))

(defun run-example (&key
                      (maximum-generations 76)
                      (size-of-population 1000)
                      (seed 1.0)
                      (verbose nil)
                      (number-of-demes *number-of-demes*)
                      (migration-rate *migration-rate*))
  (run maximum-generations
       size-of-population
       seed
       :verbose verbose
       :number-of-demes number-of-demes
       :migration-rate migration-rate))
