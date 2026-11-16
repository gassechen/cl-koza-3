(defpackage #:cl-koza-3.examples.mixed-typed-regression
  (:use #:cl
        #:cl-koza-3)
  (:export
   #:mixed-typed-regression
   #:known-program
   #:define-fitness-cases-for-mixed-typed-regression
   #:evaluate-standardized-fitness-for-mixed-typed-regression
   #:run
   #:run-example))

(in-package #:cl-koza-3.examples.mixed-typed-regression)

(defvar arg0)
(defvar arg1)
(defvar x)
(defvar y)
(defvar z)

(defun if-number (test then else)
  (if test then else))

(defun define-function-set-for-adf0 ()
  (values '(< > <= >= and or not)
          '(2 2 2 2 2 2 1)
          '((< :boolean :number :number)
            (> :boolean :number :number)
            (<= :boolean :number :number)
            (>= :boolean :number :number)
            (and :boolean :boolean :boolean)
            (or :boolean :boolean :boolean)
            (not :boolean :boolean))))

(defun define-function-set-for-adf1 ()
  (values '(+ - * if-number < adf0)
          '(2 2 2 3 2 2)
          '((+ :number :number :number)
            (- :number :number :number)
            (* :number :number :number)
            (if-number :number :boolean :number :number)
            (< :boolean :number :number))))

(defun define-function-set-for-rpb ()
  (values '(+ - * if-number < adf0 adf1)
          '(2 2 2 3 2 2 2)
          '((+ :number :number :number)
            (- :number :number :number)
            (* :number :number :number)
            (if-number :number :boolean :number :number)
            (< :boolean :number :number))))

(defun define-terminal-set-for-adf0 ()
  (values '(arg0 arg1 :integer-random-constant)
          '((arg0 . :number)
            (arg1 . :number)
            (:integer-random-constant . :number))))

(defun define-terminal-set-for-adf1 ()
  (values '(arg0 arg1 :integer-random-constant)
          '((arg0 . :number)
            (arg1 . :number)
            (:integer-random-constant . :number))))

(defun define-terminal-set-for-rpb ()
  (values '(x y z :integer-random-constant)
          '((x . :number)
            (y . :number)
            (z . :number)
            (:integer-random-constant . :number))))

(defun make-adf0-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-adf0
   :terminal-set-creator 'define-terminal-set-for-adf0
   :arity 2
   :result-type :boolean))

(defun make-adf1-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-adf1
   :terminal-set-creator 'define-terminal-set-for-adf1
   :arity 2
   :result-type :number))

(defun make-rpb-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-rpb
   :terminal-set-creator 'define-terminal-set-for-rpb
   :arity 0
   :result-type :number))

(defstruct mixed-typed-fitness-case
  x
  y
  z
  target)

(defun target-value (x y z)
  (if (< x y)
      (+ (* x x) z)
      (- (* y y) z)))

(defun define-fitness-cases-for-mixed-typed-regression ()
  (let ((fitness-cases (make-array *number-of-fitness-cases*))
        (index 0))
    (dolist (case-x '(0 1 2))
      (dolist (case-y '(0 1 2))
        (dolist (case-z '(0 1 2))
          (setf (aref fitness-cases index)
                (make-mixed-typed-fitness-case
                 :x case-x
                 :y case-y
                 :z case-z
                 :target (target-value case-x case-y case-z)))
          (incf index))))
    (values fitness-cases)))

(defun evaluate-standardized-fitness-for-mixed-typed-regression
    (program fitness-cases)
  (let ((raw-fitness 0.0)
        (hits 0))
    (dotimes (index *number-of-fitness-cases*)
      (let* ((fitness-case (aref fitness-cases index))
             (value-from-program
               (evaluate-adf-program
                program
                :environment
                (list (cons 'x (mixed-typed-fitness-case-x fitness-case))
                      (cons 'y (mixed-typed-fitness-case-y fitness-case))
                      (cons 'z (mixed-typed-fitness-case-z fitness-case)))))
             (target-value
               (mixed-typed-fitness-case-target fitness-case)))
        (incf raw-fitness (abs (- value-from-program target-value)))
        (when (= value-from-program target-value)
          (incf hits))))
    (values raw-fitness hits)))

(defun define-parameters-for-mixed-typed-regression ()
  (setf *number-of-fitness-cases* 27)
  (setf *max-depth-for-new-individuals* 5)
  (setf *max-depth-for-new-subtrees-in-mutants* 4)
  (setf *max-depth-for-individuals-after-crossover* 17)
  (setf *fitness-proportionate-reproduction-fraction* 0.09)
  (setf *crossover-at-any-point-fraction* 0.76)
  (setf *crossover-at-function-point-fraction* 0.0)
  (setf *branch-duplication-fraction* 0.05)
  (setf *argument-duplication-fraction* 0.05)
  (setf *branch-deletion-fraction* 0.005)
  (setf *argument-deletion-fraction* 0.005)
  (setf *branch-creation-fraction* 0.04)
  (setf *argument-creation-fraction* 0.0)
  (setf *branch-creation-designation-probability* 0.5)
  (setf *method-of-selection* :tournament)
  (setf *tournament-size* 7)
  (setf *method-of-generation* :ramped-half-and-half)
  (values))

(defun define-termination-criterion-for-mixed-typed-regression
    (current-generation
     maximum-generations
     best-standardized-fitness
     best-hits)
  (declare (ignore best-standardized-fitness))
  (values (or (>= current-generation maximum-generations)
              (>= best-hits *number-of-fitness-cases*))))

(defun mixed-typed-regression ()
  (values (list (make-adf0-branch-spec)
                (make-adf1-branch-spec))
          (make-rpb-branch-spec)
          'define-fitness-cases-for-mixed-typed-regression
          'evaluate-standardized-fitness-for-mixed-typed-regression
          'define-parameters-for-mixed-typed-regression
          'define-termination-criterion-for-mixed-typed-regression))

(defun known-program ()
  (make-adf-program
   :adfs (vector '(< arg0 arg1)
                 '(+ (* arg0 arg0) arg1))
   :rpb '(if-number (adf0 x y) (adf1 x z) (- (* y y) z))))

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
     'mixed-typed-regression
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
