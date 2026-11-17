(defpackage #:cl-koza-3.examples.gpps-architecture-altering
  (:use #:cl
        #:cl-koza-3)
  (:export
   #:gpps-problem
   #:define-parameters-for-gpps-architecture-altering
   #:architecture-policy
   #:run
   #:run-example))

(in-package #:cl-koza-3.examples.gpps-architecture-altering)

(defun define-function-set-for-aux ()
  (values '(+ -)
          '(2 2)
          '((+ :number :number :number)
            (- :number :number :number))))

(defun define-terminal-set-for-adl ()
  (values '(:arg0 :item :index)
          '((:arg0 . :number)
            (:item . :number)
            (:index . :number))))

(defun define-terminal-set-for-adr ()
  (values '(:arg0 :level :depth)
          '((:arg0 . :number)
            (:level . :number)
            (:depth . :number))))

(defun define-terminal-set-for-ads ()
  (values '(:arg0 :value :address)
          '((:arg0 . :number)
            (:value . :number)
            (:address . :number))))

(defun define-function-set-for-rpb ()
  (values '(+) '(2) '((+ :number :number :number))))

(defun define-terminal-set-for-rpb ()
  (values '(0) '((0 . :number))))

(defun make-adl-template ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-aux
   :terminal-set-creator 'define-terminal-set-for-adl
   :arity 1
   :result-type :number
   :iteration-source :count
   :iteration-variable :item
   :iteration-index-variable :index))

(defun make-adr-template ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-aux
   :terminal-set-creator 'define-terminal-set-for-adr
   :arity 1
   :result-type :number
   :iteration-source :limit
   :iteration-variable :level
   :iteration-index-variable :depth))

(defun make-ads-template ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-aux
   :terminal-set-creator 'define-terminal-set-for-ads
   :arity 1
   :result-type :number
   :iteration-source :cell
   :iteration-variable :value
   :iteration-index-variable :address))

(defun make-rpb-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-rpb
   :terminal-set-creator 'define-terminal-set-for-rpb
   :arity 0
   :result-type :number))

(defun define-fitness-cases-for-gpps-architecture-altering ()
  (vector
   (list (cons :count 3) (cons :limit 2) (cons :cell :a))
   (list (cons :count 4) (cons :limit 3) (cons :cell :b))))

(defun evaluate-standardized-fitness-for-gpps-architecture-altering
    (program fitness-cases)
  (let ((raw-fitness 0)
        (hits 0))
    (dotimes (index *number-of-fitness-cases*)
      (let ((*storage-cells* (make-hash-table :test #'equal))
            (*recursion-depth* 0)
            (*recursion-limit* nil)
            (value
              (evaluate-adf-program
               program
               :environment (aref fitness-cases index))))
        (incf raw-fitness (abs (- value 6)))
        (when (= value 6)
          (incf hits))))
    (values raw-fitness hits)))

(defun define-parameters-for-gpps-architecture-altering ()
  (setf *number-of-fitness-cases* 2)
  (setf *max-depth-for-new-individuals* 5)
  (setf *max-depth-for-new-subtrees-in-mutants* 4)
  (setf *max-depth-for-individuals-after-crossover* 17)
  (setf *fitness-proportionate-reproduction-fraction* 0.10)
  (setf *crossover-at-any-point-fraction* 0.50)
  (setf *crossover-at-function-point-fraction* 0.24)
  (setf *method-of-selection* :tournament)
  (setf *tournament-size* 3)
  (setf *method-of-generation* :ramped-half-and-half)
  (values))

(defun define-termination-criterion-for-gpps-architecture-altering
    (current-generation maximum-generations
     best-standardized-fitness best-hits)
  (declare (ignore best-standardized-fitness))
  (or (>= current-generation maximum-generations)
      (>= best-hits *number-of-fitness-cases*)))

(defun zero-aux-fractions ()
  (list
   :adi-creation-fraction 0.0
   :adi-deletion-fraction 0.0
   :adi-branch-duplication-fraction 0.0
   :adl-creation-fraction 0.0
   :adl-deletion-fraction 0.0
   :adl-branch-duplication-fraction 0.0
   :adr-creation-fraction 0.0
   :adr-deletion-fraction 0.0
   :adr-branch-duplication-fraction 0.0
   :ads-creation-fraction 0.0
   :ads-deletion-fraction 0.0
   :ads-branch-duplication-fraction 0.0))

(defun architecture-policy (generation population)
  (declare (ignore population))
  (case (mod generation 3)
    (0 (append (zero-aux-fractions)
               (list :adl-creation-fraction 0.03
                     :adl-deletion-fraction 0.0
                     :adl-branch-duplication-fraction 0.0)))
    (1 (append (zero-aux-fractions)
               (list :adr-creation-fraction 0.03
                     :adr-deletion-fraction 0.0
                     :adr-branch-duplication-fraction 0.0)))
    (2 (append (zero-aux-fractions)
               (list :ads-creation-fraction 0.03
                     :ads-deletion-fraction 0.0
                     :ads-branch-duplication-fraction 0.0)))))

(defun gpps-problem ()
  (make-gpps-problem
   :adf-specs nil
   :adi-specs nil
   :adl-specs nil
   :adr-specs nil
   :ads-specs nil
   :rpb-spec (make-rpb-spec)
   :fitness-cases-creator 'define-fitness-cases-for-gpps-architecture-altering
   :fitness-function 'evaluate-standardized-fitness-for-gpps-architecture-altering
   :parameter-definer 'define-parameters-for-gpps-architecture-altering
   :termination-predicate
   'define-termination-criterion-for-gpps-architecture-altering
   :aux-templates (list :adl (make-adl-template)
                        :adr (make-adr-template)
                        :ads (make-ads-template))
   :architecture-fractions
   (append
    (list :branch-duplication-fraction 0.04
          :argument-duplication-fraction 0.04
          :branch-deletion-fraction 0.005
          :argument-deletion-fraction 0.005
          :branch-creation-fraction 0.04
          :argument-creation-fraction 0.0)
    (zero-aux-fractions))
   :architecture-policy 'architecture-policy
   :maximum-branch-counts (list :adi 1 :adl 2 :adr 2 :ads 2)))

(defun run (maximum-generations size-of-population seed &key verbose)
  (let ((*verbose* verbose))
    (run-gpps-system (gpps-problem)
                     seed
                     maximum-generations
                     size-of-population)))

(defun run-example (&key
                      (maximum-generations 6)
                      (size-of-population 40)
                      (seed 1.0)
                      (verbose nil))
  (run maximum-generations size-of-population seed :verbose verbose))
