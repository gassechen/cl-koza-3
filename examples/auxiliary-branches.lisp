(defpackage #:cl-koza-3.examples.auxiliary-branches
  (:use #:cl
        #:cl-koza-3)
  (:export
   #:auxiliary-branches-problem
   #:known-program
   #:define-fitness-cases-for-auxiliary-branches
   #:evaluate-standardized-fitness-for-auxiliary-branches
   #:run
   #:run-example
   #:verify-known-program))

(in-package #:cl-koza-3.examples.auxiliary-branches)

(defun define-function-set-for-adl ()
  (values '(+ -)
          '(2 2)
          '((+ :number :number :number)
            (- :number :number :number))))

(defun define-terminal-set-for-adl ()
  (values '(:arg0 :item :index)
          '((:arg0 . :number)
            (:item . :number)
            (:index . :number))))

(defun define-function-set-for-adr ()
  (values '(if < adr0 +)
          '(3 2 1 2)
          '((if :number :boolean :number :number)
            (< :boolean :number :number)
            (adr0 :number :number)
            (+ :number :number :number))))

(defun define-terminal-set-for-adr ()
  (values '(:arg0 :level :depth)
          '((:arg0 . :number)
            (:level . :number)
            (:depth . :number))))

(defun define-function-set-for-ads ()
  (values '(+ -)
          '(2 2)
          '((+ :number :number :number)
            (- :number :number :number))))

(defun define-terminal-set-for-ads ()
  (values '(:arg0 :value :address)
          '((:arg0 . :number)
            (:value . :number)
            (:address . :number))))

(defun define-function-set-for-rpb ()
  (values '(+ adl0 adr0 ads0)
          '(2 1 1 1)
          '((+ :number :number :number)
            (adl0 :number :number)
            (adr0 :number :number)
            (ads0 :number :number))))

(defun define-terminal-set-for-rpb ()
  (values '(0) '((0 . :number))))

(defun make-adl-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-adl
   :terminal-set-creator 'define-terminal-set-for-adl
   :arity 1
   :result-type :number
   :iteration-source :count
   :iteration-variable :item
   :iteration-index-variable :index))

(defun make-adr-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-adr
   :terminal-set-creator 'define-terminal-set-for-adr
   :arity 1
   :result-type :number
   :iteration-source :limit
   :iteration-variable :level
   :iteration-index-variable :depth))

(defun make-ads-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-ads
   :terminal-set-creator 'define-terminal-set-for-ads
   :arity 1
   :result-type :number
   :iteration-source :cell
   :iteration-variable :value
   :iteration-index-variable :address))

(defun make-rpb-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-rpb
   :terminal-set-creator 'define-terminal-set-for-rpb
   :arity 0
   :result-type :number))

(defun define-fitness-cases-for-auxiliary-branches ()
  (make-array
   1
   :initial-element (list (cons :count 3)
                          (cons :limit 2)
                          (cons :cell :shared))))

(defun evaluate-standardized-fitness-for-auxiliary-branches
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

(defun define-parameters-for-auxiliary-branches ()
  (setf *number-of-fitness-cases* 1)
  (setf *max-depth-for-new-individuals* 5)
  (setf *max-depth-for-new-subtrees-in-mutants* 4)
  (setf *max-depth-for-individuals-after-crossover* 17)
  (setf *fitness-proportionate-reproduction-fraction* 0.1)
  (setf *crossover-at-any-point-fraction* 0.2)
  (setf *crossover-at-function-point-fraction* 0.7)
  (setf *method-of-selection* :tournament)
  (setf *tournament-size* 3)
  (setf *method-of-generation* :ramped-half-and-half)
  (values))

(defun define-termination-criterion-for-auxiliary-branches
    (current-generation maximum-generations
     best-standardized-fitness best-hits)
  (declare (ignore best-standardized-fitness))
  (or (>= current-generation maximum-generations)
      (>= best-hits *number-of-fitness-cases*)))

(defun auxiliary-branches-problem ()
  (values nil
          nil
          (list (make-adl-branch-spec))
          (list (make-adr-branch-spec))
          (list (make-ads-branch-spec))
          (make-rpb-branch-spec)
          'define-fitness-cases-for-auxiliary-branches
          'evaluate-standardized-fitness-for-auxiliary-branches
          'define-parameters-for-auxiliary-branches
          'define-termination-criterion-for-auxiliary-branches))

(defun make-branch-info-for-auxiliary-branches ()
  (vector
   (make-branch-info
    :name :adl0
    :kind :adl
    :arity 1
    :formals '(:arg0)
    :function-set '(+ -)
    :function-argument-map '(2 2)
    :function-types '((+ :number :number :number)
                      (- :number :number :number))
    :terminal-set '(:arg0 :item :index)
    :terminal-types '((:arg0 . :number)
                      (:item . :number)
                      (:index . :number))
    :result-type :number
    :iteration-source :count
    :iteration-variable :item
    :iteration-index-variable :index)
   (make-branch-info
    :name :adr0
    :kind :adr
    :arity 1
    :formals '(:arg0)
    :function-set '(if < adr0 +)
    :function-argument-map '(3 2 1 2)
    :function-types '((if :number :boolean :number :number)
                      (< :boolean :number :number)
                      (adr0 :number :number)
                      (+ :number :number :number))
    :terminal-set '(:arg0 :level :depth)
    :terminal-types '((:arg0 . :number)
                      (:level . :number)
                      (:depth . :number))
    :result-type :number
    :iteration-source :limit
    :iteration-variable :level
    :iteration-index-variable :depth)
   (make-branch-info
    :name :ads0
    :kind :ads
    :arity 1
    :formals '(:arg0)
    :function-set '(+ -)
    :function-argument-map '(2 2)
    :function-types '((+ :number :number :number)
                      (- :number :number :number))
    :terminal-set '(:arg0 :value :address)
    :terminal-types '((:arg0 . :number)
                      (:value . :number)
                      (:address . :number))
    :result-type :number
    :iteration-source :cell
    :iteration-variable :value
    :iteration-index-variable :address)
   (make-branch-info
    :name :rpb
    :kind :rpb
    :arity 0
    :formals nil
    :function-set '(+ adl0 adr0 ads0)
    :function-argument-map '(2 1 1 1)
    :function-types '((+ :number :number :number)
                      (adl0 :number :number)
                      (adr0 :number :number)
                      (ads0 :number :number))
    :terminal-set '(0)
    :terminal-types '((0 . :number))
    :result-type :number
    :allowed-branch-indices nil)))

(defun known-program ()
  (make-adf-program
   :adfs nil
   :adis (list '(+ arg0 index)
               '(if (< depth 2) (adr0 (+ arg0 1)) arg0)
               '(+ value 1))
   :rpb '(+ (adl0 0) (+ (adr0 0) (ads0 0)))
   :branch-info (make-branch-info-for-auxiliary-branches)))

(defun run (maximum-generations size-of-population seed &key verbose)
  (let ((*verbose* verbose))
    (run-aux-system 'auxiliary-branches-problem
                    seed
                    maximum-generations
                    size-of-population)))

(defun run-example (&key
                      (maximum-generations 10)
                      (size-of-population 100)
                      (seed 1.0)
                      (verbose nil))
  (run maximum-generations size-of-population seed :verbose verbose))

(defun verify-known-program ()
  (define-parameters-for-auxiliary-branches)
  (let ((fitness-cases (define-fitness-cases-for-auxiliary-branches))
        (program (known-program)))
    (multiple-value-bind (fitness hits)
        (evaluate-standardized-fitness-for-auxiliary-branches program
                                                              fitness-cases)
      (format t "~&Known auxiliary-branches program: standardized-fitness=~D hits=~D~%"
              fitness hits)
      (values (zerop fitness)
              (= hits *number-of-fitness-cases*)))))
