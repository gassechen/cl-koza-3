(defpackage #:cl-koza-3.examples.adi-sequence-sum
  (:use #:cl
        #:cl-koza-3)
  (:export
   #:adi-sequence-sum
   #:known-program
   #:define-fitness-cases-for-adi-sequence-sum
   #:evaluate-standardized-fitness-for-adi-sequence-sum
   #:run
   #:run-example
   #:verify-known-program))

(in-package #:cl-koza-3.examples.adi-sequence-sum)

(defparameter *sequences*
  '((1 2 3)
    (2 4 6)
    (0 0 0)
    (5)
    (-1 1 2)
    (10 20 30 40)))

(defstruct adi-fitness-case
  sequence
  target)

(defun expected-value (sequence)
  (let ((length (length sequence)))
    (+ (reduce #'+ sequence)
       (/ (* length (1- length)) 2))))

(defun define-function-set-for-adi ()
  (values '(+ -)
          '(2 2)
          '((+ :number :number :number)
            (- :number :number :number))))

(defun define-terminal-set-for-adi ()
  (values '(arg0 item index)
          '((arg0 . :number)
            (item . :number)
            (index . :number))))

(defun define-function-set-for-rpb ()
  (values '(adi0)
          '(1)
          '((adi0 :number :number))))

(defun define-terminal-set-for-rpb ()
  (values '(0)
          '((0 . :number))))

(defun make-adi-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-adi
   :terminal-set-creator 'define-terminal-set-for-adi
   :arity 1
   :result-type :number
   :iteration-source :data
   :iteration-variable :item
   :iteration-index-variable :index))

(defun make-rpb-branch-spec ()
  (make-branch-spec
   :function-set-creator 'define-function-set-for-rpb
   :terminal-set-creator 'define-terminal-set-for-rpb
   :arity 0
   :result-type :number))

(defun define-fitness-cases-for-adi-sequence-sum ()
  (let ((fitness-cases (make-array *number-of-fitness-cases*)))
    (loop for sequence in *sequences*
          for index from 0
          do (setf (aref fitness-cases index)
                   (make-adi-fitness-case
                    :sequence sequence
                    :target (expected-value sequence))))
    (values fitness-cases)))

(defun evaluate-standardized-fitness-for-adi-sequence-sum
    (program fitness-cases)
  (let ((raw-fitness 0)
        (hits 0))
    (dotimes (index *number-of-fitness-cases*)
      (let* ((fitness-case (aref fitness-cases index))
             (sequence (adi-fitness-case-sequence fitness-case))
             (target (adi-fitness-case-target fitness-case))
             (value
               (evaluate-adf-program
                program
                :environment (list (cons :data sequence)))))
        (incf raw-fitness (abs (- value target)))
        (when (= value target)
          (incf hits))))
    (values raw-fitness hits)))

(defun define-parameters-for-adi-sequence-sum ()
  (setf *number-of-fitness-cases* (length *sequences*))
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

(defun define-termination-criterion-for-adi-sequence-sum
    (current-generation
     maximum-generations
     best-standardized-fitness
     best-hits)
  (declare (ignore best-standardized-fitness))
  (or (>= current-generation maximum-generations)
      (>= best-hits *number-of-fitness-cases*)))

(defun adi-sequence-sum ()
  (values nil
          (list (make-adi-branch-spec))
          (make-rpb-branch-spec)
          'define-fitness-cases-for-adi-sequence-sum
          'evaluate-standardized-fitness-for-adi-sequence-sum
          'define-parameters-for-adi-sequence-sum
          'define-termination-criterion-for-adi-sequence-sum))

(defun make-branch-info-for-adi-sequence-sum ()
  (let ((adi-info
          (make-branch-info
           :name :adi0
           :kind :adi
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
           :iteration-source :data
           :iteration-variable :item
           :iteration-index-variable :index))
        (rpb-info
          (make-branch-info
           :name :rpb
           :kind :rpb
           :arity 0
           :formals nil
           :function-set '(adi0)
           :function-argument-map '(1)
           :function-types '((adi0 :number :number))
           :terminal-set '(0)
           :terminal-types '((0 . :number))
           :result-type :number
           :allowed-branch-indices nil)))
    (vector adi-info rpb-info)))

(defun known-program ()
  (make-adf-program
   :adfs nil
   :adis (list '(+ arg0 item index))
   :rpb '(adi0 0)
   :branch-info (make-branch-info-for-adi-sequence-sum)))

(defun run (maximum-generations
            size-of-population
            seed
            &key verbose)
  (let ((*verbose* verbose))
    (run-adi-system 'adi-sequence-sum
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
  (define-parameters-for-adi-sequence-sum)
  (let* ((fitness-cases (define-fitness-cases-for-adi-sequence-sum))
         (program (known-program)))
    (multiple-value-bind (fitness hits)
        (evaluate-standardized-fitness-for-adi-sequence-sum program
                                                            fitness-cases)
      (format t "~&Known ADI program: standardized-fitness=~D hits=~D~%"
              fitness hits)
      (values (zerop fitness)
              (= hits *number-of-fitness-cases*)))))
