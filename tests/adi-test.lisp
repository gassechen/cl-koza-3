(in-package #:cl-koza-3.tests)

(in-suite cl-koza-3-tests)

(defun make-adi-test-info (&key
                             (name :adi0)
                             (arity 1)
                             (formals '(:arg0))
                             (source :data)
                             (iteration-variable :item)
                             (iteration-index-variable :index)
                             (result-type :number))
  (make-branch-info
   :name name
   :kind :adi
   :arity arity
   :formals formals
   :function-set '(+)
   :function-argument-map '(2)
   :function-types '((+ :number :number :number))
   :terminal-set '(:arg0 :item :index)
   :terminal-types '((:arg0 . :number)
                     (:item . :number)
                     (:index . :number))
   :result-type result-type
   :iteration-source source
   :iteration-variable iteration-variable
   :iteration-index-variable iteration-index-variable))

(defun make-adi-test-rpb-info (&key (result-type :number))
  (make-branch-info
   :name :rpb
   :kind :rpb
   :arity 0
   :formals nil
   :function-set '(+ adi0)
   :function-argument-map '(2 1)
   :function-types '((+ :number :number :number)
                     (adi0 :number :number))
   :terminal-set '(:d0)
   :terminal-types '((:d0 . :number))
   :result-type result-type
   :allowed-branch-indices nil))

(defun make-adi-test-program (&key (source :data))
  (let ((adi-info (make-adi-test-info :source source))
        (rpb-info (make-adi-test-rpb-info)))
    (make-adf-program
     :adfs nil
     :adis (list '(+ arg0 (+ item index)))
     :rpb '(adi0 0)
     :branch-info (vector adi-info rpb-info))))

(defun adi-test-function-set ()
  (values '(+)
          '(2)
          '((+ :number :number :number))))

(defun adi-test-terminal-set ()
  (values '(:arg0 :item :index)
          '((:arg0 . :number)
            (:item . :number)
            (:index . :number))))

(defun adi-test-adi-spec (&key (source :data))
  (make-branch-spec
   :function-set-creator #'adi-test-function-set
   :terminal-set-creator #'adi-test-terminal-set
   :arity 1
   :result-type :number
   :iteration-source source
   :iteration-variable :item
   :iteration-index-variable :index))

(defun adi-test-rpb-function-set ()
  (values '(+ adi0)
          '(2 1)
          '((+ :number :number :number)
            (adi0 :number :number))))

(defun adi-test-rpb-terminal-set ()
  (values '(:d0)
          '((:d0 . :number))))

(defun adi-test-rpb-spec ()
  (make-branch-spec
   :function-set-creator #'adi-test-rpb-function-set
   :terminal-set-creator #'adi-test-rpb-terminal-set
   :arity 0
   :result-type :number))

(defun adi-test-fitness-cases ()
  (make-array *number-of-fitness-cases*))

(defun adi-test-parameters ()
  (setf *number-of-fitness-cases* 32)
  (setf *max-depth-for-new-individuals* 5)
  (setf *max-depth-for-new-subtrees-in-mutants* 4)
  (setf *max-depth-for-individuals-after-crossover* 17)
  (setf *fitness-proportionate-reproduction-fraction* 0.1)
  (setf *crossover-at-any-point-fraction* 0.2)
  (setf *crossover-at-function-point-fraction* 0.7)
  (setf *method-of-selection* :tournament)
  (setf *tournament-size* 7)
  (setf *method-of-generation* :ramped-half-and-half)
  (values))

(defun adi-test-termination (current-generation maximum-generations
                             best-standardized-fitness best-hits)
  (declare (ignore best-standardized-fitness best-hits))
  (>= current-generation maximum-generations))

(defun make-adi-test-problem ()
  (lambda ()
    (values nil
            (list (adi-test-adi-spec))
            (adi-test-rpb-spec)
            #'adi-test-fitness-cases
            #'constant-fitness
            #'adi-test-parameters
            #'adi-test-termination)))

(test adi-program-branch-model
  (let* ((program (make-adf-program
                   :adfs '((and arg0 arg1))
                   :adis '((+ arg0 item))
                   :rpb '(adi0 0)))
         (copy (copy-adf-program program)))
    (is (= 1 (adf-program-branch-count program)))
    (is (= 1 (adf-program-adi-count program)))
    (is (= 2 (adf-program-work-branch-count program)))
    (is (equal '(:adf0 :adi0 :rpb) (branch-names program)))
    (is (equal '(:adi . 0) (branch-index :adi0)))
    (is (= 1 (branch-info-index program :adi0)))
    (is (equal '(+ arg0 item) (adf-program-adi program 0)))
    (is (equal '(+ arg0 item) (program-branch program :adi0)))
    (is (equal '(adi0 0) (program-branch program :rpb)))
    (setf (adf-program-adi copy 0) '(+ arg0 index))
    (is (equal '(+ arg0 item) (adf-program-adi program 0)))
    (is (equal '(+ arg0 index) (adf-program-adi copy 0)))
    (let ((text (prin1-to-string program)))
      (is (search "(defun ADI0" text))
      (is (search "(defun RPB" text)))))

(test adi-evaluation-iterates-external-sequence
  (let ((program (make-adi-test-program)))
    (is (= 9 (evaluate-adf-program
              program
              :environment '((:data . (1 2 3))))))))

(test adi-evaluation-uses-dynamic-sequence-fallback
  (let ((program (make-adi-test-program)))
    (let ((*iteration-sequence* :unbound))
      (signals error
        (evaluate-adf-program program :environment nil)))
    (let ((*iteration-sequence* '(1 2 3)))
      (is (= 9 (evaluate-adf-program program :environment nil))))
    (let ((*iteration-sequence* '(100)))
      (is (= 6 (evaluate-adf-program
                program
                :environment '((:data . (2 3)))))))))

(test adi-resolution-orders-and-types-branches
  (multiple-value-bind (adf-branches rpb-branch adi-branches)
      (cl-koza-3.kernel::resolve-branch-specs
       nil
       (adi-test-rpb-spec)
       (list (adi-test-adi-spec)))
    (multiple-value-bind (resolved-adfs resolved-rpb resolved-adis)
        (cl-koza-3.kernel::resolve-terminal-sets
         adf-branches rpb-branch adi-branches)
      (declare (ignore resolved-adfs resolved-rpb resolved-adis))
      (let ((infos
              (cl-koza-3.kernel::resolve-branch-infos
               adf-branches rpb-branch adi-branches)))
        (is (= 2 (length infos)))
        (is (eq :adi (branch-info-kind (aref infos 0))))
        (is (eq :adi0 (branch-info-name (aref infos 0))))
        (is (= 1 (branch-info-arity (aref infos 0))))
        (is (eq :number (branch-info-result-type (aref infos 0))))
        (is (eq :data (branch-info-iteration-source (aref infos 0))))
        (is (eq :rpb (branch-info-kind (aref infos 1))))))))

(test adi-population-creation-includes-iterations
  (with-kernel-defaults
    (with-seed (11)
      (multiple-value-bind (adf-branches rpb-branch adi-branches)
          (cl-koza-3.kernel::resolve-branch-specs
           nil
           (adi-test-rpb-spec)
           (list (adi-test-adi-spec)))
        (cl-koza-3.kernel::resolve-terminal-sets
         adf-branches rpb-branch adi-branches)
        (let ((population
                (cl-koza-3.kernel::create-population-with-branches
                 8 adf-branches rpb-branch nil
                 :adi-branches adi-branches)))
          (dotimes (index 8)
            (let* ((program (individual-program (aref population index)))
                   (infos (adf-program-branch-info program))
                   (adi-body (adf-program-adi program 0))
                   (rpb-body (adf-program-rpb program)))
              (is (= 1 (adf-program-adi-count program)))
              (is (= 2 (length infos)))
              (is (eq :adi (branch-info-kind (aref infos 0))))
              (is (eq :number
                      (cl-koza-3.architecture::typed-tree-type
                       adi-body (aref infos 0) program)))
              (is (eq :number
                      (cl-koza-3.architecture::typed-tree-type
                       rpb-body (aref infos 1) program))))))))))

(test run-adi-system-creates-and-evolves-iterations
  (with-kernel-defaults
    (let ((population nil))
      (capture-output
       (setf population
             (run-adi-system (make-adi-test-problem) 1.0 1 8)))
      (is (= 8 (length population)))
      (is (loop for index below (length population)
                for program = (individual-program
                               (aref population index))
                always (and (= 1 (adf-program-adi-count program))
                            (= 2 (length (adf-program-branch-info program)))
                            (eq :adi0
                                (branch-info-name
                                 (aref (adf-program-branch-info program)
                                       0)))
                            (eq :rpb
                                (branch-info-name
                                 (aref (adf-program-branch-info program)
                                       1)))))))))
