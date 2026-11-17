(in-package #:cl-koza-3.tests)

(in-suite cl-koza-3-tests)

(defun make-aux-test-info (name kind
                           &key
                             (arity 1)
                             (formals '(:arg0))
                             function-set
                             (function-argument-map nil function-argument-map-p)
                             function-types
                             terminal-set
                             terminal-types
                             (result-type :number)
                             source
                             (item :item)
                             (index :index))
  (make-branch-info
   :name name
   :kind kind
   :arity arity
   :formals formals
   :function-set function-set
   :function-argument-map
   (if function-argument-map-p
       function-argument-map
       (make-list (length function-set) :initial-element 0))
   :function-types function-types
   :terminal-set terminal-set
   :terminal-types terminal-types
   :result-type result-type
   :iteration-source source
   :iteration-variable item
   :iteration-index-variable index))

(defun aux-test-arithmetic-info (name kind source item index)
  (make-aux-test-info
   name kind
   :function-set '(+ -)
   :function-argument-map '(2 2)
   :function-types '((+ :number :number :number)
                     (- :number :number :number))
   :terminal-set (list :arg0 item index)
   :terminal-types (list (cons :arg0 :number)
                         (cons item :number)
                         (cons index :number))
   :source source
   :item item
   :index index))

(defun aux-test-arithmetic-rpb-info (functions)
  (make-aux-test-info
   :rpb :rpb
   :arity 0
   :formals nil
   :function-set functions
   :function-argument-map (mapcar (lambda (function)
                                    (if (eq function 'progn) 2 1))
                                  functions)
   :function-types (append
                    (when (member 'progn functions)
                      '((progn :number :number :number)))
                    (loop for function in functions
                          unless (eq function 'progn)
                            collect (list function :number :number)))
   :terminal-set '(0)
   :terminal-types '((0 . :number))))

(defun aux-test-function-set ()
  (values '(+) '(2) '((+ :number :number :number))))

(defun aux-test-terminal-set ()
  (values '(:arg0 0) '((:arg0 . :number) (0 . :number))))

(defun aux-test-spec (&key source item index)
  (make-branch-spec
   :function-set-creator #'aux-test-function-set
   :terminal-set-creator #'aux-test-terminal-set
   :arity 1
   :result-type :number
   :iteration-source source
   :iteration-variable item
   :iteration-index-variable index))

(defun aux-test-run-rpb-function-set ()
  (values '(+ adl0 adr0 ads0)
          '(2 1 1 1)
          '((+ :number :number :number)
            (adl0 :number :number)
            (adr0 :number :number)
            (ads0 :number :number))))

(defun aux-test-run-rpb-terminal-set ()
  (values '(0) '((0 . :number))))

(defun aux-test-run-rpb-spec ()
  (make-branch-spec
   :function-set-creator #'aux-test-run-rpb-function-set
   :terminal-set-creator #'aux-test-run-rpb-terminal-set
   :arity 0
   :result-type :number))

(defun aux-test-fitness-cases ()
  (make-array *number-of-fitness-cases*))

(defun aux-test-parameters ()
  (setf *number-of-fitness-cases* 4)
  (setf *max-depth-for-new-individuals* 4)
  (setf *max-depth-for-new-subtrees-in-mutants* 3)
  (setf *max-depth-for-individuals-after-crossover* 10)
  (setf *fitness-proportionate-reproduction-fraction* 1.0)
  (setf *crossover-at-any-point-fraction* 0.0)
  (setf *crossover-at-function-point-fraction* 0.0)
  (setf *method-of-selection* :tournament)
  (setf *tournament-size* 3)
  (setf *method-of-generation* :ramped-half-and-half)
  (values))

(defun aux-test-termination (current-generation maximum-generations
                             best-standardized-fitness best-hits)
  (declare (ignore best-standardized-fitness best-hits))
  (>= current-generation maximum-generations))

(defun aux-test-known-program ()
  (make-adf-program
   :adfs nil
   :adis (list '(+ arg0 index)
               '(if (< depth 2) (adr0 (+ arg0 1)) arg0)
               '(+ value 1))
   :rpb '(progn (adl0 0) (adr0 0) (ads0 0))
   :branch-info
   (vector
    (aux-test-arithmetic-info :adl0 :adl :count :item :index)
    (make-aux-test-info
     :adr0 :adr
     :function-set '(if < adr0 +)
     :function-argument-map '(3 2 1 2)
     :function-types '((if :number :boolean :number :number)
                       (< :boolean :number :number)
                       (adr0 :number :number)
                       (+ :number :number :number))
     :terminal-set '(:arg0 :depth)
     :terminal-types '((:arg0 . :number) (:depth . :number))
     :source :limit
     :item :level
     :index :depth)
    (aux-test-arithmetic-info :ads0 :ads :cell :value :unused)
    (aux-test-arithmetic-rpb-info '(progn adl0 adr0 ads0)))))

(test aux-program-branch-model
  (let ((program (aux-test-known-program)))
    (is (= 3 (adf-program-aux-count program)))
    (is (= 0 (adf-program-adi-count program)))
    (is (= 1 (adf-program-adl-count program)))
    (is (= 1 (adf-program-adr-count program)))
    (is (= 1 (adf-program-ads-count program)))
    (is (= 3 (adf-program-work-branch-count program)))
    (is (equal '(:adl0 :adr0 :ads0 :rpb) (branch-names program)))
    (is (equal '(+ arg0 index) (adf-program-adl program 0)))
    (is (equal '(+ value 1) (adf-program-ads program 0)))
    (is (equal '(+ arg0 index) (program-branch program :adl0)))
    (is (= 1 (branch-info-index program :adr0)))))

(test adl-evaluation-repeats-with-index
  (let ((*iteration-sequence* :unbound)
        (program (make-adf-program
                  :adfs nil
                  :adis (list '(+ arg0 index))
                  :rpb '(adl0 0)
                  :branch-info
                  (vector
                   (aux-test-arithmetic-info :adl0 :adl :count :item :index)
                   (aux-test-arithmetic-rpb-info '(adl0))))))
    (is (= 3 (evaluate-adf-program program
                                   :environment '((:count . 3)))))))

(test adr-evaluation-stops-at-depth-limit
  (let ((*recursion-depth* 0)
        (program (make-adf-program
                  :adfs nil
                  :adis (list '(if (< depth 2) (adr0 (+ arg0 1)) arg0))
                  :rpb '(adr0 0)
                  :branch-info
                  (vector
                   (make-aux-test-info
                    :adr0 :adr
                    :function-set '(if < adr0 +)
                    :function-argument-map '(3 2 1 2)
                    :function-types '((if :number :boolean :number :number)
                                      (< :boolean :number :number)
                                      (adr0 :number :number)
                                      (+ :number :number :number))
                    :terminal-set '(:arg0 :depth)
                    :terminal-types '((:arg0 . :number)
                                      (:depth . :number))
                    :source :limit
                    :item :level
                    :index :depth)
                   (aux-test-arithmetic-rpb-info '(adr0))))))
    (is (= 2 (evaluate-adf-program program
                                   :environment '((:limit . 2)))))))

(test ads-evaluation-uses-mutable-storage
  (let ((*storage-cells* (make-hash-table :test #'equal))
        (program (make-adf-program
                  :adfs nil
                  :adis (list '(+ value 1))
                  :rpb '(progn (ads0 0) (ads0 0))
                  :branch-info
                  (vector
                   (aux-test-arithmetic-info :ads0 :ads :cell :value :unused)
                   (aux-test-arithmetic-rpb-info '(progn ads0))))))
    (is (= 2 (evaluate-adf-program program
                                   :environment '((:cell . :shared)))))))

(test aux-specs-resolve-in-canonical-order
  (let ((rpb-spec (aux-test-run-rpb-spec))
        (adl-spec (aux-test-spec :source :count :item :item :index :index))
        (adr-spec (aux-test-spec :source :limit :item :level :index :depth))
        (ads-spec (aux-test-spec :source :cell :item :value :index :unused)))
    (multiple-value-bind (adf-branches rpb-branch adi-branches
                          adl-branches adr-branches ads-branches)
        (cl-koza-3.kernel::resolve-branch-specs
         nil rpb-spec nil
         :adl-specs (list adl-spec)
         :adr-specs (list adr-spec)
         :ads-specs (list ads-spec))
      (is (null adf-branches))
      (is (null adi-branches))
      (is (= 1 (length adl-branches)))
      (is (= 1 (length adr-branches)))
      (is (= 1 (length ads-branches)))
      (cl-koza-3.kernel::resolve-terminal-sets
       adf-branches rpb-branch adi-branches
       adl-branches adr-branches ads-branches)
      (let ((infos
              (cl-koza-3.kernel::resolve-branch-infos
               adf-branches rpb-branch adi-branches
               :adl-branches adl-branches
               :adr-branches adr-branches
               :ads-branches ads-branches)))
        (is (equal '(:adl :adr :ads :rpb)
                   (map 'list #'branch-info-kind infos)))
         (is (eq :adl (cl-koza-3.kernel::resolved-branch-kind
                       (first adl-branches))))
         (is (eq :adf (cl-koza-3.kernel::resolved-branch-kind
                       rpb-branch)))))))

(test run-aux-system-uses-all-branch-families
  (let ((*parallel-evaluation-p* nil)
        (*parallel-generation-p* nil)
        (population nil))
    (capture-output
     (setf population
           (run-aux-system
            (lambda ()
              (values nil
                      nil
                      (list (aux-test-spec
                             :source :count :item :item :index :index))
                      (list (aux-test-spec
                             :source :limit :item :level :index :depth))
                      (list (aux-test-spec
                             :source :cell :item :value :index :unused))
                      (aux-test-run-rpb-spec)
                      #'aux-test-fitness-cases
                      #'constant-fitness
                      #'aux-test-parameters
                      #'aux-test-termination))
            1.0 1 4
            (aux-test-known-program))))
    (is (= 4 (length population)))
    (is (every (lambda (individual)
                 (let ((program (individual-program individual)))
                   (and (= 1 (adf-program-adl-count program))
                        (= 1 (adf-program-adr-count program))
                        (= 1 (adf-program-ads-count program)))))
               (coerce population 'list)))))
