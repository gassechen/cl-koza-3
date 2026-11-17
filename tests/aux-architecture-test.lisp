(in-package #:cl-koza-3.tests)

(in-suite cl-koza-3-tests)

(defun aux-architecture-rpb-info (functions)
  (make-aux-test-info
   :rpb :rpb
   :arity 0
   :formals nil
   :function-set functions
   :function-argument-map
   (mapcar (lambda (function)
             (if (eq function 'progn) 2 1))
           functions)
   :function-types
   (append
    (when (member 'progn functions)
      '((progn :number :number :number)))
    (loop for function in functions
          unless (eq function 'progn)
            collect (list function :number :number)))
   :terminal-set '(0)
   :terminal-types '((0 . :number))))

(defun aux-architecture-program
    (&key (adl-bodies '((+ arg0 index)))
          (rpb-body '(adl0 0))
          (functions '(+ adl0)))
  (let* ((adl-infos
           (loop for body in adl-bodies
                 for index from 0
                 collect (aux-test-arithmetic-info
                          (intern (format nil "ADL~D" index) :keyword)
                          :adl :count :item :index)))
         (rpb-info (aux-architecture-rpb-info functions)))
    (make-adf-program
     :adfs nil
     :adis (copy-list adl-bodies)
     :rpb rpb-body
     :branch-info (coerce (append adl-infos (list rpb-info)) 'vector))))

(defun aux-architecture-gpps-function-set ()
  (values '(+) '(2) '((+ :number :number :number))))

(defun aux-architecture-gpps-terminal-set ()
  (values '(0) '((0 . :number))))

(defun aux-architecture-gpps-rpb-spec ()
  (make-branch-spec
   :function-set-creator #'aux-architecture-gpps-function-set
   :terminal-set-creator #'aux-architecture-gpps-terminal-set
   :arity 0
   :result-type :number))

(defun aux-architecture-gpps-aux-template ()
  (make-branch-spec
   :function-set-creator #'aux-test-function-set
   :terminal-set-creator #'aux-test-terminal-set
   :arity 1
   :result-type :number
   :iteration-source :count
   :iteration-variable :item
   :iteration-index-variable :index))

(test aux-creation-appends-to-existing-family
  (let* ((program (aux-architecture-program))
         (new-program
           (with-architecture-test-seed
             (adl-creation program :branch :rpb :point 0 :body 'arg0)))
         (rpb-info (program-branch-info new-program :rpb)))
    (is (= 2 (adf-program-adl-count new-program)))
    (is (equal '(:adl0 :adl1 :rpb) (branch-names new-program)))
    (is (equal '(+ arg0 index) (adf-program-adl new-program 0)))
    (is (equal 'arg0 (adf-program-adl new-program 1)))
    (is (equal '(:adl1 (adl0 0)) (adf-program-rpb new-program)))
    (is (eq :adl1 (branch-info-name (program-branch-info new-program :adl1))))
    (is (test-function-present-p rpb-info "ADL1"))
    (is (= 1 (test-function-arity rpb-info "ADL1")))))

(test aux-deletion-renumbers-later-family
  (let* ((program
           (aux-architecture-program
            :adl-bodies '((+ arg0 index) (+ arg0 1))
            :rpb-body '(adl1 (adl0 0))
            :functions '(+ adl0 adl1)))
         (new-program
           (with-architecture-test-seed
             (adl-deletion program :branch :adl0)))
         (rpb-info (program-branch-info new-program :rpb)))
    (is (= 1 (adf-program-adl-count new-program)))
    (is (equal '(:adl0 :rpb) (branch-names new-program)))
    (is (eq :adl0 (branch-info-name (program-branch-info new-program :adl0))))
    (is (equal '(+ arg0 1) (adf-program-adl new-program 0)))
    (is (not (test-tree-contains-name-p (adf-program-rpb new-program) "ADL1")))
    (is (test-function-present-p rpb-info "ADL0"))
    (is (not (test-function-present-p rpb-info "ADL1")))
    (is (numberp
         (evaluate-adf-program
          new-program
          :environment '((:count . 3) (:d0 . 0)))))))

(test aux-duplication-adds-duplicate-call
  (let* ((program (aux-architecture-program))
         (new-program
           (with-architecture-test-seed
             (adl-branch-duplication program :branch :adl0)))
         (rpb-info (program-branch-info new-program :rpb)))
    (is (= 2 (adf-program-adl-count new-program)))
    (is (equal '(:adl0 :adl1 :rpb) (branch-names new-program)))
    (is (eq :adl1 (branch-info-name (program-branch-info new-program :adl1))))
    (is (equal '(+ arg0 index) (adf-program-adl new-program 1)))
    (is (test-function-present-p rpb-info "ADL1"))
    (is (= 1 (test-function-arity rpb-info "ADL1")))
    (is (numberp
         (evaluate-adf-program
          new-program
          :environment '((:count . 3) (:d0 . 0)))))))

(test gpps-run-creates-aux-from-empty-specs
  (let ((problem
          (make-gpps-problem
           :adf-specs nil
           :adi-specs nil
           :adl-specs nil
           :adr-specs nil
           :ads-specs nil
           :rpb-spec (aux-architecture-gpps-rpb-spec)
           :fitness-cases-creator (lambda () (make-array 4 :initial-element nil))
           :fitness-function #'constant-fitness
           :parameter-definer (lambda () (values))
           :termination-predicate #'aux-test-termination
           :aux-templates (list :adl (aux-architecture-gpps-aux-template))
           :architecture-fractions
           (list :branch-duplication-fraction 0.0
                 :argument-duplication-fraction 0.0
                 :branch-deletion-fraction 0.0
                 :argument-deletion-fraction 0.0
                 :branch-creation-fraction 0.0
                 :argument-creation-fraction 0.0
                 :adi-creation-fraction 0.0
                 :adi-deletion-fraction 0.0
                 :adi-branch-duplication-fraction 0.0
                 :adl-branch-duplication-fraction 0.0
                 :adl-deletion-fraction 0.0
                 :adr-creation-fraction 0.0
                 :adr-deletion-fraction 0.0
                 :adr-branch-duplication-fraction 0.0
                 :ads-creation-fraction 0.0
                 :ads-deletion-fraction 0.0
                 :ads-branch-duplication-fraction 0.0
                 :adl-creation-fraction 1.0)
           :architecture-policy
           (lambda (generation population)
             (declare (ignore population))
             (when (= generation 1)
               (list :adl-creation-fraction 1.0)))))
        (result nil))
    (with-kernel-defaults
      (capture-output
       (let ((*fitness-proportionate-reproduction-fraction* 0.0)
             (*crossover-at-any-point-fraction* 0.0)
             (*crossover-at-function-point-fraction* 0.0))
         (setf result (run-gpps-system problem 12345.0d0 2 4)))))
    (let* ((population result)
           (programs (population-programs population)))
      (is (= 4 (length programs)))
      (is (every (lambda (program)
                   (= 1 (adf-program-adl-count program)))
                 programs))
      (is (every (lambda (program)
                   (member :adl0 (branch-names program)))
                 programs)))))

(test gpps-example-run-smoke
  (let ((result nil))
    (with-kernel-defaults
      (capture-output
       (setf result
             (cl-koza-3.examples.gpps-architecture-altering:run
              2 8 1.0 :verbose nil))))
    (is (= 8 (length (population-programs result))))))
