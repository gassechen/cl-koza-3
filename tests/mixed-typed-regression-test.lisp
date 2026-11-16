(in-package #:cl-koza-3.tests)

(in-suite cl-koza-3-tests)

(defun mixed-typed-terminal-type (info name)
  (cdr (find name
             (branch-info-terminal-types info)
             :key #'first
             :test (lambda (left right)
                     (string= (symbol-name left)
                              (symbol-name right))))))

(test mixed-typed-regression-contract
  (multiple-value-bind (adf-specs
                        rpb-spec
                        fitness-cases-creator
                        fitness-function
                        parameter-definer
                        termination-predicate)
      (cl-koza-3.examples.mixed-typed-regression:mixed-typed-regression)
    (is (= 2 (length adf-specs)))
    (is (eq :boolean (branch-spec-result-type (first adf-specs))))
    (is (eq :number (branch-spec-result-type (second adf-specs))))
    (is (eq :number (branch-spec-result-type rpb-spec)))
    (is (fboundp fitness-cases-creator))
    (is (fboundp fitness-function))
    (is (fboundp parameter-definer))
    (is (fboundp termination-predicate))
    (multiple-value-bind (adf-branches rpb-branch)
        (cl-koza-3.kernel::resolve-branch-specs adf-specs rpb-spec)
      (cl-koza-3.kernel::resolve-terminal-sets adf-branches rpb-branch)
      (let ((infos
              (cl-koza-3.kernel::resolve-branch-infos
               adf-branches rpb-branch)))
        (is (eq :boolean (branch-info-result-type (aref infos 0))))
        (is (eq :number (branch-info-result-type (aref infos 1))))
        (is (eq :number (branch-info-result-type (aref infos 2))))
        (is (eq :number
                (mixed-typed-terminal-type (aref infos 0) :arg0)))
        (is (eq :number
                (mixed-typed-terminal-type (aref infos 2) :x)))
        (is (eq :number
                (second
                 (find "IF-NUMBER"
                       (branch-info-function-types (aref infos 2))
                       :key (lambda (entry)
                              (symbol-name (first entry)))
                       :test #'string=))))
        (is (eq :boolean
                (third
                 (find "IF-NUMBER"
                       (branch-info-function-types (aref infos 2))
                       :key (lambda (entry)
                              (symbol-name (first entry)))
                       :test #'string=))))))))

(test mixed-typed-regression-known-program
  (let* ((*number-of-fitness-cases* 27)
         (fitness-cases
           (cl-koza-3.examples.mixed-typed-regression:define-fitness-cases-for-mixed-typed-regression))
         (program
           (cl-koza-3.examples.mixed-typed-regression:known-program)))
    (multiple-value-bind (fitness hits)
        (cl-koza-3.examples.mixed-typed-regression:evaluate-standardized-fitness-for-mixed-typed-regression
         program fitness-cases)
      (is (= 0 fitness))
      (is (= 27 hits)))))

(defun make-mixed-typed-branch-info ()
  (make-test-branch-info
   :rpb :rpb 0 nil
   :function-set '(if-number + - <)
   :function-argument-map '(3 2 2 2)
   :function-types '((if-number :number :boolean :number :number)
                     (+ :number :number :number)
                     (- :number :number :number)
                     (< :boolean :number :number))
   :terminal-set '(x y)
   :terminal-types '((x . :number) (y . :number))
   :result-type :number))

(test mixed-typed-crossover-uses-point-types
  (let* ((info (make-mixed-typed-branch-info))
         (contributing
           (make-adf-program
            :adfs nil
            :rpb '(if-number (< y x) (+ x y) (- x y))
            :branch-info (vector info)))
         (receiving
           (make-adf-program
            :adfs nil
            :rpb '(if-number (< x y) (+ x y) (- x y))
            :branch-info (vector info)))
         (new-program
           (let ((*seed* 12345.0d0))
             (crossover-with-point-typing
              contributing receiving
              :contributing-branch :rpb
              :contributing-point 1
              :receiving-branch :rpb))))
    (is (adf-program-p new-program))
    (is (equal '(if-number (< y x) (+ x y) (- x y))
               (adf-program-rpb new-program)))
    (is (equal '(if-number (< x y) (+ x y) (- x y))
               (adf-program-rpb receiving)))))

(test mixed-typed-crossover-rejects-wrong-point-type
  (let* ((info (make-mixed-typed-branch-info))
         (contributing
           (make-adf-program
            :adfs nil
            :rpb '(if-number (< y x) (+ x y) (- x y))
            :branch-info (vector info)))
         (receiving
           (make-adf-program
            :adfs nil
            :rpb '(if-number (< x y) (+ x y) (- x y))
            :branch-info (vector info))))
    (is (null
         (let ((*seed* 12345.0d0))
           (crossover-with-point-typing
            contributing receiving
            :contributing-branch :rpb
            :contributing-point 2
            :receiving-branch :rpb
            :receiving-point 1))))))

(defun mixed-typed-resolved-branches ()
  (multiple-value-bind (adf-specs rpb-spec)
      (cl-koza-3.examples.mixed-typed-regression:mixed-typed-regression)
    (multiple-value-bind (adf-branches rpb-branch)
        (cl-koza-3.kernel::resolve-branch-specs adf-specs rpb-spec)
      (cl-koza-3.kernel::resolve-terminal-sets adf-branches rpb-branch)
      (values adf-branches
              rpb-branch
              (cl-koza-3.kernel::resolve-branch-infos
               adf-branches rpb-branch)))))

(defun mixed-typed-program-types-valid-p (program)
  (let ((infos (adf-program-branch-info program)))
    (and (= (length infos)
            (1+ (adf-program-branch-count program)))
         (loop for branch in (branch-names program)
               for info across infos
               always (eq (branch-info-result-type info)
                          (cl-koza-3.architecture::typed-tree-type
                           (program-branch program branch)
                           info program))))))

(test mixed-typed-regression-run-keeps-types-valid
  (let ((*parallel-evaluation-p* nil)
        (*parallel-generation-p* nil)
        (*verbose* nil)
        (*seed* 12345.0d0)
        (*number-of-demes* 1)
        (*migration-rate* 0.0))
    (multiple-value-bind (population fitness-cases)
        (run-architecture-altering-system
         'cl-koza-3.examples.mixed-typed-regression:mixed-typed-regression
         1.0 2 12)
      (declare (ignore fitness-cases))
      (is (= 12 (length population)))
      (dotimes (index (length population))
        (is (mixed-typed-program-types-valid-p
             (individual-program (aref population index))))))))

(test mixed-typed-generation-methods-keep-types
  (multiple-value-bind (adf-branches rpb-branch infos)
      (mixed-typed-resolved-branches)
    (dolist (method '(:full :grow :ramped-half-and-half))
      (let ((*method-of-generation* method)
            (*max-depth-for-new-individuals* 5)
            (*seed* 12345.0d0))
        (dotimes (index 8)
          (let ((program
                  (cl-koza-3.kernel::create-new-program
                   index (evenp index) 1 4
                   adf-branches rpb-branch infos)))
            (is (mixed-typed-program-types-valid-p program))))))))

(test mixed-typed-mutation-keeps-types
  (multiple-value-bind (adf-branches rpb-branch infos)
      (mixed-typed-resolved-branches)
    (let ((program
            (make-adf-program
             :adfs (vector '(< arg0 arg1)
                           '(+ (* arg0 arg0) arg1))
             :rpb '(if-number (adf0 x y) (adf1 x z) (- (* y y) z))
             :branch-info infos)))
      (dotimes (index 20)
        (let ((*seed* (+ 100.0d0 index))
              (*max-depth-for-new-subtrees-in-mutants* 4))
          (multiple-value-bind (mutated subtree)
              (cl-koza-3.kernel::mutate
               program adf-branches rpb-branch)
            (declare (ignore subtree))
            (is (mixed-typed-program-types-valid-p mutated))))))))

(test mixed-typed-generation-uses-adf-context
  (multiple-value-bind (adf-branches rpb-branch infos)
      (mixed-typed-resolved-branches)
    (declare (ignore adf-branches rpb-branch))
    (let* ((info (copy-structure (aref infos 2)))
           (program
             (make-adf-program
              :adfs (vector '(+ arg0 arg1)
                            '(+ arg0 arg1))
              :rpb nil
              :branch-info
              (vector (aref infos 0) (aref infos 1) info))))
      (setf (branch-info-function-set info) '(adf1))
      (setf (branch-info-function-argument-map info) '(2))
      (setf (branch-info-function-types info) nil)
      (setf (branch-info-terminal-set info) '(:integer-random-constant))
      (setf (branch-info-terminal-types info)
            '((:integer-random-constant . :number)))
      (setf (branch-info-result-type info) :number)
      (let ((tree
              (let ((*seed* 42.0d0))
                (cl-koza-3.architecture::generate-random-subtree-of-type
                 info :number program))))
        (is (= 1 (cl-koza-3.architecture::adf-symbol-index
                  (first tree))))
        (is (eq :number
                (cl-koza-3.architecture::typed-tree-type
                 tree info program)))))))

(test mixed-typed-branch-deletion-uses-adf-context
  (let* ((adf0-info
           (make-branch-info
            :name :adf0 :kind :adf
            :arity 2 :formals '(arg0 arg1)
            :function-set '(+ - *)
            :function-argument-map '(2 2 2)
            :function-types '((+ :number :number :number)
                              (- :number :number :number)
                              (* :number :number :number))
            :terminal-set '(arg0 arg1 :integer-random-constant)
            :terminal-types '((arg0 . :number)
                              (arg1 . :number)
                              (:integer-random-constant . :number))
            :result-type :number))
         (adf1-info
           (make-branch-info
            :name :adf1 :kind :adf
            :arity 2 :formals '(arg0 arg1)
            :function-set '(+ adf0)
            :function-argument-map '(2 2)
            :function-types '((+ :number :number :number))
            :terminal-set '(arg0 arg1 :integer-random-constant)
            :terminal-types '((arg0 . :number)
                              (arg1 . :number)
                              (:integer-random-constant . :number))
            :result-type :number
            :allowed-branch-indices '(0)))
         (rpb-info
           (make-branch-info
            :name :rpb :kind :rpb
            :arity 0 :formals nil
            :function-set '(adf0 adf1)
            :function-argument-map '(2 2)
            :terminal-set '(:integer-random-constant)
            :terminal-types '((:integer-random-constant . :number))
            :result-type :number
            :allowed-branch-indices '(0 1)))
         (program
           (make-adf-program
            :adfs (vector '(+ arg0 arg1)
                          '(+ arg0 arg1))
            :rpb '(adf1 (adf0 1 2) 3)
            :branch-info (vector adf0-info adf1-info rpb-info))))
    (let ((*seed* 7.0d0)
          (*max-depth-for-new-subtrees-in-mutants* 4))
      (let ((new-program (branch-deletion program :branch 0)))
        (is (not (null new-program)))
        (is (mixed-typed-program-types-valid-p new-program))
        (is (cl-koza-3.architecture::tree-contains-adf-call-p
             (adf-program-rpb new-program) 0))))))
