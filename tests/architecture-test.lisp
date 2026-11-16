(in-package #:cl-koza-3.tests)

(in-suite cl-koza-3-tests)

(defmacro with-architecture-test-seed (&body body)
  `(let ((*seed* 12345.0d0))
     ,@body))

(defun test-function-arity (info name)
  (loop for function in (branch-info-function-set info)
        for arity in (branch-info-function-argument-map info)
        when (and (symbolp function)
                  (string= (symbol-name function) name))
          return arity))

(defun test-function-present-p (info name)
  (and (test-function-arity info name) t))

(defun test-tree-contains-name-p (tree name)
  (cond ((consp tree)
         (or (test-tree-contains-name-p (car tree) name)
             (some (lambda (argument)
                     (test-tree-contains-name-p argument name))
                   (cdr tree))))
        ((symbolp tree)
         (string= (symbol-name tree) name))
        (t nil)))

(defun boolean-combinations (count)
  (if (zerop count)
      (list nil)
      (loop for rest in (boolean-combinations (1- count))
            append (list (cons nil rest)
                         (cons t rest)))))

(defun evaluate-program-over-inputs (program count)
  (loop for values in (boolean-combinations count)
        collect
        (evaluate-adf-program
         program
         :environment
         (loop for name in '(:d0 :d1 :d2 :d3 :d4)
               for value in values
               collect (cons name value)))))

(test branch-duplication-preserves-semantics
  (let* ((adf-info (make-test-branch-info
                    :adf0 :adf 2 '(:arg0 :arg1)
                    :function-set '(and)
                    :function-argument-map '(2)
                    :terminal-set '(:arg0 :arg1)))
         (rpb-info (make-test-branch-info
                    :rpb :rpb 0 nil
                    :function-set '(or adf0)
                    :function-argument-map '(2 2)
                    :terminal-set '(:d0 :d1)
                    :allowed-branch-indices '(0)))
         (program (make-adf-program
                   :adfs '((and arg0 arg1))
                   :rpb '(or (adf0 d0 d1) (adf0 d1 d0))
                   :branch-info (vector adf-info rpb-info)))
         (new-program
                       (with-architecture-test-seed
                         (branch-duplication program :branch 0))))
    (is (adf-program-p new-program))
    (is (= 2 (adf-program-branch-count new-program)))
    (is (equal '(and arg0 arg1) (adf-program-adf new-program 1)))
    (let ((new-adf-info (program-branch-info new-program :adf1))
          (new-rpb-info (program-branch-info new-program :rpb)))
      (is (eq :adf1 (branch-info-name new-adf-info)))
      (is (= 2 (branch-info-arity new-adf-info)))
      (is (equal '(:arg0 :arg1) (branch-info-formals new-adf-info)))
      (is (test-function-present-p new-rpb-info "ADF1"))
      (is (= 2 (test-function-arity new-rpb-info "ADF1")))
      (is (member 1 (branch-info-allowed-branch-indices new-rpb-info))))
    (is (equal (evaluate-program-over-inputs program 2)
               (evaluate-program-over-inputs new-program 2)))))

(test argument-duplication-preserves-semantics
  (let* ((adf-info (make-test-branch-info
                    :adf0 :adf 2 '(:arg0 :arg1)
                    :function-set '(and)
                    :function-argument-map '(2)
                    :terminal-set '(:arg0 :arg1)))
         (rpb-info (make-test-branch-info
                    :rpb :rpb 0 nil
                    :function-set '(adf0)
                    :function-argument-map '(2)
                    :terminal-set '(:d0 :d1)
                    :allowed-branch-indices '(0)))
         (program (make-adf-program
                   :adfs '((and arg0 arg1))
                   :rpb '(adf0 d0 d1)
                   :branch-info (vector adf-info rpb-info)))
         (new-program
                       (with-architecture-test-seed
                         (argument-duplication
                          program :branch 0 :argument 1))))
    (is (adf-program-p new-program))
    (let ((new-adf-info (program-branch-info new-program :adf0))
          (new-rpb-info (program-branch-info new-program :rpb)))
      (is (= 3 (branch-info-arity new-adf-info)))
      (is (equal '(:arg0 :arg1 :arg2)
                 (branch-info-formals new-adf-info)))
      (is (member :arg2 (branch-info-terminal-set new-adf-info)))
      (is (= 3 (test-function-arity new-rpb-info "ADF0")))
      (is (= 3 (length (rest (adf-program-rpb new-program))))))
    (is (equal (evaluate-program-over-inputs program 2)
               (evaluate-program-over-inputs new-program 2)))))

(test argument-creation-preserves-semantics
  (let* ((adf-info (make-test-branch-info
                    :adf0 :adf 2 '(:arg0 :arg1)
                    :function-set '(and)
                    :function-argument-map '(2)
                    :terminal-set '(:arg0 :arg1)))
         (rpb-info (make-test-branch-info
                    :rpb :rpb 0 nil
                    :function-set '(adf0)
                    :function-argument-map '(2)
                    :terminal-set '(:d0 :d1)
                    :allowed-branch-indices '(0)))
         (program (make-adf-program
                   :adfs '((and arg0 arg1))
                   :rpb '(adf0 d0 d1)
                   :branch-info (vector adf-info rpb-info)))
         (new-program
                       (with-architecture-test-seed
                         (argument-creation
                          program :branch 0 :point 2))))
    (is (adf-program-p new-program))
    (is (equal '(and arg0 :arg2) (adf-program-adf new-program 0)))
    (let ((new-adf-info (program-branch-info new-program :adf0))
          (new-rpb-info (program-branch-info new-program :rpb)))
      (is (= 3 (branch-info-arity new-adf-info)))
      (is (equal '(:arg0 :arg1 :arg2)
                 (branch-info-formals new-adf-info)))
      (is (= 3 (test-function-arity new-rpb-info "ADF0")))
      (is (= 3 (length (rest (adf-program-rpb new-program))))))
    (is (equal (evaluate-program-over-inputs program 2)
               (evaluate-program-over-inputs new-program 2)))))

(test branch-creation-preserves-semantics
  (let* ((adf-info (make-test-branch-info
                    :adf0 :adf 2 '(:arg0 :arg1)
                    :function-set '(or)
                    :function-argument-map '(2)
                    :terminal-set '(:arg0 :arg1)))
         (rpb-info (make-test-branch-info
                    :rpb :rpb 0 nil
                    :function-set '(and or)
                    :function-argument-map '(2 2)
                    :terminal-set '(:d0 :d1 :d2)
                    :allowed-branch-indices '(0)))
         (program (make-adf-program
                   :adfs '((or arg0 arg1))
                   :rpb '(and d0 (or d1 d2))
                   :branch-info (vector adf-info rpb-info)))
         (new-program
           (with-architecture-test-seed
             (let ((*branch-creation-designation-probability* 0))
               (branch-creation program :branch :rpb :point 2)))))
    (is (adf-program-p new-program))
    (is (= 2 (adf-program-branch-count new-program)))
    (is (equal '(or :arg0 :arg1) (adf-program-adf new-program 1)))
    (is (equal '(and d0 (:adf1 d1 d2))
               (adf-program-rpb new-program)))
    (let ((new-adf-info (program-branch-info new-program :adf1))
          (new-rpb-info (program-branch-info new-program :rpb)))
      (is (eq :adf1 (branch-info-name new-adf-info)))
      (is (= 2 (branch-info-arity new-adf-info)))
      (is (equal '(:arg0 :arg1) (branch-info-formals new-adf-info)))
      (is (not (member :d1 (branch-info-terminal-set new-adf-info))))
      (is (= 2 (test-function-arity new-rpb-info "ADF1")))
      (is (member 1 (branch-info-allowed-branch-indices new-rpb-info))))
    (is (equal (evaluate-program-over-inputs program 3)
               (evaluate-program-over-inputs new-program 3)))))

(test branch-deletion-renumbers-and-regenerates
  (let* ((adf0-info (make-test-branch-info
                     :adf0 :adf 2 '(:arg0 :arg1)
                     :function-set '(or)
                     :function-argument-map '(2)
                     :terminal-set '(:arg0 :arg1)
                     :allowed-branch-indices '()))
         (adf1-info (make-test-branch-info
                     :adf1 :adf 2 '(:arg0 :arg1)
                     :function-set '(and adf0)
                     :function-argument-map '(2 2)
                     :terminal-set '(:arg0 :arg1)
                     :allowed-branch-indices '(0)))
         (rpb-info (make-test-branch-info
                    :rpb :rpb 0 nil
                    :function-set '(or adf0 adf1)
                    :function-argument-map '(2 2 2)
                    :terminal-set '(:d0 :d1)
                    :allowed-branch-indices '(0 1)))
         (program (make-adf-program
                   :adfs '((or arg0 arg1)
                           (and (adf0 arg0 arg1) arg0))
                   :rpb '(or (adf0 d0 d1) (adf1 d1 d0))
                   :branch-info (vector adf0-info adf1-info rpb-info)))
         (new-program
                       (with-architecture-test-seed
                         (branch-deletion program :branch 0))))
    (is (adf-program-p new-program))
    (is (= 1 (adf-program-branch-count new-program)))
    (is (eq :adf0
            (branch-info-name
             (program-branch-info new-program :adf0))))
    (is (not (test-tree-contains-name-p
              (adf-program-rpb new-program) "ADF1")))
    (let ((new-adf-info (program-branch-info new-program :adf0))
          (new-rpb-info (program-branch-info new-program :rpb)))
      (is (test-function-present-p new-rpb-info "ADF0"))
      (is (not (test-function-present-p new-rpb-info "ADF1")))
      (is (equal '(0)
                 (branch-info-allowed-branch-indices new-rpb-info)))
      (is (not (member :adf1 (branch-info-function-set new-adf-info)))))
    (is (member (evaluate-adf-program
                 new-program
                 :environment '((:d0 . nil) (:d1 . nil)))
                '(nil t)))))

(test argument-deletion-renumbers-and-regenerates
  (let* ((adf-info (make-test-branch-info
                    :adf0 :adf 3 '(:arg0 :arg1 :arg2)
                    :function-set '(or and)
                    :function-argument-map '(2 2)
                    :terminal-set '(:arg0 :arg1 :arg2)))
         (rpb-info (make-test-branch-info
                    :rpb :rpb 0 nil
                    :function-set '(adf0)
                    :function-argument-map '(3)
                    :terminal-set '(:d0 :d1 :d2)
                    :allowed-branch-indices '(0)))
         (program (make-adf-program
                   :adfs '((or arg0 (and arg1 arg2)))
                   :rpb '(adf0 d0 d1 d2)
                   :branch-info (vector adf-info rpb-info)))
         (new-program
                       (with-architecture-test-seed
                         (argument-deletion
                          program :branch 0 :argument 1))))
    (is (adf-program-p new-program))
    (let ((new-adf-info (program-branch-info new-program :adf0))
          (new-rpb-info (program-branch-info new-program :rpb)))
      (is (= 2 (branch-info-arity new-adf-info)))
      (is (equal '(:arg0 :arg1) (branch-info-formals new-adf-info)))
      (is (not (member :arg2 (branch-info-terminal-set new-adf-info))))
      (is (= 2 (test-function-arity new-rpb-info "ADF0")))
      (is (= 2 (length (rest (adf-program-rpb new-program)))))
      (is (not (test-tree-contains-name-p
                (adf-program-adf new-program 0) "ARG2"))))
    (is (member (evaluate-adf-program
                 new-program
                 :environment '((:d0 . nil) (:d1 . nil) (:d2 . nil)))
                '(nil t)))))

(test point-typed-crossover-valid-fragment
  (let* ((info (make-test-branch-info
                :rpb :rpb 0 nil
                :function-set '(and or)
                :function-argument-map '(2 2)
                :terminal-set '(:d0 :d1)))
         (contributing (make-adf-program
                        :adfs nil
                        :rpb '(and d0 d1)
                        :branch-info (vector info)))
         (receiving (make-adf-program
                     :adfs nil
                     :rpb '(or d0 d1)
                     :branch-info (vector info)))
         (new-program
           (with-architecture-test-seed
             (crossover-with-point-typing
              contributing receiving
              :contributing-branch :rpb
              :contributing-point 1
              :receiving-branch :rpb
              :receiving-point 0))))
    (is (adf-program-p new-program))
    (is (equal 'd0 (adf-program-rpb new-program)))
    (is (equal '(or d0 d1) (adf-program-rpb receiving)))))

(test point-typed-crossover-rejects-incompatible-fragment
  (let* ((contributing-info
           (make-test-branch-info
            :rpb :rpb 0 nil
            :function-set '(and)
            :function-argument-map '(2)
            :terminal-set '(:d0 :d1)))
         (receiving-info
           (make-test-branch-info
            :rpb :rpb 0 nil
            :function-set '(or)
            :function-argument-map '(2)
            :terminal-set '(:d0 :d1)))
         (contributing (make-adf-program
                        :adfs nil
                        :rpb '(and d0 d1)
                        :branch-info (vector contributing-info)))
         (receiving (make-adf-program
                     :adfs nil
                     :rpb '(or d0 d1)
                     :branch-info (vector receiving-info))))
    (is (null
         (with-architecture-test-seed
           (crossover-with-point-typing
            contributing receiving
            :contributing-branch :rpb
            :contributing-point 0
            :receiving-branch :rpb
            :receiving-point 0))))))

(defun make-adi-architecture-program
    (&key (adi-body '(adf0 arg0 item)))
  (let* ((adf-info
           (make-test-branch-info
            :adf0 :adf 2 '(:arg0 :arg1)
            :function-set '(+)
            :function-argument-map '(2)
            :function-types '((+ :number :number :number))
            :terminal-set '(:arg0 :arg1)
            :terminal-types '((:arg0 . :number) (:arg1 . :number))
            :result-type :number))
         (adi-info
           (make-branch-info
            :name :adi0
            :kind :adi
            :arity 1
            :formals '(:arg0)
            :function-set '(adf0 +)
            :function-argument-map '(2 2)
            :function-types '((adf0 :number :number :number)
                              (+ :number :number :number))
            :terminal-set '(:arg0 :item :index)
            :terminal-types '((:arg0 . :number)
                              (:item . :number)
                              (:index . :number))
            :result-type :number
            :allowed-branch-indices '(0)
            :iteration-source :data
            :iteration-variable :item
            :iteration-index-variable :index))
         (rpb-info
           (make-test-branch-info
            :rpb :rpb 0 nil
            :function-set '(adi0)
            :function-argument-map '(1)
            :function-types '((adi0 :number :number))
            :terminal-set '(:d0)
            :terminal-types '((:d0 . :number))
            :result-type :number
            :allowed-branch-indices '(0))))
    (make-adf-program
     :adfs (list '(+ arg0 arg1))
     :adis (list adi-body)
     :rpb '(adi0 0)
     :branch-info (vector adf-info adi-info rpb-info))))

(test branch-duplication-preserves-adis
  (let* ((program (make-adi-architecture-program))
         (new-program
           (let ((*seed* 123456.0d0))
             (branch-duplication program :branch 0)))
         (adi-info (program-branch-info new-program :adi0)))
    (is (adf-program-p new-program))
    (is (= 2 (adf-program-branch-count new-program)))
    (is (= 1 (adf-program-adi-count new-program)))
    (is (eq :adi0 (branch-info-name adi-info)))
    (is (test-function-present-p adi-info "ADF1"))
    (is (= 2 (test-function-arity adi-info "ADF1")))
    (is (equal '(adf1 arg0 item)
               (adf-program-adi new-program 0)))))

(test argument-duplication-preserves-adis
  (let* ((program (make-adi-architecture-program))
         (new-program
           (with-architecture-test-seed
             (argument-duplication
              program :branch 0 :argument 0)))
         (adi-info (program-branch-info new-program :adi0)))
    (is (= 3 (branch-info-arity
              (program-branch-info new-program :adf0))))
    (is (= 1 (adf-program-adi-count new-program)))
    (is (= 3 (test-function-arity adi-info "ADF0")))
    (is (= 3 (length (rest (adf-program-adi new-program 0)))))))

(test argument-creation-preserves-adis
  (let* ((program (make-adi-architecture-program))
         (new-program
           (with-architecture-test-seed
             (argument-creation
              program :branch 0 :point 0)))
         (adi-info (program-branch-info new-program :adi0)))
    (is (= 1 (adf-program-adi-count new-program)))
    (is (= 3 (test-function-arity adi-info "ADF0")))
    (is (equal '(adf0 arg0 item (+ arg0 item))
               (adf-program-adi new-program 0)))))

(test branch-creation-from-adi-preserves-adis
  (let* ((program (make-adi-architecture-program))
         (new-program
           (with-architecture-test-seed
             (let ((*branch-creation-designation-probability* 0))
               (branch-creation program :branch :adi0 :point 0))))
         (adi-info (program-branch-info new-program :adi0)))
    (is (adf-program-p new-program))
    (is (= 2 (adf-program-branch-count new-program)))
    (is (= 1 (adf-program-adi-count new-program)))
    (is (eq :adi0 (branch-info-name adi-info)))
    (is (test-function-present-p adi-info "ADF1"))
    (is (equal '(:adf1 arg0 item)
               (adf-program-adi new-program 0)))
    (is (equal '(adf0 :arg0 :arg1)
               (adf-program-adf new-program 1)))))

(test branch-creation-from-rpb-preserves-adis
  (let* ((program (make-adi-architecture-program))
         (new-program
           (with-architecture-test-seed
             (branch-creation program :branch :rpb :point 0)))
         (infos (adf-program-branch-info new-program)))
    (is (= 2 (adf-program-branch-count new-program)))
    (is (= 1 (adf-program-adi-count new-program)))
    (is (equal '(adi0 0) (adf-program-adf new-program 1)))
    (is (equal '(:adf1) (adf-program-rpb new-program)))
    (is (eq :adi0 (branch-info-name (aref infos 2))))
    (is (equal '(adf0 arg0 item)
               (adf-program-adi new-program 0)))))

(test branch-deletion-preserves-adis
  (let* ((program (make-adi-architecture-program))
         (new-program
           (with-architecture-test-seed
             (branch-deletion program :branch 0)))
         (infos (adf-program-branch-info new-program)))
    (is (adf-program-p new-program))
    (is (zerop (adf-program-branch-count new-program)))
    (is (= 1 (adf-program-adi-count new-program)))
    (is (= 2 (length infos)))
    (is (eq :adi0 (branch-info-name (aref infos 0))))
    (is (eq :rpb (branch-info-name (aref infos 1))))
    (is (not (test-function-present-p (aref infos 0) "ADF0")))
    (is (not (test-tree-contains-name-p
              (adf-program-adi new-program 0) "ADF0")))))

(test argument-deletion-preserves-adis
  (let* ((program (make-adi-architecture-program))
         (new-program
           (with-architecture-test-seed
             (argument-deletion
              program :branch 0 :argument 0)))
         (adi-info (program-branch-info new-program :adi0)))
    (is (= 1 (adf-program-adi-count new-program)))
    (is (= 1 (branch-info-arity
              (program-branch-info new-program :adf0))))
    (is (= 1 (test-function-arity adi-info "ADF0")))
    (is (equal '(adf0 item)
               (adf-program-adi new-program 0)))))

(test point-typed-crossover-preserves-adis
  (let* ((contributing
           (make-adi-architecture-program
            :adi-body '(adf0 arg0 item)))
         (receiving
           (make-adi-architecture-program
            :adi-body '(+ arg0 item)))
         (new-program
           (with-architecture-test-seed
             (crossover-with-point-typing
              contributing receiving
              :contributing-branch :adi0
              :contributing-point 0
              :receiving-branch :adi0
              :receiving-point 0))))
    (is (adf-program-p new-program))
    (is (= 1 (adf-program-adi-count new-program)))
    (is (equal '(adf0 arg0 item)
               (adf-program-adi new-program 0)))
    (is (equal '(+ arg0 item)
               (adf-program-adi receiving 0)))))
