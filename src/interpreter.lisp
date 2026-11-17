(in-package #:cl-koza-3.interpreter)

(declaim (ftype (function (t t &key (:environment t)) t)
                evaluate-adf-tree))

(defvar *iteration-sequence* :unbound
  "Fallback sequence iterated over by ADI and ADL calls.  A branch's
   ITERATION-SOURCE metadata takes precedence when that symbol is bound
   in the caller's environment.")

(defvar *recursion-depth* 0
  "Current depth used by ADR calls.")

(defvar *recursion-limit* nil
  "Depth limit bound by the outermost ADR call.")

(defvar *storage-cells* nil
  "Hash table of mutable storage cells used by ADS calls.")

(defun binding-key-name (key)
  (etypecase key
    (symbol (symbol-name key))
    (string key)))

(defun environment-value (symbol environment)
  (let ((entry (or (assoc symbol environment :test #'eq)
                   (assoc (symbol-name symbol)
                          environment
                          :test #'string=
                          :key #'binding-key-name))))
    (if entry
        (values (cdr entry) t)
        (values nil nil))))

(defun adf-call-index (operator program)
  (if (symbolp operator)
      (let ((name (symbol-name operator)))
        (when (and (> (length name) 3)
                   (string-equal "ADF" name :end1 3 :end2 3)
                   (every #'digit-char-p (subseq name 3)))
          (let ((index (parse-integer name :start 3)))
            (unless (< index (adf-program-branch-count program))
              (error "Program has no ADF~D." index))
            (values index t))))
      (values nil nil)))

(defun aux-call-index (operator program)
  (if (symbolp operator)
      (let* ((name (symbol-name operator))
             (aux-index
               (cl-koza-3.program::aux-branch-index name)))
        (when aux-index
          (let ((kind (car aux-index))
                (index (cdr aux-index)))
            (unless (< index (cl-koza-3.program::aux-kind-count
                              program kind))
              (error "Program has no ~A~D."
                     (cl-koza-3.program::aux-branch-kind-prefix kind)
                     index))
            (values kind index t))))
      (values nil nil nil)))

(defun adi-call-index (operator program)
  (multiple-value-bind (kind index call-p)
      (aux-call-index operator program)
    (if (and call-p (eq kind :adi))
        (values index t)
        (values nil nil))))

(defun adi-symbol (index &optional (package :keyword))
  (intern (format nil "ADI~D" index) (or package :keyword)))

(defun adi-sequence (info environment)
  (let* ((source (branch-info-iteration-source info))
         (found-p nil)
         (value nil))
    (when source
      (multiple-value-setq (value found-p)
        (environment-value source environment)))
    (cond (found-p value)
          ((not (eq *iteration-sequence* :unbound))
           *iteration-sequence*)
          (t
           (error "No iteration sequence is available for branch ~S."
                  (branch-info-name info))))))

(defun adi-iteration-variable (info)
  (or (branch-info-iteration-variable info) :item))

(defun adi-iteration-index-variable (info)
  (or (branch-info-iteration-index-variable info) :index))

(defun branch-state-values (info state)
  (let ((arity (branch-info-arity info)))
    (cond ((zerop arity) nil)
          ((= arity 1) (list state))
          (t (coerce state 'list)))))

(defun initial-branch-state (info argument-values)
  (let ((arity (branch-info-arity info)))
    (unless (= (length argument-values) arity)
      (error "Branch ~S expects ~D argument~:P, got ~D."
             (branch-info-name info) arity (length argument-values)))
    (cond ((zerop arity) nil)
          ((= arity 1) (first argument-values))
          (t (copy-list argument-values)))))

(defun next-branch-state (info values)
  (let ((arity (branch-info-arity info)))
    (cond ((zerop arity) nil)
          ((= arity 1) (first values))
          ((and (= (length values) 1)
                (listp (first values))
                (= (length (first values)) arity))
           (copy-list (first values)))
          ((= (length values) arity)
           (copy-list values))
          (t
           (error "Branch ~S must produce ~D state values, got ~S."
                  (branch-info-name info) arity values)))))

(defun aux-iteration-environment (info state item index)
  (append
   (loop for formal in (branch-info-formals info)
         for value in (branch-state-values info state)
         collect (cons formal value))
   (list (cons (adi-iteration-variable info) item)
         (cons (adi-iteration-index-variable info) index))))

(defun aux-source-value (info environment)
  (let* ((source (branch-info-iteration-source info))
         (found-p nil)
         (value nil))
    (when source
      (multiple-value-setq (value found-p)
        (environment-value source environment)))
    (cond (found-p value)
          ((numberp source) source)
          ((not (eq *iteration-sequence* :unbound))
           *iteration-sequence*)
          (t
           (error "No iteration control value is available for branch ~S."
                  (branch-info-name info))))))

(defun aux-repeat-count (info environment)
  (let ((value (aux-source-value info environment)))
    (cond ((integerp value) value)
          ((listp value) (length value))
          ((vectorp value) (length value))
          (t
           (error "Branch ~S needs an integer or sequence repeat count, got ~S."
                  (branch-info-name info) value)))))

(defun aux-depth-limit (info environment)
  (let ((value (if *recursion-limit*
                   *recursion-limit*
                   (aux-source-value info environment))))
    (unless (integerp value)
      (error "Branch ~S needs an integer recursion limit, got ~S."
             (branch-info-name info) value))
    value))

(defun aux-storage-key (info environment)
  (let* ((source (branch-info-iteration-source info))
         (found-p nil)
         (value nil))
    (when source
      (multiple-value-setq (value found-p)
        (environment-value source environment)))
    (cond (found-p value)
          (source source)
          (t (branch-info-name info)))))

(defun evaluate-adi-call (adi-index arguments environment program)
  (let* ((branch (cons :adi adi-index))
         (info (program-branch-info program branch))
         (body (program-branch program branch))
         (argument-values
           (mapcar (lambda (argument)
                     (evaluate-adf-tree argument program
                                        :environment environment))
                   arguments))
         (sequence (coerce (adi-sequence info environment) 'list))
         (state (initial-branch-state info argument-values)))
    (let ((*iteration-sequence* sequence))
      (loop for item in sequence
            for index from 0
            do (setf state
                     (next-branch-state
                      info
                      (multiple-value-list
                       (evaluate-adf-tree
                        body
                        program
                        :environment
                        (aux-iteration-environment
                         info state item index)))))))
    state))

(defun evaluate-adl-call (adl-index arguments environment program)
  (let* ((branch (cons :adl adl-index))
         (info (program-branch-info program branch))
         (body (program-branch program branch))
         (argument-values
           (mapcar (lambda (argument)
                     (evaluate-adf-tree argument program
                                        :environment environment))
                   arguments))
         (count (aux-repeat-count info environment))
         (state (initial-branch-state info argument-values)))
    (let ((*iteration-sequence* (loop for index below count collect index)))
      (loop for index below count
            do (setf state
                     (next-branch-state
                      info
                      (multiple-value-list
                       (evaluate-adf-tree
                        body
                        program
                        :environment
                        (aux-iteration-environment
                         info state state index)))))))
    state))

(defun evaluate-adr-call (adr-index arguments environment program)
  (let* ((branch (cons :adr adr-index))
         (info (program-branch-info program branch))
         (body (program-branch program branch))
         (argument-values
           (mapcar (lambda (argument)
                     (evaluate-adf-tree argument program
                                        :environment environment))
                   arguments))
         (state (initial-branch-state info argument-values))
         (limit (aux-depth-limit info environment))
         (depth *recursion-depth*))
    (if (>= depth limit)
        state
        (let ((*recursion-depth* (1+ depth))
              (*recursion-limit* limit))
          (next-branch-state
           info
           (multiple-value-list
            (evaluate-adf-tree
             body
             program
             :environment
             (aux-iteration-environment info state depth depth))))))))

(defun evaluate-ads-call (ads-index arguments environment program)
  (let* ((branch (cons :ads ads-index))
         (info (program-branch-info program branch))
         (body (program-branch program branch))
         (argument-values
           (mapcar (lambda (argument)
                     (evaluate-adf-tree argument program
                                        :environment environment))
                   arguments))
         (key (aux-storage-key info environment))
         (cells (or *storage-cells*
                    (setf *storage-cells* (make-hash-table :test #'equal))))
         (initial (initial-branch-state info argument-values)))
    (multiple-value-bind (current found-p)
        (gethash key cells)
      (unless found-p
        (setf current initial
              (gethash key cells) initial))
      (let ((state
              (next-branch-state
               info
               (multiple-value-list
                (evaluate-adf-tree
                 body
                 program
                 :environment
                 (aux-iteration-environment
                  info current current 0))))))
        (setf (gethash key cells) state)
        state))))

(defun evaluate-aux-call (kind index arguments environment program)
  (ecase kind
    (:adi (evaluate-adi-call index arguments environment program))
    (:adl (evaluate-adl-call index arguments environment program))
    (:adr (evaluate-adr-call index arguments environment program))
    (:ads (evaluate-ads-call index arguments environment program))))

(defun argument-bindings (values)
  (loop for value in values
        for index from 0
        collect (cons (format nil "ARG~D" index) value)))

(defun evaluate-and (arguments environment program)
  (let ((result t))
    (dolist (argument arguments result)
      (setf result (evaluate-adf-tree argument program
                                      :environment environment))
      (unless result
        (return nil)))))

(defun evaluate-or (arguments environment program)
  (dolist (argument arguments nil)
    (let ((result (evaluate-adf-tree argument program
                                     :environment environment)))
      (when result
        (return result)))))

(defun evaluate-if (arguments environment program)
  (let ((test (evaluate-adf-tree (first arguments) program
                                 :environment environment)))
    (cond ((and test (second arguments))
           (evaluate-adf-tree (second arguments) program
                              :environment environment))
          ((and (not test) (third arguments))
           (evaluate-adf-tree (third arguments) program
                              :environment environment))
          (t nil))))

(defun evaluate-progn (arguments environment program)
  (let ((result nil))
    (dolist (argument arguments result)
      (setf result (evaluate-adf-tree argument program
                                      :environment environment)))))

(defun evaluate-function-call (operator arguments environment program)
  (case operator
    ((and) (evaluate-and arguments environment program))
    ((or) (evaluate-or arguments environment program))
    ((if) (evaluate-if arguments environment program))
    ((progn) (evaluate-progn arguments environment program))
    (otherwise
     (unless (fboundp operator)
       (error "~S is not a bound function." operator))
     (apply (symbol-function operator)
            (mapcar (lambda (argument)
                      (evaluate-adf-tree argument program
                                         :environment environment))
                    arguments)))))

(defun evaluate-adf-tree (tree program &key environment)
  "Evaluates TREE in the context of PROGRAM.

    ENVIRONMENT is an alist of terminal or argument bindings.  ADF and
    auxiliary calls are resolved against PROGRAM by branch index.  Branch
    bodies use lexical scope: they see only their formal arguments and,
    for auxiliary branches, their iteration or storage bindings."
  (cond
    ((consp tree)
     (let ((operator (first tree))
           (arguments (rest tree)))
        (multiple-value-bind (adf-index adf-call-p)
            (adf-call-index operator program)
          (multiple-value-bind (aux-kind aux-index aux-call-p)
              (aux-call-index operator program)
            (cond
              (adf-call-p
               (let* ((argument-values
                        (mapcar (lambda (argument)
                                  (evaluate-adf-tree
                                   argument program
                                   :environment environment))
                                arguments))
                      (adf-environment
                        (argument-bindings argument-values)))
                 (evaluate-adf-tree (adf-program-adf program adf-index)
                                    program
                                    :environment adf-environment)))
              (aux-call-p
               (evaluate-aux-call aux-kind aux-index arguments
                                  environment program))
              (t
               (evaluate-function-call
                operator arguments environment program)))))))

    ((symbolp tree)
     (cond ((eq tree t) t)
           ((eq tree nil) nil)
           (t
            (multiple-value-bind (value found-p)
                (environment-value tree environment)
              (unless found-p
                (error "No binding for terminal ~S." tree))
              value))))
    (t tree)))

(defun evaluate-adf-branch (program branch &key environment
                                              (iteration-sequence nil
                                                                  iteration-sequence-p))
  "Evaluates BRANCH of PROGRAM in ENVIRONMENT."
  (let ((*iteration-sequence*
          (if iteration-sequence-p
              iteration-sequence
              *iteration-sequence*)))
    (evaluate-adf-tree (program-branch program branch)
                       program
                       :environment environment)))

(defun evaluate-adf-program (program &key environment
                                          (iteration-sequence nil
                                                              iteration-sequence-p))
  "Evaluates the result-producing branch of PROGRAM in ENVIRONMENT."
  (let ((*iteration-sequence*
          (if iteration-sequence-p
              iteration-sequence
              *iteration-sequence*)))
    (evaluate-adf-branch program :rpb :environment environment)))
