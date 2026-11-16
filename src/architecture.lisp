(in-package #:cl-koza-3.architecture)

(defparameter *branch-creation-designation-probability* 1/2
  "Probability that a node below a branch-creation point becomes a
   formal argument of the newly created branch.")

(defvar *branch-duplication-fraction* 0.0
  "Fraction of each generation produced by branch duplication.")

(defvar *argument-duplication-fraction* 0.0
  "Fraction of each generation produced by argument duplication.")

(defvar *branch-creation-fraction* 0.0
  "Fraction of each generation produced by branch creation.")

(defvar *argument-creation-fraction* 0.0
  "Fraction of each generation produced by argument creation.")

(defvar *branch-deletion-fraction* 0.0
  "Fraction of each generation produced by branch deletion.")

(defvar *argument-deletion-fraction* 0.0
  "Fraction of each generation produced by argument deletion.")

(defvar *adi-creation-fraction* 0.0
  "Fraction of each generation produced by ADI creation.")

(defvar *adi-deletion-fraction* 0.0
  "Fraction of each generation produced by ADI deletion.")

(defvar *adi-creation-iteration-source* :data
  "Default iteration source for an ADI created when no ADI metadata is
   available from the selected program.")

(defvar *adi-creation-iteration-variable* :item
  "Default iteration element variable for newly created ADIs.")

(defvar *adi-creation-iteration-index-variable* :index
  "Default iteration index variable for newly created ADIs.")

(defvar *adi-creation-iteration-item-type* :number
  "Type assigned to the iteration element when a new ADI is created.")

(defun same-symbol-name-p (left right)
  (and (symbolp left)
       (symbolp right)
       (string= (symbol-name left) (symbol-name right))))

(defun name-member-p (symbol sequence)
  (and (symbolp symbol)
       (loop for item in sequence
             thereis (same-symbol-name-p symbol item))))

(defun adf-symbol-p (symbol)
  (and (symbolp symbol)
       (let ((name (symbol-name symbol)))
         (and (> (length name) 3)
              (string= name "ADF" :end1 3)
              (every #'digit-char-p (subseq name 3))))))

(defun adf-symbol-index (symbol)
  (when (adf-symbol-p symbol)
    (parse-integer (symbol-name symbol) :start 3)))

(defun adf-symbol (index &optional (package :keyword))
  (intern (format nil "ADF~D" index) (or package :keyword)))

(defun adi-symbol-p (symbol)
  (and (symbolp symbol)
       (let ((name (symbol-name symbol)))
         (and (> (length name) 3)
              (string= name "ADI" :end1 3)
              (every #'digit-char-p (subseq name 3))))))

(defun adi-symbol-index (symbol)
  (when (adi-symbol-p symbol)
    (parse-integer (symbol-name symbol) :start 3)))

(defun adi-symbol (index &optional (package :keyword))
  (intern (format nil "ADI~D" index) (or package :keyword)))

(defun argument-symbol (index &optional (package :keyword))
  (intern (format nil "ARG~D" index) (or package :keyword)))

(defun function-set-index (info function)
  (when (symbolp function)
    (loop for item in (branch-info-function-set info)
          for index from 0
          when (same-symbol-name-p function item)
            return index)))

(defun function-arity (info function)
  (let ((index (function-set-index info function)))
    (when (and index
               (< index (length (branch-info-function-argument-map info))))
      (nth index (branch-info-function-argument-map info)))))

(defun function-type-entry (info function)
  (when (symbolp function)
    (find function
          (branch-info-function-types info)
          :key #'first
          :test #'same-symbol-name-p)))

(defun terminal-type-entry (info terminal)
  (when (symbolp terminal)
    (find terminal
          (branch-info-terminal-types info)
          :key #'first
          :test #'same-symbol-name-p)))

(defun terminal-type (info terminal)
  (cond
    ((or (eq terminal :integer-random-constant)
         (eq terminal :floating-point-random-constant))
     :number)
    ((numberp terminal) :number)
    ((symbolp terminal)
     (or (let ((entry (terminal-type-entry info terminal)))
           (and entry (cdr entry)))
         :boolean))
    (t :boolean)))

(defun branch-call-index (function)
  (or (adf-symbol-index function)
      (adi-symbol-index function)))

(defun branch-call-target-info (function program)
  (let ((adf-index (adf-symbol-index function)))
    (cond
      (adf-index
       (and program
            (< adf-index (adf-program-branch-count program))
            (program-branch-info program adf-index)))
      ((adi-symbol-index function)
       (and program
            (program-branch-info
             program
             (adi-symbol (adi-symbol-index function)))))
      (t nil))))

(defun branch-call-allowed-p (info function program)
  (let ((target-info (branch-call-target-info function program))
        (adf-index (adf-symbol-index function)))
    (and target-info
         (function-arity info function)
         (if adf-index
             (or (null (branch-info-allowed-branch-indices info))
                 (member adf-index
                         (branch-info-allowed-branch-indices info)))
             t))))

(defun function-result-type-in-info (info function program)
  (let ((target-info (branch-call-target-info function program)))
    (if target-info
        (branch-info-result-type target-info)
        (let ((entry (function-type-entry info function)))
          (if entry
              (second entry)
              :boolean)))))

(defun function-argument-types-in-info (info function program)
  (let ((target-info (branch-call-target-info function program)))
    (if target-info
        (loop for formal in (branch-info-formals target-info)
              collect (terminal-type target-info formal))
        (let* ((arity (function-arity info function))
               (entry (function-type-entry info function))
               (declared (and entry (cddr entry))))
          (loop for index below (or arity 0)
                collect (or (nth index declared) :boolean))))))

(defun set-terminal-type (info terminal type)
  (when (or (branch-info-terminal-types info)
            (not (eq type :boolean)))
    (setf (branch-info-terminal-types info)
          (acons terminal type
                 (remove-if
                  (lambda (entry)
                    (same-symbol-name-p terminal (car entry)))
                  (branch-info-terminal-types info)))))
  info)

(defun valid-constant-p (constant terminal-set)
  (or (member constant terminal-set :test #'equal)
      (and (integerp constant)
           (name-member-p :integer-random-constant terminal-set))
      (and (floatp constant)
           (name-member-p :floating-point-random-constant terminal-set))))

(defun typed-tree-type (tree info &optional program)
  "Returns the type of TREE under INFO, or NIL when TREE is invalid."
  (cond
    ((consp tree)
     (let* ((operator (first tree))
            (arguments (rest tree))
            (call-p (branch-call-index operator))
            (target-info (branch-call-target-info operator program)))
       (if call-p
           (when (and target-info
                      (branch-call-allowed-p info operator program)
                      (= (length arguments)
                         (branch-info-arity target-info)))
             (let ((argument-types
                     (function-argument-types-in-info
                      info operator program)))
               (when (loop for argument in arguments
                           for type in argument-types
                           always (and type
                                       (eq (typed-tree-type
                                            argument info program)
                                           type)))
                 (branch-info-result-type target-info))))
           (let ((arity (function-arity info operator)))
             (when (and arity (= (length arguments) arity))
               (let ((argument-types
                       (function-argument-types-in-info
                        info operator program)))
                 (when (loop for argument in arguments
                             for type in argument-types
                             always (and type
                                         (eq (typed-tree-type
                                              argument info program)
                                             type)))
                   (function-result-type-in-info
                    info operator program))))))))
    ((symbolp tree)
     (when (name-member-p tree (branch-info-terminal-set info))
       (terminal-type info tree)))
    ((numberp tree)
     (when (valid-constant-p tree (branch-info-terminal-set info))
       :number))
    (t nil)))

(defun typed-branch-info-p (info)
  (or (branch-info-function-types info)
      (branch-info-terminal-types info)
      (not (eq (branch-info-result-type info) :boolean))))

(defun add-function-entry (info function arity)
  (unless (function-set-index info function)
    (setf (branch-info-function-set info)
          (append (branch-info-function-set info) (list function)))
    (setf (branch-info-function-argument-map info)
          (append (branch-info-function-argument-map info) (list arity))))
  info)

(defun set-function-arity (info function arity)
  (let ((index (function-set-index info function)))
    (when index
      (setf (nth index (branch-info-function-argument-map info)) arity)))
  info)

(defun add-function-type (info function result-type argument-types)
  (when (branch-info-function-types info)
    (setf (branch-info-function-types info)
          (acons function
                 (cons result-type (copy-list argument-types))
                 (remove-if
                  (lambda (entry)
                    (same-symbol-name-p function (car entry)))
                  (branch-info-function-types info)))))
  info)

(defun add-allowed-index (info index)
  (let ((allowed (branch-info-allowed-branch-indices info)))
    (when (and allowed (not (member index allowed)))
      (setf (branch-info-allowed-branch-indices info)
            (sort (append allowed (list index)) #'<))))
  info)

(defun copy-branch-info (info)
  (when info
    (let ((copy (copy-structure info)))
      (setf (branch-info-formals copy)
            (copy-list (branch-info-formals copy)))
      (setf (branch-info-function-set copy)
            (copy-list (branch-info-function-set copy)))
      (setf (branch-info-function-argument-map copy)
            (copy-list (branch-info-function-argument-map copy)))
      (setf (branch-info-function-types copy)
            (copy-tree (branch-info-function-types copy)))
      (setf (branch-info-terminal-set copy)
            (copy-list (branch-info-terminal-set copy)))
      (setf (branch-info-terminal-types copy)
            (copy-tree (branch-info-terminal-types copy)))
      (setf (branch-info-allowed-branch-indices copy)
            (copy-list (branch-info-allowed-branch-indices copy)))
      copy)))

(defun copy-branch-infos (program)
  (map 'vector #'copy-branch-info (adf-program-branch-info program)))

(defun ensure-branch-metadata (program)
  (let ((infos (adf-program-branch-info program)))
    (unless (= (length infos)
               (1+ (adf-program-work-branch-count program)))
      (error "PROGRAM does not have branch metadata for every branch."))
    infos))

(defun branch-info-at (program index)
  (let ((infos (ensure-branch-metadata program)))
    (unless (< index (length infos))
      (error "PROGRAM has no branch metadata for branch index ~D." index))
    (aref infos index)))

(defun branch-body-at (program index)
  (let ((adf-count (adf-program-branch-count program))
        (adi-count (adf-program-adi-count program)))
    (cond ((< index adf-count)
           (adf-program-adf program index))
          ((< index (+ adf-count adi-count))
           (adf-program-adi program (- index adf-count)))
          ((= index (+ adf-count adi-count))
           (adf-program-rpb program))
          (t
           (error "PROGRAM has no branch at index ~D." index)))))

(defun make-operation-program (adfs rpb infos &optional adis)
  (make-adf-program :adfs adfs :adis adis :rpb rpb :branch-info infos))

(defun insert-adf-info (infos info)
  (let ((adf-info-count
          (count-if (lambda (branch-info)
                      (eq (branch-info-kind branch-info) :adf))
                    infos)))
    (concatenate 'vector
                 (subseq infos 0 adf-info-count)
                 (vector info)
                 (subseq infos adf-info-count))))

(defun insert-adi-info (infos info)
  (let* ((adi-info-count
           (count-if (lambda (branch-info)
                       (eq (branch-info-kind branch-info) :adi))
                     infos))
         (adi-end (+ (count-if (lambda (branch-info)
                                 (eq (branch-info-kind branch-info)
                                     :adf))
                               infos)
                     adi-info-count)))
    (concatenate 'vector
                 (subseq infos 0 adi-end)
                 (vector info)
                 (subseq infos adi-end))))

(defun normalize-branch-index (program branch)
  (let ((index (branch-index branch)))
    (cond ((eq index :rpb)
           (adf-program-work-branch-count program))
          ((consp index)
           (let ((adi-index (cdr index)))
             (unless (< adi-index (adf-program-adi-count program))
               (error "PROGRAM has no branch named ~S." branch))
             (+ (adf-program-branch-count program) adi-index)))
          ((and (integerp index)
                (<= 0 index)
                (< index (adf-program-branch-count program)))
           index)
          (t
           (error "PROGRAM has no branch named ~S." branch)))))

(defun set-branch-body-at (program index new-body)
  (let ((adf-count (adf-program-branch-count program))
        (work-count (adf-program-work-branch-count program)))
    (cond ((< index adf-count)
           (setf (adf-program-adf program index) new-body))
          ((< index work-count)
           (setf (adf-program-adi program (- index adf-count)) new-body))
          ((= index work-count)
           (setf (adf-program-rpb program) new-body))
          (t
           (error "PROGRAM has no branch at index ~D." index))))
  program)

(defun operation-adf-branch-index (program branch)
  (cond ((null branch)
         (when (plusp (adf-program-branch-count program))
           (random-integer (adf-program-branch-count program))))
        (t
         (let ((index (branch-index branch)))
           (when (and (integerp index)
                      (<= 0 index)
                      (< index (adf-program-branch-count program)))
             index)))))

(defun normalize-argument-index (info argument)
  (let ((index
          (etypecase argument
            (integer argument)
            (symbol
             (loop for formal in (branch-info-formals info)
                   for formal-index from 0
                   when (same-symbol-name-p argument formal)
                     return formal-index)))))
    (unless (and (integerp index)
                 (<= 0 index)
                 (< index (branch-info-arity info)))
      (error "No formal argument ~S in branch ~S."
             argument (branch-info-name info)))
    index))

(defun normalize-point (body point)
  (let ((point-count (count-crossover-points body)))
    (cond ((null point)
           (random-integer point-count))
          ((and (integerp point)
                (<= 0 point)
                (< point point-count))
           point)
          (t
           (error "Point ~S is outside the branch, which has ~D point~:P."
                  point point-count)))))

(defun remove-nth (sequence index)
  (append (subseq sequence 0 index)
          (nthcdr (1+ index) sequence)))

(defun tree-contains-name-p (tree name)
  (cond ((consp tree)
         (or (tree-contains-name-p (car tree) name)
             (some (lambda (argument)
                     (tree-contains-name-p argument name))
                   (cdr tree))))
        (t
         (same-symbol-name-p tree name))))

(defun tree-contains-adf-call-p (tree index)
  (cond ((consp tree)
         (or (eql (adf-symbol-index (car tree)) index)
             (some (lambda (argument)
                     (tree-contains-adf-call-p argument index))
                   (cdr tree))))
        (t nil)))

(defun terminals-of-type (info type)
  (remove-if-not (lambda (terminal)
                   (eq (terminal-type info terminal) type))
                 (branch-info-terminal-set info)))

(defun typed-type-generatable-p (info type remaining program)
  (when (>= remaining 0)
    (or (terminals-of-type info type)
        (loop for function in (branch-info-function-set info)
              for arity in (branch-info-function-argument-map info)
              for call-p = (branch-call-index function)
              thereis
              (and (or (null call-p) program)
                   (or (null call-p)
                       (branch-call-allowed-p info function program))
                   (eq (function-result-type-in-info
                        info function program)
                       type)
                   (if (zerop arity)
                       t
                       (and (plusp remaining)
                            (let ((argument-types
                                    (function-argument-types-in-info
                                     info function program)))
                              (and (= (length argument-types) arity)
                                   (loop for argument-type in argument-types
                                         always
                                         (and argument-type
                                              (typed-type-generatable-p
                                               info argument-type
                                               (1- remaining)
                                               program))))))))))))

(defun subtree-generation-depth ()
  (if (integerp *max-depth-for-new-subtrees-in-mutants*)
      *max-depth-for-new-subtrees-in-mutants*
      4))

(defun can-generate-subtree-p (info &optional program)
  (if (typed-branch-info-p info)
      (typed-type-generatable-p
       info (branch-info-result-type info)
       (subtree-generation-depth) program)
      (or (branch-info-terminal-set info)
          (some #'zerop (branch-info-function-argument-map info)))))

(defun typed-function-indices (info type remaining program)
  (loop for function in (branch-info-function-set info)
        for arity in (branch-info-function-argument-map info)
        for index from 0
        for call-p = (branch-call-index function)
        when (and (or (null call-p) program)
                  (or (null call-p)
                      (branch-call-allowed-p info function program))
                  (eq (function-result-type-in-info
                       info function program)
                      type)
                  (if (zerop arity)
                      t
                      (and (plusp remaining)
                           (let ((argument-types
                                   (function-argument-types-in-info
                                    info function program)))
                             (and (= (length argument-types) arity)
                                  (loop for argument-type in argument-types
                                        always
                                        (and argument-type
                                             (typed-type-generatable-p
                                              info argument-type
                                              (1- remaining)
                                              program))))))))
          collect index))

(defun generate-random-typed-subtree
    (info type depth &optional program full-p)
  (labels ((build-function (index remaining)
             (let* ((function
                      (nth index (branch-info-function-set info)))
                    (argument-types
                      (function-argument-types-in-info
                       info function program)))
               (cons function
                     (loop for argument-type in argument-types
                           collect (node argument-type
                                         (1- remaining) nil)))))
           (node (type remaining top-node-p)
             (let ((function-indices
                     (typed-function-indices info type remaining program))
                   (terminals (terminals-of-type info type)))
               (cond
                 ((and function-indices
                       (or top-node-p
                           full-p
                           (null terminals)
                           (and (plusp remaining)
                                (zerop (random-integer 2)))))
                  (build-function
                   (nth (random-integer (length function-indices))
                        function-indices)
                   remaining))
                 (terminals
                  (cl-koza-3.kernel::choose-from-terminal-set terminals))
                 (function-indices
                  (build-function
                   (nth (random-integer (length function-indices))
                        function-indices)
                   remaining))
                 (t
                  (error "Cannot generate a ~S subtree for branch ~S."
                         type (branch-info-name info)))))))
    (node type (max 1 depth) t)))

(defun generate-random-subtree (info
                                &optional
                                  (depth
                                    (if (integerp
                                         *max-depth-for-new-subtrees-in-mutants*)
                                        *max-depth-for-new-subtrees-in-mutants*
                                        4))
                                  program)
  "Generates a random subtree from the function and terminal metadata in
   INFO.  Typed branches generate subtrees of the branch result type."
  (unless (can-generate-subtree-p info program)
    (error "Branch ~S cannot generate a ~S subtree."
           (branch-info-name info)
           (if (typed-branch-info-p info)
               (branch-info-result-type info)
               :untyped)))
  (if (typed-branch-info-p info)
      (generate-random-typed-subtree
       info (branch-info-result-type info) depth program)
      (let ((functions (branch-info-function-set info))
            (argument-map (branch-info-function-argument-map info))
            (terminals (branch-info-terminal-set info)))
        (labels ((choose-terminal ()
                   (cl-koza-3.kernel::choose-from-terminal-set terminals))
                 (usable-function-indices (remaining)
                   (if (plusp remaining)
                       (loop for arity in argument-map
                             for index from 0
                             when (or (plusp arity) (zerop arity))
                               collect index)
                       (loop for arity in argument-map
                             for index from 0
                             when (zerop arity)
                               collect index)))
                 (build-function (index remaining)
                   (cons (nth index functions)
                         (loop repeat (nth index argument-map)
                               collect (node (1- remaining) nil))))
                 (node (remaining top-node-p)
                   (let ((usable (usable-function-indices remaining)))
                     (cond
                       ((and functions
                             (or top-node-p
                                 (null terminals)
                                 (and (plusp remaining)
                                      terminals
                                      (zerop (random-integer 2)))))
                        (build-function
                         (nth (random-integer (length usable)) usable)
                         remaining))
                       (terminals
                        (choose-terminal))
                       (usable
                        (build-function
                         (nth (random-integer (length usable)) usable)
                         remaining))
                       (t
                        (error "Cannot generate a subtree for branch ~S."
                               (branch-info-name info)))))))
          (node (max 1 depth) t)))))

(defun generate-random-subtree-of-type (info type &optional program)
  (if (typed-branch-info-p info)
      (generate-random-typed-subtree
       info type (subtree-generation-depth) program)
      (generate-random-subtree info)))

(defun duplicate-formal-references (tree old-formal new-formal)
  (cond ((consp tree)
         (cons (car tree)
               (mapcar (lambda (argument)
                         (duplicate-formal-references
                          argument old-formal new-formal))
                       (cdr tree))))
        ((same-symbol-name-p tree old-formal)
         (if (zerop (random-integer 2))
             new-formal
             tree))
        (t tree)))

(defun duplicate-call-argument-in-tree (tree branch-index argument-index)
  (if (consp tree)
      (let* ((operator (car tree))
             (arguments
               (mapcar (lambda (argument)
                         (duplicate-call-argument-in-tree
                          argument branch-index argument-index))
                       (cdr tree))))
        (if (and (eql (adf-symbol-index operator) branch-index)
                 (< argument-index (length arguments)))
            (cons operator
                  (append (subseq arguments 0 (1+ argument-index))
                          (list (copy-tree (nth argument-index arguments)))
                          (nthcdr (1+ argument-index) arguments)))
            (cons operator arguments)))
      tree))

(defun remove-call-argument-in-tree (tree branch-index argument-index)
  (if (consp tree)
      (let* ((operator (car tree))
             (arguments
               (mapcar (lambda (argument)
                         (remove-call-argument-in-tree
                          argument branch-index argument-index))
                       (cdr tree))))
        (if (and (eql (adf-symbol-index operator) branch-index)
                 (< argument-index (length arguments)))
            (cons operator (remove-nth arguments argument-index))
            (cons operator arguments)))
      tree))

(defun duplicate-adf-calls-in-tree (tree old-index new-index)
  (if (consp tree)
      (let* ((operator (car tree))
             (arguments
               (mapcar (lambda (argument)
                         (duplicate-adf-calls-in-tree
                          argument old-index new-index))
                       (cdr tree))))
        (if (and (eql (adf-symbol-index operator) old-index)
                 (zerop (random-integer 2)))
            (cons (adf-symbol new-index (symbol-package operator))
                  arguments)
            (cons operator arguments)))
      tree))

(defun duplicate-branch-info (info new-index)
  (let ((copy (copy-branch-info info)))
    (setf (branch-info-name copy)
          (adf-symbol new-index
                      (symbol-package (branch-info-name info))))
    copy))

(defun extend-info-with-duplicated-adf (info old-index new-index arity)
  (when (function-set-index info (adf-symbol old-index :keyword))
    (add-function-entry info (adf-symbol new-index :keyword) arity)
    (add-allowed-index info new-index))
  info)

(defun branch-duplication (program &key branch)
  "Performs Koza's branch duplication operation on PROGRAM."
  (ensure-branch-metadata program)
  (when (zerop (adf-program-branch-count program))
    (return-from branch-duplication nil))
  (let* ((source-index (or (operation-adf-branch-index program branch)
                           (return-from branch-duplication nil)))
         (adf-count (adf-program-branch-count program))
         (new-index adf-count)
         (infos (adf-program-branch-info program))
         (source-info (aref infos source-index))
         (new-info (duplicate-branch-info source-info new-index))
         (new-infos (copy-branch-infos program)))
    (loop for info across new-infos
          do (extend-info-with-duplicated-adf
              info source-index new-index (branch-info-arity source-info)))
    (let ((new-adfs (map 'vector #'copy-tree (adf-program-adfs program))))
      (loop for index below adf-count
            unless (= index source-index)
              do (setf (aref new-adfs index)
                       (duplicate-adf-calls-in-tree
                        (aref new-adfs index) source-index new-index)))
      (setf (aref new-adfs source-index)
            (copy-tree (aref new-adfs source-index)))
      (let* ((new-adis
               (map 'vector #'copy-tree
                    (adf-program-adis program)))
             (new-rpb
               (duplicate-adf-calls-in-tree
                (copy-tree (adf-program-rpb program))
                source-index
                new-index)))
        (loop for index below (length new-adis)
              do (setf (aref new-adis index)
                       (duplicate-adf-calls-in-tree
                        (aref new-adis index) source-index new-index)))
        (make-operation-program
         (concatenate 'vector
                      new-adfs
                      (vector (copy-tree
                               (adf-program-adf program source-index))))
         new-rpb
         (insert-adf-info new-infos new-info)
         new-adis)))))

(defun update-adf-arity-in-infos (infos index new-arity)
  (loop for info across infos
        do (set-function-arity info (adf-symbol index :keyword) new-arity))
  infos)

(defun argument-duplication (program &key branch argument)
  "Performs Koza's argument duplication operation on PROGRAM."
  (ensure-branch-metadata program)
  (let* ((source-index (or (operation-adf-branch-index program branch)
                           (return-from argument-duplication nil)))
         (infos (adf-program-branch-info program))
         (info (aref infos source-index))
         (arity (branch-info-arity info)))
    (when (zerop arity)
      (return-from argument-duplication nil))
    (let* ((argument-index
             (if argument
                 (normalize-argument-index info argument)
                 (random-integer arity)))
           (old-formal (nth argument-index (branch-info-formals info)))
           (new-formal (argument-symbol arity :keyword))
           (new-info (copy-branch-info info))
           (new-arity (1+ arity)))
      (setf (branch-info-arity new-info) new-arity)
      (setf (branch-info-formals new-info)
            (append (copy-list (branch-info-formals info))
                    (list new-formal)))
      (setf (branch-info-terminal-set new-info)
            (append (copy-list (branch-info-terminal-set info))
                    (unless (name-member-p
                             new-formal
                             (branch-info-terminal-set info))
                      (list new-formal))))
      (set-terminal-type new-info new-formal
                         (terminal-type info old-formal))
      (let* ((new-infos (copy-branch-infos program))
             (new-adfs (map 'vector #'copy-tree
                            (adf-program-adfs program)))
             (new-adis (map 'vector #'copy-tree
                            (adf-program-adis program)))
             (new-rpb (copy-tree (adf-program-rpb program))))
        (update-adf-arity-in-infos new-infos source-index new-arity)
        (setf (aref new-infos source-index) new-info)
        (set-function-arity new-info (adf-symbol source-index :keyword) new-arity)
        (setf (aref new-adfs source-index)
              (duplicate-formal-references
               (aref new-adfs source-index) old-formal new-formal))
        (loop for index below (length new-adfs)
              do (setf (aref new-adfs index)
                       (duplicate-call-argument-in-tree
                        (aref new-adfs index) source-index argument-index)))
        (loop for index below (length new-adis)
              do (setf (aref new-adis index)
                       (duplicate-call-argument-in-tree
                        (aref new-adis index)
                        source-index argument-index)))
        (setf new-rpb
              (duplicate-call-argument-in-tree
               new-rpb source-index argument-index))
        (make-operation-program new-adfs new-rpb new-infos new-adis)))))

(defun designate-p ()
  (< (random-floating-point-number 1.0)
     *branch-creation-designation-probability*))

(defun constant-terminal-p (terminal)
  (or (numberp terminal)
      (eq terminal t)
      (eq terminal nil)
      (name-member-p terminal
                     '(:integer-random-constant
                       :floating-point-random-constant))))

(defun extract-branch-creation-parts (subtree)
  (let ((argument-count 0)
        (arguments nil))
    (labels ((new-formal ()
               (prog1 (argument-symbol argument-count :keyword)
                 (incf argument-count)))
             (designate (tree)
               (let ((formal (new-formal)))
                 (push (copy-tree tree) arguments)
                 formal))
             (walk (tree)
               (cond ((and (symbolp tree)
                           (not (constant-terminal-p tree)))
                      (designate tree))
                     ((consp tree)
                      (cons (car tree)
                            (mapcar (lambda (argument)
                                      (walk-argument argument))
                                    (cdr tree))))
                     (t tree)))
             (walk-argument (argument)
               (if (or (and (symbolp argument)
                            (not (constant-terminal-p argument)))
                       (designate-p))
                   (designate argument)
                   (walk argument))))
      (let ((new-body
              (cond ((and (symbolp subtree)
                          (not (constant-terminal-p subtree)))
                     (designate subtree))
                    ((consp subtree)
                     (cons (car subtree)
                           (mapcar #'walk-argument (cdr subtree))))
                    ((designate-p)
                     (designate subtree))
                    (t subtree))))
        (values new-body (nreverse arguments))))))

(defun remove-formals-from-terminal-set (terminal-set formals)
  (remove-if (lambda (terminal)
               (name-member-p terminal formals))
             terminal-set))

(defun constant-terminal-set (terminal-set)
  (remove-if-not #'constant-terminal-p terminal-set))

(defun branch-creation (program &key branch point)
  "Performs Koza's branch creation operation on PROGRAM."
  (ensure-branch-metadata program)
  (let* ((adf-count (adf-program-branch-count program))
         (source-index
           (if branch
               (normalize-branch-index program branch)
               (nth-value 0
                          (multiple-value-bind (selected-branch body)
                              (cl-koza-3.kernel::select-branch program)
                            (declare (ignore body))
                            (normalize-branch-index program selected-branch)))))
         (source-body (branch-body-at program source-index))
         (point-index (normalize-point source-body point))
         (source-info (branch-info-at program source-index))
         (wrapper (list (copy-tree source-body))))
    (multiple-value-bind (pointer fragment)
        (get-subtree (first wrapper) wrapper point-index)
      (unless pointer
        (return-from branch-creation nil))
      (multiple-value-bind (new-body new-arguments)
          (extract-branch-creation-parts fragment)
        (let* ((new-index adf-count)
               (new-formals
                 (loop for index below (length new-arguments)
                       collect (argument-symbol index :keyword)))
               (new-argument-types
                 (loop for argument in new-arguments
                       collect (or (typed-tree-type
                                    argument source-info program)
                                   :boolean)))
               (new-terminal-set
                 (constant-terminal-set
                  (branch-info-terminal-set source-info)))
               (new-terminal-types
                 (append
                  (loop for entry in
                          (branch-info-terminal-types source-info)
                        when (and (name-member-p (car entry)
                                                  new-terminal-set)
                                  (not (name-member-p
                                        (car entry) new-formals)))
                          collect (copy-tree entry))
                  (loop for formal in new-formals
                        for type in new-argument-types
                        when (or (branch-info-terminal-types source-info)
                                 (not (eq type :boolean)))
                          collect (cons formal type))))
               (new-info
                 (make-branch-info
                  :name (adf-symbol new-index :keyword)
                  :kind :adf
                  :arity (length new-arguments)
                  :formals new-formals
                  :function-set
                  (copy-list (branch-info-function-set source-info))
                  :function-argument-map
                  (copy-list (branch-info-function-argument-map source-info))
                  :function-types
                  (copy-tree (branch-info-function-types source-info))
                  :terminal-set
                  (append new-terminal-set
                          (loop for formal in new-formals
                                unless (name-member-p
                                        formal new-terminal-set)
                                  collect formal))
                  :terminal-types new-terminal-types
                  :result-type (branch-info-result-type source-info)
                  :allowed-branch-indices
                  (copy-list
                   (branch-info-allowed-branch-indices source-info))))
               (call-symbol (adf-symbol new-index :keyword))
               (new-call (cons call-symbol (copy-tree new-arguments)))
               (new-infos (copy-branch-infos program)))
          (setf (first pointer) new-call)
          (let ((new-source-info (aref new-infos source-index)))
            (add-function-entry
             new-source-info call-symbol (length new-arguments))
            (add-allowed-index new-source-info new-index))
          (let ((new-adfs (map 'vector #'copy-tree
                               (adf-program-adfs program)))
                (new-adis (map 'vector #'copy-tree
                               (adf-program-adis program)))
                (new-rpb (copy-tree (adf-program-rpb program))))
            (cond ((= source-index
                      (adf-program-work-branch-count program))
                   (setf new-rpb (first wrapper)))
                  ((< source-index adf-count)
                   (setf (aref new-adfs source-index) (first wrapper)))
                  (t
                   (setf (aref new-adis (- source-index adf-count))
                         (first wrapper))))
            (make-operation-program
             (concatenate 'vector new-adfs (vector new-body))
             new-rpb
             (insert-adf-info new-infos new-info)
             new-adis)))))))

(defun add-terminal-by-name (terminal terminal-set)
  (if (name-member-p terminal terminal-set)
      terminal-set
      (append terminal-set (list terminal))))

(defun adi-creation (program
                     &key
                       branch
                       point
                       body
                       iteration-source
                       iteration-variable
                       iteration-index-variable
                       (iteration-item-type
                         *adi-creation-iteration-item-type*))
  "Creates an automatically defined iteration from a subtree of PROGRAM.

   The selected subtree becomes the initial state passed to the new ADI,
   and its point is replaced by a call to the new ADI.  The new ADI has
   one state formal.  BODY supplies its initial body; when omitted, a
   body is generated from the source branch metadata, falling back to
   ARG0 when generation is not possible."
  (ensure-branch-metadata program)
  (let* ((adf-count (adf-program-branch-count program))
         (adi-count (adf-program-adi-count program))
         (source-index
           (if branch
               (normalize-branch-index program branch)
               (nth-value 0
                          (multiple-value-bind (selected-branch selected-body)
                              (cl-koza-3.kernel::select-branch program)
                            (declare (ignore selected-body))
                            (normalize-branch-index program
                                                    selected-branch)))))
         (source-body (branch-body-at program source-index))
         (point-index (normalize-point source-body point))
         (source-info (branch-info-at program source-index))
         (infos (adf-program-branch-info program))
         (existing-adi-info
           (find-if (lambda (info)
                      (eq (branch-info-kind info) :adi))
                    infos))
         (wrapper (list (copy-tree source-body))))
    (multiple-value-bind (pointer fragment)
        (get-subtree (first wrapper) wrapper point-index)
      (unless pointer
        (return-from adi-creation nil))
      (let* (             (fragment-type
               (if (typed-branch-info-p source-info)
                   (or (typed-tree-type fragment source-info program)
                       (branch-info-result-type source-info))
                   (branch-info-result-type source-info)))
             (iteration-source
               (or iteration-source
                   (and existing-adi-info
                        (branch-info-iteration-source existing-adi-info))
                   *adi-creation-iteration-source*))
             (iteration-variable
               (or iteration-variable
                   (and existing-adi-info
                        (branch-info-iteration-variable
                         existing-adi-info))
                   *adi-creation-iteration-variable*))
             (iteration-index-variable
               (or iteration-index-variable
                   (and existing-adi-info
                        (branch-info-iteration-index-variable
                         existing-adi-info))
                   *adi-creation-iteration-index-variable*))
             (iteration-item-type
               (or (and existing-adi-info
                        (typed-branch-info-p existing-adi-info)
                        (terminal-type
                         existing-adi-info
                         (branch-info-iteration-variable
                          existing-adi-info)))
                   iteration-item-type))
             (new-index adi-count)
             (new-name (adi-symbol new-index :keyword))
             (constants
               (constant-terminal-set
                (branch-info-terminal-set source-info)))
             (terminal-set
               (reduce (lambda (set terminal)
                         (add-terminal-by-name terminal set))
                       (list :arg0 iteration-variable
                             iteration-index-variable)
                       :initial-value (copy-list constants)))
             (terminal-types
               (when (typed-branch-info-p source-info)
                 (append
                  (loop for entry in
                          (branch-info-terminal-types source-info)
                        when (name-member-p (car entry) constants)
                          collect (copy-tree entry))
                  (list (cons :arg0 fragment-type)
                        (cons iteration-variable iteration-item-type)
                        (cons iteration-index-variable :number)))))
             (new-info
               (make-branch-info
                :name new-name
                :kind :adi
                :arity 1
                :formals '(:arg0)
                :function-set
                (copy-list (branch-info-function-set source-info))
                :function-argument-map
                (copy-list (branch-info-function-argument-map source-info))
                :function-types
                (copy-tree (branch-info-function-types source-info))
                :terminal-set terminal-set
                :terminal-types terminal-types
                :result-type fragment-type
                :allowed-branch-indices
                (copy-list
                 (branch-info-allowed-branch-indices source-info))
                :iteration-source iteration-source
                :iteration-variable iteration-variable
                :iteration-index-variable iteration-index-variable)))
        (unless fragment-type
          (return-from adi-creation nil))
        (let ((new-body
                (or (and body (copy-tree body))
                    (when (can-generate-subtree-p new-info program)
                      (handler-case
                          (generate-random-subtree-of-type
                           new-info fragment-type program)
                        (error () nil)))
                    'arg0)))
          (when (and body
                     (typed-branch-info-p new-info)
                     (not (eq (typed-tree-type new-body
                                               new-info program)
                              fragment-type)))
            (return-from adi-creation nil))
          (setf (first pointer)
                (list new-name (copy-tree fragment)))
          (let ((new-infos (copy-branch-infos program)))
            (let ((new-source-info (aref new-infos source-index)))
              (add-function-entry new-source-info new-name 1)
              (add-function-type new-source-info new-name
                                 fragment-type (list fragment-type)))
            (let ((new-adfs (map 'vector #'copy-tree
                                 (adf-program-adfs program)))
                  (new-adis (map 'vector #'copy-tree
                                 (adf-program-adis program)))
                  (new-rpb (copy-tree (adf-program-rpb program))))
              (cond ((= source-index
                        (adf-program-work-branch-count program))
                     (setf new-rpb (first wrapper)))
                    ((< source-index adf-count)
                     (setf (aref new-adfs source-index) (first wrapper)))
                    (t
                     (setf (aref new-adis (- source-index adf-count))
                           (first wrapper))))
              (make-operation-program
               new-adfs
               new-rpb
               (insert-adi-info new-infos new-info)
               (concatenate 'vector new-adis (vector new-body))))))))))

(defun set-point (body point new-value)
  (let ((wrapper (list (copy-tree body))))
    (multiple-value-bind (pointer fragment)
        (get-subtree (first wrapper) wrapper point)
      (declare (ignore fragment))
      (unless pointer
        (error "Point ~S is outside the branch." point))
      (setf (first pointer) new-value)
      (first wrapper))))

(defun substitute-formals (tree formals arguments)
  (cond ((consp tree)
         (cons (car tree)
               (mapcar (lambda (argument)
                         (substitute-formals argument formals arguments))
                       (cdr tree))))
        (t
         (let ((index
                 (loop for formal in formals
                       for formal-index from 0
                       when (same-symbol-name-p tree formal)
                         return formal-index)))
           (if index
               (copy-tree (nth index arguments))
               tree)))))

(defun instantiate-call-subtree (subtree info arguments)
  (substitute-formals subtree (branch-info-formals info) arguments))

(defun argument-creation (program &key branch point)
  "Performs Koza's argument creation operation on PROGRAM."
  (ensure-branch-metadata program)
  (let* ((adf-count (adf-program-branch-count program))
         (source-index (or (operation-adf-branch-index program branch)
                           (return-from argument-creation nil)))
         (infos (adf-program-branch-info program))
         (info (aref infos source-index))
         (arity (branch-info-arity info))
         (source-body (adf-program-adf program source-index)))
    (declare (ignore adf-count))
    (let* ((point-index (normalize-point source-body point))
           (fragment
             (nth-value 1
                        (get-subtree source-body (list source-body) point-index)))
           (fragment-type (typed-tree-type fragment info program))
           (new-formal (argument-symbol arity :keyword))
           (new-info (copy-branch-info info))
           (new-arity (1+ arity)))
      (unless fragment-type
        (return-from argument-creation nil))
      (setf (branch-info-arity new-info) new-arity)
      (setf (branch-info-formals new-info)
            (append (copy-list (branch-info-formals info))
                    (list new-formal)))
      (setf (branch-info-terminal-set new-info)
            (append (copy-list (branch-info-terminal-set info))
                    (unless (name-member-p
                             new-formal
                             (branch-info-terminal-set info))
                      (list new-formal))))
      (set-terminal-type new-info new-formal fragment-type)
      (let* ((new-infos (copy-branch-infos program))
             (new-adfs (map 'vector #'copy-tree
                            (adf-program-adfs program)))
             (new-adis (map 'vector #'copy-tree
                            (adf-program-adis program)))
             (new-rpb (copy-tree (adf-program-rpb program))))
        (update-adf-arity-in-infos new-infos source-index new-arity)
        (setf (aref new-infos source-index) new-info)
        (set-function-arity new-info (adf-symbol source-index :keyword) new-arity)
        (setf (aref new-adfs source-index)
              (set-point (aref new-adfs source-index)
                         point-index
                         new-formal))
        (let ((original-info info))
          (labels ((append-argument (tree)
                     (if (consp tree)
                         (let* ((operator (car tree))
                                (arguments
                                  (mapcar #'append-argument (cdr tree))))
                           (if (eql (adf-symbol-index operator) source-index)
                               (cons operator
                                     (append arguments
                                             (list
                                              (instantiate-call-subtree
                                               fragment
                                               original-info
                                               arguments))))
                               (cons operator arguments)))
                         tree)))
            (loop for index below (length new-adfs)
                  do (setf (aref new-adfs index)
                           (append-argument (aref new-adfs index))))
            (loop for index below (length new-adis)
                  do (setf (aref new-adis index)
                           (append-argument (aref new-adis index))))
            (setf new-rpb (append-argument new-rpb))))
        (make-operation-program new-adfs new-rpb new-infos new-adis)))))

(defun adjust-adf-symbol-for-deletion (symbol deleted-index)
  (let ((index (adf-symbol-index symbol)))
    (cond ((null index) symbol)
          ((= index deleted-index) nil)
          ((> index deleted-index)
           (adf-symbol (1- index) (symbol-package symbol)))
          (t symbol))))

(defun adjust-function-set-for-deletion (info deleted-index)
  (let ((new-functions nil)
        (new-argument-map nil))
    (loop for function in (branch-info-function-set info)
          for arity in (branch-info-function-argument-map info)
          for adjusted = (adjust-adf-symbol-for-deletion
                          function deleted-index)
          when adjusted
            do (push adjusted new-functions)
               (push arity new-argument-map))
    (setf (branch-info-function-set info) (nreverse new-functions))
    (setf (branch-info-function-argument-map info) (nreverse new-argument-map))
    (when (branch-info-function-types info)
      (setf (branch-info-function-types info)
            (loop for entry in (branch-info-function-types info)
                  for function = (first entry)
                  for adjusted = (adjust-adf-symbol-for-deletion
                                  function deleted-index)
                  when adjusted
                    collect (cons adjusted (copy-list (rest entry))))))
    info))

(defun adjust-allowed-indices-for-deletion (allowed deleted-index)
  (when allowed
    (sort (loop for index in allowed
                unless (= index deleted-index)
                  collect (if (> index deleted-index)
                              (1- index)
                              index))
          #'<)))

(defun adjust-info-for-branch-deletion (info deleted-index)
  (let ((copy (copy-branch-info info))
        (index (adf-symbol-index (branch-info-name info))))
    (setf (branch-info-name copy)
          (if (and index (> index deleted-index))
              (adf-symbol (1- index)
                          (symbol-package (branch-info-name info)))
              (branch-info-name info)))
    (adjust-function-set-for-deletion copy deleted-index)
    (setf (branch-info-allowed-branch-indices copy)
          (adjust-allowed-indices-for-deletion
           (branch-info-allowed-branch-indices copy)
           deleted-index))
    copy))

(defun remove-adf-info (infos deleted-index)
  (let ((new-infos (make-array (1- (length infos))))
        (new-index 0))
    (loop for old-index below (length infos)
          unless (= old-index deleted-index)
            do (setf (aref new-infos new-index)
                     (adjust-info-for-branch-deletion
                      (aref infos old-index) deleted-index))
               (incf new-index))
    new-infos))

(defun delete-adf-calls-in-tree
    (tree deleted-index info program expected-type generation-program)
  (cond
    ((consp tree)
     (let* ((operator (car tree))
            (call-index (adf-symbol-index operator)))
       (cond
         ((eql call-index deleted-index)
          (generate-random-subtree-of-type
           info
           (or expected-type
               (branch-info-result-type info))
           generation-program))
         (t
          (let* ((new-operator
                   (if (and call-index
                            (> call-index deleted-index))
                       (adf-symbol
                        (1- call-index)
                        (symbol-package operator))
                       operator))
                 (argument-types
                   (function-argument-types-in-info
                    info operator program)))
            (cons new-operator
                  (loop for argument in (cdr tree)
                        for type in argument-types
                        collect (delete-adf-calls-in-tree
                                 argument deleted-index info
                                 program type
                                 generation-program))))))))
    (t tree)))

(defun branch-deletion (program &key branch)
  "Performs Koza's branch deletion operation, regenerating calls to the
   deleted branch at random."
  (ensure-branch-metadata program)
  (when (zerop (adf-program-branch-count program))
    (return-from branch-deletion nil))
  (let* ((deleted-index
           (or (operation-adf-branch-index program branch)
               (return-from branch-deletion nil)))
         (adf-count (adf-program-branch-count program))
         (adi-count (adf-program-adi-count program))
         (infos (adf-program-branch-info program))
         (new-infos (remove-adf-info infos deleted-index))
         (generation-program
           (make-operation-program
            (make-array (1- adf-count) :initial-element nil)
            nil
            new-infos
            (make-array adi-count :initial-element nil))))
    (loop for old-index below (+ adf-count adi-count)
          unless (= old-index deleted-index)
            do (let ((new-index (if (< old-index deleted-index)
                                    old-index
                                    (1- old-index))))
                 (when (and (tree-contains-adf-call-p
                             (branch-body-at program old-index)
                             deleted-index)
                            (not (can-generate-subtree-p
                                  (aref new-infos new-index)
                                  generation-program)))
                   (return-from branch-deletion nil))))
    (when (and (tree-contains-adf-call-p
                (adf-program-rpb program) deleted-index)
               (not (can-generate-subtree-p
                     (aref new-infos (1- (length new-infos)))
                     generation-program)))
      (return-from branch-deletion nil))
    (let ((new-adfs nil)
          (new-adis (make-array adi-count)))
      (loop for old-index below adf-count
            unless (= old-index deleted-index)
              do (let ((new-index (if (< old-index deleted-index)
                                      old-index
                                      (1- old-index))))
                   (let ((new-info (aref new-infos new-index)))
                      (push (delete-adf-calls-in-tree
                             (adf-program-adf program old-index)
                             deleted-index
                             new-info
                             program
                             (branch-info-result-type new-info)
                             generation-program)
                            new-adfs))))
      (loop for adi-index below adi-count
            for new-info = (aref new-infos
                                  (+ (1- adf-count) adi-index))
            do (setf (aref new-adis adi-index)
                     (delete-adf-calls-in-tree
                      (adf-program-adi program adi-index)
                      deleted-index
                      new-info
                      program
                      (branch-info-result-type new-info)
                      generation-program)))
      (let* ((rpb-info (aref new-infos (1- (length new-infos))))
             (new-rpb
               (delete-adf-calls-in-tree
                (adf-program-rpb program)
                deleted-index
                rpb-info
                program
                (branch-info-result-type rpb-info)
                generation-program)))
        (make-operation-program (coerce (nreverse new-adfs) 'vector)
                                new-rpb
                                new-infos
                                new-adis)))))

(defun tree-contains-adi-call-p (tree index)
  (cond ((consp tree)
         (or (eql (adi-symbol-index (car tree)) index)
             (some (lambda (argument)
                     (tree-contains-adi-call-p argument index))
                   (cdr tree))))
        (t nil)))

(defun operation-adi-branch-index (program branch)
  (cond ((null branch)
         (when (plusp (adf-program-adi-count program))
           (random-integer (adf-program-adi-count program))))
        (t
         (let ((index (branch-index branch)))
           (when (and (consp index)
                      (eq (car index) :adi)
                      (< (cdr index) (adf-program-adi-count program)))
             (cdr index))))))

(defun adjust-adi-symbol-for-deletion (symbol deleted-index)
  (let ((index (adi-symbol-index symbol)))
    (cond ((null index) symbol)
          ((= index deleted-index) nil)
          ((> index deleted-index)
           (adi-symbol (1- index) (symbol-package symbol)))
          (t symbol))))

(defun adjust-function-set-for-adi-deletion (info deleted-index)
  (let ((new-functions nil)
        (new-argument-map nil))
    (loop for function in (branch-info-function-set info)
          for arity in (branch-info-function-argument-map info)
          for adjusted = (adjust-adi-symbol-for-deletion
                          function deleted-index)
          when adjusted
            do (push adjusted new-functions)
               (push arity new-argument-map))
    (setf (branch-info-function-set info) (nreverse new-functions))
    (setf (branch-info-function-argument-map info) (nreverse new-argument-map))
    (when (branch-info-function-types info)
      (setf (branch-info-function-types info)
            (loop for entry in (branch-info-function-types info)
                  for function = (first entry)
                  for adjusted = (adjust-adi-symbol-for-deletion
                                  function deleted-index)
                  when adjusted
                    collect (cons adjusted (copy-list (rest entry))))))
    info))

(defun adjust-info-for-adi-deletion (info deleted-index)
  (let ((copy (copy-branch-info info))
        (index (adi-symbol-index (branch-info-name info))))
    (setf (branch-info-name copy)
          (if (and (eq (branch-info-kind info) :adi)
                   index
                   (> index deleted-index))
              (adi-symbol (1- index)
                          (symbol-package (branch-info-name info)))
              (branch-info-name info)))
    (adjust-function-set-for-adi-deletion copy deleted-index)
    copy))

(defun remove-adi-info (infos deleted-index)
  (let* ((adf-count
           (count-if (lambda (info)
                       (eq (branch-info-kind info) :adf))
                     infos))
         (deleted-info-index (+ adf-count deleted-index))
         (new-infos (make-array (1- (length infos))))
         (new-index 0))
    (loop for old-index below (length infos)
          unless (= old-index deleted-info-index)
            do (setf (aref new-infos new-index)
                     (adjust-info-for-adi-deletion
                      (aref infos old-index) deleted-index))
               (incf new-index))
    new-infos))

(defun delete-adi-calls-in-tree
    (tree deleted-index info program expected-type generation-program)
  (cond
    ((consp tree)
     (let* ((operator (car tree))
            (call-index (adi-symbol-index operator)))
       (cond
         ((eql call-index deleted-index)
          (generate-random-subtree-of-type
           info
           (or expected-type
               (branch-info-result-type info))
           generation-program))
         (t
          (let* ((new-operator
                   (if (and call-index
                            (> call-index deleted-index))
                       (adi-symbol
                        (1- call-index)
                        (symbol-package operator))
                       operator))
                 (argument-types
                   (function-argument-types-in-info
                    info operator program)))
            (cons new-operator
                  (loop for argument in (cdr tree)
                        for type in argument-types
                        collect (delete-adi-calls-in-tree
                                 argument deleted-index info
                                 program type
                                 generation-program))))))))
    (t tree)))

(defun adi-deletion (program &key branch)
  "Deletes one ADI from PROGRAM, regenerating calls to it at random.
   Later ADI branches are renumbered and their calls are updated."
  (ensure-branch-metadata program)
  (when (zerop (adf-program-adi-count program))
    (return-from adi-deletion nil))
  (let* ((deleted-index
           (or (operation-adi-branch-index program branch)
               (return-from adi-deletion nil)))
         (adf-count (adf-program-branch-count program))
         (adi-count (adf-program-adi-count program))
         (infos (adf-program-branch-info program))
         (new-infos (remove-adi-info infos deleted-index))
         (generation-program
           (make-operation-program
            (make-array adf-count :initial-element nil)
            nil
            new-infos
            (make-array (1- adi-count) :initial-element nil))))
    (labels ((new-adi-index (old-index)
               (if (< old-index deleted-index)
                   old-index
                   (1- old-index)))
             (new-adi-info-index (old-index)
               (+ adf-count (new-adi-index old-index))))
      (loop for adf-index below adf-count
            for info = (aref new-infos adf-index)
            when (and (tree-contains-adi-call-p
                       (adf-program-adf program adf-index)
                       deleted-index)
                      (not (can-generate-subtree-p
                            info generation-program)))
              do (return-from adi-deletion nil))
      (loop for old-index below adi-count
            unless (= old-index deleted-index)
              do (let ((info (aref new-infos
                                   (new-adi-info-index old-index))))
                   (when (and (tree-contains-adi-call-p
                               (adf-program-adi program old-index)
                               deleted-index)
                              (not (can-generate-subtree-p
                                    info generation-program)))
                     (return-from adi-deletion nil))))
      (let ((rpb-info (aref new-infos (1- (length new-infos)))))
        (when (and (tree-contains-adi-call-p
                    (adf-program-rpb program) deleted-index)
                   (not (can-generate-subtree-p
                         rpb-info generation-program)))
          (return-from adi-deletion nil)))
      (let ((new-adfs (make-array adf-count))
            (new-adis (make-array (1- adi-count))))
        (loop for adf-index below adf-count
              for info = (aref new-infos adf-index)
              do (setf (aref new-adfs adf-index)
                       (delete-adi-calls-in-tree
                        (adf-program-adf program adf-index)
                        deleted-index info program
                        (branch-info-result-type info)
                        generation-program)))
        (loop for old-index below adi-count
              unless (= old-index deleted-index)
                do (let ((new-index (new-adi-index old-index))
                         (info (aref new-infos
                                     (new-adi-info-index old-index))))
                     (setf (aref new-adis new-index)
                           (delete-adi-calls-in-tree
                            (adf-program-adi program old-index)
                            deleted-index info program
                            (branch-info-result-type info)
                            generation-program))))
        (let ((rpb-info (aref new-infos (1- (length new-infos)))))
          (make-operation-program
           new-adfs
           (delete-adi-calls-in-tree
            (adf-program-rpb program)
            deleted-index rpb-info program
            (branch-info-result-type rpb-info)
            generation-program)
           new-infos
           new-adis))))))

(defun build-formal-renamings (old-formals deleted-index new-formals)
  (loop for old-formal in old-formals
        for old-index from 0
        unless (= old-index deleted-index)
          collect (cons old-formal
                        (nth (if (< old-index deleted-index)
                                 old-index
                                 (1- old-index))
                             new-formals))))

(defun rename-formal (tree renamings)
  (if (symbolp tree)
      (let ((entry
              (find (symbol-name tree)
                    renamings
                    :key (lambda (pair)
                           (symbol-name (car pair)))
                    :test #'string=)))
        (if entry (cdr entry) tree))
      tree))

(defun regenerate-and-rename-formals
    (tree deleted-formal deleted-type renamings info
     &optional generation-program)
  (cond ((consp tree)
         (cons (car tree)
               (mapcar (lambda (argument)
                         (regenerate-and-rename-formals
                          argument deleted-formal deleted-type
                          renamings info generation-program))
                       (cdr tree))))
        ((same-symbol-name-p tree deleted-formal)
         (generate-random-subtree-of-type
          info deleted-type generation-program))
        (t
         (rename-formal tree renamings))))

(defun argument-deletion (program &key branch argument)
  "Performs Koza's argument deletion operation, regenerating references
   to the deleted argument at random."
  (ensure-branch-metadata program)
  (let* ((source-index (or (operation-adf-branch-index program branch)
                           (return-from argument-deletion nil)))
         (infos (adf-program-branch-info program))
         (info (aref infos source-index))
         (arity (branch-info-arity info)))
    (when (zerop arity)
      (return-from argument-deletion nil))
    (let* ((argument-index
             (if argument
                 (normalize-argument-index info argument)
                 (random-integer arity)))
           (old-formals (branch-info-formals info))
           (deleted-formal (nth argument-index old-formals))
           (new-formals nil))
      (loop for old-index below arity
            unless (= old-index argument-index)
              do (push (argument-symbol (length new-formals) :keyword)
                       new-formals))
      (setf new-formals (nreverse new-formals))
      (let* ((new-info (copy-branch-info info))
             (new-arity (1- arity))
             (renamings
               (build-formal-renamings
                old-formals argument-index new-formals))
             (source-body (adf-program-adf program source-index))
             (needs-regeneration
               (tree-contains-name-p source-body deleted-formal)))
        (setf (branch-info-arity new-info) new-arity)
        (setf (branch-info-formals new-info) new-formals)
        (setf (branch-info-terminal-set new-info)
              (append
               (remove-formals-from-terminal-set
                (branch-info-terminal-set info)
                old-formals)
               (loop for formal in new-formals
                     unless (name-member-p
                             formal
                             (remove-formals-from-terminal-set
                              (branch-info-terminal-set info)
                              old-formals))
                       collect formal)))
        (setf (branch-info-terminal-types new-info)
              (append
               (remove-if
                (lambda (entry)
                  (name-member-p (car entry) old-formals))
                (branch-info-terminal-types info))
               (loop for pair in renamings
                     for type = (terminal-type info (car pair))
                     when (or (branch-info-terminal-types info)
                              (not (eq type :boolean)))
                       collect (cons (cdr pair) type))))
        (let* ((new-infos (copy-branch-infos program))
               (generation-program
                 (make-operation-program
                  (make-array (adf-program-branch-count program)
                              :initial-element nil)
                  nil
                  new-infos
                  (make-array (adf-program-adi-count program)
                              :initial-element nil)))
               (new-adfs (map 'vector #'copy-tree
                              (adf-program-adfs program)))
               (new-adis (map 'vector #'copy-tree
                              (adf-program-adis program)))
               (new-rpb (copy-tree (adf-program-rpb program))))
          (update-adf-arity-in-infos new-infos source-index new-arity)
          (setf (aref new-infos source-index) new-info)
          (set-function-arity new-info
                              (adf-symbol source-index :keyword)
                              new-arity)
          (when (and needs-regeneration
                     (not (can-generate-subtree-p
                           new-info generation-program)))
            (return-from argument-deletion nil))
          (setf (aref new-adfs source-index)
                (regenerate-and-rename-formals
                 (aref new-adfs source-index)
                 deleted-formal
                 (terminal-type info deleted-formal)
                 renamings
                 new-info
                 generation-program))
          (loop for index below (length new-adfs)
                do (setf (aref new-adfs index)
                         (remove-call-argument-in-tree
                          (aref new-adfs index)
                          source-index
                          argument-index)))
          (loop for index below (length new-adis)
                do (setf (aref new-adis index)
                         (remove-call-argument-in-tree
                          (aref new-adis index)
                          source-index
                          argument-index)))
          (setf new-rpb
                (remove-call-argument-in-tree
                 new-rpb source-index argument-index))
          (make-operation-program
           new-adfs new-rpb new-infos new-adis))))))

(defun adf-call-allowed-p (info index)
  (let ((allowed (branch-info-allowed-branch-indices info)))
    (and (function-arity info (adf-symbol index :keyword))
         (or (null allowed) (member index allowed)))))

(defun valid-crossover-fragment-p (fragment source-info receiving-info program)
  "Returns true when FRAGMENT can be inserted into a RECEIVING-INFO
   branch of PROGRAM without violating point typing."
  (declare (ignore source-info))
  (typed-tree-type fragment receiving-info program))

(defun collect-valid-pointers (body branch-index fragment
                               source-info receiving-info program)
  (let ((fragment-type
          (valid-crossover-fragment-p
           fragment source-info receiving-info program)))
    (when fragment-type
      (let ((wrapper (list body))
            (points nil))
        (labels ((walk (tree pointer root-p)
                   (when (eq (typed-tree-type tree receiving-info program)
                             fragment-type)
                     (push (cons branch-index (if root-p :root pointer))
                           points))
                   (when (consp tree)
                     (loop for tail on (rest tree)
                           do (walk (first tail) tail nil)))))
          (walk (first wrapper) wrapper t))
        (nreverse points)))))

(defun apply-crossover-point (program point fragment)
  (let ((branch-index (car point))
        (pointer (cdr point)))
    (if (eq pointer :root)
        (set-branch-body-at program branch-index fragment)
        (setf (first pointer) fragment)))
  program)

(defun crossover-with-point-typing (contributing receiving
                                    &key
                                      contributing-branch contributing-point
                                      receiving-branch receiving-point
                                      max-depth)
  "Performs structure-preserving crossover from CONTRIBUTING into a
   RECEIVING branch of RECEIVING.  Returns one new program, or NIL when
   no valid receiving point exists."
  (ensure-branch-metadata contributing)
  (ensure-branch-metadata receiving)
  (let* ((source-branch
           (if contributing-branch
               (normalize-branch-index contributing contributing-branch)
               (normalize-branch-index
                contributing
                (nth-value 0
                           (cl-koza-3.kernel::select-branch contributing)))))
         (source-body (branch-body-at contributing source-branch))
         (source-point (normalize-point source-body contributing-point))
         (source-info (branch-info-at contributing source-branch))
         (fragment (nth-value 1
                              (get-subtree
                               source-body (list source-body) source-point)))
         (new-program
           (make-operation-program
            (map 'vector #'copy-tree (adf-program-adfs receiving))
            (copy-tree (adf-program-rpb receiving))
            (copy-branch-infos receiving)
            (map 'vector #'copy-tree (adf-program-adis receiving))))
         (candidate-points nil))
    (labels ((collect-branch-pointers (branch-index)
               (let* ((body (branch-body-at new-program branch-index))
                      (receiving-info
                        (branch-info-at new-program branch-index)))
                 (if receiving-point
                     (let* ((wrapper (list body))
                            (pointer (get-subtree
                                      body wrapper receiving-point))
                            (fragment-type
                              (and pointer
                                   (valid-crossover-fragment-p
                                    fragment
                                    source-info
                                    receiving-info
                                    new-program)))
                            (node (if (eq pointer wrapper)
                                      (first wrapper)
                                      (first pointer))))
                       (when (and fragment-type
                                  (eq (typed-tree-type
                                       node receiving-info new-program)
                                      fragment-type))
                         (push (cons branch-index
                                     (if (eq pointer wrapper)
                                         :root
                                         pointer))
                               candidate-points)))
                     (setf candidate-points
                           (nconc candidate-points
                                  (collect-valid-pointers
                                   body branch-index fragment
                                   source-info receiving-info
                                   new-program)))))))
      (if receiving-branch
          (collect-branch-pointers
           (normalize-branch-index receiving receiving-branch))
          (loop for branch-index
                  below (1+ (adf-program-work-branch-count new-program))
                do (collect-branch-pointers branch-index)))
      (when (null candidate-points)
        (return-from crossover-with-point-typing nil))
      (let ((point
              (nth (random-integer (length candidate-points))
                   candidate-points)))
        (apply-crossover-point new-program point (copy-tree fragment)))
      (when (and max-depth
                 (loop for branch-index
                         below (1+ (adf-program-work-branch-count new-program))
                       thereis (> (max-depth-of-tree
                                   (branch-body-at new-program branch-index))
                                  max-depth)))
        (return-from crossover-with-point-typing nil))
      new-program)))

(defun point-typed-crossover-pair (male female)
  "Returns two offspring from MALE and FEMALE using point-typed
   crossover in both directions."
  (values
   (or (crossover-with-point-typing
        male female
        :max-depth *max-depth-for-individuals-after-crossover*)
       (copy-adf-program male))
   (or (crossover-with-point-typing
        female male
        :max-depth *max-depth-for-individuals-after-crossover*)
       (copy-adf-program female))))

(defun breed-architecture-altering-population
    (population new-programs adf-branches rpb-branch)
  "Breeds a new population using Koza's architecture-altering
   operations.  The operation fractions are taken from the exported
   architecture-altering fraction variables.  When an operation cannot
   be applied, the selected individual is reproduced instead."
  (declare (ignore adf-branches rpb-branch))
  (let* ((size (length population))
         (crossover-end
           (+ *crossover-at-any-point-fraction*
              *crossover-at-function-point-fraction*))
         (reproduction-end
           (+ crossover-end
              *fitness-proportionate-reproduction-fraction*))
         (branch-duplication-end
           (+ reproduction-end *branch-duplication-fraction*))
         (argument-duplication-end
           (+ branch-duplication-end *argument-duplication-fraction*))
         (branch-deletion-end
           (+ argument-duplication-end *branch-deletion-fraction*))
         (argument-deletion-end
           (+ branch-deletion-end *argument-deletion-fraction*))
         (branch-creation-end
           (+ argument-deletion-end *branch-creation-fraction*))
         (argument-creation-end
           (+ branch-creation-end *argument-creation-fraction*))
         (adi-deletion-end
           (+ argument-creation-end *adi-deletion-fraction*))
         (adi-creation-end
           (+ adi-deletion-end *adi-creation-fraction*)))
    (labels ((selected-program ()
               (cl-koza-3.kernel::find-individual population))
             (unary-operation (operation)
               (let ((program (selected-program)))
                 (or (funcall operation program)
                     (copy-adf-program program)))))
      (loop with index = 0
            while (< index size)
            do (let ((fraction (/ index size)))
                 (cond
                   ((and (< index (1- size))
                         (< fraction crossover-end))
                    (multiple-value-bind (first second)
                        (point-typed-crossover-pair
                         (selected-program)
                         (selected-program))
                      (setf (aref new-programs index) first)
                      (setf (aref new-programs (1+ index)) second))
                    (incf index 2))
                   ((< fraction reproduction-end)
                    (setf (aref new-programs index)
                          (copy-adf-program (selected-program)))
                    (incf index))
                   ((< fraction branch-duplication-end)
                    (setf (aref new-programs index)
                          (unary-operation #'branch-duplication))
                    (incf index))
                   ((< fraction argument-duplication-end)
                    (setf (aref new-programs index)
                          (unary-operation #'argument-duplication))
                    (incf index))
                   ((< fraction branch-deletion-end)
                    (setf (aref new-programs index)
                          (unary-operation #'branch-deletion))
                    (incf index))
                   ((< fraction argument-deletion-end)
                    (setf (aref new-programs index)
                          (unary-operation #'argument-deletion))
                    (incf index))
                   ((< fraction branch-creation-end)
                    (setf (aref new-programs index)
                          (unary-operation #'branch-creation))
                    (incf index))
                   ((< fraction argument-creation-end)
                    (setf (aref new-programs index)
                          (unary-operation #'argument-creation))
                    (incf index))
                   ((< fraction adi-deletion-end)
                    (setf (aref new-programs index)
                          (unary-operation #'adi-deletion))
                    (incf index))
                   ((< fraction adi-creation-end)
                    (setf (aref new-programs index)
                          (unary-operation #'adi-creation))
                    (incf index))
                   (t
                    (setf (aref new-programs index)
                          (copy-adf-program (selected-program)))
                    (incf index))))))
    (dotimes (index size)
      (setf (individual-program (aref population index))
            (aref new-programs index)))))

(defun architecture-altering-parameters ()
  "Returns the architecture-altering operation fractions for run
   reporting."
  (list (cons "Branch duplication fraction" *branch-duplication-fraction*)
        (cons "Argument duplication fraction"
              *argument-duplication-fraction*)
        (cons "Branch creation fraction" *branch-creation-fraction*)
        (cons "Argument creation fraction" *argument-creation-fraction*)
        (cons "Branch deletion fraction" *branch-deletion-fraction*)
        (cons "Argument deletion fraction"
              *argument-deletion-fraction*)
        (cons "ADI creation fraction" *adi-creation-fraction*)
        (cons "ADI deletion fraction" *adi-deletion-fraction*)))

(defun run-architecture-altering-system (problem-function
                                         seed
                                         maximum-generations
                                         size-of-population
                                         &rest seeded-programs)
  "Runs genetic programming with Koza's architecture-altering
   operations.  PROBLEM-FUNCTION returns the same six values as for
   RUN-ADF-SYSTEM."
  (cl-koza-3.kernel::validate-run-arguments
   problem-function seed maximum-generations size-of-population)
  (cl-koza-3.kernel::initialize-run seed)
  (multiple-value-bind (adf-specs
                        rpb-spec
                        fitness-cases-creator
                        fitness-function
                        parameter-definer
                        termination-predicate)
      (funcall problem-function)
    (cl-koza-3.kernel::validate-branch-specs adf-specs rpb-spec)
    (cl-koza-3.kernel::run-system
     adf-specs
     rpb-spec
     maximum-generations
     size-of-population
     seeded-programs
     fitness-cases-creator
     fitness-function
     parameter-definer
     termination-predicate
     :breeding-function #'breed-architecture-altering-population)))

(defun run-adi-architecture-altering-system
    (problem-function
     seed
     maximum-generations
     size-of-population
     &rest seeded-programs)
  "Runs genetic programming with ADIs and architecture-altering
   operations.  PROBLEM-FUNCTION returns the same seven values as for
   RUN-ADI-SYSTEM.  ADI creation uses the iteration metadata of the first
   initial ADI specification when one is available."
  (cl-koza-3.kernel::validate-run-arguments
   problem-function seed maximum-generations size-of-population)
  (cl-koza-3.kernel::initialize-run seed)
  (multiple-value-bind (adf-specs
                        adi-specs
                        rpb-spec
                        fitness-cases-creator
                        fitness-function
                        parameter-definer
                        termination-predicate)
      (funcall problem-function)
    (cl-koza-3.kernel::validate-branch-specs adf-specs rpb-spec adi-specs)
    (let* ((first-adi-spec (first adi-specs))
           (*adi-creation-iteration-source*
             (or (and first-adi-spec
                      (cl-koza-3.kernel::branch-spec-iteration-source
                       first-adi-spec))
                 *adi-creation-iteration-source*))
           (*adi-creation-iteration-variable*
             (or (and first-adi-spec
                      (cl-koza-3.kernel::branch-spec-iteration-variable
                       first-adi-spec))
                 *adi-creation-iteration-variable*))
           (*adi-creation-iteration-index-variable*
             (or (and first-adi-spec
                      (cl-koza-3.kernel::branch-spec-iteration-index-variable
                       first-adi-spec))
                 *adi-creation-iteration-index-variable*)))
      (cl-koza-3.kernel::run-system
       adf-specs
       rpb-spec
       maximum-generations
       size-of-population
       seeded-programs
       fitness-cases-creator
       fitness-function
       parameter-definer
       termination-predicate
       :adi-specs adi-specs
       :breeding-function #'breed-architecture-altering-population))))
