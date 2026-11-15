(in-package #:cl-koza-3.program)

(defstruct (branch-info
             (:constructor make-branch-info
                 (&key name kind arity formals function-set
                       function-argument-map terminal-set
                       (result-type :boolean) allowed-branch-indices)))
  "Architecture metadata for one branch of an ADF program."
  name
  kind
  arity
  formals
  function-set
  function-argument-map
  terminal-set
  result-type
  allowed-branch-indices)

(defstruct (adf-program
             (:constructor %make-adf-program (adfs rpb branch-info))
             (:copier nil)
             (:print-object print-adf-program))
  adfs
  rpb
  branch-info)

(defun make-adf-program (&key
                           (adf0 nil adf0-supplied-p)
                           (adf1 nil adf1-supplied-p)
                           rpb
                           (adfs nil adfs-supplied-p)
                           (branch-info nil branch-info-supplied-p))
  "Creates an overall program with ADF0 ... ADF(N-1) and an RPB.

   The legacy keyword form :ADF0, :ADF1 and :RPB creates a two-ADF
   program.  The keyword form :ADFS and :RPB creates a program with an
   arbitrary number of ADFs.  Passing :ADFS NIL creates a program with
   no ADFs.  BRANCH-INFO, when supplied, is a vector of BRANCH-INFO
   structures in branch order, with the RPB last."
  (when (and adfs-supplied-p (or adf0-supplied-p adf1-supplied-p))
    (error "Cannot combine :ADFS with :ADF0 or :ADF1."))
  (%make-adf-program
   (cond (adfs-supplied-p
          (copy-seq (coerce adfs 'vector)))
         ((or adf0-supplied-p adf1-supplied-p)
          (vector adf0 adf1))
         (t
          (vector nil nil)))
   rpb
   (if branch-info-supplied-p
       (copy-seq (coerce branch-info 'vector))
       (vector))))

(defun copy-adf-program (program)
  "Returns a copy of PROGRAM with independent branch and metadata vectors."
  (%make-adf-program (copy-seq (adf-program-adfs program))
                     (adf-program-rpb program)
                     (copy-seq (adf-program-branch-info program))))

(defun adf-program-adf0 (program)
  "Returns the body of ADF0, or NIL when PROGRAM has no ADF0."
  (let ((adfs (adf-program-adfs program)))
    (when (< 0 (length adfs))
      (aref adfs 0))))

(defun (setf adf-program-adf0) (new-body program)
  (let ((adfs (adf-program-adfs program)))
    (when (zerop (length adfs))
      (error "PROGRAM has no ADF0."))
    (setf (aref adfs 0) new-body)))

(defun adf-program-adf1 (program)
  "Returns the body of ADF1, or NIL when PROGRAM has no ADF1."
  (let ((adfs (adf-program-adfs program)))
    (when (< 1 (length adfs))
      (aref adfs 1))))

(defun (setf adf-program-adf1) (new-body program)
  (let ((adfs (adf-program-adfs program)))
    (when (< (length adfs) 2)
      (error "PROGRAM has no ADF1."))
    (setf (aref adfs 1) new-body)))

(defun adf-program-adf (program index)
  "Returns the body of ADF INDEX from PROGRAM."
  (let ((adfs (adf-program-adfs program)))
    (unless (and (integerp index)
                 (<= 0 index)
                 (< index (length adfs)))
      (error "No ADF with index ~S in PROGRAM." index))
    (aref adfs index)))

(defun (setf adf-program-adf) (new-body program index)
  (let ((adfs (adf-program-adfs program)))
    (unless (and (integerp index)
                 (<= 0 index)
                 (< index (length adfs)))
      (error "No ADF with index ~S in PROGRAM." index))
    (setf (aref adfs index) new-body)))

(defun adf-program-branch-count (program)
  "Returns the number of ADFs in PROGRAM, not counting the RPB."
  (length (adf-program-adfs program)))

(defun branch-index (branch)
  "Returns the ADF index for BRANCH, or :RPB for the result-producing
   branch."
  (etypecase branch
    (integer branch)
    (symbol
     (if (eq branch :rpb)
         :rpb
         (let ((name (symbol-name branch)))
           (unless (and (> (length name) 3)
                        (string= name "ADF" :end1 3)
                        (every #'digit-char-p (subseq name 3)))
             (error "Unknown branch ~S." branch))
           (parse-integer name :start 3))))))

(defun branch-info-index (program branch)
  "Returns the index of BRANCH in PROGRAM's BRANCH-INFO vector."
  (let ((index (branch-index branch)))
    (if (eq index :rpb)
        (adf-program-branch-count program)
        index)))

(defun program-branch-info (program branch)
  "Returns the BRANCH-INFO for BRANCH, or NIL when PROGRAM has no
   metadata for it."
  (let* ((infos (adf-program-branch-info program))
         (index (branch-info-index program branch)))
    (when (< index (length infos))
      (aref infos index))))

(defun branch-formals (program branch &optional (default-adf-arity 3))
  "Returns the formal arguments for BRANCH.
   Uses metadata when available; otherwise defaults to ARG0 ... for ADFs
   and NIL for the RPB."
  (let ((info (program-branch-info program branch)))
    (if info
        (branch-info-formals info)
        (if (eq (branch-index branch) :rpb)
            nil
            (loop for index below default-adf-arity
                  collect (intern (format nil "ARG~D" index)
                                  :keyword))))))

(defun valid-branch-info-p (info)
  "Returns true when INFO has the minimum metadata needed by the kernel."
  (and (branch-info-p info)
       (member (branch-info-kind info) '(:adf :rpb))
       (integerp (branch-info-arity info))
       (not (minusp (branch-info-arity info)))
       (listp (branch-info-formals info))
       (= (branch-info-arity info) (length (branch-info-formals info)))
       (listp (branch-info-function-set info))
       (listp (branch-info-function-argument-map info))
       (= (length (branch-info-function-set info))
          (length (branch-info-function-argument-map info)))
       (listp (branch-info-terminal-set info))
       (or (null (branch-info-allowed-branch-indices info))
           (listp (branch-info-allowed-branch-indices info)))))

(defparameter *branch-names* '(:adf0 :adf1 :rpb)
  "The legacy branch names of an overall program with two ADFs.")

(defun branch-names (&optional program)
  "Returns the names of the branches of an overall program.

   Without PROGRAM, returns the legacy two-ADF branch names.  With
   PROGRAM, returns :ADF0 ... :ADF(N-1) followed by :RPB."
  (if program
      (append (loop for index below (adf-program-branch-count program)
                    collect (intern (format nil "ADF~D" index) :keyword))
              '(:rpb))
      *branch-names*))

(defun program-branch (program branch)
  "Returns the body of the branch named BRANCH from PROGRAM."
  (let ((index (branch-index branch)))
    (if (eq index :rpb)
        (adf-program-rpb program)
        (adf-program-adf program index))))

(defun replace-program-branch (program branch new-body)
  "Destructively replaces the body of BRANCH in PROGRAM and returns
   PROGRAM."
  (let ((index (branch-index branch)))
    (if (eq index :rpb)
        (setf (adf-program-rpb program) new-body)
        (setf (adf-program-adf program index) new-body)))
  program)

(defun map-program-branches (function program)
  "Calls FUNCTION with each branch name and its body, collecting the
   results in branch order."
  (let ((result nil))
    (dolist (branch (branch-names program) (nreverse result))
      (push (funcall function branch (program-branch program branch))
            result))))

(defun copy-individual-substituting-branch (branch new-branch-subtree
                                            program-to-copy)
  "Makes a copy of PROGRAM-TO-COPY, substituting the branch selected by
   BRANCH with NEW-BRANCH-SUBTREE."
  (let* ((index (branch-index branch))
         (adfs (adf-program-adfs program-to-copy))
         (branch-info (copy-seq (adf-program-branch-info program-to-copy))))
    (if (eq index :rpb)
        (%make-adf-program
         (map 'vector #'copy-tree adfs)
         new-branch-subtree
         branch-info)
        (progn
          (unless (and (integerp index)
                       (<= 0 index)
                       (< index (length adfs)))
            (error "No ADF with index ~S in PROGRAM." index))
          (let ((new-adfs (map 'vector #'copy-tree adfs)))
            (setf (aref new-adfs index) new-branch-subtree)
            (%make-adf-program
             new-adfs
             (copy-tree (adf-program-rpb program-to-copy))
             branch-info))))))

(defun print-adf-program (program stream)
  (let ((adfs (adf-program-adfs program)))
    (loop for index below (length adfs)
          for branch = (intern (format nil "ADF~D" index) :keyword)
          for body = (aref adfs index)
          do (format stream
                     "~&(defun ADF~D (~{~A~^ ~})~%  (values ~S))~%"
                     index
                     (mapcar #'symbol-name (branch-formals program branch))
                     body))
    (format stream
            "~&(defun RPB (~{~A~^ ~})~%  (values ~S))"
            (mapcar #'symbol-name (branch-formals program :rpb))
            (adf-program-rpb program))))

(defun count-crossover-points (program)
  "Counts the number of points in the tree (program).
   This includes functions as well as terminals."
  (if (consp program)
      (+ 1 (reduce #'+ (mapcar #'count-crossover-points
                               (rest program))))
      1))

(defun count-function-points (program)
  "Counts the number of function (internal) points in the program."
  (if (consp program)
      (+ 1 (reduce #'+ (mapcar #'count-function-points
                               (rest program))))
      0))

(defun max-depth-of-tree (tree)
  "Returns the depth of the deepest branch of the tree (program)."
  (if (consp tree)
      (+ 1 (if (rest tree)
               (apply #'max
                      (mapcar #'max-depth-of-tree (rest tree)))
               0))
      1))

(defun get-subtree (tree pointer-to-tree index)
  "Given a tree or subtree, a pointer to that tree/subtree and an index,
   return the component subtree that is numbered by INDEX.  We number
   left to right, depth first."
  (if (= index 0)
      (values pointer-to-tree (copy-tree tree) index)
      (if (consp tree)
          (do* ((tail (rest tree) (rest tail))
                (argument (first tail) (first tail)))
               ((not tail) (values nil nil index))
            (multiple-value-bind (new-pointer new-tree new-index)
                (get-subtree argument tail (- index 1))
              (if (= new-index 0)
                  (return (values new-pointer new-tree new-index))
                  (setf index new-index))))
          (values nil nil index))))

(defun get-function-subtree (tree pointer-to-tree index)
  "Given a tree or subtree, a pointer to that tree/subtree and an index,
   return the component subtree that is labeled with an internal point
   numbered by INDEX.  We number left to right, depth first."
  (if (= index 0)
      (values pointer-to-tree (copy-tree tree) index)
      (if (consp tree)
          (do* ((tail (rest tree) (rest tail))
                (argument (first tail) (first tail)))
               ((not tail) (values nil nil index))
            (multiple-value-bind (new-pointer new-tree new-index)
                (if (consp argument)
                    (get-function-subtree argument tail (- index 1))
                    (values nil nil index))
              (if (= new-index 0)
                  (return (values new-pointer new-tree new-index))
                  (setf index new-index))))
          (values nil nil index))))
