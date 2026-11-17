(in-package #:cl-koza-3.program)

(defstruct (branch-info
             (:constructor make-branch-info
                 (&key name kind arity formals function-set
                       function-argument-map function-types terminal-set
                       terminal-types (result-type :boolean)
                       allowed-branch-indices
                       iteration-source iteration-variable
                       iteration-index-variable)))
  "Architecture metadata for one branch of a multi-part program."
  name
  kind
  arity
  formals
  function-set
  function-argument-map
  function-types
  terminal-set
  terminal-types
  result-type
  allowed-branch-indices
  iteration-source
  iteration-variable
  iteration-index-variable)

(defstruct (adf-program
             (:constructor %make-adf-program (adfs adis rpb branch-info))
             (:copier nil)
             (:print-object print-adf-program))
  adfs
  adis
  rpb
  branch-info)

(defun make-adf-program (&key
                           (adf0 nil adf0-supplied-p)
                           (adf1 nil adf1-supplied-p)
                           rpb
                           (adfs nil adfs-supplied-p)
                           (adis nil adis-supplied-p)
                           (branch-info nil branch-info-supplied-p))
  "Creates an overall program with ADF0 ... ADF(N-1), optionally
   ADI0 ... ADI(M-1), and an RPB.

   The legacy keyword form :ADF0, :ADF1 and :RPB creates a two-ADF
   program.  The keyword form :ADFS and :RPB creates a program with an
   arbitrary number of ADFs.  Passing :ADFS NIL creates a program with
   no ADFs.  :ADIS supplies automatically defined iterations.  BRANCH-INFO,
   when supplied, is a vector of BRANCH-INFO structures in ADF, ADI, RPB
   order."
  (when (and adfs-supplied-p (or adf0-supplied-p adf1-supplied-p))
    (error "Cannot combine :ADFS with :ADF0 or :ADF1."))
  (%make-adf-program
   (cond (adfs-supplied-p
          (copy-seq (coerce adfs 'vector)))
         ((or adf0-supplied-p adf1-supplied-p)
          (vector adf0 adf1))
         (t
          (vector nil nil)))
   (if adis-supplied-p
       (copy-seq (coerce adis 'vector))
       (vector))
   rpb
   (if branch-info-supplied-p
       (copy-seq (coerce branch-info 'vector))
       (vector))))

(defun copy-adf-program (program)
  "Returns a copy of PROGRAM with independent branch and metadata vectors."
  (%make-adf-program (copy-seq (adf-program-adfs program))
                     (copy-seq (adf-program-adis program))
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

(defparameter *aux-branch-kinds* '(:adi :adl :adr :ads)
  "Canonical order of automatically defined auxiliary branches.")

(defun aux-branch-kind-p (kind)
  (and (member kind *aux-branch-kinds*) t))

(defun aux-branch-kind-prefix (kind)
  (string (symbol-name kind)))

(defun aux-branch-kind-order (kind)
  (position kind *aux-branch-kinds*))

(defun aux-branch-index (name)
  (loop for kind in *aux-branch-kinds*
        for prefix = (aux-branch-kind-prefix kind)
        when (and (> (length name) (length prefix))
                  (string= name prefix :end1 (length prefix))
                  (every #'digit-char-p (subseq name (length prefix))))
          do (return (cons kind (parse-integer name :start (length prefix))))))

(defun adf-program-aux-count (program)
  "Returns the number of auxiliary branch bodies in PROGRAM."
  (length (adf-program-adis program)))

(defun aux-kind-offset (program kind)
  (let* ((infos (adf-program-branch-info program))
         (adf-count (adf-program-branch-count program))
         (order (or (aux-branch-kind-order kind) 0)))
    (loop for index from adf-count below (max adf-count (1- (length infos)))
          for other = (when (< index (length infos))
                        (branch-info-kind (aref infos index)))
          when (let ((other-order (and other
                                       (aux-branch-kind-order other))))
                 (and other-order (< other-order order)))
            count 1)))

(defun aux-kind-count (program kind)
  (let* ((infos (adf-program-branch-info program))
         (adf-count (adf-program-branch-count program))
         (aux-info-count (max 0 (- (length infos) adf-count 1))))
    (if (zerop aux-info-count)
        (if (eq kind :adi)
            (adf-program-aux-count program)
            0)
        (loop for index from adf-count below (+ adf-count aux-info-count)
              count (eq (branch-info-kind (aref infos index)) kind)))))

(defun adf-program-aux (program index)
  "Returns auxiliary branch body INDEX from PROGRAM."
  (let ((bodies (adf-program-adis program)))
    (unless (and (integerp index)
                 (<= 0 index)
                 (< index (length bodies)))
      (error "No auxiliary branch with index ~S in PROGRAM." index))
    (aref bodies index)))

(defun (setf adf-program-aux) (new-body program index)
  (let ((bodies (adf-program-adis program)))
    (unless (and (integerp index)
                 (<= 0 index)
                 (< index (length bodies)))
      (error "No auxiliary branch with index ~S in PROGRAM." index))
    (setf (aref bodies index) new-body)))

(defun aux-insert-body (bodies program kind index body)
  (let ((raw-index (+ (aux-kind-offset program kind) index)))
    (concatenate 'vector
                 (subseq bodies 0 raw-index)
                 (vector body)
                 (subseq bodies raw-index))))

(defun aux-remove-body (bodies program kind index)
  (let ((raw-index (+ (aux-kind-offset program kind) index)))
    (concatenate 'vector
                 (subseq bodies 0 raw-index)
                 (subseq bodies (1+ raw-index)))))

(defun adf-program-aux-body (program kind index)
  (adf-program-aux program (+ (aux-kind-offset program kind) index)))

(defun (setf adf-program-aux-body) (new-body program kind index)
  (setf (adf-program-aux program (+ (aux-kind-offset program kind) index))
        new-body))

(defun adf-program-adi (program index)
  "Returns the body of ADI INDEX from PROGRAM."
  (adf-program-aux-body program :adi index))

(defun (setf adf-program-adi) (new-body program index)
  (setf (adf-program-aux-body program :adi index) new-body))

(defun adf-program-adl (program index)
  "Returns the body of ADL INDEX from PROGRAM."
  (adf-program-aux-body program :adl index))

(defun (setf adf-program-adl) (new-body program index)
  (setf (adf-program-aux-body program :adl index) new-body))

(defun adf-program-adr (program index)
  "Returns the body of ADR INDEX from PROGRAM."
  (adf-program-aux-body program :adr index))

(defun (setf adf-program-adr) (new-body program index)
  (setf (adf-program-aux-body program :adr index) new-body))

(defun adf-program-ads (program index)
  "Returns the body of ADS INDEX from PROGRAM."
  (adf-program-aux-body program :ads index))

(defun (setf adf-program-ads) (new-body program index)
  (setf (adf-program-aux-body program :ads index) new-body))

(defun adf-program-branch-count (program)
  "Returns the number of ADFs in PROGRAM, not counting auxiliary branches
   or the RPB."
  (length (adf-program-adfs program)))

(defun adf-program-adi-count (program)
  "Returns the number of ADIs in PROGRAM."
  (aux-kind-count program :adi))

(defun adf-program-adl-count (program)
  "Returns the number of ADLs in PROGRAM."
  (aux-kind-count program :adl))

(defun adf-program-adr-count (program)
  "Returns the number of ADRs in PROGRAM."
  (aux-kind-count program :adr))

(defun adf-program-ads-count (program)
  "Returns the number of ADSs in PROGRAM."
  (aux-kind-count program :ads))

(defun adf-program-work-branch-count (program)
  "Returns the number of non-RPB branches: ADFs and auxiliary branches."
  (+ (adf-program-branch-count program)
     (adf-program-aux-count program)))

(defun adi-name-p (name)
  (and (> (length name) 3)
       (string= name "ADI" :end1 3)
       (every #'digit-char-p (subseq name 3))))

(defun branch-index (branch)
  "Returns the ADF index for BRANCH, (KIND . INDEX) for an auxiliary
   branch, or :RPB for the result-producing branch."
  (etypecase branch
    (integer branch)
    (cons branch)
    (symbol
     (cond
       ((eq branch :rpb) :rpb)
       (t
        (let* ((name (symbol-name branch))
               (aux-index (aux-branch-index name)))
          (cond
            ((and (> (length name) 3)
                  (string= name "ADF" :end1 3)
                  (every #'digit-char-p (subseq name 3)))
             (parse-integer name :start 3))
            (aux-index aux-index)
            (t
             (error "Unknown branch ~S." branch)))))))))

(defun branch-info-index (program branch)
  "Returns the index of BRANCH in PROGRAM's BRANCH-INFO vector."
  (let ((index (branch-index branch)))
    (cond ((eq index :rpb)
           (adf-program-work-branch-count program))
          ((consp index)
           (+ (adf-program-branch-count program)
              (aux-kind-offset program (car index))
              (cdr index)))
          (t index))))

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
        (let ((index (branch-index branch)))
          (cond ((eq index :rpb) nil)
                ((consp index)
                 (loop for argument-index below 1
                       collect (intern (format nil "ARG~D" argument-index)
                                       :keyword)))
                (t
                 (loop for index below default-adf-arity
                       collect (intern (format nil "ARG~D" index)
                                       :keyword))))))))

(defun valid-branch-info-p (info)
  "Returns true when INFO has the minimum metadata needed by the kernel."
  (and (branch-info-p info)
       (member (branch-info-kind info)
               (list* :adf *aux-branch-kinds* '(:rpb)))
       (integerp (branch-info-arity info))
       (not (minusp (branch-info-arity info)))
       (listp (branch-info-formals info))
       (= (branch-info-arity info) (length (branch-info-formals info)))
       (listp (branch-info-function-set info))
       (listp (branch-info-function-argument-map info))
       (= (length (branch-info-function-set info))
          (length (branch-info-function-argument-map info)))
       (listp (branch-info-function-types info))
       (listp (branch-info-terminal-set info))
       (listp (branch-info-terminal-types info))
       (or (null (branch-info-allowed-branch-indices info))
           (listp (branch-info-allowed-branch-indices info)))
        (or (not (aux-branch-kind-p (branch-info-kind info)))
            (and (or (null (branch-info-iteration-source info))
                     (symbolp (branch-info-iteration-source info)))
                (or (null (branch-info-iteration-variable info))
                    (symbolp (branch-info-iteration-variable info)))
                (or (null (branch-info-iteration-index-variable info))
                    (symbolp (branch-info-iteration-index-variable info)))))))

(defparameter *branch-names* '(:adf0 :adf1 :rpb)
  "The legacy branch names of an overall program with two ADFs.")

(defun branch-names (&optional program)
  "Returns the names of the branches of an overall program.

   Without PROGRAM, returns the legacy two-ADF branch names.  With
   PROGRAM, returns :ADF0 ... :ADF(N-1) followed by :RPB."
  (if program
      (append (loop for index below (adf-program-branch-count program)
                    collect (intern (format nil "ADF~D" index) :keyword))
              (loop for kind in *aux-branch-kinds*
                    append (loop for index below (aux-kind-count program kind)
                                 collect (intern
                                          (format nil "~A~D"
                                                  (aux-branch-kind-prefix kind)
                                                  index)
                                          :keyword)))
              '(:rpb))
      *branch-names*))

(defun program-branch (program branch)
  "Returns the body of the branch named BRANCH from PROGRAM."
  (let ((index (branch-index branch)))
    (cond ((eq index :rpb)
           (adf-program-rpb program))
          ((consp index)
           (adf-program-aux-body program (car index) (cdr index)))
          (t
           (adf-program-adf program index)))))

(defun replace-program-branch (program branch new-body)
  "Destructively replaces the body of BRANCH in PROGRAM and returns
   PROGRAM."
  (let ((index (branch-index branch)))
    (cond ((eq index :rpb)
           (setf (adf-program-rpb program) new-body))
          ((consp index)
           (setf (adf-program-aux-body program (car index) (cdr index))
                 new-body))
          (t
           (setf (adf-program-adf program index) new-body))))
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
         (adis (adf-program-adis program-to-copy))
         (branch-info (copy-seq (adf-program-branch-info program-to-copy)))
         (new-adfs (map 'vector #'copy-tree adfs))
         (new-adis (map 'vector #'copy-tree adis)))
    (cond
      ((eq index :rpb)
       (%make-adf-program
        new-adfs new-adis new-branch-subtree branch-info))
      ((consp index)
       (let ((raw-index (+ (aux-kind-offset program-to-copy (car index))
                           (cdr index))))
         (unless (< raw-index (length adis))
           (error "No ~A with index ~S in PROGRAM."
                  (aux-branch-kind-prefix (car index)) (cdr index)))
         (setf (aref new-adis raw-index) new-branch-subtree))
       (%make-adf-program
        new-adfs new-adis
        (copy-tree (adf-program-rpb program-to-copy))
        branch-info))
      (t
       (unless (and (integerp index)
                    (<= 0 index)
                    (< index (length adfs)))
         (error "No ADF with index ~S in PROGRAM." index))
       (setf (aref new-adfs index) new-branch-subtree)
       (%make-adf-program
        new-adfs new-adis
        (copy-tree (adf-program-rpb program-to-copy))
        branch-info)))))

(defun print-adf-program (program stream)
  (let ((adfs (adf-program-adfs program))
        (adis (adf-program-adis program)))
    (loop for index below (length adfs)
          for branch = (intern (format nil "ADF~D" index) :keyword)
          for body = (aref adfs index)
          do (format stream
                     "~&(defun ADF~D (~{~A~^ ~})~%  (values ~S))~%"
                     index
                     (mapcar #'symbol-name (branch-formals program branch))
                     body))
    (loop for index below (length adis)
          for info = (let* ((infos (adf-program-branch-info program))
                            (info-index
                              (+ (adf-program-branch-count program) index)))
                       (when (< info-index (length infos))
                         (aref infos info-index)))
          for kind = (or (and info (branch-info-kind info)) :adi)
          for prefix = (aux-branch-kind-prefix kind)
          for family-index = (- index (aux-kind-offset program kind))
          for branch = (intern (format nil "~A~D" prefix family-index)
                               :keyword)
          for body = (aref adis index)
          do (format stream
                     "~&(defun ~A~D (~{~A~^ ~})~%  (values ~S))~%"
                     prefix family-index
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
