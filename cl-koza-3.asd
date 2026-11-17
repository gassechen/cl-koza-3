(defsystem "cl-koza-3"
  :description "Koza III genetic programming with ADFs and architecture-altering operations"
  :version "0.1.0"
  :author "Gastón Novita"
  :license "MIT License (see COPYING)"
  :depends-on ("cl-cpus" "lparallel")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "random")
                             (:file "functions")
                             (:file "program")
                             (:file "interpreter")
                             (:file "kernel")
                             (:file "architecture"))))
  :in-order-to ((test-op (test-op "cl-koza-3/tests"))))

(defsystem "cl-koza-3/examples/even-5-parity"
  :description "Koza III example: even-5-parity with two automatically defined functions"
  :version "0.1.0"
  :author "Gastón Novita"
  :license "MIT License (see COPYING)"
  :depends-on ("cl-koza-3")
  :serial t
  :components ((:module "examples"
                :serial t
                :components ((:file "even-5-parity"))))
  :in-order-to ((test-op (test-op "cl-koza-3/tests"))))

(defsystem "cl-koza-3/examples/even-5-parity-3-adfs"
  :description "Koza III example: even-5-parity with three automatically defined functions"
  :version "0.1.0"
  :author "Gastón Novita"
  :license "MIT License (see COPYING)"
  :depends-on ("cl-koza-3")
  :serial t
  :components ((:module "examples"
                :serial t
                :components ((:file "even-5-parity-3-adfs"))))
  :in-order-to ((test-op (test-op "cl-koza-3/tests"))))

(defsystem "cl-koza-3/examples/even-5-parity-n-adfs"
  :description "Koza III example: even-5-parity with a configurable number of ADFs"
  :version "0.1.0"
  :author "Gastón Novita"
  :license "MIT License (see COPYING)"
  :depends-on ("cl-koza-3")
  :serial t
  :components ((:module "examples"
                :serial t
                :components ((:file "even-5-parity-n-adfs"))))
  :in-order-to ((test-op (test-op "cl-koza-3/tests"))))

(defsystem "cl-koza-3/examples/even-5-parity-architecture-altering"
  :description "Koza III example: even-5-parity with architecture-altering operations"
  :version "0.1.0"
  :author "Gastón Novita"
  :license "MIT License (see COPYING)"
  :depends-on ("cl-koza-3")
  :serial t
  :components ((:module "examples"
                :serial t
                :components ((:file "even-5-parity-architecture-altering"))))
  :in-order-to ((test-op (test-op "cl-koza-3/tests"))))


(defsystem "cl-koza-3/examples/mixed-typed-regression"
  :description "Koza III example: mixed Boolean/number regression with typed ADFs"
  :version "0.1.0"
  :author "Gastón Novita"
  :license "MIT License (see COPYING)"
  :depends-on ("cl-koza-3")
  :serial t
  :components ((:module "examples"
                :serial t
                :components ((:file "mixed-typed-regression"))))
  :in-order-to ((test-op (test-op "cl-koza-3/tests"))))

(defsystem "cl-koza-3/examples/adi-sequence-sum"
  :description "Koza III example: sum a sequence with an automatically defined iteration"
  :version "0.1.0"
  :author "Gastón Novita"
  :license "MIT License (see COPYING)"
  :depends-on ("cl-koza-3")
  :serial t
  :components ((:module "examples"
                :serial t
                :components ((:file "adi-sequence-sum"))))
   :in-order-to ((test-op (test-op "cl-koza-3/tests"))))

(defsystem "cl-koza-3/examples/adi-architecture-altering"
  :description "Koza III example: architecture-altering runs with automatically defined iterations"
  :version "0.1.0"
  :author "Gastón Novita"
  :license "MIT License (see COPYING)"
  :depends-on ("cl-koza-3" "cl-koza-3/examples/adi-sequence-sum")
  :serial t
  :components ((:module "examples"
                :serial t
                :components ((:file "adi-architecture-altering"))))
  :in-order-to ((test-op (test-op "cl-koza-3/tests"))))


(defsystem "cl-koza-3/tests"
  :description "Test suite for cl-koza-3"
  :version "0.1.0"
  :author "Gastón Novita"
  :license "MIT License (see COPYING)"
  :depends-on ("cl-koza-3"
               "cl-koza-3/examples/even-5-parity"
               "cl-koza-3/examples/even-5-parity-3-adfs"
               "cl-koza-3/examples/even-5-parity-n-adfs"
               "cl-koza-3/examples/even-5-parity-architecture-altering"
               "cl-koza-3/examples/mixed-typed-regression"
               "cl-koza-3/examples/adi-sequence-sum"
               "cl-koza-3/examples/adi-architecture-altering"
               "fiveam")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "suite")
                             (:file "helpers")
                             (:file "random-test")
                             (:file "functions-test")
                             (:file "program-test")
                             (:file "interpreter-test")
                             (:file "architecture-test")
                             (:file "architecture-run-test")
                             (:file "n-adf-test")
                             (:file "kernel-creation-test")
                             (:file "kernel-run-test")
                             (:file "kernel-report-test")
                             (:file "parallel-test")
                             (:file "even-5-parity-test")
                             (:file "even-5-parity-3-adfs-test")
                             (:file "even-5-parity-n-adfs-test")
                             (:file "mixed-typed-regression-test")
                             (:file "adi-test")
                             (:file "adi-example-test")
                             (:file "known-issues-test"))))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :cl-koza-3.tests :run-tests)
               (error "cl-koza-3 test suite failed"))))
