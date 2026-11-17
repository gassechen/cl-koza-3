# cl-koza-3

Common Lisp implementation of John Koza's genetic programming system.

This repository contains the **`cl-koza-3`** system, a Common Lisp
implementation of the Koza III genetic programming kernel. It is an
independent successor to `cl-koza-2`, based on *Genetic Programming III:
Darwinian Invention and Problem Solving*.

## cl-koza-3

### Status

Phases 1-10 are complete: the fixed-architecture base, the six
architecture-altering operations, point-typed crossover, the
architecture-altering run loop, the design documentation, the optional
synchronous island model, mixed Boolean/number point typing, the ADI core
with ADI creation/deletion, and ADL/ADR/ADS auxiliary branches are in place.

- Kernel with an arbitrary architecture of
  **ADF0 … ADF(N-1) + ADI0 … ADI(M-1) + RPB** (result-producing branch),
  where `N >= 0` and `M >= 0`. With `N = M = 0`, a program consists only of
  the result-producing branch.
- Per-branch architecture metadata is stored in each `adf-program`:
  branch kind, arity, formal arguments, function set, function argument map,
  terminal set, function and terminal types, result type, and allowed
  hierarchical references.
- Automatically defined iterations (**ADI0 … ADI(M-1)**) that iterate over
  an external sequence. An ADI body sees its state formals, the current
   element, and the zero-based iteration index. ADI branches can be created
   and deleted during architecture-altering runs.
- Automatically defined loops (**ADL0 … ADL(K-1)**) repeat a body a control
  count times, automatically defined recursion (**ADR0 … ADR(R-1)**) recurses
  to a depth limit, and automatically defined storage (**ADS0 … ADS(S-1)**)
  reads and updates mutable cells shared through the environment.
- ADF and ADI calls use lexical scope: a called body sees only its formal
  arguments and, for ADIs, the iteration bindings, not the caller's
  environment.
- The legacy two-ADF program API and `run-genetic-programming-system`
  remain supported as compatibility layers.
- Examples: **even-5-parity** with two ADFs, **even-5-parity-3-adfs**
  with three hierarchical ADFs, **even-5-parity-n-adfs** with a
  configurable number of ADFs, **even-5-parity-architecture-altering**
   with Koza's evolutionary architecture, **mixed-typed-regression** with a
   Boolean ADF and a numeric ADF, and **auxiliary-branches** with ADL, ADR,
   and ADS branches.
- Parallel fitness evaluation and initial-population generation through
  `lparallel`, with deterministic results independent of worker count.
- The six architecture-altering operations from Koza III: branch
  duplication, argument duplication, branch creation, argument creation,
  branch deletion, and argument deletion. The four duplication/creation
  operations preserve program semantics; deletion operations regenerate
  affected references. Two additional ADI operations, `adi-creation` and
  `adi-deletion`, add and remove iteration-defining branches.
- Structure-preserving crossover with point typing over `:boolean` and
  `:number`, including typed ADF calls.
- Typed initial generation for full, grow, and ramped-half-and-half runs,
  plus typed mutation that preserves the selected point's type.
- `run-architecture-altering-system` with the branch and argument
  duplication, creation, and deletion fractions used in Koza's run.
- Optional island model with a two-dimensional torus, configurable deme
  count, and best-individual migration at Koza's per-direction rate.
- FiveAM test suite integrated with ASDF.

### Requirements

- A Common Lisp implementation (tested with SBCL).
- ASDF.
- Quicklisp for `lparallel`, `cl-cpus`, and `fiveam` (the latter for the
  test suite).

### Installation

Place the repository in a directory known to ASDF, for example:

```text
~/quicklisp/local-projects/cl-koza-3
```

Then load the system:

```lisp
(asdf:load-system "cl-koza-3")
```

### Quick start

Load the example system and verify the known correct program from
*Genetic Programming II*:

```lisp
(asdf:load-system "cl-koza-3/examples/even-5-parity")

(cl-koza-3.examples.even-5-parity:verify-known-program)
```

This prints the standardized fitness and number of hits, and returns two
values: `T` when the known program has fitness 0, and `T` when it solves all
32 fitness cases.

Run a small smoke test:

```lisp
(cl-koza-3.examples.even-5-parity:run-example
 :maximum-generations 1
 :size-of-population 8
 :seed 1.0)
```

`run-example` defaults to the book-sized run of 51 generations and a
population of 4000 individuals:

```lisp
(cl-koza-3.examples.even-5-parity:run-example)
```

The same run can be started directly with `run`:

```lisp
(cl-koza-3.examples.even-5-parity:run 51 4000 1.0 :verbose t)
```

The three-ADF version uses the generalized `run-adf-system` entry point:

```lisp
(asdf:load-system "cl-koza-3/examples/even-5-parity-3-adfs")

(cl-koza-3.examples.even-5-parity-3-adfs:verify-known-program)

(cl-koza-3.examples.even-5-parity-3-adfs:run-example
 :maximum-generations 1
 :size-of-population 8
 :seed 1.0)
```

The parameterized version defaults to four ADFs and accepts any
non-negative branch count:

```lisp
(asdf:load-system "cl-koza-3/examples/even-5-parity-n-adfs")

(cl-koza-3.examples.even-5-parity-n-adfs:run-example
 :number-of-adfs 6
 :maximum-generations 1
 :size-of-population 8
 :seed 1.0)
```

### Architecture

An overall program is an `adf-program` with `N` automatically defined
function branches followed by the result-producing branch:

| Branch | Purpose |
| --- | --- |
| `ADF0 … ADF(N-1)` | Automatically defined functions |
| `ADI0 … ADI(M-1)` | Automatically defined iterations |
| `ADL0 … ADL(K-1)` | Automatically defined loops |
| `ADR0 … ADR(R-1)` | Automatically defined recursion |
| `ADS0 … ADS(S-1)` | Automatically defined storage |
| `RPB` | Result-producing branch, the program's output |

`N` may be zero, in which case the program contains only `RPB`. The kernel
does not restrict which ADF a branch calls; that is determined by the
problem's function sets. In the original even-5-parity example, there are two
ADFs. ADF0 and ADF1 each take three arguments (`ARG0`, `ARG1`, `ARG2`), ADF1
may call ADF0, and the result-producing branch may call both ADFs. ADF0 does
not call another ADF. The three-ADF example adds ADF2, which may call ADF0
and ADF1, and lets the result-producing branch call all three ADFs. The
parameterized example generalizes this pattern: `ADF(I)` may call
`ADF0 … ADF(I-1)`, and the result-producing branch may call every ADF.

Each branch has a `branch-info` record. These records are stored in the
program's `branch-info` vector in `ADF0 … ADF(N-1), ADI0 … ADI(M-1),
ADL0 … ADL(K-1), ADR0 … ADR(R-1), ADS0 … ADS(S-1), RPB`
order and include:

- `name` and `kind` (`:adf`, `:adi`, `:adl`, `:adr`, `:ads`, or `:rpb`)
- `arity` and `formals`
- `function-set` and `function-argument-map`
- `function-types`, optional entries of the form
  `(function result-type &rest argument-types)`
- `terminal-set`
- `terminal-types`, an optional alist of `(terminal . type)`
- `result-type`, the branch's point type
- `allowed-branch-indices`
- For auxiliary branches: `iteration-source`, `iteration-variable`, and
  `iteration-index-variable`

Branches without type metadata keep the original Boolean behavior.
`*integer-random-constant*` and `*floating-point-random-constant*` are
typed as `:number`.

The main program operations are exported from `cl-koza-3.program`:

- `make-adf-program`, `adf-program-adfs`, `adf-program-adis`,
  `adf-program-adf`, `adf-program-adi`, `adf-program-branch-count`,
  `adf-program-adi-count`, `adf-program-adl-count`,
  `adf-program-adr-count`, `adf-program-ads-count`,
  `adf-program-work-branch-count`, `adf-program-rpb`,
  `adf-program-branch-info`
- Auxiliary bodies: `adf-program-aux`, `adf-program-aux-body`,
  `adf-program-aux-count`, `adf-program-adl`, `adf-program-adr`,
  `adf-program-ads`
- Branch metadata: `branch-info`, `make-branch-info`, `program-branch-info`,
  `branch-formals`, `valid-branch-info-p`
- Legacy accessors: `adf-program-adf0`, `adf-program-adf1`
- `branch-index`, `branch-names`, `program-branch`,
  `replace-program-branch`, `map-program-branches`
- `copy-individual-substituting-branch`
- `count-crossover-points`, `count-function-points`, `max-depth-of-tree`
- `get-subtree`, `get-function-subtree`

`make-adf-program` accepts `:adfs` (a list of branch bodies) together with
`:rpb` and optional `:adis` and `:branch-info`. The `:adis` argument holds all
auxiliary bodies in canonical ADI, ADL, ADR, ADS order; the dedicated
`adf-program-adl`, `adf-program-adr`, and `adf-program-ads` accessors address
each family. The metadata vector is in ADF, ADI, ADL, ADR, ADS, RPB order.
The legacy keywords `:adf0` and `:adf1` remain supported
when `:adfs` is omitted; calling it with no arguments still creates a program
with two `NIL` ADFs.

The generic tree interpreter is exported from `cl-koza-3.interpreter` and
re-exported from `cl-koza-3`:

- `evaluate-adf-program` evaluates the result-producing branch.
- `evaluate-adf-branch` evaluates a selected branch.
- `evaluate-adf-tree` evaluates an arbitrary tree in the context of a
  program.

Each evaluator accepts an `:environment` alist of terminal or argument
bindings. ADF and auxiliary calls are resolved against the program's branch
vector, so the interpreter does not use dynamically bound ADF variables. ADF
bodies are lexically scoped: only their formal arguments are visible inside
the body. ADI and ADL bodies also see the current iteration element and index,
ADR bodies see the current recursion depth, and ADS bodies see the current
storage value. The evaluator supports `AND`, `OR`, `IF`, and `PROGN` as
special forms and calls other symbols as Common Lisp functions.

The generalized kernel entry point is exported from `cl-koza-3`:

```lisp
(run-adf-system problem-function
                seed
                maximum-generations
                size-of-population
                &rest seeded-programs)
```

The legacy entry point remains available:

```lisp
(run-genetic-programming-system problem-function
                                seed
                                maximum-generations
                                size-of-population
                                &rest seeded-programs)
```

#### Problem-function contract

`run-adf-system` expects `problem-function` to return six values:

1. A list of branch specs for `ADF0 … ADF(N-1)`
2. A branch spec for the result-producing branch
3. Fitness-cases creator
4. Fitness function
5. Parameter definer
6. Termination predicate

A branch spec is created with:

```lisp
(make-branch-spec :function-set-creator #'creator
                  :terminal-set-creator #'creator
                  :arity 3
                  :result-type :number)
```

The `:arity` value is optional. ADFs default to three formal arguments
(`ARG0`, `ARG1`, `ARG2`) and the RPB defaults to zero. The `:result-type`
value is optional and defaults to `:boolean`. The length of the first
value determines the number of ADFs. `NIL` creates a program with no ADFs.
The fitness function receives an `adf-program` and the
fitness cases, and returns two values: the standardized fitness and the
number of hits.

#### Automatically defined iterations

`run-adi-system` adds a second list of branch specs for `ADI0 … ADI(M-1)`.
Its `problem-function` returns seven values:

1. A list of branch specs for `ADF0 … ADF(N-1)`
2. A list of branch specs for `ADI0 … ADI(M-1)`
3. A branch spec for the result-producing branch
4. Fitness-cases creator
5. Fitness function
6. Parameter definer
7. Termination predicate

ADI branch specs support the same creator protocol plus iteration metadata:

```lisp
(make-branch-spec :function-set-creator #'creator
                  :terminal-set-creator #'creator
                  :arity 1
                  :result-type :number
                  :iteration-source :data
                  :iteration-variable :item
                  :iteration-index-variable :index)
```

An ADI is called with one state argument per formal. The state starts from
the call arguments and is updated to the body's result on every element.
`iteration-source` names a binding in the caller's environment; when that
binding is absent, the call falls back to the dynamic variable
`*iteration-sequence*`. The sequence is a Common Lisp sequence, and each
iteration binds the current element and its zero-based index. The ADI
returns the final state.

`run-adi-system` creates a fixed set of ADI branches. The ADI-aware
architecture-altering operations preserve ADIs while updating ADI calls and
metadata as ADFs are added or removed, and `adi-creation`/`adi-deletion` can
also change the number of ADI branches. A nested ADI call inside an ADI body
reuses the enclosing ADI's iteration sequence through `*iteration-sequence*`.

#### ADL, ADR, and ADS branches

`run-aux-system` combines all auxiliary families. Its `problem-function`
returns ten values: ADF specs, ADI specs, ADL specs, ADR specs, ADS specs,
the RPB spec, the fitness-cases creator, the fitness function, the parameter
definer, and the termination predicate. Any auxiliary list may be `NIL`.

The one-family wrappers `run-adl-system`, `run-adr-system`, and
`run-ads-system` keep the seven-value contract of `run-adi-system`, with the
chosen auxiliary specs in the second position.

All auxiliary branches use the same spec metadata protocol as ADIs:

- **ADL**: `iteration-source` supplies an integer count or a sequence to
  repeat. The body receives the current state, the iteration element, and the
  zero-based index, then returns the next state.
- **ADR**: `iteration-source` supplies the depth limit. The body receives the
  current state and the current depth; recursive ADR calls inherit the limit
  through `*recursion-limit*` and increment `*recursion-depth*`. The base case
  returns the current state without evaluating the body.
- **ADS**: `iteration-source` names the storage cell. The call arguments
  initialize the cell on first use, and each body result is stored back.
  `*storage-cells*` is the hash table holding the cells; bind it to a fresh
  table for each independent evaluation.

`run-genetic-programming-system` is the compatibility wrapper for the
legacy two-ADF contract. Its `problem-function` returns ten values:

1. ADF0 function-set creator
2. ADF1 function-set creator
3. Result-producing branch function-set creator
4. ADF0 terminal-set creator
5. ADF1 terminal-set creator
6. Result-producing branch terminal-set creator
7. Fitness-cases creator
8. Fitness function
9. Parameter definer
10. Termination predicate

A function-set creator returns two values: the function set and its argument
map (the number of arguments expected by each function). It may return a
third value: a list of typed function entries of the form
`(function result-type &rest argument-types)`. A terminal-set creator
returns the terminal set and may return a second value: an alist of
`(terminal . type)`. For untyped branches, these return values may be
omitted and all points default to `:boolean`.

The parameter definer sets the kernel's dynamic variables, including:

- `*number-of-fitness-cases*`
- `*max-depth-for-new-individuals*`
- `*max-depth-for-new-subtrees-in-mutants*`
- `*max-depth-for-individuals-after-crossover*`
- `*fitness-proportionate-reproduction-fraction*`
- `*crossover-at-any-point-fraction*`
- `*crossover-at-function-point-fraction*`
- `*method-of-selection*`, `*tournament-size*`
- `*method-of-generation*`
- `*verbose*`

Fitness evaluation in the legacy two-ADF example is sequential because its
evaluator relies on dynamically bound special variables and is not
thread-safe. The generic interpreter uses explicit environments and does not
depend on those dynamic variables.

### Example: even-5-parity

The `even-5-parity` example defines the Boolean function sets, terminal sets,
fitness cases, fitness function, parameters, and termination criterion used
by the book. Its known correct program is:

```lisp
(make-adf-program
 :adf0 '(or (and arg0 arg1)
            (and (nand arg0 arg0) (nand arg1 arg1)))
 :adf1 '(nand (or (and arg0 arg1)
                  (and (nand arg0 arg0) (nand arg1 arg1)))
              (or (and arg0 arg1)
                  (and (nand arg0 arg0) (nand arg1 arg1))))
 :rpb '(adf1 (adf0 (adf0 d0 d1 d0) (adf0 d2 d3 d0) d0)
             d4 d0))
```

`verify-known-program` evaluates it and reports:

```text
Known program: standardized-fitness=0 hits=32
```

### Parallel execution

`cl-koza-3` uses `lparallel` by default for fitness evaluation and for
generating the initial random population. The worker pool is created on
demand and reused across runs.

```lisp
(let ((*parallel-evaluation-p* nil))
  (run-adf-system 'my-problem 1.0 50 1000))
```

Set `*parallel-evaluation-p*` to `NIL` for fitness functions that are not
thread-safe. Set `*parallel-generation-p*` to `NIL` to generate the initial
population serially. Initial generation uses deterministic per-individual
seeds and serialized uniqueness resolution, so the same randomizer seed
produces the same population regardless of the number of workers.

### Island model

Runs use one population by default. Set `*number-of-demes*` to a value
greater than one to split the total population into semi-isolated demes.
The total `size-of-population` must be divisible by `*number-of-demes*`.

```lisp
(let ((*number-of-demes* 64)
      (*migration-rate* 0.08)
      (*deme-topology* :torus))
  (cl-koza-3.examples.even-5-parity-architecture-altering:run-example
   :maximum-generations 76
   :size-of-population 96000
   :seed 1.0))
```

This is Koza's `D = 64`, `Q = 1500`, `B = 8%` configuration. On each
generation after generation 0, the best `floor(B * Q)` individuals from
each deme are copied to each torus neighbor, replacing the worst
individuals in the destination. Migration uses a synchronous
generation-boundary schedule; Koza's original transputer run was
asynchronous.

`:torus` requires a square number of demes. The migration rate is the
fraction sent in each of the four directions, not a total budget split
across directions.

### Example: even-5-parity with three ADFs

The `even-5-parity-3-adfs` example keeps the same fitness cases and Boolean
function sets, but uses a hierarchical three-ADF architecture: ADF2 may call
ADF0 and ADF1, and the result-producing branch may call all three ADFs. Its
known correct program reuses the two-ADF solution and wraps the original
ADF1 call through ADF2:

```lisp
(make-adf-program
 :adfs (list
        '(or (and arg0 arg1)
             (and (nand arg0 arg0) (nand arg1 arg1)))
        '(nand (or (and arg0 arg1)
                   (and (nand arg0 arg0) (nand arg1 arg1)))
               (or (and arg0 arg1)
                   (and (nand arg0 arg0) (nand arg1 arg1))))
        '(adf1 arg0 arg1 arg2))
 :rpb '(adf2 (adf0 (adf0 d0 d1 d0) (adf0 d2 d3 d0) d0)
             d4 d0))
```

`verify-known-program` reports:

```text
Known program: standardized-fitness=0.0 hits=32
```

The example uses the generic `run-adf-system` entry point and the
`evaluate-adf-program` interpreter.

### Example: even-5-parity with N ADFs

The `even-5-parity-n-adfs` example parameterizes the hierarchical
architecture. `*number-of-adfs*` defaults to `4`; `run`, `run-example`, and
`verify-known-program` also accept an explicit branch count.

```lisp
(asdf:load-system "cl-koza-3/examples/even-5-parity-n-adfs")

(cl-koza-3.examples.even-5-parity-n-adfs:run-example
 :number-of-adfs 4
 :maximum-generations 1
 :size-of-population 8
 :seed 1.0)
```

`run` supports `N >= 0`, including programs with no ADFs. The known correct
program is available for `N >= 2`: it keeps the two-ADF solution in ADF0 and
ADF1, adds pass-through branches for higher indices, and calls the last ADF
from the result-producing branch.

```lisp
(cl-koza-3.examples.even-5-parity-n-adfs:verify-known-program 4)
```

### Example: mixed Boolean/number regression

The `mixed-typed-regression` example exercises point typing across two types.
`ADF0` returns `:boolean` with two numeric arguments, `ADF1` returns
`:number`, and the result-producing branch returns `:number`. The target
function is:

```lisp
(if (< x y)
    (+ (* x x) z)
    (- (* y y) z))
```

The known program is:

```lisp
(make-adf-program
 :adfs (vector '(< arg0 arg1)
               '(+ (* arg0 arg0) arg1))
 :rpb '(if-number (adf0 x y) (adf1 x z) (- (* y y) z)))
```

The example uses 27 fitness cases with `x`, `y`, and `z` ranging over
`{0, 1, 2}`. A small run can be started with:

```lisp
(asdf:load-system "cl-koza-3/examples/mixed-typed-regression")

(cl-koza-3.examples.mixed-typed-regression:run-example
 :maximum-generations 2
 :size-of-population 12
 :seed 1.0)
```

Crossover can therefore move a Boolean test into `if-number`'s test position
or a numeric expression into an arithmetic argument position, but not the
reverse.

### Example: summing a sequence with an ADI

The `adi-sequence-sum` example exercises the ADI core. `ADI0` iterates
over a sequence supplied in the fitness environment, and the target includes
both the current element and its zero-based index. The expected value for a
sequence is:

```lisp
(+ (reduce #'+ sequence)
   (/ (* (length sequence) (1- (length sequence))) 2))
```

The known program is:

```lisp
(make-adf-program
 :adfs nil
 :adis (list '(+ arg0 item index))
 :rpb '(adi0 0)
 :branch-info ...)
```

Here `arg0` is the ADI state, `item` is the current sequence element, and
`index` is the iteration index. Verify the known program and run a small
evolutionary smoke test with:

```lisp
(asdf:load-system "cl-koza-3/examples/adi-sequence-sum")

(cl-koza-3.examples.adi-sequence-sum:verify-known-program)

(cl-koza-3.examples.adi-sequence-sum:run-example
 :maximum-generations 10
 :size-of-population 100
 :seed 1.0)
```

The known program reports zero standardized fitness and six hits.

The `adi-architecture-altering` example reuses this problem with
`run-adi-architecture-altering-system`, allowing ADI branches to appear and
disappear during the run:

```lisp
(asdf:load-system "cl-koza-3/examples/adi-architecture-altering")

(cl-koza-3.examples.adi-architecture-altering:run-example
 :maximum-generations 10
 :size-of-population 100
 :seed 1.0)
```

### Example: ADL, ADR, and ADS

The `auxiliary-branches` example combines all three remaining auxiliary
families in one program. `ADL0` sums its index three times, `ADR0` increments
a value until the depth limit, and `ADS0` increments a mutable cell. The
result-producing branch adds their results:

```lisp
(make-adf-program
 :adfs nil
 :adis (list '(+ arg0 index)
             '(if (< depth 2) (adr0 (+ arg0 1)) arg0)
             '(+ value 1))
 :rpb '(+ (adl0 0) (+ (adr0 0) (ads0 0)))
 :branch-info ...)
```

Verify the known program and run a small evolutionary smoke test with:

```lisp
(asdf:load-system "cl-koza-3/examples/auxiliary-branches")

(cl-koza-3.examples.auxiliary-branches:verify-known-program)

(cl-koza-3.examples.auxiliary-branches:run-example
 :maximum-generations 10
 :size-of-population 100
 :seed 1.0)
```

The known program reports zero standardized fitness and one hit.

### Architecture-altering operations

The architecture-altering operations are exported from `cl-koza-3`.
Each returns a new program, or `NIL` when the operation is not applicable:

```lisp
(branch-duplication program &key branch)
(argument-duplication program &key branch argument)
(branch-creation program &key branch point)
(argument-creation program &key branch point)
(branch-deletion program &key branch)
(argument-deletion program &key branch argument)
(adi-creation program &key branch point body
             iteration-source iteration-variable
             iteration-index-variable iteration-item-type)
(adi-deletion program &key branch)
```

Branch duplication, argument duplication, branch creation, argument creation,
and `adi-creation` preserve the value returned by the program. `adi-creation`
replaces the selected subtree with a call to a new ADI whose state argument is
that subtree; its optional body defaults to generated code or `arg0`.
Branch deletion, argument deletion, and `adi-deletion` regenerate affected
references at random and can change the program's value. `adi-deletion`
renumbers later ADIs and updates their calls and metadata. Typed deletion
regeneration receives temporary program metadata, so replacement subtrees may
contain ADF calls.

Point-typed crossover inserts a fragment from one program into a branch of
another program. The fragment is checked against the receiving branch's
metadata: its type must match the type of the receiving point, and
function-set membership, arity, terminal-set membership, ADF references, and
numeric constants must all be valid:

```lisp
(crossover-with-point-typing contributing receiving
 :contributing-branch :rpb
 :contributing-point 0
 :receiving-branch :rpb
 :receiving-point 0)
```

A complete architecture-altering run uses
`run-architecture-altering-system`. Its problem function returns the same six
values as `run-adf-system`. The operation fractions are dynamic variables:

- `*fitness-proportionate-reproduction-fraction*`
- `*crossover-at-any-point-fraction*`
- `*crossover-at-function-point-fraction*`
- `*branch-duplication-fraction*`
- `*argument-duplication-fraction*`
- `*branch-creation-fraction*`
- `*argument-creation-fraction*`
- `*branch-deletion-fraction*`
- `*argument-deletion-fraction*`
- `*adi-creation-fraction*`
- `*adi-deletion-fraction*`

ADI-aware runs use `run-adi-architecture-altering-system`. Its problem
function returns the same seven values as `run-adi-system`, including ADI
specs. ADI creation uses the iteration metadata of the first initial ADI spec
when one is available, and can be customized with
`*adi-creation-iteration-source*`, `*adi-creation-iteration-variable*`,
`*adi-creation-iteration-index-variable*`, and
`*adi-creation-iteration-item-type*`.

The crossover fractions total the fraction of offspring produced by
point-typed crossover. When an architecture-altering operation cannot be
applied to the selected individual, the individual is reproduced instead.

### Example: even-5-parity with architecture-altering operations

This example starts from Koza's minimalist architecture: one two-argument
ADF (`ADF0`) plus the result-producing branch. The architecture then changes
during the run through the architecture-altering operations.

```lisp
(asdf:load-system
 "cl-koza-3/examples/even-5-parity-architecture-altering")

(cl-koza-3.examples.even-5-parity-architecture-altering:run-example
 :maximum-generations 76
 :size-of-population 1000
 :seed 1.0)
```

The parameter definer uses Koza's fractions: 74% crossover, 10%
reproduction, 0% mutation, 5% branch duplication, 5% argument duplication,
0.5% branch deletion, 0.5% argument deletion, 5% branch creation, and 0%
argument creation. The run report prints these fractions alongside the
standard GP parameters. `run` and `run-example` also accept
`:number-of-demes` and `:migration-rate`; see the island-model section above
for Koza's 64-deme configuration.

### Experimental result: Koza's 64-deme run

One run with seed `1.0`, `64` demes of `1,500` individuals, and an `8%`
migration rate solved even-5-parity:

- best-of-run generation: `9`
- standardized fitness: `0.0`
- hits: `32 / 32`
- best architecture: `ADF0` with 3 arguments and `ADF1` with 4 arguments
- wall-clock run time: `58.2 s`
- maximum resident memory: approximately `929 MB`

Final population architecture distribution:

| ADFs | Individuals |
| ---: | ---: |
| 0 | 19 |
| 1 | 3,942 |
| 2 | 56,509 |
| 3 | 29,311 |
| 4 | 5,661 |
| 5 | 539 |
| 6 | 19 |

For comparison, the single-population run with `1,000` individuals and the
same seed reached standardized fitness `8.0` with `24` hits and did not
solve the problem. This is a single-run illustration, not a statistical
estimate of success probability.

### Tests

The test suite uses FiveAM and is registered as `cl-koza-3/tests`:

```lisp
(asdf:test-system "cl-koza-3")
```

At the time of writing the suite reports:

```text
Pass: 3439 (100%)
Skip: 0 ( 0%)
Fail: 0 ( 0%)
```

`maximum-generations` must now be a positive integer, and `random-integer`
signals an error for non-positive sizes, so the previous known issues are
covered by active tests.

### Roadmap

- **Phase 1 (done):** fixed-architecture `cl-koza-3` base with per-branch
  metadata, explicit ADF arity, lexical ADF scope, and the ported
  fixed-ADF test suite.
- **Phase 2 (done):** the six architecture-altering operations, point-typed
  structure-preserving crossover, and semantic-preservation tests.
- **Phase 3 (done):** architecture-altering even-5-parity run with Koza's
  probabilities and reporting.
- **Phase 4 (done):** design documentation and the architecture-altering
  example.
- **Phase 5 (done):** optional synchronous island model with torus
  migration and Koza's 64-deme configuration.
- **Phase 6 (done):** mixed Boolean/number point typing, typed ADF calls,
  typed subtree generation for full/grow/ramped runs, typed mutation, and the
  mixed-typed-regression example.
- **Phase 7 (done):** fixed ADI core with external iteration sequences,
  state formals, iteration element/index bindings, `run-adi-system`, the
  `adi-sequence-sum` example, and ADI branch generation/evaluation tests.
- **Phase 8 (done):** architecture-altering operations that preserve ADIs,
  update ADI calls, and remap ADI/ADF metadata when ADF branches change.
- **Phase 9 (done):** ADI creation/deletion, ADI-aware architecture-altering
  runs, nested ADI evaluation, and the `adi-architecture-altering` example.
- **Phase 10 (done):** ADL/ADR/ADS branch representation, evaluation, run
  entries, tests, and the `auxiliary-branches` example.

### Project layout

```text
cl-koza-3.asd          ASDF systems
src/                    Koza III kernel and architecture-altering operations
examples/               Even-5-parity, mixed Boolean/number, and ADI examples
doc/                    Design notes
tests/                  FiveAM test suite
```

### References

- John R. Koza, David Andre, Forrest H. Bennett III, and Martin Keane,
  *Genetic Programming III: Darwinian Invention and Problem Solving*.
- John R. Koza, *Genetic Programming II: Automatic Discovery of Reusable
  Programs*.
- Multiple function-defining branches and architecture-altering operations:
  Koza III, chapters 2 and 3.

## License

New code and modifications in this repository are under the MIT License
(see `COPYING`).

Copyright (c) Gastón Novita, All rights reserved.

## Koza attribution

This is an independent Common Lisp implementation inspired by John R. Koza's
*Genetic Programming II: Automatic Discovery of Reusable Programs* (MIT Press,
1994) and *Genetic Programming III: Darwinian Invention and Problem Solving*
(Morgan Kaufmann, 1999). Koza's books and original Little Lisp software are
copyrighted by John R. Koza. The Koza I/Little Lisp source code is not included
in this repository.

The following license was part of the original project and is reproduced for
attribution and historical reference. It applies to Koza's Little Lisp software,
not to the new code in this repository, which remains under the MIT License above.

### Little Lisp License

Copyright (c) John Koza, All rights reserved.
U.S. Patent #4,935,877.  Other patents pending.

ROYALTY-FREE LICENSE FOR GENETIC PROGRAMMING SOFTWARE FOR ACADEMIC PURPOSES
===========================================================================

As you requested, I am including herewith, FOR YOUR INFORMATION AND
INSPECTION ONLY, a copy of my "SIMPLE LISP" software for genetic
programming.  Please be advised that this software is copyrighted and
is the subject of my United States patents 4,935,877 5,136,686, and
5,148,513, foreign counterparts, and other patents pending.

IF YOU WISH TO USE THIS SOFTWARE, you may have a royalty-free,
non-exclusive license under these proprietary rights, without the
right of sublicense, to use (but not to make or sell) the software
for academic purposes only and only then if you receive no money
or other valuable consideration as a result of its use.
Neither the software (or copies thereof) nor this license is
transferrable for any reason.  This license grants you the right
to make copies of the software which are necessary for your use,
and your use only, provided each copy incorporates this license.

The programs, procedures, and applications presented in this software
have been included for their instructional value.  The publisher and
author offer NO WARRANTY OF FITNESS OR MERCHANTABILITY for any
particular purpose or accept any liability with respect
to these programs, procedures, and applications.

If you wish such a licence under these terms, please indicate so
by signing and dating a copy of this notice and returning it to me
at John Koza, Box K, Los Altos, California 94023 USA.

I would like the above license and hereby agree to the terms set forth above.

LICENSEE:

NAME (Print) ____________________________

Signature _________________________________

Date _______________

PHYSICAL ADDRESS _______________________________

CITY, STATE, ZIP _______________________________

COUNTRY _____________________________

E-MAIL ADDRESS _______________________________________

PHONE ___________________________
