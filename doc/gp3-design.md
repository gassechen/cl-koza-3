# Koza III architecture-altering design

This document describes the `cl-koza-3` implementation of the
architecture-altering operations for automatically defined functions (ADFs)
from *Genetic Programming III* and the 1995 IJCAI architecture-altering
paper.

## Scope

The implementation covers the six architecture-altering operations for
subroutines:

1. branch duplication
2. argument duplication
3. branch creation
4. argument creation
5. branch deletion
6. argument deletion

It also provides structure-preserving crossover with point typing over
`:boolean` and `:number`, typed ADF calls, an automatically defined iteration
(ADI) core, ADL/ADR/ADS auxiliary branches, and run entry points,
`run-architecture-altering-system`, `run-adi-architecture-altering-system`, and
`run-gpps-system`, that use the operation fractions from Koza's Boolean
even-5-parity run.

The ADI core iterates each ADI over an external sequence and exposes the
current element and zero-based index. ADL repeats a body a control count of
times, ADR recurses to a caller-supplied depth limit, and ADS reads and
updates mutable cells. The architecture-altering operations preserve all
auxiliary bodies and update calls and metadata when ADF branches change.
Creation replaces a selected subtree with a call to a new ADI, ADL, ADR, or
ADS branch and uses that subtree as the new branch's initial state. Deletion
removes a branch, regenerates calls to it, and renumbers later branches of the
same family and their metadata. Duplication appends a copy of a branch and
randomly redirects some calls. `run-gpps-system` adds a lightweight GPPS-style
entry point with per-generation architecture policies and templates. The
implementation does not include the full GPPS, analog circuit,
evolvable-hardware, or biology material from the books.

## Program representation

An `adf-program` contains:

- `adfs`: a vector of function-defining branch bodies in `ADF0 … ADF(N-1)`
  order
- `adis`: a vector of auxiliary branch bodies in canonical
  `ADI0 … ADI(M-1), ADL0 … ADL(K-1), ADR0 … ADR(R-1), ADS0 … ADS(S-1)`
  order
- `rpb`: the result-producing branch
- `branch-info`: a vector of `branch-info` records in
  `ADF0 … ADF(N-1), ADI0 … ADI(M-1), ADL0 … ADL(K-1),
  ADR0 … ADR(R-1), ADS0 … ADS(S-1), RPB` order

Each `branch-info` records the branch name and kind, arity, formal
arguments, function set and function-argument map, function types, terminal
set and terminal types, result type, and allowed ADF references. Auxiliary
metadata also records the iteration source, element variable, and index
variable. The metadata is part of the program, so programs with different
architectures can coexist in one population.

ADF and auxiliary invocation use lexical scope. An ADF body sees bindings for
its own formal arguments (`ARG0`, `ARG1`, …) and never inherits the caller's
environment. ADI and ADL bodies see their state formals plus iteration
bindings; ADR bodies see the current recursion depth; ADS bodies see the
current storage value. Calls are resolved by branch kind and index using the
`ADF<N>`, `ADI<N>`, `ADL<N>`, `ADR<N>`, and `ADS<N>` symbol names. An ADI
starts from its call arguments, evaluates its body once per element, updates
the state to the body result, and returns the final state. ADL follows the
same state protocol for a repeat count; ADR increments `*recursion-depth*`
until the depth limit; ADS stores each body result in `*storage-cells*`.

## Point typing

Branches carry a result type and optional function and terminal type
metadata. The type system currently has two types:

- `:boolean`
- `:number`

Function type entries have the form
`(function result-type &rest argument-types)`. Terminal type entries are
alists of `(terminal . type)`. Integer and floating-point random constants
are `:number`. Branches with no type metadata retain the original Boolean
behavior.

`typed-tree-type` infers the type of a subtree under the receiving branch's
metadata and returns `NIL` for an invalid subtree. It checks:

- non-ADF function operators must occur in the receiving function set with
  the correct arity and declared argument types
- ADF and auxiliary calls must name an existing branch, satisfy its arity, and
  match the target branch's result type and formal-argument types; ADF calls
  must also be permitted by the receiving branch's allowed ADF indices
- terminal symbols must occur in the receiving terminal set and have the
  expected type
- numeric constants must be permitted by the receiving terminal set
- recursively, every subexpression must pass the same checks

Point-typed crossover no longer requires the contributing and receiving
branches to have the same result type. Instead, it computes the type of the
contributing fragment and requires it to match the type of the receiving
point. The receiving point is either selected explicitly or drawn from
`collect-valid-pointers`, which filters points by the fragment type. For the
Boolean examples every point still has type `:boolean`, so the original
behavior is preserved.

Typed random generation uses lookahead: a function is eligible only when all
of its argument types can be generated within the remaining depth. This
allows a Boolean branch without Boolean terminals to close through
comparison functions, while preventing `AND`/`OR`/`NOT` from requiring a
Boolean leaf at the depth limit.

Typed ADF calls take their result type from the target branch and their
argument types from the target branch's formals. Initial population
generation passes the complete program metadata to the generator, so ADF
calls are available while creating typed trees. Typed initial generation
honors the configured full, grow, or ramped-half-and-half method. Typed
mutation infers the selected point's type and regenerates that point with a
subtree of the same type.

## Operations

All operations return a new program and do not mutate the input. An
operation returns `NIL` when it cannot be applied. Branch metadata is
copied and updated whenever a branch is added, removed, duplicated, or has
its arity changed.

### Branch duplication

Select an existing ADF. Create a new ADF at the end of the ADF vector with
the same body and formals. For every invocation of the original ADF,
randomly leave the invocation as-is or replace its operator with the new
ADF. Add the new ADF to the function set and allowed references of every
branch that could call the original. The value returned by the program is
unchanged.

### Argument duplication

Select an ADF and one of its formal arguments. Append a new formal
argument. In the ADF body, randomly leave references to the original
formal as-is or replace them with the new formal. At every call to the
ADF, duplicate the actual argument corresponding to the selected formal.
Update the ADF arity in every branch metadata record that invokes it. The
value returned by the program is unchanged.

### Branch creation

Select a branch and a subtree within it. The subtree becomes the body of a
new ADF. The selected subtree is replaced by a call to the new ADF. Nodes
inside the new ADF body that are not constants are made formal arguments,
so the new ADF has no free variables. The call passes those subtrees as
arguments. The new ADF receives the function set, function types, terminal
types, and allowed ADF references of the branch it was extracted from, plus
the new formal arguments in its terminal set. Its result type is the type of
the extracted subtree, and each new formal inherits the type of the
designated subtree. The value returned by the program is unchanged.

`*branch-creation-designation-probability*` controls whether constant
subtrees are also designated as arguments. Non-constant terminal symbols
are always designated, which prevents ADF bodies from referring to
result-producing-branch globals such as `D0` or `D1`.

### Argument creation

Select an ADF branch and a subtree within its body. Replace that subtree
with a new formal argument. At every call to the ADF, append an
instantiated copy of the removed subtree as the new actual argument. Update
the ADF arity wherever the ADF is invoked. The value returned by the
program is unchanged.

### Branch deletion

Select an ADF and remove it. Renumber later `ADF<N>` branches and update
function sets, ADF references, and allowed branch indices. Each invocation
of the deleted branch is replaced by a randomly generated subtree of the
type expected by the containing branch. Deletion uses the same typed
lookahead as typed subtree generation and passes temporary program metadata
so replacement subtrees may contain ADF calls. This operation can change the
value returned by the program.

### Argument deletion

Select an ADF and one of its formal arguments. Remove the formal and shift
the remaining formals down. References to the deleted formal in the ADF body
are replaced by random regeneration. Remove the corresponding actual
argument from every call to the ADF. This operation can change the value
returned by the program.

Deletion regeneration uses `*max-depth-for-new-subtrees-in-mutants*` as the
depth limit for newly generated subtrees and may include ADF calls available
under the updated program metadata. The operation returns `NIL` when the
updated branch cannot generate a subtree of the required type within that
limit.

## Run loop

`run-architecture-altering-system` accepts the same six values from its
problem function as `run-adf-system`:

1. ADF branch specifications
2. result-producing branch specification
3. fitness-cases creator
4. fitness function
5. parameter definer
6. termination predicate

The breeding function is selected by the parameter definer. The relevant
fractions are:

| Operation | Fraction |
| --- | ---: |
| point-typed crossover | 0.74 |
| reproduction | 0.10 |
| mutation | 0.00 |
| branch duplication | 0.05 |
| argument duplication | 0.05 |
| branch deletion | 0.005 |
| argument deletion | 0.005 |
| branch creation | 0.05 |
| argument creation | 0.00 |

The crossover fraction is the sum of
`*crossover-at-any-point-fraction*` and
`*crossover-at-function-point-fraction*`. The even-5-parity example sets
the first to `0.74` and the second to `0.0`.

Crossover slots are filled in pairs. The first child receives a fragment
from the first parent; the second child receives a fragment from the second
parent. Each direction uses point-typed crossover. If point-typed crossover
cannot produce a child, the selected parent is copied instead.

Architecture-altering operations are unary. The program is selected using
the configured method of selection. If the selected operation is not
applicable, for example branch duplication when the program has no ADF, the
selected program is reproduced.

The run uses Koza's minimalist initial architecture for even-5-parity: one
two-argument `ADF0` plus the result-producing branch. Branch creation can
grow additional ADFs from either `ADF0` or the result-producing branch.

`run-adi-system` is the entry point for a fixed ADI architecture. Its problem
function returns ADF specs, ADI specs, the RPB spec, and the same four
run-control values as `run-adf-system`. `run-adi-architecture-altering-system`
uses the same seven-value contract with the architecture-altering breeder and
the ADI fractions `*adi-creation-fraction*` and `*adi-deletion-fraction*`.

ADI specs carry `:iteration-source`, `:iteration-variable`, and
`:iteration-index-variable` metadata. The iteration source is looked up in the
caller's environment and falls back to `*iteration-sequence*`. While an ADI
body is evaluated, `*iteration-sequence*` is bound to the enclosing ADI's
sequence, so nested ADI calls reuse it. ADI branches default to one state
formal. ADI creation uses the metadata of the first initial ADI spec unless
the dynamic creation defaults are overridden.

`run-aux-system` accepts ADF, ADI, ADL, ADR, and ADS specs in that order,
plus the RPB spec and the four run-control values. `run-adl-system`,
`run-adr-system`, and `run-ads-system` are one-family wrappers with the
seven-value `run-adi-system` contract. All auxiliary specs use the same
iteration metadata fields; ADL interprets the source as a repeat count,
ADR as a depth limit, and ADS as a storage-cell key. The `auxiliary-branches`
example verifies a hand-built program that exercises all three families.

### GPPS-style policies

`run-gpps-system` takes a `gpps-problem` instead of a multi-valued problem
function. The struct carries the five branch-spec lists, the RPB spec, the
four run-control callables, auxiliary body templates, initial architecture
fractions, an architecture policy, and maximum branch counts. The function
coerces a function or symbol to a `gpps-problem`, validates the branch specs,
initializes the run, and binds every architecture fraction dynamically before
calling `cl-koza-3.kernel::run-system` with the architecture-aware breeder.

The problem's `:architecture-fractions` plist is applied after the defaults.
The `:architecture-policy` is called once before each breeding generation as
`(funcall policy generation population)`. Its returned plist overrides the
fractions for that generation, so architecture decisions can depend on the
current generation and population. Names such as `:adl-creation` and
`:adl-creation-fraction` are aliases. The policy may also return
`:maximum-branch-counts` with a plist such as `(:adl 2 :adr 2 :ads 2)`.

Auxiliary creation uses `:aux-templates` from the problem first, then the
metadata of an existing branch of the same family, then the family-specific
`*ad*-creation-*` defaults. Templates are ordinary `branch-spec` objects and
are resolved to `branch-info` records through the kernel's spec resolution
functions. Creation and duplication honor `*gpps-maximum-branch-counts*`.
The `gpps-architecture-altering` example rotates the active family by
generation and uses templates and maximum counts to keep runs bounded.

## Island model

Runs use a single population when `*number-of-demes*` is `1`. For an
island-model run, set `*number-of-demes*` to a value greater than one and
set the total population size to a multiple of the deme count. The kernel
derives the deme size as `size-of-population / *number-of-demes*`.

Migration is controlled by `*migration-rate*`, Koza's boatload fraction
`B`. On every generation after generation 0, the best
`floor(B * deme-size)` individuals in each deme are copied to each neighbor
on a two-dimensional torus, replacing the worst individuals in the
destination. Destination fitness is renormalized and sorted before
breeding. A migration rate of `0.08` therefore sends 8% of a deme in each
of the four directions.

Koza's configuration for the architecture-altering even-5-parity run is:

```lisp
(let ((*number-of-demes* 64)
      (*migration-rate* 0.08)
      (*deme-topology* :torus))
  (run-architecture-altering-system ... 96000))
```

This gives 64 demes of 1,500 individuals and 120 migrants per direction.
`:torus` requires a perfect-square deme count. Tiny torus grids can have
duplicate north/south or east/west neighbors; those neighbors are
deduplicated.

Migration happens at synchronous generation boundaries. Koza's original
implementation ran the demes asynchronously on a transputer mesh, so the
migration schedule is not bit-for-bit identical even though the topology,
deme sizes, and per-direction boatload match.

## Observed 64-deme run

A run with seed `1.0`, 64 demes of 1,500 individuals, an 8% migration rate,
and the example's operation fractions solved even-5-parity at generation 9
with standardized fitness `0.0` and `32/32` hits. The best individual had
two ADFs: `ADF0` with three arguments and `ADF1` with four arguments. The
run took about 58 seconds of wall-clock time and used about 929 MB of
resident memory.

The final population contained:

| ADFs | Individuals |
| ---: | ---: |
| 0 | 19 |
| 1 | 3,942 |
| 2 | 56,509 |
| 3 | 29,311 |
| 4 | 5,661 |
| 5 | 539 |
| 6 | 19 |

The corresponding single-population run with 1,000 individuals and the same
seed reached standardized fitness `8.0` with 24 hits. The comparison is a
single-run illustration, not an estimate of success probability.

## Selected deviations and limitations

- The implemented point types are `:boolean` and `:number`; this is not the
  full Koza type hierarchy.
- Auxiliary creation adds a one-state-formal branch from an existing subtree.
  Auxiliary deletion regenerates calls to the removed branch, so it can change
  program behavior and can leave new auxiliary bodies with a reduced function
  set.
- ADI iteration sequences, ADL counts, ADR depth limits, and ADS cell names
  are supplied through the caller's environment; the kernel does not evolve
  sequence constructors, loop bounds, or storage-cell constructors.
- Typed deletion returns `NIL` when the updated branch cannot generate a
  subtree of the required type within the configured mutant-subtree depth.
- The island model uses synchronous generation-boundary migration; Koza's
  original even-5-parity runs used asynchronous demes on a transputer mesh.
- Architecture operation placement in each generation follows the existing
  kernel's deterministic fraction-window style rather than an independent
  random draw for every individual. The configured fractions therefore
  describe the composition of each generation.
- Mutation is not used in the architecture-altering example, although typed
  mutation is implemented and tested. If the configured fractions leave a
  remainder, the remainder is filled by reproduction.
- GPPS-style policies are ordinary Lisp functions called before breeding. The
  implementation does not provide Koza's declarative GPPS decision language
  or evolve the policies themselves.
- Operation applicability depends on the selected program's architecture.
  Duplication, creation, and deletion operations fall back to reproduction
  when the selected program lacks the required ADF or formal argument.
- The current branch-creation implementation preserves the value of the
  selected program by construction; the deletion operations intentionally
  regenerate removed references at random.

## References

- John R. Koza. *Gene Duplication to Enable Genetic Programming to
  Concurrently Evolve Both the Architecture and Work-Performing Steps of a
  Computer Program.* IJCAI 1995.
- John R. Koza, David Andre, Forrest H. Bennett III, and Martin Keane.
  *Genetic Programming III: Darwinian Invention and Problem Solving.*
  Chapters 2, 3, and 12.
