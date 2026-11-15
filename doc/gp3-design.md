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

It also provides structure-preserving crossover with point typing and a run
entry point, `run-architecture-altering-system`, that uses the operation
fractions from Koza's Boolean even-5-parity run.

The implementation does not include automatically defined iterations,
loops, recursion, or storage (ADI, ADL, ADR, ADS), nor the GPPS, analog
circuit, evolvable-hardware, or biology material from the books.

## Program representation

An `adf-program` contains:

- `adfs`: a vector of function-defining branch bodies in `ADF0 … ADF(N-1)`
  order
- `rpb`: the result-producing branch
- `branch-info`: a vector of `branch-info` records in
  `ADF0 … ADF(N-1), RPB` order

Each `branch-info` records the branch name and kind, arity, formal
arguments, function set and function-argument map, terminal set, result
type, and allowed ADF references. The metadata is part of the program, so
programs with different architectures can coexist in one population.

ADF invocation uses lexical scope. An ADF body sees bindings for its own
formal arguments (`ARG0`, `ARG1`, …) and never inherits the caller's
environment. ADF calls are resolved by branch index, using the `ADF<N>`
symbol name.

## Point typing

Every branch has a result type, currently `:boolean` throughout the
supplied examples. Crossover checks that the contributing branch and the
receiving branch have the same result type. It then validates the
contributing fragment against the receiving branch metadata:

- non-ADF function operators must occur in the receiving function set with
  the correct arity
- ADF calls must name an existing branch, occur in the receiving function
  set, satisfy the branch arity, and be permitted by the receiving
  branch's allowed ADF indices when that list is present
- terminal symbols must occur in the receiving terminal set
- numeric constants must be permitted by the receiving terminal set
- recursively, every subexpression must pass the same checks

For the Boolean examples every point has the same point type, so a valid
fragment can be inserted at any point in a compatible receiving branch.
This keeps the current implementation faithful to the supplied examples
without introducing typed subtrees that the examples do not use.

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
arguments. The new ADF receives the function set and allowed ADF references
of the branch it was extracted from, plus the new formal arguments in its
terminal set. The value returned by the program is unchanged.

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
of the deleted branch is replaced by a randomly generated subtree using the
containing branch's function and terminal sets. This operation can change
the value returned by the program.

### Argument deletion

Select an ADF and one of its formal arguments. Remove the formal and shift
the remaining formals down. References to the deleted formal in the ADF body
are replaced by random regeneration. Remove the corresponding actual
argument from every call to the ADF. This operation can change the value
returned by the program.

Deletion regeneration uses `*max-depth-for-new-subtrees-in-mutants*` as the
depth limit for newly generated subtrees.

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

The current implementation uses a single population. It does not implement
Koza's 64-deme island model; `lparallel` is used for fitness evaluation and
initial-population generation, not for distributed demes.

## Selected deviations and limitations

- All point types are Boolean in the current examples.
- Architecture operation placement in each generation follows the existing
  kernel's deterministic fraction-window style rather than an independent
  random draw for every individual. The configured fractions therefore
  describe the composition of each generation.
- Mutation is not used in the architecture-altering example. If the
  configured fractions leave a remainder, the remainder is filled by
  reproduction.
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
