# Constraint Satisfaction Programming Language

This project includes:

- an AST for variables, domains, values, binary constraints, and `allDifferent`
- a handwritten parser with `//` and `#` comments
- a backtracking solver with early partial-constraint pruning
- an MRV-style variable-choice heuristic based on currently viable values
- a CLI and a small test suite

## Syntax

```csp
var WA, NT, SA : { red, green, blue };
var q1, q2, q3, q4 : 1..4;

constraint WA /= NT;
constraint q1 < q2;
constraint allDifferent [q1, q2, q3, q4];

solve;
```

Supported binary operators: `=`, `==`, `/=`, `!=`, `<`, `<=`, `>`, and `>=`.
Ordering comparisons currently can be applied to integer values.

Domains can be inclusive integer ranges (`1..9`) or explicit finite sets (`{ red, green, blue }`, `{ true, false }`, `{ "a", "b" }`).

## Build and run

```bash
cabal build
cabal run csp -- examples/australia.csp
cabal run csp -- --all examples/australia.csp
cabal test
```