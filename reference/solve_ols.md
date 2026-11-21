# Internal OLS Solver

An OLS solver utilized in
[`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
supporting multiple computational methods. It utilizes
[`fastLmPure()`](https://rdrr.io/pkg/RcppEigen/man/fastLm.html) for
single-response variables to optimize speed independently of the
environment, and uses base BLAS computations for multiple responses.

## Usage

``` r
solve_ols(x, y, tol = 1e-07, method = 0L, ...)
```

## Arguments

- x:

  a design matrix.

- y:

  a numeric vector or matrix of the response variable.

- tol:

  the tolerance used in solvers. Defaults to `1e-7`.

- method:

  an integer or character string specifying the computational method. If
  a character string (e.g., `"FUNC"`) is passed, the function registered
  in the global option `"midr.solver.FUNC"` is used.

- ...:

  additional arguments.

## Value

`solve_ols()` returns a list containing at least the `coefficients`, and
potentially `residuals` or `rank` depending on the computational method.
