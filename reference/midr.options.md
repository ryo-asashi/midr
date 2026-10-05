# Set or Get Global Options for the midr Package

`midr.options()` manages global settings specific to the **midr**
package, such as computational solvers and color theme environments.

`midr.par()` manages graphical parameters for base R graphics.

## Usage

``` r
midr.options(...)

midr.par(...)
```

## Arguments

- ...:

  For `midr.options()`: options to be defined in `name = value` form. If
  no arguments are provided, it returns all current package options. If
  character strings are provided, it returns the values of those
  options. For `midr.par()`: graphical parameters in `name = value` form
  can be supplied as arguments.

## Value

When called without arguments, `midr.options()` returns a named list of
all current options with the `midr.` prefix. When called with character
strings, `midr.options()` returns a named list of the requested options.
When called with `name = value` pairs, `midr.options()` invisibly
returns a named list of the previous values.

`midr.par()` returns the previous values of the changed parameters in an
invisible named list.

## Details

`midr.options()` provides an interface to R's global
[`options()`](https://rdrr.io/r/base/options.html) but safely scopes all
parameters with a `midr.` prefix. To prevent typos, it validates option
names against expected prefixes:

- `solver.*`: For registering custom least-squares solvers; e.g.,
  `solver.TAG = FUN`, where `FUN(x, y)` returns a list containing at
  least `coefficients` and possibly `residuals` and `rank`. The custom
  solver can be called via `interpret(method = "TAG")`.

- `color.theme.*`: For configuring color theme settings and registries.

- `verbosity`: For controlling message verbosity. See
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md).

`midr.par()` wraps [`par()`](https://rdrr.io/r/graphics/par.html) to
enforce a themed aesthetic as the default, which can be further
customized by the user.
