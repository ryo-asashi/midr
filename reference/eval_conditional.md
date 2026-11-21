# Evaluate Individual Conditional Expectation (ICE)

`eval_conditional()` calculates Individual Conditional Expectation (ICE)
curves from a fitted model for a single predictor variable.

## Usage

``` r
eval_conditional(object, ...)

# S3 method for class 'mid'
eval_conditional(
  object,
  variable,
  data = NULL,
  resolution = 100L,
  max.nsamples = 500L,
  seed = NULL,
  type = c("response", "link"),
  keep.effects = TRUE,
  ...
)

# S3 method for class 'mids'
eval_conditional(
  object,
  variable,
  data = NULL,
  resolution = 100L,
  max.nsamples = 500L,
  seed = NULL,
  type = c("response", "link"),
  keep.effects = TRUE,
  ...
)

# S3 method for class 'midcon'
print(x, digits = max(3L, getOption("digits") - 2L), ...)

# S3 method for class 'midcons'
print(x, digits = max(3L, getOption("digits") - 2L), ...)

# S3 method for class 'midcon'
summary(object, ...)

# S3 method for class 'midcons'
summary(object, shape = c("long", "wide"), ids = NULL, ...)

# S3 method for class 'midcon'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- object:

  a `"mid"` or `"mids"` object.

- ...:

  additional arguments passed to methods.

- variable:

  character string naming the predictor variable to vary.

- data:

  a data.frame containing observations for ICE evaluation. Extracted
  automatically from `object` when omitted.

- resolution:

  number of evaluation grid points for continuous variables.

- max.nsamples:

  maximum number of observations to evaluate. Larger datasets are
  randomly subsampled.

- seed:

  optional integer seed for subsampling.

- type:

  character: `"response"` (default) or `"link"`.

- keep.effects:

  logical. If `TRUE` (default), stores individual term contributions for
  `variable`.

- x:

  a `"midcon"` or `"midcons"` object.

- digits:

  number of significant digits to print.

- shape:

  output table shape for `summary.midcons()`: `"long"` (default) or
  `"wide"`.

- ids:

  optional character vector of observation IDs to include.

- row.names, optional:

  passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Value

For a `"mid"` object, an object of class `"midcon"` containing
`observed`, `conditional`, `variable`, `values`, and `ids` (plus
`observed.effects` and `conditional.effects` when
`keep.effects = TRUE`). For a `"mids"` collection, a collection of class
`c("midcons", "midlist")`.

## Details

For each observation \\\mathbf{x}\_i\\, the ICE curve evaluates the
model across a grid of values \\x'\\ for the target `variable` while
holding all other predictors \\\mathbf{x}\_{i,\setminus j}\\ fixed:

\$\$f\_{\text{ICE}}(x') = g(x', \mathbf{x}\_{i,\setminus j})\$\$

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`plot.midcon`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md),
[`ggmid.midcon`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)
