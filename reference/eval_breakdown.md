# Evaluate Breakdown of Model Predictions

`eval_breakdown()` calculates the contribution of each component
function (or each variable when `by = "variable"`) of a fitted model to
one or more predictions.

## Usage

``` r
eval_breakdown(object, ...)

# S3 method for class 'mid'
eval_breakdown(
  object,
  data = NULL,
  rows = NULL,
  sort = TRUE,
  by = c("term", "variable"),
  ...
)

# S3 method for class 'mids'
eval_breakdown(
  object,
  data = NULL,
  rows = NULL,
  sort = TRUE,
  by = c("term", "variable"),
  ...
)

# S3 method for class 'midbrk'
print(x, digits = max(3L, getOption("digits") - 2L), ...)

# S3 method for class 'midbrks'
print(x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...)

# S3 method for class 'midbrk'
summary(object, ...)

# S3 method for class 'midbrks'
summary(object, shape = c("wide", "long"), terms = NULL, fill = 0, ...)

# S3 method for class 'midbrk'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- object:

  a `"mid"` or `"mids"` object.

- ...:

  additional arguments passed to methods.

- data:

  a data.frame containing one or more observations. Extracted
  automatically from `object` when omitted.

- rows:

  optional row index/name (or vector of indices/names for a single
  `"mid"` object) selecting observation(s) from `data`. If omitted and
  `data` has multiple rows, only the first observation is used.

- sort:

  logical. If `TRUE` (default), sorts contributions by descending
  absolute magnitude.

- by:

  character: `"term"` (default) for component-term breakdown or
  `"variable"` for variable-level breakdown (splitting interactions
  equally).

- x:

  a `"midbrk"` or `"midbrks"` object.

- digits:

  number of significant digits to print.

- n:

  maximum number of rows to print for `"midbrks"`.

- shape:

  output table shape for `summary.midbrks()`: `"wide"` (default) or
  `"long"`.

- terms:

  optional character vector of terms/variables to include.

- fill:

  value used for terms absent from a model in `wide` format (default
  `0`).

- row.names, optional:

  passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Value

For a single observation, an object of class `"midbrk"` containing
`breakdown`, `data`, `intercept`, `prediction`, and `by` (plus
`linear.predictor` when a link function is present). For a `"mids"`
collection or a `"mid"` object with multiple `rows` selections, a
collection of class `c("midbrks", "midlist")`.

## Details

For a target observation \\\mathbf{x}\\, the linear predictor is
decomposed into the intercept and individual component contributions:

\$\$g(\mathbf{x}) = g\_\emptyset + \sum\_{j} g_j(x_j) + \sum\_{j\<k}
g\_{jk}(x_j, x_k)\$\$

When `by = "variable"`, each main effect is attributed to its variable
and each interaction effect is split equally among its constituent
variables. For a single `"mid"` object, passing multiple indices or row
names to `rows` returns a `c("midbrks", "midlist")` collection for
comparing breakdowns across observations.

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`plot.midbrk`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md),
[`ggmid.midbrk`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md)
