# Evaluate Importance of Model Components

`eval_importance()` calculates the term/variable importance of a fitted
model. This measures the average contribution of each component
function, or each variable when `by = "variable"`, across a dataset.

## Usage

``` r
eval_importance(object, ...)

# S3 method for class 'mid'
eval_importance(
  object,
  data = NULL,
  weights = NULL,
  sort = TRUE,
  measure = c("mae", "rmse", "medae"),
  by = c("term", "variable"),
  max.nsamples = 10000L,
  seed = NULL,
  ...
)

# S3 method for class 'mids'
eval_importance(
  object,
  data = NULL,
  weights = NULL,
  sort = TRUE,
  measure = c("mae", "rmse", "medae"),
  by = c("term", "variable"),
  max.nsamples = 10000L,
  seed = NULL,
  ...
)

# S3 method for class 'midimp'
print(x, digits = max(3L, getOption("digits") - 2L), ...)

# S3 method for class 'midimps'
print(x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...)

# S3 method for class 'midimp'
summary(object, ...)

# S3 method for class 'midimps'
summary(object, shape = c("wide", "long"), terms = NULL, fill = NA_real_, ...)

# S3 method for class 'midimp'
as.data.frame(x, row.names = NULL, optional = FALSE, long = FALSE, ...)
```

## Arguments

- object:

  a `"mid"` or `"mids"` object.

- ...:

  additional arguments passed to methods.

- data:

  a data.frame containing observations for computing importance.
  Extracted automatically from `object` when omitted.

- weights:

  an optional numeric vector of sample weights.

- sort:

  logical. If `TRUE` (default), sorts output in descending order of
  importance.

- measure:

  character (or integer `1`–`3` for backward compatibility): `"mae"` (or
  `1`) for mean absolute effect, `"rmse"` (or `2`) for root mean square
  effect, or `"medae"` (or `3`) for median absolute effect.

- by:

  character: `"term"` (default) for component-term importance or
  `"variable"` for variable-level importance (splitting interactions
  equally).

- max.nsamples:

  maximum number of rows to retain in `predictions`. Larger datasets are
  randomly subsampled (weighted if `weights` is supplied).

- seed:

  optional integer seed for subsampling.

- x:

  a `"midimp"` or `"midimps"` object.

- digits:

  number of significant digits to print.

- n:

  maximum number of rows to print for `"midimps"`.

- shape:

  output table shape for `summary.midimps()`: `"wide"` (default) or
  `"long"`.

- terms:

  optional character vector of terms/variables to include.

- fill:

  value used for terms absent from a model in `wide` format (default
  `NA_real_`).

- row.names, optional:

  passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- long:

  logical. If `TRUE`, returns a long-format data.frame of sample-level
  term contributions merged with `importance` (and `order`).

## Value

For a `"mid"` object, an object of class `"midimp"` containing
`importance`, `predictions`, `measure`, and `by`. For a `"mids"`
collection, a collection of class `c("midimps", "midlist")`.

## Details

For a component function \\g_S\\, the MID importance is defined as the
mean absolute effect (`measure = "mae"`, default), root mean square
effect (`"rmse"`), or median absolute effect (`"medae"`) over the
observations in `data`:

\$\$\mathbf{I}(g_S) = \frac{1}{n} \sum\_{i=1}^n
\|g_S(\mathbf{x}\_{i,S})\|\$\$

When `by = "variable"`, each main effect is attributed to its variable
and each interaction effect is split equally among its constituent
variables before computing the importance summary.

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`plot.midimp`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md),
[`ggmid.midimp`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md)
