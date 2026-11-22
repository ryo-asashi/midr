# Evaluate Conditional Expectations

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
  ...
)
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

## Value

For a `"mid"` object, an object of class `"midcon"` containing
`observed`, `conditional`, `variable`, `values`, and `ids`. For a
`"mids"` collection, a collection of class `c("midcons", "midlist")`.

## Details

For each observation \\\mathbf{x}\_i\\, the ICE curve evaluates the
model across a grid of values \\x'\\ for the target `variable` while
holding all other predictors \\\mathbf{x}\_{i,\setminus j}\\ fixed:

\$\$f\_{\text{ICE}}(x') = g(x', \mathbf{x}\_{i,\setminus j})\$\$

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`plot.midcon`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md),
[`ggmid.midcon`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)
