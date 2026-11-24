# Evaluate Term and Variable Importance

`eval_importance()` calculates the term and variable importance of a
fitted model. This measures the average contribution of each component
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
[`plot.midimps`](https://ryo-asashi.github.io/midr/reference/plot.midimps.md)

## Examples

``` r
data(airquality, package = "datasets")
aq <- na.omit(airquality)
mid <- interpret(Ozone ~ (Solar.R + Wind + Temp)^2, data = aq, lambda = 0.5)
#> 'model' not passed: response variable in 'data' is used

eval_importance(mid)
#> 
#> MID Importance based on 111 Observations
#> 
#> Measure: Mean Absolute Contribution
#> 
#> Importance:
#>           term importance order
#> 1         Temp   13.22779     1
#> 2         Wind   11.03303     1
#> 3      Solar.R    5.93010     1
#> 4 Solar.R:Wind    1.17039     2
#> 5 Solar.R:Temp    0.98962     2
#> 6    Wind:Temp    0.59487     2
eval_importance(mid, by = "variable", measure = "rmse")
#> 
#> MID Importance based on 111 Observations
#> 
#> Measure: Root Mean Square Contribution
#> 
#> Importance:
#>      term importance
#> 1    Temp     15.354
#> 2    Wind     15.066
#> 3 Solar.R      7.200
```
