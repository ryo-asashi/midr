# Evaluate Prediction Breakdowns

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
[`plot.midbrks`](https://ryo-asashi.github.io/midr/reference/plot.midbrks.md)

## Examples

``` r
data(mtcars, package = "datasets")
mid <- interpret(mpg ~ (wt + hp + cyl)^2, data = mtcars, lambda = 0.5)
#> 'model' not passed: response variable in 'data' is used

eval_breakdown(mid, data = mtcars, rows = "Toyota Corolla")
#> 
#> MID Breakdown of a Prediction
#> 
#> Intercept: 20.091
#> 
#> Prediction: 32.598
#> 
#> Breakdown of Effects:
#>     term     effect order
#> 1     wt  6.1942339     1
#> 2     hp  6.1633548     1
#> 3    cyl  0.1606907     1
#> 4  wt:hp -0.0084929     2
#> 5 hp:cyl -0.0016391     2
#> 6 wt:cyl -0.0010518     2
eval_breakdown(mid, data = mtcars, rows = "Toyota Corolla", by = "variable")
#> 
#> MID Breakdown of a Prediction
#> 
#> Intercept: 20.091
#> 
#> Prediction: 32.598
#> 
#> Breakdown of Effects:
#>   term  effect
#> 1   wt 6.18946
#> 2   hp 6.15829
#> 3  cyl 0.15935
eval_breakdown(mid, data = mtcars, rows = c("Toyota Corolla", "Mazda RX4", "Maserati Bora"))
#> 
#> MID Breakdown of Predictions
#> 
#> Intercept: 20.091, 20.091, 20.091
#> 
#> Prediction: 32.598, 21.537, 14.617
#> 
#> Breakdown of Effects:
#>     term Toyota Corolla  Mazda RX4 Maserati Bora
#> 1     wt      6.1942339  0.2825245   -2.1008e+00
#> 2     hp      6.1633548  1.1376198   -3.2679e+00
#> 3    cyl      0.1606907  0.0137268   -1.3312e-01
#> 4  wt:hp     -0.0084929 -0.0088640    1.7928e-02
#> 5 hp:cyl     -0.0016391  0.0096769    2.1584e-07
#> 6 wt:cyl     -0.0010518  0.0118479    1.0071e-02
```
