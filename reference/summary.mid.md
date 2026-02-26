# Summarize MID Models

[`summary()`](https://rdrr.io/r/base/summary.html) methods for a fitted
MID model ("mid") or a collection of models ("mids"). It prints a
comprehensive summary of the model structure and fit quality.

## Usage

``` r
# S3 method for class 'mid'
summary(
  object,
  diagnose = FALSE,
  digits = max(3L, getOption("digits") - 2L),
  ...
)

# S3 method for class 'mids'
summary(object, max.nmodels = 1L, ...)
```

## Arguments

- object:

  a "mid" or "mids" object to be summarized.

- diagnose:

  logical. If `TRUE`, the diagnosis plot is displayed. Defaults to
  `FALSE`.

- digits:

  the number of significant digits for printing numeric values.

- ...:

  arguments to be passed to
  [`graphics::panel.smooth()`](https://rdrr.io/r/graphics/panel.smooth.html)
  for the diagnosis plot.

- max.nmodels:

  an integer specifying the maximum number of models to summarize for a
  "mids" collection.

## Value

`summary.mid()` returns the original "mid" object invisibly.

`summary.mids()` returns the original "mids" object invisibly.

## Details

The S3 method `summary.mid()` generates a comprehensive overview of the
fitted MID model. The output includes:

- **Call**: the function call used to fit the MID model.

- **Link**: name of the link function used to fit the MID model, if
  applicable.

- **Uninterpreted Variation Ratio**: proportion of target model variance
  not explained by MID model.

- **Residuals**: five-number summary of (working) residuals.

- **Encoding**: summary of encoding schemes per variable.

- **Diagnosis**: residuals vs fitted values plot (displayed only when
  `diagnose = TRUE`).

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`print.mid`](https://ryo-asashi.github.io/midr/reference/print.mid.md)

## Examples

``` r
# Summarize a fitted MID model
data(cars, package = "datasets")
mid <- interpret(dist ~ speed, cars)
#> 'model' not passed: response variable in 'data' is used
summary(mid)
#> 
#> Call:
#> interpret(formula = dist ~ speed, data = cars)
#> 
#> Uninterpreted Variation Ratio:
#> [1] 0.22636
#> 
#> Residuals:
#>       Min        1Q    Median        3Q       Max 
#> -24.50000  -8.06250  -0.33333   8.37500  29.50000 
#> 
#> Encoding:
#>       main.effect
#> speed  linear(17)
```
