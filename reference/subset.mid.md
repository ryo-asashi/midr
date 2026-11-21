# Extract Submodels from MID Models

[`subset()`](https://rdrr.io/r/base/subset.html) methods for a fitted
MID model ("mid") or a collection of models ("mids"). It creates a
lightweight view of a fitted MID model containing only a specified
subset of terms (main effects and/or interactions).

## Usage

``` r
# S3 method for class 'mid'
subset(x, terms = NULL, intercept = NULL, data = NULL, ...)

# S3 method for class 'mids'
subset(x, ...)
```

## Arguments

- x:

  a fitted model object (`"mid"` or `"mids"`).

- terms:

  a character vector, formula, or
  [`terms`](https://rdrr.io/r/stats/terms.html) object specifying the
  subset of terms to retain.

- intercept:

  logical. If `TRUE`, the intercept is preserved; if `FALSE`, it is
  zeroed out. If `NULL` (default), inferred from `terms`.

- data:

  optional data.frame used only when `terms` contains `"."` shorthand.

- ...:

  further arguments passed to methods.

## Value

A `"mid"` (or `"mids"`) object containing only the selected component
functions.

## Examples

``` r
# Extract a submodel from a fitted MID model
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ ., airquality, lambda = .5)
#> 'model' not passed: response variable in 'data' is used
predict(mid, head(airquality), type = "terms")
#>          Solar.R        Wind      Temp    Month        Day
#> [1,] -1.23180529   0.6267415 -16.54267 5.018229   7.678869
#> [2,] -2.46344993  -0.7982557 -13.38365 5.018229  -5.181203
#> [3,]  0.90231186  -9.1915231 -15.80051 5.018229 -14.584427
#> [4,] -0.03110356  -8.7762691 -17.69587 5.018229 -12.711922
#> [5,]  0.00000000 -12.6868576 -18.42727 5.018229 -10.831562
#> [6,]  0.00000000 -10.8781795 -16.49493 5.018229  -8.951203
#> attr(,"constant")
#> [1] 42.0991
submid <- subset(mid, ~ Wind + Temp)
predict(submid, head(airquality), type = "terms")
#>             Wind      Temp
#> [1,]   0.6267415 -16.54267
#> [2,]  -0.7982557 -13.38365
#> [3,]  -9.1915231 -15.80051
#> [4,]  -8.7762691 -17.69587
#> [5,] -12.6868576 -18.42727
#> [6,] -10.8781795 -16.49493
#> attr(,"constant")
#> [1] 42.0991
```
