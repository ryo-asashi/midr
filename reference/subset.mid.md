# Extract Submodels and Coefficients from MID Models

[`subset()`](https://rdrr.io/r/base/subset.html) and
[`coef()`](https://rdrr.io/r/stats/coef.html) methods for extracting a
subset of component functions from a fitted MID model (`"mid"` /
`"midrib"`) or a collection of models (`"mids"`).
[`subset()`](https://rdrr.io/r/base/subset.html) returns a lightweight
submodel object, while [`coef()`](https://rdrr.io/r/stats/coef.html)
returns a named list of the fitted intercept and component effect
tables.

## Usage

``` r
# S3 method for class 'mid'
subset(x, terms = NULL, intercept = NULL, ...)

# S3 method for class 'mids'
subset(x, ...)

# S3 method for class 'mid'
coef(object, terms = NULL, intercept = NULL, ...)

# S3 method for class 'midrib'
coef(object, ...)
```

## Arguments

- x, object:

  a fitted model object (`"mid"`, `"midrib"`, or `"mids"` for
  [`subset()`](https://rdrr.io/r/base/subset.html)).

- terms:

  an optional character vector, formula, or
  [`terms`](https://rdrr.io/r/stats/terms.html) object specifying the
  subset of terms (and intercept) to retain or extract. Defaults to all
  terms in the model.

- intercept:

  logical. If `TRUE`, the intercept is preserved or included; if
  `FALSE`, it is zeroed out or omitted. If `NULL` (default), inferred
  from `terms`.

- ...:

  further arguments passed to methods.

## Value

[`subset()`](https://rdrr.io/r/base/subset.html) returns a `"mid"` (or
`"mids"`) object containing only the selected component functions.
[`coef()`](https://rdrr.io/r/stats/coef.html) returns a named list
containing the intercept (if included) and data frames of component
effects (excluding `.density`) for each matched term.

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`predict.mid`](https://ryo-asashi.github.io/midr/reference/predict.mid.md)

## Examples

``` r
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ Wind * Temp, airquality, k = c(5, 3))
#> 'model' not passed: response variable in 'data' is used

# Extract a submodel containing only Wind and Temp
submid <- subset(mid, ~ Wind + Temp)
predict(submid, head(airquality), type = "terms")
#>           Wind      Temp
#> [1,] -6.259685 -15.91298
#> [2,] -5.589946 -12.92598
#> [3,] -8.243855 -10.81277
#> [4,] -7.588329 -18.32599
#> [5,] -9.256941 -20.73899
#> [6,] -9.614501 -16.39558
#> attr(,"constant")
#> [1] 42.12931

# Extract component effect tables without the intercept
coef(mid, terms = c("Wind", "Wind:Temp"), intercept = FALSE)
#> $Wind
#>   Wind    .effect
#> 1  2.3  73.163877
#> 2  7.4  -6.259685
#> 3  9.7  -3.692354
#> 4 11.5  -7.588329
#> 5 20.7 -13.070913
#> 
#> $`Wind:Temp`
#>   Wind Temp     .effect
#> 1  2.3   57 -34.2733452
#> 2  9.7   57  -0.4128318
#> 3 20.7   57   7.2118188
#> 4  2.3   79  39.6965554
#> 5  9.7   79  -9.4929815
#> 6 20.7   79   0.2151154
#> 7  2.3   97 -56.7056946
#> 8  9.7   97  38.4237835
#> 9 20.7   97 -42.5028164
#> 
```
