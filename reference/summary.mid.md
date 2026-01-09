# Summarize MID Models

For "mid" objects, an S3 method of
[`summary()`](https://rdrr.io/r/base/summary.html) prints a
comprehensive summary of a fitted MID model.

## Usage

``` r
# S3 method for class 'mid'
summary(object, digits = max(3L, getOption("digits") - 2L), top.n = 10L, ...)
```

## Arguments

- object:

  a "mid" object to be summarized.

- digits:

  the number of significant digits for printing numeric values.

- top.n:

  the maximum number of top-ranked terms to be printed in the MID
  importance table.

- ...:

  arguments to be passed to other methods (not used in this method).

## Value

`summary.mid()` returns the original "mid" object invisibly.

## Details

The S3 method `summary.mid()` generates a comprehensive overview of the
fitted MID model. The output includes the following components: (1)
"Call" - the function call used to fit the MID model. (2) "Uninterpreted
Variation Ratio" - a key metric indicating the proportion of the target
model's variance that is not explained by the MID model. Lower values
suggest a better fit. (3) "Residuals" - a five-number summary (Min, 1Q,
Median, 3Q, Max) of the working residuals. This aids in assessing model
fit and identifying potential biases. (4) "Encoding" - a summary of the
encoding schemes used for each variable in the MID model. (5)
"Importance" - a list of the top terms ranked by their MID importance,
which quantifies their average contribution to the model's predictions.

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
#> 
#> Importance:
#>    term importance order
#> 1 speed     18.138     1
```
