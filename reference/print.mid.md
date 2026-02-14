# Print MID Models

`print.mid()` is an S3 method for "mid" objects that prints a concise
summary of a fitted MID model.

## Usage

``` r
# S3 method for class 'mid'
print(x, digits = max(3L, getOption("digits") - 2L), main.effects = FALSE, ...)
```

## Arguments

- x:

  a "mid" object to be printed.

- digits:

  an integer specifying the number of significant digits for printing.

- main.effects:

  logical. If `TRUE`, the MID values of each main effect are also
  printed.

- ...:

  arguments to be passed to other methods (not used in this method).

## Value

`print.mid()` returns the original "mid" object invisibly.

`print.midlist()` returns the original "midlist" object invisibly.

## Details

By default, the [`print()`](https://rdrr.io/r/base/print.html) method
for "mid" objects provides a quick overview of the model structure by
listing the number of main effect and interaction terms. If
`main.effects = TRUE` is specified, the method will also print the
contribution of each main effect at its sample points, providing a more
detailed look at the model's components.

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`summary.mid`](https://ryo-asashi.github.io/midr/reference/summary.mid.md)

## Examples

``` r
data(cars, package = "datasets")
mid <- interpret(dist ~ speed, cars)
#> 'model' not passed: response variable in 'data' is used

# Default print provides a concise summary
print(mid)
#> 
#> Call:
#> interpret(formula = dist ~ speed, data = cars)
#> 
#> Intercept: 42.98
#> 
#> Main Effects:
#> 1 main effect term
#> 
#> Uninterpreted Variation Ratio: 0.22636

# Setting main.effects = TRUE prints the contributions of each main effect
print(mid, main.effects = TRUE)
#> 
#> Call:
#> interpret(formula = dist ~ speed, data = cars)
#> 
#> Intercept: 42.98
#> 
#> Main Effects:
#> ---
#> $speed
#>        4        7        8       10       11       12       13       14 
#> -36.9800 -29.9800 -31.1050 -18.3550 -20.4800 -21.4800  -7.9800   7.5200 
#>       15       16       17       18       19       20       22       24 
#>  -9.6467  -6.9800  -2.3133  21.5200   7.0200   7.4200  13.1629  48.3057 
#>       25 
#>  42.0200 
#> 
#> Uninterpreted Variation Ratio: 0.22636
```
