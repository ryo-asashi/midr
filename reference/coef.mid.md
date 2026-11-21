# Extract Component Effects from MID Models

[`coef()`](https://rdrr.io/r/stats/coef.html) methods for extracting the
fitted intercept and component effect tables from a `"mid"` (or
`"midrib"`) object.

## Usage

``` r
# S3 method for class 'mid'
coef(object, terms = NULL, ...)
```

## Arguments

- object:

  a fitted `"mid"` (or `"midrib"`) object.

- terms:

  an optional character vector, formula, or
  [`terms`](https://rdrr.io/r/stats/terms.html) object specifying a
  subset of terms (and intercept) to extract. Defaults to all terms in
  `object`.

- ...:

  further arguments passed to or from other methods.

## Value

A named list containing the intercept (if included in `terms`) and data
frames of component effects (excluding `.density`) for each matched
term.

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`predict.mid`](https://ryo-asashi.github.io/midr/reference/predict.mid.md)

## Examples

``` r
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ Wind * Temp, airquality, lambda = .5)
#> 'model' not passed: response variable in 'data' is used
coef(mid, terms = c("Wind", "Wind:Temp"))
#> $Wind
#>    Wind     .effect
#> 1   2.3  54.8061580
#> 2   4.1  43.8696926
#> 3   5.1  24.7252085
#> 4   6.3  13.0155505
#> 5   6.9   4.2885010
#> 6   7.4   0.8426415
#> 7   8.0   0.5006385
#> 8   9.2  -3.0150116
#> 9   9.7  -5.6740564
#> 10 10.3  -8.2779117
#> 11 10.9 -10.3711218
#> 12 11.5  -9.3698246
#> 13 12.0  -7.9537189
#> 14 13.8 -11.0298727
#> 15 14.3 -13.3050200
#> 16 14.9 -10.1164455
#> 17 16.6  -8.2956443
#> 18 20.7  -5.2223098
#> 
#> $`Wind:Temp`
#>    Wind Temp    .effect
#> 1   2.3   57 -5.1018298
#> 2   7.4   57 -4.2106799
#> 3   9.7   57 -2.5565704
#> 4  11.5   57  1.3129828
#> 5  20.7   57  1.7273615
#> 6   2.3   71  1.6745738
#> 7   7.4   71 -1.2645034
#> 8   9.7   71 -1.0104616
#> 9  11.5   71  0.6249259
#> 10 20.7   71  1.6762946
#> 11  2.3   79  6.7924023
#> 12  7.4   79  0.3564428
#> 13  9.7   79 -0.1583654
#> 14 11.5   79 -1.0945492
#> 15 20.7   79 -1.7968434
#> 16  2.3   85  0.4823492
#> 17  7.4   85  0.1203234
#> 18  9.7   85  1.7335484
#> 19 11.5   85 -1.0637905
#> 20 20.7   85 -4.5317609
#> 21  2.3   97 -4.6968701
#> 22  7.4   97  0.9694012
#> 23  9.7   97  6.5186886
#> 24 11.5   97  1.6150087
#> 25 20.7   97 -4.3627184
#> 
```
