# Extract Terms from MID Models

`mid.terms()` extracts term labels from a fitted MID model or derivative
objects. Its primary strength is the ability to filter terms based on
their type (main effects vs. interactions) or their associated variable
names.

## Usage

``` r
mid.terms(
  object,
  main.effects = TRUE,
  interactions = TRUE,
  require = NULL,
  remove = NULL,
  ...
)
```

## Arguments

- object:

  a "mid" object or another object that contains model terms. Can be a
  "mid.importance", "mid.conditional", or "mid.breakdown" object.

- main.effects:

  logical. If `FALSE`, the main effect terms are excluded.

- interactions:

  logical. If `FALSE`, the interactions terms are excluded.

- require:

  a character vector of variable names. Only terms related to at least
  one of these variables are returned.

- remove:

  a character vector of variable names. Terms related to any of these
  variables are excluded.

- ...:

  aliases are supported for convenience: "me" for `main.effects` and
  "ie" for `interactions`.

## Value

`mid.terms()` returns a character vector of the selected term labels.

## Details

A "term" in a MID model refers to either a main effect (e.g., "Wind") or
an interaction effect (e.g., "Wind:Temp"). This function provides a
flexible way to select a subset of these terms, which is useful for
plotting, summarizing, or other downstream analyses.

## Note

This function provides the common underlying logic for the
[`stats::terms()`](https://rdrr.io/r/stats/terms.html) S3 methods for
"mid", "mid.importance", "mid.conditional", and "mid.breakdown" objects.

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md)

## Examples

``` r
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#> 'model' not passed: response variable in 'data' is used

# Get only main effect terms
mid.terms(mid, interactions = FALSE)
#> [1] "Solar.R" "Wind"    "Temp"    "Month"   "Day"    

# Get terms related to "Wind" or "Temp"
mid.terms(mid, require = c("Wind", "Temp"))
#> [1] "Wind"         "Temp"         "Solar.R:Wind" "Solar.R:Temp" "Wind:Temp"   
#> [6] "Wind:Month"   "Wind:Day"     "Temp:Month"   "Temp:Day"    

# Get terms related to "Wind" or "Temp", but exclude any with "Day"
mid.terms(mid, require = c("Wind", "Temp"), remove = "Day")
#> [1] "Wind"         "Temp"         "Solar.R:Wind" "Solar.R:Temp" "Wind:Temp"   
#> [6] "Wind:Month"   "Temp:Month"  

# Get the predicted contributions of only the terms associated with "Wind"
terms_wind <- mid.terms(mid, require = "Wind")
predict(mid, airquality[1:3,], terms = terms_wind, type = "terms")
#>           Wind  Solar.R:Wind   Wind:Temp Wind:Month   Wind:Day
#> [1,]  1.022569 -0.0001017057 -0.62883340  2.0405310 0.04272772
#> [2,] -1.592553 -0.3294883535 -0.26746229  1.3541287 0.03556829
#> [3,] -9.264052  0.3213435867 -0.03032911 -0.4615641 0.50210370
#> attr(,"constant")
#> [1] 42.0991
```
