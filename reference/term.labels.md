# Extract and Filter Term Labels

`term.labels()` extracts term labels from a fitted model object or a
character vector. Its primary strength is the ability to filter terms
based on their order or their associated underlying variables.

## Usage

``` r
term.labels(object, ...)

# Default S3 method
term.labels(object, order = NULL, require = NULL, remove = NULL, ...)

# S3 method for class 'midlist'
term.labels(object, ...)
```

## Arguments

- object:

  an object containing model terms (such as a "lm", "glm", or "mid"
  object) or term labels directly, a "terms" object, or a character
  vector of term labels.

- ...:

  not used.

- order:

  an integer vector specifying the order of terms to retain (e.g., `1`
  for only main effects, `2` for only two-way interactions).

- require:

  a character vector of variable names. If provided, only terms
  containing at least one of these variables are returned.

- remove:

  a character vector of variable names. If provided, terms containing
  any of these variables are completely excluded.

## Value

A character vector of the selected term labels.

## Details

A "term" refers to an individual component in a formula, such as a main
effect (e.g., `"Wind"`) or an interaction effect (e.g., `"Wind:Temp"`).
This function safely parses the model's terms and provides a flexible
way to select a subset of them, which is especially useful for plotting,
summarizing, or other downstream analyses.

## Examples

``` r
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#> 'model' not passed: response variable in 'data' is used

# Get only main effect terms
term.labels(mid, order = 1)
#> [1] "Solar.R" "Wind"    "Temp"    "Month"   "Day"    

# Get terms related to "Wind" or "Temp"
term.labels(mid, require = c("Wind", "Temp"))
#> [1] "Wind"         "Temp"         "Solar.R:Wind" "Solar.R:Temp" "Wind:Temp"   
#> [6] "Wind:Month"   "Wind:Day"     "Temp:Month"   "Temp:Day"    

# Get terms related to "Wind" or "Temp", but exclude any with "Day"
term.labels(mid, require = c("Wind", "Temp"), remove = "Day")
#> [1] "Wind"         "Temp"         "Solar.R:Wind" "Solar.R:Temp" "Wind:Temp"   
#> [6] "Wind:Month"   "Temp:Month"  
```
