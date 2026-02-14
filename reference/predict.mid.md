# Predict Method for fitted MID Models

`predict.mid()` is an S3 method for "mid" objects that obtains
predictions from a fitted MID model. It can be used to predict on new
data or to retrieve the fitted values from the original data.

## Usage

``` r
# S3 method for class 'mid'
predict(
  object,
  newdata = NULL,
  na.action = "na.pass",
  type = c("response", "link", "terms"),
  terms = mid.terms(object),
  ...
)

# S3 method for class 'midlist'
predict(object, ...)
```

## Arguments

- object:

  a "mid" object to be used to make predictions.

- newdata:

  a data frame of the new observations. If `NULL`, the original fitted
  values are extracted and returned.

- na.action:

  a function or character string specifying what should happen when the
  data contain `NA` values.

- type:

  the type of prediction required. One of "response", "link", or
  "terms".

- terms:

  a character vector of term labels, specifying a subset of component
  functions to use for predictions.

- ...:

  arguments to be passed to other methods (not used in this method).

## Value

`predict.mid()` returns a numeric vector of MID model predictions, or a
matrix if `type = "terms"`.

`predict.midlist()` returns a matrix of predictions, or a list of matrix
if `type = "terms"`.

## Details

The `type` argument allows you to specify the scale of the prediction.
By default (`type = "response"`), the function returns predictions on
the original scale of the response variable. Alternatively, you can
obtain predictions on the scale of the linear predictor by setting
`type = "link"`. For a detailed breakdown, setting `type = "terms"`
returns a matrix where each column represents the contribution of a
specific model term on the linear predictor scale.

The `terms` argument allows for predictions based on a subset of the
model's component functions, excluding others.

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`mid.effect`](https://ryo-asashi.github.io/midr/reference/mid.effect.md),
[`get.yhat`](https://ryo-asashi.github.io/midr/reference/get.yhat.md)

## Examples

``` r
data(airquality, package = "datasets")
test <- 1:10
mid <- interpret(Ozone ~ .^2, airquality[-test, ], lambda = 1, link = "log")
#> 'model' not passed: response variable in 'data' is used

# Predict on new data
predict(mid, airquality[test, ])
#>  [1] 20.567319 13.072444 15.205206 11.577466  7.076900 12.749324 13.802332
#>  [8]  9.219133  6.915020 18.056718

# Get predictions on the link scale
predict(mid, airquality[test, ], type = "link")
#>  [1] 3.023703 2.570507 2.721638 2.449061 1.956836 2.545478 2.624838 2.221281
#>  [9] 1.933696 2.893518

# Get the contributions of specific terms
predict(mid, airquality[test, ], terms = c("Temp", "Wind"), type = "terms")
#>             Temp        Wind
#>  [1,] -0.4407593  0.03339958
#>  [2,] -0.4721933 -0.07545538
#>  [3,] -0.4522206 -0.08764899
#>  [4,] -0.7351731 -0.11169699
#>  [5,] -1.0414042 -0.30023272
#>  [6,] -0.4872913 -0.25282426
#>  [7,] -0.5338234 -0.10880959
#>  [8,] -0.9269931 -0.16332033
#>  [9,] -0.8125819 -0.08896022
#> [10,] -0.4332497 -0.10880959
#> attr(,"constant")
#> [1] 3.44554
```
