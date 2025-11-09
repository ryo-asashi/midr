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
#>  [1] 20.567677 13.073024 15.206606 11.576707  7.077082 12.750558 13.801620
#>  [8]  9.220003  6.914937 18.057558

# Get predictions on the link scale
predict(mid, airquality[test, ], type = "link")
#>  [1] 3.023721 2.570551 2.721730 2.448995 1.956862 2.545575 2.624786 2.221375
#>  [9] 1.933684 2.893564

# Get the contributions of specific terms
predict(mid, airquality[test, ], terms = c("Temp", "Wind"), type = "terms")
#>             Temp        Wind
#>  [1,] -0.4407995  0.03339056
#>  [2,] -0.4721648 -0.07546887
#>  [3,] -0.4521979 -0.08759411
#>  [4,] -0.7352680 -0.11168528
#>  [5,] -1.0414244 -0.30022613
#>  [6,] -0.4872791 -0.25282628
#>  [7,] -0.5338984 -0.10882497
#>  [8,] -0.9270107 -0.16329480
#>  [9,] -0.8125969 -0.08899763
#> [10,] -0.4332501 -0.10882497
#> attr(,"constant")
#> [1] 3.44554
```
