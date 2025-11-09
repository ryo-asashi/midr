# Weighted Loss Function

`weighted.loss()` computes various loss metrics (e.g., RMSE, MAE)
between two numeric vectors, or for the deviations from the weighted
mean of a numeric vector.

## Usage

``` r
weighted.loss(x, y = NULL, w = NULL, na.rm = FALSE, method = "rmse")
```

## Arguments

- x:

  a numeric vector.

- y:

  an optional numeric vector. If `NULL`, `x` is compared against its
  weighted mean.

- w:

  a numeric vector of sample weights for each value in `x`.

- na.rm:

  logical. If `TRUE`, any `NA` and `NaN`s are removed from all input
  vectors before the calculation.

- method:

  the loss measure. One of "mse" (mean square error), "rmse" (root mean
  square error), "mae" (mean absolute error), or "medae" (median
  absolute error).

## Value

`weighted.loss()` returns a single numeric value.

## Examples

``` r
# Calculate loss metrics between x and y with weights
weighted.loss(x = c(0, 10), y = c(0, 0), w = c(99, 1), method = "rmse")
#> [1] 1
weighted.loss(x = c(0, 10), y = c(0, 0), w = c(99, 1), method = "mae")
#> [1] 0.1
weighted.loss(x = c(0, 10), y = c(0, 0), w = c(99, 1), method = "medae")
#> [1] 0

# Verify uninterpreted variation ratio of a fitted MID model without weights
mid <- interpret(dist ~ speed, cars)
#> 'model' not passed: response variable in 'data' is used
RSS <- weighted.loss(cars$dist, predict(mid, cars), method = "mse")
TSS <- weighted.loss(cars$dist, method = "mse")
RSS / TSS
#> [1] 0.2263636
mid$ratio
#> [1] 0.2263636

# Verify uninterpreted variation ratio of a fitted MID model with weights
w <- 1:nrow(cars)
mid <- interpret(dist ~ speed, cars, weights = w)
#> 'model' not passed: response variable in 'data' is used
RSS <- weighted.loss(cars$dist, predict(mid, cars), w = w, method = "mse")
TSS <- weighted.loss(cars$dist, w = w, method = "mse")
RSS / TSS
#> [1] 0.2864814
mid$ratio
#> [1] 0.2864814
```
