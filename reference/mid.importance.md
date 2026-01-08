# Calculate MID Importance

`mid.importance()` calculates the MID importance of a fitted MID model.
This is a measure of feature importance that quantifies the average
contribution of each component function across a dataset.

## Usage

``` r
mid.importance(
  object,
  data = NULL,
  weights = NULL,
  sort = TRUE,
  measure = 1L,
  max.nkeeps = 10000
)
```

## Arguments

- object:

  a "mid" object.

- data:

  a data frame containing the observations to calculate the importance.
  If `NULL`, the `fitted.matrix` from the "mid" object is used.

- weights:

  an optional numeric vector of sample weights.

- sort:

  logical. If `TRUE`, the output data frame is sorted by importance in
  descending order.

- measure:

  an integer specifying the measure of importance. Possible alternatives
  are `1` for the mean absolute effect, `2` for the root mean square
  effect, and `3` for the median absolute effect.

- max.nkeeps:

  an integer specifying the maximum number of observations to retain in
  the `predictions` component of the returned object. If the number of
  observations exceeds this value, a weighted random sample is taken.
  This helps to reduce memory usage and improve plotting performance.

## Value

`mid.importance()` returns an object of class "mid.importance". This is
a list containing the following components:

- importance:

  a data frame with the calculated importance values, sorted by default.

- predictions:

  the matrix of the fitted or predicted MID values. If the number of
  observations exceeds `max.nkeeps`, this matrix contains a sampled
  subset.

- measure:

  a character string describing the type of the importance measure used.

## Details

The MID importance of a component function (e.g., a main effect or an
interaction) is defined as the mean absolute effect on the predictions
within the given data. Terms with higher importance have a larger
average impact on the model's overall predictions.

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`plot.mid.importance`](https://ryo-asashi.github.io/midr/reference/plot.mid.importance.md),
[`ggmid.mid.importance`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.importance.md)

## Examples

``` r
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#> 'model' not passed: response variable in 'data' is used

# Calculate MID importance using median absolute contribution
imp <- mid.importance(mid, data = airquality)
print(imp)
#> 
#> MID Importance based on 153 Observations
#> 
#> Measure: Mean Absolute Contribution
#> 
#> Importance:
#>             term importance order
#> 1           Temp   13.87962     1
#> 2           Wind   10.33981     1
#> 3        Solar.R    5.02973     1
#> 4            Day    4.89328     1
#> 5          Month    2.15851     1
#> 6   Solar.R:Wind    0.45096     2
#> 7       Temp:Day    0.40189     2
#> 8       Wind:Day    0.38731     2
#> 9  Solar.R:Month    0.38573     2
#> 10    Wind:Month    0.38048     2
#> 11     Month:Day    0.31373     2
#> 12     Wind:Temp    0.29742     2
#> 13   Solar.R:Day    0.28369     2
#> 14  Solar.R:Temp    0.21675     2
#> 15    Temp:Month    0.14530     2

# Calculate MID importance using root mean square contribution
imp <- mid.importance(mid, measure = 2)
#> 'data' and 'weights' are extracted from the 'object'
print(imp)
#> 
#> MID Importance based on 153 Observations
#> 
#> Measure: Root Mean Square Contribution
#> 
#> Importance:
#>             term importance order
#> 1           Temp   16.16385     1
#> 2           Wind   14.62577     1
#> 3        Solar.R    6.80267     1
#> 4            Day    6.17725     1
#> 5          Month    2.55141     1
#> 6     Wind:Month    0.59102     2
#> 7   Solar.R:Wind    0.58890     2
#> 8       Wind:Day    0.54694     2
#> 9       Temp:Day    0.53583     2
#> 10 Solar.R:Month    0.52604     2
#> 11     Month:Day    0.45375     2
#> 12     Wind:Temp    0.44336     2
#> 13   Solar.R:Day    0.35682     2
#> 14  Solar.R:Temp    0.28964     2
#> 15    Temp:Month    0.23401     2
```
