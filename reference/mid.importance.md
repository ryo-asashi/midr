# Calculate MID Importance

`mid.importance()` calculates the MID importance of a fitted MID model.
This is a measure of feature importance that quantifies the average
contribution of each component function across a dataset.

## Usage

``` r
mid.importance(object, data = NULL, weights = NULL, sort = TRUE, measure = 1L)
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

## Value

`mid.importance()` returns an object of class "mid.importance". This is
a list containing the following components:

- importance:

  a data frame with the calculated importance values, sorted by default.

- predictions:

  the matrix of the fitted or predicted MID values.

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
imp <- mid.importance(mid)
print(imp)
#> 
#> MID Importance based on 111 Observations
#> 
#> Measure: Mean Absolute Contribution
#> 
#> Importance:
#>             term importance order
#> 1           Temp   13.94096     1
#> 2           Wind   10.50540     1
#> 3        Solar.R    5.57458     1
#> 4            Day    4.39277     1
#> 5          Month    2.27995     1
#> 6   Solar.R:Wind    0.44950     2
#> 7  Solar.R:Month    0.37623     2
#> 8       Temp:Day    0.37148     2
#> 9     Wind:Month    0.36905     2
#> 10      Wind:Day    0.36547     2
#> 11     Month:Day    0.32260     2
#> 12     Wind:Temp    0.28060     2
#> 13   Solar.R:Day    0.26646     2
#> 14  Solar.R:Temp    0.23464     2
#> 15    Temp:Month    0.12076     2

# Calculate MID importance using root mean square contribution
imp <- mid.importance(mid, measure = 2)
print(imp)
#> 
#> MID Importance based on 111 Observations
#> 
#> Measure: Root Mean Square Contribution
#> 
#> Importance:
#>             term importance order
#> 1           Temp   16.23314     1
#> 2           Wind   14.77143     1
#> 3        Solar.R    7.25784     1
#> 4            Day    5.69605     1
#> 5          Month    2.66107     1
#> 6     Wind:Month    0.56396     2
#> 7   Solar.R:Wind    0.55439     2
#> 8  Solar.R:Month    0.51836     2
#> 9       Wind:Day    0.51705     2
#> 10      Temp:Day    0.48822     2
#> 11     Month:Day    0.47417     2
#> 12     Wind:Temp    0.41892     2
#> 13   Solar.R:Day    0.32831     2
#> 14  Solar.R:Temp    0.30592     2
#> 15    Temp:Month    0.20070     2
```
