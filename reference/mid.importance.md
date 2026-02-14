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
  max.nsamples = 10000L
)
```

## Arguments

- object:

  a "mid" object.

- data:

  a data frame containing the observations to calculate the importance.
  If not provided, data is automatically extracted based on the function
  call.

- weights:

  an optional numeric vector of sample weights.

- sort:

  logical. If `TRUE`, the output data frame is sorted by importance in
  descending order.

- measure:

  an integer specifying the measure of importance. Possible alternatives
  are `1` for the mean absolute effect, `2` for the root mean square
  effect, and `3` for the median absolute effect.

- max.nsamples:

  an integer specifying the maximum number of samples to retain in the
  `predictions` component of the returned object. If the number of
  observations exceeds this value, a weighted random sample is taken.

## Value

`mid.importance()` returns an object of class "mid.importance". This is
a list containing the following components:

- importance:

  a data frame with the calculated importance values, sorted by default.

- predictions:

  the matrix of the fitted or predicted MID values. If the number of
  observations exceeds `max.nsamples`, this matrix contains a sampled
  subset.

- measure:

  a character string describing the type of the importance measure used.

For "midlist", `mid.importance()` returns an object of class
"midlist.importance", a list of "mid.importance" objects.

## Details

The MID importance of a component function \\g_S\\, where \\S\\
represents a single feature \\\\j\\\\ or a feature pair \\\\j, k\\\\, is
defined as the mean absolute effect on the predictions within the given
data:

\$\$\mathbf{I}(g_S) = \frac{1}{n} \sum\_{i=1}^n
\|g_S(\mathbf{x}\_{i,S})\|\$\$

Terms with higher importance values have a larger average impact on the
model's overall predictions. Because all components (main effects and
interactions) are measured on the same scale as the response variable,
these values provide a direct and comparable measure of each term's
contribution to the model.

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
#> 1           Temp   13.87964     1
#> 2           Wind   10.33988     1
#> 3        Solar.R    5.02987     1
#> 4            Day    4.89338     1
#> 5          Month    2.15840     1
#> 6   Solar.R:Wind    0.45055     2
#> 7       Temp:Day    0.40191     2
#> 8       Wind:Day    0.38748     2
#> 9  Solar.R:Month    0.38573     2
#> 10    Wind:Month    0.38059     2
#> 11     Month:Day    0.31367     2
#> 12     Wind:Temp    0.29737     2
#> 13   Solar.R:Day    0.28360     2
#> 14  Solar.R:Temp    0.21667     2
#> 15    Temp:Month    0.14529     2

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
#> 1           Temp   16.23318     1
#> 2           Wind   14.77135     1
#> 3        Solar.R    7.25789     1
#> 4            Day    5.69615     1
#> 5          Month    2.66095     1
#> 6     Wind:Month    0.56416     2
#> 7   Solar.R:Wind    0.55387     2
#> 8  Solar.R:Month    0.51838     2
#> 9       Wind:Day    0.51729     2
#> 10      Temp:Day    0.48825     2
#> 11     Month:Day    0.47409     2
#> 12     Wind:Temp    0.41887     2
#> 13   Solar.R:Day    0.32822     2
#> 14  Solar.R:Temp    0.30586     2
#> 15    Temp:Month    0.20071     2
```
