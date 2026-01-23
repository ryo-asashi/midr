# Calculate MID Conditional Expectations

`mid.conditional()` calculates the data required to draw Individual
Conditional Expectation (ICE) curves from a fitted MID model. ICE curves
visualize how a single observation's prediction changes as a specified
variable's value varies, while all other variable are held constant.

## Usage

``` r
mid.conditional(
  object,
  variable,
  data = NULL,
  resolution = 100L,
  max.nsamples = 1000L,
  type = c("response", "link"),
  keep.effects = TRUE
)
```

## Arguments

- object:

  a "mid" object.

- variable:

  a character string or expression specifying the single predictor
  variable for which to calculate ICE curves.

- data:

  a data frame containing the observations to be used for the ICE
  calculations. If not provided, data is automatically extracted based
  on the function call.

- resolution:

  an integer specifying the number of evaluation points for the
  `variable`'s range.

- max.nsamples:

  an integer specifying the maximum number of samples. If the number of
  observations exceeds this limit, the `data` is randomly sampled.

- type:

  the type of prediction to return. "response" (default) for the
  original scale or "link" for the scale of the linear predictor.

- keep.effects:

  logical. If `TRUE`, the effects of individual component functions are
  stored in the output object.

## Value

`mid.conditional()` returns an object of class "mid.conditional". This
is a list with the following components:

- observed:

  a data frame of the original observations used, along with their
  predictions.

- conditional:

  a data frame of the hypothetical observations and their corresponding
  predictions.

- values:

  a vector of the sample points for the `variable` used in the ICE
  calculation

## Details

This function generates Individual Conditional Expectation (ICE) data by
evaluating the MID model over a range of values for a specific variable.
For a given observation \\\mathbf{x}\_i\\, the ICE value at \\X_j = x'\\
is computed by replacing the value \\x\_{i,j}\\ with \\x'\\ while
keeping all other features \\\mathbf{x}\_{i,\setminus j}\\ fixed:

\$\$f\_{\text{ICE}}(x') = g(x', \mathbf{x}\_{i,\setminus j})\$\$

The function creates a set of hypothetical observations across a grid of
evaluation points for the specified `variable`. The resulting object can
be plotted to visualize how the prediction changes for individuals as a
specific feature varies, revealing both global trends and local
departures (heterogeneity).

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`plot.mid.conditional`](https://ryo-asashi.github.io/midr/reference/plot.mid.conditional.md),
[`ggmid.mid.conditional`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.conditional.md)

## Examples

``` r
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#> 'model' not passed: response variable in 'data' is used

# Calculate the ICE values for a fitted MID model
ice <- mid.conditional(mid, variable = "Wind", data = airquality)
print(ice)
#> 
#> Individual Conditional Expectation for 153 Observations
#> 
#> Variable: Wind
#> 
#> Sample Points:
#>   [1]  2.3000  2.4859  2.6717  2.8576  3.0434  3.2293  3.4152  3.6010  3.7869
#>  [10]  3.9727  4.1586  4.3444  4.5303  4.7162  4.9020  5.0879  5.2737  5.4596
#>  [19]  5.6455  5.8313  6.0172  6.2030  6.3889  6.5747  6.7606  6.9465  7.1323
#>  [28]  7.3182  7.5040  7.6899  7.8758  8.0616  8.2475  8.4333  8.6192  8.8051
#>  [37]  8.9909  9.1768  9.3626  9.5485  9.7343  9.9202 10.1061 10.2919 10.4778
#>  [46] 10.6636 10.8495 11.0354 11.2212 11.4071 11.5929 11.7788 11.9646 12.1505
#>  [55] 12.3364 12.5222 12.7081 12.8939 13.0798 13.2657 13.4515 13.6374 13.8232
#>  [64] 14.0091 14.1949 14.3808 14.5667 14.7525 14.9384 15.1242 15.3101 15.4960
#>  [73] 15.6818 15.8677 16.0535 16.2394 16.4253 16.6111 16.7970 16.9828 17.1687
#>  [82] 17.3545 17.5404 17.7263 17.9121 18.0980 18.2838 18.4697 18.6556 18.8414
#>  [91] 19.0273 19.2131 19.3990 19.5848 19.7707 19.9566 20.1424 20.3283 20.5141
#> [100] 20.7000
```
