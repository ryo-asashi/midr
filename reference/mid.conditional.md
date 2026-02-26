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
  seed = NULL,
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

- seed:

  an integer seed for random sampling. Default is `NULL`.

- type:

  the type of prediction to return. "response" (default) for the
  original scale or "link" for the scale of the linear predictor.

- keep.effects:

  logical. If `TRUE`, the effects of individual component functions are
  stored in the output object.

## Value

`mid.conditional()` returns an object of class "midcon". This is a list
with the following components:

- observed:

  a data frame of the original observations used, along with their
  predictions.

- conditional:

  a data frame of the hypothetical observations and their corresponding
  predictions.

- variable:

  name of the target variable.

- values:

  a vector of the sample points for the `variable` used in the ICE
  calculation.

For "midlist", `mid.conditional()` returns an object of class
"midcon"-"midlist", a list of "midcon" objects.

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
[`plot.midcon`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md),
[`ggmid.midcon`](https://ryo-asashi.github.io/midr/reference/ggmid.midcon.md)

## Examples

``` r
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#> 'model' not passed: response variable in 'data' is used

# Calculate the ICE values for a fitted MID model
con <- mid.conditional(mid, variable = "Wind", data = airquality)
print(con)
#> 
#> Individual Conditional Expectation for 153 Observations
#> 
#> Variable: Wind
#> 
#> Sample Points:  2.3000, 2.4859, 2.6717, ... 
#> 
#> Conditional Expectations:
#>    .id Wind   yhat
#> 1    1  2.3 93.187
#> 2    2  2.3 80.733
#> 3    3  2.3 75.251
#> 4    4  2.3 80.683
#> 5    5  2.3 77.034
#> 6    6  2.3 80.108
#> 7    7  2.3 84.101
#> 8    8  2.3 76.878
#> 9    9  2.3 72.084
#> 10  10  2.3 86.434
#> 11  11  2.3 83.927
#> 12  12  2.3 90.309
#> 13  13  2.3 83.397
#> 14  14  2.3 87.020
#> 15  15  2.3 70.543
#> 16  16  2.3 88.585
#> 17  17  2.3 92.022
#> 18  18  2.3 72.778
#> 19  19  2.3 89.513
#> 20  20  2.3 70.525
```
