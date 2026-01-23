# Calculate MID Breakdowns

`mid.breakdown()` calculates the contribution of each component function
of a fitted MID model to a single prediction. It breaks down the total
prediction into the effects of the intercept, main effects, and
interactions.

## Usage

``` r
mid.breakdown(object, data = NULL, row = NULL, sort = TRUE)
```

## Arguments

- object:

  a "mid" object.

- data:

  a data frame containing one or more observations for which to
  calculate the MID breakdown. If not provided, data is automatically
  extracted based on the function call.

- row:

  an optional numeric value or character string specifying the row of
  `data` to be used for the breakdown. If `NULL`, and the `data`
  contains two or more observations, only the first observation is used.

- sort:

  logical. If `TRUE`, the output data frame is sorted by the absolute
  contribution of each effect.

## Value

`mid.breakdown()` returns an object of class "mid.breakdown". This is a
list with the following components:

- breakdown:

  a data frame containing the breakdown of the prediction.

- data:

  the data frame containing the predictor variable values used for the
  prediction.

- intercept:

  the intercept of the MID model.

- prediction:

  the predicted value from the MID model.

## Details

This function provides local interpretability for a specific observation
by decomposing its prediction into the individual contributions of the
MID components. For a target observation \\\mathbf{x}\\, the total
prediction is represented as the sum of all estimated terms:

\$\$g(\mathbf{x}) = g\_\emptyset + \sum\_{j} g_j(x_j) + \sum\_{j\<k}
g\_{jk}(x_j, x_k)\$\$

The output data frame itemizes the numerical value of each main effect
\\g_j(x_j)\\ and interaction effect \\g\_{jk}(x_j, x_k)\\, along with
the intercept \\g\_\emptyset\\. This decomposition makes the model's
decision for a single instance fully transparent and easy to attribute
to specific features or their combinations.

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`plot.mid.breakdown`](https://ryo-asashi.github.io/midr/reference/plot.mid.breakdown.md),
[`ggmid.mid.breakdown`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.breakdown.md)

## Examples

``` r
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#> 'model' not passed: response variable in 'data' is used

# Calculate the breakdown for the first observation in the data
mbd <- mid.breakdown(mid, data = airquality, row = 1)
print(mbd)
#> 
#> MID Breakdown of a Prediction
#> 
#> Intercept: 42.099
#> 
#> Prediction: 39.739
#> 
#> Breakdown of Effects:
#>             term         mid order
#> 1           Temp -1.5043e+01     1
#> 2            Day  4.9770e+00     1
#> 3          Month  3.7575e+00     1
#> 4     Wind:Month  2.0404e+00     2
#> 5       Temp:Day  1.3885e+00     2
#> 6  Solar.R:Month  1.3124e+00     2
#> 7           Wind  1.0227e+00     1
#> 8        Solar.R -9.4228e-01     1
#> 9      Wind:Temp -6.2914e-01     2
#> 10  Solar.R:Temp -5.6533e-01     2
#> 11     Month:Day  5.0500e-01     2
#> 12   Solar.R:Day -2.4755e-01     2
#> 13      Wind:Day  4.2771e-02     2
#> 14    Temp:Month  2.0660e-02     2
#> 15  Solar.R:Wind -3.4803e-04     2

# Calculate the breakdown for the third observation in the data
mbd <- mid.breakdown(mid, data = airquality, row = 3)
print(mbd)
#> 
#> MID Breakdown of a Prediction
#> 
#> Intercept: 42.099
#> 
#> Prediction: 9.9693
#> 
#> Breakdown of Effects:
#>             term        mid order
#> 1           Temp -16.686548     1
#> 2            Day -11.078821     1
#> 3           Wind  -9.263680     1
#> 4          Month   3.757459     1
#> 5  Solar.R:Month   0.702906     2
#> 6       Wind:Day   0.501892     2
#> 7     Wind:Month  -0.461498     2
#> 8   Solar.R:Wind   0.321963     2
#> 9      Month:Day   0.180867     2
#> 10      Temp:Day   0.120489     2
#> 11   Solar.R:Day  -0.069100     2
#> 12    Temp:Month  -0.068488     2
#> 13       Solar.R  -0.034390     1
#> 14     Wind:Temp  -0.030288     2
#> 15  Solar.R:Temp  -0.022524     2
```
