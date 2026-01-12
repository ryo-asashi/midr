# Calculate MID Breakdowns

`mid.breakdown()` calculates the contribution of each component function
of a fitted MID model to a single prediction. It breaks down the total
prediction into the effects of the intercept, main effects, and
interactions.

## Usage

``` r
mid.breakdown(object, data = NULL, row = NULL, sort = TRUE, format = list())
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

- format:

  an optional named list/vector of character strings. The names should
  correspond to term names in the model, and the values are format
  strings passed to [`sprintf()`](https://rdrr.io/r/base/sprintf.html)
  to format the feature values (e.g., `x1 = "%.2f"`). If a term is not
  specified, `"%s"` is used by default for main effects and `"%s, %s"`
  for interactions.

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

`mid.breakdown()` is a method for local interpretability. For a given
observation, it provides a clear answer to the question, "How much did
each component of the MID model contribute to the final prediction?"

The function calculates the value of each term in the MID model's
additive structure for the specified observation. The total prediction
is the sum of these individual contributions. The prediction, denoted
\\\mathcal{F}(\mathbf{x})\\, is decomposed as:
\$\$\mathcal{F}(\mathbf{x}) = f\_\phi + \sum\_{j} f\_{j}(x_j) +
\sum\_{j\<k} f\_{jk}(x_j, x_k)\$\$

The output data frame itemizes the numerical value of each main effect
(\\f\_{j}(x_j)\\) and interaction effect (\\f\_{jk}(x_j, x_k)\\), along
with the intercept (\\f\_\phi\\). This makes the prediction transparent
and easy to understand.

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
#>             term    value         mid order
#> 1           Temp       67 -1.5043e+01     1
#> 2            Day        1  4.9770e+00     1
#> 3          Month        5  3.7575e+00     1
#> 4     Wind:Month   7.4, 5  2.0404e+00     2
#> 5       Temp:Day    67, 1  1.3885e+00     2
#> 6  Solar.R:Month   190, 5  1.3124e+00     2
#> 7           Wind      7.4  1.0227e+00     1
#> 8        Solar.R      190 -9.4228e-01     1
#> 9      Wind:Temp  7.4, 67 -6.2914e-01     2
#> 10  Solar.R:Temp  190, 67 -5.6533e-01     2
#> 11     Month:Day     5, 1  5.0500e-01     2
#> 12   Solar.R:Day   190, 1 -2.4755e-01     2
#> 13      Wind:Day   7.4, 1  4.2771e-02     2
#> 14    Temp:Month    67, 5  2.0660e-02     2
#> 15  Solar.R:Wind 190, 7.4 -3.4803e-04     2

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
#>             term     value        mid order
#> 1           Temp        74 -16.686548     1
#> 2            Day         3 -11.078821     1
#> 3           Wind      12.6  -9.263680     1
#> 4          Month         5   3.757459     1
#> 5  Solar.R:Month    149, 5   0.702906     2
#> 6       Wind:Day   12.6, 3   0.501892     2
#> 7     Wind:Month   12.6, 5  -0.461498     2
#> 8   Solar.R:Wind 149, 12.6   0.321963     2
#> 9      Month:Day      5, 3   0.180867     2
#> 10      Temp:Day     74, 3   0.120489     2
#> 11   Solar.R:Day    149, 3  -0.069100     2
#> 12    Temp:Month     74, 5  -0.068488     2
#> 13       Solar.R       149  -0.034390     1
#> 14     Wind:Temp  12.6, 74  -0.030288     2
#> 15  Solar.R:Temp   149, 74  -0.022524     2
```
