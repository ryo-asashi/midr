# Evaluate Conditional Expectations

`eval_conditional()` calculates Individual Conditional Expectation (ICE)
curves from a fitted model for a single predictor variable.

## Usage

``` r
eval_conditional(object, ...)

# S3 method for class 'mid'
eval_conditional(
  object,
  variable,
  data = NULL,
  resolution = 100L,
  max.nsamples = 500L,
  seed = NULL,
  type = c("response", "link"),
  ...
)

# S3 method for class 'mids'
eval_conditional(
  object,
  variable,
  data = NULL,
  resolution = 100L,
  max.nsamples = 500L,
  seed = NULL,
  type = c("response", "link"),
  ...
)
```

## Arguments

- object:

  a `"mid"` or `"mids"` object.

- ...:

  additional arguments passed to methods.

- variable:

  character string naming the predictor variable to vary.

- data:

  a data.frame containing observations for ICE evaluation. Extracted
  automatically from `object` when omitted.

- resolution:

  number of evaluation grid points for continuous variables.

- max.nsamples:

  maximum number of observations to evaluate. Larger datasets are
  randomly subsampled.

- seed:

  optional integer seed for subsampling.

- type:

  character: `"response"` (default) or `"link"`.

## Value

For a `"mid"` object, an object of class `"midcon"` containing
`observed`, `conditional`, `variable`, `values`, and `ids`. For a
`"mids"` collection, a collection of class `c("midcons", "midlist")`.

## Details

For each observation \\\mathbf{x}\_i\\, the ICE curve evaluates the
model across a grid of values \\x'\\ for the target `variable` while
holding all other predictors \\\mathbf{x}\_{i,\setminus j}\\ fixed:

\$\$f\_{\text{ICE}}(x') = g(x', \mathbf{x}\_{i,\setminus j})\$\$

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`plot.midcon`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md),
[`ggmid.midcon`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)

## Examples

``` r
data(airquality, package = "datasets")
aq <- na.omit(airquality)
mid <- interpret(Ozone ~ (Solar.R + Wind + Temp)^2, data = aq, lambda = 0.5)
#> 'model' not passed: response variable in 'data' is used

eval_conditional(mid, variable = "Wind", data = aq, max.nsamples = 30L)
#> number of observations exceeds 'max.nsamples': a sample of 30 observations from 'data' is used
#> 
#> MID Conditional Expectation for 'Wind' (30 Observations)
#> 
#> Grid Values: 2.3000, 2.4859, 2.6717, ...
#> 
#> Observed Head:
#>   .id    yhat Ozone Solar.R Wind Temp
#> 1  76  20.903     7      48 14.3   80
#> 2  30  83.775   115     223  5.7   79
#> 3 116  49.986    45     212  9.7   79
#> 4  99 124.851   122     255  4.0   89
#> 5 142  25.830    24     238 10.3   68
#> 6 143  47.160    16     201  8.0   82
```
