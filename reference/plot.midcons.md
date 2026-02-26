# Compare MID Conditional Expectations

For "midlist.conditional" objects,
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) visualizes
Individual Conditional Expectation (ICE) curves derived from a fitted
MID model.

## Usage

``` r
# S3 method for class 'midcons'
plot(x, ...)
```

## Arguments

- x:

  a "mid.conditional" object to be visualized.

- ...:

  optional parameters passed on to the graphing functions.

## Value

`plot.midlist.conditional()` produces an ICE plot as a side-effect and
invisibly returns the ICE matrix used for the plot.

## Details

This is an S3 method for the
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) generic that
produces ICE curves from a "mid.conditional" object. ICE plots are a
model-agnostic tool for visualizing how a model's prediction for a
single observation changes as one feature varies. This function plots
one line for each observation in the data.

The `type` argument controls the visualization style: The default,
`type = "iceplot"`, plots the row ICE curves. The `type = "centered"`
option creates the centered ICE (c-ICE) plot, where each curve is
shifted so start at zero, which makes it easier to compare the slopes of
the curves.

The `var.color`, `var.alpha`, etc., arguments allow you to map
aesthetics to other variables in your data using (possibly) unquoted
expressions.

## See also

[`plot.midcon`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md),
[`ggmid.midcons`](https://ryo-asashi.github.io/midr/reference/ggmid.midcons.md)
