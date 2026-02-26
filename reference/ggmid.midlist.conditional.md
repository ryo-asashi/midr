# Compare MID Conditional Expectations across Multiple Models with ggplot2

For "mid.conditional" objects,
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
visualizes Individual Conditional Expectation (ICE) curves derived from
a fitted MID model.

## Usage

``` r
# S3 method for class 'midlist.conditional'
ggmid(object, ...)

# S3 method for class 'midlist.conditional'
autoplot(object, ...)
```

## Arguments

- object:

  a "mid.conditional" object to be visualized.

- ...:

  optional parameters passed on to the main layer.

## Value

`ggmid.midlist.conditional()` returns a "ggplot" object.

## Details

This is an S3 method for the
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
generic that produces ICE curves from a "mid.conditional" object. ICE
plots are a model-agnostic tool for visualizing how a model's prediction
for a single observation changes as one feature varies. This function
plots one line for each observation in the data.

The `type` argument controls the visualization style: The default,
`type = "iceplot"`, plots the raw ICE curves. The `type = "centered"`
option creates the centered ICE (c-ICE) plot, where each curve is
shifted to start at zero, making it easier to compare the slopes of the
curves.

The `var.color`, `var.alpha`, etc., arguments allow you to map
aesthetics to other variables in your data using (possibly) unquoted
expressions.

## See also

[`ggmid.mid.conditional`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.conditional.md),
[`plot.midlist.conditional`](https://ryo-asashi.github.io/midr/reference/plot.midlist.conditional.md)
