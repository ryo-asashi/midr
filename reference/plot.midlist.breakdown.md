# Compare MID Breakdowns in a Collection

For "mid.breakdown" objects,
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) visualizes the
breakdown of a prediction by component functions.

## Usage

``` r
# S3 method for class 'midlist.breakdown'
plot(x, ...)
```

## Arguments

- x:

  a "mid.breakdown" object to be visualized.

- ...:

  optional parameters passed on to the graphing function. Possible
  arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of
  them.

## Value

`plot.midlist.breakdown()` produces a plot as a side effect and returns
`NULL` invisibly.

## Details

This is an S3 method for the
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) generic that
produces a breakdown plot from a "mid.breakdown" object, visualizing the
contribution of each component function to a single prediction.

The `type` argument controls the visualization style. The default,
`type = "waterfall"`, creates a waterfall plot that shows how the
prediction builds from the intercept, with each term's contribution
sequentially added or subtracted. The `type = "barplot"` option creates
a standard bar plot where the length of each bar represents the
magnitude of the term's contribution. The `type = "dotchart"` option
creates a dot plot showing the contribution of each term as a point
connected to a zero baseline.

## See also

[`plot.mid.breakdown`](https://ryo-asashi.github.io/midr/reference/plot.mid.breakdown.md),
[`ggmid.midlist.breakdown`](https://ryo-asashi.github.io/midr/reference/ggmid.midlist.breakdown.md)
