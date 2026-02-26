# Compare MID Breakdowns with ggplot2

For "mid.breakdown" objects,
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
visualizes the breakdown of a prediction by component functions.

## Usage

``` r
# S3 method for class 'midlist.breakdown'
ggmid(object, ...)

# S3 method for class 'midlist.breakdown'
autoplot(object, ...)
```

## Arguments

- object:

  a "mid.breakdown" object to be visualized.

- ...:

  optional parameters passed on to the main layer.

## Value

`ggmid.midlist.breakdown()` returns a "ggplot" object.

## Details

This is an S3 method for the
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
generic that creates a breakdown plot from a "mid.breakdown" object,
visualizing the contribution of each component function to a single
prediction.

The `type` argument controls the visualization style. The default,
`type = "waterfall"` (default), creates a waterfall plot that shows how
the prediction is built up from the intercept, with each term's
contribution sequentially added or subtracted. The `type = "barplot"`
option creates a standard bar plot where the length of each bar
represents the magnitude of the term's contribution. The
`type = "dotchart"` option creates a dot plot showing the contribution
of each term as a point connected to a zero baseline.

## See also

[`ggmid.mid.breakdown`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.breakdown.md),
[`plot.mid.breakdown`](https://ryo-asashi.github.io/midr/reference/plot.mid.breakdown.md)
