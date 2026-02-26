# Compare MID Component Functions across Multiple Models with ggplot2

[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md) is an
S3 generic function for creating various visualizations from MID-related
objects using **ggplot2**. For "mid" objects (i.e., fitted MID models),
it visualizes a single component function specified by the `term`
argument.

## Usage

``` r
# S3 method for class 'midlist'
ggmid(object, ...)

# S3 method for class 'midlist'
autoplot(object, ...)
```

## Arguments

- object:

  a "mid" object to be visualized.

- ...:

  optional parameters.

## Value

`ggmid.midlist()` returns a "ggplot" object.

## Details

For "mid" objects,
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
creates a "ggplot" object that visualizes a component function of the
fitted MID model.

The `type` argument controls the visualization style. The default,
`type = "effect"`, plots the component function itself. In this style,
the plotting method is automatically selected based on the effect's
type: a line plot for quantitative main effects; a bar plot for
qualitative main effects; and a raster plot for interactions. The
`type = "data"` option creates a scatter plot of `data`, colored by the
values of the component function. The `type = "compound"` option
combines both approaches, plotting the component function alongside the
data points.

## See also

[`ggmid`](https://ryo-asashi.github.io/midr/reference/ggmid.md),
[`plot.midlist`](https://ryo-asashi.github.io/midr/reference/plot.midlist.md)
