# Plot MID Breakdowns with ggplot2

For "mid.breakdown" objects,
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
visualizes the breakdown of a prediction by component functions.

## Usage

``` r
# S3 method for class 'mid.breakdown'
ggmid(
  object,
  type = c("waterfall", "barplot", "dotchart"),
  theme = NULL,
  terms = NULL,
  max.nterms = 15L,
  width = NULL,
  vline = TRUE,
  catchall = "(others)",
  label.format = c("%t=%v", "%t:%t"),
  format.args = list(),
  ...
)

# S3 method for class 'mid.breakdown'
autoplot(object, ...)
```

## Arguments

- object:

  a "mid.breakdown" object to be visualized.

- type:

  the plotting style. One of "waterfall", "barplot" or "dotchart".

- theme:

  a character string or object defining the color theme. See
  [`color.theme`](https://ryo-asashi.github.io/midr/reference/color.theme.md)
  for details.

- terms:

  an optional character vector specifying which terms to display.

- max.nterms:

  the maximum number of terms to display in the plot. Less important
  terms will be grouped into a "catchall" category.

- width:

  a numeric value specifying the width of the bars.

- vline:

  logical. If `TRUE`, a vertical line is drawn at the zero or intercept
  line.

- catchall:

  a character string for the catchall label.

- label.format:

  a character vector of length one or two specifying the format of the
  axis labels. The first element is used for main effects (default
  `"%t = %v"`), and the second is for interactions (default `"%t:%t"`).
  Use `"%t"` for the term name and `"%v"` for its value.

- format.args:

  a named list of additional arguments passed to
  [`format`](https://rdrr.io/r/base/format.html) for formatting the
  values. Common arguments include `digits`, `nsmall`, and `big.mark`.

- ...:

  optional parameters passed on to the main layer.

## Value

`ggmid.mid.breakdown()` returns a "ggplot" object.

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

[`mid.breakdown`](https://ryo-asashi.github.io/midr/reference/mid.breakdown.md),
[`ggmid`](https://ryo-asashi.github.io/midr/reference/ggmid.md),
[`plot.mid.breakdown`](https://ryo-asashi.github.io/midr/reference/plot.mid.breakdown.md)

## Examples

``` r
data(diamonds, package = "ggplot2")
set.seed(42)
idx <- sample(nrow(diamonds), 1e4)
mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#> 'model' not passed: response variable in 'data' is used
mbd <- mid.breakdown(mid, diamonds[1L, ])

# Create a waterfall plot
ggmid(mbd, type = "waterfall")


# Create a bar plot with a different theme
ggmid(mbd, type = "barplot", theme = "highlight")
#> Warning: Ignoring empty aesthetic: `width`.


# Create a dot chart
ggmid(mbd, type = "dotchart", size = 3)
```
