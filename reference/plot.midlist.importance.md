# Compare MID Importance across Multiple Models

For "midlist.importance" objects,
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) visualizes and
compares the importance of component functions across multiple fitted
MID models.

## Usage

``` r
# S3 method for class 'midlist.importance'
plot(
  x,
  type = c("barplot", "dotchart"),
  theme = NULL,
  terms = NULL,
  max.nterms = 30L,
  ...
)
```

## Arguments

- x:

  a "midlist.importance" object to be visualized.

- type:

  the plotting style. One of "barplot" or "dotchart".

- theme:

  a character string or object defining the color theme. Defaults to
  "HCL". See
  [`color.theme`](https://ryo-asashi.github.io/midr/reference/color.theme.md)
  for details.

- terms:

  an optional character vector specifying which terms to display. If
  `NULL`, terms are extracted from the object.

- max.nterms:

  the maximum number of terms to display. Defaults to 30.

- ...:

  optional parameters passed on to the graphing functions. Possible
  arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of
  them.

## Value

`plot.midlist.importance()` produces a plot as a side effect and returns
`NULL` invisibly.

## Details

This is an S3 method for the
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) generic that
creates a comparative importance plot from a "midlist.importance"
object. It visualizes the average contribution of component functions to
the fitted MID models, allowing for easy comparison across different
models.

The `type` argument controls the visualization style. The default,
`type = "barplot"`, creates a standard grouped bar plot where the length
of each bar represents the overall importance of the term, positioned
side-by-side by model label. The `type = "dotchart"` option creates a
grouped dot plot, offering a clean alternative to the bar plot for
visualizing and comparing term importance across models.

## See also

[`plot.mid.importance`](https://ryo-asashi.github.io/midr/reference/plot.mid.importance.md),
[`ggmid.midlist.importance`](https://ryo-asashi.github.io/midr/reference/ggmid.midlist.importance.md)
