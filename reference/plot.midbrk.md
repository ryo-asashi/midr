# Plot Prediction Breakdown

Visualize the prediction breakdown for a `"midbrk"` object.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders via
base graphics, while
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns **ggplot2** objects via the `dualplot` engine.

## Usage

``` r
# S3 method for class 'midbrk'
plot(
  x,
  type = c("waterfall", "barplot", "dotchart"),
  theme = NULL,
  terms = NULL,
  max.nterms = 15L,
  vline = TRUE,
  others = "others",
  pattern = c("%t=%v", "%t:%t"),
  format.args = list(),
  ...
)

# S3 method for class 'midbrk'
ggmid(
  object,
  type = c("waterfall", "barplot", "dotchart"),
  theme = NULL,
  terms = NULL,
  max.nterms = 15L,
  vline = TRUE,
  others = "others",
  pattern = c("%t=%v", "%t:%t"),
  format.args = list(),
  ...
)
```

## Arguments

- x, object:

  a `"midbrk"` object.

- type:

  visualization style: `"waterfall"` (default), `"barplot"`, or
  `"dotchart"`.

- theme:

  optional color theme name or `"colortheme"` object.

- terms:

  optional character vector specifying which terms/variables to display.

- max.nterms:

  maximum number of terms to display; remaining terms are aggregated
  into `others`.

- vline:

  logical. If `TRUE` (default), draws a reference line at zero (or at
  the intercept for waterfall plots).

- others:

  character label for the aggregated residual category (default
  `"others"`).

- pattern:

  character vector of length 1 or 2 controlling axis label formatting
  (`"%t"` for variable/term name, `"%v"` for observed value).

- format.args:

  named list of additional arguments passed to
  [`format()`](https://rdrr.io/r/base/format.html).

- ...:

  additional graphical parameters passed to `dualplot`.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly.
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns a `"ggplot"` object.

## See also

[`eval_breakdown`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md),
[`plot.midbrks`](https://ryo-asashi.github.io/midr/reference/plot.midbrks.md)
