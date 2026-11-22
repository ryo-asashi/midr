# Compare Prediction Breakdowns Across Models

Compare prediction breakdowns across a `"midbrks"` collection.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders via
base graphics, while
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns **ggplot2** objects via the `dualplot` engine.

## Usage

``` r
# S3 method for class 'midbrks'
plot(
  x,
  type = c("barplot", "dotchart", "series"),
  theme = NULL,
  terms = NULL,
  max.nterms = 15L,
  vline = TRUE,
  others = "others",
  pattern = c("%t=%v", "%t:%t"),
  format.args = list(),
  labels = NULL,
  ...
)

# S3 method for class 'midbrks'
ggmid(
  object,
  type = c("barplot", "dotchart", "series"),
  theme = NULL,
  terms = NULL,
  max.nterms = 15L,
  vline = TRUE,
  others = "others",
  pattern = c("%t=%v", "%t:%t"),
  format.args = list(),
  labels = NULL,
  ...
)
```

## Arguments

- x, object:

  a `"midbrks"` collection object.

- type:

  visualization style: `"barplot"` (default), `"dotchart"`, or
  `"series"`.

- theme:

  optional color theme name or `"colortheme"` object.

- terms:

  optional character vector specifying which terms/variables to display.

- max.nterms:

  maximum number of terms to display; remaining terms are aggregated
  into `others`.

- vline:

  logical. If `TRUE` (default), draws a reference line at zero.

- others:

  character label for the aggregated residual category (default
  `"others"`).

- pattern:

  character vector of length 1 or 2 controlling axis label formatting
  (`"%t"` for variable/term name, `"%v"` for observed value).

- format.args:

  named list of additional arguments passed to
  [`format()`](https://rdrr.io/r/base/format.html).

- labels:

  optional numeric or character vector of model or observation labels.

- ...:

  additional graphical parameters passed to `dualplot`.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly.
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns a `"ggplot"` object.

## See also

[`eval_breakdown`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md),
[`plot.midbrk`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md),
[`midlist`](https://ryo-asashi.github.io/midr/reference/midlist.md)
