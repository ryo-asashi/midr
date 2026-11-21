# Plot and Compare MID Breakdown

Visualize the prediction breakdown for a `"midbrk"` object, or compare
breakdowns across a `"midbrks"` collection.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders via
base graphics, while
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
return **ggplot2** objects via the `dualplot` engine.

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

# S3 method for class 'midbrk'
autoplot(object, ...)

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

# S3 method for class 'midbrks'
autoplot(object, ...)
```

## Arguments

- x, object:

  a `"midbrk"` or `"midbrks"` object.

- type:

  visualization style: for `"midbrk"`, one of `"waterfall"` (default),
  `"barplot"`, or `"dotchart"`; for `"midbrks"`, one of `"barplot"`
  (default), `"dotchart"`, or `"series"`.

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

- labels:

  optional numeric or character vector of model labels for `"midbrks"`.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly.
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
return a `"ggplot"` object.

## See also

[`eval_breakdown`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md)
