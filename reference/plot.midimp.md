# Plot and Compare MID Importance

Visualize term or variable importance for a `"midimp"` object, or
compare importance across a `"midimps"` collection.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders via
base graphics, while
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
return **ggplot2** objects via the `dualplot` engine.

## Usage

``` r
# S3 method for class 'midimp'
plot(
  x,
  type = c("barplot", "dotchart", "heatmap", "boxplot"),
  theme = NULL,
  terms = NULL,
  max.nterms = 30L,
  ...
)

# S3 method for class 'midimp'
ggmid(
  object,
  type = c("barplot", "dotchart", "heatmap", "boxplot"),
  theme = NULL,
  terms = NULL,
  max.nterms = 30L,
  ...
)

# S3 method for class 'midimp'
autoplot(object, ...)

# S3 method for class 'midimps'
plot(
  x,
  type = c("barplot", "dotchart", "series"),
  theme = NULL,
  terms = NULL,
  max.nterms = 30L,
  labels = NULL,
  ...
)

# S3 method for class 'midimps'
ggmid(
  object,
  type = c("barplot", "dotchart", "series"),
  theme = NULL,
  terms = NULL,
  max.nterms = 30L,
  labels = NULL,
  ...
)

# S3 method for class 'midimps'
autoplot(object, ...)
```

## Arguments

- x, object:

  a `"midimp"` or `"midimps"` object.

- type:

  visualization style: for `"midimp"`, one of `"barplot"` (default),
  `"dotchart"`, `"heatmap"` (when `by = "term"`), or `"boxplot"`; for
  `"midimps"`, one of `"barplot"` (default), `"dotchart"`, or
  `"series"`.

- theme:

  optional color theme name or `"colortheme"` object.

- terms:

  optional character vector specifying which terms/variables to display.

- max.nterms:

  maximum number of terms to display (ignored for `"heatmap"`).

- ...:

  additional graphical parameters passed to `dualplot`.

- labels:

  optional numeric or character vector of model labels for `"midimps"`.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly.
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
return a `"ggplot"` object.

## See also

[`eval_importance`](https://ryo-asashi.github.io/midr/reference/eval_importance.md)
