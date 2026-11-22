# Plot Term or Variable Importance

Visualize term or variable importance for a `"midimp"` object.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders via
base graphics, while
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns **ggplot2** objects via the `dualplot` engine.

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
```

## Arguments

- x, object:

  a `"midimp"` object.

- type:

  visualization style: `"barplot"` (default), `"dotchart"`, `"heatmap"`
  (when `by = "term"`), or `"boxplot"`.

- theme:

  optional color theme name or `"colortheme"` object.

- terms:

  optional character vector specifying which terms/variables to display.

- max.nterms:

  maximum number of terms to display (ignored for `"heatmap"`).

- ...:

  additional graphical parameters passed to `dualplot`.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly.
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns a `"ggplot"` object.

## See also

[`eval_importance`](https://ryo-asashi.github.io/midr/reference/eval_importance.md),
[`plot.midimps`](https://ryo-asashi.github.io/midr/reference/plot.midimps.md)
