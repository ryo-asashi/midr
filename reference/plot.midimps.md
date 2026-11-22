# Compare Term or Variable Importance Across Models

Compare term or variable importance across a `"midimps"` collection.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders via
base graphics, while
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns **ggplot2** objects via the `dualplot` engine.

## Usage

``` r
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
```

## Arguments

- x, object:

  a `"midimps"` collection object.

- type:

  visualization style: `"barplot"` (default), `"dotchart"`, or
  `"series"`.

- theme:

  optional color theme name or `"colortheme"` object.

- terms:

  optional character vector specifying which terms/variables to display.

- max.nterms:

  maximum number of terms to display.

- labels:

  optional numeric or character vector of model labels.

- ...:

  additional graphical parameters passed to `dualplot`.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly.
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns a `"ggplot"` object.

## See also

[`plot.midimp`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md),
[`eval_importance`](https://ryo-asashi.github.io/midr/reference/eval_importance.md),
[`midlist`](https://ryo-asashi.github.io/midr/reference/midlist.md)
