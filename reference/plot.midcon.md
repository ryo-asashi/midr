# Plot and Compare MID Conditional Expectations (ICE)

Visualize Individual Conditional Expectation (ICE) curves for a
`"midcon"` object, or compare ICE curves across a `"midcons"`
collection. [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
renders via base graphics, while
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
return **ggplot2** objects.

## Usage

``` r
# S3 method for class 'midcon'
plot(
  x,
  type = c("iceplot", "centered"),
  theme = NULL,
  term = NULL,
  var.alpha = NULL,
  var.color = NULL,
  var.linetype = NULL,
  var.linewidth = NULL,
  reference = 1L,
  points = TRUE,
  sample = NULL,
  ...
)

# S3 method for class 'midcon'
ggmid(
  object,
  type = c("iceplot", "centered"),
  theme = NULL,
  term = NULL,
  var.alpha = NULL,
  var.color = NULL,
  var.linetype = NULL,
  var.linewidth = NULL,
  reference = 1L,
  points = TRUE,
  sample = NULL,
  ...
)

# S3 method for class 'midcon'
autoplot(object, ...)

# S3 method for class 'midcons'
plot(
  x,
  type = c("iceplot", "centered", "series"),
  theme = NULL,
  var.alpha = NULL,
  var.linetype = NULL,
  var.linewidth = NULL,
  reference = 1L,
  sample = NULL,
  labels = NULL,
  ...
)

# S3 method for class 'midcons'
ggmid(
  object,
  type = c("iceplot", "centered", "series"),
  theme = NULL,
  var.alpha = NULL,
  var.linetype = NULL,
  var.linewidth = NULL,
  reference = 1L,
  sample = NULL,
  labels = NULL,
  ...
)

# S3 method for class 'midcons'
autoplot(object, ...)
```

## Arguments

- x, object:

  a `"midcon"` or `"midcons"` object.

- type:

  visualization style: for `"midcon"`, one of `"iceplot"` (default) or
  `"centered"`; for `"midcons"`, one of `"iceplot"` (default),
  `"centered"`, or `"series"`.

- theme:

  optional color theme name or `"colortheme"` object.

- term:

  optional term label to plot a single component's ICE curve (requires
  `keep.effects = TRUE`).

- var.alpha, var.color, var.linetype, var.linewidth:

  optional variable names or expressions evaluated in `observed` to map
  aesthetics across curves.

- reference:

  integer index of the grid point used for centering when
  `type = "centered"`.

- points:

  logical. If `TRUE` (default), overlays observed predictions for
  `"midcon"`.

- sample:

  optional character vector of observation IDs (`.id`) to display.

- ...:

  additional graphical parameters.

- labels:

  optional numeric or character vector of model labels for `"midcons"`.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly.
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
return a `"ggplot"` object.

## See also

[`eval_conditional`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md)
