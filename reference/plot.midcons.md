# Compare Conditional Expectations Across Models

Compare Individual Conditional Expectation (ICE) curves across a
`"midcons"` collection.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders via
base graphics, while
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns **ggplot2** objects.

## Usage

``` r
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
```

## Arguments

- x, object:

  a `"midcons"` collection object.

- type:

  visualization style: `"iceplot"` (default), `"centered"`, or
  `"series"`.

- theme:

  optional color theme name or `"colortheme"` object.

- var.alpha, var.linetype, var.linewidth:

  optional variable names or expressions evaluated in `observed` to map
  aesthetics across curves.

- reference:

  integer index of the grid point used for centering when
  `type = "centered"`.

- sample:

  optional character vector of observation IDs (`.id`) to display.

- labels:

  optional numeric or character vector of model labels.

- ...:

  additional graphical parameters.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly.
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns a `"ggplot"` object.

## See also

[`eval_conditional`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md),
[`plot.midcon`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md),
[`midlist`](https://ryo-asashi.github.io/midr/reference/midlist.md)
