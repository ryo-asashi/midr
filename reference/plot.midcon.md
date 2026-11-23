# Plot Conditional Expectations

Visualize Individual Conditional Expectation (ICE) curves for a
`"midcon"` object.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders via
base graphics, while
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns **ggplot2** objects.

## Usage

``` r
# S3 method for class 'midcon'
plot(
  x,
  type = c("iceplot", "centered"),
  theme = NULL,
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
  var.alpha = NULL,
  var.color = NULL,
  var.linetype = NULL,
  var.linewidth = NULL,
  reference = 1L,
  points = TRUE,
  sample = NULL,
  ...
)
```

## Arguments

- x, object:

  a `"midcon"` object.

- type:

  visualization style: `"iceplot"` (default) or `"centered"`.

- theme:

  optional color theme name or `"colortheme"` object.

- var.alpha, var.color, var.linetype, var.linewidth:

  optional variable names or expressions evaluated in `observed` to map
  aesthetics across curves.

- reference:

  integer index of the grid point used for centering when
  `type = "centered"`.

- points:

  logical. If `TRUE` (default), overlays observed predictions.

- sample:

  optional character vector of observation IDs (`.id`) to display.

- ...:

  additional graphical parameters.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly.
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns a `"ggplot"` object.

## See also

[`eval_conditional`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md),
[`plot.midcons`](https://ryo-asashi.github.io/midr/reference/plot.midcons.md)

## Examples

``` r
data(airquality, package = "datasets")
aq <- na.omit(airquality)
mid <- interpret(Ozone ~ (Solar.R + Wind + Temp)^2, data = aq, lambda = 0.5)
#> 'model' not passed: response variable in 'data' is used
con <- eval_conditional(mid, variable = "Wind", data = aq, max.nsamples = 30L)
#> number of observations exceeds 'max.nsamples': a sample of 30 observations from 'data' is used
plot(con, var.color = Temp)


# Slice the Wind:Temp interaction via subset()
sub <- subset(mid, "Wind:Temp")
subcon <- eval_conditional(sub, variable = "Wind", data = aq, max.nsamples = 30L)
#> number of observations exceeds 'max.nsamples': a sample of 30 observations from 'data' is used
plot(subcon, type = "centered", var.color = Temp)


if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggmid(con, type = "centered", var.color = Temp)
}
```
