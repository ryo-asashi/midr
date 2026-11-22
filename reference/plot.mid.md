# Plot MID Component Functions

Visualize component functions of a fitted MID model (`"mid"`).
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders via
base graphics, while `ggmid()` returns **ggplot2** objects via the
`dualplot` engine.

## Usage

``` r
# S3 method for class 'mid'
plot(
  x,
  terms = NULL,
  type = c("effect", "data", "compound"),
  theme = NULL,
  intercept = FALSE,
  main.effects = FALSE,
  data = NULL,
  limits = c(NA, NA),
  jitter = NULL,
  resolution = c(100L, 100L),
  lumped = TRUE,
  ...
)

ggmid(object, ...)

# S3 method for class 'mid'
ggmid(
  object,
  terms = NULL,
  type = c("effect", "data", "compound"),
  theme = NULL,
  intercept = FALSE,
  main.effects = FALSE,
  data = NULL,
  limits = c(NA, NA),
  jitter = NULL,
  resolution = c(100L, 100L),
  lumped = TRUE,
  ...
)
```

## Arguments

- x, object:

  a `"mid"` object to be visualized.

- terms:

  a character vector of term labels/expressions or a formula specifying
  what to plot. Defaults to all main effects.

- type:

  visualization style: `"effect"`, `"data"`, or `"compound"`.

- theme:

  a color theme name or `"colortheme"` object.

- intercept:

  logical. If `TRUE`, adds the intercept to the plotted effect (or
  inferred from each term expression when omitted).

- main.effects:

  logical. If `TRUE`, includes constituent main effects when plotting an
  interaction term.

- data:

  a data.frame used for `type = "data"` / `"compound"` or when plotting
  raw-variable expressions (e.g., `"x"` when `log(x)` is in the model).

- limits:

  optional numeric vector of length 2 for the effect axis/color limits
  (may contain `NA` for partial specification).

- jitter:

  numeric amount of jitter for discrete axes when plotting points.

- resolution:

  integer (or length-2 integer vector for 2D surfaces) specifying the
  evaluation grid density for continuous variables.

- lumped:

  logical. If `TRUE` (default), uses lumped factor levels when a
  variable appears only in main effects or only in interactions;
  automatically set to `FALSE` for any variable that mixes both.

- ...:

  additional graphical parameters passed to the underlying `dualplot`
  layer.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly. `ggmid()` returns a `"ggplot"` object for a single plot, or a
named list of `"ggplot"` objects for multiple plots.

## Details

Each element of `terms` produces one plot. A single element may be a
term label (e.g., `"Wind"`, `"Wind:Temp"`) or a formula-like expression
string (e.g., `"Wind * Temp + 1"` or `"Wind + Temp"`), which is
extracted via
[`subset.mid()`](https://ryo-asashi.github.io/midr/reference/subset.mid.md)
and rendered as a 1D curve/bar chart or a 2D surface depending on the
number of constituent variables. For 2D plots, the order in which
variables appear in the term expression determines the x- and y-axes
(e.g., `"Wind:Temp"` vs `"Temp:Wind"`). If a `formula` is passed (e.g.,
`~ Wind + Temp`), it is expanded into individual term plots.

The `type` argument selects the visualization style:

- `"effect"` (default): plots the component function(s) alone.

- `"data"`: plots observed points from `data` colored by the component
  effect.

- `"compound"`: overlays observed points on top of the component
  function.

## See also

[`interpret`](https://ryo-asashi.github.io/midr/reference/interpret.md),
[`plot.mids`](https://ryo-asashi.github.io/midr/reference/plot.mids.md),
[`subset.mid`](https://ryo-asashi.github.io/midr/reference/subset.mid.md),
[`predict.mid`](https://ryo-asashi.github.io/midr/reference/predict.mid.md)
