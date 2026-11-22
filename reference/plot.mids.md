# Compare MID Component Functions Across Models

Compare main effects across a collection of MID models (`"mids"`).
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders via
base graphics, while
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns **ggplot2** objects via the `dualplot` engine.

## Usage

``` r
# S3 method for class 'mids'
plot(
  x,
  terms = NULL,
  type = c("effect", "series"),
  theme = NULL,
  intercept = FALSE,
  limits = c(NA, NA),
  resolution = NULL,
  labels = NULL,
  ...
)

# S3 method for class 'mids'
ggmid(
  object,
  terms = NULL,
  type = c("effect", "series"),
  theme = NULL,
  intercept = FALSE,
  limits = c(NA, NA),
  resolution = NULL,
  labels = NULL,
  ...
)
```

## Arguments

- x, object:

  a `"mids"` collection object.

- terms:

  optional character vector or formula specifying main-effect terms to
  plot.

- type:

  comparison style: `"effect"` (default) or `"series"`.

- theme:

  a color theme name or `"colortheme"` object.

- intercept:

  logical. If `TRUE`, adds the intercept to the plotted effect (or
  inferred from `terms` when omitted).

- limits:

  optional numeric vector of length 2 for the effect axis limits.

- resolution:

  optional integer specifying evaluation grid density for continuous
  variables.

- labels:

  optional numeric or character vector of model labels.

- ...:

  additional graphical parameters passed to the underlying `dualplot`
  layer.

## Value

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns `NULL`
invisibly.
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
returns a `"ggplot"` object for a single plot, or a named list of
`"ggplot"` objects for multiple plots.

## Details

The `type` argument selects the comparison layout:

- `"effect"` (default): overlays component curves/bars across models.

- `"series"`: transposes the comparison to show effect trends across
  models for each feature value.

## See also

[`plot.mid`](https://ryo-asashi.github.io/midr/reference/plot.mid.md),
[`midlist`](https://ryo-asashi.github.io/midr/reference/midlist.md)

## Examples

``` r
data(iris, package = "datasets")
mids <- interpret(
  cbind(Sepal.Length, Sepal.Width) ~ (Petal.Length + Petal.Width)^2,
  data = iris, lambda = 0.5
)
#> 'model' not passed: response variable in 'data' is used

plot(mids, terms = "Petal.Length", legend = "topleft")

plot(mids, terms = "Petal.Length", type = "series", legend = "bottom")


if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggmid(mids, terms = "Petal.Length", type = "series")
}


if (requireNamespace("survival", quietly = TRUE)) {
  vet <- na.omit(survival::veteran)
  fit <- survival::coxph(survival::Surv(time, status) ~ trt + celltype + karno + age, data = vet)
  survmid <- interpret(~ trt + celltype + karno + age, data = vet, model = fit, lambda = .1)
  plot(survmid, terms = c("karno+1", "celltype+1"), type = "series", legend = TRUE)
}

```
