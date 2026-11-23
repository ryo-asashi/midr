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

## Examples

``` r
data(mtcars, package = "datasets")
mid <- interpret(mpg ~ (wt + hp + cyl)^2, data = mtcars, lambda = 0.5)
#> 'model' not passed: response variable in 'data' is used
brks <- eval_breakdown(
  mid, data = mtcars,
  rows = c("Toyota Corolla", "Mazda RX4", "Maserati Bora"),
  by = "variable"
)

plot(brks, type = "dotchart")

plot(brks, type = "series")


if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggmid(brks, type = "dotchart")
}


if (requireNamespace("survival", quietly = TRUE)) {
  vet <- na.omit(survival::veteran)
  fit <- survival::coxph(survival::Surv(time, status) ~ trt + celltype + karno + age, data = vet)
  survmid <- interpret(~ trt + celltype + karno + age, data = vet, model = fit, lambda = .1)
  brk <- eval_breakdown(survmid, data = vet[1L, , drop = FALSE])
  plot(brk, type = "series")
}
```
