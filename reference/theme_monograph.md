# Monograph Visual Theme for ggplot2

Returns a clean, publication-ready **ggplot2** theme featuring a white
background, crisp dark border, optional grid lines, and serif
typography.

## Usage

``` r
theme_monograph(
  grid.type = c("none", "x", "y", "xy"),
  base_size = 11,
  base_family = "serif",
  base_line_size = base_size/22,
  base_rect_size = base_size/22,
  ...
)
```

## Arguments

- grid.type:

  character string specifying which grid lines to display: one of
  `"none"` (default), `"x"`, `"y"`, or `"xy"`.

- base_size:

  base font size in pts (default `11`).

- base_family:

  base font family (default `"serif"`).

- base_line_size:

  base size for line elements.

- base_rect_size:

  base size for rectangle elements.

- ...:

  additional arguments passed to
  [`ggplot2::theme_light()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Value

A `ggplot2` `theme` object.

## See also

[`par_monograph`](https://ryo-asashi.github.io/midr/reference/par_monograph.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "cars") +
    theme_monograph(grid.type = "xy")
}

if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(iris, ggplot2::aes(Petal.Length, Species)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = "iris") +
    theme_monograph(grid.type = "x")
}
```
