# Draw Multi-Line Series

Draw Multi-Line Series

## Usage

``` r
draw_lines(
  x,
  y,
  discrete = NULL,
  limits = NULL,
  points = NULL,
  ...,
  main = NULL,
  sub = NULL,
  xlab = NULL,
  ylab = NULL,
  theme = NULL,
  theme.values = NULL,
  legend = NULL,
  legend.title = NULL,
  engine = getOption("dualplot.engine", "graphics")
)
```

## Arguments

- x:

  vector of x-axis values (numeric, factor, or character).

- y:

  numeric matrix or vector of y-values (`NROW(y) == length(x)`).

- discrete:

  logical; if `NULL`, inferred automatically from `x`.

- limits:

  optional numeric vector of length 2 for y-axis limits.

- points:

  optional `data.frame` with columns `x` and `y` (and optional `col`,
  `pch`, `cex`) for overlaying points.

- ...:

  additional graphical parameters.

- main, sub, xlab, ylab:

  optional plot titles and axis labels.

- theme:

  optional color theme name or `"colortheme"` object.

- theme.values:

  optional vector (length `ncol(y)`) mapped to line colors via `theme`.

- legend:

  logical or character legend position.

- legend.title:

  optional title string for the legend.

- engine:

  graphics backend: `"graphics"` or `"ggplot2"`.

## Value

Invisibly returns `NULL` for `"graphics"`, or a `"ggplot"` object for
`"ggplot2"`.
