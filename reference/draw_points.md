# Draw Scatter and Jitter Plots

Draw Scatter and Jitter Plots

## Usage

``` r
draw_points(
  x,
  y,
  middle = 0,
  xticks = NULL,
  yticks = NULL,
  xlim = NULL,
  ylim = NULL,
  overlay = FALSE,
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

- x, y:

  numeric vectors of coordinates.

- middle:

  numeric midpoint for diverging color themes (default `0`).

- xticks, yticks:

  optional character vectors for discrete axis tick labels.

- xlim, ylim:

  optional axis limits.

- overlay:

  logical; if `TRUE` and `engine = "graphics"`, adds points to an
  existing plot.

- ...:

  additional graphical parameters.

- main, sub, xlab, ylab:

  optional plot titles and axis labels.

- theme:

  optional color theme name or `"colortheme"` object.

- theme.values:

  optional vector mapped to point colors via `theme`.

- legend:

  logical or character legend position.

- legend.title:

  optional title string for the legend.

- engine:

  graphics backend: `"graphics"` or `"ggplot2"`.

## Value

Invisibly returns `NULL` for `"graphics"`, or a `"ggplot"` object for
`"ggplot2"`.
