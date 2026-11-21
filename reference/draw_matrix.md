# Draw Matrix Heatmaps

Draw Matrix Heatmaps

## Usage

``` r
draw_matrix(
  z,
  ticks = rownames(z),
  middle = 0,
  ...,
  main = NULL,
  sub = NULL,
  xlab = NULL,
  ylab = NULL,
  theme = NULL,
  legend = NULL,
  legend.title = NULL,
  engine = getOption("dualplot.engine", "graphics")
)
```

## Arguments

- z:

  numeric matrix of values to plot.

- ticks:

  character vector of row/column tick labels.

- middle:

  numeric midpoint for diverging color themes (default `0`).

- ...:

  additional graphical parameters.

- main, sub, xlab, ylab:

  optional plot titles and axis labels.

- theme:

  optional color theme name or `"colortheme"` object.

- legend:

  logical or character legend position.

- legend.title:

  optional title string for the legend.

- engine:

  graphics backend: `"graphics"` or `"ggplot2"`.

## Value

Invisibly returns `NULL` for `"graphics"`, or a `"ggplot"` object for
`"ggplot2"`.
