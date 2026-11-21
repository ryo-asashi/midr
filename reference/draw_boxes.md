# Draw Boxplots

Draw Boxplots

## Usage

``` r
draw_boxes(
  x,
  horizontal = TRUE,
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

  a named list of numeric vectors (one per category).

- horizontal:

  logical; if `TRUE`, boxes are drawn horizontally.

- ...:

  additional graphical parameters.

- main, sub, xlab, ylab:

  optional plot titles and axis labels.

- theme:

  optional color theme name or `"colortheme"` object.

- theme.values:

  optional vector (length `length(x)`) mapped to box fills via `theme`.

- legend:

  logical or character legend position.

- legend.title:

  optional title string for the legend.

- engine:

  graphics backend: `"graphics"` or `"ggplot2"`.

## Value

Invisibly returns `NULL` for `"graphics"`, or a `"ggplot"` object for
`"ggplot2"`.
