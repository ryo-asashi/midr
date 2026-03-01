# Compare MID Breakdowns with ggplot2

For "midbrks" collection objects,
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
visualizes and compares the breakdown of a prediction by component
functions.

## Usage

``` r
# S3 method for class 'midbrks'
ggmid(
  object,
  type = c("barplot", "dotchart", "series"),
  theme = NULL,
  terms = NULL,
  max.nterms = 15L,
  vline = TRUE,
  others = "others",
  label.pattern = c("%t=%v", "%t:%t"),
  format.args = list(),
  labels = NULL,
  ...
)

# S3 method for class 'midbrks'
autoplot(object, ...)
```

## Arguments

- object:

  a "midbrks" collection object to be visualized.

- type:

  the plotting style. One of "barplot", "dotchart", or "series".

- theme:

  a character string or object defining the color theme. Defaults to
  "HCL". See
  [`color.theme`](https://ryo-asashi.github.io/midr/reference/color.theme.md)
  for details.

- terms:

  an optional character vector specifying which terms to display. If
  `NULL`, terms are automatically extracted from the object.

- max.nterms:

  the maximum number of terms to display. Defaults to 15.

- vline:

  logical. If `TRUE`, a vertical line is drawn at the zero or intercept
  line.

- others:

  a character string for the catchall label. Defaults to `"others"`.

- label.pattern:

  a character vector of length one or two specifying the format of the
  axis labels. The first element is used for main effects (default
  `"%t = %v"`), and the second is for interactions (default `"%t:%t"`).
  Use `"%t"` for the term name and `"%v"` for its value.

- format.args:

  a named list of additional arguments passed to
  [`format`](https://rdrr.io/r/base/format.html) for formatting the
  values. Common arguments include `digits`, `nsmall`, and `big.mark`.

- labels:

  an optional numeric or character vector to specify the model labels or
  x-axis coordinates. Defaults to the labels found in the object.

- ...:

  optional parameters passed on to the main layer (e.g.,
  [`geom_col`](https://ggplot2.tidyverse.org/reference/geom_bar.html)).

## Details

This is an S3 method for the
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
generic that creates a comparative importance plot from a "midbrks"
collection object. It visualizes the contribution of each component
function to a single prediction across multiple models, allowing for
easy comparison across different models.

The `type` argument controls the visualization style. The default,
`type = "barplot"`, creates a standard grouped bar plot where the length
of each bar represents the overall importance of the term, positioned
side-by-side (`position_dodge`) by model label. The `type = "dotchart"`
option creates a grouped dot plot, offering a clean alternative to the
bar plot for visualizing and comparing term importance across models.
