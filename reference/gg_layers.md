# Low-Level Filtered ggplot2 Layer Constructors

Low-Level Filtered ggplot2 Layer Constructors

## Usage

``` r
layer_point(mapping = NULL, data = NULL, ...)

layer_line(mapping = NULL, data = NULL, ...)

layer_path(mapping = NULL, data = NULL, ...)

layer_linerange(mapping = NULL, data = NULL, ...)

layer_col(mapping = NULL, data = NULL, ...)

layer_rect(mapping = NULL, data = NULL, ...)

layer_tile(mapping = NULL, data = NULL, ...)

layer_raster(mapping = NULL, data = NULL, ...)

layer_boxplot(mapping = NULL, data = NULL, ...)

layer_linepoint(mapping = NULL, data = NULL, ...)

layer_dotchart(mapping = NULL, data = NULL, ...)

layer_vline(xintercept = 0, scale_lwd = 0.5, ...)

layer_hline(yintercept = 0, scale_lwd = 0.5, ...)
```

## Arguments

- mapping:

  set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

- data:

  a data frame to be displayed in this layer.

- ...:

  additional parameters (both base and ggplot2 names supported).

## Value

a `ggplot2` layer object (or list of layers for compound geoms).
