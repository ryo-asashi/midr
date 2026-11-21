# Map Values to Colors using Color Themes

`map_to_colors()` maps a vector of data values to a vector of
hexadecimal color codes based on a specified color theme. It
automatically handles both discrete and continuous variables,
appropriately scaling values and handling missing data.

## Usage

``` r
map_to_colors(x, theme = NULL, discrete = NULL, middle = 0, na.value = NULL)
```

## Arguments

- x:

  a vector of data values (discrete or continuous) to be mapped to
  colors.

- theme:

  a color theme name (e.g., `"Viridis"`), a `"colortheme"` object, a
  character vector of color names, or a palette/ramp function. See
  [`colortheme`](https://ryo-asashi.github.io/midr/reference/colortheme.md)
  for details.

- discrete:

  logical. If `TRUE`, a discrete scale is used regardless of the data
  type and the theme type.

- middle:

  a numeric value specifying the midpoint for diverging color themes.
  Default is `0`.

- na.value:

  a character string specifying the color for `NA` values. If `NULL`
  (default), falls back to the theme's `na.color` option.

## Value

A character vector of hexadecimal color codes.

## See also

[`colortheme`](https://ryo-asashi.github.io/midr/reference/colortheme.md)
