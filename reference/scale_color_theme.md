# Color Theme Scales for ggplot2 Graphics

`scale_color_theme()` and its family of functions provide a unified
interface to apply color themes to the `colour` and `fill` aesthetics of
`ggplot2` objects.

## Usage

``` r
scale_color_theme(
  theme = NULL,
  ...,
  discrete = NULL,
  middle = 0,
  aesthetics = "colour"
)

scale_colour_theme(
  theme = NULL,
  ...,
  discrete = NULL,
  middle = 0,
  aesthetics = "colour"
)

scale_fill_theme(
  theme = NULL,
  ...,
  discrete = NULL,
  middle = 0,
  aesthetics = "fill"
)
```

## Arguments

- theme:

  a color theme name (e.g., `"Viridis"`), a `"colortheme"` object, a
  character vector of color names, or a palette/ramp function. See
  [`colortheme`](https://ryo-asashi.github.io/midr/reference/colortheme.md)
  for details.

- ...:

  optional arguments passed to
  [`ggplot2::continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.html)
  or
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

- discrete:

  logical. If `TRUE`, a discrete scale is used regardless of the theme
  type.

- middle:

  a numeric value specifying the midpoint for diverging color themes.
  Default is `0`.

- aesthetics:

  the aesthetic(s) to be scaled (`"colour"`, `"color"`, or `"fill"`).

## Value

A `ggplot2` scale object (`"ScaleContinuous"` or `"ScaleDiscrete"`).

## Details

This function automatically selects the appropriate **ggplot2** scale
based on the theme's type:

- If the theme is `"qualitative"`, a discrete scale is used by default
  (`discrete = TRUE`).

- If the theme is `"sequential"` or `"diverging"`, a continuous scale is
  used by default. Diverging themes use
  [`scales::rescale_mid()`](https://scales.r-lib.org/reference/rescale_mid.html)
  to center the gradient around `middle`.

## See also

[`colortheme`](https://ryo-asashi.github.io/midr/reference/colortheme.md)
