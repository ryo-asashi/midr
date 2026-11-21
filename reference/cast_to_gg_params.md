# Translate and Filter Parameters for ggplot2 Layers

Converts base graphics parameter names in `dots` to standard ggplot2
aesthetics without requiring ggplot2 to be loaded at package build time.

## Usage

``` r
cast_to_gg_params(dots = list(), allowed = NULL, defaults = list())
```

## Arguments

- dots:

  a named list of user-supplied parameters (typically `list(...)`).

- allowed:

  optional character vector of allowed parameter names for a specific
  geom.

- defaults:

  optional named list of default parameters.

## Value

a sanitized named list of parameters for a ggplot2 layer.
