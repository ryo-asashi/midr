# Translate and Merge Parameters for Base Graphics

Converts ggplot2-style parameter names in `dots` to base graphics
equivalents, merges them with `defaults`, and optionally applies alpha
transparency.

## Usage

``` r
cast_to_base_params(
  dots = list(),
  allowed = NULL,
  defaults = list(),
  alpha.on = "col"
)
```

## Arguments

- dots:

  a named list of user-supplied parameters (typically `list(...)`).

- allowed:

  optional character vector of allowed parameter names.

- defaults:

  a named list of default base graphics parameters.

- alpha.on:

  character vector of parameter names (e.g., `"col"`, `"fill"`) to which
  the `alpha` parameter should be applied. Set to `NULL` to leave
  `alpha` as-is.

## Value

a sanitized named list ready for `do.call(graphics::..., args)`.
