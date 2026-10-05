# Retrieve Color Theme Information

`color.themes()` returns a data frame listing all available color
themes.

## Usage

``` r
color.themes(env = NULL)
```

## Arguments

- env:

  an environment where the color themes are registered. Defaults to the
  default color theme registry that can be accessed via
  `midr.options("color.theme.registry")`.

## Value

A data frame with columns `"name"`, `"source"`, and `"type"` containing
the metadata of the registered color themes. If no color themes are
found or the environment is empty, it returns an empty data frame with
these exact columns.

## Details

This function provides a convenient way to inspect the color themes
currently available in the R session. It extracts metadata from a
specified registry environment and structures it into a clean data
frame, allowing users to discover themes by their type, name, and
source.

## See also

[`color.theme`](https://ryo-asashi.github.io/midr/reference/color.theme.md),
[`color.theme.register`](https://ryo-asashi.github.io/midr/reference/color.theme.register.md)

## Examples

``` r
# Get a data frame of all available themes
head(color.themes())
#>            name    source      type
#> 1      ArmyRose grDevices diverging
#> 2        Berlin grDevices diverging
#> 3      Blue-Red grDevices diverging
#> 4    Blue-Red 2 grDevices diverging
#> 5    Blue-Red 3 grDevices diverging
#> 6 Blue-Yellow 2 grDevices diverging
```
