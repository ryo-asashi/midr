# Retrieve Color Theme Information

`color.theme.info()` returns a data frame listing all available color
themes.

`color.theme.env()` provides direct access to the environment where the
color themes are registered.

## Usage

``` r
color.theme.info(env = color.theme.env())

color.theme.env()
```

## Arguments

- env:

  an environment where the color themes are registered.

## Value

`color.theme.info()` returns a data frame with columns "name", "source",
and "type".

`color.theme.env()` returns the environment currently used as the
default theme registry.

## Details

These functions provide tools for inspecting the color themes available
in the current R session.

`color.theme.info()` is the primary user-facing function for discovering
themes by name, source, and type.

`color.theme.env()` is an advanced function that returns the environment
currently used as the theme registry. It first checks for a
user-specified environment via `getOption("midr.color.theme.env")`. If
this option is `NULL` (the default), the function returns the package's
internal environment where the default themes are stored.

## See also

[`color.theme`](https://ryo-asashi.github.io/midr/reference/color.theme.md),
[`set.color.theme`](https://ryo-asashi.github.io/midr/reference/set.color.theme.md)

## Examples

``` r
# Get a data frame of all available themes
head(color.theme.info())
#>            name    source      type
#> 1      ArmyRose grDevices diverging
#> 2        Berlin grDevices diverging
#> 3      Blue-Red grDevices diverging
#> 4    Blue-Red 2 grDevices diverging
#> 5    Blue-Red 3 grDevices diverging
#> 6 Blue-Yellow 2 grDevices diverging

# Get the environment where color themes are stored
theme_env <- color.theme.env()
names(theme_env)[1:5]
#> [1] "sunset" "oleron" "Sunset" "RdYlGn" "Set 1" 
```
