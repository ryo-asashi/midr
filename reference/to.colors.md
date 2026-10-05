# Map Values to Colors using Color Themes

`to.colors()` maps a vector of data values to a vector of hexadecimal
color codes based on a specified color theme. It automatically handles
both discrete and continuous variables, appropriately scaling the values
and handling missing data.

## Usage

``` r
to.colors(x, theme = NULL, middle = 0, na.value = NULL)
```

## Arguments

- x:

  a vector of data values (discrete or continuous) to be mapped to
  colors.

- theme:

  a color theme name (e.g., "Viridis"), a character vector of color
  names, or a palette/ramp function. See
  [`?color.theme`](https://ryo-asashi.github.io/midr/reference/color.theme.md)
  for more details.

- middle:

  a numeric value specifying the middle point for the diverging color
  themes. Default is `0`.

- na.value:

  a character string specifying the color for `NA` values. If `NULL`
  (default), it falls back to the theme's default `na.color` option.

## Value

a character vector of hexadecimal color codes.

## See also

[`color.theme`](https://ryo-asashi.github.io/midr/reference/color.theme.md)

## Examples

``` r
# Continuous mapping (Sequential)
plot(cars, col = to.colors(cars$speed, "Viridis"), pch = 19)


# Discrete mapping (Qualitative)
plot(iris$Sepal.Length, iris$Sepal.Width,
     col = to.colors(iris$Species, "Set2"), pch = 19)
```
