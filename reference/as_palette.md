# Extract Palette or Ramp Function

`as_palette()` and `as_ramp()` extract the underlying discrete palette
function or continuous color ramp function from a color theme or color
kernel.

## Usage

``` r
as_palette(x, ...)

# S3 method for class 'colortheme'
as_palette(x, ...)

# Default S3 method
as_palette(x, ...)

as_ramp(x, ...)

# S3 method for class 'colortheme'
as_ramp(x, ...)

# Default S3 method
as_ramp(x, ...)
```

## Arguments

- x:

  a `"colortheme"` object, a theme name (character string), a color
  vector, or a color-generating function.

- ...:

  additional arguments passed to
  [`colortheme`](https://ryo-asashi.github.io/midr/reference/colortheme.md)
  to modify the theme before extracting the function.

## Value

- `as_palette()` returns a palette function taking an integer `n` and
  returning `n` colors.

- `as_ramp()` returns a ramp function taking a numeric vector `x` in
  `[0, 1]` and returning corresponding colors.

## See also

[`colortheme`](https://ryo-asashi.github.io/midr/reference/colortheme.md)
