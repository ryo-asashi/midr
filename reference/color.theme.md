# Color Themes for Graphics

The `color.theme()` function is the main interface for working with
"color.theme" objects. It acts as a dispatcher that, depending on the
class of `object`, can retrieve a pre-defined theme by name (see the
"Theme Name Syntax" section), create a new theme from a vector of colors
or a color-generating function, and modify an existing "color.theme"
object.

## Usage

``` r
color.theme(
  object,
  kernel.args = list(),
  options = list(),
  name = NULL,
  source = NULL,
  type = NULL,
  reverse = FALSE,
  env = color.theme.env(),
  ...
)
```

## Arguments

- object:

  a character string to retrieve a pre-defined theme, a color kernel
  (i.e., a vector of colors or a color generating function) to create a
  new theme, or a "color.theme" object to be modified. See the "Details"
  section.

- kernel.args:

  a list of arguments to be passed to the color kernel.

- options:

  a list of option values to control the color theme's behavior.

- name:

  a character string for the color theme name.

- source:

  a character string for the source name of the color theme.

- type:

  a character string specifying the type of the color theme. One of
  "sequential", "diverging", or "qualitative".

- reverse:

  logical. If `TRUE`, the order of colors is reversed.

- env:

  an environment where the color themes are registered.

- ...:

  optional named arguments used to modify the color theme. Any argument
  passed here will override the corresponding settings in `kernel.args`
  or `options`.

- kernel:

  a color vector, a palette function, or a ramp function that serves as
  the basis for generating colors.

## Value

`color.theme()` returns a "color.theme" object, which is an environment
with the special class attribute, containing the `...$palette()` and
`...$ramp` functions, along with other metadata about the theme.

## Details

The "color.theme" object is a special environment that provides two
color-generating functions: `...$palette()` and `...$ramp()`.

`...$palette()` takes an integer `n` and returns a vector of `n`
discrete colors. It is primarily intended for qualitative themes, where
distinct colors are used to represent categorical data.

`...$ramp()` takes a numeric vector `x` with values in the \[0, 1\]
interval, and returns a vector of corresponding colors. It maps numeric
values onto a continuous color gradient, making it suitable for
sequential and diverging themes.

This function, `color.theme()`, is a versatile dispatcher that behaves
differently depending on the class of the `object` argument. If `object`
is a character string (e.g., "Viridis", "grDevices/RdBu_r@q?alpha=.5"),
the string is parsed according to the theme name syntax, and the
corresponding pre-defined theme is loaded (see the "Theme Name Syntax"
section for details). If `object` is a color kernel (i.e., a character
vector of colors, a palette function, or a ramp function), a new color
theme is created from the kernel. If `object` is a "color.theme" object,
the function returns a modified version of the theme, applying any other
arguments to update its settings.

## Theme Name Syntax

When retrieving a theme using a character string, you can use a special
syntax to specify the source and apply modifications:

"`[(source)/](name)[_r][@(type)][?(query)]`"

- source: (optional) the source package or collection of the theme
  (e.g., "grDevices").

- name: the name of the theme (e.g., "RdBu").

- "\_r": (optional) a suffix to reverse the color order.

- type: (optional) the desired theme type, which will be matched with
  "sequential", "diverging" or "qualitative" (i.e., "s", "d", and "q"
  are sufficient, but longer strings such as "seq", "div", "qual" are
  also possible).

- query: (optional) a query string to overwrite the color theme's
  metadata including specific theme options or kernel arguments. Pairs
  are in `key=value` format and separated by `;` or `&` (e.g.,
  "...?alpha=0.5;na.color='gray50'"). Possible keys include "name",
  "source", "type", "reverse" and any item of the theme's `options` and
  `kernel.args`.

## See also

[`scale_color_theme`](https://ryo-asashi.github.io/midr/reference/scale_color_theme.md),
[`set.color.theme`](https://ryo-asashi.github.io/midr/reference/set.color.theme.md),
[`color.theme.info`](https://ryo-asashi.github.io/midr/reference/color.theme.info.md)

## Examples

``` r
# Retrieve a pre-defined theme
ct <- color.theme("Mako")
ct$palette(5L)
#> [1] "#070707" "#423460" "#007FA8" "#48C2B4" "#E0F7E1"
ct$ramp(seq.int(0, 1, length.out = 5))
#> [1] "#060606" "#42345F" "#007FA7" "#48C2B3" "#DFF7E0"

# Use special syntax to get a reversed, qualitative theme with alpha value
ct <- color.theme("grDevices/Zissou 1_r@qual?alpha=0.75")
ct$palette(5L)
#> [1] "#F5191CBF" "#E78F0ABF" "#EACB2BBF" "#7CBA96BF" "#3B99B1BF"
ct$ramp(seq.int(0, 1, length.out = 5))
#> [1] "#F5191CBF" "#E78E09BF" "#E6CA4CBF" "#7BBA96BF" "#3B99B1BF"

# Create a new theme from a vector of colors
ct <- color.theme(c("#003f5c", "#7a5195", "#ef5675", "#ffa600"))
ct$palette(5L)
#> [1] "#003F5B" "#614D86" "#B85485" "#F46C63" "#FFA500"
ct$ramp(seq.int(0, 1, length.out = 5))
#> [1] "#003F5B" "#614D86" "#B85485" "#F46C63" "#FFA500"

# Create a new theme from a palette function
ct <- color.theme(grDevices::rainbow)
ct$palette(5L)
#> [1] "#FF0000" "#CCFF00" "#00FF66" "#0066FF" "#CC00FF"
ct$ramp(seq.int(0, 1, length.out = 5))
#> [1] "#FF0000" "#81FF00" "#00FFFB" "#7B00FF" "#FF0006"

# Modify an existing theme
ct <- color.theme(ct, type = "qualitative", kernel.args = list(v = 0.5))
ct$palette(5L)
#> [1] "#800000" "#668000" "#008033" "#003380" "#660080"
ct$ramp(seq.int(0, 1, length.out = 5))
#> [1] "#7F0000" "#407F00" "#007F7E" "#3D007F" "#7F0002"
```
