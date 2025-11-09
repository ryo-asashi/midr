# Register Color Themes

`set.color.theme()` registers a custom color theme in the package's
theme registry.

## Usage

``` r
set.color.theme(
  kernel,
  kernel.args = list(),
  options = list(),
  name = "newtheme",
  source = "custom",
  type = NULL,
  env = color.theme.env()
)
```

## Arguments

- kernel:

  a color vector, a palette function, or a ramp function to be used as a
  color kernel. It can also be a character vector or a list (see the
  "Details" section). A "color.theme" object can also be passed.

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

- env:

  an environment where the color themes are registered.

## Value

`set.color.theme()` returns the metadata of the previous theme that was
overwritten (or `NULL` if none existed) invisibly.

## Details

This function takes a color vector, a color-generating function, or an
existing "color.theme" object and registers it under a specified `name`
and `source` (default is "custom/newtheme"). The registered color theme
can then be easily retrieved using the "Theme Name Syntax" (see
[`help(color.theme)`](https://ryo-asashi.github.io/midr/reference/color.theme.md)).

To keep the registry environment size small, the `kernel` argument
supports a form of lazy loading. To use this feature, provide a vector
or list containing two character strings. The first is an R expression
that returns a color kernel (e.g., "rainbow"), and the second is the
namespace in which to evaluate the expression (e.g., "grDevices"). The
expression is evaluated only when the color theme is loaded by
[`color.theme()`](https://ryo-asashi.github.io/midr/reference/color.theme.md).

## See also

[`color.theme`](https://ryo-asashi.github.io/midr/reference/color.theme.md),
[`color.theme.info`](https://ryo-asashi.github.io/midr/reference/color.theme.info.md)
