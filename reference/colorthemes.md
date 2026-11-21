# List, Register, and Configure Color Themes

`colorthemes()` returns a data frame listing available color themes in
the registry. `register_colortheme()` registers a custom color theme
into the registry. `colortheme_options()` sets or retrieves global
package options.

## Usage

``` r
colorthemes(name = NULL, source = NULL, type = NULL, env = NULL)

register_colortheme(
  kernel,
  kernel.args = list(),
  options = list(),
  name = "newtheme",
  source = "custom",
  type = NULL,
  env = NULL
)

colortheme_options(...)
```

## Arguments

- name:

  for `colorthemes()`, a character vector of theme names to match
  (case-insensitive prefix matching). For `register_colortheme()`, a
  single string for the new theme name.

- source:

  for `colorthemes()`, a character vector of sources to match
  (case-insensitive prefix matching). For `register_colortheme()`, a
  single string for the source name.

- type:

  a character string or vector specifying the theme type(s):
  `"sequential"`, `"diverging"`, or `"qualitative"`.

- env:

  an environment where color themes are registered. Defaults to the
  package registry (`getOption("colortheme.registry")`).

- kernel:

  a color vector, a palette function, a ramp function, a `"colortheme"`
  object, or a length-2 character vector/list for lazy evaluation. If
  `NULL`, the theme is removed.

- kernel.args:

  a list of arguments passed to the color kernel.

- options:

  a list of option values controlling the color theme's behavior.

- ...:

  for `colortheme_options()`, options to be defined in `name = value`
  form, a single named list of options, or character strings specifying
  option names to retrieve.

## Value

- `colorthemes()`: A data frame with columns `"name"`, `"source"`, and
  `"type"`.

- `register_colortheme()`: Invisibly returns the metadata of the
  previous theme that was overwritten (or `NULL` if none existed).

- `colortheme_options()`: A named list of current option values (when
  called with no arguments or character strings), or invisibly a named
  list of previous values (when setting options).

## Details

`colorthemes()` extracts metadata from the registry environment and
structures it into a data frame, allowing users to discover themes by
`name`, `source`, and `type` (with case-insensitive prefix matching).

`register_colortheme()` takes a color vector, a color-generating
function, or an existing `"colortheme"` object and registers it under a
specified `name` and `source` (defaulting to `"custom/newtheme"`). To
keep the registry lightweight, the `kernel` argument supports lazy
loading: pass a character vector or list of length 2 where the first
element is an R expression returning a kernel (e.g., `"rainbow"`) and
the second is the namespace (e.g., `"grDevices"`).

`colortheme_options()` provides a scoped interface to R's global
[`options()`](https://rdrr.io/r/base/options.html), automatically
prefixing option names with `colortheme.`. Supported options are
`"sequential"`, `"diverging"`, `"qualitative"` (default theme names or
objects), and `"registry"` (the active registry environment).

## See also

[`colortheme`](https://ryo-asashi.github.io/midr/reference/colortheme.md)
