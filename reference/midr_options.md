# Set or Get Global Options for midr

`midr_options()` provides a unified interface to R's global
[`options()`](https://rdrr.io/r/base/options.html) for midr, including
verbosity, plotting backend, default color themes, and custom solvers.

## Usage

``` r
midr_options(...)
```

## Arguments

- ...:

  options specified as `name = value` pairs, a single named list of
  options, character strings to query specific options, or empty to
  retrieve all current midr options.

## Value

When querying options (no arguments or character names), a named list of
current option values. When setting options, invisibly returns a named
list of the previous option values.

## Details

Supported option names (passed without package prefixes) include:

- `verbosity`: integer verbosity level for
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  (default `1L`).

- `message.appendLF`: logical passed to
  [`message()`](https://rdrr.io/r/base/message.html) (default `TRUE`).

- `engine`: default plotting backend, `"graphics"` or `"ggplot2"`.

- `gg.theme`: optional **ggplot2** theme object applied by
  [`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md) /
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

- `sequential`, `diverging`, `qualitative`: default color theme names or
  objects.

- `solver.<name>`: custom OLS solver function `function(x, y)` for
  `interpret(method = "<name>")`.

## Examples

``` r
midr_options()
#> $diverging
#> [1] "azurerose"
#> 
#> $qualitative
#> [1] "hcl"
#> 
#> $registry
#> <environment: 0x5704c5818bc0>
#> 
#> $sequential
#> [1] "bluescale"
#> 
#> $engine
#> [1] "graphics"
#> 
#> $message.appendLF
#> [1] TRUE
#> 
#> $verbosity
#> [1] 1
#> 
old <- midr_options(verbosity = 0L, engine = "ggplot2")
midr_options("verbosity", "engine")
#> $verbosity
#> [1] 0
#> 
#> $engine
#> [1] "ggplot2"
#> 
midr_options(old)
```
