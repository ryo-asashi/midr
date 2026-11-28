# Deprecated Functions in midr

These functions are provided for compatibility with older versions of
midr (and downstream packages transitioning to midr 0.7.0) and will be
removed in a future release.

- `mid.importance()`: use
  [`eval_importance()`](https://ryo-asashi.github.io/midr/reference/eval_importance.md).

- `mid.breakdown()`: use
  [`eval_breakdown()`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md).

- `mid.conditional()`: use
  [`eval_conditional()`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md).

- `theme_midr()`: use
  [`theme_monograph()`](https://ryo-asashi.github.io/midr/reference/theme_monograph.md).

- `par.midr()`: use
  [`par_monograph()`](https://ryo-asashi.github.io/midr/reference/par_monograph.md).

- `mid.plots()`: use
  [`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
  or [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Usage

``` r
mid.importance(...)

mid.breakdown(...)

mid.conditional(...)

theme_midr(...)

par.midr(...)

mid.plots(...)
```

## Arguments

- ...:

  Additional arguments passed to the replacement function.

## Value

The return value of the corresponding replacement function.

## Details

Deprecated Functions in midr
