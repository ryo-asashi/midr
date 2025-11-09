# Weighted Sample Quantile

`weighted.quantile()` produces weighted sample quantiles corresponding
to the given probabilities.

## Usage

``` r
weighted.quantile(
  x,
  w = NULL,
  probs = seq(0, 1, 0.25),
  na.rm = FALSE,
  names = TRUE,
  digits = 7L,
  type = 1L,
  ...
)
```

## Arguments

- x:

  a numeric vector whose weighted sample quantiles are wanted.

- w:

  a numeric vector of the sample weights for each value in `x`.

- probs:

  a numeric vector of probabilities with values in `[0, 1]`.

- na.rm:

  logical. If `TRUE`, any `NA` and `NaN`s are removed from `x` before
  the quantiles are computed.

- names:

  logical. If `TRUE`, the result has a "names" attribute.

- digits:

  used only when `names` is `TRUE`. The precision to use when formatting
  the percentages.

- type:

  an integer between `1` and `9` selecting the quantile algorithms. Only
  `1` is available for the weighted quantile.

- ...:

  further arguments passed to
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) when
  weights are not passed.

## Value

`weighted.quantile()` returns weighted sample quantiles corresponding to
the given probabilities.

## Details

`weighted.quantile()` is a wrapper function of
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) for
weighted quantiles. For the weighted quantile, only the "type 1"
quantile, the inverse of the empirical distribution function, is
available. This function is used in
[`numeric.encoder()`](https://ryo-asashi.github.io/midr/reference/numeric.encoder.md)
to enable weights-based encoding.

## See also

[`weighted.tabulate`](https://ryo-asashi.github.io/midr/reference/weighted.tabulate.md)
