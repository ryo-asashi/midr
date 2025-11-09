# Weighted Tabulation for Vectors

`weighted.tabulate()` returns the sum of weights for each integer in the
vector `bin`.

## Usage

``` r
weighted.tabulate(bin, w = NULL, nbins = max(1L, bin, na.rm = TRUE))
```

## Arguments

- bin:

  a numeric vector of positive integers, or a factor.

- w:

  a numeric vector of the sample weights for each value in `bin`.

- nbins:

  the number of bins to be used.

## Value

`weighted.tabulate()` returns a numeric vector.

## Details

`weighted.tabulate()` is a wrapper function of
[`tabulate()`](https://rdrr.io/r/base/tabulate.html) to reflect sample
weights. This function is used in
[`factor.encoder()`](https://ryo-asashi.github.io/midr/reference/factor.encoder.md)
to enable weights-based encoding.

## See also

[`weighted.quantile`](https://ryo-asashi.github.io/midr/reference/weighted.quantile.md)
