# Weighted Tabulation for Vectors

`weighted_tabulate()` returns the sum of weights for each integer in the
vector `bin`.

## Usage

``` r
weighted_tabulate(bin, w = NULL, nbins = max(1L, bin, na.rm = TRUE))
```

## Arguments

- bin:

  a numeric vector of positive integers, or a factor.

- w:

  a numeric vector of the sample weights for each value in `bin`.

- nbins:

  the number of bins to be used.

## Value

`weighted_tabulate()` returns a numeric vector.

## Details

`weighted_tabulate()` is a wrapper function of
[`tabulate()`](https://rdrr.io/r/base/tabulate.html) to reflect sample
weights. This function is used in
[`make_encoder()`](https://ryo-asashi.github.io/midr/reference/make_encoder.md)
to enable weights-based encoding.

## See also

[`weighted_quantile`](https://ryo-asashi.github.io/midr/reference/weighted_quantile.md)
