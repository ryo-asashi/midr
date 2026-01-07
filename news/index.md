# Changelog

## midr 0.5.2.903

- [`interpret.formula()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  now supports unevaluated column names for the `weights` argument.

## midr 0.5.2.902

- [`weighted.loss()`](https://ryo-asashi.github.io/midr/reference/weighted.loss.md)
  supports the R-squared metrics by passing `method = "r2"`.

## midr 0.5.2.901

- Enhanced plotting functions for “mid.importance” objects, allowing
  users to restrict which terms are displayed.
- Fixed a bug that occurred when a link function is used with a response
  variable containing `NA` values.

## midr 0.5.2.900

- Updated functions to enhance consistency with the ‘stats’ package,
  especially with regard to the return value for the
  [`stats::terms()`](https://rdrr.io/r/stats/terms.html) function.

## midr 0.5.2

CRAN release: 2025-09-07

Third release on CRAN.

## midr 0.5.1.901

- Corrected typos and improved clarity in the documentation.

## midr 0.5.1.900

- Changed some argument names for consistency (`max.nterms`,
  `max.nplots`, `max.nrow`).

## midr 0.5.1

CRAN release: 2025-08-27

Second release on CRAN.

## midr 0.5.0.909

- Vectorized the design matrix encoding process in
  [`factor.encoder()`](https://ryo-asashi.github.io/midr/reference/factor.encoder.md)
  and
  [`numeric.encoder()`](https://ryo-asashi.github.io/midr/reference/numeric.encoder.md)
  for improved performance.
- Unified `weighted.rmse()` and its related functions into a single,
  more versatile
  [`weighted.loss()`](https://ryo-asashi.github.io/midr/reference/weighted.loss.md)
  function.
- Updated and improved various help documents for clarity and
  completeness.
- Changed some argument names for consistency (e.g., changed `max.bars`
  to `max.terms`, `max.nrow` to `max.rows`, etc.).
- Deprecated `weighted()` and its family functions.
- Deprecated `mid.extract()` and `mid.frames()`.

## midr 0.5.0.908

- Rewrote
  [`color.theme()`](https://ryo-asashi.github.io/midr/reference/color.theme.md)
  to significantly enhance its functionality and flexibility.

## midr 0.5.0.906

- Fixed a memory leak issue where
  [`numeric.encoder()`](https://ryo-asashi.github.io/midr/reference/numeric.encoder.md)
  and
  [`factor.encoder()`](https://ryo-asashi.github.io/midr/reference/factor.encoder.md)
  held an unnecessary reference to the execution environment of
  [`interpret.default()`](https://ryo-asashi.github.io/midr/reference/interpret.md).
- Updated the hex logo and favicons.

## midr 0.5.0.905

- Rewrote
  [`interpret.formula()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  and
  [`factor.encoder()`](https://ryo-asashi.github.io/midr/reference/factor.encoder.md)
  to correctly support `subset` and `drop.unused.levels` arguments.
- Updated
  [`get.yhat()`](https://ryo-asashi.github.io/midr/reference/get.yhat.md)
  methods to ensure prediction outputs always have the same length as
  the number of input observations.

## midr 0.5.0.904

- Improved the computational efficiency of the parameter interpolation
  step in
  [`interpret.default()`](https://ryo-asashi.github.io/midr/reference/interpret.md).

## midr 0.5.0.903

- Fixed a bug in
  [`interpret.default()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  that caused inconsistency between “fitted.values” and “residuals”.
- Fixed an issue in
  [`mid.f()`](https://ryo-asashi.github.io/midr/reference/mid.effect.md)
  ([`mid.effect()`](https://ryo-asashi.github.io/midr/reference/mid.effect.md))
  to correctly handle vector recycling when an input’s length is 1.
- Fixed
  [`autoplot.mid.conditional()`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.conditional.md)
  to avoid redundant evaluation of the “mid” object.
- Updated the automatic determination method for the number of knots
  (`k`) in
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md).
- Added support for type suffixes in
  [`color.theme()`](https://ryo-asashi.github.io/midr/reference/color.theme.md)
  for easier theme specification.

## midr 0.5.0.902

- Rewrote
  [`interpret.formula()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  to resolve environment issues related to
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)
- Added new tests to cover this improved functionality.
- Refined the default diverging color theme “midr”.

## midr 0.5.0.901

- Corrected the “HCL” color theme to be properly categorized as a
  “qualitative” theme.
- Changed the rounding method used in numeric “encoder” objects.
- Updated the article and examples related to
  [`color.theme()`](https://ryo-asashi.github.io/midr/reference/color.theme.md).

## midr 0.5.0.900

- Default color themes can now be set globally via R options:
  `midr.diverging`, `midr.qualitative` and `midr.sequential`.
- Added the “HCL” color theme and set it as the new default for
  “qualitative” theme.
- Fixed an issue in
  [`interpret.formula()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  to ensure the evaluated `formula` is correctly stored in the function
  call.

## midr 0.5.0

CRAN release: 2025-06-23

First release on CRAN.

## midr 0.4.9.909

- The accompanying working paper is now available on arXiv.
