# Changelog

## midr 0.5.3.904

- Fixed memory issues in
  [`get.link()`](https://ryo-asashi.github.io/midr/reference/get.link.md)
  and added support for parametric link functions (including Box-Cox,
  Yeo-Johnson, Robit, and Scobit).

## midr 0.5.3.903

- Multivariate Response Support:
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  now accepts a matrix for the response variable `y`, allowing for
  simultaneous modeling of multiple responses.
- Introduction of “midlist”: Implemented the “midlist” class to handle
  multiple MID models efficiently. Methods such as
  [`predict()`](https://rdrr.io/r/stats/predict.html) and
  [`mid.effect()`](https://ryo-asashi.github.io/midr/reference/mid.effect.md)
  have been vectorized to handle “midlist” objects using optimized
  matrix operations.
- Performance Optimization: Significant reduction in compilation time
  and binary size by removing the heavy dependency on **RcppEigen**
  headers for the core Laplacian smoothing algorithm.

## midr 0.5.3.902

- The `digits` argument of
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  now defaults to `NULL`.
- [`get.yhat()`](https://ryo-asashi.github.io/midr/reference/get.yhat.md)
  has new method for `workflow` objects.

## midr 0.5.3.901

- Updated
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  and encoder generators to enhance the variable encoding functionality,
  introducing new arguments: `split` for numeric variables and `lump`
  for factor variables.
- The `catchall` and `encoding.digits` arguments are renamed to `others`
  and `digits`; the `use.catchall` argument is deprecated.

## midr 0.5.3.900

- Fixed incorrect calculation in
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  when `singular.ok = TRUE` (or, more directly, `weighted.norm = TRUE`)
  and `lambda > 0`.
- Modified weights calculation to enhance the numerical stability.

## midr 0.5.3

CRAN release: 2026-01-16

Fourth release on CRAN. This version introduces significant memory
efficiency improvements for large-scale data analysis.

#### Major Improvements

- Optimized fitting process: Enhanced the space (and time) complexity
  when constructing large design matrices, especially for datasets with
  many observations.
- Memory-efficient estimation: Introduced the `save.memory` option in
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md).
- Improved design matrix constraints: Replaced the `max.ncol` argument
  with `max.nelements` in
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  to provide more intuitive control over the memory consumption of the
  design matrix.

#### Other Enhancements

- Memory-free prediction engine: Re-implemented the prediction logic to
  avoid storing massive term effect matrices, significantly reducing the
  memory usage of
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  and
  [`predict.mid()`](https://ryo-asashi.github.io/midr/reference/predict.mid.md).
- Flexible formatting: `plot.mid.breakdown()` and
  `ggmid.mid.breakdown()` now support `format.args` and enhanced
  `label.format` for better visualization control.
- CRAN compatibility: Updated various internal functions to ensure
  consistent behavior with the ‘stats’ package (especially with
  [`stats::terms()`](https://rdrr.io/r/stats/terms.html)) and improved
  documentation clarity.

## midr 0.5.2.907

- Updated
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  to improve space and time complexity of constructing the design
  matrix.

## midr 0.5.2.906

- The `format` argument in
  [`mid.breakdown()`](https://ryo-asashi.github.io/midr/reference/mid.breakdown.md)
  is deprecated.
- `plot.mid.breakdown()` and `ggmid.mid.breakdown()` now have a new
  argument `format.args`, which is passed to
  [`base::format()`](https://rdrr.io/r/base/format.html) to format the
  predictor values stored in “mid.breakdown” objects.
- The `format` argument in `plot.mid.breakdown()` and
  `ggmid.mid.breakdown()` is renamed to `label.format`. The formatting
  strings now support more flexible formats, such as “%t=%v, %t=%v” for
  interactions.

## midr 0.5.2.905

- Updated `ggmid.mid.importance()` and `plot.mid.importance()` to modify
  appearance of the plots when color themes are applied.

## midr 0.5.2.904

- Memory-efficient
  [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md):
  The model object no longer stores the massive `fitted.matrix` (the
  term-wise decomposition of the fitted values).
- Optimized [`predict()`](https://rdrr.io/r/stats/predict.html) engine:
  Re-implemented the prediction logic using a matrix-free approach.
- On-demand Decomposition: Functions like
  [`mid.importance()`](https://ryo-asashi.github.io/midr/reference/mid.importance.md)
  now perform term-wise decomposition on-the-fly using the new optimized
  prediction engine.
- [`mid.importance()`](https://ryo-asashi.github.io/midr/reference/mid.importance.md)
  introduced a new argument `max.nkeeps` (default: 10,000). While
  importance scores are calculated using the full dataset for maximum
  accuracy, the function now optionally retains only a weighted random
  sample of the term-wise predictions.
- Standardized `predict` outputs: For `type = "terms"`, the intercept is
  now stored in the `constant` attribute of the returned matrix,
  aligning with standard R conventions (e.g., `predict.lm`).
- Removed the redundant `fitted.matrix` reference in `interpret.default`
  to prevent memory leaks during the estimation process.

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
- Fixed `autoplot.mid.conditional()` to avoid redundant evaluation of
  the “mid” object.
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
