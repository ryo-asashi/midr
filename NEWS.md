# midr 0.5.2.906

-   The `format` argument in `mid.breakdown()` is deprecated.
-   `plot.mid.breakdown()` and `ggmid.mid.breakdown()` now have a new argument `format.args`, which is passed to `base::format()` to format the predictor values stored in "mid.breakdown" objects.
-   The `format` argument in `plot.mid.breakdown()` and `ggmid.mid.breakdown()` is renamed to `label.format`. The formatting strings now support more flexible formats, such as "%t=%v, %t=%v" for interactions.

# midr 0.5.2.905

-   Updated `ggmid.mid.importance()` and `plot.mid.importance()` to modify appearance of the plots when color themes are applied.

# midr 0.5.2.904

-   Memory-efficient `interpret()`: The model object no longer stores the massive `fitted.matrix` (the term-wise decomposition of the fitted values).
-   Optimized `predict()` engine: Re-implemented the prediction logic using a matrix-free approach.
-   On-demand Decomposition: Functions like `mid.importance()` now perform term-wise decomposition on-the-fly using the new optimized prediction engine.
-   `mid.importance()` introduced a new argument `max.nkeeps` (default: 10,000). While importance scores are calculated using the full dataset for maximum accuracy, the function now optionally retains only a weighted random sample of the term-wise predictions.
-   Standardized `predict` outputs: For `type = "terms"`, the intercept is now stored in the `constant` attribute of the returned matrix, aligning with standard R conventions (e.g., `predict.lm`).
-   Removed the redundant `fitted.matrix` reference in `interpret.default` to prevent memory leaks during the estimation process.

# midr 0.5.2.903

-   `interpret.formula()` now supports unevaluated column names for the `weights` argument.

# midr 0.5.2.902

-   `weighted.loss()` supports the R-squared metrics by passing `method = "r2"`.

# midr 0.5.2.901

-   Enhanced plotting functions for "mid.importance" objects, allowing users to restrict which terms are displayed.
-   Fixed a bug that occurred when a link function is used with a response variable containing `NA` values.

# midr 0.5.2.900

-   Updated functions to enhance consistency with the 'stats' package, especially with regard to the return value for the `stats::terms()` function.

# midr 0.5.2

Third release on CRAN.

# midr 0.5.1.901

-   Corrected typos and improved clarity in the documentation.

# midr 0.5.1.900

-   Changed some argument names for consistency (`max.nterms`, `max.nplots`, `max.nrow`).

# midr 0.5.1

Second release on CRAN.

# midr 0.5.0.909

-   Vectorized the design matrix encoding process in `factor.encoder()` and `numeric.encoder()` for improved performance.
-   Unified `weighted.rmse()` and its related functions into a single, more versatile `weighted.loss()` function.
-   Updated and improved various help documents for clarity and completeness.
-   Changed some argument names for consistency (e.g., changed `max.bars` to `max.terms`, `max.nrow` to `max.rows`, etc.).
-   Deprecated `weighted()` and its family functions.
-   Deprecated `mid.extract()` and `mid.frames()`.

# midr 0.5.0.908

-   Rewrote `color.theme()` to significantly enhance its functionality and flexibility.

# midr 0.5.0.906

-   Fixed a memory leak issue where `numeric.encoder()` and `factor.encoder()` held an unnecessary reference to the execution environment of `interpret.default()`.
-   Updated the hex logo and favicons.

# midr 0.5.0.905

-   Rewrote `interpret.formula()` and `factor.encoder()` to correctly support `subset` and `drop.unused.levels` arguments.
-   Updated `get.yhat()` methods to ensure prediction outputs always have the same length as the number of input observations.

# midr 0.5.0.904

-   Improved the computational efficiency of the parameter interpolation step in `interpret.default()`.

# midr 0.5.0.903

-   Fixed a bug in `interpret.default()` that caused inconsistency between "fitted.values" and "residuals".
-   Fixed an issue in `mid.f()` (`mid.effect()`) to correctly handle vector recycling when an input's length is 1.
-   Fixed `autoplot.mid.conditional()` to avoid redundant evaluation of the "mid" object.
-   Updated the automatic determination method for the number of knots (`k`) in `interpret()`.
-   Added support for type suffixes in `color.theme()` for easier theme specification.

# midr 0.5.0.902

-   Rewrote `interpret.formula()` to resolve environment issues related to `stats::model.frame()`
-   Added new tests to cover this improved functionality.
-   Refined the default diverging color theme "midr".

# midr 0.5.0.901

-   Corrected the "HCL" color theme to be properly categorized as a "qualitative" theme.
-   Changed the rounding method used in numeric "encoder" objects.
-   Updated the article and examples related to `color.theme()`.

# midr 0.5.0.900

-   Default color themes can now be set globally via R options: `midr.diverging`, `midr.qualitative` and `midr.sequential`.
-   Added the "HCL" color theme and set it as the new default for "qualitative" theme.
-   Fixed an issue in `interpret.formula()` to ensure the evaluated `formula` is correctly stored in the function call.

# midr 0.5.0

First release on CRAN.

# midr 0.4.9.909

-   The accompanying working paper is now available on arXiv.
