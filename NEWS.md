# midr 0.5.1

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
