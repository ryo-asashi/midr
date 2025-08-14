# midr 0.5.0.906

- Fixed the issue of `numeric.encoder()` and `factor.encoder()` that they holds a reference to the execution environment of the `interpret.default()` as their caller environment.

# midr 0.5.0.905

- Rewrote `interpret.formula()` and `factor.encoder()` to make `subset` and `drop.unused.levels` available.
- Updated some methods of `get.yhat()` to enforce them to return predictions of the same length as the number of observations.

# midr 0.5.0.904

- Updated `interpret()` to improve computational efficiency of interpolating parameters.

# midr 0.5.0.903

- Fixed a bug in `interpret.default()` that caused inconsistency between "fitted.values" and "residuals.
- Updated `interpret()` to change the automatic determination method of 'k'.
- Added support for "type" suffixes ("_s": as sequential, "_q": as qualitative, "_d": as diverging) in color.theme().
- Improved `mid.f()` to correctly handle vector recycling when an input's length is 1.
- Fixed `autoplot.mid.conditional()` to avoid doubly evaluation of the "mid" object.

# midr 0.5.0.902

- Rewrote `interpret.formula()` to make it able to be called in another function: resolve problems caused by `stats::model.frame()`
- Added new tests for the above feature.
- Refined the diverging color theme "midr".

# midr 0.5.0.901

- Fix "HCL" color theme to turn it to be a "qualitative" color theme.
- Change rounding method of numeric "encoder" objects.
- Update an article and example related to `color.theme()`.

# midr 0.5.0.900

- Enable to change default color themes as options: `midr.diverging`, `midr.qualitative` and `midr.sequential`.
- Add the "HCL" color theme and set it as the default qualitative theme.
- Fix `interpret.formula()` to store the evaluated `formula` in the function call.

# midr 0.5.0

First release on CRAN.

# midr 0.4.9.909

-   Working paper is now available on arXiv.
-   Updates for final adjustments.
-   Some requirements for the 'x' argument are removed or relaxed.

# midr 0.4.9.908

-   Fix small bugs including one reported in <https://github.com/ryo-asashi/midr/issues/5>.
-   Change the default values of `k` to avoid singular fits with smaller datasets.

# midr 0.4.9.907

-   `interpret()` now includes a new `verbosity` argument for logging.
-   Fix small bugs.

# midr 0.4.9.906

-   **Important:** Update the behavior of `get.yhat()` for classification tasks: If the model returns a matrix or data.frame of class probabilities, by default, `get.yhat()` returns the probability of *not* being the base level.

# midr 0.4.9.900-0.4.9.905

-   `ggmid.mid.conditional()` and `plot.mid.conditional()` now include a new argument `reference`, which allows setting the reference point of c-ICE plot to any of the sample points.
-   `color.theme()` now includes a new argument, `pkg`, for package specification.
-   A new article about "color.theme" objects is added.
-   A small change of `print.mid()` and `print.mid.conditional()`.
-   **Important:** Update and chane the behavior of `interpret()` for classification tasks: If `y` is a factor or character, `interpret()` convert its base level to `0` and all other levels to `1`.
-   Modify `ggmid()` and `plot.mid()` to correct effect plots for factor variables with a `catchall` level. Additionallym `ggmid()` now utilize `ggplot2::geom_jitter()` and allow for adjustable jitter amounts with the `jitter` argument. Additionally, when data is not explicitly provided, it is now automatically extracted from the function call stored in the `"mid"` object.
-   Adjust `mid.conditional()` and `mid.breakdown()` so they no longer require explicit data input.
-   Update terminology.
-   The color palettes of the `khroma` package are now available for `color.theme()`.
-   The default theme for `ggmid(type = "data")` and `plot(type = "data")` is changed to a sequential color scheme: "bluescale".
-   Add link functions: `translogit`, `transprobit`, `identity-logistic` and `identity-gaussian` for the interpretation task of classification models.
-   `interpret()` now interactively confirms whether a singular fit or exceeding the maximum number of columns is an error.

# midr 0.4.3-0.4.8.909

-   Add `mid.ur()` to extract uninterpreted ratio (rate) more conveniently.
-   Modify weighted loss functions to compute deviation-from-mean based losses for one input.
-   Modify `ggmid.mid.breakdown()` , `ggmid.mid.importance()` and `plot.mid.breakdown()` to improve usability of the functions.
-   Modify `interpret()` to add the `pred.args` argument that can be used to pass optional arguments to the prediction function (`pred.fun()`).
-   Add a draft of the article on "the interpretation of classification models".
-   Modify `interpret()` to allow matrices to be used as valid inputs for `data` (`interpret.formula()`) and `x` (`interpret.default()`).
-   Add an article on "the interpretation of regression models".
-   Define `print.encoder()` for "encoder" objects to improve usability of the fitted MID models.
-   Update README.
-   Change default values of some arguments: e.g. `na.action = na.pass` of `predict.mid()`.
-   Fix small bugs of `plot.mid.conditional()`.
-   Update `interpret()` to ensure that the value of the argument `link` is a character string.
-   Update `ggmid.mid.conditional()` and `plot.mid.conditional()`: `var.color` and the similar arguments can take an expression as input.

# midr 0.4.3

midr is in the release process. We will submit the package to CRAN by mid-January 2025 .

-   Improving usability of graphing functions.
-   Finalizing code and documentation.
-   Writing some vignettes. [Submitted to JARIP BULLETIN]
