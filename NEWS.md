## midr 0.5.0 (TBR)

[First release on CRAN.]

## midr 0.4.9.908

-   Fix small bugs including reported in <https://github.com/ryo-asashi/midr/issues/5>.
-   Change the default values of `k` to avoid singular fits with smaller datasets.

## midr 0.4.9.907

-   `interpret()` now includes a new `verbosity` argument for logging.
-   Fix small bugs.

## midr 0.4.9.906

-   **Important:** Update the behavior of `get.yhat()` for classification tasks: If the model returns a matrix or data.frame of class probabilities, by default, `get.yhat()` returns the probability of *not* being the base level.

## midr 0.4.9.900-0.4.9.905

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

## midr 0.4.3-0.4.8.909

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

## midr 0.4.3

midr is in the release process. We will submit the package to CRAN by mid-January 2025 .

-   Improving usability of graphing functions.
-   Finalizing code and documentation.
-   Writing some vignettes. [Submitted to JARIP BULLETIN]
