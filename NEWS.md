## midr 0.5.0 (TBR)

[First release on CRAN.]

## midr 0.4.8.909

-   Add `mid.ur()` to extract uninterpreted ratio (rate) more conveniently.

-   Modify weighted loss functions to compute deviation-from-mean based losses for one input.

## midr 0.4.8.908

-   Modify `ggmid.mid.breakdown()` , `ggmid.mid.importance()` and `plot.mid.breakdown()` to improve usability of the functions.

## midr 0.4.8.907

-   Modify `interpret()` to add the `pred.args` argument that can be used to pass optional arguments to the prediction function (`pred.fun()`).

-   Add a draft of the article on "the interpretation of classification models".

## midr 0.4.8.906

-   Modify `interpret()` to allow matrices to be used as valid inputs for `data` (`interpret.formula()`) and `x` (`interpret.default()`).

-   Add an article on "the interpretation of regression models".

## midr 0.4.8.905

-   Define `print.encoder()` for "encoder" objects to improve usability of the fitted MID models.

-   Update README.

## midr 0.4.8.904

-   Change default values of some arguments: e.g. `na.action = na.pass` of `predict.mid()`.

-   Fix small bugs of `plot.mid.conditional()`.

## midr 0.4.8.903

-   Update `interpret()` to ensure that the value of the argument `link` is a character string.

-   Update `ggmid.mid.conditional()` and `plot.mid.conditional()`: `var.color` and the similar arguments can take an expression as input.

## midr 0.4.3-0.4.8.902

midr is in the release process. We will submit the package to CRAN by mid-January 2025 .

-   Improving usability of graphing functions.

-   Finalizing code and documentation.

-   Writing some vignettes. [Submitted to JARIP BULLETIN]
