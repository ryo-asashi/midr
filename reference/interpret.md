# Fit MID Models

`interpret()` is used to fit a Maximum Interpretation Decomposition
(MID) model. MID models are additive, highly interpretable models
composed of functions, each with up to two variables.

## Usage

``` r
interpret(object, ...)

# Default S3 method
interpret(
  object,
  x,
  y = NULL,
  weights = NULL,
  pred.fun = get.yhat,
  link = NULL,
  k = c(NA, NA),
  type = c(1L, 1L),
  frames = list(),
  interactions = FALSE,
  terms = NULL,
  singular.ok = FALSE,
  mode = 1L,
  method = NULL,
  lambda = 0,
  kappa = 1e+06,
  na.action = getOption("na.action"),
  verbosity = 1L,
  encoding.digits = 3L,
  use.catchall = FALSE,
  catchall = "(others)",
  max.ncol = 10000L,
  nil = 1e-07,
  tol = 1e-07,
  pred.args = list(),
  ...
)

# S3 method for class 'formula'
interpret(
  formula,
  data = NULL,
  model = NULL,
  pred.fun = get.yhat,
  weights = NULL,
  subset = NULL,
  na.action = getOption("na.action"),
  verbosity = 1L,
  mode = 1L,
  drop.unused.levels = FALSE,
  pred.args = list(),
  ...
)
```

## Arguments

- object:

  a fitted model object to be interpreted.

- ...:

  optional arguments. For `interpret.formula()`, arguments to be passed
  on to `interpret.default()`. For `interpret.default()`, `...` can
  include convenient aliases (e.g., "ok" for `singular.ok`, "ie" for
  `interactions`) as well as several advanced fitting options (see the
  "Advanced Fitting Options" section for details).

- x:

  a matrix or data.frame of predictor variables to be used in the
  fitting process. The response variable should not be included.

- y:

  an optional numeric vector of the model predictions or the response
  variable.

- weights:

  a numeric vector of sample weights for each observation in `x`.

- pred.fun:

  a function to obtain predictions from a fitted model, where the first
  argument is for the fitted model and the second argument is for new
  data. The default is
  [`get.yhat()`](https://ryo-asashi.github.io/midr/reference/get.yhat.md).

- link:

  a character string specifying the link function: one of "logit",
  "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2",
  "inverse", "translogit", "transprobit", "identity-logistic" and
  "identity-gaussian", or an object containing two functions `linkfun()`
  and `linkinv()`. See
  [`help(make.link)`](https://rdrr.io/r/stats/make.link.html).

- k:

  an integer or a vector of two integers specifying the maximum number
  of sample points for main effects (`k[1]`) and interactions (`k[2]`).
  If a single integer is provided, it is used for main effects while the
  value for interactions is automatically determined. Any `NA` value
  will also trigger this automatic determination. With non-positive
  values, all unique data points are used as sample points.

- type:

  an integer or integer-valued vector of length two. The type of
  encoding. The effects of quantitative variables are modeled as
  piecewise linear functions if `type` is `1`, and as step functions if
  `type` is `0`. If a vector is passed, `type[1L]` is used for main
  effects and `type[2L]` is used for interactions.

- frames:

  a named list of encoding frames ("numeric.frame" or "factor.frame"
  objects). The encoding frames are used to encode the variable of the
  corresponding name. If the name begins with "\|" or ":", the encoding
  frame is used only for main effects or interactions, respectively.

- interactions:

  logical. If `TRUE` and if `terms` and `formula` are not supplied, all
  interactions for each pair of variables are modeled and calculated.

- terms:

  a character vector of term labels or formula, specifying the set of
  component functions to be modeled. If not passed, `terms` includes all
  main effects, and all second-order interactions if `interactions` is
  `TRUE`.

- singular.ok:

  logical. If `FALSE`, a singular fit is an error.

- mode:

  an integer specifying the method of calculation. If `mode` is `1`, the
  centralization constraints are treated as penalties for the least
  squares problem. If `mode` is `2`, the constraints are used to reduce
  the number of free parameters.

- method:

  an integer specifying the method to be used to solve the least squares
  problem. A non-negative value will be passed to
  [`RcppEigen::fastLmPure()`](https://rdrr.io/pkg/RcppEigen/man/fastLm.html).
  If negative, [`stats::lm.fit()`](https://rdrr.io/r/stats/lmfit.html)
  is used.

- lambda:

  the penalty factor for pseudo smoothing. The default is `0`.

- kappa:

  the penalty factor for centering constraints. Used only when `mode` is
  `1`. The default is `1e+6`.

- na.action:

  a function or character string specifying the method of `NA` handling.
  The default is "na.omit".

- verbosity:

  the level of verbosity. `0`: fatal, `1`: warning (default), `2`: info
  or `3`: debug.

- encoding.digits:

  an integer. The rounding digits for encoding numeric variables. Used
  only when `type` is `1`.

- use.catchall:

  logical. If `TRUE`, less frequent levels of qualitative variables are
  dropped and replaced by the catchall level.

- catchall:

  a character string specifying the catchall level.

- max.ncol:

  integer. The maximum number of columns of the design matrix.

- nil:

  a threshold for the intercept and coefficients to be treated as zero.
  The default is `1e-7`.

- tol:

  a tolerance for the singular value decomposition. The default is
  `1e-7`.

- pred.args:

  optional parameters other than the fitted model and new data to be
  passed to `pred.fun()`.

- formula:

  a symbolic description of the MID model to be fit.

- data:

  a data.frame, list or environment containing the variables in
  `formula`. If not found in data, the variables are taken from
  `environment(formula)`.

- model:

  a fitted model object to be interpreted.

- subset:

  an optional vector specifying a subset of observations to be used in
  the fitting process.

- drop.unused.levels:

  logical. If `TRUE`, unused levels of factors will be dropped.

## Value

`interpret()` returns an object of class "mid". This is a list with the
following components:

- weights:

  a numeric vector of the sample weights.

- call:

  the matched call.

- terms:

  the [`terms.object`](https://rdrr.io/r/stats/terms.object.html) used.

- link:

  a "link-glm" or "link-midr" object containing the link function.

- intercept:

  the intercept.

- encoders:

  a list of variable encoders.

- main.effects:

  a list of data frames representing the main effects.

- interacions:

  a list of data frames representing the interactions.

- ratio:

  the ratio of the sum of squared error between the target model
  predictions and the fitted MID values, to the sum of squared
  deviations of the target model predictions.

- linear.predictors:

  a numeric vector of the linear predictors.

- fitted.values:

  a numeric vector of the fitted values.

- residuals:

  a numeric vector of the working residuals.

- na.action:

  information about the special handling of `NA`s.

## Details

The MID model approximates a target model's prediction function
\\f(\mathbf{x})\\, or values of the response variable \\\mathbf{y}\\.
This model, denoted as \\\mathcal{F}(\mathbf{x})\\, has the following
structure: \$\$\mathcal{F}(\mathbf{x}) = f\_\phi + \sum\_{j}
f\_{j}(x_j) + \sum\_{j\<k} f\_{jk}(x_j, x_k)\$\$ where \\f\_\phi\\ is
the intercept, \\f\_{j}(x_j)\\ is the main effect of feature \\j\\, and
\\f\_{jk}(x_j, x_k)\\ is the second-order interaction effect between
features \\j\\ and \\k\\.

To ensure that the decomposed components are unique, they are fitted
under the *centering constraints*: each main effect's average is
constrained to be zero, and each interaction effect's conditional
averages are also constrained to be zero. The model is fitted by
minimizing the squared error between the target, \\f(\mathbf{x})\\ or
\\\mathbf{y}\\, and the surrogate \\\mathcal{F}(\mathbf{x})\\, which is
typically evaluated on a representative dataset.

## Advanced Fitting Options

The `...` argument can be used to pass several advanced fitting options:

- fit.intercept:

  logical. If `TRUE`, the intercept term is fitted as part of the least
  squares problem. If `FALSE` (default), it is calculated as the
  weighted mean of the response.

- interpolate.beta:

  a character string specifying the method for interpolating unestimable
  coefficients (betas) that arise from sparse data regions. Can be
  "iterative" for an iterative smoothing process, "direct" for solving a
  linear system, or "none" to disable interpolation.

- maxit:

  an integer specifying the maximum number of iterations for the
  "iterative" interpolation method.

- weighted.norm:

  logical. If `TRUE`, the columns of the design matrix are normalized by
  the square root of their weighted sum. This is required to ensure the
  minimum-norm least squares solution obtained by appropriate methods
  (i.e., `4` or `5`) of `fastLmPure()` is the minimum-norm solution in a
  *weighted* sense.

- weighted.encoding:

  logical. If `TRUE`, sample weights are used during the encoding
  process (e.g., for calculating quantiles to determine knots).

## References

Asashiba R, Kozuma R, Iwasawa H (2025). “midr: Learning from Black-Box
Models by Maximum Interpretation Decomposition.” 2506.08338,
<https://arxiv.org/abs/2506.08338>.

## See also

[`print.mid`](https://ryo-asashi.github.io/midr/reference/print.mid.md),
[`summary.mid`](https://ryo-asashi.github.io/midr/reference/summary.mid.md),
[`predict.mid`](https://ryo-asashi.github.io/midr/reference/predict.mid.md),
[`plot.mid`](https://ryo-asashi.github.io/midr/reference/plot.mid.md),
[`ggmid`](https://ryo-asashi.github.io/midr/reference/ggmid.md),
[`mid.plots`](https://ryo-asashi.github.io/midr/reference/mid.plots.md),
[`mid.effect`](https://ryo-asashi.github.io/midr/reference/mid.effect.md),
[`mid.terms`](https://ryo-asashi.github.io/midr/reference/mid.terms.md),
[`mid.importance`](https://ryo-asashi.github.io/midr/reference/mid.importance.md),
[`mid.conditional`](https://ryo-asashi.github.io/midr/reference/mid.conditional.md),
[`mid.breakdown`](https://ryo-asashi.github.io/midr/reference/mid.breakdown.md)

## Examples

``` r
# Fit a MID model as a surrogate for another model
data(cars, package = "datasets")
model <- lm(dist ~ I(speed^2) + speed, cars)
mid <- interpret(dist ~ speed, cars, model)
plot(mid, "speed", intercept = TRUE)
points(cars)


# Fit a MID model as a standalone predictive model
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, data = airquality, lambda = .5)
#> 'model' not passed: response variable in 'data' is used
plot(mid, "Wind")

plot(mid, "Temp")

plot(mid, "Wind:Temp", main.effects = TRUE)


data(Nile, package = "datasets")
nile <- data.frame(time = 1:length(Nile), flow = as.numeric(Nile))

# A flexible fit with many knots
mid <- interpret(flow ~ time, data = nile, k = 100L)
#> 'model' not passed: response variable in 'data' is used
plot(mid, "time", intercept = TRUE, limits = c(600L, 1300L))
points(x = 1L:100L, y = Nile)


# A smoother fit with fewer knots
mid <- interpret(flow ~ time, data = nile, k = 10L)
#> 'model' not passed: response variable in 'data' is used
plot(mid, "time", intercept = TRUE, limits = c(600L, 1300L))
points(x = 1L:100L, y = Nile)


# A pseudo-smoothed fit using a penalty
mid <- interpret(flow ~ time, data = nile, k = 100L, lambda = 100L)
#> 'model' not passed: response variable in 'data' is used
plot(mid, "time", intercept = TRUE, limits = c(600L, 1300L))
points(x = 1L:100L, y = Nile)
```
