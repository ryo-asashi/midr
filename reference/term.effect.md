# Evaluate Single Component Functions of Additive Models

`term.effect()` calculates the contribution of a single component
function of a fitted additive models. It serves as a low-level helper
function for making predictions or for direct analysis of a term effect.

`term.f()` is a convenient shorthand for `term.effect()`.

## Usage

``` r
term.effect(object, ...)

# S3 method for class 'mid'
term.effect(object, term, x, y = NULL, ...)

# S3 method for class 'mids'
term.effect(object, term, x, y = NULL, ...)

# Default S3 method
term.effect(object, term, x, y = NULL, data = NULL, ...)

term.f(object, ...)
```

## Arguments

- object:

  a "mid" object, a collection of models ("mids"), or other compatible
  additive model objects (e.g., "lm", "glm").

- ...:

  optional arguments to be passed to methods.

- term:

  a character string specifying the component function (term) to
  evaluate.

- x:

  a vector of values for the first variable in the term. If a matrix or
  data frame is provided, values of the related variables are
  automatically extracted from it.

- y:

  a vector of values for the second variable in an interaction term.
  Ignored if `x` is a data frame containing both variables.

- data:

  an optional data frame containing the original training data. Required
  for additive models if the data cannot be automatically extracted from
  the model object.

## Value

`term.effect()` returns a numeric vector of the calculated term
contributions, with the same length as `x`.

For a collection of models ("mids"), `term.effect()` returns a numeric
matrix where each column corresponds to a model.

## Details

`term.effect()` is a low-level S3 generic function designed to calculate
the contribution of a single component function. Unlike
[`predict.mid()`](https://ryo-asashi.github.io/midr/reference/predict.mid.md),
which is designed to return total model predictions, `term.effect()` is
more flexible. It accepts vectors, as well as matrices or data frames,
as input for `x` and `y`. If `x` is a data frame, the necessary columns
are automatically extracted. This makes it particularly useful for
visualizing a component's effect in combination with standard plotting
functions, such as
[`graphics::curve()`](https://rdrr.io/r/graphics/curve.html).

For a main effect, the function evaluates the component function
\\f_j(x_j)\\ for a vector of values \\x_j\\. For an interaction, it
evaluates \\f\_{jk}(x_j, x_k)\\ using vectors \\x_j\\ and \\x_k\\. The
assignment of `x` and `y` strictly follows the order of variables
specified in the `term` argument. For example, if `term = "Temp:Wind"`,
`x` is assigned to `Temp` and `y` is assigned to `Wind`.

## Examples

``` r
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#> 'model' not passed: response variable in 'data' is used

# Visualize the main effect of "Wind"
curve(term.effect(mid, term = "Wind", x), from = 0, to = 25)


# Visualize the interaction of "Wind" and "Temp"
curve(term.f(mid, "Wind:Temp", x, 50), 0, 25)
curve(term.f(mid, "Wind:Temp", x, 60), 0, 25, add = TRUE, lty = 2)
curve(term.f(mid, "Wind:Temp", x, 70), 0, 25, add = TRUE, lty = 3)
```
