# Wrapper Prediction Function

`get.yhat()` is a generic function that provides a unified interface for
obtaining predictions from various fitted model objects.

## Usage

``` r
get.yhat(object, newdata, ...)

# Default S3 method
get.yhat(object, newdata, target = -1L, ...)

# S3 method for class 'mid'
get.yhat(object, newdata, ...)

# S3 method for class 'lm'
get.yhat(object, newdata, ...)

# S3 method for class 'glm'
get.yhat(object, newdata, ...)

# S3 method for class 'rpart'
get.yhat(object, newdata, target = -1L, ...)

# S3 method for class 'randomForest'
get.yhat(object, newdata, target = -1L, ...)

# S3 method for class 'ranger'
get.yhat(object, newdata, target = -1L, ...)

# S3 method for class 'svm'
get.yhat(object, newdata, target = -1L, ...)

# S3 method for class 'ksvm'
get.yhat(object, newdata, target = -1L, ...)

# S3 method for class 'AccurateGLM'
get.yhat(object, newdata, ...)

# S3 method for class 'glmnet'
get.yhat(object, newdata, ...)

# S3 method for class 'model_fit'
get.yhat(object, newdata, target = -1L, ...)

# S3 method for class 'workflow'
get.yhat(object, newdata, target = -1L, ...)

# S3 method for class 'rpf'
get.yhat(object, newdata, target = -1L, ...)
```

## Arguments

- object:

  a fitted model object.

- newdata:

  a data.frame or matrix.

- ...:

  optional arguments passed on to the underlying
  [`predict()`](https://rdrr.io/r/stats/predict.html) method for the
  `object`'s class.

- target:

  an integer or character vector specifying the target levels used for
  the classification models that return a matrix or data frame of class
  probabilities. The default, `-1`, represents the probability of not
  being the base level.

## Value

`get.yhat()` returns a numeric vector of model predictions for
`newdata`.

## Details

While many predictive models have a
[`stats::predict()`](https://rdrr.io/r/stats/predict.html) method, the
structure and type of their outputs are not uniform. For example, some
return a numeric vector, others a matrix of class probabilities, and
some a list. This function, `get.yhat()`, abstracts away this
complexity.

For regression models, it returns the numeric prediction in the original
scale of the response variable. For classification models, it returns
the sum of class probabilities for the classes specified by the `target`
argument.

Furthermore, `get.yhat()` provides more consistent handling of missing
values. While some
[`stats::predict()`](https://rdrr.io/r/stats/predict.html) methods may
return a shorter vector by omitting `NA`s, `get.yhat()` is designed to
return a vector of the same length as `newdata`, preserving `NA`s in
their original positions.

The design of `get.yhat()` is strongly influenced by
[`DALEX::yhat()`](https://modeloriented.github.io/DALEX/reference/yhat.html).

## See also

[`predict.mid`](https://ryo-asashi.github.io/midr/reference/predict.mid.md)

## Examples

``` r
data(trees, package = "datasets")
model <- glm(Volume ~ ., trees, family = Gamma(log))

# The output of stats::predict() might not be in the scale of the response variable
predict(model, trees[1:5, ])
#>        1        2        3        4        5 
#> 2.458590 2.419285 2.415186 2.811365 2.989622 

# get.yhat() returns a numeric vector in the original scale of the response variable
get.yhat(model, trees[1:5, ])
#> [1] 11.68832 11.23782 11.19185 16.63260 19.87816
predict(model, trees[1:5, ], type = "response")
#>        1        2        3        4        5 
#> 11.68832 11.23782 11.19185 16.63260 19.87816 
```
