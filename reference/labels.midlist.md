# Label MID Objects

S3 methods to get or set the labels (names) of a "midrib" or "midlist"
object.

## Usage

``` r
# S3 method for class 'midlist'
labels(object, ...)

# S3 method for class 'midrib'
labels(object, ...)

labels(object) <- value

# S3 method for class 'midrib'
labels(object) <- value

# S3 method for class 'midlist'
labels(object) <- value
```

## Arguments

- object:

  a collection object of class "midlist" or "midrib".

- ...:

  optional parameters passed to other methods.

- value:

  a character vector of the same length as the number of base objects in
  the collection object.

## Value

`labels` returns a character vector of labels of the stored base
objects.

`labels<-` returns the updated collection object with new labels.

## Details

While a "midlist" object is a standard R list containing only one of a
single base class, a "midrib" object stores multiple MID models in an
optimized struct-of-arrays format.

Because of the internal struct-of-arrays ("AsIs") structure, using
[`names()`](https://rdrr.io/r/base/names.html) on a "midrib" object
returns internal component names (e.g., "intercept", "main.effects"). To
safely access or modify the names of the models, always use
[`labels()`](https://rdrr.io/r/base/labels.html) and `labels<-`.

## Examples

``` r
# Fit a multivariate linear model
fit <- lm(cbind(y1, y2, y3) ~ x1 + I(x1^2), data = anscombe)

# Interpret the linear models
mid_collection <- interpret(cbind(y1, y2, y3) ~ x1, data = anscombe, model = fit)

# Check the default labels
labels(mid_collection)
#> [1] "y1" "y2" "y3"

# Rename the models in the collection
labels(mid_collection) <- letters[1L:3L]
labels(mid_collection)
#> [1] "a" "b" "c"

# Extract a single base "mid" object by its new name using [[
mid <- mid_collection[["a"]]
class(mid)
#> [1] "mid"

# Subset the collection to keep only the first two models using [
mid_subset <- mid_collection[1:2]
class(mid_subset) # Maintains the collection class (e.g., "mids"-"midrib")
#> [1] "mids"   "midrib"
```
