# Subset MID Objects

S3 methods to extract parts of a "midrib" or "midlist" collection
object.

## Usage

``` r
# S3 method for class 'midrib'
x[i, drop = if (missing(i)) TRUE else length(i) == 1L]

# S3 method for class 'midrib'
x[[i, exact = TRUE]]

# S3 method for class 'midlist'
x[i, drop = if (missing(i)) TRUE else length(i) == 1L]
```

## Arguments

- x:

  a collection object of class "midlist" or "midrib".

- i:

  indices specifying elements to extract. Can be numeric, character, or
  logical vectors.

- drop:

  logical. If `TRUE` the result is coerced to the lowest possible
  dimension. For a collection (e.g., "mids"), extracting a single
  element drops it to a base object (e.g., "mid").

- exact:

  logical. If `TRUE`, exact matching is used for character strings.

## Value

`[` returns a subsetted collection object (e.g., "midrib", "midlist") or
a single base object if `drop = TRUE`.

`[[` returns a single base object (e.g., "mid").

## Details

A "midlist" or "midrib" object stores multiple objects of the same
single base class: "mid", "midimp", "midcon", or "midbrk".

When extracting items using `[`, it returns a subsetted "midlist" or
"midrib" object, preserving its collection class (e.g., "mids",
"midimps"). By default, if a single base object is extracted
(`length(i) == 1L`) and `drop = TRUE`, the object is simplified to a
single base object (e.g., "mid", "midimp"). `[[` always extracts a
single base object.

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
