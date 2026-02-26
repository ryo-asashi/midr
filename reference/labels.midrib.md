# Modify Labels of a Collection of MID Models

S3 methods to get/set the labels (model names) of a "midrib" or
"midlist" object.

## Usage

``` r
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

  a "midrib" object, or its summary objects (e.g.,
  "midlist.importance").

- ...:

  optional parameters passed to other methods.

- value:

  a character vector of the same length as the number of models in the
  "midlist" object.

## Value

- `labels` returns a character vector of model names.

- `labels<-` returns the updated object with new labels.

## Details

The "midrib" object stores multiple fitted MID models in a optimized
struct-of-arrays format.

Because of the internal struct-of-arrays ("AsIs") structure, using
[`names()`](https://rdrr.io/r/base/names.html) on a "midrib" object
returns internal component names (e.g., "intercept", "main.effects"). To
safely access or modify the names of the models, always use
[`labels()`](https://rdrr.io/r/base/labels.html) and `labels<-`.
