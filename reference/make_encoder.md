# Create Variable Encoders and Encode Data

`make_encoder()` creates a variable encoder, which can then be used to
convert a vector or data frame into a design matrix.

`encode()` applies an encoder to data, returning the encoded design
matrix.

## Usage

``` r
make_encoder(x, ...)

# S3 method for class 'numeric'
make_encoder(
  x,
  k = 25L,
  type = c("linear", "constant", "null"),
  split = c("quantile", "uniform"),
  weights = NULL,
  digits = NULL,
  tag = "x",
  spec = NULL,
  ...
)

# S3 method for class 'factor'
make_encoder(
  x,
  k = 25L,
  lump = c("none", "auto", "rank", "order"),
  others = "others",
  sep = ">",
  weights = NULL,
  tag = "x",
  spec = NULL,
  ...
)

encode(object, ...)

# S3 method for class 'encoder'
encode(object, x, ...)

# S3 method for class 'mid'
encode(object, variable, order = 1L, data = NULL, ...)
```

## Arguments

- x:

  a vector, data frame, or frame specification to be encoded.

- ...:

  further arguments passed to or from other methods.

- k:

  an integer specifying the coarseness of the encoding (max knots, bins,
  or levels). If not positive or `NULL`, all unique values of `x` are
  used.

- type:

  a character string or an integer specifying the encoding method for
  numeric variables: "linear" (`1`; default) or "constant" (`0`).

- split:

  a character string specifying the splitting strategy for numeric
  variables: "quantile" (default) or "uniform".

- weights:

  an optional numeric vector of sample weights for `x`.

- digits:

  an integer specifying the rounding digits for piecewise linear
  encoding (`type = "linear"`).

- tag:

  the name of the variable.

- spec:

  an atomic vector specifying the encoding frame, or an "encoder"
  object.

- lump:

  a character string specifying the lumping strategy for factor
  variables: `"none"`, `"auto"`, `"rank"`, or `"order"`.

- others:

  a character string for the catch-all level (used when
  `lump = "rank"`).

- sep:

  a character string used to separate start and end levels when merging
  ordered factors.

- object:

  an atomic vector, "encoder" object, or other object containing
  encoding information. If an atomic vector is provided, an encoder is
  created and applied.

- variable:

  a character string specifying the variable name to encode.

- order:

  an integer specifying which encoder should be used.

- data:

  a data frame containing the variables to be encoded.

## Value

`make_encoder()` returns an object of class "encoder", which is a list
containing:

- frame:

  a single-column data.frame containing the encoding knots, interval
  labels, or factor levels.

- n:

  the number of encoding levels (columns in the design matrix).

- type:

  a character string describing the encoding type ("linear", "constant",
  "factor", or "null").

- tag:

  a character string specifying the variable name.

- envir:

  an environment enclosing the
  [`transform()`](https://rdrr.io/r/base/transform.html) and `encode()`
  functions.

- transform:

  a function `transform(x, ...)` that preprocesses a vector before
  encoding.

- encode:

  a function `encode(x, ...)` that converts a vector into the encoded
  matrix.

`encode()` returns a numeric matrix (the encoded design matrix).

## Details

For numeric variables, the encoding method is controlled by `type`: When
`type` = "linear" (or `1`), the variable's effect is modeled as a
piecewise linear function with `k` knots including both ends. For each
value, the encoder finds the two nearest knots and assigns a weight to
each based on its relative position. This results in a design matrix
where each row has at most two non-zero values that sum to `1`, creating
a smooth, continuous representation. When `type` = "constant" (or `0`),
the variable's effect is modeled as a step function by dividing its
range into `k` intervals (bins). The encoder assigns `1` to the
corresponding bin column and `0` elsewhere.

For factor, character, or logical variables, one-hot encoding is
applied: Each unique level becomes a column in the output matrix. When a
variable has high cardinality, you can use `lump` and `k` to reduce
dimensionality.

## Examples

``` r
data(iris, package = "datasets")

# Create an encoder for a quantitative variable
enc_num <- make_encoder(iris$Sepal.Length, k = 5L, tag = "Sepal.Length")
enc_num
#> 
#> Linear encoder with 5 knots
#> 
#> Frame:
#>   Sepal.Length
#> 1          4.3
#> 2          5.1
#> 3          5.8
#> 4          6.4
#> 5          7.9
#> 
encode(enc_num, c(4:8, NA, Inf))
#>        4.3   5.1       5.8       6.4 7.9
#> [1,] 1.000 0.000 0.0000000 0.0000000 0.0
#> [2,] 0.125 0.875 0.0000000 0.0000000 0.0
#> [3,] 0.000 0.000 0.6666667 0.3333333 0.0
#> [4,] 0.000 0.000 0.0000000 0.6000000 0.4
#> [5,] 0.000 0.000 0.0000000 0.0000000 1.0
#> [6,] 0.000 0.000 0.0000000 0.0000000 0.0
#> [7,] 0.000 0.000 0.0000000 0.0000000 1.0

# Create an encoder for a qualitative variable with lumping
enc_fac <- make_encoder(iris$Species, k = 2L, lump = "rank", tag = "Species")
enc_fac
#> 
#> Factor encoder with 2 levels
#> 
#> Frame:
#>   Species
#> 1  setosa
#> 2  others
#> 
encode(enc_fac, iris$Species[c(1, 51, 101)])
#>      setosa others
#> [1,]      1      0
#> [2,]      0      1
#> [3,]      0      1

# Directly encode a raw vector
head(encode(iris$Petal.Width, k = 4L, type = "constant"))
#>      [-Inf, 0.60) [0.60, 1.25) [1.25, 1.80) [1.80, Inf)
#> [1,]            1            0            0           0
#> [2,]            1            0            0           0
#> [3,]            1            0            0           0
#> [4,]            1            0            0           0
#> [5,]            1            0            0           0
#> [6,]            1            0            0           0
```
