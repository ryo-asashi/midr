# Encoder for Quantitative Variables

`numeric.encoder()` creates an encoder function for a quantitative
variable. This encoder can then be used to convert a numeric vector into
a design matrix using either piecewise linear or one-hot interval
encoding, which are core components for modeling effects in a MID model.

`numeric.frame()` is a helper function to create a "numeric.frame"
object that defines the encoding scheme.

## Usage

``` r
numeric.encoder(
  x,
  k,
  type = 1L,
  encoding.digits = NULL,
  tag = "x",
  frame = NULL,
  weights = NULL
)

numeric.frame(
  reps = NULL,
  breaks = NULL,
  type = NULL,
  encoding.digits = NULL,
  tag = "x"
)
```

## Arguments

- x:

  a numeric vector to be encoded.

- k:

  an integer specifying the coarseness of the encoding. If not positive,
  all unique values of `x` are used as knots or bins.

- type:

  an integer (`1` or `0`) specifying the encoding method (see the
  "details" section).

- encoding.digits:

  an integer specifying the rounding digits for the piecewise linear
  encoding (`type = 1`).

- tag:

  the name of the variable.

- frame:

  a "numeric.frame" object or a numeric vector that explicitly defines
  the knots or breakes for the encoding.

- weights:

  an optional numeric vector of sample weights for `x`.

- reps:

  a numeric vector to be used as the representative values (knots).

- breaks:

  a numeric vector to be used as the binning breaks.

## Value

`numeric.encoder()` returns an object of class "encoder". This is a list
containing the following components:

- frame:

  a "numeric.frame" object containing the encoding information.

- encode:

  a function to convert a numeric vector `x` into a dummy matrix.

- n:

  the number of encoding levels (i.e., columns in the design matrix).

- type:

  a character string describing the encoding type: "linear", "constant",
  or "null".

`numeric.frame()` returns a "numeric.frame" object containing the
encoding information.

## Details

The primary purpose of the encoder is to transform a single numeric
variable into a design matrix for the MID model's linear system
formulation. The output of the encoder depends on the `type` argument.

When `type = 1`, the variable's effect is modeled as a piecewise linear
function with `k` knots including both ends. For each value, the encoder
finds the two nearest knots and assigns a weight to each, based on its
relative position. This results in a design matrix where each row has at
most two non-zero values that sum to `1`. This approach creates a
smooth, continuous representation of the effect.

When `type = 0`, the variable's effect is modeled as a step function by
dividing its range into `k` intervals (bins). The encoder determines
which interval each value falls into and assigns a `1` to the
corresponding column in the design matrix, with all other columns being
`0`. This results in a standard one-hot encoded matrix and creates a
discrete, bin-based representation of the effect.

## See also

[`factor.encoder`](https://ryo-asashi.github.io/midr/reference/factor.encoder.md)

## Examples

``` r
# Create an encoder for a quantitative variable
data(iris, package = "datasets")
enc <- numeric.encoder(x = iris$Sepal.Length, k = 5L, tag = "Sepal.Length")
enc
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

# Encode a numeric vector with NA and Inf
enc$encode(x = c(4:8, NA, Inf))
#>        4.3   5.1       5.8       6.4 7.9
#> [1,] 1.000 0.000 0.0000000 0.0000000 0.0
#> [2,] 0.125 0.875 0.0000000 0.0000000 0.0
#> [3,] 0.000 0.000 0.6666667 0.3333333 0.0
#> [4,] 0.000 0.000 0.0000000 0.6000000 0.4
#> [5,] 0.000 0.000 0.0000000 0.0000000 1.0
#> [6,] 0.000 0.000 0.0000000 0.0000000 0.0
#> [7,] 0.000 0.000 0.0000000 0.0000000 1.0

# Create an encoder with a pre-defined encoding frame
frm <- numeric.frame(breaks = c(3, 5, 7, 9), type = 0L)
enc <- numeric.encoder(x = iris$Sepal.Length, frame = frm)
enc$encode(x = c(4:8, NA, Inf))
#>      [-Inf, 5) [5, 7) [7, Inf)
#> [1,]         1      0        0
#> [2,]         0      1        0
#> [3,]         0      1        0
#> [4,]         0      0        1
#> [5,]         0      0        1
#> [6,]         0      0        0
#> [7,]         0      0        1

# Create an encoder with a numeric vector specifying the knots
enc <- numeric.encoder(x = iris$Sepal.Length, frame = c(3, 5, 7, 9))
enc$encode(x = c(4:8, NA, Inf))
#>        3   5   7   9
#> [1,] 0.5 0.5 0.0 0.0
#> [2,] 0.0 1.0 0.0 0.0
#> [3,] 0.0 0.5 0.5 0.0
#> [4,] 0.0 0.0 1.0 0.0
#> [5,] 0.0 0.0 0.5 0.5
#> [6,] 0.0 0.0 0.0 0.0
#> [7,] 0.0 0.0 0.0 1.0
```
