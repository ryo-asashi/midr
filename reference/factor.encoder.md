# Encoder for Qualitative Variables

`factor.encoder()` creates an encoder function for a qualitative (factor
or character) variable. This encoder converts the variable into a
one-hot encoded (dummy) design matrix.

`factor.frame()` is a helper function to create a "factor.frame" object
that defines the encoding scheme.

## Usage

``` r
factor.encoder(
  x,
  k,
  use.catchall = TRUE,
  catchall = "(others)",
  tag = "x",
  frame = NULL,
  weights = NULL
)

factor.frame(levels, catchall = "(others)", tag = "x")
```

## Arguments

- x:

  a vector to be encoded as a qualitative variable.

- k:

  an integer specifying the maximum number of distinct levels to retain
  (including the catch-all level). If not positive, all unique values of
  `x` are used.

- use.catchall:

  logical. If `TRUE`, less frequent levels are grouped into the
  catch-all level.

- catchall:

  a character string for the catch-all level.

- tag:

  the name of the variable.

- frame:

  a "factor.frame" object or a character vector that explicitly defines
  the levels of the variable.

- weights:

  an optional numeric vector of sample weights for `x`.

- levels:

  a vector to be used as the levels of the variable.

## Value

`factor.encoder()` returns an object of class "encoder". This is a list
containing the following components:

- frame:

  a "factor.frame" object containing the encoding information (levels).

- encode:

  a function to convert a vector `x` into a one-hot encoded matrix.

- n:

  the number of encoding levels (i.e., columns in the design matrix).

- type:

  a character string describing the encoding type: "factor" or "null".

`factor.frame()` returns a "factor.frame" object containing the encoding
information.

## Details

This function is designed to handle qualitative data for use in the MID
model's linear system formulation.

The primary mechanism is one-hot encoding. Each unique level of the
input variable becomes a column in the output matrix. For a given
observation, the column corresponding to its level is assigned a `1`,
and all other columns are assigned `0`.

When a variable has many unique levels (high cardinality), you can use
the `use.catchall = TRUE` and `k` arguments. This will group the `k - 1`
most frequent levels into their own columns, while all other less
frequent levels are consolidated into a single `catchall` level (e.g.,
"(others)" by default). This is crucial for preventing MID models from
becoming overly complex.

## See also

[`numeric.encoder`](https://ryo-asashi.github.io/midr/reference/numeric.encoder.md)

## Examples

``` r
# Create an encoder for a qualitative variable
data(iris, package = "datasets")
enc <- factor.encoder(x = iris$Species, use.catchall = FALSE, tag = "Species")
enc
#> 
#> Factor encoder with 3 levels
#> 
#> Frame:
#>      Species
#> 1     setosa
#> 2 versicolor
#> 3  virginica
#> 

# Encode a vector with NA
enc$encode(x = c("setosa", "virginica", "ensata", NA, "versicolor"))
#>      setosa versicolor virginica
#> [1,]      1          0         0
#> [2,]      0          0         1
#> [3,]      0          0         0
#> [4,]      0          0         0
#> [5,]      0          1         0

# Create an encoder with a pre-defined encoding frame
frm <- factor.frame(c("setosa", "virginica"), "other iris")
enc <- factor.encoder(x = iris$Species, frame = frm)
enc
#> 
#> Factor encoder with 3 levels
#> 
#> Frame:
#>            x
#> 1     setosa
#> 2  virginica
#> 3 other iris
#> 
enc$encode(c("setosa", "virginica", "ensata", NA, "versicolor"))
#>      setosa virginica other iris
#> [1,]      1         0          0
#> [2,]      0         1          0
#> [3,]      0         0          1
#> [4,]      0         0          1
#> [5,]      0         0          1

# Create an encoder with a character vector specifying the levels
enc <- factor.encoder(x = iris$Species, frame = c("setosa", "versicolor"))
enc$encode(c("setosa", "virginica", "ensata", NA, "versicolor"))
#>      setosa versicolor
#> [1,]      1          0
#> [2,]      0          0
#> [3,]      0          0
#> [4,]      0          0
#> [5,]      0          1
```
