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
  k = NULL,
  lump = c("none", "auto", "rank", "order"),
  others = "others",
  sep = ">",
  weights = NULL,
  frame = NULL,
  tag = "x"
)

factor.frame(levels, others = NULL, map = NULL, original = NULL, tag = "x")
```

## Arguments

- x:

  a vector to be encoded as a qualitative variable.

- k:

  an integer specifying the maximum number of distinct levels to retain
  (including the catch-all level). If not positive, all unique values of
  `x` are used.

- lump:

  a character string specifying the lumping strategy: `"none"`, no
  lumping is performed; `"rank"`, lumps levels based on frequency rank;
  `"order"` merges adjacent levels based on cumulative frequency to
  preserve order; and `"auto"` automatically selects `"order"` for
  ordered factors and `"rank"` for others.

- others:

  a character string for the catch-all level (used when
  `lump = "rank"`).

- sep:

  a character string used to separate the start and end levels when
  merging ordered factors (e.g., "Level1..Level3").

- weights:

  an optional numeric vector of sample weights for `x`.

- frame:

  a "factor.frame" object or a character vector that explicitly defines
  the levels of the variable.

- tag:

  the name of the variable.

- levels:

  a vector to be used as the levels of the variable.

- map:

  a named vector that maps original levels to lumped levels.

- original:

  a character vector to be used as the original levels for expanding the
  frame. Defaults to `NULL`.

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
the `lump` and `k` arguments to reduce dimensionality. This is crucial
for preventing MID models from becoming overly complex.

## See also

[`numeric.encoder`](https://ryo-asashi.github.io/midr/reference/numeric.encoder.md)

## Examples

``` r
# Create an encoder for a qualitative variable
data(iris, package = "datasets")
enc <- factor.encoder(x = iris$Species, lump = "none", tag = "Species")
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

# Lumping by rank (retain top 1 + others)
enc <- factor.encoder(x = iris$Species, k = 2, lump = "rank", others = "other.iris")
enc$encode(head(iris$Species))
#>      setosa other.iris
#> [1,]      1          0
#> [2,]      1          0
#> [3,]      1          0
#> [4,]      1          0
#> [5,]      1          0
#> [6,]      1          0

# Lumping ordered factor (merge adjacent levels)
x <- ordered(sample(LETTERS[1:5], 20, replace = TRUE))
enc <- factor.encoder(x, k = 3, lump = "order")
enc$encode(x)
#>       A>B C>D E
#>  [1,]   0   1 0
#>  [2,]   1   0 0
#>  [3,]   0   0 1
#>  [4,]   0   0 1
#>  [5,]   0   0 1
#>  [6,]   1   0 0
#>  [7,]   0   1 0
#>  [8,]   0   1 0
#>  [9,]   0   0 1
#> [10,]   0   0 1
#> [11,]   0   1 0
#> [12,]   0   0 1
#> [13,]   1   0 0
#> [14,]   0   1 0
#> [15,]   1   0 0
#> [16,]   0   1 0
#> [17,]   1   0 0
#> [18,]   0   1 0
#> [19,]   0   0 1
#> [20,]   1   0 0
```
