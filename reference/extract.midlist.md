# Extract Parts of a Collection of MID Models

S3 methods to extract parts of a "midrib" or "midlist" object.

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

  a "midrib" object, a "midlist" object, or its summary objects (e.g.,
  "midlist.importance").

- i:

  indices specifying elements to extract. Can be numeric, character, or
  logical vectors.

- drop:

  logical. If `TRUE` the result is coerced to the lowest possible
  dimension. For a "midrib", extracting a single element drops it to a
  "mid" object.

- exact:

  logical. If `TRUE`, exact matching is used for character strings.

## Value

`[` returns a "midrib", "midlist" or "mid" object.

`[[` returns a single "mid" object.

## Details

The "midrib" object stores multiple fitted MID models in a optimized
struct-of-arrays format.

When extracting multiple models using `[`, it returns a subsetted
"midrib" or "midlist" object. By default, if a single model is extracted
(`length(i) == 1L`) and `drop = TRUE`, the object is simplified to a
single "mid" object. `[[` always extracts a single model as a "mid"
object.

Similar extraction rules apply to summary objects like
"midlist.importance".
