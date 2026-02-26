# Create a Collection of MID Models

Combines multiple "mid", "midlist", or "midrib" objects into a single
"midlist" object. This is useful for grouping models together for
comparison, summary, or visualization.

## Usage

``` r
midlist(...)
```

## Arguments

- ...:

  "mid", "midlist", or "midrib" objects to be combined.

## Value

A "midlist" object, which is a list of "mid" objects. If a single
"midrib" object is provided in `...`, the original object is returned
as-is.

## Details

A "midlist" is a standard R list containing only "mid" objects. When
multiple arguments are passed to `midlist()`, they are coerced and
flattened into a single list of models.

If a single "midrib" object is provided, it is returned as-is,
preserving its optimized struct-of-arrays format. However, if a "midrib"
object is combined with other objects via `...`, it is automatically
coerced into a pure list (array of structures) to ensure structural
consistency before concatenation.
