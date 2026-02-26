# Combine MID Objects

Combines multiple MID models ("mid") or their interpretation results
("midimp", "midcon", "midbrk") into a unified collection object. This is
useful for grouping models and their explanations together for seamless
comparison, summary, and visualization.

## Usage

``` r
midlist(...)
```

## Arguments

- ...:

  objects to be combined. All inputs must inherit from exactly one of
  the supported base classes: "mid", "midimp", "midcon", or "midbrk".
  Collection classes (e.g., "mids"-"midrib", "midimps"-"midlist") are
  also accepted and will be flattened appropriately.

## Value

`midlist()` returns a list-based collection object inheriting from
"midlist" and the appropriate collection class (e.g.,
"midcons"-"midlist"). If a single "midrib" object is provided, the
original object is returned as-is.

## Details

The `midlist()` function acts as a polymorphic constructor for
collection objects. Depending on the class of the input objects, it
automatically assigns the appropriate classes (e.g., "mid" objects
become a "mids"-"midlist" collection; "midimp" objects become a
"midimps"-"midlist" collection). All objects provided in `...` must
belong to the same base class.

If a single "midrib" object is provided, it is returned as-is,
preserving its optimized struct-of-arrays format. However, if a "midrib"
object is combined with other objects via `...`, it is automatically
coerced into a pure list (array of structures) to ensure structural
consistency before concatenation.

## Examples

``` r
# Fit models using the built-in anscombe dataset
fit_1 <- lm(cbind(y1, y2, y3) ~ x1, data = anscombe)
fit_2 <- lm(y4 ~ x4, data = anscombe)

# Create interpretation objects
# mid_1 is a "midrib" collection containing 3 models
mid_1 <- interpret(cbind(y1, y2, y3) ~ x1, data = anscombe, model = fit_1)
# mid_2 is a single "mid" object
mid_2 <- interpret(y4 ~ x4, data = anscombe, model = fit_2)

# Combine a "midrib" and a "mid" into a single "midlist" collection.
mid_collection <- midlist(mid_1, y4 = mid_2)

# Check the labels of the combined collection
labels(mid_collection)
#> [1] "y1" "y2" "y3" "y4"

# The resulting object is a flat list of models
class(mid_collection)
#> [1] "mids"    "midlist"
```
