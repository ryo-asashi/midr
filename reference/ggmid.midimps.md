# Compare MID Importance with ggplot2

For "midimps" collection objects,
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
visualizes and compares the importance of component functions across
multiple fitted MID models.

## Usage

``` r
# S3 method for class 'midimps'
ggmid(
  object,
  type = c("barplot", "dotchart"),
  theme = NULL,
  terms = NULL,
  max.nterms = 10L,
  ...
)

# S3 method for class 'midimps'
autoplot(object, ...)
```

## Arguments

- object:

  a "midimps" collection object to be visualized.

- type:

  the plotting style. One of "barplot" or "dotchart".

- theme:

  a character string or object defining the color theme. Defaults to
  "HCL". See
  [`color.theme`](https://ryo-asashi.github.io/midr/reference/color.theme.md)
  for details.

- terms:

  an optional character vector specifying which terms to display. If
  `NULL`, terms are automatically extracted from the object.

- max.nterms:

  the maximum number of terms to display. Defaults to 30.

- ...:

  optional parameters passed on to the main layer (e.g.,
  [`geom_col`](https://ggplot2.tidyverse.org/reference/geom_bar.html)).

## Value

`ggmid.midimps()` returns a "ggplot" object.

## Details

This is an S3 method for the
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
generic that creates a comparative importance plot from a "midimps"
collection object. It visualizes the average contribution of component
functions to the fitted MID models, allowing for easy comparison across
different models.

The `type` argument controls the visualization style. The default,
`type = "barplot"`, creates a standard grouped bar plot where the length
of each bar represents the overall importance of the term, positioned
side-by-side (`position_dodge`) by model label. The `type = "dotchart"`
option creates a grouped dot plot, offering a clean alternative to the
bar plot for visualizing and comparing term importance across models.

## See also

[`mid.importance`](https://ryo-asashi.github.io/midr/reference/mid.importance.md),
[`ggmid.midimp`](https://ryo-asashi.github.io/midr/reference/ggmid.midimp.md),
[`midlist`](https://ryo-asashi.github.io/midr/reference/midlist.md)

## Examples

``` r
# Use a lightweight dataset for fast execution
data(mtcars, package = "datasets")

# Fit two different models for comparison
mid1 <- interpret(mpg ~ wt + hp + cyl, data = mtcars)
#> 'model' not passed: response variable in 'data' is used
mid2 <- interpret(mpg ~ (wt + hp + cyl)^2, data = mtcars)
#> 'model' not passed: response variable in 'data' is used

# Calculate importance for both models and combine them
imps <- midlist(
  "Main Effects" = mid.importance(mid1),
  "Interactions" = mid.importance(mid2)
)

# Create a comparative grouped bar plot (default)
ggmid(imps)


# Create a comparative dot chart with a specific theme
ggmid(imps, type = "dotchart", theme = "Okabe-Ito")
```
