# Compare MID Component Functions with ggplot2

For "mids" collection objects,
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
visualizes and compares a single main effect across multiple models.

## Usage

``` r
# S3 method for class 'mids'
ggmid(
  object,
  term,
  type = c("effect", "series"),
  theme = NULL,
  intercept = FALSE,
  resolution = NULL,
  labels = base::labels(object),
  ...
)

# S3 method for class 'mids'
autoplot(object, ...)
```

## Arguments

- object:

  a "mids" or "midlist" collection object to be visualized.

- term:

  a character string specifying the main effect to evaluate.

- type:

  the plotting style: "effect" plots the effect curve per model, while
  "series" plots the effect trend over models per feature value.

- theme:

  a character string or object defining the color theme.

- intercept:

  logical. If `TRUE`, the model intercept is added to the component
  effect.

- resolution:

  an integer specifying the number of evaluation points for continuous
  variables.

- labels:

  an optional numeric or character vector to specify the x-axis
  coordinates or labels. Defaults to `labels(object)`. The function
  attempts to parse these labels into numeric values where possible.

- ...:

  optional parameters passed to the main layer (e.g., `linewidth`,
  `alpha`).

## Details

This method evaluates the specified `term` over a grid of values and
compares the results across all models in the collection. For continuous
variables, it uses line plots (or step plots for constant encodings).
For factors, it uses grouped bar plots.

Note: Comparative plotting for interaction terms (2D surfaces) is not
supported for collection objects.
