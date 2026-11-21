# Coerce, Inspect, Filter, and Match Model Terms and Labels

A standalone, zero-dependency suite of functions to seamlessly
manipulate [`terms`](https://rdrr.io/r/stats/terms.html) objects,
formulas, fitted models, and character vectors of term labels.

## Usage

``` r
as_terms(
  x,
  response = NULL,
  intercept = NULL,
  data = NULL,
  keep.order = NULL,
  env = parent.frame(),
  ...
)

filter_terms(
  x,
  keep = NULL,
  drop = NULL,
  order = NULL,
  require = NULL,
  remove = NULL,
  raw = TRUE,
  response = NULL,
  intercept = NULL,
  ...
)

get_labels(x, ...)

match_labels(x, y, nomatch = c("omit", "pass", "fail"), use.names = TRUE, ...)

get_order(x, use.names = TRUE, ...)

get_variables(x, raw = FALSE, by.term = FALSE, ...)

get_response(x, raw = FALSE, ...)

has_response(x, ...)

has_intercept(x, ...)
```

## Arguments

- x, y:

  an object representing model terms: a
  [`terms`](https://rdrr.io/r/stats/terms.html) object, a
  [`formula`](https://rdrr.io/r/stats/formula.html), a fitted model
  object, or a character vector. In `match_labels()`, `y` is the target
  object whose term labels are matched against.

- response:

  optional response specification for `as_terms()` and `filter_terms()`.
  If `NULL` (default) or `TRUE`, retains the existing response of `x`
  (or no response if `x` is a character vector). If `FALSE`, removes the
  response variable. If a character string or language object, sets or
  replaces the response variable.

- intercept:

  optional logical for `as_terms()` and `filter_terms()`. If `NULL`
  (default), retains the existing intercept status of `x` (or defaults
  to no intercept for character vectors unless explicit tokens like
  `"1"` or `"+1"` are present in `x`). If `TRUE` or `FALSE`, explicitly
  forces or removes the intercept.

- data:

  a `data.frame` used to resolve `.` in formulas or shorthand strings.

- keep.order:

  logical. Should terms be kept in their input order rather than sorted
  by interaction order? Defaults to `TRUE` for strict term label vectors
  and `FALSE` for formulas or length-1 formula shorthands.

- env:

  an environment to attach to the resulting `terms` object when coercing
  from a character vector. Defaults to
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html).

- ...:

  additional arguments passed to
  [`terms`](https://rdrr.io/r/stats/terms.html) or `as_terms()`.

- keep, drop:

  a character vector of term labels (matched order-invariantly via
  `match_labels()`) or an integer vector of term indices to explicitly
  retain (`keep`) or exclude (`drop`) in `filter_terms()`.

- order:

  an integer vector specifying interaction orders to retain in
  `filter_terms()` (e.g., `1L` for main effects, `2L` for two-way
  interactions).

- require:

  a character vector of variable names. Only terms containing at least
  one of these variables are retained.

- remove:

  a character vector of variable names. Any terms containing these
  variables are excluded.

- raw:

  logical. If `FALSE` (default in `get_variables()` and
  `get_response()`), returns model expressions (e.g., `"log(x2)"`). If
  `TRUE` (default in `filter_terms()`), extracts or filters by
  underlying raw column names (e.g., `"x2"`).

- nomatch:

  character string specifying how to handle unmatched labels in
  `match_labels()`: `"omit"` (default; drops unmatched elements),
  `"pass"` (returns `NA`), or `"fail"` (throws an error).

- use.names:

  logical. If `TRUE` (default), `match_labels()` names the output vector
  with the input labels from `x`, and `get_order()` names the integer
  vector with the model's term labels.

- by.term:

  logical. If `FALSE` (default), `get_variables()` returns a flat
  character vector across the whole model. If `TRUE`, returns a named
  list mapping each term label to its constituent variables.

## Value

- `as_terms()`, `filter_terms()`: a
  [`terms`](https://rdrr.io/r/stats/terms.html) object.

- `get_labels()`, `match_labels()`: a character vector (named by input
  labels in `match_labels()` when `use.names = TRUE`).

- `get_order()`: an integer vector of term orders (named when
  `use.names = TRUE`).

- `get_variables()`: a character vector (when `by.term = FALSE`) or a
  named list of character vectors (when `by.term = TRUE`).

- `get_response()`: a character string (when `raw = FALSE`), a character
  vector of variable names (when `raw = TRUE`), or `NULL` if no response
  exists.

- `has_response()`, `has_intercept()`: a scalar logical (`TRUE` or
  `FALSE`).

## Details

### Three Levels of Model Components

For a model formula such as
`y ~ x1 + log(x2) + x1:log(x2) + I(x1 * x2)`, this package distinguishes
three structural levels:

1.  **Term Labels** (`get_labels(x)`): The individual additive
    components of the model:
    `c("x1", "log(x2)", "x1:log(x2)", "I(x1 * x2)")`.

2.  **Model Variables** (`get_variables(x, raw = FALSE)`): The distinct
    predictor expressions forming the terms (rows of the `"factors"`
    matrix): `c("x1", "log(x2)", "I(x1 * x2)")`.

3.  **Raw Variables** (`get_variables(x, raw = TRUE)`): The underlying
    column names required in `data`: `c("x1", "x2")`.

### Character Input Rules in `as_terms()`

When `x` is a character vector, `as_terms()` distinguishes between a
vector of strict term labels and a length-1 formula shorthand:

- **Strict Term Labels (`character(n)`)**: If every element (excluding
  intercept tokens like `"1"` or `"-1"`) represents strictly a single
  valid term (e.g., `c("b:a", "a", "I(x^2)")`), input order and exact
  interaction notation (`"b:a"`) are preserved by default
  (`keep.order = TRUE`).

- **Length-1 Formula Shorthand (`character(1)`)**: If `x` is a single
  string containing formula operators or `.` (e.g., `".^2"`, `"."`, or
  `"a * b + c + 1"`), it is expanded as a right-hand side formula
  expression (using `data` if needed) and sorted by interaction order by
  default (`keep.order = FALSE`).

- **Invalid Mixed Vectors (`length(x) >= 2`)**: If `x` has length 2 or
  more and contains any non-single-term expression (such as
  `c("a * b", "c")` or `c(".", "a")`), an informative error is thrown.

### Subsetting and Filtering with `filter_terms()`

`filter_terms()` serves as a declarative, crash-safe replacement for
[`drop.terms`](https://rdrr.io/r/stats/delete.response.html),
[`delete.response`](https://rdrr.io/r/stats/delete.response.html), and
`[.terms`:

- **Direct Subsetting**: Pass character labels or integer indices to
  `keep` (the second argument) or `drop` to extract or exclude specific
  terms (order-invariantly) while preserving the intercept and response.

- **Structural Filtering**: Use `order`, `require`, and `remove` to
  filter terms by interaction order or constituent variables.

- **Response & Intercept Control**: In both `as_terms()` and
  `filter_terms()`, set `response = FALSE` to remove the response
  variable (like `stats::delete.response(x)`), pass a new variable name
  to replace it, or set `intercept = TRUE` / `FALSE` without crashing
  even when all predictor terms are dropped.
