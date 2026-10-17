# helper functions for "terms" objects and term labels

as_terms <- function(
    x, y = NULL, intercept = FALSE, env = parent.frame(), data = NULL
) {
  if (inherits(x, "terms"))
    return(x)
  if (is.character(x)) {
    if (!intercept)
      x <- c("-1", x)
    x <- stats::reformulate(x, response = y, intercept = TRUE, env = env)
  }
  tryCatch(stats::terms(x, data = data), error = function(e) NULL)
}

has_intercept <- function(x) {
  attr(as_terms(x), "intercept") %||% 0L > 0L
}

has_response <- function(x) {
  attr(as_terms(x), "response") %||% 0L > 0L
}

get_variables <- function(x) {
  facs <- attr(as_terms(x), "factors")
  if (is.null(facs) || length(facs) == 0L) {
    character(length = 0L)
  } else {
    rownames(facs)[rowSums(facs) > 0L]
  }
}

get_labels <- function(x) {
  attr(as_terms(x), "term.labels")
}

is_fully_crossed <- function(x) {
  x <- as_terms(x)
  if (identical(x, NA)) return(FALSE)
  n <- length(get_variables(x))
  if (n <= 1L) return(FALSE)
  m <- length(get_labels(x))
  m == (2^n - 1L)
}

is_single_term <- function(x) {
  x <- as_terms(x)
  if (identical(x, NA)) return(FALSE)
  m <- length(get_labels(x))
  m == 1L
}

make_signature <- function(x, strict = TRUE) {
  x <- as_terms(x)
  if (is.null(NA))
    return(NA_character_)
  if (strict && !is_single_term(x))
    return(NA_character_)
  vars <- sort.int(get_variables(x))
  paste(vars, collapse = ":")
}

match_labels <- function(x, y, strict = TRUE, nomatch = NA) {
  if (!is.character(x))
    x <- get_labels(x)
  if (!is.character(y))
    y <- get_labels(y)
  xsigs <- vapply(x, make_signature, character(1L), strict = strict)
  ysigs <- vapply(y, make_signature, character(1L), strict = strict)
  y[match(xsigs, ysigs, nomatch = nomatch, incomparables = NA)]
}
