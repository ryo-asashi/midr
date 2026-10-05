# helper functions for "terms" objects and term labels

as.terms <- function(
    x, y = NULL, intercept = FALSE, env = parent.frame(), data = NULL
) {
  if (inherits(x, "terms"))
    return(x)
  if (is.character(x)) {
    if (!intercept)
      x <- c("-1", x)
    x <- stats::reformulate(x, response = y, intercept = TRUE, env = env)
  }
  tryCatch(stats::terms(x, data = data), error = function(e) NA)
}

has.intercept <- function(x) {
  attr(as.terms(x), "intercept") %||% 0L > 0L
}

has.response <- function(x) {
  attr(as.terms(x), "response") %||% 0L > 0L
}

get.variables <- function(x) {
  facs <- attr(as.terms(x), "factors")
  if (is.null(facs) || length(facs) == 0L) {
    character(length = 0L)
  } else {
    rownames(facs)[rowSums(facs) > 0L]
  }
}

get.labels <- function(x) {
  attr(as.terms(x), "term.labels")
}

is.fully.crossed <- function(x) {
  x <- as.terms(x)
  if (identical(x, NA)) return(FALSE)
  n <- length(get.variables(x))
  if (n <= 1L) return(FALSE)
  m <- length(get.labels(x))
  m == (2^n - 1L)
}

is.single.term <- function(x) {
  x <- as.terms(x)
  if (identical(x, NA)) return(FALSE)
  m <- length(get.labels(x))
  m == 1L
}

make.signature <- function(x, single = TRUE, sort = TRUE) {
  x <- as.terms(x)
  if (identical(x, NA)) return(NA_character_)
  if (single && !is.single.term(x)) return(NA_character_)
  vars <- get.variables(x)
  if (sort) vars <- sort.int(vars)
  paste(vars, collapse = ":")
}

match.labels <- function(x, y, single = TRUE, sort = TRUE, names = NULL) {
  if (!is.character(x))
    x <- attr(as.terms(x), "term.labels")
  xsigs <- vapply(x, make.signature, character(1L), single = single, sort = sort)
  if (!is.character(y))
    y <- attr(as.terms(y), "term.labels")
  ysigs <- vapply(y, make.signature, character(1L), single = single, sort = sort)
  out <- y[match(xsigs, ysigs, incomparables = NA)]
  stats::setNames(out, names)
}
