#' Encoder for Qualitative Variables
#'
#' @description
#' \code{factor.encoder()} creates an encoder function for a qualitative (factor or character) variable.
#' This encoder converts the variable into a one-hot encoded (dummy) design matrix.
#'
#' @details
#' This function is designed to handle qualitative data for use in the MID model's linear system formulation.
#'
#' The primary mechanism is one-hot encoding.
#' Each unique level of the input variable becomes a column in the output matrix.
#' For a given observation, the column corresponding to its level is assigned a \code{1}, and all other columns are assigned \code{0}.
#'
#' When a variable has many unique levels (high cardinality), you can use the \code{lump} and \code{k} arguments to reduce dimensionality.
#' This is crucial for preventing MID models from becoming overly complex.
#'
#' @param x a vector to be encoded as a qualitative variable.
#' @param k an integer specifying the maximum number of distinct levels to retain (including the catch-all level). If not positive, all unique values of \code{x} are used.
#' @param lump a character string specifying the lumping strategy: \code{"none"}, no lumping is performed; \code{"rank"}, lumps levels based on frequency rank; \code{"order"} merges adjacent levels based on cumulative frequency to preserve order; and \code{"auto"} automatically selects \code{"order"} for ordered factors and \code{"rank"} for others.
#' @param others a character string for the catch-all level (used when \code{lump = "rank"}).
#' @param sep a character string used to separate the start and end levels when merging ordered factors (e.g., "Level1..Level3").
#' @param weights an optional numeric vector of sample weights for \code{x}.
#' @param frame a "factor.frame" object or a character vector that explicitly defines the levels of the variable.
#' @param tag the name of the variable.
#'
#' @examples
#' # Create an encoder for a qualitative variable
#' data(iris, package = "datasets")
#' enc <- factor.encoder(x = iris$Species, lump = "none", tag = "Species")
#' enc
#'
#' # Encode a vector with NA
#' enc$encode(x = c("setosa", "virginica", "ensata", NA, "versicolor"))
#'
#' # Lumping by rank (retain top 1 + others)
#' enc <- factor.encoder(x = iris$Species, k = 2, lump = "rank", others = "other.iris")
#' enc$encode(head(iris$Species))
#'
#' # Lumping ordered factor (merge adjacent levels)
#' x <- ordered(sample(LETTERS[1:5], 20, replace = TRUE))
#' enc <- factor.encoder(x, k = 3, lump = "order")
#' enc$encode(x)
#' @returns
#' \code{factor.encoder()} returns an object of class "encoder". This is a list containing the following components:
#' \item{frame}{a "factor.frame" object containing the encoding information (levels).}
#' \item{encode}{a function to convert a vector \code{x} into a one-hot encoded matrix.}
#' \item{n}{the number of encoding levels (i.e., columns in the design matrix).}
#' \item{type}{a character string describing the encoding type: "factor" or "null".}
#'
#' @seealso \code{\link{numeric.encoder}}
#'
#' @export factor.encoder
#'
factor.encoder <- function(
    x, k = NULL, lump = c("none", "auto", "rank", "order"), others = "others",
    sep = ">", weights = NULL, frame = NULL, tag = "x") {
  if (!is.null(frame)) {
    if (is.vector(frame))
      frame <- factor.frame(levels = frame, others = NULL)
    if (!inherits(frame, "factor.frame"))
      stop("'frame' must be a vector or a valid 'factor.frame' object")
    flvs <- attr(frame, "levels")
    others <- attr(frame, "others")
    map <- attr(frame, "map")
    olvs <- attr(frame, "original") %||% levels(as.factor(x))
  } else {
    # set levels based on x --------
    x <- as.factor(x)
    lump <- match.arg(lump)
    if (lump == "auto")
      lump <- if (is.ordered(x)) "order" else "rank"
    olvs <- levels(x)
    if (length(olvs) == 0L) {
      flvs <- "Void"
      others <- map <- NULL
    } else if (lump == "none" || is.null(k) || k < 1L || length(olvs) <= k) {
      lump <- "none"
      flvs <- olvs
      others <- map <- NULL
    } else if (lump == "order") {
      if (!is.ordered(x))
        x <- ordered(x)
      tbl <- weighted.tabulate(bin = match(x, olvs), w = weights)
      wcum <- cumsum(tbl)
      wsum <- wcum[length(wcum)]
      cuts <- wsum * seq(0, 1, length.out = k + 1L)
      grps <- findInterval(wcum, cuts, left.open = TRUE, all.inside = TRUE)
      flvs <- tapply(
        olvs, grps,
        function(x) {
          if (length(x) == 1L) x else paste0(x[1L], sep, x[length(x)])
        }
      )
      flvs <- as.character(flvs)
      others <- NULL
      map <- stats::setNames(rep(flvs, times = table(grps)), olvs)
    } else {
      tbl <- weighted.tabulate(bin = match(x, olvs), w = weights)
      ord <- order(tbl, decreasing = TRUE)
      flvs <- olvs[ord][seq_len(k - 1L)]
      flvs <- olvs[olvs %in% flvs]
      if (!is.null(others))
        flvs <- unique(c(flvs, others))
      map <- NULL
    }
  }
  type <- if (length(flvs) == 0L) "null" else "factor"
  nlvs <- length(flvs)
  frame <- factor.frame(
    levels = flvs, others = others, map = map, original = olvs, tag = tag
  )
  # define encoder function --------
  if (type == "factor") {
    transform <- function(x) {
      x <- as.character(x)
      if (!is.null(map)) {
        x.mapped <- map[x]
        ok <- !is.na(x.mapped)
        x[ok] <- x.mapped[ok]
      }
      x <- factor(x, flvs)
      if (!is.null(others)) {
        x[is.na(x)] <- others
      }
      x
    }
    encode <- function(x, ...) {
      n <- length(x)
      x <- transform(x)
      ok <- !is.na(x)
      mat <- matrix(0, nrow = n, ncol = nlvs)
      mat[cbind(which(ok), as.integer(x[ok]))] <- 1
      colnames(mat) <- flvs
      mat
    }
  } else {
    transform <- function(x) {
      NULL
    }
    encode <- function(x, ...) {
      mat <- matrix(0, nrow = length(x), ncol = 1L)
      colnames(mat) <- flvs
      mat
    }
  }
  fenv <- rlang::env(
    rlang::ns_env("midr"),
    nlvs = nlvs, flvs = flvs, others = others, map = map, transform = transform
  )
  environment(transform) <- fenv
  environment(encode) <- fenv
  enc <- list(
    frame = frame, transform = transform, encode = encode, n = nlvs, type = type
  )
  structure(enc, class = "encoder")
}

#' @rdname factor.encoder
#'
#' @description
#' \code{factor.frame()} is a helper function to create a "factor.frame" object that defines the encoding scheme.
#'
#' @param levels a vector to be used as the levels of the variable.
#' @param map a named vector that maps original levels to lumped levels.
#' @param original a character vector to be used as the original levels for expanding the frame. Defaults to \code{NULL}.
#'
#' @returns
#' \code{factor.frame()} returns a "factor.frame" object containing the encoding information.
#'
#' @export factor.frame
#'
factor.frame <- function(
    levels, others = NULL, map = NULL, original = NULL, tag = "x"
  ) {
  levels <- as.character(levels)
  if (!is.null(others)) {
    levels <- unique(c(levels, others))
    if (!is.null(original))
      original <- unique(c(original, others))
  }
  frame <- data.frame(factor(levels, levels = levels))
  frame[[2L]] <- as.integer(frame[[1L]])
  colnames(frame) <- paste0(tag, c("", "_level"))
  class(frame) <- c("factor.frame", "data.frame")
  structure(
    frame, levels = levels, others = others, map = map, original = original
  )
}
