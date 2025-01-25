is.color <- function(color, using.alpha = FALSE) {
  if (!(is.character(color) || is.factor(color)))
    return(FALSE)
  e <- try(grDevices::col2rgb(as.character(color), alpha = TRUE), silent = TRUE)
  if (inherits(e, "try-error"))
    return(FALSE)
  if (!using.alpha)
    return(TRUE)
  any(e[4L, ] != 255)
}

is.ramp <- function(ramp, x = seq.int(0, 1, .2)) {
  if (length(ramp) != 1L || !is.function(ramp))
    return(FALSE)
  e <- try(ramp(x), silent = TRUE)
  !inherits(e, "try-error") && length(e) == length(x) && is.color(e)
}

is.palette <- function(palette, n = 2L) {
  if (length(palette) != 1L || !is.function(palette))
    return(FALSE)
  e <- try(palette(n), silent = TRUE)
  !inherits(e, "try-error") && length(e) == n && is.color(e)
}

directed <- function(x, direction) {
  if (direction == -1L) rev(x) else x
}

to.ramp <- function(colors = NULL, palette = NULL, type = NULL) {
  if (!is.null(type) && type == "qualitative")
    return(NULL)
  if (is.null(colors)) {
    if (is.null(palette))
      stop("'colors' or 'palette' must be supplied")
    if (!is.palette(palette))
      stop("invalid 'palette' supplied")
    ptype <- attr(palette, "type")
    if (!is.null(ptype) && ptype == "qualitative") {
      message("converting qualitative palette to sequential color ramp")
      attr(palette, "type") <- "sequential"
    }
    colors <- palette(ifnot.null(attr(palette, "n.colors"), 6L))
  } else {
    if (!is.color(colors))
      stop("invalid 'colors' supplied")
  }
  names(colors) <- NULL
  alpha <- is.color(colors, using.alpha = TRUE)
  Ramp <- if (alpha) {
    grDevices::colorRamp(colors, alpha = TRUE)
  } else {
    grDevices::colorRamp(colors, space = "Lab")
  }
  ramp <- function(x) {
    if (length(x) == 0)
      return(character())
    colors <- character(length(x))
    eps <- .Machine$double.eps
    ng <- is.na(x) | x < (0 - eps) | (1 + eps) < x
    colors[ng] <- NA
    if (sum(ng) == length(x))
      return(colors)
    x[!ng] <- pmin(1, pmax(0, x[!ng]))
    m <- Ramp(x[!ng])
    colors[!ng] <-
      if (ncol(m) == 4L) {
        grDevices::rgb(m[, 1L], m[, 2L], m[, 3L], m[, 4L], maxColorValue = 255)
      } else {
        grDevices::rgb(m[, 1L], m[, 2L], m[, 3L], maxColorValue = 255)
      }
    colors
  }
  type <- ifnot.null(type, ifnot.null(attr(palette, "type"), "sequential"))
  structure(ramp, type = type)
}

to.palette <- function(colors = NULL, ramp = NULL, type = NULL) {
  if (!is.null(type) && type == "qualitative" && !is.null(colors)) {
    if (!is.color(colors))
      stop("invalid 'colors' supplied")
    names(colors) <- NULL
    n.colors <- length(colors)
    palette <- function(n) {
      stopifnot(length(n) == 1L, is.numeric(n), n > 0)
      n <- as.integer(n)
      if (n == 0L)
        return(character())
      colors[rep(seq_len(n.colors), length.out = n)]
    }
    return(structure(palette, type = type, n = n.colors))
  }
  if (is.null(ramp)) {
    if (is.null(colors))
      stop("'colors' or 'ramp' must be supplied")
    ramp <- to.ramp(colors = colors, type = type)
  } else {
    if (!is.ramp(ramp))
      stop("invalid 'ramp' supplied")
  }
  palette <- function(n) {
    stopifnot(length(n) == 1L, is.numeric(n), n > 0)
    n <- as.integer(n)
    if (n == 0L)
      return(character())
    colors <- ramp(seq.int(0, 1, length.out = n))
    colors
  }
  type <- ifnot.null(type, ifnot.null(attr(ramp, "type"), "sequential"))
  structure(palette, type = type)
}

wrap.theme <- function(
    type, palette = NULL, ramp = NULL, colors = NULL) {
  if (is.null(type))
    stop("'type' can not be NULL")
  if (sum(is.null(palette), is.null(ramp), is.null(colors)) != 2L)
    stop("invalid number of inputs are passed")
  if (type == "qualitative") {
    if (!is.null(colors)) {
      palette <- to.palette(colors = colors, type = type)
    } else if (!is.null(palette)) {
      palette <- structure(palette, type = type)
    } else if (!is.null(ramp)) {
      palette <- structure(to.palette(ramp = ramp, type = type))
      ramp <- NULL
    }
  } else {
    if (!is.null(colors)) {
      ramp <- to.ramp(colors = colors, type = type)
      palette <- to.palette(ramp = ramp, type = type)
    } else if (!is.null(palette)) {
      palette <- structure(palette, type = type)
      ramp <- to.ramp(palette = palette, type = type)
    } else if (!is.null(ramp)) {
      ramp <- structure(ramp, type = type)
      palette <- to.palette(ramp = ramp, type = type)
    }
  }
  theme <- list(type = type)
  theme$palette <- palette
  theme$ramp <- ramp
  structure(theme, colors = colors, class = "color.theme")
}

#' Color Theme for Graphics
#'
#' \code{color.theme()} returns an object of class "color.theme" that provides two types of color functions.
#'
#' "color.theme" objects is a container of the two types of color functions: \code{palette(n)} returns a color name vector of length \code{n}, and \code{ramp(x)} returns color names for each values of \code{x} within [0, 1].
#' Some color themes are "qualitative" and do not contain \code{ramp()} function.
#'
#' @param colors one of the following: a color theme name such as "Viridis" with the optional suffix "_r" for color themes in reverse order ("Viridis_r"), a character vector of color names, a palette function, or a ramp function to be used to create a color theme.
#' @param type a character string specifying the type of the color theme: One of "sequential", "qualitative" or "diverging".
#' @param ... optional arguments to be passed to palette or ramp functions.
#' @examples
#' color.theme("Zissou 1")$palette(5L)
#' color.theme("Zissou 1")$ramp(seq.int(0, 1, 1/4))
#' color.theme("Zissou 1_r")$ramp(seq.int(1, 0, -1/4))
#' color.theme("Viridis", alpha = .5)$palette(5L)
#' color.theme("Viridis_r", alpha = .2)$palette(5L)
#' color.theme("midr")$palette(5L)
#' @returns
#' \code{color.theme()} returns a "color.theme" object containing following components:
#' \item{ramp}{the function that takes a numeric vector \code{x} of the values within [0, 1] and returns a color name vector.}
#' \item{palette}{the function that takes an integer \code{n} and returns a color name vector of length \code{n}.}
#' \item{type}{the type of the color theme; "sequential", "diverging" or "qualitative".}
#' @export color.theme
#'
color.theme <- function(
    colors, type = c("sequential", "qualitative", "diverging"), ...) {
  if (is.null(colors))
    return(NULL)
  if (inherits(colors, "color.theme"))
    return(colors)
  type <- if (missing(type)) NULL else match.arg(type)
  if (!is.character(colors) && !is.function(colors))
    stop("'colors' must be a character vector or palette/ramp function")
  if (length(colors) > 1L) {
    type <- ifnot.null(type, "sequential")
    return(wrap.theme(type, colors = colors))
  }
  if (is.palette(colors)) {
    type <- ifnot.null(type, ifnot.null(attr(colors, "type"), "sequential"))
    return(wrap.theme(type, palette = colors))
  }
  if (is.ramp(colors)) {
    type <- ifnot.null(type, ifnot.null(attr(colors, "type"), "sequential"))
    return(wrap.theme(type, ramp = colors))
  }
  if (grepl("_r", colors)) {
    name <- sub("_r", "", colors)
    d <- -1L
  } else {
    name <- colors
    d <- 1L
  }
  # diverging themes
  f <- switch(
    name,
    "midr" = directed(c("#2f7a9a", "#FFFFFF", "#7e1952"), d),
    "grayscale2" = directed(c("white", "gray50", "black"), d),
    NA
  )
  if (is.color(f)) {
    return(wrap.theme(ifnot.null(type, "diverging"), colors = f))
  }
  # sequential themes
  f <- switch(
    name,
    "bluescale" = directed(c("#132B43", "#56B1F7"), d),
    "grayscale" = directed(c("white", "black"), d),
    NA
  )
  if (is.color(f)) {
    return(wrap.theme(ifnot.null(type, "sequential"), colors = f))
  }
  # sequential themes from viridisLite package
  f <- switch(
    name,
    "cividis" = function(n) viridisLite::cividis(n, direction = d, ...),
    "inferno" = function(n) viridisLite::inferno(n, direction = d, ...),
    "magma" = function(n) viridisLite::magma(n, direction = d, ...),
    "mako" = function(n) viridisLite::mako(n, direction = d, ...),
    "plasma" = function(n) viridisLite::plasma(n, direction = d, ...),
    "rocket" = function(n) viridisLite::rocket(n, direction = d, ...),
    "turbo" = function(n) viridisLite::turbo(n, direction = d, ...),
    "viridis" = function(n) viridisLite::viridis(n, direction = d, ...),
    NA)
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "sequential"), palette = f))
  }
  # diverging themes from RColorBrewer package
  names <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu",
            "RdGy", "RdYlBu", "RdYlGn", "Spectral")
  f <- if (any(name == names)) {
    directed(RColorBrewer::brewer.pal(11L, name), d)
  } else NA
  if (is.color(f)) {
    return(wrap.theme(ifnot.null(type, "diverging"), colors = f))
  }
  # sequential themes from RColorBrewer package
  names <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
             "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
             "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
  f <- if (any(name == names)) {
    directed(RColorBrewer::brewer.pal(9L, name), d)
  } else NA
  if (is.color(f)) {
    return(wrap.theme(ifnot.null(type, "sequential"), colors = f))
  }
  # qualitative themes from RColorBrewer package
  f <- switch(
    name,
    "Accent" = directed(RColorBrewer::brewer.pal(8L, name), d),
    "Dark2" = directed(RColorBrewer::brewer.pal(8L, name), d),
    "Paird" = directed(RColorBrewer::brewer.pal(12L, name), d),
    "Pastel1" = directed(RColorBrewer::brewer.pal(9L, name), d),
    "Pastel2" = directed(RColorBrewer::brewer.pal(8L, name), d),
    "Set1" = directed(RColorBrewer::brewer.pal(9L, name), d),
    "Set2" = directed(RColorBrewer::brewer.pal(8L, name), d),
    "Set3" = directed(RColorBrewer::brewer.pal(12L, name), d),
    NA)
  if (is.color(f)) {
    return(wrap.theme(ifnot.null(type, "qualitative"), colors = f))
  }
  # diverging themes from grDevices package
  f <- if (any(name == grDevices::hcl.pals("diverging"))) {
    function(n) grDevices::hcl.colors(n, name, rev = (d < 0), ...)
  } else NA
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "diverging"), palette = f))
  }
  # sequential themes from grDevices package
  f <- if (any(name == grDevices::hcl.pals("sequential"))) {
    function(n) grDevices::hcl.colors(n, name, rev = (d < 0), ...)
  } else NA
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "sequential"), palette = f))
  }
  # sequential themes from grDevices package (divergingx)
  f <- if (any(name == grDevices::hcl.pals("divergingx"))) {
    function(n) grDevices::hcl.colors(n, name, rev = (d < 0), ...)
  } else NA
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "sequential"), palette = f))
  }
  # qualitative themes from grDevices package (hcl.pals)
  f <- if (any(name == grDevices::hcl.pals("qualitative"))) {
    function(n) grDevices::hcl.colors(n, name, rev = (d < 0), ...)
  } else NA
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "qualitative"), palette = f))
  }
  # qualitative themes from grDevices package (palette.pals)
  f <- switch(
    name,
    "R3" = directed(grDevices::palette.colors(8L, name), d),
    "R4" = directed(grDevices::palette.colors(8L, name), d),
    "ggplot2" = directed(grDevices::palette.colors(8L, name), d),
    "Okabe-Ito" = directed(grDevices::palette.colors(8L, name), d),
    "Accent" = directed(grDevices::palette.colors(8L, name), d),
    "Dark 2" = directed(grDevices::palette.colors(8L, name), d),
    "Paired" = directed(grDevices::palette.colors(12L, name), d),
    "Pastel 1" = directed(grDevices::palette.colors(9L, name), d),
    "Pastel 2" = directed(grDevices::palette.colors(8L, name), d),
    "Set 1" = directed(grDevices::palette.colors(9L, name), d),
    "Set 2" = directed(grDevices::palette.colors(8L, name), d),
    "Set 3" = directed(grDevices::palette.colors(12L, name), d),
    "Tableau 10" = directed(grDevices::palette.colors(10L, name), d),
    "Classic Tableau" = directed(grDevices::palette.colors(10L, name), d),
    "Polychrome 36" = directed(grDevices::palette.colors(36L, name), d),
    "Alphabet" = directed(grDevices::palette.colors(26L, name), d),
    NA)
  if (is.color(f)) {
    return(wrap.theme(ifnot.null(type, "qualitative"), colors = f))
  }
  stop(paste0("not implemented palette/ramp name: '", colors, "'"))
}


#' Color Scales for ggplot2 Graphics based on Color Themes
#'
#' \code{scale_color_theme()} and family functions returns color scales for the "colour" and "fill" aesthetics of ggplot objects.
#'
#' @param theme one of the following: a color theme name such as "Viridis", a character vector of color names, a palette function, or a ramp function to be used to create a color theme.
#' @param ... optional arguments to be passed to \code{ggplot2::continuous_scale()} or \code{ggplot2::discrete_scale()}.
#' @param discrete logical. If \code{TRUE}, a discrete scale is returned.
#' @param middle a numeric value specifying the middle point for the diverging color themes.
#' @param aesthetics character string: "fill" or "color".
#' @examples
#' data(txhousing, package = "ggplot2")
#' cities <- c("Houston", "Fort Worth", "San Antonio", "Dallas", "Austin")
#' df <- subset(txhousing, city %in% cities)
#' d <- ggplot2::ggplot(data = df, ggplot2::aes(x = sales, y = median)) +
#'   ggplot2::geom_point(ggplot2::aes(colour = city))
#' d + scale_color_theme("Set 1")
#' d + scale_color_theme("R3")
#' d + scale_color_theme("Blues", discrete = TRUE)
#' d + scale_color_theme("SunsetDark", discrete = TRUE)
#' data(faithfuld, package = "ggplot2")
#' v <- ggplot2::ggplot(faithfuld) +
#'   ggplot2::geom_tile(ggplot2::aes(waiting, eruptions, fill = density))
#' v + scale_fill_theme("Plasma")
#' v + scale_fill_theme("Spectral")
#' v + scale_fill_theme("Spectral_r")
#' v + scale_fill_theme("midr", middle = 0.017)
#' @export scale_color_theme
#'
scale_color_theme <- function(
    theme, ..., discrete = NULL, middle = 0, aesthetics = "colour") {
  theme <- color.theme(theme)
  if (is.null(discrete))
    discrete <- theme$type == "qualitative"
  if (discrete) {
    scale <- ggplot2::discrete_scale(
      aesthetics, palette = theme$palette,
      guide = "legend", ...)
  } else {
    if (theme$type == "qualitative")
      stop("qualitative color theme can't be used for continuous scales")
    rescale_fun <- if (theme$type == "sequential") {
      scales::rescale
    } else if (theme$type == "diverging") {
      function(x, to = c(0, 1), from = range(x, na.rm = TRUE))
        scales::rescale_mid(x, to, from, mid = middle)
    }
    scale <- ggplot2::continuous_scale(
      aesthetics, palette = theme$ramp,
      guide = "colorbar", rescaler = rescale_fun, ...)
  }
  scale
}

#' @rdname scale_color_theme
#' @export scale_colour_theme
#'
scale_colour_theme <- function(
    theme, ..., discrete = NULL, middle = 0, aesthetics = "colour") {
  scale_color_theme(theme = theme, ..., discrete = discrete,
                    middle = middle, aesthetics = aesthetics)
}

#' @rdname scale_color_theme
#' @export scale_fill_theme
#'
scale_fill_theme <- function(
    theme, ..., discrete = NULL, middle = 0, aesthetics = "fill") {
  scale_color_theme(theme = theme, ..., discrete = discrete,
                    middle = middle, aesthetics = aesthetics)
}
