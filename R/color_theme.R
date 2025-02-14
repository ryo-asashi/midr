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
  if (isTRUE(type == "qualitative"))
    return(NULL)
  if (is.null(colors)) {
    if (is.null(palette))
      stop("'colors' or 'palette' must be supplied")
    if (!is.palette(palette))
      stop("invalid 'palette' supplied")
    ptype <- attr(palette, "type")
    if (isTRUE(ptype == "qualitative")) {
      message("converting qualitative palette to sequential color ramp")
      attr(palette, "type") <- "sequential"
    }
    colors <- palette(ifnot.null(attr(palette, "n"), 7L))
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
  if (isTRUE(type == "qualitative") && !is.null(colors)) {
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
    type, palette = NULL, ramp = NULL, colors = NULL, name = NULL) {
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
  theme$name <- name
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
#' @param name an optional character string, specifying the name of the color theme.
#' @param ... optional arguments to be passed to palette or ramp functions.
#' @examples
#' ct <- color.theme("Mako")
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, 1/4))
#' ct <- color.theme("RdBu")
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, 1/4))
#' ct <- color.theme("Tableau 10")
#' ct$palette(10L)
#' pals <- c("midr", "grayscale", "bluescale", hcl.pals(), palette.pals())
#' par(mfrow = c(5L, 2L))
#' for (pal in pals[1:10]) plot(color.theme(pal))
#' for (pal in pals[11:20]) plot(color.theme(pal))
#' for (pal in pals[21:30]) plot(color.theme(pal))
#' for (pal in pals[31:40]) plot(color.theme(pal))
#' for (pal in pals[41:50]) plot(color.theme(pal))
#' for (pal in pals[51:60]) plot(color.theme(pal))
#' for (pal in pals[61:70]) plot(color.theme(pal))
#' for (pal in pals[71:80]) plot(color.theme(pal))
#' for (pal in pals[81:90]) plot(color.theme(pal))
#' for (pal in pals[91:100]) plot(color.theme(pal))
#' for (pal in pals[101:110]) plot(color.theme(pal))
#' for (pal in pals[111:120]) plot(color.theme(pal))
#' for (pal in pals[121:130]) plot(color.theme(pal))
#' for (pal in pals[131:134]) plot(color.theme(pal))
#' par(mfrow = c(1L, 1L))
#' @returns
#' \code{color.theme()} returns a "color.theme" object containing following components:
#' \item{ramp}{the function that takes a numeric vector \code{x} of the values within [0, 1] and returns a color name vector.}
#' \item{palette}{the function that takes an integer \code{n} and returns a color name vector of length \code{n}.}
#' \item{type}{the type of the color theme; "sequential", "diverging" or "qualitative".}
#' \item{name}{the name of the color theme.}
#' @export color.theme
#'
color.theme <- function(
    colors, type = c("sequential", "qualitative", "diverging"),
    name = NULL, ...) {
  if (is.null(colors))
    return(NULL)
  if (inherits(colors, "color.theme"))
    return(colors)
  type <- if (missing(type)) NULL else match.arg(type)
  if (!is.character(colors) && !is.function(colors))
    stop("'colors' must be a character vector or palette/ramp function")
  if (length(colors) > 1L) {
    type <- ifnot.null(type, "sequential")
    return(wrap.theme(type, colors = colors, name = name))
  }
  if (is.palette(colors)) {
    type <- ifnot.null(type, ifnot.null(attr(colors, "type"), "sequential"))
    return(wrap.theme(type, palette = colors, name = name))
  }
  if (is.ramp(colors)) {
    type <- ifnot.null(type, ifnot.null(attr(colors, "type"), "sequential"))
    return(wrap.theme(type, ramp = colors, name = name))
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
    "midr" = to.palette(directed(c("#005587", "#FFFFFF", "#8B0058"), d)),
    NA
  )
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "diverging"), palette = f, name = name))
  }
  # sequential themes
  f <- switch(
    name,
    "bluescale" = to.palette(directed(c("#132B43", "#56B1F7"), d)),
    "grayscale" = to.palette(directed(c("white", "black"), d)),
    NA
  )
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "sequential"), palette = f, name = name))
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
    return(wrap.theme(ifnot.null(type, "sequential"), palette = f, name = name))
  }
  # diverging themes from RColorBrewer package
  names <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
  f <- if (any(name == names)) {
    try(directed(RColorBrewer::brewer.pal(11L, name), d), silent = TRUE)
  } else NA
  if (is.color(f)) {
    return(wrap.theme(ifnot.null(type, "diverging"), colors = f, name = name))
  }
  # sequential themes from RColorBrewer package
  names <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
             "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
             "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
  f <- if (any(name == names)) {
    try(directed(RColorBrewer::brewer.pal(9L, name), d), silent = TRUE)
  } else NA
  if (is.color(f)) {
    return(wrap.theme(ifnot.null(type, "sequential"), colors = f, name = name))
  }
  # qualitative themes from RColorBrewer package
  names <- c("Accent", "Dark2", "Paird", "Pastel1", "Pastel2", "Set1", "Set2", "Set3")
  if (any(name == names)) {
    npal <- switch(name, Paird = 12L, Pastel1 = 9L, Pastel2 = 8L, Set1 = 9L, 8L)
    f <- try(directed(RColorBrewer::brewer.pal(npal, name), d), silent = TRUE)
  } else {
    f <- NA
  }
  if (is.color(f)) {
    return(wrap.theme(ifnot.null(type, "qualitative"), colors = f, name = name))
  }
  # diverging themes from grDevices package
  f <- if (any(name == grDevices::hcl.pals("diverging"))) {
    function(n) grDevices::hcl.colors(n, name, rev = (d < 0), ...)
  } else NA
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "diverging"), palette = f, name = name))
  }
  # diverging themes from grDevices package
  f <- if (any(name == grDevices::hcl.pals("divergingx"))) {
    function(n) grDevices::hcl.colors(n, name, rev = (d < 0), ...)
  } else NA
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "diverging"), palette = f, name = name))
  }
  # sequential themes from grDevices package
  f <- if (any(name == grDevices::hcl.pals("sequential"))) {
    function(n) grDevices::hcl.colors(n, name, rev = (d < 0), ...)
  } else NA
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "sequential"), palette = f, name = name))
  }
  # qualitative themes from grDevices package (hcl.pals)
  f <- if (any(name == grDevices::hcl.pals("qualitative"))) {
    function(n) grDevices::hcl.colors(n, name, rev = (d < 0), ...)
  } else NA
  if (is.palette(f)) {
    return(wrap.theme(ifnot.null(type, "qualitative"), palette = f, name = name))
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
    return(wrap.theme(ifnot.null(type, "qualitative"), colors = f, name = name))
  }
  stop(paste0("not implemented palette/ramp name: '", colors, "'"))
}

#' @rdname color.theme
#' @param x a "color.theme" object to be displayed.
#' @param n integer. the number of colors.
#' @param text a character string to be displayed.
#' @exportS3Method base::plot
#'
plot.color.theme <- function(x, n = NULL, text = x$name, ...) {
  if (is.null(n))
    n <- if (x$type == "qualitative") {
      ifnot.null(attr(x$palette, "n"), 8L)
    } else 100L
  opar <- graphics::par("mai", "mar")
  on.exit(graphics::par(opar))
  graphics::par(mar = c(1, 1, 1, 1))
  graphics::plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
                 axes = FALSE, xlab = "", ylab = "")
  graphics::text(0.5, 0.7, text, pos = 3L)
  graphics::rect((seq_len(n) - 1L) / n, 0.3, seq_len(n) / n, 0.7,
                 col = x$palette(n), border = NA)
}

#' @rdname color.theme
#' @param x a "color.theme" object to be displayed.
#' @param display logical. If \code{TRUE}, colors are displayed in the plot area.
#' @exportS3Method base::print
#'
print.color.theme <- function(x, display = TRUE, ...) {
  type <- switch(x$type, qualitative = "Qualitative",
                 diverging = "Diverging", sequential = "Sequential")
  text <- paste0(type, " Color Theme")
  if (!is.null(x$name)) text <- paste0(text, ' : "', x$name, '" ')
  cat(text)
  if (display)
    plot.color.theme(x, text = text)
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


rescale <- function(x, middle = NULL) {
  if (is.character(x))
    x <- as.factor(x)
  if (is.factor(x) || is.logical(x))
    x <- as.numeric(x)
  from <- range(x, na.rm = TRUE, finite = TRUE)
  if (is.null(middle)) {
    d <- from[2L] - from[1L]
    if (d == 0)
      return(ifelse(is.na(x), NA, 0.5))
    res <- (x - from[1L]) / d
  } else {
    d <- 2 * max(abs(from - middle))
    if (d == 0)
      return(ifelse(is.na(x), NA, 0.5))
    res <- (x - middle) / d + 0.5
  }
  pmax(0, pmin(1, res))
}


to.colors <- function(x, theme, middle = 0, na.value = "gray50") {
  theme <- color.theme(theme)
  if (is.discrete(x)) {
    x <- as.integer(as.factor(x))
    cols <- theme$palette(max(x, na.rm = TRUE))[x]
  } else {
    if (theme$type == "qualitative") {
      stop("qualitative color theme can't be used for discrete variable")
    } else if (theme$type == "sequential") {
      cols <- theme$ramp(rescale(x))
    } else if (theme$type == "diverging") {
      cols <- theme$ramp(rescale(x, middle = middle))
    } else
      cols <- rep.int(1L, length(x))
  }
  cols[is.na(cols)] <- na.value
  cols
}
