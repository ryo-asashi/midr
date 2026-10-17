# ggplot wrappers

standardize_param_names <- function(dots) {
  names(dots) <- ggplot2::standardise_aes_names(names(dots))
  dots[!duplicated(names(dots), fromLast = TRUE)]
}

filter_params <- function(dots, allowed) {
  dots[names(dots) %in% allowed]
}

# basic dotted geom

.geom_point <- function(
    mapping = NULL, data = NULL, ...
) {
  allowed_params <- c(
    "colour", "alpha", "shape", "size", "fill", "stroke",
    "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
  )
  dots <- standardize_param_names(list(...))
  args <- c(
    list(mapping = mapping, data = data),
    filter_params(dots, allowed_params)
  )
  do.call(ggplot2::geom_point, args)
}

.geom_line <- function(
    mapping = NULL, data = NULL, ...
) {
  allowed_params <- c(
    "colour", "alpha", "linetype", "linewidth", "lineend", "linejoin", "linemitre", "arrow",
    "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
  )
  dots <- standardize_param_names(list(...))
  args <- c(
    list(mapping = mapping, data = data),
    filter_params(dots, allowed_params)
  )
  do.call(ggplot2::geom_line, args)
}

.geom_linerange <- function(
    mapping = NULL, data = NULL, ...
) {
  allowed_params <- c(
    "colour", "alpha", "linetype", "linewidth",
    "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
  )
  dots <- standardize_param_names(list(...))
  args <- c(
    list(mapping = mapping, data = data),
    filter_params(dots, allowed_params)
  )
  do.call(ggplot2::geom_linerange, args)
}

.geom_jitter <- function(
    mapping = NULL, data = NULL, ...
) {
  allowed_params <- c(
    "stat", "na.rm", "show.legend", "inherit.aes",
    "colour", "alpha", "shape", "size", "fill", "stroke"
  )
  dots <- standardize_param_names(list(...))
  haspos <- any(c("width", "height", "seed") %in% names(dots))
  pos <- dots$position
  if (haspos && is.null(pos)) {
    pos.args <- filter_params(dots, c("width", "height", "seed"))
    pos <- do.call(ggplot2::position_jitter, pos.args)
  }
  args <- c(
    list(mapping = mapping, data = data),
    filter_params(dots, allowed_params)
  )
  args$position <- pos
  do.call(ggplot2::geom_jitter, args)
}

.geom_col <- function(
    mapping = NULL, data = NULL, ...
) {
  allowed_params <- c(
    "colour", "fill", "alpha", "linewidth", "linetype", "width", "just",
    "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
  )
  dots <- standardize_param_names(list(...))
  args <- c(
    list(mapping = mapping, data = data),
    filter_params(dots, allowed_params)
  )
  do.call(ggplot2::geom_col, args)
}

.geom_path <- function(
    mapping = NULL, data = NULL, ...
) {
  allowed_params <- c(
    "colour", "alpha", "linetype", "linewidth", "lineend", "linejoin", "linemitre", "arrow",
    "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
  )
  dots <- standardize_param_names(list(...))
  args <- c(
    list(mapping = mapping, data = data),
    filter_params(dots, allowed_params)
  )
  do.call(ggplot2::geom_path, args)
}

.geom_raster <- function(
    mapping = NULL, data = NULL, ...
) {
  allowed_params <- c(
    "fill", "alpha", "interpolate", "hjust", "vjust",
    "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
  )
  dots <- standardize_param_names(list(...))
  args <- c(
    list(mapping = mapping, data = data),
    filter_params(dots, allowed_params)
  )
  do.call(ggplot2::geom_raster, args)
}

.geom_rect <- function(
    mapping = NULL, data = NULL, ...
) {
  allowed_params <- c(
    "colour", "fill", "alpha", "linewidth", "linetype", "linejoin",
    "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
  )
  dots <- standardize_param_names(list(...))
  args <- c(
    list(mapping = mapping, data = data),
    filter_params(dots, allowed_params)
  )
  do.call(ggplot2::geom_rect, args)
}

.geom_boxplot <- function(
    mapping = NULL, data = NULL, ...
) {
  allowed_params <- c(
    "colour", "fill", "alpha", "linewidth", "linetype", "shape", "size",
    "width", "outlier.colour", "outlier.color", "outlier.fill", "outlier.shape",
    "outlier.size", "outlier.stroke", "outlier.alpha", "notch", "notchwidth",
    "varwidth", "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
  )
  dots <- standardize_param_names(list(...))
  args <- c(
    list(mapping = mapping, data = data),
    filter_params(dots, allowed_params)
  )
  do.call(ggplot2::geom_boxplot, args)
}

.geom_tile <- function(
    mapping = NULL, data = NULL, ...
) {
  allowed_params <- c(
    "colour", "fill", "alpha", "linewidth", "linetype", "width", "height",
    "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
  )
  dots <- standardize_param_names(list(...))
  args <- c(
    list(mapping = mapping, data = data),
    filter_params(dots, allowed_params)
  )
  do.call(ggplot2::geom_tile, args)
}

# compound dotted geom

.geom_linepoint <- function(
    mapping = NULL, data = NULL, ...
) {
  list(
    .geom_line(mapping = mapping, data = data, ...),
    .geom_point(mapping = mapping, data = data, ...)
  )
}

.geom_dotchart <- function(
    mapping = NULL, data = NULL, ...
) {
  dots <- standardize_param_names(list(...))
  linedots <- dots[names(dots) != "colour"]
  linetheme <- ggplot2::theme_get()$line
  linedefault <- list(
    colour = linetheme$colour %||% "black",
    linetype = 3L,
    linewidth = linetheme$linewidth %||% 0.5
  )
  lineargs <- c(
    list(mapping = mapping, data = data),
    utils::modifyList(linedefault, linedots)
  )
  mapping$xmin <- mapping$xmax <- mapping$ymin <- mapping$ymax <- NULL
  pointargs <- c(list(mapping = mapping, data = data), dots)
  list(
    do.call(.geom_linerange, lineargs),
    do.call(.geom_point, pointargs)
  )
}
