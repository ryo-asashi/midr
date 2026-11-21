# R/colortheme_registry.R

#' List, Register, and Configure Color Themes
#'
#' @description
#' \code{colorthemes()} returns a data frame listing available color themes in the registry.
#' \code{register_colortheme()} registers a custom color theme into the registry.
#' \code{colortheme_options()} sets or retrieves global package options.
#'
#' @details
#' \code{colorthemes()} extracts metadata from the registry environment and structures it into a data frame, allowing users to discover themes by \code{name}, \code{source}, and \code{type} (with case-insensitive prefix matching).
#'
#' \code{register_colortheme()} takes a color vector, a color-generating function, or an existing \code{"colortheme"} object and registers it under a specified \code{name} and \code{source} (defaulting to \code{"custom/newtheme"}).
#' To keep the registry lightweight, the \code{kernel} argument supports lazy loading: pass a character vector or list of length 2 where the first element is an R expression returning a kernel (e.g., \code{"rainbow"}) and the second is the namespace (e.g., \code{"grDevices"}).
#'
#' \code{colortheme_options()} provides a scoped interface to R's global \code{\link[base]{options}()}, automatically prefixing option names with \code{colortheme.}. Supported options are \code{"sequential"}, \code{"diverging"}, \code{"qualitative"} (default theme names or objects), and \code{"registry"} (the active registry environment).
#'
#' @param name for \code{colorthemes()}, a character vector of theme names to match (case-insensitive prefix matching). For \code{register_colortheme()}, a single string for the new theme name.
#' @param source for \code{colorthemes()}, a character vector of sources to match (case-insensitive prefix matching). For \code{register_colortheme()}, a single string for the source name.
#' @param type a character string or vector specifying the theme type(s): \code{"sequential"}, \code{"diverging"}, or \code{"qualitative"}.
#' @param env an environment where color themes are registered. Defaults to the package registry (\code{getOption("colortheme.registry")}).
#' @param kernel a color vector, a palette function, a ramp function, a \code{"colortheme"} object, or a length-2 character vector/list for lazy evaluation. If \code{NULL}, the theme is removed.
#' @param kernel.args a list of arguments passed to the color kernel.
#' @param options a list of option values controlling the color theme's behavior.
#' @param ... for \code{colortheme_options()}, options to be defined in \code{name = value} form, a single named list of options, or character strings specifying option names to retrieve.
#'
#' @returns
#' \itemize{
#'   \item \code{colorthemes()}: A data frame with columns \code{"name"}, \code{"source"}, and \code{"type"}.
#'   \item \code{register_colortheme()}: Invisibly returns the metadata of the previous theme that was overwritten (or \code{NULL} if none existed).
#'   \item \code{colortheme_options()}: A named list of current option values (when called with no arguments or character strings), or invisibly a named list of previous values (when setting options).
#' }
#'
#' @seealso \code{\link{colortheme}}
#'
#' @name colorthemes
#' @keywords internal
NULL


# local helpers

colortheme_env <- new.env(parent = emptyenv())


colortheme_registry <- function() {
  getOption("colortheme.registry", default = colortheme_env)
}


colortheme_default <- function(
    type = c("sequential", "diverging", "qualitative"), use.options = TRUE
) {
  type <- match.arg(type)
  package_default <- switch(
    type, sequential = "bluescale", diverging = "azurerose", qualitative = "hcl"
  )
  if (!use.options) return(package_default)
  getOption(paste0("colortheme.", type), default = package_default)
}


# exports

#' @rdname colorthemes
#' @keywords internal
colorthemes <- function(name = NULL, source = NULL, type = NULL, env = NULL) {
  env <- env %||% colortheme_registry()
  nst <- c("name", "source", "type")
  err <- data.frame(
    name = character(),
    source = character(),
    type = character(),
    stringsAsFactors = FALSE
  )
  lenv <- as.list(env)
  if (!is.null(name)) {
    orgs <- names(lenv)
    nms <- unique(unlist(lapply(name, function(nm) {
      orgs[startsWith(tolower(orgs), tolower(nm))]
    }), use.names = FALSE))
    lenv <- lenv[nms]
  }
  if (length(lenv) == 0L) return(err)
  collect <- function(x, source = NULL) {
    if (!is.null(source)) {
      nms <- names(x)
      srcs <- unique(unlist(lapply(source, function(src) {
        nms[startsWith(tolower(nms), tolower(src))]
      }), use.names = FALSE))
      x <- x[srcs]
    }
    if (!is.list(x) || length(x) == 0L)
      return(matrix(NA_character_, nrow = 3, ncol = 0))
    vapply(
      X = x,
      FUN = function(y) {
        if (is.list(y) && all(nst %in% names(y))) {
          z <- y[nst]
          if (all(vapply(z, is.character, logical(1L))) && all(lengths(z) == 1L))
            return(unlist(z, use.names = FALSE))
        }
        rep.int(NA_character_, 3L)
      },
      FUN.VALUE = character(3L)
    )
  }
  res <- lapply(lenv, collect, source = source)
  mat <- do.call(cbind, res)
  if (is.null(mat) || NCOL(mat) == 0L) return(err)
  res <- as.data.frame(t(mat), stringsAsFactors = FALSE)
  res <- stats::setNames(res, nst)
  res <- res[rowSums(is.na(res)) < 3L, , drop = FALSE]
  if (!is.null(type)) {
    type <- match.arg(
      type, c("sequential", "diverging", "qualitative"), several.ok = TRUE
    )
    res <- res[res$type %in% type, , drop = FALSE]
  }
  if (nrow(res) == 0L) return(err)
  res <- res[order(res$type, res$name, res$source), , drop = FALSE]
  rownames(res) <- NULL
  res
}


#' @rdname colorthemes
#' @keywords internal
register_colortheme <- function(
    kernel, kernel.args = list(), options = list(), name = "newtheme",
    source = "custom", type = NULL, env = NULL
) {
  env <- env %||% colortheme_registry()
  if (is.null(kernel)) {
    old <- env[[name]][[source]]
    env[[name]][[source]] <- NULL
    if (length(env[[name]]) == 0L) remove(list = name, envir = env)
    return(invisible(old))
  }
  if (is_colortheme(kernel)) {
    args <- as.list(kernel)[c("kernel", "kernel.args", "options", "type")]
    args$name <- name %||% kernel$name
    args$source <- source %||% kernel$source
    args$env <- env
    return(do.call(register_colortheme, args))
  }
  lazy_load_kernel(kernel, args = kernel.args)
  if (!is.list(kernel.args))
    stop("'kernel.args' must be a list", call. = FALSE)
  if (!is.list(options))
    stop("'options' must be a list", call. = FALSE)
  if (!is.character(name) || length(name) != 1L)
    stop("'name' must be a character string of length 1", call. = FALSE)
  if (grepl("(/|_r$|@|\\?)", name))
    stop("'name' must not contain the reserved characters: '/', '_r', '@', or '?'", call. = FALSE)
  if (!is.character(source) || length(source) != 1L)
    stop("'source' must be a character string of length 1", call. = FALSE)
  type <- match.arg(type, c("sequential", "diverging", "qualitative"))
  new <- list(
    kernel = kernel, kernel.args = kernel.args,
    options = options, name = name, source = source, type = type
  )
  old <- env[[name]][[source]]
  env[[name]][[source]] <- new
  invisible(old)
}


#' @rdname colorthemes
#' @keywords internal
colortheme_options <- function(...) {
  args <- list(...)
  if (length(args) == 0L) {
    opts <- options()
    opts <- opts[grep("^colortheme\\.", names(opts))]
    if (length(opts) == 0L) return(NULL)
    names(opts) <- sub("^colortheme\\.", "", names(opts))
    return(opts)
  }
  if (length(args) == 1L && is.list(args[[1L]]) && !is.null(names(args[[1L]]))) {
    args <- args[[1L]]
  }
  if (is.null(names(args)) && all(vapply(args, is.character, logical(1L)))) {
    nms <- unlist(args)
    req <- paste0("colortheme.", nms)
    res <- lapply(req, getOption)
    names(res) <- nms
    return(res)
  }
  if (is.null(names(args)) || any(names(args) == "")) {
    stop("options must be specified as 'name = value'", call. = FALSE)
  }
  valid <- c("registry", "sequential", "diverging", "qualitative")
  new <- list()
  for (nm in names(args)) {
    if (!nm %in% valid) {
      warning(
        sprintf("option '%s' might be a typo: expected one of %s",
                nm, paste(paste0("'", valid, "'"), collapse = ", ")),
        call. = FALSE
      )
    }
    new[[paste0("colortheme.", nm)]] <- args[[nm]]
  }
  old <- options(new)
  names(old) <- sub("^colortheme\\.", "", names(old))
  invisible(old)
}


# internal helpers

init_colortheme <- function(
    overwrite.options = FALSE, env = NULL, clear.env = FALSE
) {
  set_colortheme_options(overwrite = overwrite.options, env = env)
  env <- env %||% colortheme_registry()
  if (isTRUE(clear.env)) {
    rm(list = ls(envir = env, all.names = TRUE), envir = env)
  }
  register_themes_colortheme(env = env)
  register_themes_grDevices(env = env)
  register_themes_viridisLite(env = env)
  register_themes_RColorBrewer(env = env)
  register_themes_khroma(env = env)
  invisible(env)
}


set_colortheme_options <- function(overwrite = FALSE, env = NULL) {
  ops <- list(
    colortheme.registry    = env %||% colortheme_env,
    colortheme.sequential  = colortheme_default("sequential", use.options = FALSE),
    colortheme.diverging   = colortheme_default("diverging", use.options = FALSE),
    colortheme.qualitative = colortheme_default("qualitative", use.options = FALSE)
  )
  if (!overwrite) {
    toset <- !(names(ops) %in% names(options()))
    ops <- ops[toset]
  }
  if (length(ops) > 0L) options(ops)
  invisible(ops)
}


register_themes_colortheme <- function(env = colortheme_registry()) {

  register_colortheme(
    name = "hcl", source = "colortheme", type = "qualitative", env = env,
    kernel = c(
      text = paste0(
        "function(n, direction, alpha, chroma, luminance, offset, ...) {",
        "  if (n <= 0L) return(character(0L));",
        "  hues <- seq(offset, offset + 360 * (1 - 1 / n) * sign(direction), length.out = n) %% 360;",
        "  hcl(h = hues, c = chroma, l = luminance, alpha = alpha)",
        "}"
      ),
      namespace = "grDevices"
    ),
    kernel.args = list(
      direction = 1L, alpha = NULL, chroma = 100, luminance = 65, offset = 15
    ),
    options = list(
      kernel.size = Inf,
      reverse.method = "kernel.args$direction <- - kernel.args$direction"
    )
  )
  register_colortheme(
    name = "highlight", source = "colortheme", type = "qualitative", env = env,
    kernel = c(
      text = paste0(
        "function(n, accent, base, which = 1L, alpha = NULL, ...) {",
        "  if (n <= 0L) return(character(0L));",
        "  x <- rep_len(base, n);",
        "  if (!is.list(which)) which <- as.list(which);",
        "  acc <- rep_len(accent, length(which));",
        "  for (i in seq_along(which)) {",
        "    idx <- which[[i]];",
        "    idx <- ifelse(idx < 0L, n + 1L + idx, idx);",
        "    idx <- idx[!is.na(idx) & idx >= 1L & idx <= n];",
        "    x[idx] <- acc[i];",
        "  };",
        "  if (!is.null(alpha) && !is.na(alpha)) x <- adjustcolor(x, alpha.f = alpha);",
        "  x",
        "}"
      ),
      namespace = "grDevices"
    ),
    kernel.args = list(
      accent = c("#D64550", "#2B5C8F", "#E09F3E", "#2A9D8F"),
      base = "#BFBFBF",
      which = list(1L, 5L, 9L, 13L),
      alpha = NULL
    ),
    options = list(
      kernel.size = 16L,
      reverse.method = "kernel.args$accent <- rev(kernel.args$accent)"
    )
  )

  register_colortheme(
    name = "bluescale", source = "colortheme", type = "sequential", env = env,
    kernel = c("#132B43", "#56B1F7"),
    kernel.args = list(mode = "ramp")
  )
  register_colortheme(
    name = "grayscale", source = "colortheme", type = "sequential", env = env,
    kernel = c("#FEFEFE", "#0F0F0F"),
    kernel.args = list(mode = "ramp")
  )

  register_colortheme(
    name = "neon", source = "colortheme", type = "sequential", env = env,
    kernel = c(
      "#1E56C6", "#4B52D4", "#7B49D6", "#A63EC9", "#CD2FB2", "#EA2A91", "#FF3B6B"
    ),
    kernel.args = list(mode = "ramp")
  )
  register_colortheme(
    name = "azurerose", source = "colortheme", type = "diverging", env = env,
    kernel = c(
      "#005AAD", "#3B78C2", "#729BD6", "#AFC4E8", "#F7F7F7", "#E8B3CC", "#D67FA8", "#BD4989", "#A4136E"
    ),
    kernel.args = list(mode = "ramp")
  )

  register_colortheme(
    name = "steps", source = "colortheme", type = "diverging", env = env,
    kernel = c("#2B5C8F", "#EAEAEA", "#D64550"),
    options = list(ramp.steps = 11L)
  )
  register_colortheme(
    name = "bluesteps", source = "colortheme", type = "sequential", env = env,
    kernel = c("#132B43", "#56B1F7"),
    options = list(ramp.steps = 11L)
  )

  register_colortheme(
    name = "ukiyoe", source = "colortheme", type = "qualitative", env = env,
    kernel = c(
      aitetsu = "#1D3557", bengara = "#C85A32", yamabuki = "#E09F3E",
      aotake = "#2A9D8F", edomurasaki = "#6D597A", rikyunezumi = "#8D99AE"
    ),
    kernel.args = list(mode = "palette")
  )
  register_colortheme(
    name = "kasane", source = "colortheme", type = "diverging", env = env,
    kernel = c(
      konjo = "#1A365D", hanada = "#3B6998", byakugun = "#84A9C0", kinari = "#F4F4F0",
      ikkonzome = "#E8A598", sohi = "#D16654", kokihi = "#9E2A2B"
    ),
    kernel.args = list(mode = "ramp")
  )
  register_colortheme(
    name = "ai", source = "colortheme", type = "sequential", env = env,
    kernel = c(
      kamenozoki = "#E6F2F2", mizuasagi = "#A8D5BA", asagi = "#56A3A6",
      nando = "#2B728E", ai = "#1D4E6E", kachigaeshi = "#0D2538"
    ),
    kernel.args = list(mode = "ramp")
  )
  register_colortheme(
    name = "shiki", source = "colortheme", type = "qualitative", env = env,
    kernel = c(
      koubai = "#B8336A", usubeni = "#E58FB1", tokiwa = "#2D6A4F", wakatake = "#74C69D",
      kohaku = "#B5651D", kariyasu = "#E9C46A", tetsukon = "#264653", hisoku = "#8AB17D"
    ),
    kernel.args = list(mode = "palette")
  )
  invisible(NULL)
}


register_themes_grDevices <- function(env = colortheme_registry()) {
  if (!requireNamespace("grDevices", quietly = TRUE)) return(invisible(NULL))

  hcl_types <- c(
    sequential  = "sequential",
    diverging   = "diverging",
    divergingx  = "diverging",
    qualitative = "qualitative"
  )
  for (pal_type in names(hcl_types)) {
    theme_type <- hcl_types[[pal_type]]
    for (x in grDevices::hcl.pals(type = pal_type)) {
      register_colortheme(
        name = x, source = "grDevices", type = theme_type, env = env,
        kernel = c(text = "hcl.colors", namespace = "grDevices"),
        kernel.args = list(palette = x, alpha = NULL, rev = FALSE),
        options = list(
          kernel.size = Inf,
          reverse.method = "kernel.args$rev <- !kernel.args$rev"
        )
      )
    }
  }

  for (x in grDevices::palette.pals()) {
    register_colortheme(
      name = x, source = "grDevices", type = "qualitative", env = env,
      kernel = c(text = "palette.colors", namespace = "grDevices"),
      kernel.args = list(palette = x, alpha = NULL, recycle = FALSE),
      options = list(kernel.size = switch(
        x, 8L, Paired = 12L, "Pastel 1" = 9L, "Set 1" = 9L, "Set 3" = 12L,
        "Tableau 10" = 10L, "Classic Tableau" = 10L, "Polychrome 36" = 36L,
        Alphabet = 26L
      ))
    )
  }
  invisible(NULL)
}


register_themes_viridisLite <- function(env = colortheme_registry()) {
  if (!requireNamespace("viridisLite", quietly = TRUE)) return(invisible(NULL))

  pals <- c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")
  for (x in pals) {
    register_colortheme(
      name = x, source = "viridisLite", type = "sequential", env = env,
      kernel = c(text = "viridis", namespace = "viridisLite"),
      kernel.args = list(option = x, alpha = 1, begin = 0.2, end = 0.8, direction = 1),
      options = list(
        kernel.size = Inf,
        reverse.method = "kernel.args$direction <- - kernel.args$direction"
      )
    )
  }
  invisible(NULL)
}


register_themes_RColorBrewer <- function(env = colortheme_registry()) {
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) return(invisible(NULL))

  info <- RColorBrewer::brewer.pal.info
  for (i in seq_len(nrow(info))) {
    x <- rownames(info)[i]
    type <- as.character(info[i, 2L])
    register_colortheme(
      name = x, source = "RColorBrewer", type = type, env = env,
      kernel = c(
        text = sprintf("brewer.pal(n = %d, name = '%s')", info[i, 1L], x),
        namespace = "RColorBrewer"
      )
    )
  }
  invisible(NULL)
}


register_themes_khroma <- function(env = colortheme_registry()) {
  if (!requireNamespace("khroma", quietly = TRUE)) return(invisible(NULL))

  info <- khroma::info()
  info <- info[
    info$type %in% c("sequential", "diverging", "qualitative"), , drop = FALSE
  ]
  for (i in seq_len(nrow(info))) {
    x <- info[i, 1L]
    register_colortheme(
      name = x, source = "khroma", type = info[i, 2L], env = env,
      kernel = c(text = sprintf("color(palette = '%s')", x), namespace = "khroma"),
      kernel.args = list(range = c(0.0, 1.0)),
      options = list(kernel.size = info[i, 3L], na.color = info[i, 4L])
    )
  }
  invisible(NULL)
}
