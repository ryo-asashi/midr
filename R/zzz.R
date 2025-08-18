kernel.env <- rlang::env(rlang::ns_env("midr"))

.onLoad <- function(libname, pkgname) {
  # midr --------
  set.color.theme(
    name = "midr", source = "midr", type = "diverging",
    kernel = c("#005AAD", "#688BC0", "#A8B8D6", "#D7DCE7", "#F1F1F1", "#E9D8DF",
               "#D9ABBF", "#C27098", "#A4136E"),
    kernel.args = list(mode = "ramp")
  )
  set.color.theme(
    name = "bluescale", source = "midr", type = "sequential",
    kernel = c("#132B43", "#56B1F7"),
    kernel.args = list(mode = "ramp")
  )
  set.color.theme(
    name = "grayscale", source = "midr", type = "sequential",
    kernel = c("#EFEFEF", "#1F1F1F"),
    kernel.args = list(mode = "ramp")
  )
  set.color.theme(
    name = "shap", source = "midr", type = "sequential",
    kernel = c("#2C87E1", "#2A6BE9", "#774DCF", "#9C30BB", "#C60099", "#E7007E",
               "#F72A5A"),
    kernel.args = list(mode = "ramp")
  )
  set.color.theme(
    name = "HCL", source = "midr", type = "qualitative",
    kernel = c(text = "hcl.palette", namespace = "midr"),
    kernel.args = list(direction = 1L, alpha = NULL, chroma = 100, luminance = 65)
  )
  set.color.theme(
    name = "bicolor", source = "midr", type = "diverging",
    kernel = c(text = "function(x, first, second) ifelse(x <= 0.5, first, second)",
               namespace = "base"),
    kernel.args = list(first = "#6A0DAD", second = "#00A0A0")
  )
  set.color.theme(
    name = "highlight", source = "midr", type = "qualitative",
    kernel = c(text = paste0("function(n, accent, base, which = 1L) {",
                             "ret <- rep(base, n);",
                             "if (which <= n) ret[which] <- accent;",
                             "ret}"), namespace = "base"),
    kernel.args = list(accent = "#55AAA5", base = "gray75", which = 1L)
  )
  # grDevices --------
  if (requireNamespace("grDevices", quietly = TRUE)) {
    for (x in grDevices::hcl.pals(type = "sequential")) {
      set.color.theme(
        name = x, source = "grDevices", type = "sequential",
        kernel = c(text = "hcl.colors", namespace = "grDevices"),
        kernel.args = list(palette = x, alpha = 1, rev = FALSE),
        options = list(
          kernel.size = Inf,
          reverse.method = "kernel.args$rev <- !kernel.args$rev"
        )
      )
    }
    for (x in grDevices::hcl.pals(type = "diverging")) {
      set.color.theme(
        name = x, source = "grDevices", type = "diverging",
        kernel = c(text = "hcl.colors", namespace = "grDevices"),
        kernel.args = list(palette = x, alpha = 1, rev = FALSE),
        options = list(
          kernel.size = Inf,
          reverse.method = "kernel.args$rev <- !kernel.args$rev"
        )
      )
    }
    for (x in grDevices::hcl.pals(type = "divergingx")) {
      set.color.theme(
        name = x, source = "grDevices", type = "diverging",
        kernel = c(text = "hcl.colors", namespace = "grDevices"),
        kernel.args = list(palette = x, alpha = 1, rev = FALSE),
        options = list(
          kernel.size = Inf,
          reverse.method = "kernel.args$rev <- !kernel.args$rev"
        )
      )
    }
    for (x in grDevices::hcl.pals(type = "qualitative")) {
      set.color.theme(
        name = x, source = "grDevices", type = "qualitative",
        kernel = c(text = "hcl.colors", namespace = "grDevices"),
        kernel.args = list(palette = x, alpha = 1, rev = FALSE),
        options = list(
          kernel.size = Inf,
          reverse.method = "kernel.args$rev <- !kernel.args$rev"
        )
      )
    }
    for (x in grDevices::palette.pals()) {
      set.color.theme(
        name = x, source = "grDevices", type = "qualitative",
        kernel = c(text = "palette.colors", namespace = "grDevices"),
        kernel.args = list(palette = x, alpha = 1, recycle = FALSE),
        options = list(kernel.size = switch(
          x, 8L, Paird = 12L, "Pastel 1" = 9L, "Set 1" = 9L, "Set 3" = 12L,
          "Tableau 10" = 10L, "Classic Tableau" = 10L, "Polychrome 36" = 36L,
          Alphabet = 26L
        ))
      )
    }
  }
  # viridisLite --------
  if (requireNamespace("viridisLite", quietly = TRUE)) {
    for (x in c("magma", "inferno", "plasma", "viridis", "cividis", "rocket",
                "mako", "turbo")) {
      set.color.theme(
        name = x, source = "viridisLite", type = "sequential",
        kernel = c(text = "viridis", namespace = "viridisLite"),
        kernel.args = list(option = x, alpha = 1, begin = 0, end = 1, direction = 1),
        options = list(
          kernel.size = Inf,
          reverse.method = "kernel.args$direction <- - kernel.args$direction"
        )
      )
    }
  }
  # RColorBrewer --------
  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    info <- RColorBrewer::brewer.pal.info
    for (i in seq_len(nrow(info))) {
      x <- rownames(info)[i]
      set.color.theme(
        name = x, source = "RColorBrewer", type = info[i, 2L],
        kernel = c(
          text = sprintf("brewer.pal(n = %d, name = '%s')", info[i, 1L], x),
          namespace = "RColorBrewer"
        )
      )
    }
  }
  # khroma --------
  if (requireNamespace("khroma", quietly = TRUE)) {
    info <- khroma::info()
    for (i in seq_len(nrow(info))) {
      x <- info[i, 1L]
      set.color.theme(
        name = x, source = "khroma", type = info[i, 2L],
        kernel = c(text = sprintf("color(palette = '%s')", x),
                   namespace = "khroma"),
        kernel.args = list(range = c(0, 1)),
        options = list(kernel.size = info[i, 3L],
                       na.color = info[i, 4L])
      )
    }
  }
}
