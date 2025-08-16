kernel.env <- rlang::env(rlang::ns_env("midr"))

.onLoad <- function(libname, pkgname) {
  # midr --------
  set.color.theme(
    name = "midr", source = "midr", type = "diverging",
    kernel = as.ramp(c("#005AAD", "#688BC0", "#A8B8D6", "#D7DCE7", "#F1F1F1",
                       "#E9D8DF", "#D9ABBF", "#C27098", "#A4136E")),
    kernel.args = list(direction = 1L, alpha = 1)
  )
  set.color.theme(
    name = "bluescale", source = "midr", type = "sequential",
    kernel = as.ramp(c("#132B43", "#56B1F7")),
    kernel.args = list(direction = 1L, alpha = 1)
  )
  set.color.theme(
    name = "grayscale", source = "midr", type = "sequential",
    kernel = as.ramp(c("white", "black")),
    kernel.args = list(direction = 1L, alpha = 1)
  )
  set.color.theme(
    name = "grayscale", source = "midr", type = "sequential",
    kernel = as.ramp(c("white", "black")),
    kernel.args = list(direction = 1L, alpha = 1)
  )
  set.color.theme(
    name = "shap", source = "midr", type = "sequential",
    kernel = as.ramp(c("#2C87E1", "#2A6BE9", "#774DCF", "#9C30BB", "#C60099",
                       "#E7007E", "#F72A5A")),
    kernel.args = list(direction = 1L, alpha = 1)
  )
  set.color.theme(
    name = "HCL", source = "midr", type = "qualitative", kernel = hcl.palette,
    kernel.args = list(direction = 1L, alpha = NULL, chroma = 100, luminance = 65)
  )
  # grDevices --------
  if (requireNamespace("grDevices", quietly = TRUE)) {
    for (x in grDevices::hcl.pals(type = "sequential")) {
      set.color.theme(
        name = x, source = "grDevices", type = "sequential",
        kernel = grDevices::hcl.colors,
        kernel.args = list(palette = x, alpha = 1)
      )
    }
    for (x in grDevices::hcl.pals(type = "diverging")) {
      set.color.theme(
        name = x, source = "grDevices", type = "diverging",
        kernel = grDevices::hcl.colors,
        kernel.args = list(palette = x, alpha = 1)
      )
    }
    for (x in grDevices::hcl.pals(type = "divergingx")) {
      set.color.theme(
        name = x, source = "grDevices", type = "diverging",
        kernel = grDevices::hcl.colors,
        kernel.args = list(palette = x, alpha = 1)
      )
    }
    for (x in grDevices::hcl.pals(type = "qualitative")) {
      set.color.theme(
        name = x, source = "grDevices", type = "qualitative",
        kernel = grDevices::hcl.colors,
        kernel.args = list(palette = x, alpha = 1),
        options = list(palette.max = 256L)
      )
    }
    for (x in grDevices::palette.pals()) {
      set.color.theme(
        name = x, source = "grDevices", type = "qualitative",
        kernel = grDevices::palette.colors,
        kernel.args = list(palette = x, alpha = 1, recycle = FALSE),
        options = list(palette.max = switch(
          x, 8L, Paird = 12L, "Pastel 1" = 9L, "Set 1" = 9L, "Set 3" = 12L,
          "Tableau 10" = 10L, "Classic Tableau" = 10L, "Polychrome 36" = 36L,
          Alphabet = 26L
        ))
      )
    }
  }
}
