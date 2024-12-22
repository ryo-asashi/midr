library(hexSticker)
library(midr)
library(ggplot2)
theme_set(theme_void())

z <- seq(-4, 4, length.out=100)
x1 <- rep(z, each = 100)
x2 <- x1 ^ 2 - z ^ 2
X <- weighted(data.frame(x1, x2), dnorm(x1) * dnorm(z))
mid <- interpret(x1 * x2 ~ (x1 + x2)^2, X,
                 k = c(40, 15), lambda = 1)
p <- ggmid(mid, "x1:x2", scale.type = "viridis",
      interaction.type = "rectangle") +
  guides(fill = "none")

st <- sticker(
  subplot = p,
  package = " ",
  p_family = "serif",
  p_size = 28,
  p_x = 1,
  p_y = 1.5,
  s_x = 1,
  s_y = 1,
  s_width = 1.9,
  s_height = 2.2,
  p_color = "#454565",
  h_fill = "#440155",
  h_color = "#440155",
  filename = "man/figures/logo_raw.png"
)

st

theme_sticker <- function (size = 1.2, ...)
{
  center <- 1
  radius <- 1
  h <- radius
  w <- sqrt(3)/2 * radius
  m <- 1.02
  list(theme_transparent() + theme(plot.margin = margin(unit = "lines"), strip.text = element_blank(),
                                   line = element_blank(), text = element_blank(), title = element_blank(),
                                   ...), coord_fixed(), scale_y_continuous(expand = c(0,
                                                                                      0), limits = c(center - h * m, center + h * m)), scale_x_continuous(expand = c(0,
                                                                                                                                                                     0), limits = c(center - w * m, center + w * m)))
}


sticker <- function (subplot, s_x = 0.8, s_y = 0.75, s_width = 0.4, s_height = 0.5,
          package, p_x = 1, p_y = 1.4, p_color = "#FFFFFF", p_family = "Aller_Rg",
          p_fontface = "plain", p_size = 8, h_size = 1.2, h_fill = "#1881C2",
          h_color = "#87B13F", spotlight = FALSE, l_x = 1, l_y = 0.5,
          l_width = 3, l_height = 3, l_alpha = 0.4, url = "", u_x = 1,
          u_y = 0.08, u_color = "black", u_family = "Aller_Rg", u_size = 1.5,
          u_angle = 30, white_around_sticker = FALSE, ..., filename = paste0(package,
                                                                             ".png"), asp = 1, dpi = 300)
{
  hex <- ggplot() + geom_hexagon(size = h_size, fill = h_fill,
                                 color = NA)
  if (inherits(subplot, "character")) {
    d <- data.frame(x = s_x, y = s_y, image = subplot)
    sticker <- hex + geom_image(aes_(x = ~x, y = ~y, image = ~image),
                                d, size = s_width, asp = asp)
  }
  else {
    sticker <- hex + geom_subview(subview = subplot, x = s_x,
                                  y = s_y, width = s_width, height = s_height)
  }
  sticker <- sticker + geom_hexagon(size = h_size, fill = NA,
                                    color = h_color)
  if (spotlight)
    sticker <- sticker + geom_subview(subview = spotlight(l_alpha),
                                      x = l_x, y = l_y, width = l_width, height = l_height)
  sticker <- sticker + geom_pkgname(package, p_x, p_y, color = p_color,
                                    family = p_family, fontface = p_fontface, size = p_size,
                                    ...)
  sticker <- sticker + geom_url(url, x = u_x, y = u_y, color = u_color,
                                family = u_family, size = u_size, angle = u_angle)
  if (white_around_sticker)
    sticker <- sticker + white_around_hex(size = h_size)
  sticker <- sticker + theme_sticker(size = h_size)
  save_sticker(filename, sticker, dpi = dpi)
  class(sticker) <- c("sticker", class(sticker))
  invisible(sticker)
}
