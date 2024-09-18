#'
#' An Example from the Paper by Iwasawa & Otsuka
#'
library(midr)
z <- seq(-6, 6, length.out=100)
x1 <- rep(z, each = 100)
x2 <- x1 ^ 2 - z ^ 2
X <- weighted(data.frame(x1, x2), dnorm(x1) * dnorm(z))
# one-dimensional MID decomposition
mid <- interpret(x1 * x2 ~ x1 + x2, X, k = c(100, 20))
mid.plots(mid, engine = "base")
summary(mid)
# two-dimensional MID decomposition
mid2 <- interpret(x1 * x2 ~ (x1 + x2)^2, X, k = c(100, 20))
mid.plots(mid2, terms(mid2), engine = "base")
summary(mid2)
# comparison of x1 main effects
plot(mid, "x1")
curve(mid.f(mid2, "x1", x), add = TRUE, lty = 3)
