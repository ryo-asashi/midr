#'
#' Iterative Decomposition vs All At Once Decomposition
#'
library(midr)
library(ggplot2)
m <- 10L
v <- seq(-6, 6, length.out = m)
z <- rep(v, times = m ^ 3)
u1 <- rep(v, times = m ^ 2, each = m)
u2 <- rep(v, times = m, each = m ^ 2)
u3 <- rep(v, each = m ^ 3)
w <- dnorm(z) * dnorm(u1) * dnorm(u2) * dnorm(u3)
x <- weighted(data.frame(z, x1 = u1 + z, x2 = u2 + z, x3 = u3 + z), w)
y <- x$x1 * x$x2 + x$x1 * x$x3 + x$x2 * x$x3
# one-dimensional MID decomposition
mid <- interpret(y ~ x1 + x2 + x3, data = x, k = 0)
summary(mid)
# uninterpreted rate : 24.2...%
# iterative two-dimensional decomposition
x$r <- mid$residuals
mid_it <- interpret(r ~ x1:x2 + x2:x3 + x3:x1, data = x, k = 0, ok = TRUE)
weighted.mean(mid_it$residuals ^ 2, w) /
  weighted.mean((y - weighted.mean(y, w))^2, w)
ggmid(mid_it, "x1:x2")
#> uninterpreted rate : 0.12%
# two-dimensional MID decomposition
mid2 <- interpret(y ~ (x1 + x2 + x3) ^ 2, data = x, k = 0, ok = TRUE, mode = 2)
summary(mid2)
ggmid(mid2, "x1:x2")
#> uninterpreted rate : 0%
# compare x1 main effect
ggplot() + xlim(-11.1, 11.1) +
  geom_function(fun = function(x) mid.f(mid, "x1", x)) +
  geom_function(fun = function(x) (5 / 6 * (x ^ 2 - 2)), lty = 2, col = 1) +
  geom_function(fun = function(x) mid.f(mid2, "x1", x)) +
  geom_function(fun = function(x) (4 / 5 * (x ^ 2 - 2)), lty = 2, col = 2)
