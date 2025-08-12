test_that("interpret() returns weighted least-norm solutions", {
  # test 1 : rank deficient case 1
  x1 = 1L:2L
  x2 = 1L:2L
  mid <- interpret(formula = x1^2 ~ x1 + x2,
                   singular.ok = TRUE)
  expect_equal(mid$ratio, 0L)
  expect_equal(mid$main.effects$x1$mid, c(-.75, .75),
               ignore_attr = TRUE)
  # test 2 : rank deficient case 2
  x1 <- c(1, 1, 2, 2, -1, -1, -2, -2)
  x2 <- c(1, -1, 2, -2, 1, -1, 2, -2)
  mid <- interpret(formula = x1^2 ~ x1 + x2,
                   singular.ok = TRUE)
  expect_equal(mid$ratio, 0L)
  expect_equal(mid$main.effects$x1$mid, c(.75, -.75, -.75, .75),
               ignore_attr = TRUE)
  # test 3 : rank deficient case 3
  x1 <- c(1, 1, 2, 2)
  x2 <- c(1, 2, 1, 1)
  x3 <- c(1, 2, 2, 2)
  mid <- interpret(formula = x1 * x2 * x3 ~ x1 + x2 + x3,
                   singular.ok = TRUE)
  expect_equal(mid$ratio, 0L)
  expect_equal(mid$main.effects$x1$mid, c(-.45, .45),
               ignore_attr = TRUE)
})


test_that("interpret() returns valid 'na.action'", {
  x <- c(1, 2, 3, 4, NA)
  y <- c(1, 2, NA, 4, 5)
  X <- data.frame(x = x, y = c(2, 2, 2, NA, 2))
  # test 1 : formula, vector x
  mid <- interpret(formula = y ~ x)
  expect_equal(mid$na.action, c(3, 5), ignore_attr = TRUE)
  expect_s3_class(mid$na.action, "omit")
  # test 2 : formula, use model
  mid <- interpret(formula = y ~ x, model = NULL, data = X,
                   pred.fun = function(X.model, newdata) {newdata$x})
  expect_equal(mid$na.action, c(4, 5), ignore_attr = TRUE)
  # test 3 : formula, na.exclude
  mid <- interpret(formula = y ~ x, na.action = "na.exclude")
  expect_s3_class(mid$na.action, "exclude")
  expect_equal(predict(mid), c(1, 2, NA, 4, NA))
  # test 4 : default, vector x
  mid <- interpret(object = NULL, x = x, y = y)
  expect_equal(mid$na.action, c(3, 5), ignore_attr = TRUE)
  # test 5 : default, na.exclude (character)
  mid <- interpret(object = NULL, x = X, y = y, na.action = "na.exclude")
  expect_equal(mid$na.action, c(3, 4, 5), ignore_attr = TRUE)
  # test 6 : default, stats::na.exclude (function)
  mid <- interpret(object = NULL, x = x, y = y, na.action = stats::na.exclude)
  expect_equal(mid$na.action, c(3, 5), ignore_attr = TRUE)
})


test_that("interpret() runs successfully for factor responses", {
  # test 1
  x <- 1:5
  y <- factor(c("a", "a", "a", "b", "b"), levels = c("b", "a"))
  mid <- interpret(y ~ x)
  expect_equal(mid$intercept, 0.6)
  # test 2
  f <- function(X.model, newdata) ifelse(newdata$x < 4, "alice", "bob")
  mid <- interpret(object = NULL, x = x, pred.fun = f)
  expect_equal(mid$intercept, 0.4)
})


test_that("interpret() returns valid result with weighted data", {
  # test 1
  x <- c(1, 1, 2, 2, 3)
  mid1 <- interpret(2 * x ~ x)
  mid2 <- interpret(x = 1:3, y = (1:3) * 2, weights = c(2, 2, 1))
  expect_equal(mid1$main.effects, mid2$main.effects)
  expect_equal(mid1$intercept, mid2$intercept)
})

test_that("weights works", {
  # test 1
  x1 <- c(1, 1, 1, 1, 2, 2, 2)
  x2 <- c(1, 1, 1, 2, 1, 2, 2)
  x3 <- c(1, 1, 2, 1, 2, 1, 2)
  X1 <- data.frame(cbind(x1, x2, x3))
  x1 <- c(1, 1, 1, 2, 2, 2)
  x2 <- c(1, 1, 2, 1, 2, 2)
  x3 <- c(1, 2, 1, 2, 1, 2)
  weights <- c(2, 1, 1, 1, 1, 1)
  X2 <- data.frame(cbind(x1, x2, x3))
  X3 <- weighted(X2, weights)
  r1 <- interpret(x1 * x2 * x3 ~ (x1 + x2 + x3), X1)$ratio
  r2 <- interpret(x1 * x2 * x3 ~ (x1 + x2 + x3), X2)$ratio
  r3 <- interpret(x1 * x2 * x3 ~ (x1 + x2 + x3), X2, weights = weights)$ratio
  r4 <- interpret(x1 * x2 * x3 ~ (x1 + x2 + x3), X3)$ratio
  r5 <- interpret(x1 * x2 * x3 ~ (x1 + x2 + x3), X3, weights = rep(1, 6))$ratio
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(r2, r5)
  expect_false(r1 == r2)
})
