test_that("interpret() returns weighted least-norm solutions", {
  # test 1 : rank deficient case 1
  x1 = 1L:2L
  x2 = 1L:2L
  mid <- interpret(formula = x1^2 ~ x1 + x2,
                   singular.ok = TRUE)
  expect_equal(mid$uninterpreted.rate, 0L)
  expect_equal(mid$main.effects$x1$mid, c(-.75, .75),
               ignore_attr = TRUE)
  # test 1 : rank deficient case 2
  x1 <- c(1, 1, 2, 2, -1, -1, -2, -2)
  x2 <- c(1, -1, 2, -2, 1, -1, 2, -2)
  mid <- interpret(formula = x1^2 ~ x1 + x2,
                   singular.ok = TRUE)
  expect_equal(mid$uninterpreted.rate, 0L)
  expect_equal(mid$main.effects$x1$mid, c(.75, -.75, -.75, .75),
               ignore_attr = TRUE)
  # test 3 : rank deficient case 3
  x1 <- c(1, 1, 2, 2)
  x2 <- c(1, 2, 1, 1)
  x3 <- c(1, 2, 2, 2)
  mid <- interpret(formula = x1 * x2 * x3 ~ x1 + x2 + x3,
                   singular.ok = TRUE)
  expect_equal(mid$uninterpreted.rate, 0L)
  expect_equal(mid$main.effects$x1$mid, c(-.45, .45),
               ignore_attr = TRUE)
})


test_that("interpret() returns valid 'na.action'", {
  x <- c(1, 2, 3, 4, NA)
  y <- c(1, 2, NA, 4, 5)
  X <- data.frame(x = x, z = c(2, 2, 2, NA, 2))
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
  x <- 1:5
  y <- factor(c("a", "a", "a", "b", "b"), levels = c("b", "a"))
  mid <- interpret(y ~ x)
  expect_equal(mid$intercept, 0.4)
  expect_equal(mid$target.level, "b")
  f <- function(X.model, newdata) ifelse(newdata$x < 4, "alice", "bob")
  mid <- interpret(object = NULL, x = x, pred.fun = f)
  expect_equal(mid$intercept, 0.6)
  expect_equal(mid$target.level, "alice")
})


test_that("interpret() returns valid result with weighted data", {
  x <- c(1, 1, 2, 2, 3)
  mid <- interpret(2 * x ~ x)
  mid2 <- interpret(x = 1:3, y = (1:3) * 2, weights = c(2, 2, 1))
  expect_equal(mid$main.effects, mid2$main.effects)
  expect_equal(mid$intercept, mid2$intercept)
})
