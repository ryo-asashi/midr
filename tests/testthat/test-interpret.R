# tests/testthat/test-interpret.R

test_that("interpret() returns weighted least-norm solutions", {
  # test 1 : rank deficient case 1
  x1 = 1L:2L
  x2 = 1L:2L
  mid <- interpret(formula = x1^2 ~ x1 + x2, singular.ok = TRUE)
  expect_equal(mid$ratio, 0L, ignore_attr = TRUE)
  expect_equal(mid$main.effects$x1$.effect, c(-.75, .75), ignore_attr = TRUE)
  # test 2 : rank deficient case 2
  x1 <- c(1, 1, 2, 2, -1, -1, -2, -2)
  x2 <- c(1, -1, 2, -2, 1, -1, 2, -2)
  mid <- interpret(formula = x1^2 ~ x1 + x2, singular.ok = TRUE)
  expect_equal(mid$ratio, 0L, ignore_attr = TRUE)
  expect_equal(mid$main.effects$x1$.effect, c(.75, -.75, -.75, .75),
               ignore_attr = TRUE)
  # test 3 : rank deficient case 3
  x1 <- c(1, 1, 2, 2)
  x2 <- c(1, 2, 1, 1)
  x3 <- c(1, 2, 2, 2)
  mid <- interpret(formula = x1 * x2 * x3 ~ x1 + x2 + x3, singular.ok = TRUE)
  expect_equal(mid$ratio, 0L, ignore_attr = TRUE)
  expect_equal(mid$main.effects$x1$.effect, c(-.45, .45), ignore_attr = TRUE)
})


test_that("interpret() returns valid 'na.action'", {
  x <- c(1, 2, 3, 4, NA)
  y <- c(1, 2, NA, 4, 5)
  X <- data.frame(x = x, y = c(2, 2, 2, NA, 2))
  # test 1 : formula, vector x
  mid <- interpret(formula = y ~ x)
  expect_equal(stats::na.action(mid), c(3, 5), ignore_attr = TRUE)
  expect_s3_class(stats::na.action(mid), "omit")
  # test 2 : formula, use model
  mid <- interpret(formula = y ~ x, model = NULL, data = X,
                   pred.fun = function(X.model, newdata) {newdata$x})
  expect_equal(stats::na.action(mid), c(4, 5), ignore_attr = TRUE)
  # test 3 : formula, na.exclude
  mid <- interpret(formula = y ~ x, na.action = "na.exclude")
  expect_s3_class(stats::na.action(mid), "exclude")
  expect_equal(predict(mid, na.action = "na.exclude"),
               c(1, 2, NA, 4, NA), ignore_attr = TRUE)
  # test 4 : default, vector x
  mid <- interpret(object = NULL, x = x, y = y)
  expect_equal(stats::na.action(mid), c(3, 5), ignore_attr = TRUE)
  # test 5 : default, na.exclude (character)
  mid <- interpret(object = NULL, x = X, y = y, na.action = "na.exclude")
  expect_equal(stats::na.action(mid), c(3, 4, 5), ignore_attr = TRUE)
  # test 6 : default, stats::na.exclude (function)
  mid <- interpret(object = NULL, x = x, y = y, na.action = stats::na.exclude)
  expect_equal(stats::na.action(mid), c(3, 5), ignore_attr = TRUE)
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

test_that("interpret() runs successfully with weights", {
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
  r1 <- interpret(x1 * x2 * x3 ~ (x1 + x2 + x3), X1)$ratio
  r2 <- interpret(x1 * x2 * x3 ~ (x1 + x2 + x3), X2)$ratio
  r3 <- interpret(x1 * x2 * x3 ~ (x1 + x2 + x3), X2, weights = weights)$ratio
  expect_equal(r1, r3)
  expect_false(r1 == r2)
  # test 2
  X2[, "wcol"] <- weights
  r4 <- interpret(x1 * x2 * x3 ~ (x1 + x2 + x3), X2, weights = wcol)$ratio
  expect_equal(r1, r4)
})


test_that("interpret() runs successfully with terms", {
  x1 <- 1L:5L
  x2 <- x1 ^ 2
  y <- x1 + x2
  d <- data.frame(cbind(x1, x2, y))
  # test 0: default method without terms input
  fit <- interpret(x = cbind(x1, x2), y = y)
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
  fit <- interpret(x = cbind(x1, x2), y = y, interactions = TRUE)
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
  # test 1: default method with label-based inputs
  fit <- interpret(x = d, y = y, terms = c("x2", "x1", "x1 : x2"))
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
  fit <- interpret(x = d, y = y, terms = c("x2 * x1"))
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
  fit <- interpret(x = d, y = y, terms = c("x2 : x1", "x1", "x2"))
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
  fit <- interpret(x = cbind(x1, x2), y = y, terms = c(".^2"))
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
  # test 2: default method with formula-based inputs
  fit <- interpret(x = d, y = y, terms = y ~ .^2)
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
  # test 3: formula method
  fit <- interpret(y ~ .^2, data = d, verbosity = 0L)
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
  fit <- interpret(y ~ x2 * x1, data = d, verbosity = 0L)
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
  fit <- interpret(y ~ x2:x1 + x1 + x2, data = d, verbosity = 0L)
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
  fit <- interpret(y ~ x1 + x2 + I(x1 * x2), data = d, verbosity = 0L)
  expect_setequal(
    labels(fit),
    c(names(fit$main.effects), names(fit$interactions))
  )
})

test_that("interpret() works in another function", {
  # test 1: formula
  fun <- function(formula) {
    interpret(formula, data = data.frame(x = 1:3, y = 1:3))
  }
  expect_no_error(mid <- fun(y ~ x))
  expect_no_error(mid <- fun(x ~ y))
  expect_error(mid <- fun(~ x))
  # test 2: formula and data
  fun <- function(formula, data) {
    interpret(formula, data = data)
  }
  train <- data.frame(x = 1:10, y = 11:20)
  mid <- fun(y ~ x, train)
  expect_equal(mid$intercept, mean(train$y))
  mid <- fun(x ~ y, train)
  expect_equal(mid$intercept, mean(train$x))
  cv_train <- train[1:5, ]
  mid <- fun(y ~ x, cv_train)
  expect_equal(mid$intercept, mean(cv_train$y))
})

test_that("interpret() returns consistent results", {
  # test 1: without link
  train <- data.frame(x = 1:10, y = seq(1, 2, length.out = 10))
  mid <- interpret(y ~ x, data = train, nil = 0.1)
  expect_true(all(abs(mid$fitted.values + mid$residuals - train$y) < 1e-10))
  # test 2: with link
  mid <- interpret(y ~ x, data = train, nil = 0.1, link = "log")
  expect_true(
    all(abs(mid$fitted.values + mid$response.residuals - train$y) < 1e-10)
  )
  expect_true(
    all(abs(mid$linear.predictors + mid$residuals - log(train$y) < 1e-10))
  )
})

test_that("subset works", {
  # test 1 : subset works
  model <- lm(Ozone ~ Temp + Wind, airquality, subset = Temp > 80)
  mid <- interpret(Ozone ~ Temp + Wind, airquality, subset = Temp > 80)
  expect_equal(length(model$fitted.values), length(mid$fitted.values))
  mid <- interpret(Ozone ~ Temp + Wind, airquality, model, subset = Temp > 80)
  expect_equal(length(model$fitted.values), length(mid$fitted.values))
  # test 2 : subset works 2
  model <- lm(Ozone ~ Temp + Wind, airquality, subset = 1:50)
  mid <- interpret(Ozone ~ Temp + Wind, airquality, subset = 1:50)
  expect_equal(length(model$fitted.values), length(mid$fitted.values))
  mid <- interpret(Ozone ~ Temp + Wind, airquality, model, subset = 1:50)
  expect_equal(length(model$fitted.values), length(mid$fitted.values))
  # test 2b : subset works 2b
  attach(airquality)
  on.exit(detach(airquality), add = TRUE)
  mid <- interpret(Ozone ~ Temp, subset = 1:50)
  cp <- stats::model.frame(Ozone ~ Temp, subset = 1:50)
  expect_equal(length(mid$fitted.values), nrow(cp))
  # test 3 : drop unused levels works
  X <- data.frame(y = 1:10, x = factor(rep(1:5, 2), levels = 1:6))
  mid <- interpret(y ~ x, X)
  expect_equal(length(mid$main.effects$x$x), 6)
  mid <- interpret(y ~ x, X, drop.unused.levels = TRUE)
  expect_equal(length(mid$main.effects$x$x), 5)
  # test 4 : na.action works - na.pass
  expect_error(interpret(Ozone ~ ., airquality, na.action = "na.pass"))
  # test 5 : na.action works - na.exclude
  mid <- interpret(Ozone ~ ., airquality, na.action = "na.exclude")
  cp <- stats::na.action(stats::na.exclude(airquality))
  expect_true(all(cp == mid$na.action))
})
