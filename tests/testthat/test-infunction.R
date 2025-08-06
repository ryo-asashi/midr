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
