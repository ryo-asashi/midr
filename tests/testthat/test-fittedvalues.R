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
