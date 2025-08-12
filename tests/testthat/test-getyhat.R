test_that("get.yhat() returns numeric vector of length n", {
  # test 1 : lm - regression
  model <- lm(Ozone ~ ., airquality)
  preds <- get.yhat(model, airquality)
  expect_vector(preds, ptype = double(), size = nrow(airquality))
  # test 2 : glm - regression
  model <- glm(Ozone ~ ., Gamma("log"), airquality)
  preds <- get.yhat(model, airquality)
  expect_vector(preds, ptype = double(), size = nrow(airquality))
})
