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
  mid <- interpret(Ozone ~ Temp, subset = 1:50)
  cp <- stats::model.frame(Ozone ~ Temp, subset = 1:50)
  expect_equal(length(mid$fitted.values), nrow(cp))
  # test 3 : drop unused levels works
  X <- data.frame(y = 1:10, x = factor(rep(1:5, 2), levels = 1:6))
  mid <- interpret(y ~ x, X)
  expect_equal(length(mid$main.effects$x$x_level), 6)
  mid <- interpret(y ~ x, X, drop.unused.levels = TRUE)
  expect_equal(length(mid$main.effects$x$x_level), 5)
  # test 4 : na.action works - na.pass
  expect_error(interpret(Ozone ~ ., airquality, na.action = "na.pass"))
  # test 5 : na.action works - na.exclude
  mid <- interpret(Ozone ~ ., airquality, na.action = "na.exclude")
  cp <- stats::na.action(stats::na.exclude(airquality))
  expect_true(all(cp == mid$na.action))
})
