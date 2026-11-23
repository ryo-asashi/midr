# tests/testthat/test-eval.R

test_that("eval_importance() works for mid and mids with term and variable aggregation", {
  aq <- stats::na.omit(airquality)
  mid <- interpret(Ozone ~ (Wind + Temp)^2, data = aq, lambda = 0.5, verbosity = 0L)

  imp_t <- eval_importance(mid, data = aq)
  expect_s3_class(imp_t, "midimp")
  expect_true(all(c("term", "importance", "order") %in% names(summary(imp_t))))
  expect_equal(nrow(as.data.frame(imp_t, long = TRUE)), nrow(aq) * nrow(summary(imp_t)))

  imp_v <- eval_importance(mid, data = aq, by = "variable", measure = "rmse")
  expect_equal(sort(as.character(summary(imp_v)$term)), c("Temp", "Wind"))
  expect_false("order" %in% names(summary(imp_v)))

  mids <- interpret(cbind(Sepal.Length, Sepal.Width) ~ Petal.Length + Species, data = iris, verbosity = 0L)
  imps <- eval_importance(mids, by = "variable")
  expect_s3_class(imps, "midimps")
  expect_equal(colnames(summary(imps)), c("term", "Sepal.Length", "Sepal.Width"))
})

test_that("eval_breakdown() works for single and multiple rows", {
  mid <- interpret(mpg ~ (wt + hp)^2, data = mtcars, lambda = 0.5, verbosity = 0L)

  brk1 <- eval_breakdown(mid, data = mtcars, rows = "Mazda RX4")
  expect_s3_class(brk1, "midbrk")
  expect_true(all(c("term", "effect", "order") %in% names(brk1$breakdown)))
  expect_equal(brk1$intercept + sum(brk1$breakdown$effect), brk1$prediction, tolerance = 1e-10)

  brks <- eval_breakdown(mid, data = mtcars, rows = c("Mazda RX4", "Toyota Corolla"), by = "variable")
  expect_s3_class(brks, "midbrks")
  expect_equal(colnames(summary(brks)), c("term", "Mazda RX4", "Toyota Corolla"))
  expect_equal(colnames(as.data.frame(brks)), c("term", "label", "effect"))
})

test_that("eval_conditional() evaluates ICE and preserves non-syntactic column names", {
  aq <- stats::na.omit(airquality)
  mid <- interpret(Ozone ~ log(Wind) + Temp, data = aq, lambda = 0.5, verbosity = 0L)

  con <- eval_conditional(mid, variable = "log(Wind)", data = aq, max.nsamples = 10L)
  expect_s3_class(con, "midcon")
  expect_true("log(Wind)" %in% names(con$observed))
  expect_true("log(Wind)" %in% names(con$conditional))
})

test_that("plot(), persp(), and ggmid() run without error across classes", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  mid <- interpret(Volume ~ Girth * Height, data = trees, lambda = 0.2, verbosity = 0L)
  imp <- eval_importance(mid)
  brk <- eval_breakdown(mid, data = trees, rows = 1L)
  con <- eval_conditional(mid, variable = "Girth", data = trees, max.nsamples = 10L)

  expect_no_error(plot(mid, terms = c("Girth", "Girth:Height")))
  expect_no_error(persp(mid, terms = "Girth:Height"))
  expect_no_error(plot(imp))
  expect_no_error(plot(imp, type = "heatmap"))
  expect_no_error(plot(brk))
  expect_no_error(plot(con, var.color = Height))

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_s3_class(ggmid(mid, terms = "Girth"), "ggplot")
    expect_s3_class(ggmid(imp, type = "heatmap"), "ggplot")
    expect_s3_class(ggmid(brk), "ggplot")
    expect_s3_class(ggmid(con, var.color = Height), "ggplot")
  }
})
