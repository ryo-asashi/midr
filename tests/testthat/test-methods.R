# tests/testthat/test-methods.R

test_that("subset.mid() and coef.mid() extract submodels and component tables", {
  mid <- interpret(Ozone ~ (Wind + Temp)^2, data = airquality, lambda = 0.5, verbosity = 0L)
  sub <- subset(mid, ~ Wind + Temp)
  expect_s3_class(sub, "mid")
  expect_setequal(labels(sub), c("Wind", "Temp"))
  expect_equal(
    predict(sub, head(airquality)),
    rowSums(predict(mid, head(airquality), type = "terms")[, c("Wind", "Temp")]) + mid$intercept,
    ignore_attr = TRUE
  )

  sub_noint <- subset(mid, terms = c("Wind", "Wind:Temp"), intercept = FALSE)
  expect_equal(sub_noint$intercept, 0)
  expect_setequal(labels(sub_noint), c("Wind", "Wind:Temp"))

  cf <- coef(mid, terms = c("Wind", "Wind:Temp"), intercept = FALSE)
  expect_type(cf, "list")
  expect_named(cf, c("Wind", "Wind:Temp"))
  expect_false(".density" %in% names(cf$Wind))

  cf_int <- coef(mid, terms = ~ Wind, intercept = TRUE)
  expect_named(cf_int, c("(Intercept)", "Wind"))
})

test_that("predict.mid() and predict.mids() handle terms and link scales", {
  mids <- interpret(
    cbind(Sepal.Length, Sepal.Width) ~ Petal.Length + Petal.Width,
    data = iris, lambda = 0.5, verbosity = 0L
  )
  p_mat <- predict(mids, head(iris, 4L))
  expect_true(is.matrix(p_mat))
  expect_equal(dim(p_mat), c(4L, 2L))
  expect_equal(colnames(p_mat), c("Sepal.Length", "Sepal.Width"))

  p_terms <- predict(mids[[1L]], head(iris, 4L), type = "terms")
  expect_equal(colnames(p_terms), c("Petal.Length", "Petal.Width"))
})

test_that("make_encoder() and encoder getters work across types", {
  enc_num <- make_encoder(1:10, k = 4L, type = "linear", tag = "x")
  expect_s3_class(enc_num, "encoder")
  expect_equal(ncol(enc_num$frame), 1L)
  expect_equal(encoder_range(enc_num), c(1, 10))
  expect_length(encoder_knots(enc_num), 4L)

  enc_fac <- make_encoder(factor(c("a", "b", "c", "c")), k = 2L, tag = "f", lump = "order")
  expect_equal(encoder_levels(enc_fac, original = TRUE), c("a", "b", "c"))
  expect_length(encoder_levels(enc_fac, original = FALSE), 2L)
})

test_that("make_link() and midr_options() work as expected", {
  lnk <- make_link("log")
  expect_s3_class(lnk, "link-glm")
  expect_equal(lnk$linkinv(lnk$linkfun(2.5)), 2.5)

  old <- midr_options(verbosity = 0L, sequential = "Viridis")
  on.exit(midr_options(old), add = TRUE)
  expect_equal(midr_options("verbosity")$verbosity, 0L)
  expect_equal(midr_options("sequential")$sequential, "Viridis")
})
