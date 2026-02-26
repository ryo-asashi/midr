make_mock_midrib <- function() {
  x <- list(
    intercept = c(m1 = 1.0, m2 = 2.0, m3 = 3.0),
    main.effects = list(
      varA = list(
        mid = matrix(1:6, nrow = 2, dimnames = list(NULL, c("m1", "m2", "m3")))
      )
    ),
    interactions = NULL,
    fitted.values = matrix(
      1:6, nrow = 2, dimnames = list(NULL, c("m1", "m2", "m3"))
    ),
    residuals = matrix(0.1, nrow = 2, ncol = 3),
    ratio = c(m1 = 0.5, m2 = 0.6, m3 = 0.7)
  )
  class(x) <- c("mids", "midrib")
  x
}

make_mock_importance <- function() {
  x <- list(
    m1 = list(
      importance = data.frame(term = c("A", "B"), importance = c(0.8, 0.2))
    ),
    m2 = list(
      importance = data.frame(term = c("A", "B"), importance = c(0.7, 0.3))
    )
  )
  attr(x$m1, "term.labels") <- c("A", "B")
  class(x) <- c("midimps", "midlist")
  x
}

make_mock_breakdown <- function() {
  x <- list(
    m1 = list(
      breakdown = data.frame(term = c("A", "B"), mid = c(0.8, -0.2))
    ),
    m2 = list(
      breakdown = data.frame(term = c("A", "B"), mid = c(0.7, -0.3))
    )
  )
  attr(x$m1, "term.labels") <- c("A", "B")
  class(x) <- c("midbrks", "midlist")
  x
}

make_mock_conditional <- function() {
  x <- list(
    m1 = list(
      conditional = data.frame(.id = 1:2, varA = c(10, 20), yhat = c(1.5, 2.5))
    ),
    m2 = list(
      conditional = data.frame(.id = 1:2, varA = c(10, 20), yhat = c(1.6, 2.6))
    )
  )
  x$m1$ids <- 1:2
  x$m1$variable <- "varA"
  x$m1$values <- c(10, 20)
  class(x) <- c("midcons", "midlist")
  x
}

test_that("[.midrib subsets correctly with positive indices", {
  ml <- make_mock_midrib()
  sub2 <- ml[c(1, 3)]
  expect_s3_class(sub2, "midrib")
  expect_equal(names(sub2$intercept), c("m1", "m3"))
  expect_equal(ncol(sub2$fitted.values), 2)
  expect_equal(length(sub2$ratio), 2)
})

test_that("[.midrib drops to 'mid' class when length(i) == 1", {
  ml <- make_mock_midrib()
  sub1 <- ml[2]
  expect_s3_class(sub1, "mid")
  expect_null(names(sub1$intercept))
  expect_null(names(sub1$ratio))
  expect_equal(sub1$intercept, 2.0, ignore_attr = TRUE)
  sub1_nodrop <- ml[2, drop = FALSE]
  expect_s3_class(sub1_nodrop, "midrib")
  expect_equal(names(sub1_nodrop$intercept), "m2")
})

test_that("[.midrib handles negative indices (Negative Indexing)", {
  ml <- make_mock_midrib()
  sub_neg <- ml[-1]
  expect_s3_class(sub_neg, "midrib")
  expect_equal(names(sub_neg$intercept), c("m2", "m3"))
  sub_neg2 <- ml[-c(1, 3)]
  expect_s3_class(sub_neg2, "mid")
  expect_equal(sub_neg2$intercept, 2.0, ignore_attr = TRUE)
  expect_error(ml[c(1, -2)], "only 0's may be mixed with negative subscripts")
})

test_that("[.midrib handles character and logical indices", {
  ml <- make_mock_midrib()
  expect_equal(names(ml[c("m1", "m3")]$intercept), c("m1", "m3"))
  sub_log <- ml[c(TRUE, FALSE, TRUE)]
  expect_equal(names(sub_log$intercept), c("m1", "m3"))
})

test_that("[.midrib edge cases and errors", {
  ml <- make_mock_midrib()
  expect_error(ml[c(1, 4)], "subscript out of bounds")
  expect_error(ml[NA], "undefined item selected")
  expect_equal(names(ml[c(0, 1, 2)]$intercept), c("m1", "m2"))
  expect_null(ml[0])
})

test_that("[[.midrib extracts a single element", {
  ml <- make_mock_midrib()
  expect_s3_class(ml[[1]], "mid")
  expect_s3_class(ml[["m2"]], "mid")
  expect_error(ml[[c(1, 2)]], "attempt to select more than one element")
})

test_that("as.list.midrib converts to a list of 'mid' objects", {
  ml <- make_mock_midrib()
  lst <- as.list(ml)
  expect_type(lst, "list")
  expect_equal(length(lst), 3)
  expect_equal(names(lst), c("m1", "m2", "m3"))
  expect_s3_class(lst[["m1"]], "mid")
})

test_that("summary.midlist.importance works for wide and long shapes", {
  ml_imp <- make_mock_importance()
  sum_wide <- summary(ml_imp, shape = "wide")
  expect_s3_class(sum_wide, "data.frame")
  expect_equal(colnames(sum_wide), c("term", "m1", "m2"))
  expect_equal(sum_wide$m1, c(0.8, 0.2))
  sum_long <- summary(ml_imp, shape = "long")
  expect_s3_class(sum_long, "data.frame")
  expect_equal(colnames(sum_long), c("term", "importance", "label"))
  expect_equal(nrow(sum_long), 4) # 2 terms * 2 models
  expect_equal(sum_long$label, c("m1", "m1", "m2", "m2"))
})

test_that("summary.midlist.breakdown works for wide and long shapes", {
  ml_bdn <- make_mock_breakdown()
  sum_wide <- summary(ml_bdn, shape = "wide")
  expect_s3_class(sum_wide, "data.frame")
  expect_equal(colnames(sum_wide), c("term", "m1", "m2"))
  expect_equal(sum_wide$m1, c(0.8, -0.2))
  sum_long <- summary(ml_bdn, shape = "long")
  expect_s3_class(sum_long, "data.frame")
  expect_equal(colnames(sum_long), c("term", "mid", "label"))
  expect_equal(nrow(sum_long), 4) # 2 terms * 2 models
  expect_equal(sum_long$label, c("m1", "m1", "m2", "m2"))
})

test_that("summary.midlist.conditional works for wide and long shapes", {
  ml_cond <- make_mock_conditional()
  sum_wide <- summary(ml_cond, shape = "wide")
  expect_s3_class(sum_wide, "data.frame")
  expect_equal(colnames(sum_wide), c(".id", "varA", "m1", "m2"))
  expect_equal(sum_wide$m2, c(1.6, 2.6))
  sum_long <- summary(ml_cond, shape = "long")
  expect_s3_class(sum_long, "data.frame")
  expect_equal(colnames(sum_long), c(".id", "varA", "yhat", "label"))
  expect_equal(nrow(sum_long), 4) # 2 ids * 2 models
})

test_that("[.midlist.importance preserves class", {
  ml_imp <- make_mock_importance()
  sub_imp <- ml_imp[1, drop = FALSE]
  expect_s3_class(sub_imp, "midimps")
})
