# tests/testthat/test-utility.R

test_that("attract works correctly", {
  x <- c(-1.5, -0.5, 0, 0.5, 1.5)
  expect_equal(attract(x, margin = 0.5), c(-1.5, 0, 0, 0, 1.5))
  expect_equal(attract(x, margin = 1.0), c(-1.5, 0, 0, 0, 1.5))
  expect_identical(attract(x, margin = 0), x)
})

test_that("is_discrete identifies types correctly", {
  expect_true(is_discrete(factor(c("A", "B"))))
  expect_true(is_discrete(c("A", "B")))
  expect_true(is_discrete(c(TRUE, FALSE)))
  expect_false(is_discrete(c(1, 2, 3)))
  expect_false(is_discrete(c(1.5, 2.5)))
})

test_that("interaction_frame combinations are correct", {
  xfrm <- data.frame(x = 1:2)
  yfrm <- data.frame(y = c("a", "b", "c"))
  res <- interaction_frame(xfrm, yfrm)
  expect_equal(nrow(res), 6)
  expect_equal(names(res), c("x", "y"))
  expect_equal(res$x, c(1, 2, 1, 2, 1, 2))
  expect_equal(res$y, c("a", "a", "b", "b", "c", "c"))
})

test_that("verbose handles messages and verbosity levels", {
  expect_message(
    verbose("Test message", verbosity = 1, level = 1), "Test message"
  )
  expect_message(
    verbose("Debug", verbosity = 3, level = 3), "\\[debug\\] Debug"
  )
  expect_no_message(
    verbose("Test message", verbosity = 0, level = 1)
  )
})

test_that("examples formats vectors into strings", {
  x <- c(10, 20, 30, 40, 50)
  expect_equal(examples(x), "10, 20, 30, ...")
  expect_equal(examples(c(1, 2), n = 3), "1, 2")
  expect_equal(examples(
    c(1.111, 2.222, 3.333, 4.444), digits = 2
  ), "1.1, 2.2, 3.3, ...")
})

test_that("weighted quantile() works", {
  # test 1
  probs <- seq(0, 1, by = 0.005)
  wq <- weighted_quantile(1:4, 1:4, probs)
  q <- quantile(c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4), probs, type = 1L)
  expect_equal(wq, q)
  # test 2 including NAs
  wq <- weighted_quantile(c(1, 2, 3, NA, 4), c(1, 2, 2, 0, 2),
                          probs, na.rm = TRUE)
  q <- quantile(c(1, 2, 2, 3, 3, NA, 4, 4, NA, NA), probs, type = 1L,
                na.rm = TRUE)
  expect_equal(wq, q)
})
