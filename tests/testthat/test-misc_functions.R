test_that("weighted quantile() works", {
  # test 1
  probs <- seq(0, 1, by = 0.005)
  wq <- weighted.quantile(1:4, 1:4, probs)
  q <- quantile(c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4), probs, type = 1L)
  expect_equal(wq, q)
  # test 2 including NAs
  wq <- weighted.quantile(c(1, 2, 3, NA, 4), c(1, 2, 2, 0, 2),
                          probs, na.rm = TRUE)
  q <- quantile(c(1, 2, 2, 3, 3, NA, 4, 4, NA, NA), probs, type = 1L,
                na.rm = TRUE)
  expect_equal(wq, q)
})
