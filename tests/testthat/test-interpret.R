test_that("interpret() correctly runs (rank-deficient 1)", {
  x1 = 1L:2L
  x2 = 1L:2L
  mid <- interpret(formula = x1^2 ~ x1 + x2,
                   singular.ok = TRUE)
  expect_equal(mid$uninterpreted.rate, 0L)
  expect_equal(mid$main.effects$x1$mid, c(-.75, .75),
               ignore_attr = TRUE)
})

test_that("interpret() correctly runs (rank-deficient 2)", {
  x1 <- c(1, 1, 2, 2, -1, -1, -2, -2)
  x2 <- c(1, -1, 2, -2, 1, -1, 2, -2)
  mid <- interpret(formula = x1^2 ~ x1 + x2,
                   singular.ok = TRUE)
  expect_equal(mid$uninterpreted.rate, 0L)
  expect_equal(mid$main.effects$x1$mid, c(.75, -.75, -.75, .75),
               ignore_attr = TRUE)
})

test_that("interpret() correctly runs (rank-deficient 3)", {
  x1 <- c(1, 1, 2, 2)
  x2 <- c(1, 2, 1, 1)
  x3 <- c(1, 2, 2, 2)
  mid <- interpret(formula = x1 * x2 * x3 ~ x1 + x2 + x3,
                   singular.ok = TRUE)
  expect_equal(mid$uninterpreted.rate, 0L)
  expect_equal(mid$main.effects$x1$mid, c(-.45, .45),
               ignore_attr = TRUE)
})
