test_that("attract works correctly", {
  x <- c(-1.5, -0.5, 0, 0.5, 1.5)
  expect_equal(attract(x, margin = 0.5), c(-1.5, 0, 0, 0, 1.5))
  expect_equal(attract(x, margin = 1.0), c(-1.5, 0, 0, 0, 1.5))
  expect_identical(attract(x, margin = 0), x)
})

test_that("is.discrete identifies types correctly", {
  expect_true(is.discrete(factor(c("A", "B"))))
  expect_true(is.discrete(c("A", "B")))
  expect_true(is.discrete(c(TRUE, FALSE)))
  expect_false(is.discrete(c(1, 2, 3)))
  expect_false(is.discrete(c(1.5, 2.5)))
})

test_that("get.variables splits interaction terms properly", {
  expect_equal(get.variables("A:B"), c("A", "B"))
  expect_equal(get.variables("A:B:C"), c("A", "B", "C"))
  expect_equal(get.variables("A"), "A")
})

test_that("match.labels validates term labels correctly", {
  labs <- c("A", "B", "A:B", "C:D:E")
  expect_equal(match.labels("A", labs), "A")
  expect_equal(match.labels("B:A", labs, names = "a"), c(a = "A:B"))
  expect_equal(match.labels("A * B", labs, single = FALSE), "A:B")
  expect_equal(match.labels("E:C:D", labs), "C:D:E")
  expect_true(is.na(match.labels("B : A", labs, sort = FALSE)))
  expect_true(is.na(match.labels("C", labs)))
})

test_that("interaction.frame combinations are correct", {
  xfrm <- data.frame(x = 1:2)
  yfrm <- data.frame(y = c("a", "b", "c"))
  res <- interaction.frame(xfrm, yfrm)
  expect_equal(nrow(res), 6)
  expect_equal(names(res), c("x", "y"))
  expect_equal(res$x, c(1, 2, 1, 2, 1, 2))
  expect_equal(res$y, c("a", "a", "b", "b", "c", "c"))
})

test_that("override translates arguments correctly", {
  args <- list(col = "black", cex = 1, pch = 16, main = "Title")
  dots <- list(colour = "red", size = 2, shape = 1)
  res <- override(args, dots)
  expect_equal(res$col, "red")
  expect_equal(res$cex, 2)
  expect_equal(res$pch, 1)
  expect_equal(res$main, "Title")
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
