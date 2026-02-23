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

test_that("term.split splits interaction terms properly", {
  expect_equal(term.split("A:B"), c("A", "B"))
  expect_equal(term.split("A:B:C"), c("A", "B", "C"))
  expect_equal(term.split("A"), "A")
})

test_that("term.check validates and reverses terms correctly", {
  terms <- c("A", "B", "A:B", "C:D:E")
  expect_equal(term.check("A", terms), "A")
  expect_equal(term.check("A:B", terms), "A:B")
  expect_equal(term.check("B:A", terms), "A:B")
  expect_error(term.check("C", terms, stop = TRUE), "does not exist")
  expect_error(term.check("X:Y", terms, stop = TRUE), "does not exist")
  expect_message(
    res <- term.check("C", terms, stop = FALSE),
    "does not exist"
  )
  expect_true(is.na(res))
  expect_error(term.check(NA, terms, stop = TRUE), "term can't be NA")
  expect_true(is.na(term.check(NA, terms, stop = FALSE)))
})

test_that("make.formula creates formula correctly", {
  f1 <- make.formula(xlabels = c("x1", "x2"), ylabel = "y")
  expect_s3_class(f1, "formula")
  expect_equal(deparse(f1), "y ~ x1 + x2")
  f2 <- make.formula(xlabels = c("x1", "x2"))
  expect_equal(deparse(f2), "~x1 + x2")
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
