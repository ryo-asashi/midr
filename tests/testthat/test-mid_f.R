test_that("mid.f() correctly runs for interactions", {
  # test 1 : x and y are exchangeable
  mid <- interpret(Volume ~ .^2, trees, ok = TRUE)
  h <- c(70, 65, 63, 72, 81)
  g <- c(8.3, 8.6, 8.8, 10.5, 10.8)
  fx <- mid.f(mid, "Height:Girth", x = data.frame(Height = h, Girth = g))
  fxy <- mid.f(mid, "Height:Girth", x = h, y = g)
  expect_equal(fxy, fx)
  fyx <- mid.f(mid, "Girth:Height", x = g, y = h)
  expect_equal(fyx, fx)
})
