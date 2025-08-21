test_that("color.theme", {
  # test 1
  ct <- color.theme("midr")
  expect_equal(length(ct$palette(257L)), 257L)
  expect_equal(length(ct$ramp(seq.int(0, 1, length.out = 257L))), 257L)
  # test 2
  ct2 <- color.theme(ct)
  expect_identical(ct, ct2)
  # test 3
  ct2 <- color.theme(ct, reverse = TRUE)
  expect_equal(ct2$palette(2L), rev(ct$palette(2L)))
  # test 4a
  ct3 <- color.theme("midr/midr_r")
  expect_equal(ct2$palette(12L), ct3$palette(12L))
  # test 4b
  ct3 <- color.theme("midr_r", source = "midr")
  expect_equal(ct2$palette(12L), ct3$palette(12L))
  # test 5
  ct3 <- color.theme("midr?reverse=TRUE")
  expect_equal(ct2$palette(12L), ct3$palette(12L))
  # test 6a
  ct2 <- color.theme("midr@seq")
  expect_equal(ct2$type, "sequential")
  # test 6b
  ct3 <- color.theme("midr@qual")
  expect_equal(ct3$type, "qualitative")
  # test 7a
  ct <- color.theme("grDevices/Paired")
  expect_equal(ct$options$kernel.size, 8)
  # test 7b
  ct <- color.theme("Paired", source = "grDevices")
  expect_equal(ct$options$kernel.size, 8)
  # test 7c
  ct <- color.theme("RColorBrewer/Paired")
  expect_equal(ct$options$kernel.size, 12)
  # test 7d
  ct <- color.theme("Paired", source = "RColorBrewer")
  expect_equal(ct$options$kernel.size, 12)
  # test 8
  expect_no_error(ct <- color.theme(grDevices::rainbow))
  expect_equal(ct$palette(1), ct$ramp(0))
  # test 9
  ct <- color.theme("Viridis?alpha=0.5")
  ct2 <- color.theme("Viridis", kernel.args = list(alpha = 0.5))
  ct3 <- color.theme("Viridis", alpha = 0.5)
  expect_true(all(ct$palette(3) == ct2$palette(3)))
  expect_true(all(ct$palette(3) == ct3$palette(3)))
  # test 10
  ct <- color.theme("Mako_r")
  expect_true(ct$kernel.args$rev)
  # test 11
  set.color.theme(c("white", "black"), type = "qualitative",
                  name = "mytheme", source = "custom")
  ct <- color.theme("mytheme")
  expect_equal(ct$palette(1), "white")
})
