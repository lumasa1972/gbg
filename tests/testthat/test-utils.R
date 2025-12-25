test_that("movavg_k works correctly", {
  expect_equal(length(movavg_k(1:5, k = 3)), 5)
  expect_true(is.na(movavg_k(1:5, k = 3)[1]))
  expect_true(is.na(movavg_k(1:5, k = 3)[2]))
  expect_equal(movavg_k(c(1, 2, 3), k = 3)[3], 2)
})

test_that("fmt_miles formats correctly", {
  expect_equal(fmt_miles(1000), "1,000")
  expect_equal(fmt_miles(1234567), "1,234,567")
  expect_equal(fmt_miles(0), "0")
})

test_that("norm_key normalizes correctly", {
  expect_equal(norm_key("san josé"), "SAN JOSE")
  expect_equal(norm_key("  multiple   spaces  "), "MULTIPLE SPACES")
  expect_equal(norm_key("áéíóú"), "AEIOU")
})
