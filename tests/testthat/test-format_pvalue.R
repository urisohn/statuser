test_that("format_pvalue formats basic p-values correctly", {
  # Note: format_pvalue uses format() with nsmall=digits, so it always shows full decimal places
  expect_equal(format_pvalue(0.05), "= .0500")
  expect_equal(format_pvalue(0.001), "= .0010")
  expect_equal(format_pvalue(0.5), "= .5000")
  expect_equal(format_pvalue(0.1234), "= .1234")
})

test_that("format_pvalue handles edge cases", {
  expect_equal(format_pvalue(0.00001), "< .0001")
  expect_equal(format_pvalue(0.99999), "> .9999")
  expect_equal(format_pvalue(0), "< .0001")
  expect_equal(format_pvalue(1), "> .9999")
  expect_equal(format_pvalue(0.0001), "= .0001")
  expect_equal(format_pvalue(0.9999), "= .9999")
})

test_that("format_pvalue handles NA values", {
  result <- format_pvalue(c(0.05, NA, 0.01))
  expect_equal(result[1], "= .0500")
  expect_true(is.na(result[2]))
  expect_equal(result[3], "= .0100")
  
  # All NA
  result <- format_pvalue(c(NA, NA))
  expect_true(all(is.na(result)))
})

test_that("format_pvalue works with include_p parameter", {
  expect_equal(format_pvalue(0.05, include_p = TRUE), "p = .0500")
  expect_equal(format_pvalue(0.001, include_p = TRUE), "p = .0010")
  expect_equal(format_pvalue(0.00001, include_p = TRUE), "p < .0001")
  expect_equal(format_pvalue(0.99999, include_p = TRUE), "p > .9999")
})

test_that("format_pvalue handles vector input", {
  result <- format_pvalue(c(0.05, 0.001, 0.00001))
  expect_equal(length(result), 3)
  expect_equal(result[1], "= .0500")
  expect_equal(result[2], "= .0010")
  expect_equal(result[3], "< .0001")
})

test_that("format_pvalue respects digits parameter", {
  expect_equal(format_pvalue(0.12345, digits = 2), "= .12")
  expect_equal(format_pvalue(0.12345, digits = 3), "= .123")
  expect_equal(format_pvalue(0.12345, digits = 4), "= .1235")
  expect_equal(format_pvalue(0.1, digits = 1), "= .1")
})

test_that("format_pvalue handles boundary values correctly", {
  # Test values just above and below thresholds
  expect_equal(format_pvalue(0.00009, digits = 4), "< .0001")
  expect_equal(format_pvalue(0.0001, digits = 4), "= .0001")
  expect_equal(format_pvalue(0.9999, digits = 4), "= .9999")
  expect_equal(format_pvalue(0.99991, digits = 4), "> .9999")
})

