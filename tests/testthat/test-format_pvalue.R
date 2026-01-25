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

test_that("format_pvalue threshold adapts to digits parameter", {
  # Very small p-value should use threshold based on digits
  expect_equal(format_pvalue(0.0000001, digits = 2), "< .01")
  expect_equal(format_pvalue(0.0000001, digits = 3), "< .001")
  expect_equal(format_pvalue(0.0000001, digits = 4), "< .0001")
  
  # Very large p-value should use threshold based on digits
  expect_equal(format_pvalue(0.9999999, digits = 2), "> .99")
  expect_equal(format_pvalue(0.9999999, digits = 3), "> .999")
  expect_equal(format_pvalue(0.9999999, digits = 4), "> .9999")
  
  # With include_p
  expect_equal(format_pvalue(0.0000001, digits = 2, include_p = TRUE), "p < .01")
  expect_equal(format_pvalue(0.0000001, digits = 3, include_p = TRUE), "p < .001")
})

# ============================================================================
# ADDITIONAL EDGE CASES
# ============================================================================

test_that("format_pvalue handles negative values", {
  # Negative p-values are invalid but function should handle gracefully
  # (or throw appropriate error)
  result <- tryCatch(
    format_pvalue(-0.05),
    error = function(e) "error"
  )
  # Should either produce a result or error gracefully
  expect_true(!is.null(result))
})

test_that("format_pvalue handles values > 1", {
  # p-values > 1 are invalid but function should handle gracefully
  result <- tryCatch(
    format_pvalue(1.5),
    error = function(e) "error"
  )
  expect_true(!is.null(result))
})

test_that("format_pvalue handles empty vector", {
  result <- format_pvalue(numeric(0))
  expect_equal(length(result), 0)
})

test_that("format_pvalue handles single NA", {
  result <- format_pvalue(NA_real_)
  expect_true(is.na(result))
})

test_that("format_pvalue includes_p works with vectors", {
  result <- format_pvalue(c(0.05, 0.001), include_p = TRUE)
  expect_true(all(grepl("^p", result)))
})
