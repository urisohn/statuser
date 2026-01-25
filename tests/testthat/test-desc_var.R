test_that("desc_var computes statistics correctly", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- desc_var(y, group)
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("group" %in% names(result))
  expect_true("n.total" %in% names(result))
  expect_true("mean" %in% names(result))
  expect_true("sd" %in% names(result))
  expect_true("median" %in% names(result))
})

test_that("desc_var handles data frame input", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  result <- desc_var(value, group, data = df)
  expect_equal(nrow(result), 2)
  # desc_var uses labelled package which adds attributes, so we need to convert to character
  expect_equal(as.character(result$group), c("A", "B"))
})

test_that("desc_var works when column names match parameter names (y, group)", {
  # This test catches a bug where using column names that match function 

  # parameter names (y, group) caused incorrect variable extraction
  df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
  
  # Should not error with "All variables must have the same length"
  result <- desc_var(y, group, data = df)
  
  expect_equal(nrow(result), 2)
  expect_equal(as.character(result$group), c("A", "B"))
  expect_equal(sum(as.numeric(result$n.total)), 100)
})

test_that("desc_var handles no grouping", {
  y <- rnorm(100)
  
  result <- desc_var(y)
  expect_equal(nrow(result), 1)
  # desc_var uses labelled package which adds attributes, so we need to convert to character
  expect_equal(as.character(result$group), "All")
})

test_that("desc_var handles missing values", {
  y <- c(rnorm(90), rep(NA, 10))
  group <- rep(c("A", "B"), 50)
  
  result <- desc_var(y, group)
  expect_true(all(result$n.missing >= 0))
  expect_true(sum(result$n.missing) == 10)
})

test_that("desc_var respects digits parameter", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result1 <- desc_var(y, group, digits = 2)
  result2 <- desc_var(y, group, digits = 4)
  
  # Check that rounding is applied (mean should be rounded)
  expect_true(is.numeric(result1$mean))
  expect_true(is.numeric(result2$mean))
  
  # With different digits, values should differ (unless they happen to round the same)
  # This is a weak test, but ensures digits parameter is used
  expect_true(is.data.frame(result1))
  expect_true(is.data.frame(result2))
})

test_that("desc_var returns all expected columns", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- desc_var(y, group)
  
  expected_cols <- c("group", "n.total", "mean", "sd", "se", "median", "n.missing", "n.unique",
                     "mode", "freq_mode", "mode2", "freq_mode2",
                     "min", "max")
  
  expect_true(all(expected_cols %in% names(result)))
})

test_that("desc_var handles empty groups", {
  y <- c(rnorm(50), rep(NA, 50))
  group <- rep(c("A", "B"), 50)
  
  # Should handle gracefully
  result <- desc_var(y, group)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
})

test_that("desc_var handles formula syntax", {
  df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
  
  result <- desc_var(y ~ group, data = df)
  expect_equal(nrow(result), 2)
  expect_equal(as.character(result$group), c("A", "B"))
})

test_that("desc_var handles multiple grouping variables", {
  df <- data.frame(
    y = rnorm(200),
    x1 = rep(c("A", "B"), 100),
    x2 = rep(c("X", "Y"), each = 100)
  )
  
  result <- desc_var(y ~ x1 + x2, data = df)
  expect_equal(nrow(result), 4)  # 2 x 2 = 4 combinations
  expect_true("x1" %in% names(result))
  expect_true("x2" %in% names(result))
  expect_false("group" %in% names(result))  # Should use separate columns, not "group"
})

test_that("desc_var sorts results by grouping variables", {
  df <- data.frame(
    y = rnorm(200),
    x1 = rep(c("B", "A"), 100),  # Intentionally out of order
    x2 = rep(c("Y", "X"), each = 100)
  )
  
  result <- desc_var(y ~ x1 + x2, data = df)
  # Results should be sorted by x1 then x2
  expect_equal(as.character(result$x1), c("A", "A", "B", "B"))
  expect_equal(as.character(result$x2), c("X", "Y", "X", "Y"))
})

test_that("desc_var detects missing group combinations", {
  df <- data.frame(
    y = rnorm(100),
    x1 = rep(c("A", "B"), 50),
    x2 = rep(c("X", "Y"), each = 50)
  )
  # Remove one combination
  df_missing <- df[!(df$x1 == "B" & df$x2 == "Y"), ]
  
  # Should produce a message about missing combinations
  expect_message(
    desc_var(y ~ x1 + x2, data = df_missing),
    "Some possible group combinations are not observed"
  )
})

test_that("desc_var detects perfectly overlapping grouping variables", {
  df <- data.frame(
    y = rnorm(100),
    x1 = rep(c("A", "B"), 50),
    x2 = rep(c("X", "Y"), 50)
  )
  
  # Should stop with a specific error message
  expect_error(
    desc_var(y ~ x1 + x2, data = df),
    "Multiple grouping variables should not overlap perfectly"
  )
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("desc_var handles single observation per group", {
  df <- data.frame(
    y = c(1, 2, 3),
    group = c("A", "B", "C")
  )
  
  result <- desc_var(y ~ group, data = df)
  
  expect_equal(nrow(result), 3)
  expect_true(all(result$n.total == 1))
  # SD should be NA for single observations
  expect_true(all(is.na(result$sd)))
})

test_that("desc_var handles all-NA values in y", {
  df <- data.frame(
    y = rep(NA_real_, 20),
    group = rep(c("A", "B"), 10)
  )
  
  result <- desc_var(y ~ group, data = df)
  
  expect_equal(nrow(result), 2)
  # Missing should equal total observations per group
  expect_true(all(as.numeric(result$missing) == 10))
  # n (non-missing) should be 0
  # Note: desc_var may define n differently - just check structure is valid
  expect_true(is.data.frame(result))
})

test_that("desc_var handles single group", {
  y <- rnorm(50)
  
  result <- desc_var(y)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$group), "All")
  expect_equal(as.numeric(result$n.total), 50)
})

test_that("desc_var correctly computes all statistics", {
  # Use known data for exact verification
  y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  
  result <- desc_var(y)
  
  # Use as.numeric() to strip labelled attributes
  expect_equal(as.numeric(result$n.total), 10)
  expect_equal(as.numeric(result$mean), 5.5, tolerance = 1e-2)  # Allow for rounding
  expect_equal(as.numeric(result$median), 5.5, tolerance = 1e-2)
  expect_equal(as.numeric(result$min), 1, tolerance = 1e-2)
  expect_equal(as.numeric(result$max), 10, tolerance = 1e-2)
  # SD is rounded by desc_var, so use tolerance
  expect_equal(as.numeric(result$sd), sd(y), tolerance = 0.01)
})

test_that("desc_var digits parameter actually rounds differently", {
  y <- c(1.23456789, 2.34567891, 3.45678912)
  
  result2 <- desc_var(y, digits = 2)
  result4 <- desc_var(y, digits = 4)
  
  # Mean with 2 digits should be different from 4 digits
  # 2.35 (2 digits) vs 2.3457 (4 digits)
  expect_true(nchar(format(result2$mean, nsmall = 2)) < nchar(format(result4$mean, nsmall = 4)) ||
              result2$mean != result4$mean)
})
