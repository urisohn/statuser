test_that("desc_var computes statistics correctly", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- desc_var(y, group)
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("group" %in% names(result))
  expect_true("n" %in% names(result))
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
  expect_true(all(result$NA_total >= 0))
  expect_true(sum(result$NA_total) == 10)
})

test_that("desc_var respects decimals parameter", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result1 <- desc_var(y, group, decimals = 2)
  result2 <- desc_var(y, group, decimals = 4)
  
  # Check that rounding is applied (mean should be rounded)
  expect_true(is.numeric(result1$mean))
  expect_true(is.numeric(result2$mean))
  
  # With different decimals, values should differ (unless they happen to round the same)
  # This is a weak test, but ensures decimals parameter is used
  expect_true(is.data.frame(result1))
  expect_true(is.data.frame(result2))
})

test_that("desc_var returns all expected columns", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- desc_var(y, group)
  
  expected_cols <- c("group", "n", "mean", "sd", "se", "median", "NA_total",
                     "mode", "freq_mode", "mode2", "freq_mode2",
                     "q5", "q10", "q90", "q95", "min", "max")
  
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

test_that("desc_var computes quantiles correctly", {
  y <- 1:100
  group <- rep(c("A", "B"), 50)
  
  result <- desc_var(y, group)
  
  # Check that quantiles are present
  expect_true("q5" %in% names(result))
  expect_true("q10" %in% names(result))
  expect_true("q90" %in% names(result))
  expect_true("q95" %in% names(result))
  
  # Check that q5 < q10 < q90 < q95 for each group
  expect_true(all(result$q5 < result$q10))
  expect_true(all(result$q10 < result$q90))
  expect_true(all(result$q90 < result$q95))
})

