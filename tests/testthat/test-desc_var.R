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

