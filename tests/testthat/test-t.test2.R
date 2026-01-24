# Tests for t.test2() function

# ============================================================================
# BASIC FUNCTIONALITY
# ============================================================================

test_that("t.test2 is exported and returns correct class", {
  x <- rnorm(50)
  y <- rnorm(50)
  result <- t.test2(x, y)
  
  expect_s3_class(result, "t.test2")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("t.test2 returns all expected columns", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- t.test2(y ~ group)
  
  # Essential columns

  expect_true("p.value" %in% names(result))
  expect_true("t" %in% names(result))
  expect_true("df" %in% names(result))
  expect_true("ci.L" %in% names(result))
  expect_true("ci.H" %in% names(result))
  
  # Attributes
  expect_true(!is.null(attr(result, "method_type")))
  expect_true(attr(result, "method_type") %in% c("student", "welch"))
})

# ============================================================================
# INPUT SYNTAX VARIATIONS
# ============================================================================

test_that("t.test2 handles formula syntax with and without data", {
  # With data argument
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  result1 <- t.test2(value ~ group, data = df)
  expect_true("A" %in% names(result1))
  expect_true("B" %in% names(result1))
  

  # Without data argument (variables in environment)
  value <- rnorm(100)
  group <- rep(c("X", "Y"), 50)
  result2 <- t.test2(value ~ group)
  expect_true("X" %in% names(result2))
  expect_true("Y" %in% names(result2))
})

test_that("t.test2 handles two-vector syntax", {
  x1 <- rnorm(50, mean = 5)
  x2 <- rnorm(50, mean = 4.8)
  
  result <- t.test2(x1, x2)
  
  expect_true("x1" %in% names(result))
  expect_true("x2" %in% names(result))
  expect_true("x1-x2" %in% names(result))
  expect_true("N(x1)" %in% names(result))
  expect_true("N(y2)" %in% names(result) || "N(x2)" %in% names(result))
})

test_that("t.test2 handles paired tests", {
  set.seed(12)
  y1 <- rnorm(100)
  y2 <- y1 + rnorm(100, sd = 0.5)  # Correlated
  
  result <- t.test2(y1, y2, paired = TRUE)
  
  expect_true(attr(result, "is_paired"))
  expect_true("N" %in% names(result))  # Single N for paired
  expect_true("r(y1,y2)" %in% names(result))  # Correlation
})

test_that("t.test2 handles one-sample test", {
  x <- rnorm(50, mean = 5)
  
  result <- t.test2(x, mu = 5)
  
  expect_true("mean" %in% names(result))
  expect_true("N" %in% names(result))
  expect_false("Group 1" %in% names(result))
  expect_false(any(grepl("-", names(result), fixed = TRUE)))  # No diff column
  
  output <- capture.output(print(result))
  expect_true(any(grepl("One sample", output)))
})

# ============================================================================
# COLUMN NAMING CONVENTIONS
# ============================================================================

test_that("t.test2 uses group values as column names for short names", {
  set.seed(12)
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- t.test2(y ~ group)
  
  expect_true("A" %in% names(result))
  expect_true("B" %in% names(result))
  expect_true("A-B" %in% names(result))
  expect_true("N(A)" %in% names(result))
  expect_true("N(B)" %in% names(result))
})

test_that("t.test2 uses Group 1/2 format for long group names", {
  set.seed(12)
  y <- rnorm(100)
  group <- rep(c("Low construal", "High construal"), 50)
  
  result <- t.test2(y ~ group)
  
  expect_true("Group 1" %in% names(result))
  expect_true("Group 2" %in% names(result))
  expect_true("1-2" %in% names(result))
  expect_true("N1" %in% names(result))
  expect_true("N2" %in% names(result))
  
  # Print should show group mapping
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("Group 1:", output_text))
  expect_true(grepl("Group 2:", output_text))
})

test_that("t.test2 formats 0/1 grouping as varname=value", {
  set.seed(12)
  y <- rnorm(100)
  x4 <- rep(0:1, 50)
  
  result <- t.test2(y ~ x4)
  
  expect_true("x4=0" %in% names(result))
  expect_true("x4=1" %in% names(result))
  expect_true("x4=0-x4=1" %in% names(result))
  expect_true("N(x4=0)" %in% names(result))
  expect_true("N(x4=1)" %in% names(result))
})

test_that("t.test2 uses Group 1/2 for long variable names with 0/1", {
  set.seed(12)
  y <- rnorm(100)
  xlongversion <- rep(0:1, 50)
  
  result <- t.test2(y ~ xlongversion)
  
  expect_true("Group 1" %in% names(result))
  expect_true("Group 2" %in% names(result))
  
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("xlongversion=0", output_text))
  expect_true(grepl("xlongversion=1", output_text))
})

# ============================================================================
# MISSING DATA HANDLING
# ============================================================================

test_that("t.test2 reports missing data for two-sample tests", {
  set.seed(12)
  y <- rnorm(100)
  y[c(10, 20, 60)] <- NA  # Missing in both groups
  group <- rep(c("A", "B"), 50)
  
  result <- t.test2(y ~ group)
  
  expect_true(attr(result, "NA1") > 0 || attr(result, "NA2") > 0)
  
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("missing", output_text, ignore.case = TRUE))
})

test_that("t.test2 reports missing data for paired tests", {
  set.seed(12)
  y1 <- rnorm(100)
  y2 <- rnorm(100)
  y1[c(50, 60)] <- NA
  
  result <- t.test2(y1, y2, paired = TRUE)
  
  expect_true(attr(result, "NA_paired") > 0)
  
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("pairs were dropped due to missing values", output_text))
})

# ============================================================================
# CONSISTENCY WITH BASE t.test()
# ============================================================================

test_that("t.test2 produces identical results to t.test", {
  set.seed(100)
  x <- rnorm(50, mean = 5)
  y <- rnorm(50, mean = 5.5)
  
  tt1 <- t.test(x, y)
  tt2 <- t.test2(x, y)
  
  expect_equal(tt2$p.value, tt1$p.value, tolerance = 1e-10)
  expect_equal(tt2$t, as.numeric(tt1$statistic), tolerance = 1e-10)
  expect_equal(tt2$df, as.numeric(tt1$parameter), tolerance = 1e-10)
  expect_equal(tt2$ci.L, tt1$conf.int[1], tolerance = 1e-10)
  expect_equal(tt2$ci.H, tt1$conf.int[2], tolerance = 1e-10)
})

test_that("t.test2 matches t.test for paired and one-sample", {
  set.seed(104)
  
  # Paired
  x <- rnorm(30, mean = 10)
  y <- x + rnorm(30, mean = 0.5, sd = 0.5)
  
  tt1_paired <- t.test(x, y, paired = TRUE)
  tt2_paired <- t.test2(x, y, paired = TRUE)
  expect_equal(tt2_paired$p.value, tt1_paired$p.value, tolerance = 1e-10)
  
  # One-sample
  z <- rnorm(50, mean = 5)
  tt1_one <- t.test(z, mu = 5)
  tt2_one <- t.test2(z, mu = 5)
  expect_equal(tt2_one$p.value, tt1_one$p.value, tolerance = 1e-10)
})

test_that("t.test2 matches t.test with formula syntax", {
  df <- data.frame(
    value = c(rnorm(30, mean = 5), rnorm(30, mean = 5.5)),
    group = rep(c("A", "B"), each = 30)
  )
  
  tt1 <- t.test(value ~ group, data = df)
  tt2 <- t.test2(value ~ group, data = df)
  
  expect_equal(tt2$p.value, tt1$p.value, tolerance = 1e-10)
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("t.test2 handles small samples", {
  # Very small (n=3)
  result1 <- t.test2(c(1, 2, 3), c(4, 5, 6))
  expect_s3_class(result1, "t.test2")
  expect_true(!is.na(result1$p.value))
  
  # Minimum (n=2)
  result2 <- t.test2(c(1, 2), c(3, 4))
  expect_s3_class(result2, "t.test2")
  expect_true(!is.na(result2$p.value))
})

test_that("t.test2 handles extreme cases", {
  set.seed(107)
  
  # Equal means (p should be large)
  x_eq <- rnorm(100, mean = 5)
  y_eq <- rnorm(100, mean = 5)
  result_eq <- t.test2(x_eq, y_eq)
  expect_true(result_eq$p.value > 0.01)
  
  # Very different means (p should be small)
  x_diff <- rnorm(50, mean = 0)
  y_diff <- rnorm(50, mean = 10)
  result_diff <- t.test2(x_diff, y_diff)
  expect_true(result_diff$p.value < 0.001)
  
  # Zero variance in one group
  x_zero <- rep(5, 10)
  y_var <- rnorm(10, mean = 6)
  result_zero <- t.test2(x_zero, y_var)
  expect_s3_class(result_zero, "t.test2")
})

test_that("t.test2 handles different grouping variable types", {
  set.seed(108)
  y <- rnorm(40)
  
  # Factor
  result_factor <- t.test2(y ~ factor(rep(c("A", "B"), 20)))
  expect_s3_class(result_factor, "t.test2")
  
  # Logical
  result_logical <- t.test2(y ~ rep(c(TRUE, FALSE), 20))
  expect_s3_class(result_logical, "t.test2")
  
  # Numeric 0/1
  result_numeric <- t.test2(y ~ rep(0:1, 20))
  expect_s3_class(result_numeric, "t.test2")
})

# ============================================================================
# REGRESSION TESTS
# ============================================================================

test_that("t.test2 uses environment variables when data= specified but vars not in data", {
  # Bug regression: t.test2 should match t.test behavior
  set.seed(12)
  n <- 100
  
  # Variables in environment
  y_env <- round(rnorm(n * 2, mean = 100, sd = 4), 0)
  group_env <- rep(c("A", "B"), n)
  
  # Different data frame
  df <- data.frame(
    y = round(rnorm(n * 2, mean = 50, sd = 2), 0),
    group = rep(c("X", "Y"), n)
  )
  
  result <- t.test2(y_env ~ group_env, data = df)
  
  # Should use environment variables (A/B, not X/Y)
  expect_true("A" %in% names(result))
  expect_true("B" %in% names(result))
  expect_false("X" %in% names(result))
  
  # Means should be ~100 (from environment), not ~50 (from data frame)
  expect_true(result[["A"]] > 90)
  expect_true(result[["B"]] > 90)
})

# ============================================================================
# SNAPSHOT TESTS FOR OUTPUT FORMAT
# ============================================================================

test_that("t.test2 print output for two-sample test is stable", {
  # Use fixed seed for reproducible output
  set.seed(42)
  x <- round(rnorm(30, mean = 100, sd = 15), 1)
  y <- round(rnorm(30, mean = 105, sd = 15), 1)
  
  result <- t.test2(x, y)
  
  expect_snapshot(print(result))
})

test_that("t.test2 print output for formula syntax is stable", {
  set.seed(42)
  df <- data.frame(
    value = round(c(rnorm(25, mean = 50), rnorm(25, mean = 55)), 1),
    group = rep(c("Control", "Treatment"), each = 25)
  )
  
  result <- t.test2(value ~ group, data = df)
  
  expect_snapshot(print(result))
})

test_that("t.test2 print output for paired test is stable", {
  set.seed(42)
  before <- round(rnorm(20, mean = 100, sd = 10), 1)
  after <- round(before + rnorm(20, mean = 5, sd = 3), 1)
  
  result <- t.test2(before, after, paired = TRUE)
  
  expect_snapshot(print(result))
})

test_that("t.test2 print output for one-sample test is stable", {
  set.seed(42)
  x <- round(rnorm(30, mean = 100, sd = 15), 1)
  
  result <- t.test2(x, mu = 100)
  
  expect_snapshot(print(result))
})

test_that("t.test2 print output with missing data is stable", {
  set.seed(42)
  x <- round(rnorm(30, mean = 100, sd = 15), 1)
  y <- round(rnorm(30, mean = 105, sd = 15), 1)
  x[c(5, 10, 15)] <- NA  # Add missing values
  
  result <- t.test2(x, y)
  
  expect_snapshot(print(result))
})

test_that("t.test2 print output with long group names is stable", {
  set.seed(42)
  df <- data.frame(
    score = round(rnorm(40, mean = 75, sd = 10), 1),
    condition = rep(c("Experimental Group A", "Control Group B"), each = 20)
  )
  
  result <- t.test2(score ~ condition, data = df)
  
  expect_snapshot(print(result))
})
