test_that("table2 creates basic table", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  result <- table2(x, y)
  
  # Result is now a list with class table2
  expect_true(inherits(result, "table2"))
  expect_true(is.list(result))
  expect_true("freq" %in% names(result))
  expect_true("prop" %in% names(result))
  expect_true("chisq" %in% names(result))
  
  # freq should be the frequency table
  expect_true(inherits(result$freq, "table"))
  expect_equal(length(dim(result$freq)), 2)
  expect_equal(dim(result$freq), c(2, 2))
  
  # prop and chisq should be NULL when not requested
  expect_null(result$prop)
  expect_null(result$chisq)
})

test_that("table2 handles data frame column references", {
  df <- data.frame(
    group = c("A", "A", "B", "B", "A"),
    status = c("X", "Y", "X", "Y", "X")
  )
  
  result <- table2(df$group, df$status)
  
  expect_true(inherits(result, "table2"))
  expect_true(inherits(result$freq, "table"))
  expect_equal(length(dim(result$freq)), 2)
  
  # Check that dimension names are set on freq
  dim_names <- names(dimnames(result$freq))
  expect_true(!is.null(dim_names))
  expect_equal(length(dim_names), 2)
})

test_that("table2 handles three-way table", {
  df <- data.frame(
    x = c("A", "A", "B", "B"),
    y = c("X", "Y", "X", "Y"),
    z = c("high", "low", "high", "low")
  )
  
  result <- table2(df$x, df$y, df$z)
  
  expect_true(inherits(result, "table2"))
  expect_true(inherits(result$freq, "table"))
  expect_equal(length(dim(result$freq)), 3)
  expect_equal(dim(result$freq), c(2, 2, 2))
})

test_that("table2 handles prop parameter", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  # Overall proportions
  result0 <- table2(x, y, prop = 0)
  expect_true(inherits(result0, "table2"))
  expect_true(!is.null(result0$prop))
  expect_true(is.numeric(result0$prop))
  expect_true(!is.null(result0$freq))
  
  # Row proportions
  result1 <- table2(x, y, prop = 1)
  expect_true(!is.null(result1$prop))
  expect_true(is.numeric(result1$prop))
  
  # Column proportions
  result2 <- table2(x, y, prop = 2)
  expect_true(!is.null(result2$prop))
  expect_true(is.numeric(result2$prop))
  
  # Character prop values
  result_row <- table2(x, y, prop = "row")
  expect_true(!is.null(result_row$prop))
  
  result_col <- table2(x, y, prop = "col")
  expect_true(!is.null(result_col$prop))
})

test_that("table2 prop prints with decimal formatting", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  result <- table2(x, y, prop = 1)
  
  # Capture printed output
  output <- capture.output(print(result))
  

  # Should contain decimal proportions (e.g., .500, .333), not just 0s and 1s
  # Look for pattern like .XXX (decimal point followed by digits)
  expect_true(any(grepl("\\.[0-9]{3}", output)), 
              info = "Proportions should print with decimal formatting")
})

test_that("table2 handles digits parameter", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  result <- table2(x, y, prop = 1, digits = 2)
  
  # Check that prop is numeric
  expect_true(!is.null(result$prop))
  expect_true(is.numeric(result$prop))
})

test_that("table2 handles useNA parameter", {
  x <- c("A", "A", NA, "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  # Without NA - should exclude NA values
  result_no <- table2(x, y, useNA = "no")
  dimnames_x_no <- dimnames(result_no$freq)[[1]]
  expect_false(any(is.na(dimnames_x_no)))
  
  # With NA if any - may include NA if present
  result_ifany <- table2(x, y, useNA = "ifany")
  # May or may not include NA depending on data
  
  # Always with NA - should include NA in dimnames when there are NAs in data
  result_always <- table2(x, y, useNA = "always")
  expect_true(!is.null(result_always$freq))
  expect_true(!is.null(dimnames(result_always$freq)))
  dimnames_x_always <- dimnames(result_always$freq)[[1]]
  expect_true(!is.null(dimnames_x_always))
  expect_true(length(dimnames_x_always) > 0)
  # When useNA="always" and there are NAs, NA should be in dimnames
  expect_true(any(is.na(dimnames_x_always)) || "<NA>" %in% dimnames_x_always)
})

test_that("table2 handles exclude parameter", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  # Should work with exclude
  result <- table2(x, y, exclude = NULL)
  expect_true(inherits(result, "table2"))
  expect_true(inherits(result$freq, "table"))
})

test_that("table2 returns correct structure", {
  x <- c("A", "A", "B", "B")
  y <- c("X", "Y", "X", "Y")
  
  result <- table2(x, y)
  
  # Check structure - result is a list with table2 class
  expect_true(inherits(result, "table2"))
  expect_true(inherits(result$freq, "table"))
  expect_equal(length(dim(result$freq)), 2)
  
  # Check values sum correctly
  expect_equal(sum(result$freq), length(x))
})

test_that("table2 handles single vector", {
  x <- c("A", "A", "B", "B", "A")
  
  result <- table2(x)
  
  expect_true(inherits(result, "table2"))
  expect_true(inherits(result$freq, "table"))
  expect_equal(length(dim(result$freq)), 1)
  expect_equal(sum(result$freq), length(x))
})

test_that("table2 prop='ALL' Total row sums to 1.0", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  # Test with prop='ALL' (overall proportions)
  result <- table2(x, y, prop = "ALL")
  prop_table <- result$prop
  
  # Check that the Total row (excluding the bottom right 1.0) sums to 1.0
  n_rows <- nrow(prop_table)
  n_cols <- ncol(prop_table)
  total_row <- prop_table[n_rows, -n_cols, drop = FALSE]
  total_row_sum <- sum(total_row, na.rm = TRUE)
  
  # Should sum to 1.0 (within rounding tolerance)
  expect_equal(total_row_sum, 1.0, tolerance = 0.001)
  
  # Also test with prop=0 (numeric equivalent)
  result0 <- table2(x, y, prop = 0)
  prop_table0 <- result0$prop
  n_rows0 <- nrow(prop_table0)
  n_cols0 <- ncol(prop_table0)
  total_row0 <- prop_table0[n_rows0, -n_cols0, drop = FALSE]
  total_row_sum0 <- sum(total_row0, na.rm = TRUE)
  expect_equal(total_row_sum0, 1.0, tolerance = 0.001)
})

test_that("table2 chi parameter returns chi-square test", {
  # Use larger sample to avoid chi-squared approximation warning
  set.seed(123)
  x <- sample(c("A", "B"), 100, replace = TRUE)
  y <- sample(c("X", "Y"), 100, replace = TRUE)
  
  # Without chi test
  result_no_chi <- table2(x, y)
  expect_null(result_no_chi$chisq)
  
  # With chi test
  result_chi <- table2(x, y, chi = TRUE)
  expect_true(!is.null(result_chi$chisq))
  expect_true(inherits(result_chi$chisq, "htest"))
  
  # Chi-square test with proportions
  result_chi_prop <- table2(x, y, prop = 1, chi = TRUE)
  expect_true(!is.null(result_chi_prop$chisq))
  expect_true(!is.null(result_chi_prop$prop))
  expect_true(!is.null(result_chi_prop$freq))
})

# ============================================================================
# EDGE CASES AND ADDITIONAL TESTS
# ============================================================================

test_that("table2 handles factors", {
  x <- factor(c("A", "A", "B", "B"))
  y <- factor(c("X", "Y", "X", "Y"))
  
  result <- table2(x, y)
  
  expect_true(inherits(result, "table2"))
  expect_equal(dim(result$freq), c(2, 2))
})

test_that("table2 handles numeric variables", {
  x <- c(1, 1, 2, 2)
  y <- c(10, 20, 10, 20)
  
  result <- table2(x, y)
  
  expect_true(inherits(result, "table2"))
  expect_true(inherits(result$freq, "table"))
})

test_that("table2 handles many categories", {
  x <- sample(LETTERS[1:5], 100, replace = TRUE)
  y <- sample(letters[1:4], 100, replace = TRUE)
  
  result <- table2(x, y)
  
  expect_equal(dim(result$freq), c(5, 4))
})

test_that("table2 chi-square test produces valid statistics", {
  set.seed(456)
  # Create data with known association
  x <- sample(c("A", "B"), 200, replace = TRUE)
  y <- ifelse(x == "A", 
              sample(c("X", "Y"), 200, replace = TRUE, prob = c(0.8, 0.2)),
              sample(c("X", "Y"), 200, replace = TRUE, prob = c(0.2, 0.8)))
  
  result <- table2(x, y, chi = TRUE)
  
  # Chi-square statistic should be positive
  expect_true(result$chisq$statistic > 0)
  # p-value should be between 0 and 1
  expect_true(result$chisq$p.value >= 0 && result$chisq$p.value <= 1)
})

test_that("table2 prop values sum correctly for row proportions", {
  x <- c("A", "A", "A", "B", "B")
  y <- c("X", "X", "Y", "X", "Y")
  
  result <- table2(x, y, prop = 1)  # Row proportions
  
  # Each row (excluding Total) should sum to 1
  prop_matrix <- result$prop
  n_rows <- nrow(prop_matrix)
  n_cols <- ncol(prop_matrix)
  
  # Check first row sums to 1 (excluding Total column)
  row1_sum <- sum(prop_matrix[1, 1:(n_cols-1)], na.rm = TRUE)
  expect_equal(row1_sum, 1.0, tolerance = 0.001)
})

test_that("table2 prop values sum correctly for column proportions", {
  x <- c("A", "A", "A", "B", "B")
  y <- c("X", "X", "Y", "X", "Y")
  
  result <- table2(x, y, prop = 2)  # Column proportions
  
  # Each column (excluding Total row) should sum to 1
  prop_matrix <- result$prop
  n_rows <- nrow(prop_matrix)
  n_cols <- ncol(prop_matrix)
  
  # Check first column sums to 1 (excluding Total row)
  col1_sum <- sum(prop_matrix[1:(n_rows-1), 1], na.rm = TRUE)
  expect_equal(col1_sum, 1.0, tolerance = 0.001)
})

# ============================================================================
# SNAPSHOT TESTS FOR OUTPUT FORMAT
# ============================================================================

test_that("table2 print output for frequency table is stable", {
  set.seed(42)
  gender <- sample(c("Male", "Female"), 100, replace = TRUE)
  response <- sample(c("Yes", "No", "Maybe"), 100, replace = TRUE)
  
  result <- table2(gender, response)
  
  expect_snapshot(print(result))
})

test_that("table2 print output with row proportions is stable", {
  set.seed(42)
  gender <- sample(c("Male", "Female"), 100, replace = TRUE)
  response <- sample(c("Yes", "No"), 100, replace = TRUE)
  
  result <- table2(gender, response, prop = 1)
  
  expect_snapshot(print(result))
})

test_that("table2 print output with column proportions is stable", {
  set.seed(42)
  gender <- sample(c("Male", "Female"), 100, replace = TRUE)
  response <- sample(c("Yes", "No"), 100, replace = TRUE)
  
  result <- table2(gender, response, prop = 2)
  
  expect_snapshot(print(result))
})

test_that("table2 print output with chi-square test is stable", {
  set.seed(42)
  gender <- sample(c("Male", "Female"), 100, replace = TRUE)
  response <- sample(c("Yes", "No"), 100, replace = TRUE)
  
  result <- table2(gender, response, chi = TRUE)
  
  expect_snapshot(print(result))
})

test_that("table2 print output for three-way table is stable", {
  set.seed(42)
  x <- sample(c("A", "B"), 60, replace = TRUE)
  y <- sample(c("X", "Y"), 60, replace = TRUE)
  z <- sample(c("High", "Low"), 60, replace = TRUE)
  
  result <- table2(x, y, z)
  
  expect_snapshot(print(result))
})
