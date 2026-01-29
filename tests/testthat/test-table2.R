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

test_that("table2 single vector prints without error", {
  # Regression test: table2() with one variable used to produce
  # "Error in NextMethod() : generic function not specified"
  x <- c("A", "A", "B", "B", "A")
  
  result <- table2(x)
  
  # Printing should not error
  expect_no_error(print(result))
  
  # Capture output and verify it contains expected values
  output <- capture.output(print(result))
  expect_true(any(grepl("A", output)))
  expect_true(any(grepl("B", output)))
})

test_that("table2 single vector with prop and chi prints chi-square only once", {
  # Regression test: chi-square was being printed twice for 1D tables with prop
  x <- c("A", "A", "A", "B", "B")
  
  result <- table2(x, prop = "all", chi = TRUE)
  
  # Capture output
  output <- capture.output(print(result))
  
  # Count occurrences of chi-square header
  chi_count <- sum(grepl("Chi-squared test", output))
  expect_equal(chi_count, 1, info = "Chi-square test should only be printed once")
  
  # Verify chi-square result is present
  expect_true(any(grepl("\u03c7\u00b2", output)) || any(grepl("χ²", output)), 
              info = "Chi-square statistic should be in output")
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

# ============================================================================
# CONSISTENCY TESTS WITH BASE R FUNCTIONS
# ============================================================================

test_that("table2 freq matches table() output", {
  set.seed(123)
  x <- sample(c("A", "B", "C"), 100, replace = TRUE)
  y <- sample(c("X", "Y"), 100, replace = TRUE)
  
  result <- table2(x, y)
  base_table <- table(x, y)
  

  # Frequencies should match exactly
  expect_equal(as.vector(result$freq), as.vector(base_table))
  
  # Dimensions should match
  expect_equal(dim(result$freq), dim(base_table))
})

test_that("table2 freq matches table() for single variable", {
  set.seed(456)
  x <- sample(c("A", "B", "C"), 50, replace = TRUE)
  
  result <- table2(x)
  base_table <- table(x)
  
  # Frequencies should match exactly
  expect_equal(as.vector(result$freq), as.vector(base_table))
})

test_that("table2 freq matches table() for three-way table", {
  set.seed(789)
  x <- sample(c("A", "B"), 80, replace = TRUE)
  y <- sample(c("X", "Y"), 80, replace = TRUE)
  z <- sample(c("High", "Low"), 80, replace = TRUE)
  
  result <- table2(x, y, z)
  base_table <- table(x, y, z)
  
  # Frequencies should match exactly
  expect_equal(as.vector(result$freq), as.vector(base_table))
  
  # Dimensions should match
  expect_equal(dim(result$freq), dim(base_table))
})

test_that("table2 prop='all' matches prop.table() overall proportions", {
  set.seed(111)
  x <- sample(c("A", "B", "C"), 100, replace = TRUE)
  y <- sample(c("X", "Y"), 100, replace = TRUE)
  
  result <- table2(x, y, prop = "all")
  base_table <- table(x, y)
  base_prop <- prop.table(base_table)
  
 # For prop="all", table2 adds both Total row and Total column
  # Extract inner part by excluding "Total" from both dimensions
  prop_matrix <- result$prop
  row_names <- rownames(prop_matrix)
  col_names <- colnames(prop_matrix)
  inner_rows <- row_names[row_names != "Total"]
  inner_cols <- col_names[col_names != "Total"]
  inner_prop <- prop_matrix[inner_rows, inner_cols]
  
  # table2 rounds to 3 decimal places for display
  # Compare element-by-element (same row/column order)
  for (r in inner_rows) {
    for (c in inner_cols) {
      expect_true(abs(inner_prop[r, c] - base_prop[r, c]) < 0.001,
                  info = paste("Mismatch at row", r, "col", c, 
                               "table2:", inner_prop[r, c], "prop.table:", base_prop[r, c]))
    }
  }
})

test_that("table2 prop=1 matches prop.table(margin=1) row proportions", {
  set.seed(222)
  x <- sample(c("A", "B", "C"), 100, replace = TRUE)
  y <- sample(c("X", "Y"), 100, replace = TRUE)
  
  result <- table2(x, y, prop = 1)
  base_table <- table(x, y)
  base_prop <- prop.table(base_table, margin = 1)
  
  # For prop=1 (row), table2 adds only Total column (no Total row)
  prop_matrix <- result$prop
  col_names <- colnames(prop_matrix)
  inner_cols <- col_names[col_names != "Total"]
  inner_prop <- prop_matrix[, inner_cols]
  
  # table2 rounds to 3 decimal places for display
  # Compare element-by-element (same row/column order)
  for (r in rownames(inner_prop)) {
    for (c in inner_cols) {
      expect_true(abs(inner_prop[r, c] - base_prop[r, c]) < 0.001,
                  info = paste("Mismatch at row", r, "col", c,
                               "table2:", inner_prop[r, c], "prop.table:", base_prop[r, c]))
    }
  }
})

test_that("table2 prop=2 matches prop.table(margin=2) column proportions", {
  set.seed(333)
  x <- sample(c("A", "B", "C"), 100, replace = TRUE)
  y <- sample(c("X", "Y"), 100, replace = TRUE)
  
  result <- table2(x, y, prop = 2)
  base_table <- table(x, y)
  base_prop <- prop.table(base_table, margin = 2)
  
  # For prop=2 (column), table2 adds only Total row (no Total column)
  prop_matrix <- result$prop
  row_names <- rownames(prop_matrix)
  inner_rows <- row_names[row_names != "Total"]
  inner_prop <- prop_matrix[inner_rows, ]
  
  # table2 rounds to 3 decimal places for display
  # Compare element-by-element (same row/column order)
  for (r in inner_rows) {
    for (c in colnames(inner_prop)) {
      expect_true(abs(inner_prop[r, c] - base_prop[r, c]) < 0.001,
                  info = paste("Mismatch at row", r, "col", c,
                               "table2:", inner_prop[r, c], "prop.table:", base_prop[r, c]))
    }
  }
})

test_that("table2 chi=TRUE matches chisq.test() results", {
  set.seed(444)
  x <- sample(c("A", "B", "C"), 150, replace = TRUE)
  y <- sample(c("X", "Y", "Z"), 150, replace = TRUE)
  
  result <- table2(x, y, chi = TRUE)
  base_table <- table(x, y)
  base_chisq <- chisq.test(base_table)
  
  # Chi-square statistic should match
  expect_equal(
    as.numeric(result$chisq$statistic), 
    as.numeric(base_chisq$statistic), 
    tolerance = 1e-10
  )
  
  # Degrees of freedom should match
  expect_equal(result$chisq$parameter, base_chisq$parameter)
  
  # p-value should match
  expect_equal(result$chisq$p.value, base_chisq$p.value, tolerance = 1e-10)
})

test_that("table2 chi=TRUE matches chisq.test() for 2x2 table", {
  set.seed(555)
  x <- sample(c("A", "B"), 100, replace = TRUE)
  y <- sample(c("X", "Y"), 100, replace = TRUE)
  
  # table2 defaults to correct=FALSE
  result <- table2(x, y, chi = TRUE)
  base_table <- table(x, y)
  base_chisq <- chisq.test(base_table, correct = FALSE)
  
  # Chi-square statistic should match
  expect_equal(
    as.numeric(result$chisq$statistic), 
    as.numeric(base_chisq$statistic), 
    tolerance = 1e-10
  )
  
  # p-value should match
  expect_equal(result$chisq$p.value, base_chisq$p.value, tolerance = 1e-10)
})

test_that("table2 correct parameter controls Yates continuity correction", {
  set.seed(556)
  x <- sample(c("A", "B"), 100, replace = TRUE)
  y <- sample(c("X", "Y"), 100, replace = TRUE)
  
  # Without correction (default for table2)
  result_uncorrected <- table2(x, y, chi = TRUE)
  base_uncorrected <- chisq.test(table(x, y), correct = FALSE)
  
  # With correction (explicit)
  result_corrected <- table2(x, y, chi = TRUE, correct = TRUE)
  base_corrected <- chisq.test(table(x, y), correct = TRUE)
  
  # Uncorrected results should match (default)
  expect_equal(
    as.numeric(result_uncorrected$chisq$statistic),
    as.numeric(base_uncorrected$statistic),
    tolerance = 1e-10
  )
  
  # Corrected results should match
  expect_equal(
    as.numeric(result_corrected$chisq$statistic),
    as.numeric(base_corrected$statistic),
    tolerance = 1e-10
  )
  
  # Corrected and uncorrected should differ for 2x2 tables
  expect_false(
    isTRUE(all.equal(
      as.numeric(result_corrected$chisq$statistic),
      as.numeric(result_uncorrected$chisq$statistic)
    ))
  )
})

test_that("table2 chi-square matches prop.test for 2x2 tables", {
  set.seed(557)
  x <- sample(c("A", "B"), 100, replace = TRUE)
  y <- sample(c("X", "Y"), 100, replace = TRUE)
  
  base_table <- table(x, y)
  
  # Without Yates correction (default for table2)
  result_uncorrected <- table2(x, y, chi = TRUE)
  prop_uncorrected <- prop.test(base_table, correct = FALSE)
  
  # Chi-square statistic should match prop.test (uncorrected)
  expect_equal(
    as.numeric(result_uncorrected$chisq$statistic),
    as.numeric(prop_uncorrected$statistic),
    tolerance = 1e-10
  )
  
  # p-value should match prop.test (uncorrected)
  expect_equal(
    result_uncorrected$chisq$p.value,
    prop_uncorrected$p.value,
    tolerance = 1e-10
  )
  
  # With Yates correction (explicit)
  result_corrected <- table2(x, y, chi = TRUE, correct = TRUE)
  prop_corrected <- prop.test(base_table, correct = TRUE)
  
  # Chi-square statistic should match prop.test
  expect_equal(
    as.numeric(result_corrected$chisq$statistic),
    as.numeric(prop_corrected$statistic),
    tolerance = 1e-10
  )
  
  # p-value should match prop.test
  expect_equal(
    result_corrected$chisq$p.value,
    prop_corrected$p.value,
    tolerance = 1e-10
  )
})
