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
  x <- c("A", "A", "B", "B", "A", "B")
  y <- c("X", "Y", "X", "Y", "X", "Y")
  
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
