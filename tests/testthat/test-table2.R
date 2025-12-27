test_that("table2 creates basic table", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  result <- table2(x, y)
  
  expect_true(inherits(result, "table"))
  expect_equal(length(dim(result)), 2)
  expect_equal(dim(result), c(2, 2))
})

test_that("table2 handles data frame column references", {
  df <- data.frame(
    group = c("A", "A", "B", "B", "A"),
    status = c("X", "Y", "X", "Y", "X")
  )
  
  result <- table2(df$group, df$status)
  
  expect_true(inherits(result, "table"))
  expect_equal(length(dim(result)), 2)
  
  # Check that dimension names are set
  dim_names <- names(dimnames(result))
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
  
  expect_true(inherits(result, "table"))
  expect_equal(length(dim(result)), 3)
  expect_equal(dim(result), c(2, 2, 2))
})

test_that("table2 handles prop parameter", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  # Overall proportions
  result0 <- table2(x, y, prop = 0)
  # When prop is specified, result may be a matrix/array with table2 class
  # Check that it has the proportion attribute
  expect_true(attr(result0, "is_proportion"))
  expect_true(is.numeric(result0))
  
  # Row proportions
  result1 <- table2(x, y, prop = 1)
  expect_true(attr(result1, "is_proportion"))
  expect_true(is.numeric(result1))
  
  # Column proportions
  result2 <- table2(x, y, prop = 2)
  expect_true(attr(result2, "is_proportion"))
  expect_true(is.numeric(result2))
  
  # Character prop values
  result_row <- table2(x, y, prop = "row")
  expect_true(attr(result_row, "is_proportion"))
  
  result_col <- table2(x, y, prop = "col")
  expect_true(attr(result_col, "is_proportion"))
})

test_that("table2 handles digits parameter", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  result <- table2(x, y, prop = 1, digits = 2)
  
  # Check that it's numeric and has the digits attribute
  expect_true(is.numeric(result))
  expect_equal(attr(result, "proportion_digits"), 2)
})

test_that("table2 handles useNA parameter", {
  x <- c("A", "A", NA, "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  # Without NA - should exclude NA values
  result_no <- table2(x, y, useNA = "no")
  dimnames_x_no <- dimnames(result_no)[[1]]
  expect_false(any(is.na(dimnames_x_no)))
  
  # With NA if any - may include NA if present
  result_ifany <- table2(x, y, useNA = "ifany")
  # May or may not include NA depending on data
  
  # Always with NA - should include NA in dimnames when there are NAs in data
  result_always <- table2(x, y, useNA = "always")
  # Check that result is valid and has dimnames
  expect_true(!is.null(result_always))
  expect_true(!is.null(dimnames(result_always)))
  dimnames_x_always <- dimnames(result_always)[[1]]
  expect_true(!is.null(dimnames_x_always))
  expect_true(length(dimnames_x_always) > 0)
  # When useNA="always" and there are NAs, NA should be in dimnames
  # Check that NA is present in the dimnames (since we have NA in x)
  expect_true(any(is.na(dimnames_x_always)) || "<NA>" %in% dimnames_x_always)
})

test_that("table2 handles exclude parameter", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  # Should work with exclude
  result <- table2(x, y, exclude = NULL)
  expect_true(inherits(result, "table"))
})

test_that("table2 returns correct structure", {
  x <- c("A", "A", "B", "B")
  y <- c("X", "Y", "X", "Y")
  
  result <- table2(x, y)
  
  # Check structure
  expect_true(inherits(result, "table"))
  expect_equal(length(dim(result)), 2)
  
  # Check values sum correctly
  expect_equal(sum(result), length(x))
})

test_that("table2 handles single vector", {
  x <- c("A", "A", "B", "B", "A")
  
  result <- table2(x)
  
  expect_true(inherits(result, "table"))
  expect_equal(length(dim(result)), 1)
  expect_equal(sum(result), length(x))
})

test_that("table2 prop='ALL' Total row sums to 1.0", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  # Test with prop='ALL' (overall proportions)
  result <- table2(x, y, prop = "ALL")
  
  # Check that the Total row (excluding the bottom right 1.0) sums to 1.0
  n_rows <- nrow(result)
  n_cols <- ncol(result)
  total_row <- result[n_rows, -n_cols, drop = FALSE]  # Total row excluding last column
  total_row_sum <- sum(total_row, na.rm = TRUE)
  
  # Should sum to 1.0 (within rounding tolerance)
  expect_equal(total_row_sum, 1.0, tolerance = 10^(-attr(result, "proportion_digits") - 1))
  
  # Also test with prop=0 (numeric equivalent)
  result0 <- table2(x, y, prop = 0)
  n_rows0 <- nrow(result0)
  n_cols0 <- ncol(result0)
  total_row0 <- result0[n_rows0, -n_cols0, drop = FALSE]
  total_row_sum0 <- sum(total_row0, na.rm = TRUE)
  expect_equal(total_row_sum0, 1.0, tolerance = 10^(-attr(result0, "proportion_digits") - 1))
})

