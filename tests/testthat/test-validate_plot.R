test_that("validate_plot handles standard syntax correctly", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- validate_plot(y, group, require_group = TRUE)
  expect_equal(length(result$y), 100)
  expect_equal(length(result$group), 100)
  expect_true(is.numeric(result$y))
  expect_equal(result$y_name_raw, "y")
  expect_equal(result$group_name_raw, "group")
})

test_that("validate_plot handles data frame input", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  result <- validate_plot("value", "group", data = df, require_group = TRUE)
  expect_equal(length(result$y), 100)
  expect_equal(result$y_name, "value")
  expect_equal(result$group_name, "group")
})

test_that("validate_plot handles formula syntax", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  result <- validate_plot(value ~ group, data = df, require_group = TRUE)
  expect_equal(length(result$y), 100)
  expect_equal(result$y_name, "value")
  expect_equal(result$group_name, "group")
})

test_that("validate_plot handles formula syntax without data", {
  value <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- validate_plot(value ~ group, require_group = TRUE)
  expect_equal(length(result$y), 100)
  expect_equal(result$y_name, "value")
  expect_equal(result$group_name, "group")
})

test_that("validate_plot throws error for invalid inputs", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  # Wrong length
  expect_error(validate_plot(y, group[1:50], require_group = TRUE))
  
  # Non-numeric y
  expect_error(validate_plot(letters[1:10], rep(c("A", "B"), 5), require_group = TRUE))
  
  # Missing group when required
  expect_error(validate_plot(y, NULL, require_group = TRUE))
  
  # Missing column in data
  df <- data.frame(value = rnorm(100))
  expect_error(validate_plot("value", "group", data = df, require_group = TRUE))
  
  # Invalid data (not a data frame)
  expect_error(validate_plot("value", "group", data = list(value = 1:10), require_group = TRUE))
})

test_that("validate_plot handles optional group", {
  y <- rnorm(100)
  
  # Should work when group is optional
  result <- validate_plot(y, NULL, require_group = FALSE)
  expect_equal(length(result$y), 100)
  expect_null(result$group)
})

test_that("validate_plot handles df$var syntax", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  result <- validate_plot(df$value, df$group, require_group = TRUE)
  expect_equal(length(result$y), 100)
  expect_equal(result$y_name, "value")
  expect_equal(result$group_name, "group")
})

# ============================================================================
# ERROR MESSAGE QUALITY TESTS
# ============================================================================

test_that("validate_plot error includes variable name", {
  df <- data.frame(value = rnorm(10))
  
  expect_error(
    validate_plot(missing_var ~ group, data = df),
    "missing_var"
  )
})

test_that("validate_plot error message includes function name", {
  y <- letters[1:10]  # Non-numeric
  
  expect_error(
    validate_plot(y, NULL, func_name = "my_func", require_group = FALSE),
    "my_func"
  )
})

test_that("validate_plot error for length mismatch includes lengths", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 25)  # Length 50
  
  expect_error(
    validate_plot(y, group, require_group = TRUE),
    "100"
  )
  expect_error(
    validate_plot(y, group, require_group = TRUE),
    "50"
  )
})

# ============================================================================
# ADDITIONAL VALIDATION TESTS
# ============================================================================

test_that("validate_plot handles formula with no group (y ~ 1)", {
  df <- data.frame(value = rnorm(50))
  
  # Formula with just y ~ 1 should work when group not required
  result <- validate_plot(value ~ 1, data = df, require_group = FALSE)
  expect_equal(length(result$y), 50)
  expect_null(result$group)
})

test_that("validate_plot with explicit data_name parameter", {
  my_data <- data.frame(value = rnorm(50), group = rep("A", 50))
  
  # When data_name is explicitly provided, it should be used
  result <- validate_plot(value ~ group, data = my_data, require_group = TRUE, data_name = "my_data")
  expect_equal(result$data_name, "my_data")
})





