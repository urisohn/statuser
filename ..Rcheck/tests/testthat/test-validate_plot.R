#validate_plot_001
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

#validate_plot_002
test_that("validate_plot handles data frame input", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  result <- validate_plot("value", "group", data = df, require_group = TRUE)
  expect_equal(length(result$y), 100)
  expect_equal(result$y_name, "value")
  expect_equal(result$group_name, "group")
})

#validate_plot_003
test_that("validate_plot handles formula syntax", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  result <- validate_plot(value ~ group, data = df, require_group = TRUE)
  expect_equal(length(result$y), 100)
  expect_equal(result$y_name, "value")
  expect_equal(result$group_name, "group")
})

#validate_plot_004
test_that("validate_plot handles formula syntax without data", {
  value <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- validate_plot(value ~ group, require_group = TRUE)
  expect_equal(length(result$y), 100)
  expect_equal(result$y_name, "value")
  expect_equal(result$group_name, "group")
})

#validate_plot_005
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

#validate_plot_006
test_that("validate_plot handles optional group", {
  y <- rnorm(100)
  
  # Should work when group is optional
  result <- validate_plot(y, NULL, require_group = FALSE)
  expect_equal(length(result$y), 100)
  expect_null(result$group)
})

#validate_plot_007
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

#validate_plot_008
test_that("validate_plot error includes variable name", {
  df <- data.frame(value = rnorm(10))
  
  expect_error(
    validate_plot(missing_var ~ group, data = df),
    "missing_var"
  )
})

#validate_plot_009
test_that("validate_plot error message includes function name", {
  y <- letters[1:10]  # Non-numeric
  
  expect_error(
    validate_plot(y, NULL, func_name = "my_func", require_group = FALSE),
    "my_func"
  )
})

#validate_plot_010
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

#validate_plot_011
test_that("validate_plot handles formula with no group (y ~ 1)", {
  df <- data.frame(value = rnorm(50))
  
  # Formula with just y ~ 1 should work when group not required
  result <- validate_plot(value ~ 1, data = df, require_group = FALSE)
  expect_equal(length(result$y), 50)
  expect_null(result$group)
})

#validate_plot_012
test_that("validate_plot with explicit data_name parameter", {
  my_data <- data.frame(value = rnorm(50), group = rep("A", 50))
  
  # When data_name is explicitly provided, it should be used
  result <- validate_plot(value ~ group, data = my_data, require_group = TRUE, data_name = "my_data")
  expect_equal(result$data_name, "my_data")
})

#validate_plot_013
test_that("extract_expr_label strips df$ prefixes and keeps expression structure", {
  expect_equal(
    statuser:::.extract_expr_label(quote(df$gpa8 - df$gpa7)),
    "gpa8 - gpa7"
  )
  expect_equal(
    statuser:::.extract_expr_label(quote(df$gpa8-df$gpa7)),
    "gpa8 - gpa7"
  )
  expect_equal(
    statuser:::.extract_expr_label(quote(round(df$gpa5, 1))),
    "round(gpa5, 1)"
  )
  expect_equal(
    statuser:::.extract_expr_label(quote(mean(df$gpa5))),
    "mean(gpa5)"
  )
  expect_equal(
    statuser:::.extract_expr_label(quote(df$gpa7)),
    "gpa7"
  )
})

#validate_plot_014
test_that("evaluate_variable_arguments labels complex expressions via extract_expr_label", {
  df <- data.frame(gpa8 = 1:5, gpa7 = 6:10, gpa5 = 1:5)
  res <- statuser:::evaluate_variable_arguments(
    arg_expr = quote(df$gpa8 - df$gpa7),
    calling_env = environment(),
    func_name = "test"
  )
  expect_equal(res$name, "gpa8 - gpa7")
  res2 <- statuser:::evaluate_variable_arguments(
    arg_expr = quote(round(df$gpa5, 1)),
    calling_env = environment(),
    func_name = "test"
  )
  expect_equal(res2$name, "round(gpa5, 1)")
})

#validate_plot_015
test_that("validate_plot labels formula with $ expressions correctly", {
  df <- data.frame(gpa5 = 1:5, completed = rep(c("Y", "N"), length.out = 5))
  result <- validate_plot(round(df$gpa5, 1) ~ df$completed, require_group = TRUE)
  expect_equal(result$y_name, "round(gpa5, 1)")
  expect_equal(result$group_name, "completed")
})



