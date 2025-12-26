test_that("plot_cdf runs without errors", {
  y <- rnorm(100)
  group <- rep(c("A", "B", "C"), c(30, 40, 30))
  
  # Should not throw errors
  expect_error(plot_cdf(y ~ group), NA)
  
  # Should return invisible list with ECDFs
  result <- plot_cdf(y ~ group)
  expect_true(is.list(result))
  expect_true("ecdfs" %in% names(result))
  expect_equal(length(result$ecdfs), 3)
  expect_true(all(sapply(result$ecdfs, is.function)))
})

test_that("plot_cdf handles data frame input", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  # Use formula syntax
  expect_error(plot_cdf(value ~ group, data = df), NA)
  
  # Check return value
  result <- plot_cdf(value ~ group, data = df)
  expect_equal(length(result$ecdfs), 2)
})

test_that("plot_cdf handles formula syntax", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  expect_error(plot_cdf(value ~ group, data = df), NA)
  
  result <- plot_cdf(value ~ group, data = df)
  expect_equal(length(result$ecdfs), 2)
})

test_that("plot_cdf handles missing values", {
  y <- c(rnorm(90), rep(NA, 10))
  group <- rep(c("A", "B"), 50)
  
  # Should handle NAs gracefully
  expect_error(plot_cdf(y ~ group), NA)
  
  result <- plot_cdf(y ~ group)
  expect_equal(length(result$ecdfs), 2)
})

test_that("plot_cdf handles custom parameters", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  # Custom colors
  expect_error(plot_cdf(y ~ group, col = c("red", "blue")), NA)
  
  # Custom line width
  expect_error(plot_cdf(y ~ group, lwd = 2), NA)
  
  # Custom line type
  expect_error(plot_cdf(y ~ group, lty = c(1, 2)), NA)
})

test_that("plot_cdf handles show.ks parameter", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  expect_error(plot_cdf(y ~ group, show.ks = TRUE), NA)
  expect_error(plot_cdf(y ~ group, show.ks = FALSE), NA)
  
  # With 2 groups, KS test should be performed
  result <- plot_cdf(y ~ group, show.ks = TRUE)
  if (length(unique(group)) == 2) {
    expect_true("ks_test" %in% names(result))
  }
})

test_that("plot_cdf handles show.quantiles parameter", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  expect_error(plot_cdf(y ~ group, show.quantiles = TRUE), NA)
  expect_error(plot_cdf(y ~ group, show.quantiles = FALSE), NA)
})

test_that("plot_cdf handles different numbers of groups", {
  y <- rnorm(100)
  
  # Two groups
  group2 <- rep(c("A", "B"), 50)
  expect_error(plot_cdf(y ~ group2), NA)
  
  # Three groups
  group3 <- rep(c("A", "B", "C"), c(30, 40, 30))
  expect_error(plot_cdf(y ~ group3), NA)
  
  # Four groups
  group4 <- rep(c("A", "B", "C", "D"), c(25, 25, 25, 25))
  expect_error(plot_cdf(y ~ group4), NA)
})

test_that("plot_cdf returns correct structure", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- plot_cdf(y ~ group)
  
  # Check structure
  expect_true(is.list(result))
  expect_true("ecdfs" %in% names(result))
  expect_true(is.list(result$ecdfs))
  
  # Check ECDF functions
  expect_true(all(sapply(result$ecdfs, is.function)))
  expect_equal(names(result$ecdfs), c("A", "B"))
  
  # ECDFs should be monotonic
  test_vals <- seq(min(y), max(y), length.out = 10)
  for (ecdf_func in result$ecdfs) {
    vals <- ecdf_func(test_vals)
    expect_true(all(diff(vals) >= 0))
  }
})

test_that("plot_cdf error message shows correct dataset name", {
  # Create a dataset with a specific name
  IV5 <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  # Try to access a variable that doesn't exist - should show "IV5" not "data"
  expect_error(
    plot_cdf(nonexistent ~ group, data = IV5),
    'Variable "nonexistent" not found in dataset "IV5"',
    fixed = TRUE
  )
})

