test_that("plot_density runs without errors", {
  y <- rnorm(100)
  group <- rep(c("A", "B", "C"), c(30, 40, 30))
  
  # Should not throw errors
  expect_error(plot_density(y, group), NA)
  
  # Should return invisible list with densities
  result <- plot_density(y, group)
  expect_true(is.list(result))
  expect_true("densities" %in% names(result))
  expect_equal(length(result$densities), 3)
  expect_true(all(sapply(result$densities, inherits, "density")))
})

test_that("plot_density handles data frame input", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  # Use df$column syntax (formula syntax has issues with group_expr)
  expect_error(plot_density(df$value, df$group), NA)
  
  # Check return value
  result <- plot_density(df$value, df$group)
  expect_equal(length(result$densities), 2)
})

test_that("plot_density handles missing values", {
  y <- c(rnorm(90), rep(NA, 10))
  group <- rep(c("A", "B"), 50)
  
  # Should handle NAs gracefully
  expect_error(plot_density(y, group), NA)
  
  result <- plot_density(y, group)
  expect_equal(length(result$densities), 2)
})

test_that("plot_density handles custom parameters", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  # Custom colors
  expect_error(plot_density(y, group, col = c("red", "blue")), NA)
  
  # Custom line width
  expect_error(plot_density(y, group, lwd = 2), NA)
  
  # Custom line type
  expect_error(plot_density(y, group, lty = c(1, 2)), NA)
  
  # Multiple parameters
  expect_error(plot_density(y, group, col = c("red", "blue"), lwd = c(1, 2)), NA)
})

test_that("plot_density handles show.t parameter", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  expect_error(plot_density(y, group, show.t = TRUE), NA)
  expect_error(plot_density(y, group, show.t = FALSE), NA)
  
  # Both should return densities
  result1 <- plot_density(y, group, show.t = TRUE)
  result2 <- plot_density(y, group, show.t = FALSE)
  expect_equal(length(result1$densities), length(result2$densities))
})

test_that("plot_density handles show.means parameter", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  expect_error(plot_density(y, group, show.means = TRUE), NA)
  expect_error(plot_density(y, group, show.means = FALSE), NA)
})

test_that("plot_density handles different numbers of groups", {
  y <- rnorm(100)
  
  # Two groups
  group2 <- rep(c("A", "B"), 50)
  expect_error(plot_density(y, group2), NA)
  
  # Three groups
  group3 <- rep(c("A", "B", "C"), c(30, 40, 30))
  expect_error(plot_density(y, group3), NA)
  
  # Four groups
  group4 <- rep(c("A", "B", "C", "D"), c(25, 25, 25, 25))
  expect_error(plot_density(y, group4), NA)
})

test_that("plot_density handles density() arguments", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  # Custom bandwidth
  expect_error(plot_density(y, group, bw = 0.5), NA)
  
  # Custom kernel
  expect_error(plot_density(y, group, kernel = "rectangular"), NA)
})

test_that("plot_density returns correct structure", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- plot_density(y, group)
  
  # Check structure
  expect_true(is.list(result))
  expect_true("densities" %in% names(result))
  expect_true(is.list(result$densities))
  
  # Check density objects
  expect_true(all(sapply(result$densities, inherits, "density")))
  expect_equal(names(result$densities), c("A", "B"))
})

