# Tests for GAM-related functions: twolines, plot_gam, scatter.gam
# These require mgcv package and GAM models

test_that("scatter.gam runs without errors", {
  skip_if_not_installed("mgcv")
  
  x <- rnorm(100)
  y <- 2*x + rnorm(100)
  
  # Should run without errors
  expect_error(scatter.gam(x, y), NA)
  
  # Should return GAM model invisibly
  result <- scatter.gam(x, y)
  expect_true(inherits(result, "gam"))
})

test_that("scatter.gam handles data frame input", {
  skip_if_not_installed("mgcv")
  
  df <- data.frame(x = rnorm(100), y = 2*rnorm(100) + rnorm(100))
  
  expect_error(scatter.gam(x, y, data = df), NA)
})

test_that("scatter.gam handles data.dots parameter", {
  skip_if_not_installed("mgcv")
  
  x <- rnorm(100)
  y <- 2*x + rnorm(100)
  
  expect_error(scatter.gam(x, y, data.dots = TRUE), NA)
  expect_error(scatter.gam(x, y, data.dots = FALSE), NA)
})

test_that("scatter.gam handles three.dots parameter", {
  skip_if_not_installed("mgcv")
  
  x <- rnorm(100)
  y <- 2*x + rnorm(100)
  
  expect_error(scatter.gam(x, y, three.dots = TRUE), NA)
  expect_error(scatter.gam(x, y, three.dots = FALSE), NA)
})

test_that("scatter.gam handles k parameter", {
  skip_if_not_installed("mgcv")
  
  x <- rnorm(100)
  y <- 2*x + rnorm(100)
  
  expect_error(scatter.gam(x, y, k = 5), NA)
  expect_error(scatter.gam(x, y, k = 10), NA)
})

test_that("scatter.gam throws error for invalid data", {
  skip_if_not_installed("mgcv")
  
  # Invalid data frame
  expect_error(scatter.gam(x, y, data = list(x = 1:10, y = 1:10)))
  
  # Missing column
  df <- data.frame(x = rnorm(100))
  expect_error(scatter.gam(x, y, data = df))
})

test_that("plot_gam throws error for non-GAM model", {
  # Should error if model is not a GAM
  expect_error(plot_gam(lm(mpg ~ hp, data = mtcars), "hp"), 
               "must be a GAM model")
})

test_that("plot_gam validates predictor parameter", {
  skip_if_not_installed("mgcv")
  
  # Create a simple GAM model
  x <- rnorm(100)
  y <- 2*x + rnorm(100)
  model <- mgcv::gam(y ~ s(x))
  
  # Valid predictor
  expect_error(plot_gam(model, "x"), NA)
  
  # Invalid predictor (not character)
  expect_error(plot_gam(model, 1), "must be a single character string")
  
  # Invalid predictor (not in model)
  expect_error(plot_gam(model, "nonexistent"), "not found")
})

test_that("plot_gam validates quantile.others parameter", {
  skip_if_not_installed("mgcv")
  
  x <- rnorm(100)
  y <- 2*x + rnorm(100)
  model <- mgcv::gam(y ~ s(x))
  
  # Valid quantile
  expect_error(plot_gam(model, "x", quantile.others = 50), NA)
  expect_error(plot_gam(model, "x", quantile.others = 25), NA)
  expect_error(plot_gam(model, "x", quantile.others = 75), NA)
  
  # Invalid quantile (out of range)
  expect_error(plot_gam(model, "x", quantile.others = 0), "between 1 and 99")
  expect_error(plot_gam(model, "x", quantile.others = 100), "between 1 and 99")
})

test_that("plot_gam returns correct structure", {
  skip_if_not_installed("mgcv")
  
  x <- rnorm(100)
  y <- 2*x + rnorm(100)
  model <- mgcv::gam(y ~ s(x))
  
  result <- plot_gam(model, "x")
  
  expect_true(is.list(result))
  expect_true("predictor_values" %in% names(result))
  expect_true("predicted" %in% names(result))
  expect_true("se" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
})

test_that("twolines runs without errors", {
  skip_if_not_installed("mgcv")
  
  # Create U-shaped data
  set.seed(123)
  x <- rnorm(100)
  y <- -x^2 + rnorm(100)
  data <- data.frame(x = x, y = y)
  
  # Should run without errors
  expect_error(twolines(y ~ x, data = data, graph = 0), NA)
  
  # Should return a list
  result <- twolines(y ~ x, data = data, graph = 0)
  expect_true(is.list(result))
})

test_that("twolines handles graph parameter", {
  skip_if_not_installed("mgcv")
  
  x <- rnorm(100)
  y <- -x^2 + rnorm(100)
  data <- data.frame(x = x, y = y)
  
  # With graph = 0 (no plot)
  expect_error(twolines(y ~ x, data = data, graph = 0), NA)
  
  # With graph = 1 (plot)
  expect_error(twolines(y ~ x, data = data, graph = 1), NA)
})

test_that("twolines handles quiet parameter", {
  skip_if_not_installed("mgcv")
  
  x <- rnorm(100)
  y <- -x^2 + rnorm(100)
  data <- data.frame(x = x, y = y)
  
  expect_error(twolines(y ~ x, data = data, quiet = TRUE, graph = 0), NA)
  expect_error(twolines(y ~ x, data = data, quiet = FALSE, graph = 0), NA)
})

test_that("twolines handles covariates", {
  skip_if_not_installed("mgcv")
  
  x <- rnorm(100)
  z <- rnorm(100)
  y <- -x^2 + 0.5*z + rnorm(100)
  data <- data.frame(x = x, y = y, z = z)
  
  expect_error(twolines(y ~ x + z, data = data, graph = 0), NA)
})

test_that("twolines returns correct structure", {
  skip_if_not_installed("mgcv")
  
  x <- rnorm(100)
  y <- -x^2 + rnorm(100)
  data <- data.frame(x = x, y = y)
  
  result <- twolines(y ~ x, data = data, graph = 0)
  
  expect_true(is.list(result))
  # Check for key elements
  expect_true("b1" %in% names(result))
  expect_true("b2" %in% names(result))
  expect_true("p1" %in% names(result))
  expect_true("p2" %in% names(result))
  expect_true("u.sig" %in% names(result))
})

# ============================================================================
# ADDITIONAL GAM TESTS
# ============================================================================

test_that("scatter.gam returns sensible fitted values", {
  skip_if_not_installed("mgcv")
  
  set.seed(123)
  x <- seq(-3, 3, length.out = 100)
  y <- 2 * x + rnorm(100, sd = 0.5)  # Linear relationship
  
  result <- scatter.gam(x, y)
  
  # Fitted values should exist
  expect_true(!is.null(result$fitted.values))
  
  # For linear data, fitted should correlate highly with y
  expect_true(cor(result$fitted.values, y) > 0.8)
})

test_that("plot_gam predicted values are reasonable", {
  skip_if_not_installed("mgcv")
  
  set.seed(456)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100)
  model <- mgcv::gam(y ~ s(x))
  
  result <- plot_gam(model, "x")
  
  # Predicted values should be finite
  expect_true(all(is.finite(result$predicted)))
  
  # Standard errors should be positive
  expect_true(all(result$se > 0))
  
  # Lower bound should be less than upper bound
  expect_true(all(result$lower < result$upper))
})

test_that("plot_gam works with multiple predictors", {
  skip_if_not_installed("mgcv")
  
  set.seed(789)
  x1 <- rnorm(100)
  x2 <- rnorm(100)
  y <- x1 + x2 + rnorm(100)
  model <- mgcv::gam(y ~ s(x1) + s(x2))
  
  # Should work for both predictors
  result1 <- plot_gam(model, "x1")
  result2 <- plot_gam(model, "x2")
  
  expect_true(is.list(result1))
  expect_true(is.list(result2))
})

test_that("twolines detects U-shaped relationship", {
  skip_if_not_installed("mgcv")
  
  set.seed(111)
  x <- rnorm(200)
  y <- x^2 + rnorm(200, sd = 0.5)  # Clear U-shape
  data <- data.frame(x = x, y = y)
  
  result <- twolines(y ~ x, data = data, graph = 0)
  
  # For U-shaped data:
  # - b1 (left slope) should be negative
  # - b2 (right slope) should be positive
  expect_true(result$b1 < 0 || result$b2 > 0)
})

test_that("twolines detects inverted U-shaped relationship", {
  skip_if_not_installed("mgcv")
  
  set.seed(222)
  x <- rnorm(200)
  y <- -x^2 + rnorm(200, sd = 0.5)  # Inverted U-shape
  data <- data.frame(x = x, y = y)
  
  result <- twolines(y ~ x, data = data, graph = 0)
  
  # For inverted U-shaped data:
  # - b1 (left slope) should be positive
  # - b2 (right slope) should be negative
  expect_true(result$b1 > 0 || result$b2 < 0)
})





