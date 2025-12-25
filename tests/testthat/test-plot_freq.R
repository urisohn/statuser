test_that("plot_freq runs without errors", {
  x <- c(1, 1, 2, 2, 2, 5, 5)
  
  # Should not throw errors
  expect_error(plot_freq(x), NA)
  
  # Should return invisible data frame
  result <- plot_freq(x)
  expect_true(is.data.frame(result))
  expect_true("value" %in% names(result) || "x" %in% names(result))
})

test_that("plot_freq handles data frame input", {
  df <- data.frame(value = c(1, 1, 2, 2, 2, 5, 5))
  
  # Use df$column syntax
  expect_error(plot_freq(df$value), NA)
  
  result <- plot_freq(df$value)
  expect_true(is.data.frame(result))
})

test_that("plot_freq handles grouping", {
  x <- c(1, 1, 2, 2, 2, 5, 5)
  group <- c("A", "A", "A", "B", "B", "A", "B")
  
  expect_error(plot_freq(x, group = group), NA)
  
  result <- plot_freq(x, group = group)
  expect_true(is.data.frame(result))
})

test_that("plot_freq handles group with data frame", {
  df <- data.frame(
    value = c(1, 1, 2, 2, 2, 5, 5),
    group = c("A", "A", "A", "B", "B", "A", "B")
  )
  
  # Use df$column syntax
  expect_error(plot_freq(df$value, group = df$group), NA)
  
  result <- plot_freq(df$value, group = df$group)
  expect_true(is.data.frame(result))
})

test_that("plot_freq handles freq parameter", {
  x <- c(1, 1, 2, 2, 2)
  
  # Frequency plot
  expect_error(plot_freq(x, freq = TRUE), NA)
  
  # Proportion plot
  expect_error(plot_freq(x, freq = FALSE), NA)
})

test_that("plot_freq handles custom parameters", {
  x <- c(1, 1, 2, 2, 2)
  
  # Custom color
  expect_error(plot_freq(x, col = "red"), NA)
  
  # Custom line width
  expect_error(plot_freq(x, lwd = 5), NA)
  
  # Custom width
  expect_error(plot_freq(x, width = 0.5), NA)
})

test_that("plot_freq handles value.labels parameter", {
  x <- c(1, 1, 2, 2, 2)
  
  # With labels
  expect_error(plot_freq(x, value.labels = TRUE), NA)
  
  # Without labels
  expect_error(plot_freq(x, value.labels = FALSE), NA)
})

test_that("plot_freq handles add parameter", {
  x <- c(1, 1, 2, 2, 2)
  
  # Create new plot
  expect_error(plot_freq(x, add = FALSE), NA)
  
  # Add to existing plot
  plot_freq(x, add = FALSE)
  expect_error(plot_freq(x + 1, add = TRUE), NA)
})

test_that("plot_freq handles show.legend parameter", {
  x <- c(1, 1, 2, 2, 2)
  group <- c("A", "A", "B", "B", "A")
  
  # With legend
  expect_error(plot_freq(x, group = group, show.legend = TRUE), NA)
  
  # Without legend
  expect_error(plot_freq(x, group = group, show.legend = FALSE), NA)
})

test_that("plot_freq returns correct structure", {
  x <- c(1, 1, 2, 2, 2, 5, 5)
  
  result <- plot_freq(x)
  
  # Check structure
  expect_true(is.data.frame(result))
  
  # Should have at least value and frequency columns
  expect_true(ncol(result) >= 2)
  expect_true(nrow(result) > 0)
})

test_that("plot_freq handles missing values", {
  x <- c(1, 1, 2, NA, 2, 5, 5)
  
  # Should handle NAs gracefully
  expect_error(plot_freq(x), NA)
  
  result <- plot_freq(x)
  expect_true(is.data.frame(result))
})

