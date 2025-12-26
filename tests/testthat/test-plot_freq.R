test_that("plot_freq runs without errors", {
  x <- c(1, 1, 2, 2, 2, 5, 5)
  
  # Should not throw errors (single variable uses x ~ 1)
  expect_error(plot_freq(x ~ 1), NA)
  
  # Should return invisible data frame
  result <- plot_freq(x ~ 1)
  expect_true(is.data.frame(result))
  expect_true("value" %in% names(result) || "x" %in% names(result))
})

test_that("plot_freq handles data frame input", {
  df <- data.frame(value = c(1, 1, 2, 2, 2, 5, 5))
  
  # Use formula syntax
  expect_error(plot_freq(value ~ 1, data = df), NA)
  
  result <- plot_freq(value ~ 1, data = df)
  expect_true(is.data.frame(result))
})

test_that("plot_freq handles grouping", {
  x <- c(1, 1, 2, 2, 2, 5, 5)
  group <- c("A", "A", "A", "B", "B", "A", "B")
  
  expect_error(plot_freq(x ~ group), NA)
  
  result <- plot_freq(x ~ group)
  expect_true(is.data.frame(result))
})

test_that("plot_freq handles group with data frame", {
  df <- data.frame(
    value = c(1, 1, 2, 2, 2, 5, 5),
    group = c("A", "A", "A", "B", "B", "A", "B")
  )
  
  # Use formula syntax
  expect_error(plot_freq(value ~ group, data = df), NA)
  
  result <- plot_freq(value ~ group, data = df)
  expect_true(is.data.frame(result))
})

test_that("plot_freq handles freq parameter", {
  x <- c(1, 1, 2, 2, 2)
  
  # Frequency plot
  expect_error(plot_freq(x ~ 1, freq = TRUE), NA)
  
  # Proportion plot
  expect_error(plot_freq(x ~ 1, freq = FALSE), NA)
})

test_that("plot_freq handles custom parameters", {
  x <- c(1, 1, 2, 2, 2)
  
  # Custom color
  expect_error(plot_freq(x ~ 1, col = "red"), NA)
  
  # Custom line width
  expect_error(plot_freq(x ~ 1, lwd = 5), NA)
  
  # Custom width
  expect_error(plot_freq(x ~ 1, width = 0.5), NA)
})

test_that("plot_freq handles value.labels parameter", {
  x <- c(1, 1, 2, 2, 2)
  
  # With labels
  expect_error(plot_freq(x ~ 1, value.labels = TRUE), NA)
  
  # Without labels
  expect_error(plot_freq(x ~ 1, value.labels = FALSE), NA)
})

test_that("plot_freq handles add parameter", {
  x <- c(1, 1, 2, 2, 2)
  
  # Create new plot
  expect_error(plot_freq(x ~ 1, add = FALSE), NA)
  
  # Add to existing plot (note: x + 1 needs to be evaluated first for formula)
  plot_freq(x ~ 1, add = FALSE)
  x_plus_1 <- x + 1
  expect_error(plot_freq(x_plus_1 ~ 1, add = TRUE), NA)
})

test_that("plot_freq handles show.legend parameter", {
  x <- c(1, 1, 2, 2, 2)
  group <- c("A", "A", "B", "B", "A")
  
  # With legend
  expect_error(plot_freq(x ~ group, show.legend = TRUE), NA)
  
  # Without legend
  expect_error(plot_freq(x ~ group, show.legend = FALSE), NA)
})

test_that("plot_freq returns correct structure", {
  x <- c(1, 1, 2, 2, 2, 5, 5)
  
  result <- plot_freq(x ~ 1)
  
  # Check structure
  expect_true(is.data.frame(result))
  
  # Should have at least value and frequency columns
  expect_true(ncol(result) >= 2)
  expect_true(nrow(result) > 0)
})

test_that("plot_freq handles missing values", {
  x <- c(1, 1, 2, NA, 2, 5, 5)
  
  # Should handle NAs gracefully
  expect_error(plot_freq(x ~ 1), NA)
  
  result <- plot_freq(x ~ 1)
  expect_true(is.data.frame(result))
})

test_that("plot_freq error message shows correct dataset name", {
  # Create a dataset with a specific name
  IV5 <- data.frame(value = c(1, 1, 2, 2, 2, 5, 5))
  
  # Try to access a variable that doesn't exist - should show "IV5" not "data"
  expect_error(
    plot_freq(nonexistent ~ 1, data = IV5),
    'Variable "nonexistent" not found in dataset "IV5"',
    fixed = TRUE
  )
})

