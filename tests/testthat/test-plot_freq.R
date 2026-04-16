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
  
  # Character options
  expect_error(plot_freq(x ~ 1, value.labels = "all"), NA)
  expect_error(plot_freq(x ~ 1, value.labels = "auto"), NA)
  
  # Top-N labels
  expect_error(plot_freq(x ~ 1, value.labels = 5), NA)
  
  # None / all
  expect_error(plot_freq(x ~ 1, value.labels = 0), NA)
  expect_error(plot_freq(x ~ 1, value.labels = -1), NA)
  
  # Grouped path also accepts numeric / logical
  df <- data.frame(value = x, group = c("A", "A", "B", "B", "A"))
  expect_error(plot_freq(value ~ group, data = df, value.labels = 2), NA)
  expect_error(plot_freq(value ~ group, data = df, value.labels = TRUE), NA)
  expect_error(plot_freq(value ~ group, data = df, value.labels = FALSE), NA)
  expect_error(plot_freq(value ~ group, data = df, value.labels = "all"), NA)
  expect_error(plot_freq(value ~ group, data = df, value.labels = "auto"), NA)
  
  # In grouped plots, numeric value.labels applies per group (still should not error)
  df2 <- data.frame(value = c(1, 1, 1, 2, 2, 3, 3, 4),
                    group = c("A", "A", "A", "A", "B", "B", "B", "B"))
  expect_error(plot_freq(value ~ group, data = df2, value.labels = 1), NA)
  
  # Labeling does not affect returned data
  r1 <- plot_freq(value ~ group, data = df2, value.labels = 1)
  r0 <- plot_freq(value ~ group, data = df2, value.labels = 0)
  expect_equal(r1, r0)
  
  # Invalid inputs
  expect_error(plot_freq(x ~ 1, value.labels = -2))
  expect_error(plot_freq(x ~ 1, value.labels = NA))
  expect_error(plot_freq(x ~ 1, value.labels = c(1, 2)))
  expect_error(plot_freq(x ~ 1, value.labels = "nope"))
})

test_that("plot_freq handles show.x.value parameter", {
  x <- c(1, 1, 2, 2, 2, 3, 3)
  df <- data.frame(value = x, group = rep(c("A", "B"), length.out = length(x)))
  
  # Non-grouped: x= only where frequency labels are drawn
  expect_error(plot_freq(x ~ 1, value.labels = 2, show.x.value = "auto"), NA)
  expect_error(plot_freq(x ~ 1, value.labels = 0, show.x.value = "auto"), NA)
  
  # Grouped
  expect_error(plot_freq(value ~ group, data = df, value.labels = 2, show.x.value = "auto"), NA)
  
  # Explicit TRUE/FALSE still accepted
  expect_error(plot_freq(x ~ 1, show.x.value = TRUE), NA)
  expect_error(plot_freq(x ~ 1, show.x.value = FALSE), NA)
})

test_that("plot_freq handles ticks.max parameter", {
  x <- 1:50
  expect_error(plot_freq(x ~ 1, ticks.max = 5), NA)
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

test_that("plot_freq handles order parameter for groups", {
  df <- data.frame(
    value = c(1, 1, 2, 2, 2, 5, 5, 1, 5),
    group = c("A", "A", "A", "B", "B", "A", "B", "B", "A")
  )
  
  # Custom group order
  expect_error(plot_freq(value ~ group, data = df, order = c("B", "A")), NA)
  result <- plot_freq(value ~ group, data = df, order = c("B", "A"))
  expect_true(is.data.frame(result))
  
  # Check that columns are in specified order
  expect_equal(names(result), c("value", "B", "A"))
})

test_that("plot_freq respects factor levels for groups when order is NULL", {
  # Create factor with specific level order
  df <- data.frame(
    value = c(1, 1, 2, 2, 2, 5, 5, 1, 5),
    group = factor(c("A", "A", "A", "B", "B", "A", "B", "B", "A"),
                   levels = c("B", "A"))
  )
  
  expect_error(plot_freq(value ~ group, data = df), NA)
  result <- plot_freq(value ~ group, data = df)
  expect_true(is.data.frame(result))
  
  # Check that factor levels are respected (B before A)
  expect_equal(names(result), c("value", "B", "A"))
})

test_that("plot_freq order parameter validates group names", {
  df <- data.frame(
    value = c(1, 1, 2, 2, 2, 5, 5),
    group = c("A", "A", "A", "B", "B", "A", "B")
  )
  
  # Missing a group in order
  expect_error(
    plot_freq(value ~ group, data = df, order = c("A")),
    "order.*missing group"
  )
  
  # Extra group in order (should warn, not error)
  expect_warning(
    plot_freq(value ~ group, data = df, order = c("A", "B", "C")),
    "order.*contains group.*not in data"
  )
})

test_that("plot_freq order works with 3 groups", {
  df <- data.frame(
    value = rep(c(1, 2, 5), 6),
    group = rep(c("A", "B", "C"), each = 6)
  )
  
  # Custom order for 3 groups
  expect_error(plot_freq(value ~ group, data = df, order = c("C", "A", "B")), NA)
  result <- plot_freq(value ~ group, data = df, order = c("C", "A", "B"))
  expect_true(is.data.frame(result))
  
  # Check column order
  expect_equal(names(result), c("value", "C", "A", "B"))
})

test_that("plot_freq default group order is sorted", {
  df <- data.frame(
    value = c(1, 1, 2, 2, 2, 5, 5, 1, 5),
    group = c("C", "C", "C", "A", "A", "C", "A", "A", "C")
  )
  
  # Without order parameter, groups should be sorted (A before C)
  result <- plot_freq(value ~ group, data = df)
  expect_true(is.data.frame(result))
  expect_equal(names(result), c("value", "A", "C"))
})

test_that("plot_freq order = -1 reverses default order", {
  df <- data.frame(
    value = c(1, 1, 2, 2, 2, 5, 5, 1, 5),
    group = c("A", "A", "A", "B", "B", "A", "B", "B", "A")
  )
  
  # Default order is A, B (sorted)
  result_default <- plot_freq(value ~ group, data = df)
  expect_equal(names(result_default), c("value", "A", "B"))
  
  # order = -1 should reverse to B, A
  result_reversed <- plot_freq(value ~ group, data = df, order = -1)
  expect_true(is.data.frame(result_reversed))
  expect_equal(names(result_reversed), c("value", "B", "A"))
})

test_that("plot_freq order = -1 reverses factor levels", {
  df <- data.frame(
    value = c(1, 1, 2, 2, 2, 5, 5, 1, 5),
    group = factor(c("A", "A", "A", "B", "B", "A", "B", "B", "A"),
                   levels = c("A", "B"))
  )
  
  # Default respects factor levels: A, B
  result_default <- plot_freq(value ~ group, data = df)
  expect_equal(names(result_default), c("value", "A", "B"))
  
  # order = -1 should reverse factor levels to B, A
  result_reversed <- plot_freq(value ~ group, data = df, order = -1)
  expect_true(is.data.frame(result_reversed))
  expect_equal(names(result_reversed), c("value", "B", "A"))
})

test_that("plot_freq order = -1 works with 3 groups", {
  df <- data.frame(
    value = rep(c(1, 2, 5), 6),
    group = rep(c("A", "B", "C"), each = 6)
  )
  
  # Default order is A, B, C (sorted)
  result_default <- plot_freq(value ~ group, data = df)
  expect_equal(names(result_default), c("value", "A", "B", "C"))
  
  # order = -1 should reverse to C, B, A
  result_reversed <- plot_freq(value ~ group, data = df, order = -1)
  expect_true(is.data.frame(result_reversed))
  expect_equal(names(result_reversed), c("value", "C", "B", "A"))
})

test_that("plot_freq handles two-vector comparison", {
  y1 <- c(1, 1, 2, 2, 2, 5, 5)
  y2 <- c(1, 2, 2, 3, 3, 3)
  
  # Should not throw errors
  expect_error(plot_freq(y1, y2), NA)
  
  result <- plot_freq(y1, y2)
  expect_true(is.data.frame(result))
  
  # Should have columns for value and both vector names
  expect_equal(ncol(result), 3)
  expect_true("value" %in% names(result))
})

test_that("plot_freq two-vector comparison returns correct structure", {
  y1 <- c(1, 1, 2, 2, 2)
  y2 <- c(2, 3, 3, 3)
  
  result <- plot_freq(y1, y2)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 3)
  expect_true("value" %in% names(result))
  expect_true("y1" %in% names(result))
  expect_true("y2" %in% names(result))
})

test_that("plot_freq two-vector comparison with custom parameters", {
  y1 <- c(1, 1, 2, 2, 2)
  y2 <- c(2, 3, 3, 3)
  
  # Custom colors
  expect_error(plot_freq(y1, y2, col = c("red", "blue")), NA)
  
  # Percentages
  expect_error(plot_freq(y1, y2, freq = FALSE), NA)
  
  # Without legend
  expect_error(plot_freq(y1, y2, show.legend = FALSE), NA)
})

test_that("plot_freq two-vector comparison handles order parameter", {
  y1 <- c(1, 1, 2, 2, 2)
  y2 <- c(2, 3, 3, 3)
  
  # Default order (alphabetical)
  result_default <- plot_freq(y1, y2)
  expect_equal(names(result_default), c("value", "y1", "y2"))
  
  # Reverse order
  result_reversed <- plot_freq(y1, y2, order = -1)
  expect_equal(names(result_reversed), c("value", "y2", "y1"))
  
  # Custom order
  result_custom <- plot_freq(y1, y2, order = c("y2", "y1"))
  expect_equal(names(result_custom), c("value", "y2", "y1"))
})

test_that("plot_freq two-vector comparison handles missing values", {
  y1 <- c(1, 1, 2, NA, 2)
  y2 <- c(2, NA, 3, 3)
  
  # Should handle NAs gracefully
  expect_error(plot_freq(y1, y2), NA)
  
  result <- plot_freq(y1, y2)
  expect_true(is.data.frame(result))
})

test_that("plot_freq two-vector comparison validates inputs", {
  y1 <- c(1, 1, 2, 2, 2)
  
  # Non-existent variable name as second argument
  expect_error(
    plot_freq(y1, "not_a_vector"),
    "Could not find variable"
  )
  
  # Non-existent variable name as first argument
  expect_error(
    plot_freq("not_a_vector", y1),
    "Could not find variable"
  )
})

test_that("plot_freq two-vector uses deduced variable names", {
  y1 <- c(1, 1, 2, 2, 2)
  y2 <- c(2, 3, 3, 3)
  
  result <- plot_freq(y1, y2)
  expect_true(is.data.frame(result))
  expect_true("y1" %in% names(result))
  expect_true("y2" %in% names(result))
})

test_that("plot_freq two-vector can resolve columns from data", {
  set.seed(1)
  y1 <- sample(100, replace = TRUE)
  y2 <- sample(100, replace = TRUE)
  df1 <- data.frame(a = y1, b = y2)
  
  expect_error(plot_freq(a, b, data = df1), NA)
})

test_that("plot_freq two-vector order parameter works", {
  y1 <- c(1, 1, 2, 2, 2)
  y2 <- c(2, 3, 3, 3)
  
  # Reversed order
  result <- plot_freq(y1, y2, order = -1)
  expect_equal(names(result), c("value", "y2", "y1"))
  
  # Explicit order
  result2 <- plot_freq(y1, y2, order = c("y2", "y1"))
  expect_equal(names(result2), c("value", "y2", "y1"))
})

test_that("plot_freq two-vector handles continuous data", {
  # Test with continuous data (runif, rnorm) which have floating point values
  set.seed(123)
  y1 <- runif(50)
  y2 <- rnorm(50)
  
  # Should not throw errors with continuous data
  expect_error(plot_freq(y1, y2), NA)
  
  result <- plot_freq(y1, y2)
  expect_true(is.data.frame(result))
})

test_that("plot_freq xlim includes all bars", {
  # Test that bars at edges are not clipped
  y1 <- c(1, 1, 2, 2, 3)
  y2 <- c(1, 4, 4, 4)
  
  # Should include padding so bars at min (1) and max (4) are visible
  expect_error(plot_freq(y1, y2), NA)
  
  # Test with single variable at edges
  x <- c(1, 1, 5, 5)
  expect_error(plot_freq(x), NA)
})

test_that("plot_freq uses pretty x-ticks for many unique values", {
  # With >30 unique values, plot_freq should still run (and not attempt to label every x)
    x <- 1:40
    expect_error(plot_freq(x, ylim = c(0, 10)), NA)
    
  # Grouped path also should run
    df <- data.frame(value = 1:40, group = rep(c("A", "B"), each = 20))
    expect_error(plot_freq(value ~ group, data = df, ylim = c(0, 10)), NA)
})

test_that("plot_freq reserves space for legend", {
  y1 <- c(1, 1, 2, 2, 3)
  y2 <- c(1, 4, 4, 4)
  
  # With legend (default)
  expect_error(plot_freq(y1, y2, show.legend = TRUE), NA)
  
  # Without legend
  expect_error(plot_freq(y1, y2, show.legend = FALSE), NA)
})

test_that("plot_freq two-vector supports col and freq args", {
  y1 <- c(1, 1, 2, 2, 2)
  y2 <- c(2, 3, 3, 3)
  
  # col argument works
  expect_error(plot_freq(y1, y2, col = c("blue", "pink")), NA)
  
  # Percentages work
  result_pct <- plot_freq(y1, y2, freq = FALSE)
  expect_true(is.data.frame(result_pct))
})