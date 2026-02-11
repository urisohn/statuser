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
  
  # Non-numeric second argument
  expect_error(
    plot_freq(y1, "not a vector"),
    "must be a numeric vector"
  )
  
  # Non-numeric first argument
  expect_error(
    plot_freq("not a vector", y1),
    "must be a numeric vector"
  )
})

test_that("plot_freq two-vector with custom labels", {
  y1 <- c(1, 1, 2, 2, 2)
  y2 <- c(2, 3, 3, 3)
  
  # Custom labels
  result <- plot_freq(y1, y2, labels = c("men", "women"))
  expect_true(is.data.frame(result))
  expect_true("men" %in% names(result))
  expect_true("women" %in% names(result))
})

test_that("plot_freq validates labels parameter", {
  y1 <- c(1, 1, 2, 2, 2)
  y2 <- c(2, 3, 3, 3)
  
  # Wrong length
  expect_error(
    plot_freq(y1, y2, labels = c("men")),
    "labels.*must be.*length 2"
  )
  
  expect_error(
    plot_freq(y1, y2, labels = c("men", "women", "other")),
    "labels.*must be.*length 2"
  )
  
  # Wrong type
  expect_error(
    plot_freq(y1, y2, labels = c(1, 2)),
    "labels.*must be.*character"
  )
})

test_that("plot_freq custom labels work with order parameter", {
  y1 <- c(1, 1, 2, 2, 2)
  y2 <- c(2, 3, 3, 3)
  
  # Custom labels with reversed order
  result <- plot_freq(y1, y2, labels = c("men", "women"), order = -1)
  expect_equal(names(result), c("value", "women", "men"))
  
  # Custom labels with explicit order
  result2 <- plot_freq(y1, y2, labels = c("men", "women"), order = c("women", "men"))
  expect_equal(names(result2), c("value", "women", "men"))
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

test_that("plot_freq reserves space for legend", {
  y1 <- c(1, 1, 2, 2, 3)
  y2 <- c(1, 4, 4, 4)
  
  # With legend (default)
  expect_error(plot_freq(y1, y2, show.legend = TRUE), NA)
  
  # Without legend
  expect_error(plot_freq(y1, y2, show.legend = FALSE), NA)
})

test_that("plot_freq labels parameter works correctly", {
  y1 <- c(1, 1, 2, 2, 2)
  y2 <- c(2, 3, 3, 3)
  
  # Test with custom labels
  result <- plot_freq(y1, y2, labels = c("before", "after"))
  expect_true("before" %in% names(result))
  expect_true("after" %in% names(result))
  
  # Test labels with colors
  expect_error(plot_freq(y1, y2, labels = c("men", "women"), 
                         col = c("blue", "pink")), NA)
  
  # Test labels with percentages
  result_pct <- plot_freq(y1, y2, labels = c("group A", "group B"), freq = FALSE)
  expect_true("group A" %in% names(result_pct))
  expect_true("group B" %in% names(result_pct))
})

test_that("plot_freq labels validation works", {
  y1 <- c(1, 1, 2)
  y2 <- c(2, 3, 3)
  
  # Labels must be character
  expect_error(
    plot_freq(y1, y2, labels = c(1, 2)),
    "character vector"
  )
  
  # Labels must be length 2
  expect_error(
    plot_freq(y1, y2, labels = c("one")),
    "length 2"
  )
  
  expect_error(
    plot_freq(y1, y2, labels = c("one", "two", "three")),
    "length 2"
  )
})

test_that("plot_freq labels work with order parameter", {
  y1 <- c(1, 1, 2)
  y2 <- c(2, 3, 3)
  
  # Custom labels with custom order
  result <- plot_freq(y1, y2, labels = c("first", "second"), 
                      order = c("second", "first"))
  expect_equal(names(result), c("value", "second", "first"))
  
  # Custom labels with reversed order
  result2 <- plot_freq(y1, y2, labels = c("A", "B"), order = -1)
  expect_equal(names(result2), c("value", "B", "A"))
})
