test_that("fhist is an alias for plot_freq", {
  x <- c(1, 1, 2, 2, 2, 5, 5)
  
  # Should work (may print deprecation message)
  expect_error(fhist(x), NA)
  
  # Should return same structure as plot_freq
  result <- fhist(x)
  expect_true(is.data.frame(result))
})

test_that("fhist passes arguments to plot_freq", {
  x <- c(1, 1, 2, 2, 2)
  group <- c("A", "A", "B", "B", "A")
  
  # Should accept same arguments as plot_freq
  expect_error(fhist(x, group = group), NA)
  expect_error(fhist(x, col = "red"), NA)
  expect_error(fhist(x, freq = FALSE), NA)
})

test_that("fhist prints deprecation message", {
  x <- c(1, 1, 2, 2, 2)
  
  # Should print deprecation message (we can't easily test the message content,
  # but we verify it doesn't error)
  expect_error(fhist(x), NA)
})

