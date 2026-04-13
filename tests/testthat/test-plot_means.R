test_that("plot_means runs with formula + data and returns desc_var object", {
  df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))

  result <- plot_means(y ~ group, data = df)

  expect_true(is.data.frame(result))
  expect_true(inherits(result, "desc_var"))
  expect_true("mean" %in% names(result))
  expect_true("group" %in% names(result))
})

test_that("plot_means works with multiple grouping variables (y ~ x1 + x2)", {
  df <- data.frame(
    y = rnorm(200),
    x1 = rep(c("A", "B"), 100),
    x2 = rep(c("X", "Y"), each = 100)
  )

  result <- plot_means(y ~ x1 + x2, data = df)

  expect_true(is.data.frame(result))
  expect_true(inherits(result, "desc_var"))
  expect_true("mean" %in% names(result))
  expect_true("x1" %in% names(result))
  expect_true("x2" %in% names(result))
  expect_false("group" %in% names(result))
})

test_that("plot_means order reorders groups for single grouping variable", {
  df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))

  result_default <- plot_means(y ~ group, data = df)
  expect_equal(as.character(result_default$group), c("A", "B"))

  result_reversed <- plot_means(y ~ group, data = df, order = -1)
  expect_equal(as.character(result_reversed$group), c("B", "A"))

  result_custom <- plot_means(y ~ group, data = df, order = c("B", "A"))
  expect_equal(as.character(result_custom$group), c("B", "A"))
})

