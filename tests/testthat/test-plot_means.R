test_that("plot_means runs with formula + data and returns desc_var object", {
  df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))

  result <- plot_means(y ~ group, data = df, save.as = NULL)

  expect_null(result)
})

test_that("plot_means works with multiple grouping variables (y ~ x1 + x2)", {
  df <- data.frame(
    y = rnorm(200),
    x1 = rep(c("A", "B"), 100),
    x2 = rep(c("X", "Y"), each = 100)
  )

  result <- plot_means(y ~ x1 + x2, data = df, save.as = NULL)

  expect_null(result)
})

test_that("plot_means order reorders groups for single grouping variable", {
  df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))

  result_default <- plot_means(y ~ group, data = df, save.as = NULL)
  expect_null(result_default)

  result_reversed <- plot_means(y ~ group, data = df, order = -1, save.as = NULL)
  expect_null(result_reversed)

  result_custom <- plot_means(y ~ group, data = df, order = c("B", "A"), save.as = NULL)
  expect_null(result_custom)
})

test_that("plot_means works with three grouping variables and missing combinations", {
  set.seed(1)
  df <- data.frame(
    y = rnorm(96),
    x1 = rep(c("A", "B"), 48),
    x2 = rep(rep(c("X", "Y"), each = 24), 2),
    x3 = rep(c("M", "N"), each = 48)
  )

  # Remove one x1 level from one (x2,x3) block to create a missing combination
  df <- df[!(df$x1 == "B" & df$x2 == "Y" & df$x3 == "N"), , drop = FALSE]

  result <- plot_means(y ~ x1 + x2 + x3, data = df, save.as = NULL)
  expect_null(result)
})

test_that("plot_means tests=auto works for scenario 1 (binary x1 only)", {
  df <- data.frame(y = rnorm(60), x1 = rep(c("A", "B"), each = 30))
  expect_error(plot_means(y ~ x1, data = df, tests = "auto", save.as = NULL), NA)
})

test_that("plot_means tests=auto works for scenario 2 (binary x1 and x2)", {
  df <- data.frame(
    y = rnorm(120),
    x1 = rep(c("A", "B"), 60),
    x2 = rep(rep(c("X", "Y"), each = 30), 2)
  )
  expect_error(plot_means(y ~ x1 + x2, data = df, tests = "auto", save.as = NULL), NA)
})

test_that("plot_means tests=auto works for scenario 3 (binary x1, x2 has >2 levels)", {
  df <- expand.grid(
    x1 = c("A", "B"),
    x2 = c("X", "Y", "Z"),
    rep = seq_len(30),
    stringsAsFactors = FALSE
  )
  df$y <- rnorm(nrow(df))
  
  expect_error(plot_means(y ~ x1 + x2, data = df, tests = "auto", save.as = NULL), NA)
})

test_that("plot_means save.as saves png", {
  df <- data.frame(y = rnorm(80), x1 = rep(c("A", "B"), each = 40))
  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)
  
  grDevices::png(tempfile(fileext = ".png"), width = 800, height = 500, res = 100)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  result <- plot_means(y ~ x1, data = df, save.as = out)
  expect_null(result)
  
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})

