test_that("t.test2 runs without errors for two-sample test", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- t.test2(y, group)
  expect_true(inherits(result, "data.frame"))
  expect_true(nrow(result) == 1)
  expect_true("p.value" %in% names(result))
  expect_true("t" %in% names(result))
  expect_true("df" %in% names(result))
})

test_that("t.test2 handles data frame input", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  # Use formula syntax for data frame input
  result <- t.test2(value ~ group, data = df)
  expect_true(inherits(result, "data.frame"))
  expect_true(nrow(result) == 1)
})

test_that("t.test2 handles formula syntax", {
  df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
  
  result <- t.test2(value ~ group, data = df)
  expect_true(inherits(result, "data.frame"))
  expect_true(nrow(result) == 1)
})

test_that("t.test2 handles two vectors", {
  x1 <- rnorm(50, mean = 5)
  x2 <- rnorm(50, mean = 4.8)
  
  result <- t.test2(x1, x2)
  expect_true(inherits(result, "data.frame"))
  expect_true("x1" %in% names(result))
  expect_true("x2" %in% names(result))
})

test_that("t.test2 returns correct columns", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- t.test2(y, group)
  
  # Check for essential columns
  expect_true("p.value" %in% names(result))
  expect_true("t" %in% names(result))
  expect_true("df" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("conf.intL" %in% names(result))
  expect_true("conf.intH" %in% names(result))
})

test_that("t.test2 handles digits parameter", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  # Should not error with different digits
  expect_error(t.test2(y, group, digits = 2), NA)
  expect_error(t.test2(y, group, digits = 4), NA)
})

test_that("t.test2 handles Welch vs Student test", {
  y1 <- rnorm(50, mean = 5, sd = 1)
  y2 <- rnorm(50, mean = 4.8, sd = 2)  # Different SDs trigger Welch
  
  result <- t.test2(y1, y2)
  expect_true("method" %in% names(result))
  expect_true(result$method %in% c("student", "welch"))
})

test_that("t.test2 handles one-sample test", {
  x <- rnorm(50, mean = 5)
  
  result <- t.test2(x, mu = 5)
  expect_true(inherits(result, "data.frame"))
  expect_true(nrow(result) == 1)
  expect_true("p.value" %in% names(result))
})

