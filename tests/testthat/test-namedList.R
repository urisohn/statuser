test_that("namedList creates list with object names", {
  x <- 1:5
  y <- letters[1:3]
  z <- matrix(1:4, nrow = 2)
  
  result <- namedList(x, y, z)
  
  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_equal(names(result), c("x", "y", "z"))
  expect_equal(result$x, x)
  expect_equal(result$y, y)
  expect_equal(result$z, z)
})

test_that("namedList works with explicit names", {
  x <- 1:5
  y <- letters[1:3]
  
  result <- namedList(a = x, b = y)
  
  expect_equal(names(result), c("a", "b"))
  expect_equal(result$a, x)
  expect_equal(result$b, y)
})

test_that("namedList works with mixed named and unnamed", {
  x <- 1:5
  y <- letters[1:3]
  z <- matrix(1:4, nrow = 2)
  
  result <- namedList(a = x, y, z)
  
  expect_equal(names(result), c("a", "y", "z"))
  expect_equal(result$a, x)
  expect_equal(result$y, y)
  expect_equal(result$z, z)
})

test_that("namedList handles single object", {
  x <- 1:5
  
  result <- namedList(x)
  
  expect_equal(length(result), 1)
  expect_equal(names(result), "x")
  expect_equal(result$x, x)
})

test_that("namedList handles empty call", {
  result <- namedList()
  
  expect_true(is.list(result))
  expect_equal(length(result), 0)
})

test_that("namedList preserves object values", {
  x <- c(1, 2, 3)
  y <- list(a = 1, b = 2)
  z <- data.frame(col1 = 1:3, col2 = 4:6)
  
  result <- namedList(x, y, z)
  
  expect_equal(result$x, x)
  expect_equal(result$y, y)
  expect_equal(result$z, z)
})

test_that("namedList works with complex expressions", {
  x <- 1:5
  
  # Should work even with expressions
  result <- namedList(x, x + 1)
  
  expect_true(is.list(result))
  expect_equal(length(result), 2)
  # Second element should be x + 1
  expect_equal(result[[2]], x + 1)
})

