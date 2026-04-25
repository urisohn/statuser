#list2_001
test_that("list2 creates list with object names", {
  x <- 1:5
  y <- letters[1:3]
  z <- matrix(1:4, nrow = 2)
  
  result <- list2(x, y, z)
  
  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_equal(names(result), c("x", "y", "z"))
  expect_equal(result$x, x)
  expect_equal(result$y, y)
  expect_equal(result$z, z)
})

#list2_002
test_that("list2 works with explicit names", {
  x <- 1:5
  y <- letters[1:3]
  
  result <- list2(a = x, b = y)
  
  expect_equal(names(result), c("a", "b"))
  expect_equal(result$a, x)
  expect_equal(result$b, y)
})

#list2_003
test_that("list2 works with mixed named and unnamed", {
  x <- 1:5
  y <- letters[1:3]
  z <- matrix(1:4, nrow = 2)
  
  result <- list2(a = x, y, z)
  
  expect_equal(names(result), c("a", "y", "z"))
  expect_equal(result$a, x)
  expect_equal(result$y, y)
  expect_equal(result$z, z)
})

#list2_004
test_that("list2 handles single object", {
  x <- 1:5
  
  result <- list2(x)
  
  expect_equal(length(result), 1)
  expect_equal(names(result), "x")
  expect_equal(result$x, x)
})

#list2_005
test_that("list2 handles empty call", {
  result <- list2()
  
  expect_true(is.list(result))
  expect_equal(length(result), 0)
})

#list2_006
test_that("list2 preserves object values", {
  x <- c(1, 2, 3)
  y <- list(a = 1, b = 2)
  z <- data.frame(col1 = 1:3, col2 = 4:6)
  
  result <- list2(x, y, z)
  
  expect_equal(result$x, x)
  expect_equal(result$y, y)
  expect_equal(result$z, z)
})

#list2_007
test_that("list2 works with complex expressions", {
  x <- 1:5
  
  # Should work even with expressions
  result <- list2(x, x + 1)
  
  expect_true(is.list(result))
  expect_equal(length(result), 2)
  # Second element should be x + 1
  expect_equal(result[[2]], x + 1)
})



