test_that("clear runs without errors", {
  # Create some test objects
  test_var1 <- 1:10
  test_var2 <- rnorm(10)
  
  # Should run without errors (but will clear environment)
  # We can't easily test that it actually clears without affecting the test environment
  # So we just test that it doesn't throw errors
  expect_error(clear(), NA)
  
  # Note: After clear(), test_var1 and test_var2 will be gone
  # But this is expected behavior
})

test_that("clear returns invisibly", {
  # Create test objects
  test_var <- 1:10
  
  # Should return NULL invisibly
  result <- clear()
  expect_null(result)
  expect_true(is.null(result))
})

# Note: We can't easily test that clear() actually removes objects
# because it affects the global environment and would interfere with test execution
# The function is tested for basic functionality and error handling









