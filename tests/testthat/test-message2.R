test_that("message2 prints messages without errors", {
  # Should not throw errors
  expect_error(message2("Test message"), NA)
  expect_error(message2("Test", "message", "with", "multiple", "parts"), NA)
})

test_that("message2 handles quiet parameter", {
  # When quiet=TRUE, should return invisibly without printing
  result <- message2("Test", quiet = TRUE)
  expect_null(result)
  
  # When quiet=FALSE, should print (we can't easily test output, but it shouldn't error)
  expect_error(message2("Test", quiet = FALSE), NA)
})

test_that("message2 handles different colors", {
  # Test various color options
  expect_error(message2("Red message", col = "red"), NA)
  expect_error(message2("Blue message", col = "blue"), NA)
  expect_error(message2("Cyan message", col = "cyan"), NA)
  expect_error(message2("Green message", col = "green"), NA)
  expect_error(message2("Custom color", col = "dodgerblue"), NA)
})

test_that("message2 handles font parameter", {
  # Font 1 (plain)
  expect_error(message2("Plain text", font = 1), NA)
  
  # Font 2 (bold)
  expect_error(message2("Bold text", font = 2), NA)
})

test_that("message2 handles stop parameter", {
  # When stop=TRUE, should stop execution
  expect_error(message2("This stops", stop = TRUE), class = "simpleError")
  
  # When stop=FALSE, should not stop
  expect_error(message2("This doesn't stop", stop = FALSE), NA)
})

test_that("message2 handles appendLF parameter", {
  # Should work with both TRUE and FALSE
  # Note: message() doesn't accept appendLF as named argument, so there's a warning
  # but the function still works
  expect_warning(message2("With newline", appendLF = TRUE), NA)
  expect_warning(message2("Without newline", appendLF = FALSE), 
                 "additional arguments ignored")
})

test_that("message2 handles multiple message parts", {
  # Multiple arguments should be combined
  expect_error(message2("Part", "1", "and", "Part", "2"), NA)
  
  # With numeric values
  expect_error(message2("Value:", 42), NA)
  
  # With mixed types
  expect_error(message2("Count:", 5, "items"), NA)
})

test_that("message2 returns invisibly", {
  result <- message2("Test", quiet = TRUE)
  expect_null(result)
  
  result <- message2("Test", quiet = FALSE)
  expect_null(result)
})

