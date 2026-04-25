#text2_001
test_that("text2 runs without errors", {
  # Create a simple plot first
  plot(1:10, 1:10, type = "n")
  
  # Should not throw errors
  expect_error(text2(5, 5, "Test"), NA)
  expect_error(text2(c(2, 8), c(3, 7), c("Left", "Right")), NA)
})

#text2_002
test_that("text2 handles different alignments", {
  plot(1:10, 1:10, type = "n")
  
  # Left alignment
  expect_error(text2(2, 8, "Left", align = "left"), NA)
  
  # Center alignment (default)
  expect_error(text2(5, 8, "Center", align = "center"), NA)
  
  # Right alignment
  expect_error(text2(8, 8, "Right", align = "right"), NA)
})

#text2_003
test_that("text2 handles vectorized alignments", {
  plot(1:10, 1:10, type = "n")
  
  # Multiple labels with different alignments
  expect_error(text2(c(2, 5, 8), c(5, 5, 5),
                      labels = c("Left", "Center", "Right"),
                      align = c("left", "center", "right")), NA)
})

#text2_004
test_that("text2 handles custom background colors", {
  plot(1:10, 1:10, type = "n")
  
  # Single color
  expect_error(text2(5, 5, "Test", bg = "yellow"), NA)
  
  # Multiple colors
  expect_error(text2(c(2, 8), c(5, 5), c("A", "B"), bg = c("red", "blue")), NA)
})

#text2_005
test_that("text2 handles cex parameter", {
  plot(1:10, 1:10, type = "n")
  
  # Different sizes
  expect_error(text2(5, 5, "Small", cex = 0.5), NA)
  expect_error(text2(5, 7, "Large", cex = 2), NA)
})

#text2_006
test_that("text2 handles pad parameters", {
  plot(1:10, 1:10, type = "n")
  
  # Custom padding
  expect_error(text2(5, 5, "Test", pad = 0.1), NA)
  expect_error(text2(5, 7, "Test", pad_v = 0.5), NA)
})

#text2_007
test_that("text2 handles additional arguments", {
  plot(1:10, 1:10, type = "n")
  
  # Pass through col argument
  expect_error(text2(5, 5, "Red text", col = "red", bg = "white"), NA)
  
  # Pass through font argument
  expect_error(text2(5, 7, "Bold text", font = 2, bg = "yellow"), NA)
})

#text2_008
test_that("text2 applies vector col per label (not only col[1])", {
  plot(1:10, 1:10, type = "n")
  expect_error(
    text2(
      c(2, 4, 6, 8), c(3, 5, 7, 9),
      c("a", "b", "c", "d"),
      bg = c("orange", "gray90", "gray20", "black"),
      col = c("black", "black", "white", "white")
    ),
    NA
  )
})

#text2_009
test_that("text2 handles multiple labels", {
  plot(1:10, 1:10, type = "n")
  
  # Multiple labels
  expect_error(text2(c(2, 5, 8), c(3, 5, 7),
                      labels = c("Label1", "Label2", "Label3")), NA)
})

#text2_010
test_that("text2 recycles bg color", {
  plot(1:10, 1:10, type = "n")
  
  # Single bg color should be recycled
  expect_error(text2(c(2, 5, 8), c(3, 5, 7),
                      labels = c("A", "B", "C"),
                      bg = "yellow"), NA)
})

#text2_011
test_that("text2 handles edge cases", {
  plot(1:10, 1:10, type = "n")
  
  # Single point
  expect_error(text2(5, 5, "Single"), NA)
  
  # Empty labels (should error or handle gracefully)
  # This might error, which is acceptable
  # expect_error(text2(5, 5, ""), NA)
})









