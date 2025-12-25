test_that("resize_images handles non-existent folder gracefully", {
  # Non-existent folder should error or handle gracefully
  expect_error(resize_images("nonexistent_folder_12345", width = 800), 
               class = "error")
})

test_that("resize_images handles empty folder", {
  # Create temporary empty folder
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # Should handle empty folder (will error because no images found)
  # This test ensures it errors gracefully
  expect_error(resize_images(temp_dir, width = 800), 
               "No supported image")
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

# Note: Full testing of resize_images requires actual image files
# which is complex in a test environment. These tests cover basic
# error handling and edge cases.

test_that("resize_images accepts width parameter", {
  # Test that function signature accepts width
  # Full functionality requires image files
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # Should accept single width (will error because no images, but that's expected)
  expect_error(resize_images(temp_dir, width = 800), 
               "No supported image")
  
  # Should accept multiple widths (will error because no images, but that's expected)
  expect_error(resize_images(temp_dir, width = c(800, 1200)), 
               "No supported image")
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

# Note: Testing actual image conversion would require:
# 1. Creating test image files
# 2. Verifying output files are created
# 3. Checking image dimensions
# This is complex and may require additional dependencies
# The current tests verify basic parameter handling

