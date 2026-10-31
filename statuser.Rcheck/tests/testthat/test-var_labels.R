#var_labels_001
test_that("var_labels gets/sets label for vectors", {
  x <- 1:3
  expect_true(is.na(var_labels(x)))
  
  var_labels(x) <- "this is x"
  expect_equal(var_labels(x), "this is x")
  
  var_labels(x) <- NA_character_
  expect_true(is.na(var_labels(x)))
})

#var_labels_002
test_that("var_labels gets/sets labels for data frames (positional)", {
  df <- data.frame(x = 1:3, y = 4:6)
  
  expect_equal(var_labels(df), c(x = NA_character_, y = NA_character_))
  
  var_labels(df) <- c("this is x", "this is y")
  expect_equal(var_labels(df), c(x = "this is x", y = "this is y"))
})

#var_labels_003
test_that("var_labels supports named assignment for data frames", {
  df <- data.frame(x = 1:3, y = 4:6)
  
  var_labels(df) <- c(x = "this is x")
  expect_equal(var_labels(df), c(x = "this is x", y = NA_character_))
})

#var_labels_004
test_that("var_labels recycles length-1 assignment for data frames", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  
  var_labels(df) <- "same"
  expect_equal(var_labels(df), c(x = "same", y = "same", z = "same"))
})

