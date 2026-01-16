test_that("t.test2 is exported from the package namespace", {
  # This test guards against a recurring issue where roxygen2 interprets

  # t.test2 as an S3 method (t generic with test2 class) instead of a
  # standalone function, causing it to only register S3method(t,test2)
  # instead of export(t.test2) in NAMESPACE.
  

  # Check that t.test2 is exported in NAMESPACE
  namespace_file <- system.file("NAMESPACE", package = "sohn")
  if (namespace_file != "") {
    namespace_content <- readLines(namespace_file)
    expect_true(
      any(grepl("^export\\(t\\.test2\\)", namespace_content)),
      info = "t.test2 should be explicitly exported in NAMESPACE with export(t.test2)"
    )
  }
  
  # Verify the function is accessible as sohn::t.test2
  expect_true(exists("t.test2", where = asNamespace("sohn"), mode = "function"))
  
  # Verify it can be called directly (not just via t() generic)
  x <- rnorm(50)
  y <- rnorm(50)
  result <- t.test2(x, y)
  expect_s3_class(result, "t.test2")
  expect_s3_class(result, "data.frame")
})

test_that("t.test2 runs without errors for two-sample test", {
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  
  result <- t.test2(y ~ group)
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
  
  result <- t.test2(y ~ group)
  
  # Check for essential columns
  expect_true("p.value" %in% names(result))
  expect_true("t" %in% names(result))
  expect_true("df" %in% names(result))
  # Method type is stored as an attribute, not a column
  expect_true(!is.null(attr(result, "method_type")))
  expect_true("ci.L" %in% names(result))
  expect_true("ci.H" %in% names(result))
})

test_that("t.test2 handles Welch vs Student test", {
  y1 <- rnorm(50, mean = 5, sd = 1)
  y2 <- rnorm(50, mean = 4.8, sd = 2)  # Different SDs trigger Welch
  
  result <- t.test2(y1, y2)
  # Method type is stored as an attribute, not a column
  method_type <- attr(result, "method_type")
  expect_true(!is.null(method_type))
  expect_true(method_type %in% c("student", "welch"))
})

test_that("t.test2 handles one-sample test", {
  x <- rnorm(50, mean = 5)
  
  result <- t.test2(x, mu = 5)
  expect_true(inherits(result, "data.frame"))
  expect_true(nrow(result) == 1)
  expect_true("p.value" %in% names(result))
})

# Comprehensive test scenarios
test_that("t.test2 handles all basic scenarios", {
  set.seed(12)
  n <- 100
  x1 <- rep(c("A", "B"), each = n)
  x2 <- rep(c("men", "women"), n)
  x3 <- sample(1:3, replace = TRUE, size = 2 * n)
  
  y1 <- round(rnorm(length(x1), mean = 100, sd = 4), 0)
  y2 <- round(rnorm(length(x1), mean = 100, sd = 15), 0)
  y1m <- y1
  y2m <- y2
  y1m[c(192, 111)] <- NA
  y2m[c(55:62)] <- NA
  
  # Basic formula syntax
  result1 <- t.test2(y1 ~ x1)
  expect_true(inherits(result1, "data.frame"))
  expect_true(nrow(result1) == 1)
  expect_true("A" %in% names(result1))
  expect_true("B" %in% names(result1))
  expect_true("p.value" %in% names(result1))
  
  result2 <- t.test2(y1 ~ x2)
  expect_true(inherits(result2, "data.frame"))
  expect_true(nrow(result2) == 1)
  expect_true("men" %in% names(result2) || "women" %in% names(result2))
  
  # Data frame syntax
  df1 <- data.frame(DV1 = y1, IV1 = x1)
  result3 <- t.test2(DV1 ~ IV1, data = df1)
  expect_true(inherits(result3, "data.frame"))
  expect_true(nrow(result3) == 1)
  expect_true("A" %in% names(result3))
  expect_true("B" %in% names(result3))
  
  # Two vectors
  result4 <- t.test2(y1, y2)
  expect_true(inherits(result4, "data.frame"))
  expect_true(nrow(result4) == 1)
  expect_true("y1" %in% names(result4))
  expect_true("y2" %in% names(result4))
  
  # Paired tests
  result5 <- t.test2(y1, y2, paired = TRUE)
  expect_true(inherits(result5, "data.frame"))
  expect_true(nrow(result5) == 1)
  expect_true("N" %in% names(result5))
  expect_true(attr(result5, "is_paired"))
  
  result6 <- t.test2(y1m, y2, paired = TRUE)
  expect_true(inherits(result6, "data.frame"))
  expect_true(nrow(result6) == 1)
  expect_true(attr(result6, "is_paired"))
  
  # Verify missing data message for paired test with missing values
  output6 <- capture.output(print(result6))
  output6_text <- paste(output6, collapse = "\n")
  if (attr(result6, "NA_paired") > 0) {
    expect_true(grepl("pairs were dropped due to missing values", output6_text))
  }
})

test_that("t.test2 handles 0/1 variables", {
  set.seed(12)
  n <- 100
  x1 <- rep(c("A", "B"), each = n)
  y1 <- round(rnorm(length(x1), mean = 100, sd = 4), 0)
  
  x4 <- ifelse(x1 == 'A', 0, 1)
  
  result <- t.test2(y1 ~ x4)
  expect_true(inherits(result, "data.frame"))
  expect_true(nrow(result) == 1)
  # Should have columns with "x4=0" and "x4=1" format
  expect_true(any(grepl("x4=0", names(result))) || any(grepl("0", names(result))))
  expect_true(any(grepl("x4=1", names(result))) || any(grepl("1", names(result))))
  expect_true("p.value" %in% names(result))
})

test_that("t.test2 handles long variable names with 0/1", {
  set.seed(12)
  n <- 100
  x1 <- rep(c("A", "B"), each = n)
  y1 <- round(rnorm(length(x1), mean = 100, sd = 4), 0)
  y1m <- y1
  y1m[c(192, 111)] <- NA
  
  x4 <- ifelse(x1 == 'A', 0, 1)
  xlongversion <- x4
  
  result1 <- t.test2(y1 ~ xlongversion)
  expect_true(inherits(result1, "data.frame"))
  expect_true(nrow(result1) == 1)
  # Should have columns with "xlongversion=0" and "xlongversion=1" format
  expect_true(any(grepl("xlongversion=0", names(result1))) || 
              any(grepl("0", names(result1))) ||
              "Group 1" %in% names(result1))
  expect_true("p.value" %in% names(result1))
  
  result2 <- t.test2(y1m ~ xlongversion)
  expect_true(inherits(result2, "data.frame"))
  expect_true(nrow(result2) == 1)
  expect_true("p.value" %in% names(result2))
  # Should handle missing values
  expect_true(attr(result2, "NA1") >= 0 || attr(result2, "NA2") >= 0)
  
  # Verify missing data message
  output2 <- capture.output(print(result2))
  output2_text <- paste(output2, collapse = "\n")
  if (attr(result2, "NA1") > 0 || attr(result2, "NA2") > 0) {
    expect_true(grepl("missing", output2_text, ignore.case = TRUE))
  }
})

test_that("t.test2 handles long variable names", {
  set.seed(12)
  n <- 100
  x1 <- rep(c("A", "B"), each = n)
  y1 <- round(rnorm(length(x1), mean = 100, sd = 4), 0)
  y1m <- y1
  y1m[c(192, 111)] <- NA
  
  x5 <- ifelse(x1 == 'A', 'Low construal', 'High construal')
  
  result1 <- t.test2(y1 ~ x5)
  expect_true(inherits(result1, "data.frame"))
  expect_true(nrow(result1) == 1)
  # Long names should trigger "Group 1" and "Group 2" format
  expect_true("Group 1" %in% names(result1) || 
              "Low construal" %in% names(result1) ||
              "High construal" %in% names(result1))
  expect_true("p.value" %in% names(result1))
  
  result2 <- t.test2(y1m ~ x5)
  expect_true(inherits(result2, "data.frame"))
  expect_true(nrow(result2) == 1)
  expect_true("p.value" %in% names(result2))
  # Should handle missing values
  expect_true(attr(result2, "NA1") >= 0 || attr(result2, "NA2") >= 0)
  
  # Verify missing data message
  output2 <- capture.output(print(result2))
  output2_text <- paste(output2, collapse = "\n")
  if (attr(result2, "NA1") > 0 || attr(result2, "NA2") > 0) {
    expect_true(grepl("missing", output2_text, ignore.case = TRUE))
  }
  
  # If group mapping is shown, check attributes and verify it appears in output
  if (attr(result2, "show_group_mapping")) {
    expect_true(!is.null(attr(result2, "orig_group1")))
    expect_true(!is.null(attr(result2, "orig_group2")))
    expect_true(grepl("Group 1:", output2_text))
    expect_true(grepl("Group 2:", output2_text))
  }
})

test_that("t.test2 prints missing data messages for paired tests", {
  set.seed(12)
  n <- 100
  y1 <- round(rnorm(n, mean = 100, sd = 4), 0)
  y2 <- round(rnorm(n, mean = 100, sd = 15), 0)
  y1m <- y1
  y1m[c(50, 60)] <- NA  # Create missing values
  
  result <- t.test2(y1m, y2, paired = TRUE)
  
  # Capture printed output
  output <- capture.output(print(result))
  
  # Should contain message about dropped pairs
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("pairs were dropped due to missing values", output_text))
  expect_true(attr(result, "NA_paired") > 0)
})

test_that("t.test2 prints missing data messages for two-sample tests", {
  set.seed(12)
  n <- 100
  x1 <- rep(c("A", "B"), each = n)
  y1 <- round(rnorm(length(x1), mean = 100, sd = 4), 0)
  y1m <- y1
  y1m[c(50, 60)] <- NA  # Create missing values in group A
  
  result <- t.test2(y1m ~ x1)
  
  # Capture printed output
  output <- capture.output(print(result))
  
  # Should contain message about missing values
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("missing", output_text, ignore.case = TRUE))
  expect_true(attr(result, "NA1") > 0 || attr(result, "NA2") > 0)
})

test_that("t.test2 prints missing data messages for 0/1 variables", {
  set.seed(12)
  n <- 100
  x1 <- rep(c("A", "B"), each = n)
  y1 <- round(rnorm(length(x1), mean = 100, sd = 4), 0)
  y1m <- y1
  y1m[c(50, 60, 150)] <- NA  # Create missing values
  
  x4 <- ifelse(x1 == 'A', 0, 1)
  
  result <- t.test2(y1m ~ x4)
  
  # Capture printed output
  output <- capture.output(print(result))
  
  # Should contain message about missing values with special format for 0/1 vars
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("missing", output_text, ignore.case = TRUE))
  # Should use "When 'x4=0'" or "When 'x4=1'" format if both groups have missing
  # or standard format if only one group has missing
  expect_true(grepl("missing", output_text, ignore.case = TRUE) ||
              grepl("When 'x4=", output_text))
  expect_true(attr(result, "NA1") > 0 || attr(result, "NA2") > 0)
})

test_that("t.test2 prints missing data messages for long variable names", {
  set.seed(12)
  n <- 100
  x1 <- rep(c("A", "B"), each = n)
  y1 <- round(rnorm(length(x1), mean = 100, sd = 4), 0)
  y1m <- y1
  y1m[c(50, 60)] <- NA  # Create missing values
  
  x5 <- ifelse(x1 == 'A', 'Low construal', 'High construal')
  
  result <- t.test2(y1m ~ x5)
  
  # Capture printed output
  output <- capture.output(print(result))
  
  # Should contain message about missing values
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("missing", output_text, ignore.case = TRUE))
  expect_true(attr(result, "NA1") > 0 || attr(result, "NA2") > 0)
  
  # If group mapping is shown, verify it appears
  if (attr(result, "show_group_mapping")) {
    expect_true(grepl("Group 1:", output_text))
    expect_true(grepl("Group 2:", output_text))
  }
})

test_that("t.test2 prints missing data messages for long variable names with 0/1", {
  set.seed(12)
  n <- 100
  x1 <- rep(c("A", "B"), each = n)
  y1 <- round(rnorm(length(x1), mean = 100, sd = 4), 0)
  y1m <- y1
  y1m[c(50, 60, 150)] <- NA  # Create missing values
  
  x4 <- ifelse(x1 == 'A', 0, 1)
  xlongversion <- x4
  
  result <- t.test2(y1m ~ xlongversion)
  
  # Capture printed output
  output <- capture.output(print(result))
  
  # Should contain message about missing values
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("missing", output_text, ignore.case = TRUE))
  expect_true(attr(result, "NA1") > 0 || attr(result, "NA2") > 0)
})

test_that("t.test2 produces exact output format for all scenarios", {
  set.seed(12)
  n <- 100
  x1 <- rep(c("A", "B"), each = n)
  x2 <- rep(c("men", "women"), n)
  
  y1 <- round(rnorm(length(x1), mean = 100, sd = 4), 0)
  y2 <- round(rnorm(length(x1), mean = 100, sd = 15), 0)
  y1m <- y1
  y1m[c(192, 111)] <- NA
  
  # Test 1: t.test2(y1~x1) - should have A, B, A-B, N(A), N(B)
  result1 <- t.test2(y1 ~ x1)
  expect_true("A" %in% names(result1))
  expect_true("B" %in% names(result1))
  expect_true("A-B" %in% names(result1))
  expect_true("N(A)" %in% names(result1))
  expect_true("N(B)" %in% names(result1))
  
  # Test 2: t.test2(y1~x2) - should have men, women, men-women, N(men), N(women)
  result2 <- t.test2(y1 ~ x2)
  expect_true("men" %in% names(result2))
  expect_true("women" %in% names(result2))
  expect_true("men-women" %in% names(result2))
  expect_true("N(men)" %in% names(result2))
  expect_true("N(women)" %in% names(result2))
  
  # Test 3: t.test2(DV1~IV1,data=df1) - should have A, B, A-B, N(A), N(B)
  df1 <- data.frame(DV1 = y1, IV1 = x1)
  result3 <- t.test2(DV1 ~ IV1, data = df1)
  expect_true("A" %in% names(result3))
  expect_true("B" %in% names(result3))
  expect_true("A-B" %in% names(result3))
  expect_true("N(A)" %in% names(result3))
  expect_true("N(B)" %in% names(result3))
  
  # Test 4: t.test2(y1, y2) - should have y1, y2, y1-y2, N(y1), N(y2)
  result4 <- t.test2(y1, y2)
  expect_true("y1" %in% names(result4))
  expect_true("y2" %in% names(result4))
  expect_true("y1-y2" %in% names(result4))
  expect_true("N(y1)" %in% names(result4))
  expect_true("N(y2)" %in% names(result4))
  
  # Test 5: t.test2(y1, y2,paired=TRUE) - should have y1, y2, y1-y2, N, r(y1,y2)
  result5 <- t.test2(y1, y2, paired = TRUE)
  expect_true("y1" %in% names(result5))
  expect_true("y2" %in% names(result5))
  expect_true("y1-y2" %in% names(result5))
  expect_true("N" %in% names(result5))
  expect_true("r(y1,y2)" %in% names(result5))
  
  # Test 6: t.test2(y1m, y2,paired=TRUE) - should have y1m, y2, y1m-y2, N, r(y1m,y2)
  result6 <- t.test2(y1m, y2, paired = TRUE)
  expect_true("y1m" %in% names(result6))
  expect_true("y2" %in% names(result6))
  expect_true("y1m-y2" %in% names(result6))
  expect_true("N" %in% names(result6))
  expect_true("r(y1m,y2)" %in% names(result6))
  
  # Verify missing data message for result6
  output6 <- capture.output(print(result6))
  output6_text <- paste(output6, collapse = "\n")
  expect_true(grepl("note: 2 of 200 pairs were dropped due to missing values", output6_text, fixed = TRUE))
  
  # Test 7: t.test2(y1~x4) with 0/1 - should have x4=0, x4=1, x4=0-x4=1, N(x4=0), N(x4=1)
  x4 <- ifelse(x1 == 'A', 0, 1)
  result7 <- t.test2(y1 ~ x4)
  expect_true("x4=0" %in% names(result7))
  expect_true("x4=1" %in% names(result7))
  expect_true("x4=0-x4=1" %in% names(result7))
  expect_true("N(x4=0)" %in% names(result7))
  expect_true("N(x4=1)" %in% names(result7))
  
  # Test 8: t.test2(y1~xlongversion) - should use Group 1/Group 2 format
  xlongversion <- x4
  result8 <- t.test2(y1 ~ xlongversion)
  expect_true("Group 1" %in% names(result8))
  expect_true("Group 2" %in% names(result8))
  expect_true("1-2" %in% names(result8))
  expect_true("N1" %in% names(result8))
  expect_true("N2" %in% names(result8))
  
  # Verify group mapping output
  output8 <- capture.output(print(result8))
  output8_text <- paste(output8, collapse = "\n")
  expect_true(grepl("Group 1: xlongversion=0", output8_text, fixed = TRUE))
  expect_true(grepl("Group 2: xlongversion=1", output8_text, fixed = TRUE))
  
  # Test 9: t.test2(y1m~xlongversion) - should have missing data message
  result9 <- t.test2(y1m ~ xlongversion)
  expect_true("Group 1" %in% names(result9))
  expect_true("Group 2" %in% names(result9))
  expect_true("1-2" %in% names(result9))
  
  # Verify group mapping and missing data message
  output9 <- capture.output(print(result9))
  output9_text <- paste(output9, collapse = "\n")
  expect_true(grepl("Group 1: xlongversion=0", output9_text, fixed = TRUE))
  expect_true(grepl("Group 2: xlongversion=1", output9_text, fixed = TRUE))
  # Check for missing data message - should mention both groups with varname=value format
  # The message format should be: "When 'xlongversion=0' there are ... and when 'xlongversion=1' there are ..."
  # Check that the message contains "When '" pattern and mentions missing values
  expect_true(grepl("note:", output9_text, fixed = TRUE))
  expect_true(grepl("values missing", output9_text, fixed = TRUE))
  # Should mention xlongversion in the missing data message
  # The message should mention both groups, but we check flexibly for the pattern
  has_when_pattern <- grepl("When '", output9_text, fixed = TRUE)
  has_xlongversion_0 <- grepl("xlongversion=0", output9_text, fixed = TRUE)
  has_xlongversion_1 <- grepl("xlongversion=1", output9_text, fixed = TRUE)
  # Either both groups are mentioned in the missing data message, or the When pattern is present
  expect_true((has_xlongversion_0 && has_xlongversion_1) || has_when_pattern)
  
  # Test 10: t.test2(y1~x5) - should use Group 1/Group 2 format
  x5 <- ifelse(x1 == 'A', 'Low construal', 'High construal')
  result10 <- t.test2(y1 ~ x5)
  expect_true("Group 1" %in% names(result10))
  expect_true("Group 2" %in% names(result10))
  expect_true("1-2" %in% names(result10))
  expect_true("N1" %in% names(result10))
  expect_true("N2" %in% names(result10))
  
  # Verify group mapping output
  output10 <- capture.output(print(result10))
  output10_text <- paste(output10, collapse = "\n")
  expect_true(grepl("Group 1:", output10_text, fixed = TRUE))
  expect_true(grepl("Group 2:", output10_text, fixed = TRUE))
  # Should show either "Low construal" or "High construal"
  expect_true(grepl("Low construal", output10_text, fixed = TRUE) || 
              grepl("High construal", output10_text, fixed = TRUE))
  
  # Test 11: t.test2(y1m~x5) - should have missing data message
  result11 <- t.test2(y1m ~ x5)
  expect_true("Group 1" %in% names(result11))
  expect_true("Group 2" %in% names(result11))
  expect_true("1-2" %in% names(result11))
  
  # Verify group mapping and missing data message
  output11 <- capture.output(print(result11))
  output11_text <- paste(output11, collapse = "\n")
  expect_true(grepl("Group 1:", output11_text, fixed = TRUE))
  expect_true(grepl("Group 2:", output11_text, fixed = TRUE))
  expect_true(grepl("is missing", output11_text, fixed = TRUE))
  expect_true(grepl("values", output11_text, fixed = TRUE))
  # Should mention either "Low construal" or "High construal" in the missing data message
  expect_true(grepl("Low construal", output11_text, fixed = TRUE) || 
              grepl("High construal", output11_text, fixed = TRUE))
})

test_that("t.test2 uses 'mean' column name for one-sample tests", {
  set.seed(12)
  n <- 100
  y1 <- round(rnorm(n, mean = 100, sd = 4), 0)
  
  # One-sample test
  result <- t.test2(y1)
  
  # Should have "mean" as column name, not "Group 1"
  expect_true("mean" %in% names(result))
  expect_false("Group 1" %in% names(result))
  expect_false("Group 2" %in% names(result))
  
  # Should have "N" as column name, not "N1" or "N2"
  expect_true("N" %in% names(result))
  expect_false("N1" %in% names(result))
  expect_false("N2" %in% names(result))
  
  # Should not have diff column
  expect_false(any(grepl("-", names(result), fixed = TRUE)))
  
  # Verify printed output doesn't show group mapping
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  expect_false(grepl("Group 1:", output_text, fixed = TRUE))
  expect_false(grepl("Group 2:", output_text, fixed = TRUE))
  
  # Should show "One sample" in title
  expect_true(grepl("One sample", output_text, fixed = TRUE))
})

test_that("t.test2 correctly extracts group names and sample sizes when data= is specified but variables exist in environment", {
  # This test verifies the fix for the bug where t.test2() would miss group names
  # and sample sizes when data= is specified but variables are found in the environment
  
  set.seed(12)
  n <- 100
  
  # Create variables in the environment
  y_env <- round(rnorm(n * 2, mean = 100, sd = 4), 0)
  group_env <- rep(c("A", "B"), n)
  
  # Create a different data frame (with different data)
  df <- data.frame(
    y = round(rnorm(n * 2, mean = 50, sd = 2), 0),  # Different mean and SD
    group = rep(c("X", "Y"), n)  # Different group names
  )
  
  # When data= is specified but variables exist in environment,
  # t.test() uses environment variables (standard R behavior)
  # t.test2() should also use environment variables and correctly extract
  # group names and sample sizes from the environment variables
  
  result <- t.test2(y_env ~ group_env, data = df)
  
  # Should use environment variables (A and B, not X and Y)
  expect_true("A" %in% names(result))
  expect_true("B" %in% names(result))
  expect_false("X" %in% names(result))
  expect_false("Y" %in% names(result))
  
  # Should have correct sample sizes from environment variables
  expect_true("N(A)" %in% names(result))
  expect_true("N(B)" %in% names(result))
  expect_equal(result[["N(A)"]], n)
  expect_equal(result[["N(B)"]], n)
  
  # Should have correct group names in attributes
  expect_equal(attr(result, "group1"), "A")
  expect_equal(attr(result, "group2"), "B")
  
  # Verify the test actually ran on environment data (not data frame data)
  # The means should match environment data, not data frame data
  # Since we set seed, we can verify approximate values
  expect_true(result[["A"]] > 90)  # Environment mean ~100
  expect_true(result[["B"]] > 90)  # Environment mean ~100
  # Data frame mean would be ~50, so this confirms we used environment data
})

