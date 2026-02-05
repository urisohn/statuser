# Test script for validate_formula changes
library(statuser)

cat("=== Testing validate_formula functionality ===\n\n")

# Test 1: Non-existent object should give clean error
cat("Test 1: Non-existent object (df233$ymadeup where df233 doesn't exist)\n")
result1 <- tryCatch({
  plot_freq(df233$ymadeup)
  "ERROR: Should have failed"
}, error = function(e) {
  cat("Error message:", e$message, "\n")
  e$message
})
cat("Result:", if(grepl("df233", result1)) "PASS" else "FAIL", "\n\n")

# Test 2: Column doesn't exist in existing dataframe
cat("Test 2: Column doesn't exist in existing dataframe\n")
df1 <- data.frame(x = 1:10, y = 11:20)
result2 <- tryCatch({
  plot_freq(df1$ymadeup)
  "ERROR: Should have failed"
}, error = function(e) {
  cat("Error message:", e$message, "\n")
  e$message
})
cat("Result:", if(grepl("ymadeup", result2) && grepl("df1", result2)) "PASS" else "FAIL", "\n\n")

# Test 3: Formula with missing variables in data should fail
cat("Test 3: Formula with missing variables in data\n")
df2 <- data.frame(x = 1:10)
result3 <- tryCatch({
  plot_freq(y ~ group, data = df2)
  "ERROR: Should have failed"
}, error = function(e) {
  cat("Error message:", e$message, "\n")
  e$message
})
cat("Result:", if(grepl("not found in data", result3)) "PASS" else "FAIL", "\n\n")

# Test 4: Formula with missing variables in environment should fail
cat("Test 4: Formula with missing variables in environment\n")
if(exists("missing_var123")) rm(missing_var123)
result4 <- tryCatch({
  plot_freq(missing_var123 ~ 1)
  "ERROR: Should have failed"
}, error = function(e) {
  cat("Error message:", e$message, "\n")
  e$message
})
cat("Result:", if(grepl("not found", result4)) "PASS" else "FAIL", "\n\n")

# Test 5: Valid inputs should work
cat("Test 5: Valid inputs should work\n")
result5 <- tryCatch({
  x <- c(1, 1, 2, 2, 2, 5, 5)
  invisible(plot_freq(x ~ 1))
  "SUCCESS"
}, error = function(e) {
  cat("Error message:", e$message, "\n")
  paste("FAIL:", e$message)
})
cat("Result:", if(result5 == "SUCCESS") "PASS" else "FAIL", "\n\n")

# Test 6: Valid inputs with data frame should work
cat("Test 6: Valid inputs with data frame should work\n")
result6 <- tryCatch({
  df3 <- data.frame(value = c(1, 1, 2, 2, 2, 5, 5))
  invisible(plot_freq(value ~ 1, data = df3))
  "SUCCESS"
}, error = function(e) {
  cat("Error message:", e$message, "\n")
  paste("FAIL:", e$message)
})
cat("Result:", if(result6 == "SUCCESS") "PASS" else "FAIL", "\n\n")

# Test 7: t.test2 with formula
cat("Test 7: t.test2 with formula (missing variables)\n")
result7 <- tryCatch({
  t.test2(missing_y ~ missing_group)
  "ERROR: Should have failed"
}, error = function(e) {
  cat("Error message:", e$message, "\n")
  e$message
})
cat("Result:", if(grepl("not found", result7)) "PASS" else "FAIL", "\n\n")

# Test 8: lm2 with formula
cat("Test 8: lm2 with formula (missing variables)\n")
result8 <- tryCatch({
  lm2(missing_y ~ missing_x)
  "ERROR: Should have failed"
}, error = function(e) {
  cat("Error message:", e$message, "\n")
  e$message
})
cat("Result:", if(grepl("not found", result8)) "PASS" else "FAIL", "\n\n")

cat("=== All tests completed ===\n")
