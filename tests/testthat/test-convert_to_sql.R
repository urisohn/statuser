test_that("convert_to_sql creates SQL file", {
  # Create temporary CSV file
  temp_csv <- tempfile(fileext = ".csv")
  temp_sql <- tempfile(fileext = ".sql")
  
  # Create test data
  test_df <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    age = c(25, 30, 35),
    score = c(85.5, 92.0, 78.5)
  )
  
  write.csv(test_df, temp_csv, row.names = FALSE)
  
  # Should run without errors
  expect_error(convert_to_sql(temp_csv, temp_sql), NA)
  
  # Check that output file was created
  expect_true(file.exists(temp_sql))
  
  # Check that file has content
  sql_content <- readLines(temp_sql)
  expect_true(length(sql_content) > 0)
  
  # Cleanup
  unlink(temp_csv)
  unlink(temp_sql)
})

test_that("convert_to_sql creates INSERT statements", {
  temp_csv <- tempfile(fileext = ".csv")
  temp_sql <- tempfile(fileext = ".sql")
  
  test_df <- data.frame(
    name = c("Alice", "Bob"),
    value = c(10, 20)
  )
  
  write.csv(test_df, temp_csv, row.names = FALSE)
  
  convert_to_sql(temp_csv, temp_sql)
  
  sql_content <- readLines(temp_sql)
  
  # Should have INSERT statements
  expect_true(any(grepl("INSERT INTO", sql_content)))
  
  # Cleanup
  unlink(temp_csv)
  unlink(temp_sql)
})

test_that("convert_to_sql handles create_table parameter", {
  temp_csv <- tempfile(fileext = ".csv")
  temp_sql <- tempfile(fileext = ".sql")
  
  test_df <- data.frame(
    name = c("Alice"),
    age = c(25)
  )
  
  write.csv(test_df, temp_csv, row.names = FALSE)
  
  # With create_table = TRUE
  convert_to_sql(temp_csv, temp_sql, create_table = TRUE)
  
  sql_content <- readLines(temp_sql)
  
  # Should have CREATE TABLE statement
  expect_true(any(grepl("CREATE TABLE", sql_content)))
  
  # Cleanup
  unlink(temp_csv)
  unlink(temp_sql)
})

test_that("convert_to_sql handles date columns", {
  temp_csv <- tempfile(fileext = ".csv")
  temp_sql <- tempfile(fileext = ".sql")
  
  test_df <- data.frame(
    name = c("Alice"),
    date = c("2024-01-01")
  )
  
  write.csv(test_df, temp_csv, row.names = FALSE)
  
  convert_to_sql(temp_csv, temp_sql, create_table = TRUE)
  
  sql_content <- paste(readLines(temp_sql), collapse = "\n")
  
  # Should detect DATE type
  expect_true(grepl("DATE", sql_content))
  
  # Cleanup
  unlink(temp_csv)
  unlink(temp_sql)
})

test_that("convert_to_sql handles numeric columns", {
  temp_csv <- tempfile(fileext = ".csv")
  temp_sql <- tempfile(fileext = ".sql")
  
  test_df <- data.frame(
    name = c("Alice"),
    value = c(123.45)
  )
  
  write.csv(test_df, temp_csv, row.names = FALSE)
  
  convert_to_sql(temp_csv, temp_sql, create_table = TRUE)
  
  sql_content <- paste(readLines(temp_sql), collapse = "\n")
  
  # Should detect REAL type for numeric columns
  expect_true(grepl("REAL", sql_content))
  
  # Cleanup
  unlink(temp_csv)
  unlink(temp_sql)
})

test_that("convert_to_sql escapes single quotes", {
  temp_csv <- tempfile(fileext = ".csv")
  temp_sql <- tempfile(fileext = ".sql")
  
  test_df <- data.frame(
    name = c("Alice's name"),
    value = c(10)
  )
  
  write.csv(test_df, temp_csv, row.names = FALSE)
  
  convert_to_sql(temp_csv, temp_sql)
  
  sql_content <- readLines(temp_sql)
  
  # Should escape single quotes (double them)
  expect_true(any(grepl("''", sql_content)))
  
  # Cleanup
  unlink(temp_csv)
  unlink(temp_sql)
})

test_that("convert_to_sql returns invisibly", {
  temp_csv <- tempfile(fileext = ".csv")
  temp_sql <- tempfile(fileext = ".sql")
  
  test_df <- data.frame(name = c("Alice"), value = c(10))
  write.csv(test_df, temp_csv, row.names = FALSE)
  
  result <- convert_to_sql(temp_csv, temp_sql)
  expect_null(result)
  
  # Cleanup
  unlink(temp_csv)
  unlink(temp_sql)
})

