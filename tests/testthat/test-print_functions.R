#print.table2_001
test_that("print.table2 runs without errors", {
  df <- data.frame(
    group = c("A", "A", "B", "B", "A"),
    status = c("X", "Y", "X", "Y", "X")
  )
  
  tbl <- table2(df$group, df$status)
  
  # Should print without errors
  expect_error(print(tbl), NA)
  
  # Should return invisibly
  result <- print(tbl)
  expect_equal(result, tbl)
})

#print.table2_002
test_that("print.table2 handles proportion tables", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  tbl <- table2(x, y, prop = 1)
  
  expect_error(print(tbl), NA)
})

#print.table2_003
test_that("print.table2 handles three-way tables", {
  df <- data.frame(
    x = c("A", "A", "B", "B"),
    y = c("X", "Y", "X", "Y"),
    z = c("high", "low", "high", "low")
  )
  
  tbl <- table2(df$x, df$y, df$z)
  
  expect_error(print(tbl), NA)
})

#print.table2_004
test_that("print.table2 handles tables without dimension names", {
  x <- c("A", "A", "B", "B")
  y <- c("X", "Y", "X", "Y")
  
  # Create table without using df$ syntax (no dimension names)
  tbl <- table2(x, y)
  
  # Should still print (may fall back to default print)
  expect_error(print(tbl), NA)
})





