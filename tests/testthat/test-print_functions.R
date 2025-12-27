test_that("print.simplified_ttest runs without errors", {
  # Create a t-test result
  y <- rnorm(100)
  group <- rep(c("A", "B"), 50)
  tt_result <- t.test(y ~ group)
  
  # Add required fields for simplified_ttest
  tt_result$y_var_name <- "y"
  tt_result$group_var_name <- "group"
  tt_result$is_formula <- TRUE
  tt_result$digits <- 3
  tt_result$y_var <- y
  tt_result$group_var <- group
  class(tt_result) <- c("simplified_ttest", class(tt_result))
  
  # Should print without errors
  expect_error(print(tt_result), NA)
  
  # Should return invisibly
  result <- print(tt_result)
  expect_equal(result, tt_result)
})

test_that("print.simplified_ttest handles two-sample test", {
  x1 <- rnorm(50, mean = 5)
  x2 <- rnorm(50, mean = 4.8)
  tt_result <- t.test(x1, x2)
  
  tt_result$x.name <- "x1"
  tt_result$y.name <- "x2"
  tt_result$is_formula <- FALSE
  tt_result$digits <- 3
  tt_result$diff <- tt_result$estimate[1] - tt_result$estimate[2]
  class(tt_result) <- c("simplified_ttest", class(tt_result))
  
  expect_error(print(tt_result), NA)
})

test_that("print.simplified_ttest handles one-sample test", {
  x <- rnorm(50, mean = 5)
  tt_result <- t.test(x, mu = 5)
  
  tt_result$x.name <- "x"
  tt_result$is_formula <- FALSE
  tt_result$digits <- 3
  class(tt_result) <- c("simplified_ttest", class(tt_result))
  
  expect_error(print(tt_result), NA)
})

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

test_that("print.table2 handles proportion tables", {
  x <- c("A", "A", "B", "B", "A")
  y <- c("X", "Y", "X", "Y", "X")
  
  tbl <- table2(x, y, prop = 1)
  
  expect_error(print(tbl), NA)
})

test_that("print.table2 handles three-way tables", {
  df <- data.frame(
    x = c("A", "A", "B", "B"),
    y = c("X", "Y", "X", "Y"),
    z = c("high", "low", "high", "low")
  )
  
  tbl <- table2(df$x, df$y, df$z)
  
  expect_error(print(tbl), NA)
})

test_that("print.table2 handles tables without dimension names", {
  x <- c("A", "A", "B", "B")
  y <- c("X", "Y", "X", "Y")
  
  # Create table without using df$ syntax (no dimension names)
  tbl <- table2(x, y)
  
  # Should still print (may fall back to default print)
  expect_error(print(tbl), NA)
})

test_that("print.simplified_ttest handles small p-values", {
  # Create test with very small p-value
  set.seed(123)
  x1 <- rnorm(1000, mean = 10)
  x2 <- rnorm(1000, mean = 5)
  tt_result <- t.test(x1, x2)
  
  tt_result$x.name <- "x1"
  tt_result$y.name <- "x2"
  tt_result$is_formula <- FALSE
  tt_result$digits <- 3
  tt_result$diff <- tt_result$estimate[1] - tt_result$estimate[2]
  class(tt_result) <- c("simplified_ttest", class(tt_result))
  
  # Should handle small p-values
  expect_error(print(tt_result), NA)
})

test_that("print.simplified_ttest handles Welch vs Student", {
  x1 <- rnorm(50, mean = 5, sd = 1)
  x2 <- rnorm(50, mean = 4.8, sd = 2)  # Different SDs trigger Welch
  
  tt_result <- t.test(x1, x2)
  tt_result$x.name <- "x1"
  tt_result$y.name <- "x2"
  tt_result$is_formula <- FALSE
  tt_result$digits <- 3
  tt_result$diff <- tt_result$estimate[1] - tt_result$estimate[2]
  class(tt_result) <- c("simplified_ttest", class(tt_result))
  
  expect_error(print(tt_result), NA)
})







