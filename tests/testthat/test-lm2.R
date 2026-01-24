# Tests for lm2() function

test_that("lm2 returns statuser output by default", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  
  # Check it has the correct classes (lm2 first, then inherits from lm_robust)
  expect_s3_class(result, "lm2")
  expect_s3_class(result, "lm_robust")
  
  # Check the statuser_table attribute exists and has expected columns
  tbl <- attr(result, "statuser_table")
  expect_true(!is.null(tbl))
  expected_cols <- c("term", "estimate", "SE.robust", "SE.classical", "t", "df", "p.value", "B")
  expect_true(all(expected_cols %in% names(tbl)))
  
  # Check correct number of rows (intercept + 2 predictors)
  expect_equal(nrow(tbl), 3)
  
  # Check term names
  expect_equal(tbl$term, c("(Intercept)", "wt", "hp"))
})

test_that("lm2 returns estimatr output when requested", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars, output = "estimatr")
  
  # Check it's an lm_robust object
  expect_s3_class(result, "lm_robust")
})

test_that("lm2 uses HC3 standard errors by default", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  
  # Check se_type (it's a list element since lm2 inherits from lm_robust)
  expect_equal(result$se_type, "HC3")
})

test_that("lm2 standardized coefficients are calculated correctly", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  tbl <- attr(result, "statuser_table")
  
  # Intercept should have NA for standardized coefficient
  expect_true(is.na(tbl$B[tbl$term == "(Intercept)"]))
  
  # Other coefficients should have non-NA standardized values
  expect_false(is.na(tbl$B[tbl$term == "wt"]))
  expect_false(is.na(tbl$B[tbl$term == "hp"]))
  
  # Manually calculate standardized coefficient for 'wt'
  b_wt <- tbl$estimate[tbl$term == "wt"]
  sd_wt <- sd(mtcars$wt)
  sd_mpg <- sd(mtcars$mpg)
  expected_B_wt <- b_wt * (sd_wt / sd_mpg)
  
  expect_equal(tbl$B[tbl$term == "wt"], expected_B_wt, tolerance = 1e-10)
})

test_that("lm2 robust and classical SEs differ", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  tbl <- attr(result, "statuser_table")
  
  # Robust and classical SEs should generally differ
  # (they might be similar but not exactly equal)
  expect_false(all(tbl$SE.robust == tbl$SE.classical))
})

test_that("lm2 passes additional arguments to lm_robust", {
  skip_if_not_installed("estimatr")
  
  # Test with different se_type
  result_hc2 <- lm2(mpg ~ wt + hp, data = mtcars, se_type = "HC2")
  result_hc3 <- lm2(mpg ~ wt + hp, data = mtcars, se_type = "HC3")
  
  tbl_hc2 <- attr(result_hc2, "statuser_table")
  tbl_hc3 <- attr(result_hc3, "statuser_table")
  
  # SEs should differ between HC2 and HC3
  expect_false(all(tbl_hc2$SE.robust == tbl_hc3$SE.robust))
})

test_that("lm2 stores model attributes", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  
  # Check that it inherits from lm_robust (so it has these as list elements, not attributes)
  expect_true(!is.null(result$r.squared))
  expect_true(!is.null(result$adj.r.squared))
  expect_true(!is.null(result$nobs))
  
  # Check statuser-specific attributes exist
  expect_true(!is.null(attr(result, "statuser_table")))
  expect_true(!is.null(attr(result, "classical_fit")))
})

test_that("lm2 validates output argument", {
  skip_if_not_installed("estimatr")
  
  expect_error(
    lm2(mpg ~ wt + hp, data = mtcars, output = "invalid"),
    "should be one of"
  )
})

test_that("lm2 predict matches lm_robust predict with clustering", {
  skip_if_not_installed("estimatr")
  
  # Create clustered data
  set.seed(123)
  cluster_id <- rep(1:10, each = 20)
  x <- rnorm(200)
  y <- x + rnorm(200) + rnorm(10)[cluster_id]
  test_data <- data.frame(y = y, x = x, cluster_id = cluster_id)
  
  # Fit with lm2 and lm_robust
 m1 <- lm2(y ~ x, data = test_data, clusters = cluster_id)
  m2 <- estimatr::lm_robust(y ~ x, data = test_data, clusters = cluster_id)
  
  # Get predictions with SEs
  yh1 <- predict(m1, newdata = test_data, se.fit = TRUE)
  yh2 <- predict(m2, newdata = test_data, se.fit = TRUE)
  
  # Fitted values should be identical
  expect_equal(mean(yh1$fit == yh2$fit), 1)
  
  # Standard errors should be identical
  expect_equal(mean(yh1$se.fit == yh2$se.fit), 1)
})

test_that("lm2 predict matches lm_robust predict without clustering", {
  skip_if_not_installed("estimatr")
  
  # Fit with lm2 and lm_robust
  m1 <- lm2(mpg ~ wt + hp, data = mtcars)
  m2 <- estimatr::lm_robust(mpg ~ wt + hp, data = mtcars, se_type = "HC3")
  
  # Get predictions with SEs
  yh1 <- predict(m1, newdata = mtcars, se.fit = TRUE)
  yh2 <- predict(m2, newdata = mtcars, se.fit = TRUE)
  
  # Fitted values should be identical
  expect_equal(mean(yh1$fit == yh2$fit), 1)
  
  # Standard errors should be identical
  expect_equal(mean(yh1$se.fit == yh2$se.fit), 1)
})

test_that("lm2 is compatible with marginaleffects", {
  skip_if_not_installed("estimatr")
  skip_if_not_installed("marginaleffects")
  
  # lm2 should work with marginaleffects since it inherits from lm_robust
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  
  # Test that slopes() works
  slopes_result <- marginaleffects::slopes(result)
  expect_true(inherits(slopes_result, "data.frame"))
  expect_true(nrow(slopes_result) > 0)
  
  # Test that predictions() works
  pred_result <- marginaleffects::predictions(result)
  expect_true(inherits(pred_result, "data.frame"))
  expect_equal(nrow(pred_result), nrow(mtcars))
})

# ============================================================================
# RED FLAG DETECTION TESTS
# ============================================================================

test_that("lm2 shows red flag (!) when SE differs by >25%", {
  skip_if_not_installed("estimatr")
  
  # Create data with moderate heteroskedasticity
  set.seed(42)
  x <- 1:50
  y <- 2 * x + rnorm(50, sd = x * 0.3)  # Variance increases with x
  
  result <- lm2(y ~ x)
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  

  # Should show at least one red flag indicator (!, !!, or !!!)
  expect_true(
    grepl("!", output_text, fixed = TRUE),
    info = "Expected red flag (!) for heteroskedastic data"
  )
})

test_that("lm2 shows severe red flag (!!!) for extreme heteroskedasticity", {
  skip_if_not_installed("estimatr")
  
  # Create data with severe heteroskedasticity (SE should differ by >100%)
  set.seed(123)
  x <- 1:100
  y <- x + rnorm(100, sd = x)  # Strong heteroskedasticity
  
  result <- lm2(y ~ x)
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  # Should show severe red flag
  expect_true(
    grepl("!!!", output_text, fixed = TRUE),
    info = "Expected severe red flag (!!!) for extreme heteroskedasticity"
  )
})

test_that("lm2 shows X* flag for significantly correlated interaction terms", {
  skip_if_not_installed("estimatr")
  
  # Create data with highly correlated predictors in an interaction
  set.seed(456)
  n <- 200
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.2)  # x2 highly correlated with x1
  y <- x1 + x2 + x1 * x2 + rnorm(n)
  
  result <- lm2(y ~ x1 * x2)
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  # Should show X* flag for the interaction term
  expect_true(
    grepl("X\\*", output_text),
    info = "Expected X* flag for significantly correlated interaction terms"
  )
})

test_that("lm2 shows X flag for moderately correlated interaction terms", {
  skip_if_not_installed("estimatr")
  
  # Create data with moderately correlated predictors (|r| > 0.3 but p > .05 with small n)
  set.seed(789)
  n <- 20  # Small n so correlation may not be significant
  x1 <- rnorm(n)
  x2 <- 0.35 * x1 + rnorm(n, sd = 0.9)  # Moderate correlation ~0.35
  y <- x1 + x2 + x1 * x2 + rnorm(n)
  
  result <- lm2(y ~ x1 * x2)
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  # Should show either X or X* flag (depending on exact p-value)
  has_x_flag <- grepl("X", output_text, fixed = TRUE)
  expect_true(
    has_x_flag,
    info = "Expected X or X* flag for correlated interaction terms"
  )
})

test_that("lm2 does not show interaction flags for uncorrelated terms", {
  skip_if_not_installed("estimatr")
  
  # Create data with uncorrelated predictors
  set.seed(111)
  n <- 100
  x1 <- rnorm(n)
  x2 <- rnorm(n)  # Independent of x1
  y <- x1 + x2 + x1 * x2 + rnorm(n)
  
  result <- lm2(y ~ x1 * x2)
  output <- capture.output(print(result))
  
  # Find the line with the interaction term
  interaction_line <- grep("x1:x2", output, value = TRUE)
  
  # Should not show X or X* in the red.flag column for the interaction
  # The red.flag column is the last column, so check it doesn't contain X as a flag
  if (length(interaction_line) > 0) {
    # The interaction line should have x1:x2 in the term name
    # Check that X* flag is not present (X by itself is part of the term name)
    # X* would indicate significant correlation between predictors
    expect_false(
      any(grepl("X\\*", interaction_line)),
      info = "Should not show X* flag for uncorrelated interaction terms"
    )
  }
})

# ============================================================================
# CLUSTERED STANDARD ERRORS TESTS
# ============================================================================

test_that("lm2 with clusters shows SE.cluster column", {
  skip_if_not_installed("estimatr")
  
  # Create clustered data
  set.seed(222)
  cluster_id <- rep(1:20, each = 10)
  x <- rnorm(200)
  cluster_effect <- rnorm(20)[cluster_id]
  y <- x + cluster_effect + rnorm(200)
  test_data <- data.frame(y = y, x = x, cluster_id = cluster_id)
  
  result <- lm2(y ~ x, data = test_data, clusters = cluster_id)
  
  # Check that SE.cluster is in the statuser_table
  tbl <- attr(result, "statuser_table")
  expect_true("SE.cluster" %in% names(tbl))
  
  # SE.cluster should differ from SE.robust
  expect_false(all(tbl$SE.cluster == tbl$SE.robust))
})

test_that("lm2 with clusters prints cluster info", {
  skip_if_not_installed("estimatr")
  
  set.seed(333)
  cluster_id <- rep(1:15, each = 10)
  x <- rnorm(150)
  y <- x + rnorm(15)[cluster_id] + rnorm(150)
  test_data <- data.frame(y = y, x = x, cluster_id = cluster_id)
  
  result <- lm2(y ~ x, data = test_data, clusters = cluster_id)
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  # Should mention CR2 (cluster-robust) SE type
  expect_true(
    grepl("CR2", output_text),
    info = "Should mention CR2 for clustered SE"
  )
  
  # Should mention cluster in output
  expect_true(
    grepl("cluster", output_text, ignore.case = TRUE),
    info = "Should mention 'cluster' in output"
  )
})

test_that("lm2 clustered SE is larger than robust SE when there's clustering", {
  skip_if_not_installed("estimatr")
  
  # Create data with strong within-cluster correlation
  set.seed(444)
  cluster_id <- rep(1:10, each = 30)
  x <- rnorm(300)
  cluster_effect <- rnorm(10, sd = 3)[cluster_id]  # Strong cluster effects
  y <- x + cluster_effect + rnorm(300, sd = 0.5)
  test_data <- data.frame(y = y, x = x, cluster_id = cluster_id)
  
  result <- lm2(y ~ x, data = test_data, clusters = cluster_id)
  tbl <- attr(result, "statuser_table")
  
  # For the x coefficient, clustered SE should be larger than robust SE
  # due to the within-cluster correlation
  x_row <- tbl[tbl$term == "x", ]
  expect_true(
    x_row$SE.cluster > x_row$SE.robust,
    info = "Clustered SE should be larger when there's within-cluster correlation"
  )
})

# ============================================================================
# PRINT METHOD TESTS
# ============================================================================

test_that("print.lm2 shows notes by default", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt, data = mtcars)
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(grepl("Notes:", output_text))
  expect_true(grepl("effect.size", output_text))
  expect_true(grepl("red.flag", output_text))
})

test_that("print.lm2 respects notes=FALSE", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt, data = mtcars)
  output <- capture.output(print(result, notes = FALSE))
  output_text <- paste(output, collapse = "\n")
  
  expect_false(grepl("Notes:", output_text))
})

test_that("print.lm2 respects notes=FALSE from lm2 call", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt, data = mtcars, notes = FALSE)
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  expect_false(grepl("Notes:", output_text))
})

test_that("print.lm2 shows Call correctly", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(grepl("Call:", output_text))
  expect_true(grepl("lm2", output_text))
})

test_that("print.lm2 shows model summary statistics", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt, data = mtcars)
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(grepl("N =", output_text))
  expect_true(grepl("R²", output_text) || grepl("R2", output_text))
  expect_true(grepl("missing", output_text))
})

test_that("print.lm2 shows interaction notes when model has interactions", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt * hp, data = mtcars)
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  # Should show interaction-specific notes
  expect_true(
    grepl("X:", output_text) || grepl("interaction", output_text, ignore.case = TRUE),
    info = "Should show interaction-related notes"
  )
})

# ============================================================================
# COEFFICIENT CONSISTENCY TESTS
# ============================================================================

test_that("lm2 produces same coefficients as lm", {
  skip_if_not_installed("estimatr")
  
  result_lm2 <- lm2(mpg ~ wt + hp, data = mtcars)
  result_lm <- lm(mpg ~ wt + hp, data = mtcars)
  
  expect_equal(
    unname(coef(result_lm2)),
    unname(coef(result_lm)),
    tolerance = 1e-10
  )
})

test_that("lm2 produces same R-squared as lm", {
  skip_if_not_installed("estimatr")
  
  result_lm2 <- lm2(mpg ~ wt + hp, data = mtcars)
  result_lm <- lm(mpg ~ wt + hp, data = mtcars)
  
  expect_equal(
    result_lm2$r.squared,
    summary(result_lm)$r.squared,
    tolerance = 1e-10
  )
})

test_that("lm2 classical SE matches lm SE", {
  skip_if_not_installed("estimatr")
  
  result_lm2 <- lm2(mpg ~ wt + hp, data = mtcars)
  result_lm <- lm(mpg ~ wt + hp, data = mtcars)
  
  tbl <- attr(result_lm2, "statuser_table")
  lm_se <- summary(result_lm)$coefficients[, "Std. Error"]
  
  expect_equal(
    tbl$SE.classical,
    unname(lm_se),
    tolerance = 1e-10
  )
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("lm2 handles missing values correctly", {
  skip_if_not_installed("estimatr")
  
  # Create data with missing values
  data_with_na <- mtcars
  data_with_na$mpg[c(1, 5, 10)] <- NA
  data_with_na$wt[c(3, 7)] <- NA
  
  result <- lm2(mpg ~ wt + hp, data = data_with_na)
  
  # Should report missing observations
  expect_true(attr(result, "n_missing") > 0)
  
  # Output should mention missing
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("missing", output_text))
})

test_that("lm2 handles single predictor", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt, data = mtcars)
  
  tbl <- attr(result, "statuser_table")
  expect_equal(nrow(tbl), 2)  # intercept + 1 predictor
})

test_that("lm2 handles many predictors", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp + drat + qsec + vs + am, data = mtcars)
  
  tbl <- attr(result, "statuser_table")
  expect_equal(nrow(tbl), 7)  # intercept + 6 predictors
})

# ============================================================================
# SNAPSHOT TESTS FOR OUTPUT FORMAT
# ============================================================================

test_that("lm2 print output format is stable", {
  skip_if_not_installed("estimatr")
  
  # Use mtcars for reproducible output
  result <- lm2(mpg ~ wt + hp, data = mtcars, notes = FALSE)
  
  expect_snapshot(print(result))
})

test_that("lm2 print output with notes is stable", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt, data = mtcars, notes = TRUE)
  
  expect_snapshot(print(result))
})

test_that("lm2 print output with interaction is stable", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt * hp, data = mtcars, notes = FALSE)
  
  expect_snapshot(print(result))
})