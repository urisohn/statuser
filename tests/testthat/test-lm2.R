# Tests for lm2() function

test_that("lm2 returns statuser output by default", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  
  # Check it's a data frame with correct class

  expect_s3_class(result, "lm2")
  expect_s3_class(result, "data.frame")
  
  # Check expected columns exist
  expected_cols <- c("term", "estimate", "SE.robust", "SE.classical", "t", "df", "p.value", "B")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check correct number of rows (intercept + 2 predictors)
  expect_equal(nrow(result), 3)
  
  # Check term names
  expect_equal(result$term, c("(Intercept)", "wt", "hp"))
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
  
  # Check se_type attribute
  expect_equal(attr(result, "se_type"), "HC3")
})

test_that("lm2 standardized coefficients are calculated correctly", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  
  # Intercept should have NA for standardized coefficient
  expect_true(is.na(result$B[result$term == "(Intercept)"]))
  
  # Other coefficients should have non-NA standardized values
  expect_false(is.na(result$B[result$term == "wt"]))
  expect_false(is.na(result$B[result$term == "hp"]))
  
  # Manually calculate standardized coefficient for 'wt'
  b_wt <- result$estimate[result$term == "wt"]
  sd_wt <- sd(mtcars$wt)
  sd_mpg <- sd(mtcars$mpg)
  expected_B_wt <- b_wt * (sd_wt / sd_mpg)
  
  expect_equal(result$B[result$term == "wt"], expected_B_wt, tolerance = 1e-10)
})

test_that("lm2 robust and classical SEs differ", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  
  # Robust and classical SEs should generally differ
  # (they might be similar but not exactly equal)
  expect_false(all(result$SE.robust == result$SE.classical))
})

test_that("lm2 passes additional arguments to lm_robust", {
  skip_if_not_installed("estimatr")
  
  # Test with different se_type
  result_hc2 <- lm2(mpg ~ wt + hp, data = mtcars, se_type = "HC2")
  result_hc3 <- lm2(mpg ~ wt + hp, data = mtcars, se_type = "HC3")
  
  # SEs should differ between HC2 and HC3
  expect_false(all(result_hc2$SE.robust == result_hc3$SE.robust))
})

test_that("lm2 stores model attributes", {
  skip_if_not_installed("estimatr")
  
  result <- lm2(mpg ~ wt + hp, data = mtcars)
  
  # Check attributes exist
  expect_true(!is.null(attr(result, "robust_fit")))
  expect_true(!is.null(attr(result, "classical_fit")))
  expect_true(!is.null(attr(result, "r.squared")))
  expect_true(!is.null(attr(result, "adj.r.squared")))
  expect_true(!is.null(attr(result, "nobs")))
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