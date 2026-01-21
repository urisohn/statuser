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
