# ============================================================================
# explore_lm2.R - Interactive exploration script for lm2()
# 
# This script generates various data structures to test lm2() with:
# 1 Simple regressions
# 2 Multiple predictors
# 3 Interactions (numeric * numeric, numeric * factor, factor * factor)
# 4 Factor predictors (2-level, multi-level)
# 5 Clustered data
# 6 Fixed effects / panel data
# 7 Missing data patterns
# 8 Outliers and heteroskedasticity
# 9 Polynomial and nonlinear relationships
#  ============================================================================
    path="c:/git/statuser"
    devtools::load_all(path)
    set.seed(2024)
    n <- 200

# ============================================================================
# SECTION 1: SIMPLE REGRESSIONS
# ============================================================================

  #Gen data
    simple_data <- data.frame(
      x = rnorm(n),
      z = rnorm(n)
    )
    simple_data$y <- 2 + 0.5 * simple_data$x + rnorm(n, sd = 1)
    simple_data$y2 <- 1 + 0.5 * simple_data$x + 0.3 * simple_data$z + rnorm(n, sd = 1)

# 1.1 - Simple regression (y ~ x)
  lm2(y ~ x, data = simple_data)

# 1.2 - Multiple predictors (y ~ x + z)
  lm2(y2 ~ x + z, data = simple_data)


# ============================================================================
# SECTION 2: FACTOR PREDICTORS
# ============================================================================
  
  factor_data <- data.frame(
    x = rnorm(n),
    group2 = factor(sample(c("A", "B"), n, replace = TRUE)),
    group3 = factor(sample(c("Low", "Med", "High"), n, replace = TRUE), 
                    levels = c("Low", "Med", "High"))
  )
  factor_data$y <- 5 + 
    0.5 * factor_data$x + 
    ifelse(factor_data$group2 == "B", 2, 0) + 
    rnorm(n, sd = 1)
  factor_data$y2 <- 3 + 
    0.4 * factor_data$x +
    ifelse(factor_data$group3 == "Med", 1.5, 0) +
    ifelse(factor_data$group3 == "High", 3, 0) +
    rnorm(n, sd = 1)
  factor_data$group3_ord <- ordered(factor_data$group3, levels = c("Low", "Med", "High"))

# 2.1 - Two-level factor
  lm2(y ~ x + group2, data = factor_data)

# 2.2 - Three-level factor
  lm2(y2 ~ x + group3, data = factor_data)

# 2.3 - Ordered factor (polynomial contrasts)
  lm2(y2 ~ x + group3_ord, data = factor_data)


# ============================================================================
# SECTION 3: INTERACTIONS
# ============================================================================

  
  #Data
      interact_data <- data.frame(
        x1 = rnorm(n),
        x2 = rnorm(n)
      )
      interact_data$y <- 1 + 
        0.5 * interact_data$x1 + 
        0.3 * interact_data$x2 + 
        0.4 * interact_data$x1 * interact_data$x2 +
        rnorm(n, sd = 1)
      interact_data$x2_corr <- interact_data$x1 + rnorm(n, sd = 0.5)  # r ~ 0.9
      interact_data$y_corr <- 1 + 
        0.5 * interact_data$x1 + 
        0.3 * interact_data$x2_corr + 
        0.4 * interact_data$x1 * interact_data$x2_corr +
        rnorm(n, sd = 1)
      interact_data$group <- factor(sample(c("Control", "Treatment"), n, replace = TRUE))
      interact_data$y_nf <- 2 + 
        0.5 * interact_data$x1 +
        ifelse(interact_data$group == "Treatment", 1, 0) +
        ifelse(interact_data$group == "Treatment", 0.8, 0) * interact_data$x1 +
        rnorm(n, sd = 1)
      interact_data$group2 <- factor(sample(c("Young", "Old"), n, replace = TRUE))
      interact_data$y_ff <- 3 +
        ifelse(interact_data$group == "Treatment", 1, 0) +
        ifelse(interact_data$group2 == "Old", 0.5, 0) +
        ifelse(interact_data$group == "Treatment" & interact_data$group2 == "Old", 2, 0) +
        rnorm(n, sd = 1)
      interact_data$y_3way <- 1 +
        0.3 * interact_data$x1 +
        ifelse(interact_data$group == "Treatment", 0.5, 0) +
        ifelse(interact_data$group2 == "Old", 0.4, 0) +
        0.2 * interact_data$x1 * ifelse(interact_data$group == "Treatment", 1, 0) * 
          ifelse(interact_data$group2 == "Old", 1, 0) +
        rnorm(n, sd = 1)

# 3.1 - Numeric * Numeric interaction (uncorrelated)
  lm2(y ~ x1 * x2, data = interact_data)

# 3.2 - Numeric * Numeric interaction (CORRELATED - expect X flag)
  cor(interact_data$x1, interact_data$x2_corr)  # Check correlation
  lm2(y_corr ~ x1 * x2_corr, data = interact_data)

# 3.3 - Numeric * Factor interaction
  lm2(y_nf ~ x1 * group, data = interact_data)

# 3.4 - Factor * Factor interaction
  lm2(y_ff ~ group * group2, data = interact_data)

# 3.5 - Three-way interaction
  lm2(y_3way ~ x1 * group * group2, data = interact_data)


# ============================================================================
# SECTION 4: CLUSTERED DATA
# ============================================================================
        
  
  #Data
        n_clusters <- 30
        n_per_cluster <- 20
        n_total <- n_clusters * n_per_cluster
        
        cluster_data <- data.frame(
          cluster_id = rep(1:n_clusters, each = n_per_cluster),
          x = rnorm(n_total)
        )
        cluster_effects <- rnorm(n_clusters, sd = 2)
        cluster_data$cluster_effect <- cluster_effects[cluster_data$cluster_id]
        cluster_data$y <- 1 + 
          0.5 * cluster_data$x + 
          cluster_data$cluster_effect + 
          rnorm(n_total, sd = 1)
        cluster_data$treatment <- factor(rep(c("Control", "Treatment"), length.out = n_total))
        cluster_data$y2 <- 1 + 
          0.5 * cluster_data$x + 
          ifelse(cluster_data$treatment == "Treatment", 1, 0) +
          0.3 * cluster_data$x * ifelse(cluster_data$treatment == "Treatment", 1, 0) +
          cluster_data$cluster_effect + 
          rnorm(n_total, sd = 1)
        
# 4.1 - Ignoring clusters (incorrect)
  lm2(y ~ x, data = cluster_data)

# 4.2 - Clustered standard errors (correct)
  lm2(y ~ x, data = cluster_data, clusters = cluster_id)

# 4.3 - Clustered with interaction
  lm2(y2 ~ x * treatment, data = cluster_data, clusters = cluster_id)


# ============================================================================
# SECTION 5: FIXED EFFECTS / PANEL DATA
# ============================================================================

n_firms <- 50
n_years <- 8
n_panel <- n_firms * n_years

panel_data <- data.frame(
  firm_id = factor(rep(1:n_firms, each = n_years)),
  year = factor(rep(2015:2022, times = n_firms)),
  x = rnorm(n_panel)
)
firm_effects <- rnorm(n_firms, sd = 3)
year_effects <- seq(-1, 1, length.out = n_years)
panel_data$firm_effect <- firm_effects[as.numeric(panel_data$firm_id)]
panel_data$year_effect <- year_effects[as.numeric(panel_data$year)]
panel_data$y <- 5 + 
  0.8 * panel_data$x + 
  panel_data$firm_effect + 
  panel_data$year_effect + 
  rnorm(n_panel, sd = 1)

# 5.1 - No fixed effects (omitted variable bias)
  lm2(y ~ x, data = panel_data)

# 5.2 - Firm fixed effects only
  lm2(y ~ x, data = panel_data, fixed_effects = ~ firm_id)

# 5.3 - Two-way fixed effects (firm + year)
  lm2(y ~ x, data = panel_data, fixed_effects = ~ firm_id + year)

  summary(estimatr::lm_robust(y ~ x, data = panel_data, fixed_effects = ~ firm_id + year))
  
# 5.4 - Fixed effects + clustered SE
  lm2(y ~ x, data = panel_data, fixed_effects = ~ firm_id, clusters = firm_id)


# ============================================================================
# SECTION 6: MISSING DATA PATTERNS
# ============================================================================

missing_data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
missing_data$y <- 1 + 0.5 * missing_data$x1 + 0.3 * missing_data$x2 + 0.2 * missing_data$x3 + 
  rnorm(n, sd = 1)
missing_data$x1[sample(n, 5)] <- NA   # 5 missing
missing_data$x2[sample(n, 20)] <- NA  # 20 missing
missing_data$x3[sample(n, 40)] <- NA  # 40 missing
missing_data$y[sample(n, 10)] <- NA   # 10 missing in outcome

# 6.1 - Multiple predictors with different missingness rates
  lm2(y ~ x1 + x2 + x3, data = missing_data)


# ============================================================================
# SECTION 7: HETEROSKEDASTICITY & OUTLIERS (RED FLAG TRIGGERS)
# ============================================================================

hetero_data <- data.frame(x = runif(n, 1, 10))
hetero_data$y <- 2 + 0.5 * hetero_data$x + rnorm(n, sd = 0.5 * hetero_data$x)

outlier_data <- data.frame(x = rnorm(n))
outlier_data$y <- 1 + 0.5 * outlier_data$x + rnorm(n, sd = 1)
outlier_data$y[which.max(outlier_data$x)] <- 50  # Extreme outlier

leverage_data <- data.frame(x = rnorm(n))
leverage_data$y <- 1 + 0.5 * leverage_data$x + rnorm(n, sd = 1)
leverage_data$x[1] <- 20  # High leverage point

multi_outlier <- data.frame(x = rnorm(n))
multi_outlier$y <- 1 + 0.5 * multi_outlier$x + rnorm(n, sd = 1)
multi_outlier$y[1:5] <- multi_outlier$y[1:5] + 10  # 5 outliers

# 7.1 - Heteroskedasticity (variance increases with x)
  lm2(y ~ x, data = hetero_data)

# 7.2 - Outlier in y (single extreme value)
  lm2(y ~ x, data = outlier_data)

# 7.3 - High leverage point (outlier in x)
  lm2(y ~ x, data = leverage_data)

# 7.4 - Multiple outliers in y
  lm2(y ~ x, data = multi_outlier)


# ============================================================================
# SECTION 8: NONLINEAR RELATIONSHIPS
# ============================================================================

nonlin_data <- data.frame(x = runif(n, -3, 3))
nonlin_data$y_quad <- nonlin_data$x^2 + rnorm(n, sd = 1)
nonlin_data$y_exp <- exp(0.5 * nonlin_data$x) + rnorm(n, sd = 1)
nonlin_data$x_pos <- nonlin_data$x - min(nonlin_data$x) + 1  # Ensure positive
nonlin_data$y_log <- 2 * log(nonlin_data$x_pos) + rnorm(n, sd = 0.5)

# 8.1 - Quadratic relationship (misspecified as linear)
  lm2(y_quad ~ x, data = nonlin_data)

# 8.1b - Quadratic relationship (correct polynomial specification)
  lm2(y_quad ~ x + I(x^2), data = nonlin_data)

# 8.2 - Exponential relationship (misspecified as linear)
  lm2(y_exp ~ x, data = nonlin_data)

# 8.3 - Log relationship (misspecified as linear)
  lm2(y_log ~ x_pos, data = nonlin_data)


# ============================================================================
# SECTION 9: DIFFERENT SE TYPES
# ============================================================================

se_data <- data.frame(x = runif(n, 1, 10))
se_data$y <- 2 + 0.5 * se_data$x + rnorm(n, sd = 0.5 * se_data$x)  # Heteroskedastic

# 9.1 - HC0 (no small sample correction)
  lm2(y ~ x, data = se_data, se_type = "HC0")

# 9.2 - HC1 (Stata's default)
  lm2(y ~ x, data = se_data, se_type = "HC1")

# 9.3 - HC2 (estimatr's default)
  lm2(y ~ x, data = se_data, se_type = "HC2")

# 9.4 - HC3 (lm2's default - more conservative)
  lm2(y ~ x, data = se_data, se_type = "HC3")


# ============================================================================
# SECTION 10: EDGE CASES
# ============================================================================

small_data <- data.frame(x = rnorm(15), y = rnorm(15))
small_data$y <- 1 + 0.5 * small_data$x + rnorm(15, sd = 1)

many_pred <- data.frame(matrix(rnorm(n * 20), ncol = 20))
names(many_pred) <- paste0("x", 1:20)
many_pred$y <- 1 + rowSums(many_pred[, 1:5] * 0.3) + rnorm(n, sd = 1)

multicol <- data.frame(x1 = rnorm(n))
multicol$x2 <- multicol$x1 + rnorm(n, sd = 0.01)  # Almost identical to x1
multicol$y <- 1 + multicol$x1 + rnorm(n, sd = 1)

binary_data <- data.frame(x = rnorm(n))
binary_data$y <- rbinom(n, 1, plogis(binary_data$x))

many_levels <- data.frame(
  x = rnorm(n),
  state = factor(sample(state.abb[1:20], n, replace = TRUE))
)
many_levels$y <- 1 + 0.5 * many_levels$x + rnorm(n)

# 10.1 - Small sample (n=15)
  lm2(y ~ x, data = small_data)

# 10.2 - Many predictors (20 predictors, n=200)
  lm2(y ~ ., data = many_pred)

# 10.3 - Near-perfect multicollinearity
  tryCatch(
    lm2(y ~ x1 + x2, data = multicol),
    error = function(e) cat("Error:", e$message, "\n")
  )

# 10.4 - Binary outcome (linear probability model)
  lm2(y ~ x, data = binary_data)

# 10.5 - Factor with many levels
  lm2(y ~ x + state, data = many_levels)


# ============================================================================
# SECTION 11: REALISTIC SCENARIOS
# ============================================================================

wage_data <- data.frame(
  education = sample(12:20, n, replace = TRUE),
  experience = pmax(0, rnorm(n, mean = 15, sd = 8)),
  female = factor(sample(c(0, 1), n, replace = TRUE), labels = c("Male", "Female"))
)
wage_data$wage <- exp(
  2 + 
  0.08 * wage_data$education + 
  0.03 * wage_data$experience - 
  0.001 * wage_data$experience^2 +
  ifelse(wage_data$female == "Female", -0.15, 0) +
  rnorm(n, sd = 0.3)
)

rct_data <- data.frame(
  treatment = factor(sample(c("Control", "Treatment"), n, replace = TRUE)),
  age = rnorm(n, mean = 45, sd = 10),
  baseline = rnorm(n, mean = 50, sd = 15)
)
rct_data$outcome <- 50 + 
  ifelse(rct_data$treatment == "Treatment", 5, 0) +
  0.3 * rct_data$age +
  0.5 * rct_data$baseline +
  rnorm(n, sd = 10)

n_did <- 400
did_data <- data.frame(
  treated = factor(rep(c(0, 1), each = n_did/2), labels = c("Control", "Treated")),
  post = factor(rep(c(0, 0, 1, 1), each = n_did/4), labels = c("Pre", "Post"))
)
did_data$y <- 10 + 
  ifelse(did_data$treated == "Treated", 2, 0) +
  ifelse(did_data$post == "Post", 1, 0) +
  ifelse(did_data$treated == "Treated" & did_data$post == "Post", 3, 0) +
  rnorm(n_did, sd = 2)

# 11.1 - Wage regression (education, experience, gender)
  lm2(log(wage) ~ education + experience + female, data = wage_data)

# 11.1b - Wage with education * gender interaction
  lm2(log(wage) ~ education * female + experience, data = wage_data)

# 11.2 - RCT with covariates
  lm2(outcome ~ treatment + age + baseline, data = rct_data)

# 11.3 - Difference-in-differences
  lm2(y ~ treated * post, data = did_data)


# ============================================================================
# SECTION 12: COMBINING FEATURES
# ============================================================================

complex_data <- data.frame(
  cluster = rep(1:25, each = 12),
  x1 = rnorm(300),
  x2 = rnorm(300),
  group = factor(sample(c("A", "B", "C"), 300, replace = TRUE))
)
cluster_eff <- rnorm(25, sd = 2)
complex_data$cluster_effect <- cluster_eff[complex_data$cluster]
complex_data$y <- 5 +
  0.5 * complex_data$x1 +
  0.3 * complex_data$x2 +
  0.2 * complex_data$x1 * complex_data$x2 +
  ifelse(complex_data$group == "B", 1, 0) +
  ifelse(complex_data$group == "C", 2, 0) +
  complex_data$cluster_effect +
  rnorm(300, sd = 1)
complex_data$x1[sample(300, 15)] <- NA
complex_data$x2[sample(300, 10)] <- NA

# 12.1 - Complex: interactions + factors + clusters + missing
  lm2(y ~ x1 * x2 + group, data = complex_data, clusters = cluster)


# ============================================================================
# AVAILABLE DATA OBJECTS
# ============================================================================
# After running, these data frames are available for experimentation:
#   - simple_data: basic continuous predictors
#   - factor_data: factor/categorical predictors
#   - interact_data: various interaction types
#   - cluster_data: clustered observations
#   - panel_data: panel/longitudinal data
#   - missing_data: various missingness patterns
#   - hetero_data, outlier_data, leverage_data, multi_outlier: diagnostic scenarios
#   - nonlin_data: nonlinear relationships
#   - se_data: for comparing SE types
#   - small_data, many_pred, multicol, binary_data, many_levels: edge cases
#   - wage_data, rct_data, did_data: realistic scenarios
#   - complex_data: combined features
