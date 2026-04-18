#plot_means_001
test_that("plot_means runs with formula + data and returns desc_var object", {
  df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))

  result <- plot_means(y ~ group, data = df, save.as = NULL)

  expect_true(is.list(result))
  expect_true(is.data.frame(result$means))
})

#plot_means_002
test_that("plot_means works with multiple grouping variables (y ~ x1 + x2)", {
  df <- data.frame(
    y = rnorm(200),
    x1 = rep(c("A", "B"), 100),
    x2 = rep(c("X", "Y"), each = 100)
  )

  result <- plot_means(y ~ x1 + x2, data = df, save.as = NULL)

  expect_true(is.list(result))
  expect_true(is.data.frame(result$means))
})

#plot_means_003
test_that("plot_means order reorders groups for single grouping variable", {
  df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))

  result_default <- plot_means(y ~ group, data = df, save.as = NULL)
  expect_true(is.list(result_default))

  result_reversed <- plot_means(y ~ group, data = df, order = -1, save.as = NULL)
  expect_true(is.list(result_reversed))

  result_custom <- plot_means(y ~ group, data = df, order = c("B", "A"), save.as = NULL)
  expect_true(is.list(result_custom))
})

#plot_means_004
test_that("plot_means works with three grouping variables and missing combinations", {
  set.seed(1)
  df <- data.frame(
    y = rnorm(96),
    x1 = rep(c("A", "B"), 48),
    x2 = rep(rep(c("X", "Y"), each = 24), 2),
    x3 = rep(c("M", "N"), each = 48)
  )

  # Remove one x1 level from one (x2,x3) block to create a missing combination
  df <- df[!(df$x1 == "B" & df$x2 == "Y" & df$x3 == "N"), , drop = FALSE]

  result <- plot_means(y ~ x1 + x2 + x3, data = df, save.as = NULL)
  expect_true(is.list(result))
})

#plot_means_005
test_that("plot_means tests=auto works for scenario 1 (binary x1 only)", {
  df <- data.frame(y = rnorm(60), x1 = rep(c("A", "B"), each = 30))
  expect_error(plot_means(y ~ x1, data = df, tests = "auto", save.as = NULL), NA)
})

#plot_means_006
test_that("plot_means tests=auto works for scenario 2 (binary x1 and x2)", {
  df <- data.frame(
    y = rnorm(120),
    x1 = rep(c("A", "B"), 60),
    x2 = rep(rep(c("X", "Y"), each = 30), 2)
  )
  expect_error(plot_means(y ~ x1 + x2, data = df, tests = "auto", save.as = NULL), NA)
})

#plot_means_007
test_that("plot_means tests=auto works for scenario 3 (binary x1, x2 has >2 levels)", {
  df <- expand.grid(
    x1 = c("A", "B"),
    x2 = c("X", "Y", "Z"),
    rep = seq_len(30),
    stringsAsFactors = FALSE
  )
  df$y <- rnorm(nrow(df))
  
  expect_error(plot_means(y ~ x1 + x2, data = df, tests = "auto", save.as = NULL), NA)
})

#plot_means_008
test_that("plot_means save.as saves png", {
  df <- data.frame(y = rnorm(80), x1 = rep(c("A", "B"), each = 40))
  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)
  
  grDevices::png(tempfile(fileext = ".png"), width = 800, height = 500, res = 100)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  result <- plot_means(y ~ x1, data = df, save.as = out)
  expect_true(is.list(result))
  
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})

#plot_means_009
test_that("plot_means means + per-cell CIs match t.test() when cluster=NULL", {
  set.seed(123)
  df <- data.frame(
    y = rnorm(120),
    x1 = rep(c("A", "B", "C"), each = 40),
    stringsAsFactors = FALSE
  )
  ci_level <- 90
  
  pm <- plot_means(y ~ x1, data = df, tests = "none", ci.level = ci_level, save.as = NULL, quiet = TRUE)
  expect_true(is.data.frame(pm$means))
  
  # Means/sd/n should match base computations
  m <- tapply(df$y, df$x1, mean)
  s <- tapply(df$y, df$x1, sd)
  n <- tapply(df$y, df$x1, function(z) sum(is.finite(z)))
  
  idx <- match(pm$means$x1, names(m))
  expect_false(anyNA(idx))
  
  # desc_var()/plot_means may round display columns; allow small tolerance.
  expect_true(max(abs(unname(pm$means$mean) - as.numeric(m[idx]))) < 1e-3)
  expect_true(max(abs(unname(pm$means$sd) - as.numeric(s[idx]))) < 1e-3)
  expect_true(max(abs(unname(pm$means$n.total) - as.numeric(n[idx]))) < 1e-12)
  
  # CI should match one-sample t.test(y ~ 1) within each cell
  for (g in pm$means$x1) {
    yy <- df$y[df$x1 == g]
    tt <- stats::t.test(yy, conf.level = ci_level / 100)
    pm_row <- pm$means[pm$means$x1 == g, , drop = FALSE]
    expect_equal(pm_row$ciL, as.numeric(tt$conf.int[1]), tolerance = 1e-10)
    expect_equal(pm_row$ciH, as.numeric(tt$conf.int[2]), tolerance = 1e-10)
  }
})

#plot_means_010
test_that("plot_means tests=auto (scenario 1) matches Welch t.test()", {
  set.seed(1)
  df <- data.frame(y = rnorm(80), x1 = rep(c("A", "B"), each = 40))
  
  pm <- plot_means(y ~ x1, data = df, tests = "auto", ci.level = 95, save.as = NULL, quiet = TRUE)
  expect_true(is.data.frame(pm$means))
  expect_true(is.data.frame(pm$tests))
  expect_gt(nrow(pm$tests), 0)
  
  # First (and only) comparison should correspond to Welch two-sample t-test
  tt <- stats::t.test(y ~ x1, data = df, var.equal = FALSE)
  row <- pm$tests[1, , drop = FALSE]
  
  expect_true(is.finite(row$t.value))
  expect_true(is.finite(row$p.value))
  expect_equal(as.numeric(row$p.value), as.numeric(tt$p.value), tolerance = 1e-10)
  expect_equal(as.numeric(row$df), as.numeric(tt$parameter[[1]]), tolerance = 1e-10)
  # Our diff uses mean2 - mean1, and our t.value is aligned to that sign.
  expect_equal(as.numeric(row$diff), as.numeric(row$mean2 - row$mean1), tolerance = 1e-12)
})

#plot_means_011
test_that("plot_means 2x2 interaction p-value matches estimatr::lm_robust(HC3)", {
  skip_if_not_installed("estimatr")
  
  set.seed(2)
  df <- data.frame(
    y = rnorm(400),
    x1 = rep(c("A", "B"), 200),
    x2 = rep(rep(c("X", "Y"), each = 100), 2),
    stringsAsFactors = FALSE
  )
  
  pm <- plot_means(y ~ x1 + x2, data = df, tests = "auto", ci.level = 95, save.as = NULL, quiet = TRUE)
  expect_true(is.data.frame(pm$tests))
  
  int_row <- pm$tests[grepl("^interaction\\(", as.character(pm$tests$group1)), , drop = FALSE]
  expect_equal(nrow(int_row), 1)
  
  fit <- estimatr::lm_robust(y ~ x1 * x2, data = df, se_type = "HC3")
  sm <- summary(fit)
  coef_tab <- sm$coefficients
  
  int_term <- rownames(coef_tab)[grepl(":", rownames(coef_tab))]
  expect_equal(length(int_term), 1)
  
  expect_equal(as.numeric(int_row$p.value), as.numeric(coef_tab[int_term, "Pr(>|t|)"]), tolerance = 1e-10)
  expect_equal(as.numeric(int_row$t.value), as.numeric(coef_tab[int_term, "t value"]), tolerance = 1e-10)
  expect_equal(as.numeric(int_row$df), as.numeric(sm$df.residual), tolerance = 1e-10)
})

#plot_means_012
test_that("plot_means clustered CI uses regression-based intervals governed by ci.level", {
  skip_if_not_installed("estimatr")
  
  set.seed(3)
  n_cl <- 40
  n_per <- 20
  cl <- rep(seq_len(n_cl), each = n_per)
  df <- data.frame(
    y = rnorm(n_cl * n_per),
    x1 = rep(c("A", "B"), length.out = n_cl * n_per),
    cl = cl,
    stringsAsFactors = FALSE
  )
  
  ci_level <- 90
  pm <- plot_means(y ~ x1, data = df, cluster = df$cl, tests = "none", ci.level = ci_level, save.as = NULL, quiet = TRUE)
  
  # Reconstruct the same cell_key as plot_means_compute() uses (x1|All|All)
  df2 <- df
  df2$cell_key <- paste(as.character(df2$x1), "All", "All", sep = "|")
  df2$cell_factor <- factor(df2$cell_key)
  
  fit <- estimatr::lm_robust(y ~ 0 + cell_factor, data = df2, clusters = df2$cl, se_type = "CR2")
  beta <- stats::coef(fit)
  V <- stats::vcov(fit)
  df_fit <- fit$df
  if (length(df_fit) != 1) df_fit <- suppressWarnings(min(df_fit, na.rm = TRUE))
  if (!is.finite(df_fit) || df_fit <= 0) df_fit <- 1
  alpha <- 1 - ci_level / 100
  tcrit <- stats::qt(1 - alpha / 2, df = df_fit)
  
  lev <- levels(df2$cell_factor)
  newdata <- data.frame(cell_factor = factor(lev, levels = lev))
  X <- stats::model.matrix(~ 0 + cell_factor, data = newdata)
  
  b_idx <- match(colnames(X), names(beta))
  X <- X[, !is.na(b_idx), drop = FALSE]
  b_idx <- b_idx[!is.na(b_idx)]
  beta_use <- beta[b_idx]
  V_use <- V[b_idx, b_idx, drop = FALSE]
  
  fit_hat <- as.numeric(X %*% beta_use)
  se_hat <- sqrt(pmax(0, diag(X %*% V_use %*% t(X))))
  lwr <- fit_hat - tcrit * se_hat
  upr <- fit_hat + tcrit * se_hat
  
  ci_df <- data.frame(cell_key = lev, lwr = lwr, upr = upr, stringsAsFactors = FALSE)
  
  for (i in seq_len(nrow(pm$means))) {
    key <- paste(as.character(pm$means$x1[i]), "All", "All", sep = "|")
    j <- match(key, ci_df$cell_key)
    expect_false(is.na(j))
    expect_equal(pm$means$ciL[i], ci_df$lwr[j], tolerance = 1e-3)
    expect_equal(pm$means$ciH[i], ci_df$upr[j], tolerance = 1e-3)
  }
})

