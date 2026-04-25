# Backend equivalence battery: compare statuser outputs to underlying engines.

#t.test2_027
test_that("backend: t.test2 matches stats::t.test across core scenarios", {
  set.seed(101)
  
  x <- rnorm(60, mean = 0.2)
  y <- rnorm(55, mean = 0.5)
  
  scenarios <- list(
    list(label = "two_sample_welch", ref = list(x = x, y = y), args = list()),
    list(label = "two_sample_student", ref = list(x = x, y = y), args = list(var.equal = TRUE)),
    list(label = "paired", ref = list(x = x[1:50], y = y[1:50]), args = list(paired = TRUE)),
    list(label = "one_sample", ref = list(x = x, y = NULL), args = list(mu = 0.1)),
    list(label = "alt_less", ref = list(x = x, y = y), args = list(alternative = "less")),
    list(label = "conf_99", ref = list(x = x, y = y), args = list(conf.level = 0.99))
  )
  
  for (sc in scenarios) {
    if (is.null(sc$ref$y)) {
      tt_ref <- do.call(stats::t.test, c(list(x = sc$ref$x), sc$args))
      tt2 <- do.call(statuser::t.test2, c(list(sc$ref$x), sc$args))
    } else {
      tt_ref <- do.call(stats::t.test, c(list(x = sc$ref$x, y = sc$ref$y), sc$args))
      tt2 <- do.call(statuser::t.test2, c(list(sc$ref$x, sc$ref$y), sc$args))
    }
    
    be_expect_equal_num(tt2$p.value, tt_ref$p.value, tol = 1e-10)
    be_expect_equal_num(tt2$t, as.numeric(tt_ref$statistic), tol = 1e-10)
    be_expect_equal_num(tt2$df, as.numeric(tt_ref$parameter), tol = 1e-10)
    be_expect_equal_num(tt2$ci.L, tt_ref$conf.int[1], tol = 1e-10)
    be_expect_equal_num(tt2$ci.H, tt_ref$conf.int[2], tol = 1e-10)
  }
})

#t.test2_028
test_that("backend: t.test2 formula interface matches stats::t.test", {
  set.seed(102)
  
  n <- 120
  g <- factor(rep(c("A", "B"), each = n / 2), levels = c("A", "B"))
  y <- rnorm(n, mean = ifelse(g == "B", 0.3, 0))
  df <- data.frame(y = y, g = g)
  
  tt_ref <- stats::t.test(y ~ g, data = df)
  tt2 <- statuser::t.test2(y ~ g, data = df)
  
  be_expect_equal_num(tt2$p.value, tt_ref$p.value, tol = 1e-10)
  be_expect_equal_num(tt2$t, as.numeric(tt_ref$statistic), tol = 1e-10)
  be_expect_equal_num(tt2$df, as.numeric(tt_ref$parameter), tol = 1e-10)
  be_expect_equal_num(tt2$ci.L, tt_ref$conf.int[1], tol = 1e-10)
  be_expect_equal_num(tt2$ci.H, tt_ref$conf.int[2], tol = 1e-10)
})

#lm2_051
test_that("backend: lm2 matches estimatr::lm_robust coefficients and inference", {
  testthat::skip_if_not_installed("estimatr")
  
  set.seed(201)
  n <- 300
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + 0.5 * x1 - 0.2 * x2 + rnorm(n)
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  
  m2 <- statuser::lm2(y ~ x1 + x2, data = df, notes = FALSE)
  ref <- estimatr::lm_robust(y ~ x1 + x2, data = df, se_type = "HC3")
  
  be_expect_equal_num(unname(m2$coefficients), unname(ref$coefficients), tol = 1e-10)
  be_expect_equal_num(unname(m2$std.error), unname(ref$std.error), tol = 1e-10)
  be_expect_equal_num(unname(m2$statistic), unname(ref$statistic), tol = 1e-10)
  be_expect_equal_num(unname(m2$p.value), unname(ref$p.value), tol = 1e-10)
  be_expect_equal_num(unname(m2$df), unname(ref$df), tol = 1e-10)
})

#interprobe_012
test_that("backend: interprobe JN output matches marginaleffects slopes and regions of significance", {
  testthat::skip_if_not_installed("marginaleffects")
  testthat::skip_if_not_installed("mgcv")
  
  set.seed(301)
  n <- 500
  x <- rnorm(n)
  z <- rnorm(n)
  y <- x * z + rnorm(n)
  df <- data.frame(y = y, x = x, z = z)
  
  m <- stats::lm(y ~ x * z, data = df)
  
  spotlights <- c(-1, 0, 1)
  probe_bins <- 40
  zs <- seq(min(df$z), max(df$z), length.out = probe_bins)
  
  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  res <- statuser::interprobe(
    model = m,
    x = "x",
    z = "z",
    draw = "jn",
    histogram = FALSE,
    quiet = TRUE,
    spotlights = spotlights,
    probe.bins = probe_bins
  )
  
  jn_ref_list <- lapply(spotlights, function(xj) {
    nd <- data.frame(x = xj, z = zs)
    marginaleffects::slopes(m, newdata = nd, var = "x")
  })
  jn_ref <- do.call(rbind, jn_ref_list)
  jn_ref$x <- rep(spotlights, each = probe_bins)
  
  jn_stat <- res$johnson.neyman
  
  jn_ref <- jn_ref[order(jn_ref$x, jn_ref$z), ]
  jn_stat <- jn_stat[order(jn_stat$x, jn_stat$z), ]
  
  testthat::expect_equal(nrow(jn_ref), nrow(jn_stat))
  
  be_expect_equal_num(jn_stat$marginal.effect, jn_ref$estimate, tol = 1e-8)
  be_expect_equal_num(jn_stat$conf.low, jn_ref$conf.low, tol = 1e-8)
  be_expect_equal_num(jn_stat$conf.high, jn_ref$conf.high, tol = 1e-8)
  
  sig_ref <- be_sig_from_ci(jn_ref$conf.low, jn_ref$conf.high)
  sig_stat <- be_sig_from_ci(jn_stat$conf.low, jn_stat$conf.high)
  testthat::expect_identical(sig_stat, sig_ref)
})

#interprobe_013
test_that("backend: interprobe internal interaction test p-values match returned testing models (binary x)", {
  testthat::skip_if_not_installed("marginaleffects")
  testthat::skip_if_not_installed("mgcv")
  
  set.seed(302)
  n <- 240
  z <- rnorm(n)
  x <- rep(c(0, 1), each = n / 2)
  y <- 0.2 + 0.5 * x + 0.8 * z + 0.9 * x * z + rnorm(n)
  
  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  out <- capture.output(
    res <- statuser::interprobe(
      x, z, y,
      quiet = FALSE,
      draw = "jn",
      histogram = FALSE,
      spotlights = c(-1, 0, 1),
      probe.bins = 30
    )
  )
  
  testthat::expect_true(is.list(res))
  testthat::expect_true("gam_results_testing" %in% names(res))
  
  gam_p <- NA_real_
  sm <- summary(res$gam_results_testing)
  st <- sm$s.table
  rn <- rownames(st)
  if (!is.null(rn)) {
    hits <- which(
      grepl("ti(", rn, fixed = TRUE) &
        grepl("x", rn, fixed = TRUE) &
        grepl("z", rn, fixed = TRUE)
    )
    if (length(hits) > 0) {
      gam_p <- as.numeric(st[hits[1], "p-value"])
    }
  }
  
  gam_line <- out[grepl("^\\s*GAM:", out)]
  gam_line <- if (length(gam_line) > 0) gam_line[1] else NA_character_
  gam_p_reported <- be_parse_p_from_apa_line(gam_line)
  
  # If output was of the form p < .001, we parse as 0.001 and only check the direction.
  if (is.finite(gam_p_reported) && identical(gam_p_reported, 0.001)) {
    testthat::expect_true(is.finite(gam_p))
    testthat::expect_true(gam_p < 0.001)
  } else {
    be_expect_equal_num(gam_p_reported, gam_p, tol = 1e-10)
  }
  
  # Linear interaction p-value: use lm2_results if available, otherwise compute from lm on the same data.
  lin_line <- out[grepl("^\\s*linear model:", out)]
  lin_line <- if (length(lin_line) > 0) lin_line[1] else NA_character_
  lin_p_reported <- be_parse_p_from_apa_line(lin_line)
  
  lin_p_ref <- NA_real_
  if ("lm2_results" %in% names(res) && !is.null(res$lm2_results)) {
    tbl <- attr(res$lm2_results, "statuser_table")
    if (!is.null(tbl) && "term" %in% names(tbl) && "p.value" %in% names(tbl)) {
      idx2 <- match(c("x:z", "z:x"), tbl$term)
      idx2 <- idx2[!is.na(idx2)]
      if (length(idx2) > 0) lin_p_ref <- as.numeric(tbl$p.value[idx2[1]])
    }
  }
  if (!is.finite(lin_p_ref)) {
    df <- data.frame(y = y, x = x, z = z)
    m <- stats::lm(y ~ x * z, data = df)
    lin_p_ref <- be_extract_linear_interaction_p(m, "x", "z")
  }
  
  if (is.finite(lin_p_reported) && identical(lin_p_reported, 0.001)) {
    testthat::expect_true(is.finite(lin_p_ref))
    testthat::expect_true(lin_p_ref < 0.001)
  } else {
    be_expect_equal_num(lin_p_reported, lin_p_ref, tol = 1e-10)
  }
})

