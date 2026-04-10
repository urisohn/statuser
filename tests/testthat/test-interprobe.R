# Tests for interprobe()

test_that("interprobe works with lm() model input and returns expected structure", {
  skip_if_not_installed("marginaleffects")

  set.seed(111)
  n <- 600
  x1 <- rnorm(n)
  z1 <- rnorm(n, mean = 10, sd = 2)
  y.raw <- x1 * z1
  e <- rnorm(n, sd = sd(y.raw))
  y1 <- y.raw + e

  df <- data.frame(x1, y1, z1)
  lm1 <- lm(y1 ~ x1 * z1, data = df)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    model = lm1,
    x = "x1",
    z = "z1",
    draw = "jn",
    histogram = FALSE,
    quiet = TRUE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true(all(c("simple.slopes", "johnson.neyman", "frequencies") %in% names(res)))
  expect_true(is.data.frame(res$johnson.neyman))
  expect_true(nrow(res$johnson.neyman) > 0)

  # For a linear model, dy/dx does not depend on x itself (only on z due to interaction),
  # so the marginal effects curves should be (nearly) identical across the x-spotlights.
  jn <- res$johnson.neyman
  expect_true("marginal.effect" %in% names(jn))
  expect_true("x1" %in% names(jn))
  expect_true("z1" %in% names(jn))

  eff_by_x <- split(jn$marginal.effect, jn$x1)
  expect_true(length(eff_by_x) == 3)
  expect_equal(unname(eff_by_x[[1]]), unname(eff_by_x[[2]]), tolerance = 1e-8)
  expect_equal(unname(eff_by_x[[2]]), unname(eff_by_x[[3]]), tolerance = 1e-8)
})

test_that("interprobe returns fitted model when estimating internally", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("estimatr")

  set.seed(333)
  n <- 250
  x <- rnorm(n)
  z <- rnorm(n)
  y <- x * z + rnorm(n)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x, z, y,
    quiet = TRUE,
    draw = "jn",
    histogram = FALSE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true("gam_results" %in% names(res))
  expect_s3_class(res$gam_results, "gam")
  expect_true("lm2_results" %in% names(res))
  expect_s3_class(res$lm2_results, "lm2")
})

test_that("quiet=TRUE suppresses interaction APA printing", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")

  set.seed(444)
  n <- 200
  x <- rnorm(n)
  z <- rnorm(n)
  y <- x * z + rnorm(n)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  out <- capture.output(
    interprobe(
      x, z, y,
      quiet = TRUE,
      draw = "jn",
      histogram = FALSE,
      spotlights = c(-1, 0, 1),
      probe.bins = 30
    )
  )

  expect_false(any(grepl("p-value for the interaction", out, fixed = TRUE)))
})

test_that("interprobe works with lm2() model input", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  set.seed(111)
  n <- 600
  x1 <- rnorm(n)
  z1 <- rnorm(n, mean = 10, sd = 2)
  y.raw <- x1 * z1
  e <- rnorm(n, sd = sd(y.raw))
  y1 <- y.raw + e

  df <- data.frame(x1, y1, z1)
  lm2_1 <- lm2(y1 ~ x1 * z1, data = df, notes = FALSE)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    model = lm2_1,
    x = "x1",
    z = "z1",
    draw = "jn",
    histogram = FALSE,
    quiet = TRUE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true(all(c("simple.slopes", "johnson.neyman", "frequencies") %in% names(res)))
  expect_true(nrow(res$johnson.neyman) > 0)
  jn <- res$johnson.neyman
  eff_by_x <- split(jn$marginal.effect, jn$x1)
  expect_true(length(eff_by_x) == 3)
  expect_equal(unname(eff_by_x[[1]]), unname(eff_by_x[[2]]), tolerance = 1e-8)
  expect_equal(unname(eff_by_x[[2]]), unname(eff_by_x[[3]]), tolerance = 1e-8)
})

test_that("interprobe with data fits lm2 when model = linear or lm sentinel", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  set.seed(111)
  n <- 600
  x1 <- rnorm(n)
  z1 <- rnorm(n, mean = 10, sd = 2)
  y.raw <- x1 * z1
  e <- rnorm(n, sd = sd(y.raw))
  y1 <- y.raw + e

  df <- data.frame(x1, y1, z1)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res_linear <- interprobe(
    data = df,
    x = "x1",
    z = "z1",
    y = "y1",
    model = "linear",
    draw = "jn",
    histogram = FALSE,
    quiet = TRUE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res_linear))
  expect_true(nrow(res_linear$johnson.neyman) > 0)
  jn <- res_linear$johnson.neyman
  eff_by_x <- split(jn$marginal.effect, jn$x1)
  expect_true(length(eff_by_x) == 3)
  expect_equal(unname(eff_by_x[[1]]), unname(eff_by_x[[2]]), tolerance = 1e-8)
  expect_equal(unname(eff_by_x[[2]]), unname(eff_by_x[[3]]), tolerance = 1e-8)

  res_lm_sentinel <- interprobe(
    data = df,
    x = "x1",
    z = "z1",
    y = "y1",
    model = lm,
    draw = "jn",
    histogram = FALSE,
    quiet = TRUE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_equal(nrow(res_lm_sentinel$johnson.neyman), nrow(res_linear$johnson.neyman))
})

test_that("interprobe(x, z, y vectors) works with model = linear", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  set.seed(111)
  n <- 600
  x <- rnorm(n)
  z <- rnorm(n, mean = 10, sd = 2)
  y.raw <- x * z
  e <- rnorm(n, sd = sd(y.raw))
  y <- y.raw + e

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x, z, y,
    model = "linear",
    quiet = TRUE,
    draw = "jn",
    histogram = FALSE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

test_that("bare model = linear is keyword when linear not in calling env", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  # Child of .GlobalEnv: no `linear` binding here; interprobe's parent.frame() is this env.
  res <- local(
    {
      set.seed(111)
      n <- 200
      x <- rnorm(n)
      z <- rnorm(n)
      y <- x * z + rnorm(n)
      grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
      on.exit(grDevices::dev.off(), add = TRUE)
      interprobe(
        x, z, y,
        model = linear,
        quiet = TRUE,
        draw = "jn",
        histogram = FALSE,
        spotlights = c(-1, 0, 1),
        probe.bins = 30
      )
    },
    envir = new.env(parent = globalenv())
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

test_that("bare model = linear uses fitted object when linear exists", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  res <- local(
    {
      set.seed(111)
      n <- 200
      x1 <- rnorm(n)
      z1 <- rnorm(n)
      y1 <- x1 * z1 + rnorm(n)
      df <- data.frame(x1, z1, y1)
      linear <- lm2(y1 ~ x1 * z1, data = df, notes = FALSE)
      grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
      on.exit(grDevices::dev.off(), add = TRUE)
      interprobe(
        model = linear,
        x = "x1",
        z = "z1",
        draw = "jn",
        histogram = FALSE,
        quiet = TRUE,
        spotlights = c(-1, 0, 1),
        probe.bins = 30
      )
    },
    envir = new.env(parent = globalenv())
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

test_that("bare model = lm on vectors uses stats::lm as keyword", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  set.seed(111)
  n <- 200
  x <- rnorm(n)
  z <- rnorm(n)
  y <- x * z + rnorm(n)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x, z, y,
    model = lm,
    quiet = TRUE,
    draw = "jn",
    histogram = FALSE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

test_that("error message shows original model name (linear)", {
  set.seed(123)
  x <- rnorm(200)
  z <- rnorm(200)
  y <- x * z + rnorm(200)
  x2 <- rnorm(200)

  linear <- lm(y ~ x2 * z)

  msgs <- character(0)
  out <- withCallingHandlers(
    withRestarts(
      interprobe(x, z, y, model = linear, quiet = TRUE),
      abort = function(...) "aborted"
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_identical(out, "aborted")
  expect_true(any(grepl("model 'linear'", msgs, fixed = TRUE)))
})

