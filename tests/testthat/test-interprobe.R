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

