# Manual visual check for interprobe() sample-size panels and y-axis scales
# 1. Setup (resolve package root; open multipage PDF)
# 2. Part A — sample-size / frequency display variants (small & big n)
# 3. Part B — y-axis tick/ylab spacing across DV scales (small & big n)
# 4. PDF: description page, then figure page
#
# Run:
#   source("C:/git/statuser/examples/interprobe_visual_check.R")
# Output: examples/interprobe_visual_check.pdf

# 1. Setup
  script_path <- NULL
  if (sys.nframe() >= 1L) {
    for (i in seq.int(sys.nframe(), 1L)) {
      f <- sys.frame(i)$ofile
      if (!is.null(f) && nzchar(f)) {
        script_path <- f
        break
      }
    }
  }
  candidates <- c(
    if (!is.null(script_path)) file.path(dirname(script_path), ".."),
    getwd(),
    "C:/git/statuser"
  )
  pkg_root <- NULL
  for (cand in candidates) {
    desc <- file.path(cand, "DESCRIPTION")
    if (file.exists(desc)) {
      first <- tryCatch(readLines(desc, n = 1L), error = function(e) "")
      if (length(first) == 1L && grepl("^Package", first)) {
        pkg_root <- normalizePath(cand, winslash = "/", mustWork = TRUE)
        break
      }
    }
  }
  if (is.null(pkg_root)) {
    stop("Could not find statuser package root (DESCRIPTION).")
  }

  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(pkg_root, quiet = TRUE)
  } else if (requireNamespace("statuser", quietly = TRUE)) {
    library(statuser)
  } else {
    stop("Install pkgload (dev) or statuser, then re-run this script.")
  }

  set.seed(29)
  probe.bins <- 40
  out_pdf <- file.path(pkg_root, "examples", "interprobe_visual_check.pdf")
  ns <- c(small = 80L, big = 2000L)

  draw_desc_page <- function(caption) {
    graphics::par(mfrow = c(1, 1), mar = c(1, 1, 1, 1), oma = c(0, 0, 0, 0))
    graphics::plot.new()
    lines <- unlist(strsplit(caption, "\n", fixed = TRUE), use.names = FALSE)
    graphics::text(0.02, 0.92, lines[1], adj = c(0, 1), cex = 1.3, font = 2, xpd = NA)
    if (length(lines) > 1L) {
      y <- 0.78 - 0.08 * seq_along(lines[-1])
      graphics::text(0.02, y, lines[-1], adj = c(0, 1), cex = 1.05, font = 1, xpd = NA, family = "sans")
    }
  }

# 2. Part A — sample-size / frequency display
  make_continuous <- function(n) {
    x <- rnorm(n)
    z <- rnorm(n, mean = 10, sd = 2)
    y <- 0.3 * x + 0.2 * z + 0.8 * x * z + rnorm(n, sd = 2)
    data.frame(x = x, z = z, y = y)
  }

  make_binary_x <- function(n) {
    z <- rnorm(n)
    x <- factor(sample(c("A", "B"), n, replace = TRUE))
    y <- as.numeric(x) + 0.5 * z + 0.7 * (as.numeric(x) - 1) * z + rnorm(n, sd = 1.5)
    data.frame(x = x, z = z, y = y)
  }

  make_ternary_x <- function(n) {
    z <- rnorm(n)
    x <- factor(sample(c("lo", "mid", "hi"), n, replace = TRUE))
    y <- as.numeric(x) + 0.4 * z + 0.5 * (as.numeric(x) - 2) * z + rnorm(n, sd = 1.5)
    data.frame(x = x, z = z, y = y)
  }

  make_discrete_z <- function(n) {
    x <- rnorm(n)
    z <- sample(1:5, n, replace = TRUE)
    y <- 0.2 * x + 0.3 * z + 0.6 * x * z + rnorm(n, sd = 2)
    data.frame(x = x, z = z, y = y)
  }

  make_discrete_x <- function(n) {
    x <- sample(seq(-2, 2, by = 1), n, replace = TRUE)
    z <- rnorm(n)
    y <- 0.2 * x + 0.3 * z + 0.6 * x * z + rnorm(n, sd = 2)
    data.frame(x = x, z = z, y = y)
  }

  sample_size_variants <- list(
    list(
      id = "continuous_both_hist",
      desc = "Continuous x & z; default GAM; draw='both'; frequency panels ON. Check stacked hist under both curves, count labels inside gray band, no collisions.",
      builder = make_continuous,
      args = list(draw = "both", histogram = TRUE)
    ),
    list(
      id = "continuous_both_nohist",
      desc = "Continuous x & z; default GAM; draw='both'; frequency panels OFF. Baseline layout without the bottom frequency row.",
      builder = make_continuous,
      args = list(draw = "both", histogram = FALSE)
    ),
    list(
      id = "continuous_jn_hist",
      desc = "Continuous x & z; single Johnson-Neyman panel with frequency subplot underneath.",
      builder = make_continuous,
      args = list(draw = "jn", histogram = TRUE)
    ),
    list(
      id = "continuous_slopes_hist",
      desc = "Continuous x & z; single simple-slopes panel with frequency subplot underneath.",
      builder = make_continuous,
      args = list(draw = "simple slopes", histogram = TRUE)
    ),
    list(
      id = "continuous_linear_hist",
      desc = "Continuous x & z; model='linear' (lm2) instead of GAM; both panels with frequency subplots.",
      builder = make_continuous,
      args = list(draw = "both", histogram = TRUE, model = "linear")
    ),
    list(
      id = "continuous_x_on_axis_hist",
      desc = "Continuous x & z; moderator.on.x.axis=FALSE so focal x is on the x-axis; frequency panels still aligned to that axis.",
      builder = make_continuous,
      args = list(draw = "both", histogram = TRUE, moderator.on.x.axis = FALSE)
    ),
    list(
      id = "continuous_nbins_5",
      desc = "Continuous x & z; n.bin.continuous=5 (coarser frequency bins). Check fewer, wider stacked bars and readable count labels.",
      builder = make_continuous,
      args = list(draw = "both", histogram = TRUE, n.bin.continuous = 5)
    ),
    list(
      id = "continuous_nbins_20",
      desc = "Continuous x & z; n.bin.continuous=20 (finer frequency bins). Check denser bars still readable at small and big n.",
      builder = make_continuous,
      args = list(draw = "both", histogram = TRUE, n.bin.continuous = 20)
    ),
    list(
      id = "continuous_nmax_10",
      desc = "Continuous x & z; n.max=10 so line darkness/width saturates early. Check that sparse bins stay light and dense bins look fully saturated.",
      builder = make_continuous,
      args = list(draw = "both", histogram = TRUE, n.max = 10)
    ),
    list(
      id = "binary_x_hist",
      desc = "Binary/categorical focal x (2 levels) with continuous z; two colored curves; frequency panels ON.",
      builder = make_binary_x,
      args = list(draw = "both", histogram = TRUE, cols = c("red4", "dodgerblue"))
    ),
    list(
      id = "ternary_x_hist",
      desc = "3-level categorical focal x with continuous z; three curves; frequency panels ON.",
      builder = make_ternary_x,
      args = list(draw = "both", histogram = TRUE)
    ),
    list(
      id = "discrete_z_hist",
      desc = "Continuous x with discrete moderator z (5 values); k=3 so GAM can fit; discrete frequency segments.",
      builder = make_discrete_z,
      args = list(draw = "both", histogram = TRUE, k = 3)
    ),
    list(
      id = "discrete_x_hist",
      desc = "Discrete focal x (5 values) with continuous z; k=3; spotlight curves with continuous-z frequency bins.",
      builder = make_discrete_x,
      args = list(draw = "both", histogram = TRUE, k = 3)
    )
  )

# 3. Part B — y-axis tick / ylab spacing across DV scales
  make_y_range <- function(n, ymin, ymax) {
    x <- rnorm(n)
    z <- rnorm(n, mean = 10, sd = 2)
    y_raw <- 0.3 * x + 0.2 * z + 0.8 * x * z + rnorm(n, sd = 2)
    yr <- range(y_raw)
    y <- ymin + (y_raw - yr[1]) / diff(yr) * (ymax - ymin)
    data.frame(x = x, z = z, y = y)
  }

  y_ranges <- list(
    list(
      id = "y_0_1",
      ymin = 0,
      ymax = 1,
      desc = "DV scaled to [0, 1]. Check y-tick decimals and ylab spacing (ticks like 0.0, 0.2, …)."
    ),
    list(
      id = "y_1_10",
      ymin = 1,
      ymax = 10,
      desc = "DV scaled to [1, 10]. Check short integer/decimal ticks and ylab clearance."
    ),
    list(
      id = "y_1_1000",
      ymin = 1,
      ymax = 1000,
      desc = "DV scaled to [1, 1000]. Check medium-width tick labels (e.g. 200, 400) vs ylab."
    ),
    list(
      id = "y_1_5e7",
      ymin = 1,
      ymax = 5e7,
      desc = "DV scaled to [1, 5e7]. Check wide tick labels (scientific or large integers) vs ylab left padding."
    )
  )

# 4. Multipage PDF
  grDevices::pdf(out_pdf, width = 14, height = 8.5, onefile = TRUE)

  page <- 0L

  for (n_label in names(ns)) {
    n <- ns[[n_label]]
    for (v in sample_size_variants) {
      page <- page + 1L
      caption <- paste0(
        "Part A (sample sizes) | Page ", page, " | ", v$id,
        " | n=", n, " (", n_label, ")\n",
        v$desc
      )
      draw_desc_page(caption)

      d <- v$builder(n)
      args <- c(
        list(
          x = "x", z = "z", y = "y", data = d,
          quiet = TRUE,
          probe.bins = probe.bins
        ),
        v$args
      )
      do.call(interprobe, args)
    }
  }

  for (n_label in names(ns)) {
    n <- ns[[n_label]]
    for (yr in y_ranges) {
      page <- page + 1L
      caption <- paste0(
        "Part B (y-values) | Page ", page, " | ", yr$id,
        " | y in [", yr$ymin, ", ", format(yr$ymax, scientific = FALSE), "]",
        " | n=", n, " (", n_label, ")\n",
        yr$desc, "\n",
        "Same continuous x*z GAM, draw='both', histogram=TRUE. Focus: y-axis tick width and ylab placement."
      )
      draw_desc_page(caption)

      d <- make_y_range(n, yr$ymin, yr$ymax)
      interprobe(
        x = "x", z = "z", y = "y", data = d,
        quiet = TRUE,
        probe.bins = probe.bins,
        draw = "both",
        histogram = TRUE
      )
    }
  }

  grDevices::dev.off()

  cat(paste0(
    "Wrote ", page, " figure cases (", page * 2L, " PDF pages: desc + figure) to:\n",
    out_pdf, "\n"
  ))
