# Extensive interprobe() SVG grid across DGPs, n, and focal-x support sizes
# 1. Setup (pkg root, load_all, output dir)
# 2. Helpers (make_x, simulate, out paths)
# 3. Grid loops (form x n x n_x -> SVG)
# 4. Summary
#
# Run:
#   "C:/Program Files/R/R-4.6.0/bin/Rscript.exe" "C:/git/statuser/examples/interprobe_plot_grid.R"
# Output: c:/temp/test_interplot/*.svg

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

  out_dir <- "c:/temp/test_interplot"
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  set.seed(20251031)
  probe.bins <- 40L
  forms <- c("linear", "exp_rise", "quadratic_u", "threshold", "none", "sign_flip")
  ns <- c(50L, 500L, 1000L)
  n_xs <- c(2L, 3L, 7L, 100L, 500L)

# 2. Helpers
  make_x <- function(n, n_x) {
    # Draw from a fixed support of size n_x; coerce 2/3 to factor for categorical path
    support <- if (n_x == 2L) {
      c(0, 1)
    } else if (n_x == 3L) {
      c(0, 1, 2)
    } else {
      seq(0, 1, length.out = n_x)
    }
    x_num <- sample(support, n, replace = TRUE)
    if (n_x <= 3L) {
      factor(x_num, levels = support)
    } else {
      x_num
    }
  }

  x_num <- function(x) {
    if (is.factor(x)) as.numeric(x) - 1 else as.numeric(x)
  }

  simulate <- function(form, n, n_x) {
    x <- make_x(n, n_x)
    z <- rnorm(n)
    xn <- x_num(x)
    # Center/scale for interpretable interaction magnitudes across supports
    xn_c <- as.numeric(scale(xn))
    z_c <- as.numeric(scale(z))
    z_pos <- (z_c - min(z_c)) / diff(range(z_c)) + 1e-6

    mu <- switch(
      form,
      linear = 0.2 * xn_c + 0.3 * z_c + 0.8 * xn_c * z_c,
      exp_rise = 0.2 * z_c + xn_c * (2 * (1 - exp(-z_pos / 3))),
      quadratic_u = 0.2 * xn_c + 0.1 * z_c + 0.9 * xn_c * (z_c^2),
      threshold = {
        above <- as.numeric(z >= median(z))
        0.2 * xn_c + 0.2 * z_c + 1.2 * xn_c * above
      },
      none = 0.5 * xn_c + 0.4 * z_c + 0.3 * (z_c^2),
      sign_flip = 0.1 * z_c + xn_c * (1.5 * tanh(z_c)),
      stop("Unknown form: ", form)
    )
    y <- mu + rnorm(n, sd = 1.2)
    data.frame(x = x, z = z, y = y)
  }

  out_svg <- function(form, n, n_x) {
    file.path(out_dir, paste0(form, "_n", n, "_x", n_x, ".svg"))
  }

  out_err <- function(form, n, n_x) {
    file.path(out_dir, paste0(form, "_n", n, "_x", n_x, ".txt"))
  }

# 3. Grid loops
  n_ok <- 0L
  n_fail <- 0L
  failed <- character(0)

  for (form in forms) {
    for (n in ns) {
      for (n_x in n_xs) {
        svg_path <- out_svg(form, n, n_x)
        err_path <- out_err(form, n, n_x)
        if (file.exists(err_path)) {
          unlink(err_path)
        }

        d <- simulate(form, n, n_x)
        nux <- length(unique(d$x))

        args <- list(
          x = "x",
          z = "z",
          y = "y",
          data = d,
          quiet = TRUE,
          probe.bins = probe.bins,
          draw = "both",
          histogram = TRUE,
          save.as = svg_path
        )
        # Discrete focal (4..max.unique): keep k small so s(x)/ti can fit
        if (nux >= 4L && nux <= 11L) {
          args$k <- 3L
        }
        if (nux == 2L) {
          args$cols <- c("red4", "dodgerblue")
        }

        ok <- TRUE
        err_msg <- NULL
        tryCatch(
          do.call(interprobe, args),
          error = function(e) {
            ok <<- FALSE
            err_msg <<- conditionMessage(e)
          }
        )

        if (ok && file.exists(svg_path)) {
          n_ok <- n_ok + 1L
        } else {
          n_fail <- n_fail + 1L
          label <- paste0(form, "_n", n, "_x", n_x)
          failed <- c(failed, label)
          if (is.null(err_msg)) {
            err_msg <- "interprobe() returned without writing SVG"
          }
          writeLines(err_msg, err_path)
          if (file.exists(svg_path)) {
            unlink(svg_path)
          }
        }
      }
    }
  }

# 4. Summary
  msg <- paste0(
    "Wrote ", n_ok, " SVG(s) to ", out_dir, "\n",
    "Failed cells: ", n_fail, "\n"
  )
  if (n_fail > 0L) {
    msg <- paste0(msg, "  ", paste(failed, collapse = "\n  "), "\n")
  }
  cat(msg)
