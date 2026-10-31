# Maintenance: add #function_NNN before each test_that().
# Run: Rscript tests/testthat/_annotate_test_ids.R
# (Working directory should be the package root.)

strip_test_ids <- function(lines) {
  n <- length(lines)
  if (n < 2L) {
    return(lines)
  }
  drop <- rep(FALSE, n)
  for (i in 2L:n) {
    if (grepl("^test_that\\(", lines[i]) &&
        grepl("^#[A-Za-z0-9._]+_[0-9]{3}\\s*$", lines[i - 1L])) {
      drop[i - 1L] <- TRUE
    }
  }
  lines[!drop]
}

test_title <- function(line) {
  m <- regexpr("^test_that\\(\"([^\"]+)\"", line, perl = TRUE)
  if (m < 1L) {
    return(NA_character_)
  }
  st <- attr(m, "capture.start")[1L]
  ln <- attr(m, "capture.length")[1L]
  substr(line, st, st + ln - 1L)
}

prefix_for_line <- function(basename, title) {
  if (is.na(title)) {
    return("unknown")
  }
  if (identical(basename, "test-backend_equivalence.R")) {
    if (grepl("^backend: t\\.test2", title)) {
      return("t.test2")
    }
    if (grepl("^backend: lm2", title)) {
      return("lm2")
    }
    if (grepl("^backend: interprobe", title)) {
      return("interprobe")
    }
    return("backend_equivalence")
  }
  if (identical(basename, "test-gam_functions.R")) {
    if (grepl("^scatter\\.gam", title)) {
      return("scatter.gam")
    }
    if (grepl("^plot_gam", title)) {
      return("plot_gam")
    }
    if (grepl("^twolines", title)) {
      return("twolines")
    }
    return("gam")
  }
  known <- c(
    "test-message2.R" = "message2",
    "test-list2.R" = "list2",
    "test-text2.R" = "text2",
    "test-format_pvalue.R" = "format_pvalue",
    "test-t.test2.R" = "t.test2",
    "test-lm2.R" = "lm2",
    "test-plot_cdf.R" = "plot_cdf",
    "test-plot_freq.R" = "plot_freq",
    "test-plot_density.R" = "plot_density",
    "test-plot_means.R" = "plot_means",
    "test-validate_plot.R" = "validate_plot",
    "test-table2.R" = "table2",
    "test-print_functions.R" = "print.table2",
    "test-convert_to_sql.R" = "convert_to_sql",
    "test-resize_images.R" = "resize_images",
    "test-desc_var.R" = "desc_var",
    "test-interprobe.R" = "interprobe"
  )
  key <- known[[basename]]
  if (is.null(key)) {
    stop("No prefix mapping for ", basename, call. = FALSE)
  }
  key
}

annotate_file <- function(path, bump) {
  basename <- basename(path)
  lines <- readLines(path, warn = FALSE)
  lines <- strip_test_ids(lines)
  out <- character()
  for (ln in lines) {
    if (grepl("^test_that\\(", ln)) {
      title <- test_title(ln)
      pfx <- prefix_for_line(basename, title)
      k <- bump(pfx)
      id_line <- sprintf("#%s_%03d", pfx, k)
      out <- c(out, id_line, ln)
    } else {
      out <- c(out, ln)
    }
  }
  writeLines(out, path)
  message("Wrote ", path)
}

script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  z <- args[startsWith(args, "--file=")]
  if (length(z)) {
    return(dirname(normalizePath(sub("^--file=", "", z[1L]), winslash = "\\")))
  }
  getwd()
}

main <- function() {
  td <- file.path(script_dir())
  flist <- list.files(td, pattern = "^test-.*\\.R$", full.names = TRUE)
  if (!length(flist)) {
    stop("No test-*.R files in ", td, call. = FALSE)
  }
  flist <- sort(flist)
  # Cross-file backend checks last so function IDs (e.g. t.test2_001) stay in primary test files first.
  is_be <- basename(flist) == "test-backend_equivalence.R"
  flist <- c(flist[!is_be], flist[is_be])
  ctr <- new.env(parent = emptyenv())
  bump <- function(pfx) {
    cur <- ctr[[pfx]]
    if (is.null(cur)) {
      cur <- 0L
    }
    cur <- cur + 1L
    ctr[[pfx]] <- cur
    cur
  }
  for (f in flist) {
    annotate_file(f, bump)
  }
  invisible(NULL)
}

main()
