be_expect_equal_num <- function(a, b, tol = 1e-10) {
  
  if (is.null(a) && is.null(b)) {
    testthat::expect_true(TRUE)
    return(invisible(NULL))
  }
  
  if (is.null(a) || is.null(b)) {
    testthat::expect_identical(a, b)
    return(invisible(NULL))
  }
  
  if (isTRUE(all(is.na(a))) && isTRUE(all(is.na(b)))) {
    testthat::expect_true(TRUE)
    return(invisible(NULL))
  }
  
  if (is.numeric(a) || is.numeric(b)) {
    testthat::expect_equal(as.numeric(a), as.numeric(b), tolerance = tol)
    return(invisible(NULL))
  }
  
  testthat::expect_identical(a, b)
  invisible(NULL)
}

be_extract_linear_interaction_p <- function(model, xvar, zvar) {
  
  cf <- tryCatch(stats::coef(summary(model)), error = function(e) NULL)
  if (is.null(cf)) return(NA_real_)
  
  rn <- rownames(cf)
  if (is.null(rn)) return(NA_real_)
  
  candidates <- c(paste0(xvar, ":", zvar), paste0(zvar, ":", xvar))
  idx <- match(candidates, rn)
  idx <- idx[!is.na(idx)]
  if (length(idx) < 1) return(NA_real_)
  
  pcol <- intersect(colnames(cf), c("Pr(>|t|)", "Pr(>|z|)"))
  if (length(pcol) < 1) return(NA_real_)
  
  as.numeric(cf[idx[1], pcol[1]])
}

be_sig_from_ci <- function(conf_low, conf_high) {
  
  (conf_low > 0) | (conf_high < 0)
}

be_parse_p_from_apa_line <- function(x) {
  
  # Parse "p < .001" or "p = .023" into numeric (use 0.001 for p<.001)
  if (length(x) != 1 || is.na(x)) return(NA_real_)
  
  if (grepl("p\\s*<\\s*\\.001", x)) return(0.001)
  
  m <- regexec("p\\s*=\\s*([0-9]*\\.?[0-9]+)", x)
  r <- regmatches(x, m)
  if (length(r) < 1 || length(r[[1]]) < 2) return(NA_real_)
  
  as.numeric(r[[1]][2])
}

