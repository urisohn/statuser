#' Enhanced Linear Model Function
#'
#' A wrapper around \code{\link[estimatr]{lm_robust}} that provides richer output
#' including standardized coefficients and both robust and classical standard errors.
#'
#' @param formula An object of class \code{\link{formula}}: a symbolic description
#'   of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param se_type The type of standard error to use. Default is \code{"HC3"}.
#'   See \code{\link[estimatr]{lm_robust}} for options.
#' @param output Character string specifying output format. \code{"statuser"} (default)
#'   returns an enhanced table with standardized coefficients and both robust and
#'   classical standard errors. \code{"estimatr"} returns the standard
#'   \code{lm_robust} output.
#' @param ... Additional arguments passed to \code{\link[estimatr]{lm_robust}}.
#'
#' @return When \code{output = "estimatr"}, returns an object of class \code{lm_robust}.
#'   When \code{output = "statuser"}, returns a data frame with the following columns:
#'   \itemize{
#'     \item \code{term}: The predictor name
#'     \item \code{estimate}: The coefficient estimate
#'     \item \code{SE.robust}: Robust standard error (using \code{se_type})
#'     \item \code{SE.classical}: Classical (OLS) standard error
#'     \item \code{t}: t-statistic (based on robust SE)
#'     \item \code{df}: Degrees of freedom
#'     \item \code{p.value}: p-value (based on robust SE)
#'     \item \code{B}: Standardized coefficient (beta)
#'   }
#'
#' @examples
#' # Basic usage with mtcars data
#' lm2(mpg ~ wt + hp, data = mtcars)
#'
#' # Get estimatr's native output
#' lm2(mpg ~ wt + hp, data = mtcars, output = "estimatr")
#'
#' # Use different robust SE type
#' lm2(mpg ~ wt + hp, data = mtcars, se_type = "HC2")
#'
#' @seealso \code{\link[estimatr]{lm_robust}}
#'
#' @export lm2
lm2 <- function(formula, data = NULL, se_type = "HC3", output = "statuser", ...) {
  
  # Capture the call
  cl <- match.call()
  
  # Check that estimatr is available
  if (!requireNamespace("estimatr", quietly = TRUE)) {
    stop("Package 'estimatr' is required for lm2(). Please install it with: install.packages('estimatr')")
  }
  
  # Validate output argument
  output <- match.arg(output, choices = c("statuser", "estimatr"))
  
  # Validate inputs and construct data frame if needed
  validated <- validate_lm2(
    formula = formula,
    data = data,
    se_type = se_type,
    se_type_missing = missing(se_type),
    dots = list(...),
    calling_env = parent.frame()
  )
  
  data <- validated$data
  se_type <- validated$se_type
  
  # Run lm_robust with specified se_type
  robust_fit <- estimatr::lm_robust(formula = formula, data = data, se_type = se_type, ...)
  
  # If user wants estimatr output, return it directly
  if (output == "estimatr") {
    return(robust_fit)
  }
  
  # For statuser output, build enhanced table
  # Extract components from lm_robust object
  term_names <- names(robust_fit$coefficients)
  estimates <- as.numeric(robust_fit$coefficients)
  robust_se <- robust_fit$std.error
  t_values <- robust_fit$statistic
  p_values <- robust_fit$p.value
  df_values <- robust_fit$df
  
  # Run classical OLS to get classical standard errors
  classical_fit <- stats::lm(formula = formula, data = data)
  classical_summary <- summary(classical_fit)
  classical_se <- classical_summary$coefficients[, "Std. Error"]
  
  # Calculate standardized coefficients (beta weights)
  # Beta = b * (SD_x / SD_y)
  # For intercept, beta is NA
  model_data <- stats::model.frame(formula, data = data)
  y <- model_data[[1]]
  sd_y <- stats::sd(y, na.rm = TRUE)
  
  # Calculate total missing observations (rows excluded due to NAs)
  n_original <- nrow(data)
  n_used <- robust_fit$nobs
  n_missing <- n_original - n_used
  
  # Calculate standardized coefficients and NA counts
  standardized_coefs <- numeric(length(term_names))
  na_counts <- integer(length(term_names))
  names(standardized_coefs) <- term_names
  names(na_counts) <- term_names
  
  # Get original data to count NAs (before model.frame removes them)
  # We need to look at the raw data columns
  for (i in seq_along(term_names)) {
    term <- term_names[i]
    if (term == "(Intercept)") {
      standardized_coefs[i] <- NA_real_
      na_counts[i] <- NA_integer_  # NA count not meaningful for intercept
    } else {
      # Get the coefficient
      b <- estimates[i]
      
      # Try to get the SD of this predictor
      # Handle factor variables and interactions
      if (term %in% names(model_data)) {
        x <- model_data[[term]]
        if (is.numeric(x)) {
          sd_x <- stats::sd(x, na.rm = TRUE)
          standardized_coefs[i] <- b * (sd_x / sd_y)
        } else {
          standardized_coefs[i] <- NA_real_
        }
        # Count NAs in original data column
        if (term %in% names(data)) {
          na_counts[i] <- sum(is.na(data[[term]]))
        } else {
          na_counts[i] <- NA_integer_
        }
      } else {
        # For factor levels, interactions, etc., set to NA
        # (standardized coefficients are typically not meaningful for these)
        standardized_coefs[i] <- NA_real_
        na_counts[i] <- NA_integer_
      }
    }
  }
  
  # Build output data frame
  result <- data.frame(
    term = term_names,
    estimate = estimates,
    SE.robust = robust_se,
    SE.classical = classical_se[term_names],
    t = t_values,
    df = df_values,
    p.value = p_values,
    B = standardized_coefs,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  # Store the model objects as attributes for potential later use
  attr(result, "call") <- cl
  attr(result, "robust_fit") <- robust_fit
  attr(result, "classical_fit") <- classical_fit
  attr(result, "se_type") <- se_type
  attr(result, "formula") <- formula
  attr(result, "nobs") <- robust_fit$nobs
  attr(result, "r.squared") <- robust_fit$r.squared
  attr(result, "adj.r.squared") <- robust_fit$adj.r.squared
  attr(result, "na_counts") <- na_counts
  attr(result, "n_missing") <- n_missing
  
  # Add class for custom print method
  class(result) <- c("lm2", class(result))
  
  return(result)
}

#' Print method for lm2 objects
#'
#' @param x An object of class \code{lm2}
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the original object
#' @export
print.lm2 <- function(x, ...) {
  
  # Helper: smart rounding based on magnitude
  # >=100: 1 decimal, >=10: 2 decimals, >=0.01: 3 decimals
  # <0.01: show 2 significant non-zero digits (e.g., .000042)
  smart_round <- function(val) {
    if (is.na(val)) return(NA_character_)
    abs_val <- abs(val)
    if (abs_val >= 100) {
      decimals <- 1
      return(format(round(val, decimals), nsmall = decimals))
    } else if (abs_val >= 10) {
      decimals <- 2
      return(format(round(val, decimals), nsmall = decimals))
    } else if (abs_val >= 0.01 || abs_val == 0) {
      decimals <- 3
      return(format(round(val, decimals), nsmall = decimals))
    } else {
      # For values < 0.01, show 2 significant digits
      # e.g., 0.000042323 -> .000042
      sign_char <- if (val < 0) "-" else ""
      # Find how many decimal places needed for 2 sig figs
      log_val <- floor(log10(abs_val))
      decimals <- -log_val + 1  # +1 to get 2 sig figs
      rounded <- round(abs_val, decimals)
      formatted <- format(rounded, nsmall = decimals, scientific = FALSE)
      # Remove leading zero
      formatted <- sub("^0\\.", ".", formatted)
      return(paste0(sign_char, formatted))
    }
  }
  
  # Helper: format p-value (no leading zero, <.0001 if tiny)
  format_p <- function(p) {
    if (is.na(p)) return(NA_character_)
    if (p < 0.0001) return("<.0001")
    # Round to 4 decimals, remove leading zero
    p_rounded <- round(p, 4)
    p_str <- format(p_rounded, nsmall = 4, scientific = FALSE)
    p_str <- sub("^0\\.", ".", p_str)
    p_str
  }
  
  # Get NA counts from attribute before modifying display_df
  na_counts <- attr(x, "na_counts")
  
  # Calculate SE flag: ! if robust and classical differ by >25%, !! if >50%, !!! if >100%
  se_flag <- character(nrow(x))
  for (i in seq_len(nrow(x))) {
    se_classical <- x$SE.classical[i]
    se_robust <- x$SE.robust[i]
    if (!is.na(se_classical) && !is.na(se_robust) && se_classical != 0) {
      ratio_diff <- abs(se_classical - se_robust) / se_classical
      if (ratio_diff > 1) {
        se_flag[i] <- "!!!"
      } else if (ratio_diff > 0.5) {
        se_flag[i] <- "!!"
      } else if (ratio_diff > 0.25) {
        se_flag[i] <- "!"
      } else {
        se_flag[i] <- ""
      }
    } else {
      se_flag[i] <- ""
    }
  }
  
  # Format B column, using "--" for intercept instead of NA
  B_formatted <- sapply(seq_along(x$B), function(i) {
    if (x$term[i] == "(Intercept)") return("--")
    smart_round(x$B[i])
  })
  
  # Format missing column, using "--" for intercept instead of NA
  missing_formatted <- if (!is.null(na_counts) && length(na_counts) == nrow(x)) {
    sapply(seq_along(na_counts), function(i) {
      if (x$term[i] == "(Intercept)") return("--")
      if (is.na(na_counts[i])) return("--")
      as.character(na_counts[i])
    })
  } else {
    NULL
  }
  
  # Build display dataframe from scratch with desired column order
  # Add leading padding spaces to values for better column separation
  pad <- function(x, n) paste0(strrep(" ", n), x)
  
  display_df <- data.frame(
    term = gsub("^\\(Intercept\\)$", "Intercept", x$term),
    estimate = pad(sapply(x$estimate, smart_round), 1),
    SE.robust = pad(sapply(x$SE.robust, smart_round), 2),
    SE.classical = pad(sapply(x$SE.classical, smart_round), 3),
    t.value = pad(sapply(x$t, smart_round), 1),
    p.value = pad(sapply(x$p.value, format_p), 1),
    effect.size = pad(B_formatted, 1),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Add missing column if available
  if (!is.null(missing_formatted)) {
    display_df$missing <- pad(missing_formatted, 2)
  }
  
  # Add red flag column at the end (use "--" when no flag)
  display_df$red.flag <- pad(ifelse(se_flag == "", "--", se_flag), 4)
  
  # Use term values as row names, then remove the term column
  rownames(display_df) <- display_df$term
  display_df$term <- NULL
  
  # Print the original call
  cat("Call: ")
  print(attr(x, "call"))
  cat("\n")
  
  # Print the table
  print(display_df, right = FALSE)
  
  # Print model summary info
  cat("\n")
  cat("N =", attr(x, "nobs"), " | ")
  cat("missing =", attr(x, "n_missing"), " | ")
  cat("df =", round(x$df[1], 0), " | ")
  cat("R² =", format(round(attr(x, "r.squared"), 3), nsmall = 3), " | ")
  cat("Adj. R² =", format(round(attr(x, "adj.r.squared"), 3), nsmall = 3), " | ")
  cat("SE type:", attr(x, "se_type"), "\n")
  cat("\nNotes:\n")
  cat("  - 'effect.size' is the standardized coefficient: beta = b * sd(x) / sd(y)\n")
  cat("  - 'missing' is the number of NA values for that variable\n")
  cat("  - 'red.flag' indicates when robust and classical SEs differ by\n     <25% (--), >25% (!), >50% (!!) or >100% (!!!)\n")
  
  invisible(x)
}
