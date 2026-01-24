#' Enhanced Linear Regression (lm)
#'
#' Runs a linear regression with better defaults (robust SE), and richer & better 
#' formatted output than \code{lm}. For robust and clustered errors it relies on \code{\link[estimatr]{lm_robust}}. 
#' The output reports classical and robust errors, number of missing observations per
#' variable, an effect size column (standardized regression coefficient), and a red.flag column per variable 
#' flagging the need to conduct specific diagnostics. It relies by default on HC3 for standard errors;
#' \code{lm_robust} relies on HC2 (and Stata's 'reg y x, robust' on HC1), which can have
#' inflated false-positive rates in smaller samples (Long & Ervin, 2000).
#'
#' When \code{clusters} is specified, the output includes three SE columns: \code{SE.cluster} (CR2),
#' \code{SE.robust} (HC3), and \code{SE.classical}. The red flag diagnostics compare HC3 to classical SE,
#' not the clustered SE, because clustered SEs are expected to be larger when there is within-cluster
#' correlation (which is not a problem). Comparing HC3 to classical SE isolates heteroskedasticity
#' and outlier issues from the expected clustering effect.
#' 
#' @param formula An object of class \code{\link{formula}}: a symbolic description
#'   of the model to be fitted.
#' @param data An optional data frame, list or environment containing the variables 
#'   in the model. If not found in \code{data}, the variables are taken from the 
#'   environment from which \code{lm2} is called.
#' @param se_type The type of standard error to use. Default is \code{"HC3"}.
#'   Without clusters: \code{"HC0"}, \code{"HC1"}, \code{"HC2"}, or \code{"HC3"}.
#'   When \code{clusters} is specified, \code{se_type} is automatically set to \code{"CR2"}.
#' @param output Character string specifying output format. \code{"statuser"} (default)
#'   returns an enhanced table with standardized coefficients and both robust and
#'   classical standard errors. \code{"estimatr"} returns the standard
#'   \code{lm_robust} output.
#' @param notes Logical. If TRUE (default), print explanatory notes below the table
#'   when the result is printed.
#' @param clusters An optional variable indicating clusters for cluster-robust standard 
#'   errors. When specified, \code{se_type} is automatically set to \code{"CR2"} 
#'   (bias-reduced cluster-robust estimator). Passed to \code{\link[estimatr]{lm_robust}}.
#' @param fixed_effects An optional right-sided formula containing the fixed effects 
#'   to be projected out (absorbed) before estimation. Useful for models with many 
#'   fixed effect groups (e.g., \code{~ firm_id} or \code{~ firm_id + year}). 
#'   Passed to \code{\link[estimatr]{lm_robust}}.
#' @param ... Additional arguments passed to \code{\link[estimatr]{lm_robust}}.
#'
#' @return When \code{output = "estimatr"}, returns an object of class \code{lm_robust}.
#'   When \code{output = "statuser"}, returns an object that inherits from \code{lm_robust}
#'   (for compatibility with packages like \code{marginaleffects}) with class \code{c("lm2", "lm_robust", ...)}
#'   and additional attributes for enhanced display:
#'   \itemize{
#'     \item \code{statuser_table}: A data frame with columns for display (estimate, SE.robust, 
#'       SE.classical, SE.cluster if clustered, t, df, p.value, B, term)
#'     \item \code{classical_fit}: The classical OLS fit for comparison
#'     \item \code{na_counts}: Number of NA values per variable
#'     \item \code{n_missing}: Total observations excluded due to missing values
#'     \item \code{notes}: Whether to print notes
#'     \item \code{has_clusters}: Whether clustered SE was used
#'   }
#'   The print method displays an enhanced table with standardized coefficients (effect sizes),
#'   both robust and classical standard errors, and diagnostic red flags.
#'
#' @details
#' Robust standard errors and clustered standard errors are computed using 
#' \code{\link[estimatr]{lm_robust}}; see the documentation of that function for details.
#'
#' The \code{red.flag} column provides diagnostic warnings:
#' \itemize{
#'   \item \code{!}, \code{!!}, \code{!!!}: Robust and classical standard errors differ by 
#'     more than 25\%, 50\%, or 100\%, respectively. Large differences may suggest model 
#'     misspecification or outliers (but they may also be benign). When encountering a red flag,
#'     authors should plot the distributions to look for outliers or skewed data, and use scatter.gam()
#'     to look for possible nonlinearities in the relevant variables.
#'     King & Roberts (2015) propose a higher cutoff, at 100\%, and a bootstrapped significance test; 
#'     \code{statuser} does not follow either recommendation. The former seems too liberal, the 
#'     latter too time consuming to include in every regression.
#'   \item \code{X} and \code{X*}: For interaction terms, the component variables are correlated with 
#'     |r| > 0.3 (\code{X}) or p < .05 (\code{X*}); this can produce spurious interactions. Authors are advised
#'     to not rely on the linear model and instead use GAM (Simonsohn, 2024).
#' }
#'
#' @references
#' King, G., & Roberts, M. E. (2015). How robust standard errors expose methodological 
#' problems they do not fix, and what to do about it. \emph{Political Analysis}, 23(2), 159-179.
#'
#' Long, J. S., & Ervin, L. H. (2000). Using heteroscedasticity consistent standard errors 
#' in the linear regression model. \emph{The American Statistician}, 54(3), 217-224.
#'
#' Simonsohn, U. (2024). Interacting with curves: How to validly test and probe 
#' interactions in the real (nonlinear) world. \emph{Advances in Methods and Practices in 
#' Psychological Science}, 7(1), 1-22.
#'
#' @examples
#' # Basic usage with data argument
#' lm2(mpg ~ wt + hp, data = mtcars)
#'
#' # Without data argument (variables from environment)
#' y <- mtcars$mpg
#' x1 <- mtcars$wt
#' x2 <- mtcars$hp
#' lm2(y ~ x1 + x2)
#'
#' # RED FLAG EXAMPLES
#' 
#' # Example 1: red flag catches a nonlinearity
#' # True model is quadratic: y = x^2
#' set.seed(123)
#' x <- runif(200, -3, 3)
#' y <- x^2 + rnorm(200, sd = 2)
#' 
#' # lm2() shows red flag due to misspecification
#' lm2(y ~ x)
#' 
#' # Follow up with scatter.gam() to diagnose it
#' scatter.gam(x, y)
#'
#' # Example 2: red flag catches an outlier in y
#' # True model is y = x, but one observation has a very large y value
#' set.seed(123)
#' x <- sort(rnorm(200))
#' y <- round(x + rnorm(200, sd = 2), 1)
#' y[200] <- 100  # Outlier
#' 
#' # lm2() flags x
#' lm2(y ~ x)
#' 
#' # Look at distribution of y to spot the outlier
#' plot_freq(y)
#'
#' # Example 3: red flag catches an outlier in one predictor
#' # True model is y = x1 + x2, but x2 has an extreme value
#' set.seed(123)
#' x1 <- round(rnorm(200),.1)
#' x2 <- round(rnorm(200),.1)
#' y <- x1 + x2 + rnorm(200, sd = 0.5)
#' x2[200] <- 50  # Outlier in x2
#' 
#' # lm2() flags x2 (but not x1)
#' lm2(y ~ x1 + x2)
#' 
#' # Look at distribution of x2 to spot the outlier
#' plot_freq(x2)
#'
#' @seealso \code{\link[estimatr]{lm_robust}}, \code{\link{scatter.gam}}
#'
#' @export lm2
lm2 <- function(formula, data = NULL, se_type = "HC3", output = "statuser", notes = TRUE, 
                clusters = NULL, fixed_effects = NULL, ...) {
  
  # Capture the call
  cl <- match.call()
  
  # Check that estimatr is available
  if (!requireNamespace("estimatr", quietly = TRUE)) {
    stop("Package 'estimatr' is required for lm2(). Please install it with: install.packages('estimatr')")
  }
  
  # Validate output argument
  output <- match.arg(output, choices = c("statuser", "estimatr"))
  
  # Check if clusters are specified (either as argument or in ...)
  has_clusters <- !is.null(clusters) || "clusters" %in% names(list(...))
  
  # Validate inputs and construct data frame if needed
  validated <- validate_lm2(
    formula = formula,
    data = data,
    se_type = se_type,
    se_type_missing = missing(se_type),
    dots = list(..., clusters = clusters),
    calling_env = parent.frame()
  )
  
  data <- validated$data
  se_type <- validated$se_type
  

  # Build arguments for lm_robust (only include non-NULL optional args)
  lm_robust_args <- list(formula = formula, data = data, se_type = se_type, ...)
  if (!is.null(clusters)) lm_robust_args$clusters <- clusters
  if (!is.null(fixed_effects)) lm_robust_args$fixed_effects <- fixed_effects
  
  # Run lm_robust with specified se_type
  robust_fit <- do.call(estimatr::lm_robust, lm_robust_args)
  
  # If user wants estimatr output, return it directly
  if (output == "estimatr") {
    return(robust_fit)
  }
  
  # For statuser output, build enhanced object that inherits from lm_robust
  # This allows packages like marginaleffects to work with it seamlessly
  
  # Extract components from lm_robust object
  term_names <- names(robust_fit$coefficients)
  estimates <- as.numeric(robust_fit$coefficients)
  
  # When clusters are used, we need both clustered SE and HC3 robust SE
  # The clustered SE comes from robust_fit (CR2)
  # The HC3 robust SE comes from a separate run without clusters
  # This lets us flag heteroskedasticity/outliers separately from clustering effects
  if (has_clusters) {
    cluster_se <- robust_fit$std.error
    # t-values and p-values based on clustered SE
    t_values <- robust_fit$statistic
    p_values <- robust_fit$p.value
    df_values <- robust_fit$df
    # Run HC3 without clusters to get heteroskedasticity-robust SE (for red flag only)
    hc3_args <- list(formula = formula, data = data, se_type = "HC3", ...)
    if (!is.null(fixed_effects)) hc3_args$fixed_effects <- fixed_effects
    hc3_fit <- do.call(estimatr::lm_robust, hc3_args)
    robust_se <- hc3_fit$std.error
  } else {
    cluster_se <- NULL
    robust_se <- robust_fit$std.error
    t_values <- robust_fit$statistic
    p_values <- robust_fit$p.value
    df_values <- robust_fit$df
  }
  
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
  
  # Build display table data frame (used by print method)
  statuser_table <- data.frame(
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
  
  # Add SE.cluster column if clusters were used
  if (!is.null(cluster_se)) {
    statuser_table$SE.cluster <- cluster_se
  }
  
  # Start with the robust_fit object so we inherit from lm_robust
  # This ensures compatibility with marginaleffects and other packages
  result <- robust_fit
  
  # Store the statuser-specific attributes
 attr(result, "statuser_table") <- statuser_table
  attr(result, "lm2_call") <- cl
  attr(result, "classical_fit") <- classical_fit
  attr(result, "na_counts") <- na_counts
  attr(result, "n_missing") <- n_missing
  attr(result, "notes") <- notes
  attr(result, "has_clusters") <- has_clusters
  
  # Prepend "lm2" class so our print method is used, but keep lm_robust inheritance
  class(result) <- c("lm2", class(result))
  
  return(result)
}

#' Print method for lm2 objects
#'
#' @param x An object of class \code{lm2}
#' @param notes Logical. If TRUE (default), print explanatory notes below the table.
#'   If not specified, uses the value set when \code{lm2()} was called.
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the original object
#' @export
print.lm2 <- function(x, notes = NULL, ...) {
  
  # Use notes attribute from lm2() call if not explicitly specified
  if (is.null(notes)) {
    notes <- attr(x, "notes")
    if (is.null(notes)) notes <- TRUE  
  }
  
  # Get the statuser table from attribute
  tbl <- attr(x, "statuser_table")
  
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
  
  # Get model data for correlation checks (x itself is now the lm_robust object)
  model_data <- x$model
  if (is.null(model_data)) {
    # Fallback: reconstruct from formula and data
    model_data <- tryCatch(
      stats::model.frame(x$terms, data = attr(x, "classical_fit")$model),
      error = function(e) NULL
    )
  }
  
  # Calculate SE flag: ! if robust and classical differ by >25%, !! if >50%, !!! if >100%
  # Also check for interaction term correlations (X, X*)
  se_flag <- character(nrow(tbl))
  has_interactions <- FALSE
  
  for (i in seq_len(nrow(tbl))) {
    flags <- character(0)
    
    # Check SE difference
    se_classical <- tbl$SE.classical[i]
    se_robust <- tbl$SE.robust[i]
    if (!is.na(se_classical) && !is.na(se_robust) && se_classical != 0) {
      ratio_diff <- abs(se_classical - se_robust) / se_classical
      if (ratio_diff > 1) {
        flags <- c(flags, "!!!")
      } else if (ratio_diff > 0.5) {
        flags <- c(flags, "!!")
      } else if (ratio_diff > 0.25) {
        flags <- c(flags, "!")
      }
    }
    
    # Check if this is an interaction term (contains ":")
    term <- tbl$term[i]
    if (grepl(":", term)) {
      has_interactions <- TRUE
      # Extract the component variable names
      components <- strsplit(term, ":")[[1]]
      
      # Check correlation between components if we have 2 numeric variables
      if (length(components) == 2 && !is.null(model_data)) {
        var1_name <- components[1]
        var2_name <- components[2]
        
        # Try to get the variables from model data or original data
        var1 <- NULL
        var2 <- NULL
        
        if (var1_name %in% names(model_data)) {
          var1 <- model_data[[var1_name]]
        }
        if (var2_name %in% names(model_data)) {
          var2 <- model_data[[var2_name]]
        }
        
        # Calculate correlation if both are numeric
        if (!is.null(var1) && !is.null(var2) && is.numeric(var1) && is.numeric(var2)) {
          cor_test <- tryCatch(
            stats::cor.test(var1, var2),
            error = function(e) NULL
          )
          
          if (!is.null(cor_test)) {
            cor_val <- abs(cor_test$estimate)
            cor_p <- cor_test$p.value
            
            # X* if correlation is significant at p < .05
            if (!is.na(cor_p) && cor_p < 0.05) {
              flags <- c(flags, "X*")
            } else if (!is.na(cor_val) && cor_val > 0.3) {
              # X if correlation > .3 (but not significant)
              flags <- c(flags, "X")
            }
          }
        }
      }
    }
    
    se_flag[i] <- paste(flags, collapse = " ")
  }
  
  # Format B column, using "--" for intercept instead of NA
  B_formatted <- sapply(seq_along(tbl$B), function(i) {
    if (tbl$term[i] == "(Intercept)") return("--")
    smart_round(tbl$B[i])
  })
  
  # Format missing column, using "--" for intercept instead of NA
  missing_formatted <- if (!is.null(na_counts) && length(na_counts) == nrow(tbl)) {
    sapply(seq_along(na_counts), function(i) {
      if (tbl$term[i] == "(Intercept)") return("--")
      if (is.na(na_counts[i])) return("--")
      as.character(na_counts[i])
    })
  } else {
    NULL
  }
  
  # Build display dataframe from scratch with desired column order
  # Add leading padding spaces to values for better column separation
  pad <- function(x, n) paste0(strrep(" ", n), x)
  
  # Right-align values within column, then add trailing space
  right_align <- function(x, trail = 1) {
    max_width <- max(nchar(x))
    formatted <- format(x, width = max_width, justify = "right")
    paste0(formatted, strrep(" ", trail))
  }
  
  # Check if we have clustered SE
  has_clusters <- attr(x, "has_clusters")
  if (is.null(has_clusters)) has_clusters <- FALSE
  
  # Format columns
  estimate_vals <- sapply(tbl$estimate, smart_round)
  t_vals <- sapply(tbl$t, smart_round)
  
  # Check if df varies across coefficients
  df_vals <- tbl$df
  df_varies <- length(unique(round(df_vals, 2))) > 1
  
  # Format term names: lowercase, with 2 trailing spaces
  term_names_formatted <- paste0(tolower(gsub("^\\(Intercept\\)$", "intercept", tbl$term)), "  ")
  
  if (has_clusters) {
    # With clusters: SE.cluster, SE.robust (HC3), SE.classical
    if (df_varies) {
      # df varies: include as column between t.value and p.value
      display_df <- data.frame(
        term = term_names_formatted,
        estimate = right_align(estimate_vals, 1),
        SE.cluster = pad(sapply(tbl$SE.cluster, smart_round), 2),
        SE.robust = pad(sapply(tbl$SE.robust, smart_round), 2),
        SE.classical = pad(sapply(tbl$SE.classical, smart_round), 3),
        t.value = right_align(t_vals, 1),
        df = pad(sapply(df_vals, function(d) format(round(d, 1), nsmall = 1)), 1),
        p.value = pad(sapply(tbl$p.value, format_p), 1),
        effect.size = pad(B_formatted, 1),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      display_df <- data.frame(
        term = term_names_formatted,
        estimate = right_align(estimate_vals, 1),
        SE.cluster = pad(sapply(tbl$SE.cluster, smart_round), 2),
        SE.robust = pad(sapply(tbl$SE.robust, smart_round), 2),
        SE.classical = pad(sapply(tbl$SE.classical, smart_round), 3),
        t.value = right_align(t_vals, 1),
        p.value = pad(sapply(tbl$p.value, format_p), 1),
        effect.size = pad(B_formatted, 1),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  } else {
    # Without clusters: SE.robust, SE.classical
    if (df_varies) {
      # df varies: include as column between t.value and p.value
      display_df <- data.frame(
        term = term_names_formatted,
        estimate = right_align(estimate_vals, 1),
        SE.robust = pad(sapply(tbl$SE.robust, smart_round), 2),
        SE.classical = pad(sapply(tbl$SE.classical, smart_round), 3),
        t.value = right_align(t_vals, 1),
        df = pad(sapply(df_vals, function(d) format(round(d, 1), nsmall = 1)), 1),
        p.value = pad(sapply(tbl$p.value, format_p), 1),
        effect.size = pad(B_formatted, 1),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      display_df <- data.frame(
        term = term_names_formatted,
        estimate = right_align(estimate_vals, 1),
        SE.robust = pad(sapply(tbl$SE.robust, smart_round), 2),
        SE.classical = pad(sapply(tbl$SE.classical, smart_round), 3),
        t.value = right_align(t_vals, 1),
        p.value = pad(sapply(tbl$p.value, format_p), 1),
        effect.size = pad(B_formatted, 1),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  
  # Add missing column if available
  if (!is.null(missing_formatted)) {
    display_df$missing <- pad(missing_formatted, 2)
  }
  
  # Add red flag column at the end (use "--" when no flag)
  display_df$red.flag <- pad(ifelse(se_flag == "", "--", se_flag), 4)
  
  # Use term values as row names, then remove the term column
  rownames(display_df) <- display_df$term
  display_df$term <- NULL
  
  # Print the original call (use lm2_call attribute which stores the lm2() call)
  cat("Call: ")
  print(attr(x, "lm2_call"))
  cat("\n")
  
  # Print the table
  print(display_df, right = FALSE)
  
  # Print model summary info
  cat("\n")
  cat("N =", x$nobs, " | ")
  cat("missing =", attr(x, "n_missing"), " | ")
  if (!df_varies) {
    cat("df =", round(tbl$df[1], 0), " | ")
  }
  cat("R² =", format(round(x$r.squared, 3), nsmall = 3), " | ")
  cat("Adj. R² =", format(round(x$adj.r.squared, 3), nsmall = 3), " | ")
  if (has_clusters) {
    cat("SE type: CR2 (cluster)\n")
  } else {
    cat("SE type:", x$se_type, "\n")
  }
  if (notes) {
    cat("\nNotes:\n")
    if (has_clusters) {
      cat("  - t.value & p.value are based on clustered SE (CR2)\n")
      cat("  - SE.robust (HC3) used only to contrast with SE.classical to flag observations\n")
    } else {
      cat("  - t.value & p.value are based on robust SE (HC3)\n")
    }
    cat("  - effect.size is the standardized coefficient: beta = b * sd(x) / sd(y)\n")
    cat("  - missing: number of observations excluded due to missing values\n")
    if (has_interactions) {
      cat("  - red.flag:\n")
      cat("     !, !!, !!!: robust & classical SE differ by more than 25%, 50%, 100%\n")
      cat("     X: terms in interaction are correlated r > .3 (see Simonsohn 2025)\n")
      cat("     X*: terms in interaction are correlated p < .05 (see Simonsohn 2025)\n")
    } else {
      cat("  - red.flag: !, !!, !!!: robust & classical SE differ by more than 25%, 50%, 100%\n")
    }
    cat("    (set notes=FALSE to prevent printing these notes)\n")
  }
  
  invisible(x)
}

#' Summary method for lm2 objects
#'
#' @param object An object of class \code{lm2}
#' @param ... Additional arguments passed to \code{\link{print.lm2}}
#'
#' @return Invisibly returns the original object
#' @export
summary.lm2 <- function(object, ...) {
  print(object, ...)
  message2("print() and summary() show the same information for lm2()", col = "blue")
  invisible(object)
}

#' Predict method for lm2 objects
#'
#' @param object An object of class \code{lm2}
#' @param ... Additional arguments passed to \code{\link[estimatr]{predict.lm_robust}},
#'   including \code{newdata}, \code{se.fit}, and \code{interval}.
#'
#' @return A vector of predicted values (or a data frame if \code{se.fit = TRUE} or 
#'   \code{interval} is specified)
#' @export
predict.lm2 <- function(object, ...) {
  # The lm2 object inherits from lm_robust, so we can use NextMethod()
  # to call predict.lm_robust
  NextMethod("predict")
}
