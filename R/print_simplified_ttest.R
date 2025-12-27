#' Print method for simplified t-test output
#'
#' @param x An object of class \code{simplified_ttest}
#' @param quiet Logical. If TRUE, suppresses missing value messages. Default is FALSE.
#' @param ... Additional arguments passed to print
#'
#' @export
print.simplified_ttest <- function(x, quiet = FALSE, ...) {
  # Determine if it's a paired test
  is_paired <- grepl("Paired", x$method, ignore.case = TRUE)
  
  # Header
  if (is_paired) {
    cat("Paired t-test results\n\n")
  } else {
    cat("t-test results\n\n")
  }
  
  # Determine test type and alternative
  is_welch <- grepl("Welch", x$method, ignore.case = TRUE)
  test_type <- if (is_welch) "WELCH T-TEST" else "STUDENT T-TEST"
  
  # Determine one-sided vs two-sided
  alternative_text <- if (grepl("two.sided", x$alternative, ignore.case = TRUE)) {
    "two-sided"
  } else if (grepl("greater", x$alternative, ignore.case = TRUE)) {
    "one-sided (greater)"
  } else if (grepl("less", x$alternative, ignore.case = TRUE)) {
    "one-sided (less)"
  } else {
    "two-sided"
  }
  
  # Get digits (default to 3 if not set)
  digits <- if (!is.null(x$digits)) x$digits else 3
  
  # Variables to store missing value information for display at the end
  missing_msg <- NULL
  
  # MEANS section
  # Handle paired tests first
  if (is_paired && !is.null(x$x_var) && !is.null(x$y_var) && 
      length(x$x_var) > 0 && length(x$y_var) > 0) {
    # Paired t-test: show means of both variables, difference, and correlation
    cat("MEANS\n")
    
    # Check for missing values (store for later display)
    missing_x <- sum(is.na(x$x_var))
    missing_y <- sum(is.na(x$y_var))
    if (missing_x > 0 || missing_y > 0) {
      # Get variable names for the message
      x_name <- if (!is.null(x$x.name)) x$x.name else "group 1"
      y_name <- if (!is.null(x$y.name)) x$y.name else "group 2"
      missing_msg <- list(missing_x = missing_x, missing_y = missing_y, 
                         x_name = x_name, y_name = y_name)
    }
    
    mean_x <- mean(x$x_var, na.rm = TRUE)
    mean_y <- mean(x$y_var, na.rm = TRUE)
    diff_paired <- mean_x - mean_y
    
    # Get variable names
    x_name <- if (!is.null(x$x.name)) x$x.name else "x"
    y_name <- if (!is.null(x$y.name)) x$y.name else "y"
    
    cat("   ", x_name, ": ", format(round(mean_x, digits), nsmall = digits), "\n", sep = "")
    cat("   ", y_name, ": ", format(round(mean_y, digits), nsmall = digits), "\n", sep = "")
    cat("   ", x_name, "-", y_name, ": ", format(round(diff_paired, digits), nsmall = digits), "\n", sep = "")
    
    # Calculate and show correlation
    if (length(x$x_var) == length(x$y_var)) {
      corr_val <- cor(x$x_var, x$y_var, use = "complete.obs")
      if (!is.na(corr_val)) {
        cat("   corr(", x_name, ",", y_name, ") = ", 
            format(round(corr_val, digits), nsmall = digits), "\n", sep = "")
      }
    }
    
    # Show SE(Diff) if available
    if (!is.null(x$se_diff) && !is.na(x$se_diff)) {
      cat("   SE(Diff): ", format(round(x$se_diff, digits), nsmall = digits), "\n", sep = "")
    }
    
    # Confidence interval for difference
    if (!is.null(x$conf.int)) {
      conf_level <- format(100 * attr(x$conf.int, "conf.level"), nsmall = 1)
      cat("   ", conf_level, "% CI = [", 
          format(round(x$conf.int[1], digits), nsmall = digits), ", ", 
          format(round(x$conf.int[2], digits), nsmall = digits), "]\n", sep = "")
    }
  } else if (!is.null(x$is_formula) && x$is_formula) {
    # Formula syntax: show means by group (exactly 2 groups for t-test)
    cat("MEANS of ", x$y_var_name, ":\n", sep = "")
    
    # Check if we have the original data (from t.test2) or just estimates (from simplify)
    if (!is.null(x$group_var) && !is.null(x$y_var) && 
        length(x$group_var) > 0 && length(x$y_var) > 0) {
      # We have original data - show "When cond==0" format or simple group names
      unique_groups <- sort(unique(x$group_var))
      if (length(unique_groups) == 2) {
        g1 <- unique_groups[1]
        g2 <- unique_groups[2]
        
        # Check for missing values in each group (store for later display)
        g1_data <- x$y_var[x$group_var == g1]
        g2_data <- x$y_var[x$group_var == g2]
        missing_g1 <- sum(is.na(g1_data))
        missing_g2 <- sum(is.na(g2_data))
        if (missing_g1 > 0 || missing_g2 > 0) {
          missing_msg <- list(missing_g1 = missing_g1, missing_g2 = missing_g2)
        }
        
        mean1 <- mean(g1_data, na.rm = TRUE)
        mean2 <- mean(g2_data, na.rm = TRUE)
        
        # Check if we should show simple group names (just "a" instead of "When by==a")
        show_simple <- isTRUE(x$show_simple_groups)
        
        if (show_simple) {
          # Show just the group values
          cat("   ", g1, ": ", format(round(mean1, digits), nsmall = digits), "\n", sep = "")
          cat("   ", g2, ": ", format(round(mean2, digits), nsmall = digits), "\n", sep = "")
          
          # Show difference
          if (!is.null(x$diff)) {
            cat("   Diff: ", g1, " - ", g2, " = ", 
                format(round(x$diff, digits), nsmall = digits), "\n", sep = "")
            # Show SE(Diff) if available
            if (!is.null(x$se_diff) && !is.na(x$se_diff)) {
              cat("   SE(Diff): ", format(round(x$se_diff, digits), nsmall = digits), "\n", sep = "")
            }
          }
        } else {
          # Show "When cond==0" format
          cat("   When ", x$group_var_name, " == ", g1, ": ", 
              format(round(mean1, digits), nsmall = digits), "\n", sep = "")
          cat("   When ", x$group_var_name, " == ", g2, ": ", 
              format(round(mean2, digits), nsmall = digits), "\n", sep = "")
          
          # Show difference
          if (!is.null(x$diff)) {
            cat("   Diff: (", x$group_var_name, " == ", g1, 
                ") - (", x$group_var_name, " == ", g2, 
                ") = ", format(round(x$diff, digits), nsmall = digits), "\n", sep = "")
            # Show SE(Diff) if available
            if (!is.null(x$se_diff) && !is.na(x$se_diff)) {
              cat("   SE(Diff): ", format(round(x$se_diff, digits), nsmall = digits), "\n", sep = "")
            }
          }
        }
      }
    } else {
      # We only have estimates - use estimates directly
      if (length(x$estimate) == 2) {
        # For formula syntax without original data, we can't show group values
        # So we'll show the means with generic labels
        cat("   ", x$group_var_name, " (group 1): ", 
            format(round(x$estimate[1], digits), nsmall = digits), 
            " ", x$group_var_name, " (group 2): ", 
            format(round(x$estimate[2], digits), nsmall = digits), "\n", sep = "")
        
        if (!is.null(x$diff)) {
          cat("   Diff: (group 1) - (group 2) = ", 
              format(round(x$diff, digits), nsmall = digits), "\n", sep = "")
          # Show SE(Diff) if available
          if (!is.null(x$se_diff) && !is.na(x$se_diff)) {
            cat("   SE(Diff): ", format(round(x$se_diff, digits), nsmall = digits), "\n", sep = "")
          }
        }
      }
    }
    
    # Confidence interval for difference
    if (!is.null(x$conf.int)) {
      conf_level <- format(100 * attr(x$conf.int, "conf.level"), nsmall = 1)
      cat("   ", conf_level, "% CI for difference = [", 
          format(round(x$conf.int[1], digits), nsmall = digits), ", ", 
          format(round(x$conf.int[2], digits), nsmall = digits), "]\n", sep = "")
    }
  } else if (!is_paired || is.null(x$x_var) || is.null(x$y_var)) {
    # Standard syntax (skip if we already handled a paired test)
    cat("MEANS\n")
    if (length(x$estimate) == 2 && !is.null(x$y.name)) {
      # Two-sample test
      # Check for missing values (store for later display)
      if (!is.null(x$x_var) && !is.null(x$y_var) && 
          length(x$x_var) > 0 && length(x$y_var) > 0) {
        missing_x <- sum(is.na(x$x_var))
        missing_y <- sum(is.na(x$y_var))
        if (missing_x > 0 || missing_y > 0) {
          x_name <- if (!is.null(x$x.name)) x$x.name else "group 1"
          y_name <- if (!is.null(x$y.name)) x$y.name else "group 2"
          missing_msg <- list(missing_x = missing_x, missing_y = missing_y, 
                             x_name = x_name, y_name = y_name)
        }
      }
      
      cat("   ", x$x.name, ": ", format(round(x$estimate[1], digits), nsmall = digits), "\n", sep = "")
      cat("   ", x$y.name, ": ", format(round(x$estimate[2], digits), nsmall = digits), "\n", sep = "")
      cat("   ", x$x.name, "-", x$y.name, ": ", format(round(x$diff, digits), nsmall = digits), "\n", sep = "")
      # Show SE(Diff) if available
      if (!is.null(x$se_diff) && !is.na(x$se_diff)) {
        cat("   SE(Diff): ", format(round(x$se_diff, digits), nsmall = digits), "\n", sep = "")
      }
      
      # Confidence interval for difference
      if (!is.null(x$conf.int)) {
        conf_level <- format(100 * attr(x$conf.int, "conf.level"), nsmall = 1)
        cat("   ", conf_level, "% CI = [", 
            format(round(x$conf.int[1], digits), nsmall = digits), ", ", 
            format(round(x$conf.int[2], digits), nsmall = digits), "]\n", sep = "")
      }
    } else if (length(x$estimate) == 1) {
      # One-sample test
      cat("   ", x$x.name, ": ", format(round(x$estimate, digits), nsmall = digits), "\n", sep = "")
      if (!is.null(x$conf.int)) {
        conf_level <- format(100 * attr(x$conf.int, "conf.level"), nsmall = 1)
        cat("   ", conf_level, "% CI for ", x$x.name, " = [", 
            format(round(x$conf.int[1], digits), nsmall = digits), ", ", 
            format(round(x$conf.int[2], digits), nsmall = digits), "]\n", sep = "")
      }
    }
  }
  
  # Test results section
  cat("\n", test_type, " (", alternative_text, ")\n", sep = "")
  cat("   t = ", format(x$statistic, digits = 4), "\n", sep = "")
  cat("   df = ", format(x$parameter, digits = 4), "\n", sep = "")
  # Always use format_pvalue() for p-value display
  if (!is.na(x$p.value)) {
    formatted_p <- format_pvalue(x$p.value, include_p = TRUE)
    cat("   ", formatted_p, "\n", sep = "")
    # If p < 0.0001, show precise p-value in scientific notation
    if (x$p.value < 0.0001) {
      raw_p_str <- format(x$p.value, scientific = TRUE, digits = 4)
      cat("   (precise p: ", raw_p_str, ")\n", sep = "")
    }
  } else {
    cat("   p-value = NA\n", sep = "")
  }
  
  # Display missing value message at the end if there are any (unless quiet=TRUE)
  if (!quiet && !is.null(missing_msg)) {
    if (!is.null(missing_msg$missing_x) && !is.null(missing_msg$missing_y)) {
      # Paired or standard syntax
      message2("There are ", missing_msg$missing_x, " observations missing in ", 
               missing_msg$x_name, " and ", missing_msg$missing_y, " in ", 
               missing_msg$y_name, col = "red")
    } else if (!is.null(missing_msg$missing_g1) && !is.null(missing_msg$missing_g2)) {
      # Formula syntax
      message2("There are ", missing_msg$missing_g1, " observations missing in group 1 and ", 
               missing_msg$missing_g2, " in group 2", col = "red")
    }
  }
  
  invisible(x)
}
