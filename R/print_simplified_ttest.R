#' Print method for simplified t-test output
#'
#' @param x An object of class \code{simplified_ttest}
#' @param ... Additional arguments passed to print
#'
#' @export
print.simplified_ttest <- function(x, ...) {
  # Header
  cat("t-test results\n\n")
  
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
  
  # MEANS section
  if (!is.null(x$is_formula) && x$is_formula) {
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
        mean1 <- mean(x$y_var[x$group_var == g1], na.rm = TRUE)
        mean2 <- mean(x$y_var[x$group_var == g2], na.rm = TRUE)
        
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
          cat("   When ", x$group_var_name, "==", g1, ": ", 
              format(round(mean1, digits), nsmall = digits), "\n", sep = "")
          cat("   When ", x$group_var_name, "==", g2, ": ", 
              format(round(mean2, digits), nsmall = digits), "\n", sep = "")
          
          # Show difference
          if (!is.null(x$diff)) {
            cat("   Diff: (", x$group_var_name, "==", g1, 
                ") - (", x$group_var_name, "==", g2, 
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
      conf_level <- round(100 * attr(x$conf.int, "conf.level"))
      cat("   ", conf_level, "% CI for difference = [", 
          format(round(x$conf.int[1], digits), nsmall = digits), ", ", 
          format(round(x$conf.int[2], digits), nsmall = digits), "]\n", sep = "")
    }
  } else {
    # Standard syntax
    cat("MEANS\n")
    if (length(x$estimate) == 2 && !is.null(x$y.name)) {
      # Two-sample test
      cat("   ", x$x.name, ": ", format(round(x$estimate[1], digits), nsmall = digits), "\n", sep = "")
      cat("   ", x$y.name, ": ", format(round(x$estimate[2], digits), nsmall = digits), "\n", sep = "")
      cat("   ", x$x.name, "-", x$y.name, ": ", format(round(x$diff, digits), nsmall = digits), "\n", sep = "")
      # Show SE(Diff) if available
      if (!is.null(x$se_diff) && !is.na(x$se_diff)) {
        cat("   SE(Diff): ", format(round(x$se_diff, digits), nsmall = digits), "\n", sep = "")
      }
      
      # Confidence interval for difference
      if (!is.null(x$conf.int)) {
        conf_level <- round(100 * attr(x$conf.int, "conf.level"))
        cat("   ", conf_level, "% CI = [", 
            format(round(x$conf.int[1], digits), nsmall = digits), ", ", 
            format(round(x$conf.int[2], digits), nsmall = digits), "]\n", sep = "")
      }
    } else if (length(x$estimate) == 1) {
      # One-sample test
      cat("   ", x$x.name, ": ", format(round(x$estimate, digits), nsmall = digits), "\n", sep = "")
      if (!is.null(x$conf.int)) {
        conf_level <- round(100 * attr(x$conf.int, "conf.level"))
        cat("   ", conf_level, "% CI for ", x$x.name, " = [", 
            format(round(x$conf.int[1], digits), nsmall = digits), ", ", 
            format(round(x$conf.int[2], digits), nsmall = digits), "]\n", sep = "")
      }
    }
  }
  
  # Test results section
  cat("\n", test_type, " (", alternative_text, ")\n", sep = "")
  cat("   t=", format(x$statistic, digits = 4), "\n", sep = "")
  cat("   df=", format(x$parameter, digits = 4), "\n", sep = "")
  # Show formatted p-value and raw p-value (only if p < 0.001)
  if (!is.na(x$p.value) && x$p.value < 0.001) {
    formatted_p <- format_pvalue(x$p.value, include_p = TRUE)
    # Format raw p-value: always use scientific notation
    raw_p_str <- format(x$p.value, scientific = TRUE, digits = 4)
    # Remove leading zero from scientific notation (e.g., "3.400e-15" stays as is, "0.3019e-08" becomes "3.019e-09")
    # Actually, format() with scientific=TRUE already handles this correctly
    cat("   ", formatted_p, "\n   (precise p=", raw_p_str, ")\n", sep = "")
  } else {
    # For p >= 0.001, just show raw p-value
    if (!is.na(x$p.value)) {
      raw_p_str <- sprintf("%.6f", x$p.value)
      # Remove trailing zeros
      raw_p_str <- sub("0+$", "", raw_p_str)
      raw_p_str <- sub("\\.$", "", raw_p_str)
      # Remove leading zero
      raw_p_str <- sub("^0\\.", ".", raw_p_str)
      cat("   p-value=", raw_p_str, "\n", sep = "")
    } else {
      cat("   p-value=NA\n", sep = "")
    }
  }
  
  invisible(x)
}
