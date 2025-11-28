#' t-test with improved output over stats::t.test()
#'
#' Performs a t-test and displays results with actual variable names instead of
#' generic "mean of x" and "mean of y" labels. Also adds the observed difference
#' of means to the output.
#'
#' @param x A numeric vector of data values, a formula of the form \code{y ~ group},
#'   or a (non-empty) numeric vector of data values.
#' @param y An optional numeric vector of data values, or a data frame if \code{x}
#'   is a formula.
#' @param alternative A character string specifying the alternative hypothesis,
#'   must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#'   You can specify just the initial letter.
#' @param mu A number indicating the true value of the mean (or difference in means
#'   if you are performing a two sample test). Default is 0.
#' @param paired A logical indicating whether you want a paired t-test. Default is \code{FALSE}.
#' @param var.equal A logical variable indicating whether to treat the two variances
#'   as being equal. If \code{TRUE} then the pooled variance is used to estimate the
#'   variance otherwise the Welch (or Satterthwaite) approximation to the degrees
#'   of freedom is used. Default is \code{FALSE}.
#' @param conf.level Confidence level of the interval. Default is 0.95.
#' @param digits Number of decimal places to display for means and difference
#'   of means. Default is 3.
#' @param ... Additional arguments passed to \code{\link[stats]{t.test}}.
#'
#' @return An object of class \code{c("t.test2.enhanced", "htest")} containing
#'   all standard \code{t.test} components plus:
#'   \itemize{
#'     \item \code{diff}: The observed difference of means (x - y)
#'     \item \code{x.name}: The name of the x variable
#'     \item \code{y.name}: The name of the y variable
#'   }
#'
#' @details
#' This function wraps \code{\link[stats]{t.test}} and enhances the output by:
#' \itemize{
#'   \item Displaying actual variable names instead of "mean of x" and "mean of y"
#'   \item Adding the observed difference of means (x - y) to the output
#'   \item Storing the difference in \code{result$diff} for programmatic access
#' }
#'
#' @examples
#' # Two-sample t-test with named variables
#' men <- rnorm(100, mean = 5, sd = 1)
#' women <- rnorm(100, mean = 4.8, sd = 1)
#' result <- t.test2(men, women)
#' result$diff  # Access the difference
#'
#' # One-sample t-test
#' x <- rnorm(50, mean = 10, sd = 2)
#' t.test2(x, mu = 9)
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @export
t.test2 <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
                    mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, 
                    digits = 3, ...) {
  # Check if x is a formula
  if (inherits(x, "formula")) {
    # Formula syntax: y ~ group
    formula <- x
    data <- y  # data frame should be in y position
    
    # Extract variable names from formula
    formula_str <- deparse(formula)
    y_var_name <- all.vars(formula)[1]
    group_var_name <- all.vars(formula)[2]
    
    # Get data from environment or provided data frame
    if (is.null(data)) {
      # Try to get from parent environment
      y_var <- eval(formula[[2]], envir = parent.frame())
      group_var <- eval(formula[[3]], envir = parent.frame())
    } else {
      # Extract from data frame
      y_var <- data[[y_var_name]]
      group_var <- data[[group_var_name]]
    }
    
    # Call t.test with formula
    result <- stats::t.test(formula, data = data, alternative = alternative, 
                            mu = mu, paired = paired, var.equal = var.equal, 
                            conf.level = conf.level, ...)
    
    # Store variable names and group information
    result$y_var_name <- y_var_name
    result$group_var_name <- group_var_name
    result$y_var <- y_var
    result$group_var <- group_var
    result$is_formula <- TRUE
    
    # Calculate means by group
    unique_groups <- sort(unique(group_var))
    result$group_means <- sapply(unique_groups, function(g) mean(y_var[group_var == g], na.rm = TRUE))
    names(result$group_means) <- unique_groups
    
    # Calculate difference (first group - second group) to match display format
    if (length(result$group_means) == 2) {
      result$diff <- result$group_means[1] - result$group_means[2]
    } else {
      result$diff <- NULL
    }
    
  } else {
    # Standard syntax: t.test(x, y)
    # Capture variable names before calling t.test
    x_name <- deparse(substitute(x))
    
    # Check if y was actually provided (not just default NULL)
    y_provided <- !missing(y) && !is.null(y)
    y_name <- if (y_provided) deparse(substitute(y)) else NULL
    
    # Call the original t.test
    result <- stats::t.test(x, y, alternative = alternative, mu = mu, 
                            paired = paired, var.equal = var.equal, 
                            conf.level = conf.level, ...)
    
    # Store variable names
    result$x.name <- x_name
    result$y.name <- y_name
    result$is_formula <- FALSE
    
    # Calculate and store the difference
    if (y_provided && length(result$estimate) == 2) {
      # Two-sample test
      result$diff <- result$estimate[1] - result$estimate[2]
    } else {
      # One-sample test - no difference to calculate
      result$diff <- NULL
    }
  }
  
  # Store digits for printing
  result$digits <- digits
  
  # Change class to add our custom print method
  class(result) <- c("t.test2.enhanced", class(result))
  
  return(result)
}

#' Print method for enhanced t-test
#'
#' @param x An object of class \code{t.test2.enhanced}
#' @param ... Additional arguments passed to print
#'
#' @export
print.t.test2.enhanced <- function(x, ...) {
  # Header
  cat("t-test results\n")
  cat("(produced by `sohn::t.test2`)\n\n")
  
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
    if (!is.null(x$group_var) && !is.null(x$y_var)) {
      # We have original data - show "When cond==0" format
      unique_groups <- sort(unique(x$group_var))
      g1 <- unique_groups[1]
      g2 <- unique_groups[2]
      mean1 <- mean(x$y_var[x$group_var == g1], na.rm = TRUE)
      mean2 <- mean(x$y_var[x$group_var == g2], na.rm = TRUE)
      
      cat("   When ", x$group_var_name, "==", g1, ": ", 
          format(round(mean1, digits), nsmall = digits), 
          " When ", x$group_var_name, "==", g2, ": ", 
          format(round(mean2, digits), nsmall = digits), "\n", sep = "")
      
      # Show difference
      if (!is.null(x$diff)) {
        cat("   Diff: (", x$group_var_name, "==", g1, 
            ") - (", x$group_var_name, "==", g2, 
            ") = ", format(round(x$diff, digits), nsmall = digits), "\n", sep = "")
      }
    } else {
      # We only have estimates - use simpler format
      if (length(x$estimate) == 2) {
        cat("   ", x$group_var_name, " (group 1): ", 
            format(round(x$estimate[1], digits), nsmall = digits), 
            " ", x$group_var_name, " (group 2): ", 
            format(round(x$estimate[2], digits), nsmall = digits), "\n", sep = "")
        
        if (!is.null(x$diff)) {
          cat("   Diff: (group 1) - (group 2) = ", 
              format(round(x$diff, digits), nsmall = digits), "\n", sep = "")
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
  cat("   p-value=", format.pval(x$p.value), "\n", sep = "")
  
  invisible(x)
}


