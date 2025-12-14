# Internal helper function to simplify t-test output for printing
# This function is used internally by t.test2() to format console output
simplify_ttest <- function(object, digits = 3, calling_env = NULL, ...) {
  if (!inherits(object, "htest")) {
    stop("object must be of class 'htest'")
  }
  
  if (!grepl("t-test", object$method, ignore.case = TRUE)) {
    stop("simplify_ttest() only works with t.test() output")
  }
  
  # Parse data.name to extract variable names
  data_name <- object$data.name
  
  # Check if it's formula syntax (contains "by" or "~")
  is_formula <- grepl(" by | ~ ", data_name)
  
  if (is_formula) {
    # Formula syntax: "y by group" or "y ~ group"
    # Split on " by " or " ~ "
    parts <- strsplit(data_name, " by | ~ ")[[1]]
    if (length(parts) == 2) {
      y_var_name <- trimws(parts[1])
      group_var_name <- trimws(parts[2])
      
      object$y_var_name <- y_var_name
      object$group_var_name <- group_var_name
      object$is_formula <- TRUE
      
      # Try to get original data from the environment
      if (is.null(calling_env)) {
        env <- tryCatch(parent.frame(), error = function(e) .GlobalEnv)
      } else {
        env <- calling_env
      }
      
      # Check if variables are already provided (from t.test2)
      if (!is.null(object$y_var) && !is.null(object$group_var)) {
        # Variables already provided, use them
        # (y_var and group_var are already set)
      } else {
        # Try to get variables from the environment
        tryCatch({
          # Try to get variables from the environment
          y_var <- tryCatch(get(y_var_name, envir = env), error = function(e) NULL)
          group_var <- tryCatch(get(group_var_name, envir = env), error = function(e) NULL)
          
          # If we got both variables and they have the same length, store them
          if (!is.null(y_var) && !is.null(group_var) && 
              length(y_var) > 0 && length(group_var) > 0 && 
              length(y_var) == length(group_var)) {
            object$y_var <- y_var
            object$group_var <- group_var
          } else {
            object$y_var <- NULL
            object$group_var <- NULL
          }
        }, error = function(e) {
          object$y_var <- NULL
          object$group_var <- NULL
        })
      }
      
      # Calculate difference
      if (length(object$estimate) == 2) {
        object$diff <- object$estimate[1] - object$estimate[2]
      }
    }
  } else {
    # Standard syntax: "x and y" or just "x"
    if (grepl(" and ", data_name)) {
      parts <- strsplit(data_name, " and ")[[1]]
      x_name <- trimws(parts[1])
      y_name <- trimws(parts[2])
      object$x.name <- x_name
      object$y.name <- y_name
      object$is_formula <- FALSE
      
      if (length(object$estimate) == 2) {
        object$diff <- object$estimate[1] - object$estimate[2]
      }
    } else {
      # One-sample test
      object$x.name <- trimws(data_name)
      object$y.name <- NULL
      object$is_formula <- FALSE
      object$diff <- NULL
    }
  }
  
  # Store digits
  object$digits <- digits
  
  # Change class to use our print method
  class(object) <- c("simplified_ttest", class(object))
  
  # Print the enhanced output
  print(object)
  
  invisible(object)
}

#' Enhanced t-test function
#'
#' Runs \code{\link[stats]{t.test}} and returns results as a dataframe
#' while displaying simplified, readable output with variable names.
#'
#' @param ... Arguments passed to \code{\link[stats]{t.test}}
#' @param digits Number of decimal places to display. Default is 3.
#'
#' @return A dataframe (returned invisibly) with columns named after the variables
#'   or group values, plus: diff, SE_diff, conf.intL, conf.intH, t, df, p.value, se1, se2, method
#'
#' @details
#' This function:
#' \itemize{
#'   \item Runs \code{t.test()} with the provided arguments
#'   \item Displays simplified, readable output with variable names
#'   \item Returns results as a dataframe (invisibly) instead of a list
#' }
#'
#' The dataframe contains:
#' \itemize{
#'   \item Variable-named columns: For standard syntax (\code{t.test2(x1, x2)}),
#'     columns are named after the variables (e.g., \code{x1}, \code{x2}).
#'     For formula syntax (\code{t.test2(y ~ group, data=df)}), columns are
#'     named after the group values (e.g., \code{low}, \code{high}).
#'   \item diff: Difference of means (mean1 - mean2, NA for one-sample test)
#'   \item SE_diff: Standard error of the difference of means (NA for one-sample test)
#'   \item conf.intL: Lower bound of confidence interval
#'   \item conf.intH: Upper bound of confidence interval
#'   \item t: t-statistic
#'   \item df: Degrees of freedom
#'   \item p.value: p-value
#'   \item se1: Standard error of first group
#'   \item se2: Standard error of second group (NA for one-sample test)
#'   \item method: 'student' or 'welch'
#' }
#'
#' @examples
#' # Two-sample t-test
#' men <- rnorm(100, mean = 5, sd = 1)
#' women <- rnorm(100, mean = 4.8, sd = 1)
#' t.test2(men, women)
#'
#' # Formula syntax
#' data <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
#' t.test2(y ~ group, data = data)
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @name t.test2
#' @export t.test2
t.test2 <- function(..., digits = 3) {
  
  # FUNCTION OUTLINE:
  # 1. Capture function call to extract variable names and original data
  # 2. Call stats::t.test() to perform the t-test
  # 3. Extract test results: means, test statistics, and method type
  # 4. EXTRACT COLUMN NAMES: Determine column names from variable names or group values
  # 5. CALCULATE STANDARD ERRORS: Extract original data to compute standard errors for each group
  # 6. DISPLAY OUTPUT: Print simplified t-test results
  # 7. BUILD DATAFRAME: Create dataframe with dynamic column names based on variables/groups
  # 8. Return dataframe invisibly (only console output is visible)
  
  # TASK 1: Capture function call to extract variable names and original data
  # Use match.call to capture the original call expressions for variable name extraction
    call_args <- match.call(expand.dots = TRUE)
    calling_env <- parent.frame()
  
  # TASK 2: Call stats::t.test() to perform the t-test
  # Pass all arguments through to stats::t.test()
    tt_result <- stats::t.test(...)
  
  # TASK 3: Extract test results: means, test statistics, and method type
  # Determine test type (Student vs Welch) from method string
    is_welch <- grepl("Welch", tt_result$method, ignore.case = TRUE)
    method_type <- if (is_welch) "welch" else "student"
  
  # Extract means (remove names to get plain numeric values)
  # tt_result$estimate is a named vector, so convert to numeric to remove names
    mean1 <- if (length(tt_result$estimate) >= 1) as.numeric(tt_result$estimate[1]) else NA_real_
    mean2 <- if (length(tt_result$estimate) >= 2) as.numeric(tt_result$estimate[2]) else NA_real_
  
  # Calculate difference of means
    diff <- if (!is.na(mean1) && !is.na(mean2)) mean1 - mean2 else NA_real_
  
  # Extract test statistics (convert to numeric to remove any names)
    t_stat <- as.numeric(tt_result$statistic)
    df <- as.numeric(tt_result$parameter)
    p_value <- as.numeric(tt_result$p.value)
  
  # Calculate SE(diff) - standard error of the difference
  # SE(diff) = diff / t (since t = diff / SE(diff))
    se_diff <- if (!is.na(diff) && !is.na(t_stat) && t_stat != 0) abs(diff / t_stat) else NA_real_
  
  # Extract confidence interval
    conf_int <- tt_result$conf.int
    conf_intL <- if (!is.null(conf_int) && length(conf_int) >= 1) as.numeric(conf_int[1]) else NA_real_
    conf_intH <- if (!is.null(conf_int) && length(conf_int) >= 2) as.numeric(conf_int[2]) else NA_real_
  
  # TASK 4: EXTRACT COLUMN NAMES - Determine column names from variable names or group values
  # Initialize column names with defaults (will be overwritten if we can extract names)
    col1_name <- "mean1"
    col2_name <- "mean2"
    se1 <- NA_real_
    se2 <- NA_real_
  
  # Helper function to extract variable name from call expression
  # Handles cases like df$x1 (extracts "x1") or just x1 (keeps "x1")
  extract_var_name <- function(expr) {
    expr_str <- deparse(expr, width.cutoff = 500)
    # If it contains $, extract the part after the last $
    if (grepl("\\$", expr_str)) {
      # Extract everything after the last $ (e.g., "x1" from "df$x1")
      var_name <- sub(".*\\$", "", expr_str)
      # Remove any whitespace
      var_name <- trimws(var_name)
      return(var_name)
    }
    # Otherwise, return the expression as-is (trimmed)
    return(trimws(expr_str))
  }
  
  # TASK 5: CALCULATE STANDARD ERRORS - Extract original data to compute standard errors
  # Variables to pass to simplify_ttest for display
  extracted_y_var <- NULL
  extracted_group_var <- NULL
  
  # Try to extract data from the call for standard error calculation
  tryCatch({
    # Check if it's formula syntax (y ~ group)
    if (length(call_args) >= 2 && is.call(call_args[[2]]) && 
        as.character(call_args[[2]][[1]]) %in% c("~", "formula")) {
      # TASK 4 & 5: Formula syntax - Extract group values for column names and calculate SEs
      # Formula syntax: y ~ group
      formula <- eval(call_args[[2]], envir = calling_env)
      data_arg <- if ("data" %in% names(call_args)) eval(call_args$data, envir = calling_env) else NULL
      
      # Extract variables from formula (with or without data argument)
      if (!is.null(data_arg)) {
        # Extract variables from formula and data
        y_var <- eval(formula[[2]], envir = data_arg)
        group_var <- eval(formula[[3]], envir = data_arg)
      } else {
        # No data argument - evaluate in calling environment
        y_var <- eval(formula[[2]], envir = calling_env)
        group_var <- eval(formula[[3]], envir = calling_env)
      }
      
      # Store extracted variables for simplify_ttest
      extracted_y_var <- y_var
      extracted_group_var <- group_var
      
      # Calculate standard errors for each group and get group values for column names
      unique_groups <- sort(unique(group_var))
      if (length(unique_groups) == 2) {
        g1_data <- y_var[group_var == unique_groups[1]]
        g2_data <- y_var[group_var == unique_groups[2]]
        
        # TASK 5: Calculate standard errors (sd / sqrt(n))
        se1 <- sd(g1_data, na.rm = TRUE) / sqrt(length(g1_data))
        se2 <- sd(g2_data, na.rm = TRUE) / sqrt(length(g2_data))
        
        # TASK 4: Use group values as column names (e.g., "a", "b")
        col1_name <- as.character(unique_groups[1])
        col2_name <- as.character(unique_groups[2])
      }
    } else {
      # TASK 4 & 5: Standard syntax - Extract variable names for column names and calculate SEs
      # Standard syntax: x, y or just x
      # Extract x and y arguments (both values and expressions)
      x_arg <- NULL
      y_arg <- NULL
      x_expr <- NULL
      y_expr <- NULL
      
      # Find x and y in the call
      # Check for named arguments first (x=..., y=...)
      if ("x" %in% names(call_args)) {
        x_arg <- eval(call_args$x, envir = calling_env)
        x_expr <- call_args$x
      }
      
      if ("y" %in% names(call_args)) {
        y_arg <- eval(call_args$y, envir = calling_env)
        y_expr <- call_args$y
      }
      
      # If not found as named arguments, try positional arguments
      # Skip first element (function name) and skip "digits" if it's a named argument
      if (is.null(x_arg) && length(call_args) >= 2) {
        arg2_name <- names(call_args)[2]
        # Only process if it's a positional argument (empty name) or a named argument that's not "digits"
        if ((is.null(arg2_name) || arg2_name == "") || 
            (!is.null(arg2_name) && arg2_name != "" && arg2_name != "digits")) {
          x_arg <- eval(call_args[[2]], envir = calling_env)
          x_expr <- call_args[[2]]
        }
      }
      
      if (is.null(y_arg) && length(call_args) >= 3) {
        arg3_name <- names(call_args)[3]
        # Only process if it's a positional argument (empty name) or a named argument that's not "digits"
        if ((is.null(arg3_name) || arg3_name == "") || 
            (!is.null(arg3_name) && arg3_name != "" && arg3_name != "digits")) {
          y_arg <- eval(call_args[[3]], envir = calling_env)
          y_expr <- call_args[[3]]
        }
      }
      
      # TASK 4: Extract variable names for column names
      if (!is.null(x_expr)) {
        col1_name <- extract_var_name(x_expr)
      }
      
      if (!is.null(y_expr)) {
        col2_name <- extract_var_name(y_expr)
      }
      
      # TASK 5: Calculate standard errors (sd / sqrt(n))
      if (!is.null(x_arg) && is.numeric(x_arg)) {
        se1 <- sd(x_arg, na.rm = TRUE) / sqrt(length(x_arg))
      }
      
      if (!is.null(y_arg) && is.numeric(y_arg)) {
        se2 <- sd(y_arg, na.rm = TRUE) / sqrt(length(y_arg))
      }
    }
  }, error = function(e) {
    # If we can't extract data, leave se1 and se2 as NA
    # Column names will default to "mean1" and "mean2"
    # This can happen in some edge cases
  })
  
  # TASK 6: DISPLAY OUTPUT - Print simplified t-test results
  # Use simplify_ttest helper function to print console output
  # Pass the calling environment and extracted variables so simplify_ttest can display means
  # Add flag to show simple group names (just "a" instead of "When by==a")
  tt_result$show_simple_groups <- TRUE
  # Pass extracted variables directly if we have them (handles df$var syntax)
  if (!is.null(extracted_y_var) && !is.null(extracted_group_var)) {
    tt_result$y_var <- extracted_y_var
    tt_result$group_var <- extracted_group_var
  }
  # Pass se_diff for console display
  tt_result$se_diff <- se_diff
  simplify_ttest(tt_result, digits = digits, calling_env = calling_env)
  
  # TASK 7: BUILD DATAFRAME - Create dataframe with dynamic column names based on variables/groups
  # Build a list first, then convert to dataframe (creates 1-row dataframe automatically)
  result_list <- list()
  
  # Add columns with dynamic names (variable names or group values)
  result_list[[col1_name]] <- mean1
  if (!is.na(mean2)) {
    result_list[[col2_name]] <- mean2
  }
  
  # Add difference column with dynamic name (var1-var2 format)
  if (!is.na(mean2)) {
    # For two-sample tests, use variable names in diff column name (e.g., "x1-x2")
    # Use "-" directly (not " - ") and check.names=FALSE will preserve it
    diff_col_name <- paste0(col1_name, "-", col2_name)
    result_list[[diff_col_name]] <- diff
    # Add SE(diff) column right after diff
    se_diff_col_name <- paste0("SE_", col1_name, "-", col2_name)
    result_list[[se_diff_col_name]] <- se_diff
    # Add confidence interval columns after SE(diff)
    result_list$conf.intL <- conf_intL
    result_list$conf.intH <- conf_intH
  } else {
    # For one-sample test, diff is NA, so use default name
    result_list$diff <- diff
    # Add confidence interval columns for one-sample test as well
    result_list$conf.intL <- conf_intL
    result_list$conf.intH <- conf_intH
  }
  
  # Add other test statistics columns
  result_list$t <- t_stat
  result_list$df <- df
  result_list$p.value <- p_value
  result_list$method <- method_type
  # Use variable names for SE columns (e.g., se_wannda, se_yolannda)
  result_list[[paste0("se_", col1_name)]] <- se1
  if (!is.na(mean2)) {
    result_list[[paste0("se_", col2_name)]] <- se2
  }
  
  # Convert list to dataframe (creates a 1-row dataframe)
  # Use check.names = FALSE to preserve column names with special characters like "-"
  result_df <- as.data.frame(result_list, stringsAsFactors = FALSE, check.names = FALSE)
  
  # TASK 8: Return dataframe invisibly (only console output is visible)
  return(invisible(result_df))
}

