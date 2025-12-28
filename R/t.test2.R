#' Enhanced t-test function
#'
#' Runs \code{\link[stats]{t.test}} and returns results as a dataframe
#' while displaying simplified, readable output with variable names.
#'
#' @param ... Arguments passed to \code{\link[stats]{t.test}}
#' @param digits Number of decimal places to display. Default is 3.
#'
#' @return A dataframe (returned invisibly) with columns named after the variables
#'   or group values, plus: diff, SE_diff, conf.intL, conf.intH, level, t, df, p.value, se1, se2, method
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
#'   \item level: Confidence level of the confidence interval (e.g., 95 for 95\%)
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
  
  # Check if we need to convert to formula syntax (y, group case)
  # If second positional argument exists and is not numeric, treat as grouping variable
  dots_list <- list(...)
  needs_formula_conversion <- FALSE
  y_var_for_formula <- NULL
  group_var_for_formula <- NULL
  
  if (length(dots_list) >= 2) {
    # Check if first two arguments are vectors (not named)
    arg_names <- names(dots_list)
    if (is.null(arg_names) || (length(arg_names) >= 2 && (arg_names[1] == "" || arg_names[2] == ""))) {
      arg1 <- dots_list[[1]]
      arg2 <- dots_list[[2]]
      
      # If arg1 is numeric and arg2 is not numeric (likely a grouping variable)
      if (is.numeric(arg1) && !is.numeric(arg2) && length(arg1) == length(arg2)) {
        needs_formula_conversion <- TRUE
        y_var_for_formula <- arg1
        group_var_for_formula <- arg2
        
        # Create temporary data frame and use formula syntax
        temp_df <- data.frame(y_var = arg1, group_var = arg2, stringsAsFactors = FALSE)
        formula_obj <- y_var ~ group_var
        
        # Create modified dots list with formula and data
        modified_dots <- list(formula_obj, data = temp_df)
        # Add any additional arguments (skip first two)
        if (length(dots_list) > 2) {
          # Preserve named arguments
          additional_args <- dots_list[-(1:2)]
          # Filter out 'data' if present (we're providing our own)
          if ("data" %in% names(additional_args)) {
            additional_args <- additional_args[names(additional_args) != "data"]
          }
          modified_dots <- c(modified_dots, additional_args)
        }
        
        # Call t.test with formula syntax
        tt_result <- do.call(stats::t.test, modified_dots)
      } else {
        # Standard case: pass through to t.test
        tt_result <- stats::t.test(...)
      }
    } else {
      # Standard case: pass through to t.test
      tt_result <- stats::t.test(...)
    }
  } else {
    # Standard case: pass through to t.test
    tt_result <- stats::t.test(...)
  }
  
  # TASK 3: Extract test results: means, test statistics, and method type
  # Determine test type (Student vs Welch) from method string
    is_welch <- grepl("Welch", tt_result$method, ignore.case = TRUE)
    is_paired <- grepl("Paired", tt_result$method, ignore.case = TRUE)
    method_type <- if (is_welch) "welch" else "student"
  
  # Extract means (remove names to get plain numeric values)
  # tt_result$estimate is a named vector, so convert to numeric to remove names
  # For paired tests, estimate only contains the mean difference, not both means
  # We'll calculate the individual means from the data later if needed
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
  # Extract confidence level (e.g., 0.95 for 95% CI)
    conf_level <- if (!is.null(conf_int)) attr(conf_int, "conf.level") else NA_real_
    level <- if (!is.na(conf_level)) paste0(100 * conf_level, "%") else NA_character_
  
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
  # Variables for paired tests
  x_arg <- NULL
  y_arg <- NULL
  # Variables to store N (non-NA counts)
  N1 <- NA_integer_
  N2 <- NA_integer_
  # Variable to store correlation
  corr_value <- NA_real_
  # Variables to store missing value counts (for warnings)
  n_missing_1 <- 0L
  n_missing_2 <- 0L
  
  # Try to extract data from the call for standard error calculation
  tryCatch({
    # Check if we converted to formula syntax (needs_formula_conversion case)
    if (needs_formula_conversion && !is.null(y_var_for_formula) && !is.null(group_var_for_formula)) {
      # Use the variables we already extracted
      y_var <- y_var_for_formula
      group_var <- group_var_for_formula
      
      # Calculate standard errors for each group and get group values for column names
      unique_groups <- sort(unique(group_var))
      if (length(unique_groups) == 2) {
        g1_data <- y_var[group_var == unique_groups[1]]
        g2_data <- y_var[group_var == unique_groups[2]]
        
        # Check for missing values and store counts
        n_missing_g1 <- sum(is.na(g1_data))
        n_missing_g2 <- sum(is.na(g2_data))
        n_missing_1 <- n_missing_g1
        n_missing_2 <- n_missing_g2
        
        # TASK 5: Calculate standard errors (sd / sqrt(n))
        se1 <- sd(g1_data, na.rm = TRUE) / sqrt(length(g1_data))
        se2 <- sd(g2_data, na.rm = TRUE) / sqrt(length(g2_data))
        
        # Calculate N (non-NA counts)
        N1 <- sum(!is.na(g1_data))
        N2 <- sum(!is.na(g2_data))
        
        # TASK 4: Use group values as column names (e.g., "a", "b")
        col1_name <- as.character(unique_groups[1])
        col2_name <- as.character(unique_groups[2])
      }
    } else if (length(call_args) >= 2 && is.call(call_args[[2]]) && 
        as.character(call_args[[2]][[1]]) %in% c("~", "formula")) {
      # Check if it's formula syntax (y ~ group)
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
      
      # Calculate standard errors for each group and get group values for column names
      unique_groups <- sort(unique(group_var))
      if (length(unique_groups) == 2) {
        g1_data <- y_var[group_var == unique_groups[1]]
        g2_data <- y_var[group_var == unique_groups[2]]
        
        # Check for missing values and store counts
        n_missing_g1 <- sum(is.na(g1_data))
        n_missing_g2 <- sum(is.na(g2_data))
        n_missing_1 <- n_missing_g1
        n_missing_2 <- n_missing_g2
        
        # TASK 5: Calculate standard errors (sd / sqrt(n))
        se1 <- sd(g1_data, na.rm = TRUE) / sqrt(length(g1_data))
        se2 <- sd(g2_data, na.rm = TRUE) / sqrt(length(g2_data))
        
        # Calculate N (non-NA counts)
        N1 <- sum(!is.na(g1_data))
        N2 <- sum(!is.na(g2_data))
        
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
      # Also calculate means from original data (important for paired tests where estimate only has diff)
      # For paired tests, remove pairs where either value is missing
      if (is_paired && !is.null(x_arg) && !is.null(y_arg) && 
          is.numeric(x_arg) && is.numeric(y_arg) &&
          length(x_arg) == length(y_arg)) {
        # Find complete cases (both values present)
        complete <- complete.cases(x_arg, y_arg)
        n_dropped <- sum(!complete)
        
        # Report dropped observations
        if (n_dropped > 0) {
          message2(paste0("Dropped ", n_dropped, " observation(s) with missing values in paired t-test"), col = "red")
        }
        
        # Use only complete cases for paired tests
        x_arg <- x_arg[complete]
        y_arg <- y_arg[complete]
      }
      
      if (!is.null(x_arg) && is.numeric(x_arg)) {
        # Check for missing values and store counts (only for non-paired tests)
        if (!is_paired) {
          n_missing_x <- sum(is.na(x_arg))
          n_missing_1 <- n_missing_x
        }
        se1 <- sd(x_arg, na.rm = TRUE) / sqrt(length(x_arg))
        N1 <- sum(!is.na(x_arg))
        # Calculate mean from data (for paired tests, estimate only has mean difference)
        mean1 <- mean(x_arg, na.rm = TRUE)
      }
      
      if (!is.null(y_arg) && is.numeric(y_arg)) {
        # Check for missing values and store counts (only for non-paired tests)
        if (!is_paired) {
          n_missing_y <- sum(is.na(y_arg))
          n_missing_2 <- n_missing_y
        }
        se2 <- sd(y_arg, na.rm = TRUE) / sqrt(length(y_arg))
        N2 <- sum(!is.na(y_arg))
        # Calculate mean from data (for paired tests, estimate only has mean difference)
        mean2 <- mean(y_arg, na.rm = TRUE)
      }
      
      # Calculate correlation between x and y (only for paired tests)
      if (is_paired && !is.null(x_arg) && !is.null(y_arg) && 
          is.numeric(x_arg) && is.numeric(y_arg) &&
          length(x_arg) == length(y_arg) && length(x_arg) > 1) {
        # For paired tests, x_arg and y_arg are already cleaned of missing values
        corr_value <- cor(x_arg, y_arg)
      }
      
      # Recalculate diff from the means we just calculated
      if (!is.na(mean1) && !is.na(mean2)) {
        diff <- mean1 - mean2
        # Recalculate SE(diff) after recalculating diff (important for paired tests)
        if (!is.na(diff) && !is.na(t_stat) && t_stat != 0) {
          se_diff <- abs(diff / t_stat)
        }
      }
    }
  }, error = function(e) {
    # If we can't extract data, leave se1 and se2 as NA
    # Column names will default to "mean1" and "mean2"
    # This can happen in some edge cases
  })
  
  # TASK 6: BUILD DATAFRAME - Create dataframe with dynamic column names based on variables/groups
  # Build a list first, then convert to dataframe (creates 1-row dataframe automatically)
  result_list <- list()
  
  # Add columns with dynamic names (variable names or group values)
  result_list[[col1_name]] <- mean1
  if (!is.na(mean2)) {
    result_list[[col2_name]] <- mean2
    # Add correlation column after group2 (only for paired tests)
    if (is_paired) {
      result_list$corr <- corr_value
    }
  }
  
  # Add difference column with dynamic name (var1-var2 format)
  if (!is.na(mean2)) {
    # For two-sample tests, use variable names in diff column name (e.g., "x1-x2")
    # Use "-" directly (not " - ") and check.names=FALSE will preserve it
    diff_col_name <- paste0(col1_name, "-", col2_name)
    result_list[[diff_col_name]] <- diff
    # Add confidence interval columns after diff
    result_list$ci <- level
    result_list$ci.L <- conf_intL
    result_list$ci.H <- conf_intH
  } else {
    # For one-sample test, diff is NA, so use default name
    result_list$diff <- diff
    # Add confidence interval columns for one-sample test as well
    result_list$ci <- level
    result_list$ci.L <- conf_intL
    result_list$ci.H <- conf_intH
  }
  
  # Add other test statistics columns
  result_list$t <- t_stat
  result_list$df <- df
  result_list$p.value <- p_value
  # Add N columns right after p.value
  # Check if we're using formula syntax by checking data.name
  is_formula_syntax <- grepl(" by | ~ ", tt_result$data.name, ignore.case = TRUE)
  if (!is.na(mean2)) {
    # Two-sample test - use group values for formula syntax, variable names for standard syntax
    # For formula syntax, col1_name and col2_name already contain the group values (e.g., "A", "B")
    result_list[[paste0("N_", col1_name)]] <- N1
    result_list[[paste0("N_", col2_name)]] <- N2
  } else {
    # One-sample test
    result_list[[paste0("N_", col1_name)]] <- N1
  }
  result_list$method <- method_type
  # Use variable names for SE columns (e.g., se_wannda, se_yolannda)
  result_list[[paste0("se_", col1_name)]] <- se1
  if (!is.na(mean2)) {
    result_list[[paste0("se_", col2_name)]] <- se2
    # Add SE(diff) column at the end, after SE columns
    se_diff_col_name <- paste0("SE_", col1_name, "-", col2_name)
    result_list[[se_diff_col_name]] <- se_diff
  }
  
  # Convert list to dataframe (creates a 1-row dataframe)
  # Use check.names = FALSE to preserve column names with special characters like "-"
  result_df <- as.data.frame(result_list, stringsAsFactors = FALSE, check.names = FALSE)
  
  # TASK 8: Display dataframe on console with formatting
  # Create a copy for display
  display_df <- result_df
  
  # Check if group names are too long (>= 5 characters) and replace with "Group 1" and "Group 2"
  # Store original names for potential message in print method
  orig_col1 <- NULL
  orig_col2 <- NULL
  if (!is.na(mean2) && nchar(col1_name) >= 5 && nchar(col2_name) >= 5) {
    # Store original names for message
    orig_col1 <- col1_name
    orig_col2 <- col2_name
    
    # Create mapping for column name replacements
    col_mapping <- list()
    col_mapping[[col1_name]] <- "Group 1"
    col_mapping[[col2_name]] <- "Group 2"
    col_mapping[[paste0("N_", col1_name)]] <- "N1"
    col_mapping[[paste0("N_", col2_name)]] <- "N2"
    col_mapping[[paste0("se_", col1_name)]] <- "SE1"
    col_mapping[[paste0("se_", col2_name)]] <- "SE2"
    col_mapping[[paste0(col1_name, "-", col2_name)]] <- "1-2"
    col_mapping[[paste0("SE_", col1_name, "-", col2_name)]] <- "SE(1-2)"
    
    # Rename columns in display_df
    new_names <- names(display_df)
    for (i in seq_along(new_names)) {
      if (new_names[i] %in% names(col_mapping)) {
        new_names[i] <- col_mapping[[new_names[i]]]
      }
    }
    names(display_df) <- new_names
  }
  
  # Helper function to format values based on size
  format_value <- function(val, is_df = FALSE) {
    if (is.na(val) || !is.numeric(val)) {
      return(val)
    }
    if (is_df) {
      # df always has 1 decimal
      return(round(val, 1))
    }
    # For means, CIs, SEs: >100 = 1 decimal, >10 = 2 decimals, else 3 decimals
    abs_val <- abs(val)
    if (abs_val > 100) {
      return(round(val, 1))
    } else if (abs_val > 10) {
      return(round(val, 2))
    } else {
      return(round(val, 3))
    }
  }
  
  # Format each column based on type
  for (col in names(display_df)) {
    if (col == "p.value") {
      # Format p-value using format_pvalue
      if (!is.na(display_df$p.value) && is.numeric(result_df$p.value)) {
        formatted_p <- format_pvalue(display_df$p.value, include_p = FALSE)
        # Remove "= " and trim spaces after "<" and ">"
        formatted_p <- gsub("= ", "", formatted_p)
        formatted_p <- gsub("< ", "<", formatted_p)
        formatted_p <- gsub("> ", ">", formatted_p)
        display_df$p.value <- formatted_p
      }
    } else if (col == "df") {
      # df always has 1 decimal
      if (is.numeric(display_df[[col]])) {
        display_df[[col]] <- format_value(display_df[[col]], is_df = TRUE)
      }
    } else if (grepl("^N_", col)) {
      # N columns are integers, no decimals needed
      if (is.numeric(display_df[[col]])) {
        display_df[[col]] <- round(display_df[[col]], 0)
      }
    } else if (col == "method" || col == "ci") {
      # Character columns, no formatting
      next
    } else if (is.numeric(display_df[[col]])) {
      # Means, CIs, SEs: apply value-based formatting
      display_df[[col]] <- format_value(display_df[[col]])
    }
  }
  
  # Remove last 4 columns from console output: method and the 3 SE columns
  cols_to_remove <- c("method")
  # Find SE columns to remove (check both original names and potentially renamed ones)
  se_cols_to_check <- c(
    paste0("se_", col1_name),
    paste0("se_", col2_name),
    paste0("SE_", col1_name, "-", col2_name),
    "SE1", "SE2", "SE(1-2)"  # In case names were replaced
  )
  for (col in se_cols_to_check) {
    if (col %in% names(display_df)) {
      cols_to_remove <- c(cols_to_remove, col)
    }
  }
  # Remove columns from display_df
  display_df <- display_df[, !names(display_df) %in% cols_to_remove, drop = FALSE]
  
  # Store display information as attributes for print method
  attr(result_df, "display_df") <- display_df
  attr(result_df, "col1_name") <- col1_name
  attr(result_df, "col2_name") <- col2_name
  attr(result_df, "show_group_mapping") <- !is.null(orig_col1) && !is.null(orig_col2)
  if (attr(result_df, "show_group_mapping")) {
    attr(result_df, "orig_col1") <- orig_col1
    attr(result_df, "orig_col2") <- orig_col2
  }
  # Store missing value counts for warnings (only for non-paired tests)
  if (!is_paired) {
    attr(result_df, "n_missing_1") <- n_missing_1
    attr(result_df, "n_missing_2") <- n_missing_2
  }
  
  # Add class for print method
  class(result_df) <- c("simplified_ttest", class(result_df))
  
  # Return dataframe visibly (original, unrounded values)
  # When not assigned, R will automatically call print.simplified_ttest()
  # When assigned, the print method won't be called
  return(result_df)
}

