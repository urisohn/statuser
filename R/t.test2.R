#' Enhanced alternative to t.test()
#' 
#'  The basic t-test function in R, \code{\link[stats]{t.test}}, does not report 
#'  the observed difference of means, does not stipulate which mean
#'  is subtracted from which (i.e., whether it computed A-B or B-A), and
#'  presents the test results on the console in a verbose unorganized 
#'  paragraph of text. \code{t.test2} improves on all those counts, and 
#'  in addition, it reports the number of observations per group and if any observations 
#'  are missing it issues a warning. It returns a dataframe instead of a list.
#'
#' @param ... Arguments passed to \code{\link[stats]{t.test}}
#'
#' @return A data frame with class \code{c("t.test2", "data.frame")} containing 
#'   a single row with the following columns:
#'   \describe{
#'     \item{mean columns}{One or two columns containing group means, named after 
#'       the input variables (e.g., \code{men}, \code{women}) or \code{Group 1}, 
#'       \code{Group 2} for long names.}
#'     \item{diff column}{For two-sample tests, the difference between means 
#'       (e.g., \code{men-women}).}
#'     \item{ci}{The confidence level as a string (e.g., "95 percent").}
#'     \item{ci.L, ci.H}{Lower and upper bounds of the confidence interval.}
#'     \item{t}{The t-statistic.}
#'     \item{df}{Degrees of freedom.}
#'     \item{p.value}{The p-value.}
#'     \item{N columns}{Sample sizes, named \code{N(group1)}, \code{N(group2)} or 
#'       \code{N1}, \code{N2}. For paired tests, a single \code{N} column.}
#'     \item{correlation}{For paired tests only, the correlation between pairs.}
#'   }
#'   Attributes store additional information including missing value counts and 
#'   test type (one-sample, two-sample, paired, Welch vs. Student).
#'
#' @importFrom stats cor
#' @usage NULL
#'
#' @examples
#' # Two-sample t-test
#' men <- rnorm(100, mean = 5, sd = 1)
#' women <- rnorm(100, mean = 4.8, sd = 1)
#' t.test2(men, women)
#'
#' # Paired t-test
#' x <- rnorm(50, mean = 5, sd = 1)
#' y <- rnorm(50, mean = 5.2, sd = 1)
#' t.test2(x, y, paired = TRUE)
#'
#' # One-sample t-test
#' data <- rnorm(100, mean = 5, sd = 1)
#' t.test2(data, mu = 0)
#'
#' # Formula syntax
#' data <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
#' t.test2(y ~ group, data = data)
#'
#' @export t.test2
t.test2 <- function(..., digits = NULL) {
  
  # Note: digits parameter is captured separately so it doesn't go into ...
  # This allows us to pass ... directly to t.test without do.call
  
  # Get dots_list for validation and later use (extracting group names, etc.)
  dots_list <- list(...)
  
  # Validate formula early if first argument is one
  if (length(dots_list) > 0) {
    validate_formula(dots_list[[1]], dots_list$data, func_name = "t.test2", calling_env = parent.frame())
  }
  
  # Run t.test with provided arguments (avoiding do.call to prevent expensive deparse)
  tt_result <- stats::t.test(...)
  
  # Determine test type
  is_paired <- grepl("Paired", tt_result$method, ignore.case = TRUE)
  is_one_sample <- length(tt_result$estimate) == 1 && !is_paired
  
  # Extract means
  mean1 <- if (length(tt_result$estimate) >= 1) as.numeric(tt_result$estimate[1]) else NA_real_
  mean2 <- if (length(tt_result$estimate) >= 2) as.numeric(tt_result$estimate[2]) else NA_real_
  
  # Extract test statistics
  t_stat <- as.numeric(tt_result$statistic)
  df <- as.numeric(tt_result$parameter)
  p_value <- as.numeric(tt_result$p.value)
  
  # Extract confidence interval
  conf_int <- tt_result$conf.int
  ci_L <- if (!is.null(conf_int) && length(conf_int) >= 1) as.numeric(conf_int[1]) else NA_real_
  ci_H <- if (!is.null(conf_int) && length(conf_int) >= 2) as.numeric(conf_int[2]) else NA_real_
  conf_level <- if (!is.null(conf_int)) attr(conf_int, "conf.level") else NA_real_
  ci_level <- if (!is.na(conf_level)) paste0(100 * conf_level, "%") else NA_character_
  
  # Calculate difference of means
  diff <- if (!is.na(mean1) && !is.na(mean2)) mean1 - mean2 else NA_real_
  
  # Initialize group names and sample sizes
  group1 <- NA_character_
  group2 <- NA_character_
  group_var_name <- NA_character_  # Store grouping variable name for integer groups
  N1 <- NA_integer_
  N2 <- NA_integer_
  NA1 <- NA_integer_
  NA2 <- NA_integer_
  NA_paired <- NA_integer_  # Number of dropped pairs for paired tests
  corr_value <- NA_real_
  
  # Extract data to get group names and sample sizes
  call_args <- sys.call()
  calling_env <- parent.frame()
  dots_list <- list(...)
  
  # Helper function to extract variable name from expression
  extract_var_name <- function(expr) {
    # Fast path for simple symbols (common case) - avoid expensive deparse()
    if (is.symbol(expr)) {
      return(as.character(expr))
    }
    # For complex expressions (e.g., df$col), use deparse
    expr_str <- deparse(expr, width.cutoff = 500)
    if (grepl("\\$", expr_str)) {
      var_name <- sub(".*\\$", "", expr_str)
      return(trimws(var_name))
    }
    return(trimws(expr_str))
  }
  
  # Helper function to check if variables were read from environment when data= was specified
  # Takes a list of variable info: list(list(name="var1", from_env=TRUE, from_data=FALSE), ...)
  # Returns warning message or NULL (warning is printed by print method at the end)
  check_env_vars <- function(var_info_list, data_arg, call_args) {
    if (is.null(data_arg) || !is.data.frame(data_arg)) {
      return(NULL)
    }
    
    # Collect variables that need warnings
    impacted_vars <- character(0)
    for (var_info in var_info_list) {
      if (!is.null(var_info$name) && isTRUE(var_info$from_env) && !isTRUE(var_info$from_data)) {
        impacted_vars <- c(impacted_vars, var_info$name)
      }
    }
    
    # If any variables are impacted, return the warning message
    if (length(impacted_vars) > 0) {
      data_name <- if ("data" %in% names(call_args)) {
        deparse(call_args$data)
      } else {
        "data"
      }
      
      var_list <- paste0("'", impacted_vars, "'", collapse = ", ")
      if (length(impacted_vars) == 1) {
        return(sprintf("Warning: you specified %s and data='%s' but that variable is not in that data set.\nThe t-test was run reading %s directly from the R environment.", var_list, data_name, var_list))
      } else {
        return(sprintf("Warning: you specified %s and data='%s' but those variables are not in that data set.\nThe t-test was run reading %s directly from the R environment.", var_list, data_name, var_list))
      }
    }
    return(NULL)
  }
  
  # Variable to store env warning message (will be added as attribute)
  env_warning_msg <- NULL
  
  # Try to extract data and group names
  tryCatch({
    # Check if it's formula syntax
    if (length(call_args) >= 2 && is.call(call_args[[2]]) && 
        as.character(call_args[[2]][[1]]) %in% c("~", "formula")) {
      # Formula syntax: y ~ group
      formula <- eval(call_args[[2]], envir = calling_env)
      data_arg <- if ("data" %in% names(call_args)) eval(call_args$data, envir = calling_env) else NULL
      
      # Extract variable names from formula
      y_var_name <- extract_var_name(formula[[2]])
      group_var_name <- extract_var_name(formula[[3]])
      
      # Check if the formula uses $ notation (e.g., df$minutes.watched ~ df$cond)
      y_expr_str <- deparse(formula[[2]], width.cutoff = 500)
      group_expr_str <- deparse(formula[[3]], width.cutoff = 500)
      y_uses_dollar <- grepl("\\$", y_expr_str)
      group_uses_dollar <- grepl("\\$", group_expr_str)
      
      # Extract y and group variables
      # Match t.test() behavior: check environment first, then data frame
      # This is important because t.test() uses environment variables if they exist,
      # even when data= is specified
      
      # For $ notation, evaluate the full expression directly
      if (y_uses_dollar) {
        y_var <- eval(formula[[2]], envir = calling_env)
        y_from_env <- TRUE
        y_from_data <- FALSE
      } else {
        y_from_env <- exists(y_var_name, envir = calling_env, inherits = TRUE)
        y_from_data <- !is.null(data_arg) && is.data.frame(data_arg) && y_var_name %in% names(data_arg)
        
        if (y_from_env) {
          y_var <- eval(as.name(y_var_name), envir = calling_env)
        } else if (y_from_data) {
          y_var <- data_arg[[y_var_name]]
        } else {
          y_var <- eval(as.name(y_var_name), envir = calling_env)  # Will throw error if not found
        }
      }
      
      if (group_uses_dollar) {
        group_var <- eval(formula[[3]], envir = calling_env)
        group_from_env <- TRUE
        group_from_data <- FALSE
      } else {
        group_from_env <- exists(group_var_name, envir = calling_env, inherits = TRUE)
        group_from_data <- !is.null(data_arg) && is.data.frame(data_arg) && group_var_name %in% names(data_arg)
        
        if (group_from_env) {
          group_var <- eval(as.name(group_var_name), envir = calling_env)
        } else if (group_from_data) {
          group_var <- data_arg[[group_var_name]]
        } else {
          group_var <- eval(as.name(group_var_name), envir = calling_env)  # Will throw error if not found
        }
      }
      
      # Check if data= is specified but variables were read from environment instead
      env_warning_msg <- check_env_vars(
        list(
          list(name = y_var_name, from_env = y_from_env, from_data = y_from_data),
          list(name = group_var_name, from_env = group_from_env, from_data = group_from_data)
        ),
        data_arg, call_args
      )
      
      # Get unique groups
      unique_groups <- sort(unique(group_var))
      if (length(unique_groups) == 2) {
        # Check if groups are integers
        g1_val <- unique_groups[1]
        g2_val <- unique_groups[2]
        
        # Check if both are integers (numeric and equal to their integer value)
        is_g1_int <- is.numeric(g1_val) && !is.na(g1_val) && g1_val == as.integer(g1_val)
        is_g2_int <- is.numeric(g2_val) && !is.na(g2_val) && g2_val == as.integer(g2_val)
        
        if (is_g1_int && is_g2_int) {
          # Format as "varname=value"
          group1 <- paste0(group_var_name, "=", as.character(g1_val))
          group2 <- paste0(group_var_name, "=", as.character(g2_val))
        } else {
          group1 <- as.character(g1_val)
          group2 <- as.character(g2_val)
        }
        
        g1_data <- y_var[group_var == unique_groups[1]]
        g2_data <- y_var[group_var == unique_groups[2]]
        
        N1 <- sum(!is.na(g1_data))
        N2 <- sum(!is.na(g2_data))
        NA1 <- sum(is.na(g1_data))
        NA2 <- sum(is.na(g2_data))
        
        # Recalculate means from data
        mean1 <- mean(g1_data, na.rm = TRUE)
        mean2 <- mean(g2_data, na.rm = TRUE)
        diff <- mean1 - mean2
        
        # Calculate correlation for paired tests and track dropped pairs
        if (is_paired && length(g1_data) == length(g2_data)) {
          complete <- complete.cases(g1_data, g2_data)
          NA_paired <- sum(!complete)  # Number of pairs dropped due to missing values
          if (sum(complete) > 1) {
            corr_value <- cor(g1_data[complete], g2_data[complete])
          }
        }
      }
    } else {
      # Standard syntax: x, y or just x
      x_arg <- NULL
      y_arg <- NULL
      x_expr <- NULL
      y_expr <- NULL
      x_from_env <- FALSE
      x_from_data <- FALSE
      y_from_env <- FALSE
      y_from_data <- FALSE
      x_var_name <- NULL
      y_var_name <- NULL
      data_arg <- if ("data" %in% names(call_args)) eval(call_args$data, envir = calling_env) else NULL
      
      # Find x and y in the call
      if ("x" %in% names(call_args)) {
        x_arg <- eval(call_args$x, envir = calling_env)
        x_expr <- call_args$x
        # For named arguments, assume they come from environment (they're evaluated directly)
        if (is.symbol(x_expr)) {
          x_var_name <- as.character(x_expr)
          x_from_env <- exists(x_var_name, envir = calling_env, inherits = TRUE)
          x_from_data <- !is.null(data_arg) && is.data.frame(data_arg) && x_var_name %in% names(data_arg)
        }
      }
      
      if ("y" %in% names(call_args)) {
        y_arg <- eval(call_args$y, envir = calling_env)
        y_expr <- call_args$y
        # For named arguments, assume they come from environment (they're evaluated directly)
        if (is.symbol(y_expr)) {
          y_var_name <- as.character(y_expr)
          y_from_env <- exists(y_var_name, envir = calling_env, inherits = TRUE)
          y_from_data <- !is.null(data_arg) && is.data.frame(data_arg) && y_var_name %in% names(data_arg)
        }
      }
      
      # If not found as named arguments, try positional arguments
      if (is.null(x_arg) && length(call_args) >= 2) {
        arg2_name <- names(call_args)[2]
        if (is.null(arg2_name) || arg2_name == "") {
          x_expr <- call_args[[2]]
          
          if (is.symbol(x_expr)) {
            x_var_name <- as.character(x_expr)
            # Match t.test() behavior: check environment first, then data frame
            x_from_env <- exists(x_var_name, envir = calling_env, inherits = TRUE)
            x_from_data <- !is.null(data_arg) && is.data.frame(data_arg) && x_var_name %in% names(data_arg)
            
            if (x_from_env) {
              x_arg <- eval(x_expr, envir = calling_env)
            } else if (x_from_data) {
              x_arg <- data_arg[[x_var_name]]
            } else {
              x_arg <- eval(x_expr, envir = calling_env)  # Will throw error if not found
            }
          } else {
            x_arg <- eval(x_expr, envir = calling_env)
          }
        }
      }
      
      if (is.null(y_arg) && length(call_args) >= 3) {
        arg3_name <- names(call_args)[3]
        if (is.null(arg3_name) || arg3_name == "") {
          y_expr <- call_args[[3]]
          
          if (is.symbol(y_expr)) {
            y_var_name <- as.character(y_expr)
            # Match t.test() behavior: check environment first, then data frame
            y_from_env <- exists(y_var_name, envir = calling_env, inherits = TRUE)
            y_from_data <- !is.null(data_arg) && is.data.frame(data_arg) && y_var_name %in% names(data_arg)
            
            if (y_from_env) {
              y_arg <- eval(y_expr, envir = calling_env)
            } else if (y_from_data) {
              y_arg <- data_arg[[y_var_name]]
            } else {
              y_arg <- eval(y_expr, envir = calling_env)  # Will throw error if not found
            }
          } else {
            y_arg <- eval(y_expr, envir = calling_env)
          }
        }
      }
      
      # Check if data= is specified but variables were read from environment instead
      env_warning_msg <- check_env_vars(
        list(
          list(name = x_var_name, from_env = x_from_env, from_data = x_from_data),
          list(name = y_var_name, from_env = y_from_env, from_data = y_from_data)
        ),
        data_arg, call_args
      )
      
      # Extract group names (only for two-sample tests)
      if (!is_one_sample) {
        if (!is.null(x_expr)) {
          group1 <- extract_var_name(x_expr)
        } else if (!is.null(x_arg)) {
          group1 <- "x"
        }
        
        if (!is.null(y_expr)) {
          group2 <- extract_var_name(y_expr)
        } else if (!is.null(y_arg)) {
          group2 <- "y"
        }
      }
      
      # Calculate sample sizes and means from data
      if (!is.null(x_arg) && is.numeric(x_arg)) {
        N1 <- sum(!is.na(x_arg))
        NA1 <- sum(is.na(x_arg))
        # Recalculate mean from data (important for paired tests)
        mean1 <- mean(x_arg, na.rm = TRUE)
      }
      
      if (!is.null(y_arg) && is.numeric(y_arg)) {
        N2 <- sum(!is.na(y_arg))
        NA2 <- sum(is.na(y_arg))
        # Recalculate mean from data (important for paired tests)
        mean2 <- mean(y_arg, na.rm = TRUE)
      }
      
      # For paired tests, calculate correlation and track dropped pairs
      if (is_paired && !is.null(x_arg) && !is.null(y_arg) && 
          is.numeric(x_arg) && is.numeric(y_arg) &&
          length(x_arg) == length(y_arg)) {
        complete <- complete.cases(x_arg, y_arg)
        NA_paired <- sum(!complete)  # Number of pairs dropped due to missing values
        if (sum(complete) > 1) {
          corr_value <- cor(x_arg[complete], y_arg[complete])
        }
        # Recalculate diff from cleaned data
        if (sum(complete) > 0) {
          diff <- mean(x_arg[complete] - y_arg[complete], na.rm = TRUE)
        }
      } else if (!is.na(mean1) && !is.na(mean2)) {
        # Recalculate diff for non-paired tests
        diff <- mean1 - mean2
      }
    }
  }, error = function(e) {
    # If we can't extract data, use defaults
    # Group names will remain NA or "x"/"y"
  })
  
  # Set group1 and group2 to NA for one-sample tests
  if (is_one_sample) {
    group1 <- NA_character_
    group2 <- NA_character_
  }
  
  #Decide suffix for groups
  ng <- if (!is.na(group1) && !is.na(group2)) max(nchar(group1), nchar(group2), na.rm = TRUE) else 0
  
  # Store original group names before potentially replacing them
  orig_group1 <- group1
  orig_group2 <- group2
  show_group_mapping <- FALSE
  
  # Handle one-sample tests separately
  if (is_one_sample) {
    name_1 <- "mean"
    name_2 <- NA_character_
    name_N1 <- "N"
    name_N2 <- NA_character_
  } else if (ng <= 8 && !is.na(group1) && !is.na(group2)) {
    name_1 <- group1
    name_2 <- group2
    name_N1 <- paste0("N(", group1, ")")
    name_N2 <- paste0("N(", group2, ")")
  } else {
    name_1 <- "Group 1"
    name_2 <- "Group 2"
    name_N1 <- "N1"
    name_N2 <- "N2"
    show_group_mapping <- TRUE
  }

  # Build data.frame with dynamic column names
  # Use a list first, then convert to dataframe to allow dynamic column names
  result_list <- list()
  result_list[[name_1]] <- mean1
  
  # For one-sample tests, don't add group2 or diff columns
  if (!is_one_sample) {
    result_list[[name_2]] <- mean2
    
    # Create diff column name as "group1-group2" (e.g., "men-women")
    # If using "Group 1" and "Group 2", use "1-2" instead
    if (!is.na(mean2)) {
      if (name_1 == "Group 1" && name_2 == "Group 2") {
        diff_col_name <- "1-2"
      } else {
        diff_col_name <- paste0(name_1, "-", name_2)
      }
      result_list[[diff_col_name]] <- diff
    }
  }
  
  result_list$ci <- ci_level
  result_list$ci.L <- ci_L
  result_list$ci.H <- ci_H
  result_list$t <- t_stat
  result_list$df <- df
  result_list$p.value <- p_value
  
  # For paired tests, only show one N column (both sample sizes are the same)
  if (is_paired) {
    result_list$N <- N1
    # Add correlation for paired tests with name "r(var1,var2)"
    corr_col_name <- paste0("r(", name_1, ",", name_2, ")")
    result_list[[corr_col_name]] <- corr_value
  } else if (is_one_sample) {
    # For one-sample tests, only show N (not N1 or N2)
    result_list[[name_N1]] <- N1
  } else {
    result_list[[name_N1]] <- N1
    result_list[[name_N2]] <- N2
  }
  
  # Convert list to dataframe
  # Use check.names = FALSE to preserve spaces in column names (e.g., "N A" instead of "N.A")
  result <- as.data.frame(result_list, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Determine method type (Welch vs Student)
  is_welch <- grepl("Welch", tt_result$method, ignore.case = TRUE)
  method_type <- if (is_welch) "welch" else "student"
  
  # Store attributes for print method
  attr(result, "is_one_sample") <- is_one_sample
  attr(result, "is_paired") <- is_paired
  attr(result, "method_type") <- method_type
  attr(result, "NA1") <- NA1
  attr(result, "NA2") <- NA2
  attr(result, "NA_paired") <- NA_paired
  attr(result, "group1") <- group1
  attr(result, "group2") <- group2
  attr(result, "name_1") <- name_1
  attr(result, "name_2") <- name_2
  attr(result, "name_N1") <- name_N1
  attr(result, "name_N2") <- name_N2
  attr(result, "show_group_mapping") <- show_group_mapping
  if (show_group_mapping) {
    attr(result, "orig_group1") <- orig_group1
    attr(result, "orig_group2") <- orig_group2
  }
  attr(result, "env_warning") <- env_warning_msg
  
  # Add class for print method
  class(result) <- c("t.test2", class(result))
  
  return(result)
}
