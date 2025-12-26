#' Describe a variable, optionally by groups
#'
#' Computes mean, SD, number of missing observations, mode, 2nd mode, median, and quantiles
#' Returns a dataframe with one row per group
#'
#' @param y A numeric vector of values, a column name (character string or unquoted) if \code{data} is provided,
#'   or a formula of the form \code{y ~ x} or \code{y ~ x1 + x2} (for multiple grouping variables).
#' @param group Optional grouping variable, if not provided computed for the full data.
#'   Ignored if \code{y} is a formula.
#' @param data Optional data frame containing the variable(s).
#' @param decimals Number of decimal to round to. Default is 3.
#'
#' @return A data frame with one row per group (or one row if no group is specified) containing:
#'   \itemize{
#'     \item \code{group}: Group identifier
#'     \item \code{n}: Number of observations
#'     \item \code{mean}: Mean
#'     \item \code{sd}: Standard deviation
#'     \item \code{se}: Standard error
#'     \item \code{median}: Median
#'     \item \code{NA_total}: Number of observations with missing (NA) values
#'     \item \code{mode}: Most frequent value
#'     \item \code{freq_mode}: Frequency of mode
#'     \item \code{mode2}: 2nd most frequent value
#'     \item \code{freq_mode2}: Frequency of 2nd mode
#'     \item \code{min}: Minimum
#'     \item \code{max}: Maximum
#'   }
#'
#' @details
#' This function computes descriptive statistics similar to \code{psych::describeBy()}, but:
#' \itemize{
#'   \item Returns a single dataframe with one row per group (instead of a list)
#'   \item Excludes kurtosis, skewness, and range
#'   \item Includes count of missing (NA) observations
#'   \item Includes mode statistics (most frequent value and 2nd most frequent value with their frequencies)
#'   \item Adds descriptive labels to all columns using the \code{labelled} package
#' }
#'
#' @examples
#' # With grouping
#' df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
#' desc_var(y, group, data = df)
#'
#' # Without grouping (full dataset)
#' desc_var(y, data = df)
#'
#' # Direct vectors
#' y <- rnorm(100)
#' group <- rep(c("A", "B"), 50)
#' desc_var(y, group)
#'
#' # With custom decimal places
#' desc_var(y, group, data = df, decimals = 2)
#'
#' # Using formula syntax: y ~ x
#' desc_var(y ~ group, data = df)
#'
#' # Using formula syntax with multiple grouping variables: y ~ x1 + x2
#' df2 <- data.frame(y = rnorm(200), x1 = rep(c("A", "B"), 100), x2 = rep(c("X", "Y"), each = 100))
#' desc_var(y ~ x1 + x2, data = df2)
#'
#' @export
desc_var <- function(y, group = NULL, data = NULL, decimals = 3) {
  
  # Function outline:
  # 1. Check if y is a formula
  # 2. If formula, parse it and extract variables
  # 3. Capture variable expressions using rlang
  # 4. Extract variables from data frame if provided
  # 5. Validate that y is numeric
  # 6. Define helper function to compute statistics for a vector
  # 7. Compute statistics (either for full dataset or by group)
  # 8. Round numeric columns to specified decimal places
  # 9. Add descriptive labels to columns using labelled package
  # 10. Return result dataframe
  
  # 1. Check if y is a formula
  is_formula <- inherits(y, "formula")
  
  if (is_formula) {
    # Parse formula: y ~ x or y ~ x1 + x2
    formula_vars <- all.vars(y)
    
    if (length(formula_vars) < 1) {
      stop("Formula must have at least one variable")
    }
    
    # First variable is response, rest are grouping variables
    y_var_name <- formula_vars[1]
    group_var_names <- formula_vars[-1]
    
    if (length(group_var_names) == 0) {
      # No grouping variables: y ~ 1 or just y
      group_var_names <- NULL
    }
    
    # Get calling environment for evaluating variables
    calling_env <- parent.frame()
    
    if (!is.null(data)) {
      if (!is.data.frame(data)) {
        stop("'data' must be a data frame")
      }
      
      # Extract response variable
      if (!y_var_name %in% names(data)) {
        stop(sprintf("Column '%s' not found in data", y_var_name))
      }
      y <- data[[y_var_name]]
      
      # Extract grouping variables
      if (!is.null(group_var_names)) {
        for (gv in group_var_names) {
          if (!gv %in% names(data)) {
            stop(sprintf("Column '%s' not found in data", gv))
          }
        }
        # Create interaction of all grouping variables
        group_list <- lapply(group_var_names, function(gv) data[[gv]])
        names(group_list) <- group_var_names
        group <- do.call(interaction, c(group_list, sep = " & "))
      } else {
        group <- NULL
      }
    } else {
      # No data frame: evaluate in calling environment
      y <- tryCatch({
        get(y_var_name, envir = calling_env)
      }, error = function(e) {
        stop(sprintf("Variable '%s' not found: %s", y_var_name, e$message))
      })
      
      if (!is.null(group_var_names)) {
        group_list <- lapply(group_var_names, function(gv) {
          tryCatch({
            get(gv, envir = calling_env)
          }, error = function(e) {
            stop(sprintf("Variable '%s' not found: %s", gv, e$message))
          })
        })
        names(group_list) <- group_var_names
        group <- do.call(interaction, c(group_list, sep = " & "))
      } else {
        group <- NULL
      }
    }
    
    # Set group_was_provided based on whether we have grouping variables
    group_was_provided <- !is.null(group)
  } else {
    # Not a formula: use original logic
    # 2. Capture variable expressions using rlang
    y_quo <- rlang::enquo(y)
    group_was_provided <- !missing(group)
    group_quo <- if (group_was_provided) rlang::enquo(group) else NULL
    
    # Get variable names for error messages
    y_name <- rlang::as_name(y_quo)
    group_name <- if (group_was_provided) rlang::as_name(group_quo) else NULL
  
    
    # 3. Extract variables from data frame if provided
    if (!is.null(data)) {
      if (!is.data.frame(data)) {
        stop("'data' must be a data frame")
      }
      
      # Evaluate y in data context
      y <- tryCatch({
        rlang::eval_tidy(y_quo, data = data)
      }, error = function(e) {
        stop(sprintf("Column '%s' not found in data: %s", y_name, e$message))
      })
      
      # Evaluate group in data context if provided
      if (group_was_provided) {
        group <- tryCatch({
          rlang::eval_tidy(group_quo, data = data)
        }, error = function(e) {
          stop(sprintf("Column '%s' not found in data: %s", group_name, e$message))
        })
      } else {
        group <- NULL
      }
    } else {
      # No data frame provided - evaluate in calling environment
      y <- tryCatch({
        rlang::eval_tidy(y_quo)
      }, error = function(e) {
        stop(sprintf("Could not evaluate 'y': %s", e$message))
      })
      
      if (group_was_provided) {
        group <- tryCatch({
          rlang::eval_tidy(group_quo)
        }, error = function(e) {
          stop(sprintf("Could not evaluate 'group': %s", e$message))
        })
        
        # Validate group exists and has correct length
        if (is.null(group)) {
          group <- NULL
        } else if (length(group) != length(y)) {
          stop(sprintf("'group' must have the same length as 'y' (got %d, expected %d)", 
                       length(group), length(y)))
        }
      } else {
        group <- NULL
      }
    }
  }
  
  # 3. Validate that y is numeric
  if (!is.numeric(y)) {
    stop(sprintf("'y' must be numeric, but '%s' is not", y_name))
  }
    
    
  
  # 4. Helper function to compute statistics for a vector
      compute_stats <- function(x) {
        na_count <- sum(is.na(x))
        x <- x[!is.na(x)]  # Remove NAs
        n <- length(x)
        
        # Calculate mode statistics
        mode_val <- NA_real_
        freq_mode <- NA_integer_
        mode2nd_val <- NA_real_
        freq_mode2nd <- NA_integer_
        
        if (n > 0) {
          # Check if all values are unique (continuous variable with no repeats)
          n_unique <- length(unique(x))
          all_unique <- n_unique == n
          
          # Only compute mode statistics if there are repeated values
          if (!all_unique) {
            # Count frequencies
            tab <- table(x)
            ord <- order(tab, decreasing = TRUE)
            first_two <- names(tab)[ord[1:min(2, length(ord))]]
            
            if (length(first_two) > 0) {
              # Mode (most frequent value)
              mode_val <- as.numeric(first_two[1])
              freq_mode <- as.integer(tab[ord[1]])
              
              # 2nd mode (2nd most frequent value)
              if (length(first_two) > 1) {
                mode2nd_val <- as.numeric(first_two[2])
                freq_mode2nd <- as.integer(tab[ord[2]])
              }
            }
          }
          # If all_unique is TRUE, mode statistics remain NA
        }
        
        # Compute statistics (use NA values when n == 0)
        mean_val <- if (n > 0) mean(x) else NA_real_
        sd_val <- if (n > 0) sd(x) else NA_real_
        se_val <- if (n > 0) sd_val / sqrt(n) else NA_real_
        median_val <- if (n > 0) median(x) else NA_real_
        min_val <- if (n > 0) min(x) else NA_real_
        max_val <- if (n > 0) max(x) else NA_real_
        
        # Return single list structure
        result <- list(
          n = as.integer(n),
          mean = as.numeric(mean_val),
          sd = as.numeric(sd_val),
          se = as.numeric(se_val),
          median = as.numeric(median_val),
          na = as.integer(na_count),
          min = as.numeric(min_val),
          max = as.numeric(max_val)
        )
        
        # Only include mode statistics if there are repeated values
        if (n > 0 && !all_unique) {
          result$mode <- as.numeric(mode_val)
          result$freq_mode <- as.integer(freq_mode)
          result$mode2 <- as.numeric(mode2nd_val)
          result$freq_mode2 <- as.integer(freq_mode2nd)
        }
        
        return(result)
      }
  
  # 5. Compute statistics
  if (is.null(group)) {
    # No grouping: compute for full dataset
    stats <- compute_stats(y)
    
    # Build base data frame
    result_df <- data.frame(
      group = "All",
      n = stats$n,
      mean = stats$mean,
      sd = stats$sd,
      se = stats$se,
      median = stats$median,
      NA_total = stats$na,
      min = stats$min,
      max = stats$max,
      stringsAsFactors = FALSE
    )
    
    # Add mode columns only if they exist (i.e., if there are repeated values)
    if ("mode" %in% names(stats)) {
      result_df$mode <- stats$mode
      result_df$freq_mode <- stats$freq_mode
      result_df$mode2 <- stats$mode2
      result_df$freq_mode2 <- stats$freq_mode2
    } else {
      message2("Note: mode not reported because all values are unique")
    }
  } else {
    # Grouping: compute for each group
    if (length(group) != length(y)) {
      stop("'group' must have the same length as 'y'")
    }
    
    unique_groups <- sort(unique(group))
    result_list <- list()
    has_mode_stats <- FALSE
    
    # First pass: check if any group has mode stats
    for (g in unique_groups) {
      y_group <- y[group == g]
      stats <- compute_stats(y_group)
      if ("mode" %in% names(stats)) {
        has_mode_stats <- TRUE
        break
      }
    }
    
    # Second pass: build data frames with consistent columns
    for (g in unique_groups) {
      y_group <- y[group == g]
      stats <- compute_stats(y_group)
      
      # Build base data frame
      result_row <- data.frame(
        group = as.character(g),
        n = stats$n,
        mean = stats$mean,
        sd = stats$sd,
        se = stats$se,
        median = stats$median,
        NA_total = stats$na,
        min = stats$min,
        max = stats$max,
        stringsAsFactors = FALSE
      )
      
      # Add mode columns only if any group has mode stats (use NA if this group doesn't)
      if (has_mode_stats) {
        if ("mode" %in% names(stats)) {
          result_row$mode <- stats$mode
          result_row$freq_mode <- stats$freq_mode
          result_row$mode2 <- stats$mode2
          result_row$freq_mode2 <- stats$freq_mode2
        } else {
          result_row$mode <- NA_real_
          result_row$freq_mode <- NA_integer_
          result_row$mode2 <- NA_real_
          result_row$freq_mode2 <- NA_integer_
        }
      }
      
      result_list[[length(result_list) + 1]] <- result_row
    }
    
    result_df <- do.call(rbind, result_list)
    
    # Show message if no group has mode stats
    if (!has_mode_stats) {
      message2("Note: mode not reported because all values are unique")
    }
  }
  
  # 6. Round numeric columns (except n, NA_total, freq_mode, and freq_mode2, which are integers) to specified decimals
  numeric_cols <- c("mean", "sd", "se", "median", "mode", "mode2", "min", "max")
  numeric_cols <- numeric_cols[numeric_cols %in% names(result_df)]
  result_df[numeric_cols] <- lapply(result_df[numeric_cols], round, digits = decimals)
  
  # 7. Add descriptive labels to columns using labelled package
  label_list <- list(
    group = "Group identifier",
    n = "Number of observations",
    mean = "Mean",
    sd = "Standard deviation",
    se = "Standard error",
    median = "Median (50th percentile)",
    NA_total = "Number of observations with missing values (NA)",
    mode = "Most frequent value",
    freq_mode = "Frequency of mode",
    mode2 = "2nd most frequent value",
    freq_mode2 = "Frequency of 2nd mode",
    min = "Minimum value",
    max = "Maximum value"
  )
  
  # Only include labels for columns that exist
  labelled::var_label(result_df) <- label_list[names(result_df)]
  
  rownames(result_df) <- NULL
  
  # 8. Return result dataframe (will print if called directly, won't print if assigned)
  return(result_df)
}

