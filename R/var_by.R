#' Descriptive statistics by group (or full dataset)
#'
#' Computes descriptive statistics for a variable, optionally grouped by another variable.
#' Returns a dataframe with one row per group (or one row for the full dataset if no group is specified).
#'
#' @param y A numeric vector of values, or a column name (character string or unquoted) if \code{data} is provided.
#' @param group Optional grouping variable, or a column name (character string or unquoted) if \code{data} is provided.
#'   If not provided, statistics are computed for the full dataset.
#' @param data Optional data frame containing the variables \code{y} and optionally \code{group}.
#' @param decimals Number of decimal places to display for numeric statistics. Default is 3.
#'
#' @return A data frame with one row per group (or one row if no group is specified) containing:
#'   \itemize{
#'     \item \code{group}: Group identifier (or "All" if no grouping)
#'     \item \code{n}: Number of observations
#'     \item \code{mean}: Mean
#'     \item \code{sd}: Standard deviation
#'     \item \code{se}: Standard error
#'     \item \code{median}: Median
#'     \item \code{NA_total}: Number of missing (NA) observations
#'     \item \code{mode}: Most frequent value
#'     \item \code{freq_mode}: Frequency of mode
#'     \item \code{mode2}: 2nd most frequent value
#'     \item \code{freq_mode2}: Frequency of 2nd mode
#'     \item \code{q5, q10, q90, q95}: Quantiles (5th, 10th, 90th, 95th percentiles)
#'     \item \code{min}: Minimum
#'     \item \code{max}: Maximum
#'   }
#'
#' @details
#' This function computes descriptive statistics similar to \code{psych::describeBy()}, but:
#' \itemize{
#'   \item Returns a single dataframe with one row per group (instead of a list)
#'   \item Excludes kurtosis, skewness, and range
#'   \item Includes specific quantiles: 5, 10, 90, 95
#'   \item Includes count of missing (NA) observations
#'   \item Includes mode statistics (most frequent value and 2nd most frequent value with their frequencies)
#'   \item Adds descriptive labels to all columns using the \code{labelled} package
#' }
#'
#' @examples
#' # With grouping
#' df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
#' var_by(y, group, data = df)
#'
#' # Without grouping (full dataset)
#' var_by(y, data = df)
#'
#' # Direct vectors
#' y <- rnorm(100)
#' group <- rep(c("A", "B"), 50)
#' var_by(y, group)
#'
#' # With custom decimal places
#' var_by(y, group, data = df, decimals = 2)
#'
#' @export
var_by <- function(y, group = NULL, data = NULL, decimals = 3) {
  
  # Capture variable names before evaluation
  y_expr <- substitute(y)
  y_name_raw <- deparse(y_expr)
  y_name_raw <- gsub('^"|"$', '', y_name_raw)
  
  # Handle group: capture name BEFORE evaluating
  group_expr <- substitute(group)
  group_name_raw <- NULL
  # Check if group was actually provided (not just using default NULL)
  group_was_provided <- !is.null(group_expr) && !missing(group)
  
  if (group_was_provided) {
    if (is.character(group_expr) && length(group_expr) == 1) {
      group_name_raw <- group_expr
    } else {
      group_name_raw <- deparse(group_expr)
      group_name_raw <- gsub('^"|"$', '', group_name_raw)
    }
  }
  
  # Extract variables from data frame if provided
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame")
    }
    
    # Extract y column from data frame
    if (!y_name_raw %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", y_name_raw))
    }
    y <- data[[y_name_raw]]
    
    # Extract group column from data frame if provided
    if (group_was_provided) {
      # Check if group_name_raw looks like a column name
      if (grepl("[()\\[\\]\\+\\-\\*/]", group_name_raw)) {
        # group appears to be a vector expression, try to evaluate it
        tryCatch({
          group <- eval(group_expr, envir = parent.frame())
        }, error = function(e) {
          stop(sprintf("Could not evaluate 'group' expression '%s'", group_name_raw))
        })
        # If evaluated to NULL, treat as no grouping
        if (is.null(group)) {
          group <- NULL
        }
      } else {
        # Check if it's explicitly NULL
        if (group_name_raw == "NULL") {
          group <- NULL
        } else {
          # group appears to be a column name, extract from data
          if (!group_name_raw %in% names(data)) {
            stop(sprintf("Column '%s' not found in data", group_name_raw))
          }
          group <- data[[group_name_raw]]
        }
      }
    } else {
      group <- NULL
    }
  } else {
    # No data frame provided
    if (!group_was_provided) {
      group <- NULL
    }
  }
  
  # Validate that y is numeric
  if (!is.numeric(y)) {
    stop(sprintf("'y' must be numeric, but '%s' is not", y_name_raw))
  }
  
  # Helper function to compute statistics for a vector
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
      # Count frequencies
      freq_table <- table(x)
      freq_sorted <- sort(freq_table, decreasing = TRUE)
      
      if (length(freq_sorted) > 0) {
        # Mode (most frequent value)
        mode_val <- as.numeric(names(freq_sorted)[1])
        freq_mode <- as.integer(freq_sorted[1])
        
        # 2nd mode (2nd most frequent value)
        if (length(freq_sorted) > 1) {
          mode2nd_val <- as.numeric(names(freq_sorted)[2])
          freq_mode2nd <- as.integer(freq_sorted[2])
        }
      }
    }
    
    if (n == 0) {
      return(list(
        n = as.integer(0),
        mean = NA_real_,
        sd = NA_real_,
        se = NA_real_,
        median = NA_real_,
        na = as.integer(na_count),
        mode = as.numeric(mode_val),
        freq_mode = as.integer(freq_mode),
        mode2 = as.numeric(mode2nd_val),
        freq_mode2 = as.integer(freq_mode2nd),
        q5 = NA_real_,
        q10 = NA_real_,
        q90 = NA_real_,
        q95 = NA_real_,
        min = NA_real_,
        max = NA_real_
      ))
    }
    
    list(
      n = as.integer(n),
      mean = as.numeric(mean(x)),
      sd = as.numeric(sd(x)),
      se = as.numeric(sd(x) / sqrt(n)),
      median = as.numeric(median(x)),
      na = as.integer(na_count),
      mode = as.numeric(mode_val),
      freq_mode = as.integer(freq_mode),
      mode2 = as.numeric(mode2nd_val),
      freq_mode2 = as.integer(freq_mode2nd),
      q5 = as.numeric(quantile(x, 0.05, names = FALSE)),
      q10 = as.numeric(quantile(x, 0.10, names = FALSE)),
      q90 = as.numeric(quantile(x, 0.90, names = FALSE)),
      q95 = as.numeric(quantile(x, 0.95, names = FALSE)),
      min = as.numeric(min(x)),
      max = as.numeric(max(x))
    )
  }
  
  # Compute statistics
  if (is.null(group)) {
    # No grouping: compute for full dataset
    stats <- compute_stats(y)
    result_df <- data.frame(
      group = "All",
      n = stats$n,
      mean = stats$mean,
      sd = stats$sd,
      se = stats$se,
      median = stats$median,
      NA_total = stats$na,
      mode = stats$mode,
      freq_mode = stats$freq_mode,
      mode2 = stats$mode2,
      freq_mode2 = stats$freq_mode2,
      q5 = stats$q5,
      q10 = stats$q10,
      q90 = stats$q90,
      q95 = stats$q95,
      min = stats$min,
      max = stats$max,
      stringsAsFactors = FALSE
    )
  } else {
    # Grouping: compute for each group
    if (length(group) != length(y)) {
      stop("'group' must have the same length as 'y'")
    }
    
    unique_groups <- sort(unique(group))
    result_list <- list()
    
    for (g in unique_groups) {
      y_group <- y[group == g]
      stats <- compute_stats(y_group)
      
      result_list[[length(result_list) + 1]] <- data.frame(
        group = as.character(g),
        n = stats$n,
        mean = stats$mean,
        sd = stats$sd,
        se = stats$se,
        median = stats$median,
        NA_total = stats$na,
      mode = stats$mode,
      freq_mode = stats$freq_mode,
      mode2 = stats$mode2,
      freq_mode2 = stats$freq_mode2,
        q5 = stats$q5,
        q10 = stats$q10,
        q90 = stats$q90,
        q95 = stats$q95,
        min = stats$min,
        max = stats$max,
        stringsAsFactors = FALSE
      )
    }
    
    result_df <- do.call(rbind, result_list)
  }
  
  # Round numeric columns (except n, NA_total, freq_mode, and freq_mode2, which are integers) to specified decimals
  numeric_cols <- c("mean", "sd", "se", "median", "mode", "mode2", "q5", "q10", "q90", "q95", "min", "max")
  for (col in numeric_cols) {
    if (col %in% names(result_df)) {
      result_df[[col]] <- round(result_df[[col]], digits = decimals)
    }
  }
  
  # Add descriptive labels to columns using labelled package
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
    q5 = "5th percentile",
    q10 = "10th percentile",
    q90 = "90th percentile",
    q95 = "95th percentile",
    min = "Minimum value",
    max = "Maximum value"
  )
  
  # Only assign labels for columns that exist in result_df
  label_list <- label_list[names(label_list) %in% names(result_df)]
  
  labelled::var_label(result_df) <- label_list
  
  rownames(result_df) <- NULL
  
  # Return result_df (will print if called directly, won't print if assigned)
  return(result_df)
}

