#' Describe a variable, optionally by groups
#'
#' Computes mean, SD, number of missing observations, mode, 2nd mode, median, and quantiles
#' Returns a dataframe with one row per group
#'
#' @param y A numeric vector of values, or a column name (character string or unquoted) if \code{data} is provided.
#' @param group Optional grouping variable, if not provided  computed for the full data
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
  
  # Function outline:
  # 1. Capture variable expressions using rlang
  # 2. Extract variables from data frame if provided
  # 3. Validate that y is numeric
  # 4. Define helper function to compute statistics for a vector
  # 5. Compute statistics (either for full dataset or by group)
  # 6. Round numeric columns to specified decimal places
  # 7. Add descriptive labels to columns using labelled package
  # 8. Return result dataframe
  
  # 1. Capture variable expressions using rlang
  y_quo <- rlang::enquo(y)
  group_was_provided <- !missing(group)
  group_quo <- if (group_was_provided) rlang::enquo(group) else NULL
  
  # Get variable names for error messages
  y_name <- rlang::as_name(y_quo)
  group_name <- if (group_was_provided) rlang::as_name(group_quo) else NULL
  
  # 2. Extract variables from data frame if provided
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
        
        # Compute statistics (use NA values when n == 0)
        mean_val <- if (n > 0) mean(x) else NA_real_
        sd_val <- if (n > 0) sd(x) else NA_real_
        se_val <- if (n > 0) sd_val / sqrt(n) else NA_real_
        median_val <- if (n > 0) median(x) else NA_real_
        if (n > 0) {
          qs <- quantile(x, c(0.05, 0.10, 0.90, 0.95), names = FALSE)
          q5_val <- qs[1]
          q10_val <- qs[2]
          q90_val <- qs[3]
          q95_val <- qs[4]
        } else {
          q5_val <- q10_val <- q90_val <- q95_val <- NA_real_
        }
        min_val <- if (n > 0) min(x) else NA_real_
        max_val <- if (n > 0) max(x) else NA_real_
        
        # Return single list structure
        list(
          n = as.integer(n),
          mean = as.numeric(mean_val),
          sd = as.numeric(sd_val),
          se = as.numeric(se_val),
          median = as.numeric(median_val),
          na = as.integer(na_count),
          mode = as.numeric(mode_val),
          freq_mode = as.integer(freq_mode),
          mode2 = as.numeric(mode2nd_val),
          freq_mode2 = as.integer(freq_mode2nd),
          q5 = as.numeric(q5_val),
          q10 = as.numeric(q10_val),
          q90 = as.numeric(q90_val),
          q95 = as.numeric(q95_val),
          min = as.numeric(min_val),
          max = as.numeric(max_val)
        )
      }
  
  # 5. Compute statistics
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
  
  # 6. Round numeric columns (except n, NA_total, freq_mode, and freq_mode2, which are integers) to specified decimals
  numeric_cols <- c("mean", "sd", "se", "median", "mode", "mode2", "q5", "q10", "q90", "q95", "min", "max")
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
    q5 = "5th percentile",
    q10 = "10th percentile",
    q90 = "90th percentile",
    q95 = "95th percentile",
    min = "Minimum value",
    max = "Maximum value"
  )
  
  labelled::var_label(result_df) <- label_list[names(result_df)]
  
  rownames(result_df) <- NULL
  
  # 8. Return result dataframe (will print if called directly, won't print if assigned)
  return(result_df)
}

