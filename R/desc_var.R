#' Describe a variable, optionally by groups
#'
#' Computes mean, SD, number of missing observations, mode, 2nd mode, max, min
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
  
  # Helper function to format error messages with conditional newline
  format_msg <- function(msg) {
    total_length <- nchar(msg) + nchar("desc_var() says: ")
    if (total_length > 40) {
      return(paste0("desc_var() says:\n", msg))
    } else {
      return(paste0("desc_var() says: ", msg))
    }
  }
  
  # Helper function to validate that variables have the same length
  validate_lengths <- function(vars, y_len, var_names = NULL) {
    for (i in seq_along(vars)) {
      if (length(vars[[i]]) != y_len) {
        message2(format_msg("All variables must have the same length"), col = 'red', stop = TRUE)
      }
    }
  }
  
  # Helper function to extract variable from data or environment
  extract_var <- function(var_name, data, calling_env, error_msg) {
    if (!is.null(data)) {
      if (!var_name %in% names(data)) {
        message2(format_msg(sprintf("'%s' not found in data", var_name)), col = 'red', stop = TRUE)
      }
      return(data[[var_name]])
    } else {
      tryCatch({
        get(var_name, envir = calling_env)
      }, error = function(e) {
        message2(format_msg(sprintf("'%s' not found", var_name)), col = 'red', stop = TRUE)
      })
    }
  }
  
  # Helper function to extract and validate grouping variables
  extract_group_vars <- function(group_var_names, data, calling_env, y_len) {
    if (is.null(group_var_names) || length(group_var_names) == 0) {
      return(list(group_list = NULL, use_separate_cols = FALSE))
    }
    
    # Extract grouping variables
    group_list <- lapply(group_var_names, function(gv) {
      extract_var(gv, data, calling_env, sprintf("'%s' not found", gv))
    })
    names(group_list) <- group_var_names
    
    # Validate all variables have the same length
    validate_lengths(group_list, y_len)
    
    # For multiple grouping variables, keep them separate
    # For single grouping variable, create a single group vector
    use_separate_cols <- length(group_var_names) > 1
    
    return(list(group_list = group_list, use_separate_cols = use_separate_cols))
  }
  
  # 1. Check if y is a formula and extract variables
  is_formula <- inherits(y, "formula")
  calling_env <- parent.frame()
  
  if (is_formula) {
    # Parse formula: y ~ x or y ~ x1 + x2
    formula_vars <- all.vars(y)
    
    if (length(formula_vars) < 1) {
      message2(format_msg("Formula must have at least one variable"), col = 'red', stop = TRUE)
    }
    
    # First variable is response, rest are grouping variables
    y_var_name <- formula_vars[1]
    group_var_names <- if (length(formula_vars) > 1) formula_vars[-1] else NULL
    y_name <- y_var_name  # For consistent error messages
    
    # Validate data frame if provided
    if (!is.null(data) && !is.data.frame(data)) {
      message2(format_msg("'data' must be a data frame"), col = 'red', stop = TRUE)
    }
    
    # Extract response variable
    y <- extract_var(y_var_name, data, calling_env, sprintf("'%s' not found", y_var_name))
    y_len <- length(y)
    
    # Extract grouping variables
    group_result <- extract_group_vars(group_var_names, data, calling_env, y_len)
    group_list <- group_result$group_list
    use_separate_group_cols <- group_result$use_separate_cols
    
    # Create single group vector if single grouping variable, otherwise keep separate
    if (!is.null(group_list)) {
      if (use_separate_group_cols) {
        group <- NULL  # Keep separate, don't create interaction
      } else {
        group <- group_list[[1]]  # Single grouping variable
      }
    } else {
      group <- NULL
    }
    
  } else {
    # Not a formula: use rlang to capture expressions
    y_quo <- rlang::enquo(y)
    group_was_provided <- !missing(group)
    group_quo <- if (group_was_provided) rlang::enquo(group) else NULL
    
    # Get variable names for error messages
    y_name <- rlang::as_name(y_quo)
    group_name <- if (group_was_provided) rlang::as_name(group_quo) else NULL
    
    # Validate data frame if provided
    if (!is.null(data) && !is.data.frame(data)) {
      message2(format_msg("'data' must be a data frame"), col = 'red', stop = TRUE)
    }
    
    # Extract y variable
    y <- tryCatch({
      if (!is.null(data)) {
        rlang::eval_tidy(y_quo, data = data)
      } else {
        rlang::eval_tidy(y_quo)
      }
    }, error = function(e) {
      if (!is.null(data)) {
        message2(format_msg(sprintf("Column '%s' not found in data: %s", y_name, e$message)), col = 'red', stop = TRUE)
      } else {
        message2(format_msg(sprintf("Could not evaluate 'y': %s", e$message)), col = 'red', stop = TRUE)
      }
    })
    
    y_len <- length(y)
    
    # Extract group variable if provided
    if (group_was_provided) {
      group <- tryCatch({
        if (!is.null(data)) {
          rlang::eval_tidy(group_quo, data = data)
        } else {
          rlang::eval_tidy(group_quo)
        }
      }, error = function(e) {
        if (!is.null(data)) {
          message2(format_msg(sprintf("Column '%s' not found in data: %s", group_name, e$message)), col = 'red', stop = TRUE)
        } else {
          message2(format_msg(sprintf("Could not evaluate 'group': %s", e$message)), col = 'red', stop = TRUE)
        }
      })
      
      # Validate length
      if (!is.null(group)) {
        validate_lengths(list(group), y_len)
      }
    } else {
      group <- NULL
    }
    
    # For non-formula case, always use single group column
    group_list <- NULL
    use_separate_group_cols <- FALSE
  }
  
  # 2. Validations
  
  # 2.1. Validate that y is numeric
  if (!is.numeric(y)) {
    message2(format_msg(sprintf("The dv is numeric: '%s' is not numeric", y_name)), col = 'red', stop = TRUE)
  }
  
  # 2.2. Check that multiple grouping variables do not overlap perfectly (formula syntax only)
  if (use_separate_group_cols && !is.null(group_list) && length(group_list) > 1) {
    # Check all pairs of grouping variables for perfect overlap
    for (i in 1:(length(group_list) - 1)) {
      for (j in (i + 1):length(group_list)) {
        var1 <- group_list[[i]]
        var2 <- group_list[[j]]
        # Perfect overlap: one-to-one mapping means unique combinations equals max unique values
        n_unique_combos <- length(unique(paste(var1, var2, sep = "|")))
        n_unique_v1 <- length(unique(var1))
        n_unique_v2 <- length(unique(var2))
        if (n_unique_combos == max(n_unique_v1, n_unique_v2)) {
          var1_name <- names(group_list)[i]
          var2_name <- names(group_list)[j]
          message2(format_msg(sprintf("Multiple grouping variables do not overlap perfectly: '%s' and '%s' overlap perfectly", var1_name, var2_name)), col = 'red', stop = TRUE)
        }
      }
    }
  }
    
    
  
  # 3. Helper function to compute statistics for a vector
  compute_stats <- function(x) {
    na_count <- sum(is.na(x))
    x <- x[!is.na(x)]  # Remove NAs
    n <- length(x)
    
    # Handle empty case
    if (n == 0) {
      return(list(
        n = 0L,
        mean = NA_real_,
        sd = NA_real_,
        se = NA_real_,
        median = NA_real_,
        na = as.integer(na_count),
        min = NA_real_,
        max = NA_real_,
        mode = NA_real_,
        freq_mode = NA_integer_,
        mode2 = NA_real_,
        freq_mode2 = NA_integer_
      ))
    }
    
    # Compute basic statistics
    mean_val <- mean(x)
    sd_val <- sd(x)
    se_val <- sd_val / sqrt(n)
    median_val <- median(x)
    min_val <- min(x)
    max_val <- max(x)
    
    # Compute mode statistics only if there are repeated values
    n_unique <- length(unique(x))
    has_repeats <- n_unique < n
    
    if (has_repeats) {
      tab <- table(x)
      ord <- order(tab, decreasing = TRUE)
      mode_val <- as.numeric(names(tab)[ord[1]])
      freq_mode <- as.integer(tab[ord[1]])
      mode2nd_val <- if (length(ord) > 1) as.numeric(names(tab)[ord[2]]) else NA_real_
      freq_mode2nd <- if (length(ord) > 1) as.integer(tab[ord[2]]) else NA_integer_
    } else {
      mode_val <- NA_real_
      freq_mode <- NA_integer_
      mode2nd_val <- NA_real_
      freq_mode2nd <- NA_integer_
    }
    
    # Return consistent structure
    return(list(
      n = as.integer(n),
      mean = as.numeric(mean_val),
      sd = as.numeric(sd_val),
      se = as.numeric(se_val),
      median = as.numeric(median_val),
      na = as.integer(na_count),
      min = as.numeric(min_val),
      max = as.numeric(max_val),
      mode = as.numeric(mode_val),
      freq_mode = as.integer(freq_mode),
      mode2 = as.numeric(mode2nd_val),
      freq_mode2 = as.integer(freq_mode2nd)
    ))
  }
  
  # 4. Compute statistics
  has_grouping <- !is.null(group) || (use_separate_group_cols && !is.null(group_list))
  
  if (!has_grouping) {
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
    
    # Add mode columns only if there are repeated values (mode is not NA)
    if (!is.na(stats$mode)) {
      result_df$mode <- stats$mode
      result_df$freq_mode <- stats$freq_mode
      result_df$mode2 <- stats$mode2
      result_df$freq_mode2 <- stats$freq_mode2
    } else {
      message2(format_msg("Note: mode not reported because all values are unique"))
    }
  } else {
    # Grouping: compute for each group
    result_list <- list()
    has_mode_stats <- FALSE
    
    if (use_separate_group_cols) {
      # Multiple grouping variables: iterate over unique combinations
      # Create a data frame to get unique combinations efficiently
      group_df <- as.data.frame(group_list, stringsAsFactors = FALSE)
      unique_combos <- unique(group_df)
      
      # First pass: check if any group has mode stats
      for (i in seq_len(nrow(unique_combos))) {
        combo <- unique_combos[i, , drop = FALSE]
        # Create mask: all grouping variables match this combination
        mask <- Reduce(`&`, lapply(names(group_list), function(nm) {
          group_list[[nm]] == combo[[nm]]
        }))
        y_group <- y[mask]
        stats <- compute_stats(y_group)
        if (!is.na(stats$mode)) {
          has_mode_stats <- TRUE
          break
        }
      }
      
      # Second pass: build data frames
      for (i in seq_len(nrow(unique_combos))) {
        combo <- unique_combos[i, , drop = FALSE]
        # Create mask: all grouping variables match this combination
        mask <- Reduce(`&`, lapply(names(group_list), function(nm) {
          group_list[[nm]] == combo[[nm]]
        }))
        y_group <- y[mask]
        stats <- compute_stats(y_group)
        
        # Build data frame with grouping variables as separate columns
        result_row <- data.frame(
          combo,
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
        
        # Add mode columns if any group has mode stats
        if (has_mode_stats) {
          result_row$mode <- stats$mode
          result_row$freq_mode <- stats$freq_mode
          result_row$mode2 <- stats$mode2
          result_row$freq_mode2 <- stats$freq_mode2
        }
        
        result_list[[length(result_list) + 1]] <- result_row
      }
    } else {
      # Single group column: use group vector directly
      unique_groups <- sort(unique(group))
      
      # First pass: check if any group has mode stats
      for (g in unique_groups) {
        y_group <- y[group == g]
        stats <- compute_stats(y_group)
        if (!is.na(stats$mode)) {
          has_mode_stats <- TRUE
          break
        }
      }
      
      # Second pass: build data frames
      for (g in unique_groups) {
        y_group <- y[group == g]
        stats <- compute_stats(y_group)
        
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
        
        # Add mode columns if any group has mode stats
        if (has_mode_stats) {
          result_row$mode <- stats$mode
          result_row$freq_mode <- stats$freq_mode
          result_row$mode2 <- stats$mode2
          result_row$freq_mode2 <- stats$freq_mode2
        }
        
        result_list[[length(result_list) + 1]] <- result_row
      }
    }
    
    result_df <- do.call(rbind, result_list)
    
    # Sort by grouping variables
    if (use_separate_group_cols && !is.null(group_list)) {
      # Sort by all grouping variables in order
      sort_cols <- names(group_list)
      result_df <- result_df[do.call(order, result_df[sort_cols]), , drop = FALSE]
    } else if (!is.null(group)) {
      # Sort by single group column
      result_df <- result_df[order(result_df$group), , drop = FALSE]
    }
    
    # Show message if no group has mode stats
    if (!has_mode_stats) {
      message2(format_msg("Note: mode not reported because all values are unique"))
    }
  }
  
  # 5. Round numeric columns (except n, NA_total, freq_mode, and freq_mode2, which are integers) to specified decimals
  numeric_cols <- c("mean", "sd", "se", "median", "mode", "mode2", "min", "max")
  numeric_cols <- numeric_cols[numeric_cols %in% names(result_df)]
  result_df[numeric_cols] <- lapply(result_df[numeric_cols], round, digits = decimals)
  
  # 6. Add descriptive labels to columns using labelled package
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
  
  # Add labels for individual grouping variable columns if they exist
  if (use_separate_group_cols && !is.null(group_list)) {
    for (gv in names(group_list)) {
      if (gv %in% names(result_df)) {
        label_list[[gv]] <- paste("Grouping variable:", gv)
      }
    }
  }
  
  # Only include labels for columns that exist
  labelled::var_label(result_df) <- label_list[names(result_df)]
  
  rownames(result_df) <- NULL
  
  # 7. Return result dataframe (will print if called directly, won't print if assigned)
  return(result_df)
}

