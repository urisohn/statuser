#' Describe a variable, optionally by groups
#'
#' Returns a dataframe with one row per group
#'
#' @param y A numeric vector of values, a column name (character string or unquoted) if \code{data} is provided,
#'   or a formula of the form \code{y ~ x} or \code{y ~ x1 + x2} (for multiple grouping variables).
#' @param group Optional grouping variable, if not provided computed for the full data.
#'   Ignored if \code{y} is a formula.
#' @param data Optional data frame containing the variable(s).
#' @param digits Number of decimal places to round to. Default is 3.
#'
#' @return A data frame with one row per group (or one row if no group is specified) containing:
#'   \itemize{
#'     \item \code{group}: Group identifier
#'     \item \code{mean}: Mean
#'     \item \code{sd}: Standard deviation
#'     \item \code{se}: Standard error
#'     \item \code{median}: Median
#'     \item \code{min}: Minimum
#'     \item \code{max}: Maximum
#'     \item \code{mode}: Most frequent value
#'     \item \code{freq_mode}: Frequency of mode
#'     \item \code{mode2}: 2nd most frequent value
#'     \item \code{freq_mode2}: Frequency of 2nd mode
#'     \item \code{n.total}: Number of observations
#'     \item \code{n.missing}: Number of observations with missing (NA) values
#'     \item \code{n.unique}: Number of unique values
#'   }
#'
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
#' desc_var(y, group, data = df, digits = 2)
#'
#' # Using formula syntax: y ~ x
#' desc_var(y ~ group, data = df)
#'
#' # Using formula syntax with multiple grouping variables: y ~ x1 + x2
#' df2 <- data.frame(y = rnorm(200), x1 = rep(c("A", "B"), 100), x2 = rep(c("X", "Y"), each = 100))
#' desc_var(y ~ x1 + x2, data = df2)
#'
#' @export
desc_var <- function(y, group = NULL, data = NULL, digits = 3) {
  
  # FUNCTION OUTLINE:
  # 1. Check if y is a formula and extract variables (formula vs non-formula syntax)
  # 2. Validate inputs (numeric check, length checks, perfect overlap check)
  # 3. Define helper function to compute statistics for a vector
  # 4. Compute statistics (either for full dataset or by group)
  # 5. Round numeric columns to specified decimal places
  # 6. Add descriptive labels to columns using labelled package
  # 7. Return result dataframe
  
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
  # Use tryCatch to avoid "object not found" error if y is a symbol in data
  is_formula <- tryCatch(inherits(y, "formula"), error = function(e) FALSE)
  calling_env <- parent.frame()
  call_match <- match.call()
  
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
    # Not a formula: use match.call() and deparse(substitute()) like other functions
    group_was_provided <- !missing(group)
    
    # Get variable names from the call (before any evaluation)
    y_expr <- call_match$y
    y_name_raw <- if (!is.null(y_expr)) deparse(y_expr) else "y"
    # Clean variable name: remove df$ prefix if present
    y_name <- if (grepl("\\$", y_name_raw)) {
      strsplit(y_name_raw, "\\$")[[1]][length(strsplit(y_name_raw, "\\$")[[1]])]
    } else {
      y_name_raw
    }
    
    group_name <- NULL
    group_name_raw <- NULL
    if (group_was_provided) {
      group_expr <- call_match$group
      group_name_raw <- if (!is.null(group_expr)) deparse(group_expr) else "group"
      # Clean variable name: remove df$ prefix if present
      group_name <- if (grepl("\\$", group_name_raw)) {
        strsplit(group_name_raw, "\\$")[[1]][length(strsplit(group_name_raw, "\\$")[[1]])]
      } else {
        group_name_raw
      }
    }
    
    # Validate data frame if provided
    if (!is.null(data) && !is.data.frame(data)) {
      message2(format_msg("'data' must be a data frame"), col = 'red', stop = TRUE)
    }
    
    # Extract y variable
    if (!is.null(data)) {
      # Data provided: extract from data frame
      if (!y_name %in% names(data)) {
        message2(format_msg(sprintf("'%s' not found in data", y_name)), col = 'red', stop = TRUE)
      }
      y <- data[[y_name]]
    } else {
      # No data: evaluate from calling environment
      y_exists <- exists(y_name, envir = calling_env, inherits = TRUE)
      if (!y_exists) {
        # y might already be a vector passed directly, check if it's numeric
        if (is.numeric(y) && is.vector(y)) {
          # y is already evaluated, use it as-is
        } else {
          message2(format_msg(sprintf("'%s' not found", y_name)), col = 'red', stop = TRUE)
        }
      } else {
        y <- get(y_name, envir = calling_env)
      }
    }
    
    y_len <- length(y)
    
    # Extract group variable if provided
    if (group_was_provided) {
      if (!is.null(data)) {
        # Data provided: extract from data frame
        if (!group_name %in% names(data)) {
          message2(format_msg(sprintf("'%s' not found in data", group_name)), col = 'red', stop = TRUE)
        }
        group <- data[[group_name]]
      } else {
        # No data: evaluate from calling environment
        group_exists <- exists(group_name, envir = calling_env, inherits = TRUE)
        if (!group_exists) {
          # group might already be a vector passed directly
          if (is.vector(group)) {
            # group is already evaluated, use it as-is
          } else {
            message2(format_msg(sprintf("'%s' not found", group_name)), col = 'red', stop = TRUE)
          }
        } else {
          group <- get(group_name, envir = calling_env)
        }
      }
      
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
  
  # 2. Validate inputs
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
        # Perfect overlap: one-to-one mapping means unique combinations equals both unique value counts
        # This means each value in var1 maps to exactly one value in var2, and vice versa
        n_unique_combos <- length(unique(paste(var1, var2, sep = "|")))
        n_unique_v1 <- length(unique(var1))
        n_unique_v2 <- length(unique(var2))
        # Perfect overlap occurs when: n_unique_combos == n_unique_v1 == n_unique_v2
        # This ensures one-to-one mapping (no many-to-one or one-to-many)
        if (n_unique_combos == n_unique_v1 && n_unique_combos == n_unique_v2 && n_unique_v1 == n_unique_v2) {
          var1_name <- names(group_list)[i]
          var2_name <- names(group_list)[j]
          message2(format_msg(sprintf("Multiple grouping variables should not overlap perfectly: '%s' and '%s' overlap perfectly", var1_name, var2_name)), col = 'red', stop = TRUE)
        }
      }
    }
  }
    
    
  
  # 3. Define helper function to compute statistics for a vector
  compute_stats <- function(x) {
    n_total <- length(x)  # Total count including missing values
    na_count <- sum(is.na(x))
    x <- x[!is.na(x)]  # Remove NAs for statistics computation
    n <- length(x)  # Count of non-missing values
    
    # Handle empty case (all values are missing)
    if (n == 0) {
      return(list(
        n = as.integer(n_total),  # Total count including missing
        mean = NA_real_,
        sd = NA_real_,
        se = NA_real_,
        median = NA_real_,
        missing = as.integer(na_count),
        unique = 0L,
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
    
    # Compute number of unique values
    n_unique <- length(unique(x))
    
    # Compute mode statistics only if there are repeated values
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
      n = as.integer(n_total),  # Total count including missing values
      mean = as.numeric(mean_val),
      sd = as.numeric(sd_val),
      se = as.numeric(se_val),
      median = as.numeric(median_val),
      missing = as.integer(na_count),
      unique = as.integer(n_unique),
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
    
    # Build base data frame (counts at end: n, n.missing, n.unique)
    result_df <- data.frame(
      group = "All",
      mean = stats$mean,
      sd = stats$sd,
      se = stats$se,
      median = stats$median,
      min = stats$min,
      max = stats$max,
      mode = stats$mode,
      freq_mode = stats$freq_mode,
      mode2 = stats$mode2,
      freq_mode2 = stats$freq_mode2,
      n.total = stats$n,
      n.missing = stats$missing,
      n.unique = stats$unique,
      stringsAsFactors = FALSE
    )
    
  } else {
    # Grouping: compute for each group
    result_list <- list()
    
    if (use_separate_group_cols) {
      # Multiple grouping variables: iterate over unique combinations
      # Create a data frame to get unique combinations efficiently
      group_df <- as.data.frame(group_list, stringsAsFactors = FALSE)
      unique_combos <- unique(group_df)
      
      # Check for missing combinations (combinations that don't exist in data)
      all_possible_combos <- expand.grid(lapply(group_list, function(x) sort(unique(x))), stringsAsFactors = FALSE)
      all_possible_str <- do.call(paste, c(all_possible_combos, sep = "|"))
      unique_combos_str <- do.call(paste, c(unique_combos, sep = "|"))
      missing_indices <- which(!all_possible_str %in% unique_combos_str)
      
      if (length(missing_indices) > 0) {
        missing_combos_df <- all_possible_combos[missing_indices, , drop = FALSE]
        # Format missing combinations as readable strings
        missing_str <- apply(missing_combos_df, 1, function(row) {
          paste(paste(names(missing_combos_df), row, sep = "="), collapse = ", ")
        })
        missing_list <- paste(missing_str, collapse = "\n")
        message2(format_msg(sprintf("Some possible group combinations are not observed:\n%s", missing_list)), col = 'blue')
      }
      
      # Build data frames for each combination
      for (i in seq_len(nrow(unique_combos))) {
        combo <- unique_combos[i, , drop = FALSE]
        # Create mask: all grouping variables match this combination
        mask <- Reduce(`&`, lapply(names(group_list), function(nm) {
          group_list[[nm]] == combo[[nm]]
        }))
        y_group <- y[mask]
        stats <- compute_stats(y_group)
        
        # Build data frame with grouping variables as separate columns (counts at end)
        result_row <- data.frame(
          combo,
          mean = stats$mean,
          sd = stats$sd,
          se = stats$se,
          median = stats$median,
          min = stats$min,
          max = stats$max,
          mode = stats$mode,
          freq_mode = stats$freq_mode,
          mode2 = stats$mode2,
          freq_mode2 = stats$freq_mode2,
          n.total = stats$n,
          n.missing = stats$missing,
          n.unique = stats$unique,
          stringsAsFactors = FALSE
        )
        
        result_list[[length(result_list) + 1]] <- result_row
      }
    } else {
      # Single group column: use group vector directly
      unique_groups <- sort(unique(group))
      
      # Second pass: build data frames
      for (g in unique_groups) {
        y_group <- y[group == g]
        stats <- compute_stats(y_group)
        
        # Build data frame (counts at end: n, n.missing, n.unique)
        result_row <- data.frame(
          group = as.character(g),
          mean = stats$mean,
          sd = stats$sd,
          se = stats$se,
          median = stats$median,
          min = stats$min,
          max = stats$max,
          mode = stats$mode,
          freq_mode = stats$freq_mode,
          mode2 = stats$mode2,
          freq_mode2 = stats$freq_mode2,
          n.total = stats$n,
          n.missing = stats$missing,
          n.unique = stats$unique,
          stringsAsFactors = FALSE
        )
        
        result_list[[length(result_list) + 1]] <- result_row
      }
    }
    
    result_df <- do.call(rbind, result_list)
    
    # Check if any group has 0 observations
    if (any(result_df$n.total == 0)) {
      zero_groups <- sum(result_df$n.total == 0)
      if (zero_groups == 1) {
        message2(format_msg("1 group has 0 observations"))
      } else {
        message2(format_msg(sprintf("%d groups have 0 observations", zero_groups)))
      }
    }
    
    # Sort by grouping variables
      if (use_separate_group_cols && !is.null(group_list)) {
        # Sort by all grouping variables in order
        sort_cols <- names(group_list)
        result_df <- result_df[do.call(order, result_df[sort_cols]), , drop = FALSE]
      } else if (!is.null(group)) {
        # Sort by single group column
        result_df <- result_df[order(result_df$group), , drop = FALSE]
      }
      
  }
  
  # 5. Round numeric columns to specified decimal places
    numeric_cols <- c("mean", "sd", "se", "median", "mode", "mode2", "min", "max")
    numeric_cols <- numeric_cols[numeric_cols %in% names(result_df)]
    result_df[numeric_cols] <- lapply(result_df[numeric_cols], round, digits = digits)
  
  # 5b. Replace NA mode values with "-" and flag if all modes are NA
    all_modes_na <- all(is.na(result_df$mode))
    # Convert mode columns to character and replace NA with "-"
    result_df$mode <- ifelse(is.na(result_df$mode), "-", as.character(result_df$mode))
    result_df$freq_mode <- ifelse(is.na(result_df$freq_mode), "-", as.character(result_df$freq_mode))
    result_df$mode2 <- ifelse(is.na(result_df$mode2), "-", as.character(result_df$mode2))
    result_df$freq_mode2 <- ifelse(is.na(result_df$freq_mode2), "-", as.character(result_df$freq_mode2))
    
  # 6. Add descriptive labels to columns using labelled package
    label_list <- list(
      group = "Group identifier",
      mean = "Mean",
      sd = "Standard deviation",
      se = "Standard error",
      median = "Median (50th percentile)",
      min = "Minimum value",
      max = "Maximum value",
      mode = "Most frequent value",
      freq_mode = "Frequency of mode",
      mode2 = "2nd most frequent value",
      freq_mode2 = "Frequency of 2nd mode",
      n.total = "Number of observations",
      n.missing = "Number of observations with missing values (NA)",
      n.unique = "Number of unique values"
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
  
  # 7. Add class and attributes for print method, then return
  attr(result_df, "all_modes_na") <- all_modes_na
  attr(result_df, "y_name") <- y_name
  class(result_df) <- c("desc_var", class(result_df))
  return(result_df)
}

#' Print method for desc_var objects
#'
#' @param x An object of class \code{desc_var}
#' @param ... Additional arguments passed to print.data.frame
#'
#' @return Invisibly returns the original object
#' @export
print.desc_var <- function(x, ...) {
  # Remove the desc_var class temporarily to use default data.frame printing
  class(x) <- class(x)[class(x) != "desc_var"]
  
  print(x, ...)
  
  # Print note if all modes were NA
  if (isTRUE(attr(x, "all_modes_na"))) {
    y_name <- attr(x, "y_name")
    if (is.null(y_name) || y_name == "") y_name <- "y"
    cat(sprintf("\nNote: mode not reported because all values of %s are unique\n", y_name))
  }
  
  invisible(x)
}

