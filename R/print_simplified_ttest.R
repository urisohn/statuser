#' Print method for simplified t-test output
#'
#' @param x An object of class \code{simplified_ttest}
#' @param quiet Logical. If TRUE, suppresses missing value messages. Default is FALSE.
#' @param ... Additional arguments passed to print
#'
#' @export
print.simplified_ttest <- function(x, quiet = FALSE, ...) {
  # Ensure x is a data frame
  if (!is.data.frame(x)) {
    # Remove the simplified_ttest class temporarily to avoid infinite recursion
    class(x) <- setdiff(class(x), "simplified_ttest")
    return(print(x, ...))
  }
  
  # Get display dataframe from attributes
  display_df <- attr(x, "display_df")
  
  # If display_df is not available or invalid, print the dataframe directly
  if (is.null(display_df) || !is.data.frame(display_df)) {
    # Remove the simplified_ttest class temporarily to avoid infinite recursion
    class(x) <- setdiff(class(x), "simplified_ttest")
    # Print as a regular dataframe
    print.data.frame(x, row.names = FALSE, ...)
    return(invisible(x))
  }
  
  # Show group mapping messages if needed
  if (isTRUE(attr(x, "show_group_mapping"))) {
    orig_col1 <- attr(x, "orig_col1")
    orig_col2 <- attr(x, "orig_col2")
    if (!is.null(orig_col1) && !is.null(orig_col2)) {
      message2(paste0("Group 1: ", orig_col1), col = "blue")
      message2(paste0("Group 2: ", orig_col2), col = "blue")
    }
  }
  
  # Show missing value warnings if present (only for non-paired tests)
  if (!quiet) {
    n_missing_1 <- attr(x, "n_missing_1")
    n_missing_2 <- attr(x, "n_missing_2")
    col1_name <- attr(x, "col1_name")
    col2_name <- attr(x, "col2_name")
    
    # Check if either group has missing values (only show if at least one has missing values)
    n_missing_1 <- if (is.null(n_missing_1)) 0L else n_missing_1
    n_missing_2 <- if (is.null(n_missing_2)) 0L else n_missing_2
    has_any_missing <- (n_missing_1 > 0 || n_missing_2 > 0) && !is.null(col1_name) && !is.null(col2_name)
    
    if (has_any_missing) {
      # Get N values (non-NA counts) from dataframe to calculate totals
      N1_col <- paste0("N_", col1_name)
      N2_col <- paste0("N_", col2_name)
      N1 <- if (N1_col %in% names(x)) x[[N1_col]] else NA_integer_
      N2 <- if (N2_col %in% names(x)) x[[N2_col]] else NA_integer_
      
      # Calculate totals (N + missing)
      total_1 <- if (!is.na(N1)) N1 + n_missing_1 else NA_integer_
      total_2 <- if (!is.na(N2)) N2 + n_missing_2 else NA_integer_
      
      # Build consolidated message: "note: col1_name is missing k1 out of N1 values, col2_name k2 out of N2 values"
      if (!is.na(total_1) && !is.na(total_2)) {
        message2(paste0("note: ", col1_name, " is missing ", n_missing_1, " out of ", total_1, 
                       " values, ", col2_name, " ", n_missing_2, " out of ", total_2, " values"), col = "red")
      } else {
        # Fallback if totals can't be calculated
        msg <- paste0("note: ", col1_name)
        if (!is.na(total_1)) {
          msg <- paste0(msg, " is missing ", n_missing_1, " out of ", total_1, " values")
        } else {
          msg <- paste0(msg, " has ", n_missing_1, " missing values")
        }
        msg <- paste0(msg, ", ", col2_name)
        if (!is.na(total_2)) {
          msg <- paste0(msg, " ", n_missing_2, " out of ", total_2, " values")
        } else {
          msg <- paste0(msg, " has ", n_missing_2, " missing values")
        }
        message2(msg, col = "red")
      }
    }
  }
  
  # Print the formatted display dataframe
  # Use base print.data.frame to avoid any custom methods
  print.data.frame(display_df, row.names = FALSE, ...)
  
  # Return invisibly
  invisible(x)
}

