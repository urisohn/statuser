#' Print method for t.test2 output
#'
#' @param x An object of class \code{t.test2}
#' @param quiet Logical. If TRUE, suppresses missing value messages. Default is FALSE.
#' @param ... Additional arguments passed to print
#'
#' @export
print.t.test2 <- function(x, quiet = FALSE, ...) {
  # Ensure x is a data frame
  if (!is.data.frame(x)) {
    # Remove the t.test2 class temporarily to avoid infinite recursion
    class(x) <- setdiff(class(x), "t.test2")
    return(print(x, ...))
  }
  
  # Get display dataframe from attributes
  display_df <- attr(x, "display_df")
  
  # If display_df is not available or invalid, print the dataframe directly
  if (is.null(display_df) || !is.data.frame(display_df)) {
    # Remove the t.test2 class temporarily to avoid infinite recursion
    class(x) <- setdiff(class(x), "t.test2")
    # Print as a regular dataframe
    print.data.frame(x, row.names = FALSE, ...)
    return(invisible(x))
  }
  
  # Display test type right above the data.frame
  is_one_sample <- attr(x, "is_one_sample")
  is_paired <- attr(x, "is_paired")
  method_type <- attr(x, "method_type")
  
  # Determine test title
  if (isTRUE(is_one_sample)) {
    title_text <- "One sample Student t-test"
  } else if (isTRUE(is_paired)) {
    title_text <- "Two sample paired t-test"
  } else if (identical(method_type, "welch")) {
    title_text <- "Two sample Welch t-test"
  } else {
    title_text <- "Two sample Student t-test"
  }
  
  # Calculate total width of dataframe: sum of column name lengths + spaces between them
  # Each column name is separated by a space, so we need (n_cols - 1) spaces
  col_names <- names(display_df)
  total_col_width <- sum(nchar(col_names))
  n_spaces_between_cols <- length(col_names) - 1
  total_df_width <- total_col_width + n_spaces_between_cols
  
  # Calculate spaces needed to center the title
  title_length <- nchar(title_text)
  spaces_needed <- max(0, floor((total_df_width - title_length) / 2))
  
  # Print centered title
  cat(paste0(paste(rep(" ", spaces_needed), collapse = ""), title_text, "\n\n"))
  
  # Print the formatted display dataframe
  # Use base print.data.frame to avoid any custom methods
  print.data.frame(display_df, row.names = FALSE, ...)
  
  # Show group mapping messages if needed (under the dataframe)
  if (isTRUE(attr(x, "show_group_mapping"))) {
    orig_col1 <- attr(x, "orig_col1")
    orig_col2 <- attr(x, "orig_col2")
    if (!is.null(orig_col1) && !is.null(orig_col2)) {
      cat(paste0("\nGroup 1: ", orig_col1, "\n"))
      cat(paste0("Group 2: ", orig_col2, "\n"))
    }
  }
  
  # Show missing value warnings if present (only for non-paired tests) - after all cat() output
  if (!quiet) {
    n_missing_1 <- attr(x, "n_missing_1")
    n_missing_2 <- attr(x, "n_missing_2")
    col1_name <- attr(x, "col1_name")
    col2_name <- attr(x, "col2_name")
    is_one_sample <- attr(x, "is_one_sample")
    
    # Check if this is a one-sample test
    if (isTRUE(is_one_sample)) {
      # One-sample test - only show missing values for the single variable
      n_missing_1 <- if (is.null(n_missing_1)) 0L else n_missing_1
      if (n_missing_1 > 0 && !is.null(col1_name)) {
        # Get N value from dataframe (use "N" column for one-sample tests)
        N1 <- if ("N" %in% names(x)) x[["N"]] else NA_integer_
        total_1 <- if (!is.na(N1)) N1 + n_missing_1 else NA_integer_
        
        if (!is.na(total_1)) {
          cat(paste0("\nnote: ", n_missing_1, " of ", total_1, " values are missing\n"))
        } else {
          cat(paste0("\nnote: ", n_missing_1, " values are missing\n"))
        }
      }
    } else {
      # Two-sample test - show missing values for both variables
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
          cat(paste0("\nnote: ", col1_name, " is missing ", n_missing_1, " of ", total_1, 
                     " values, while ", col2_name, " is missing ", n_missing_2, " of ", total_2, "\n"))
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
          cat(paste0("\n", msg, "\n"))
        }
      }
    }
  }
  
  # Return invisibly
  invisible(x)
}




