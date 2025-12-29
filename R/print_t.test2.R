#' Print method for t.test2 output
#'
#' @param x An object of class \code{t.test2}
#' @param ... Additional arguments passed to print
#'
#' @export
print.t.test2 <- function(x, ...) {
  # Ensure x is a data frame
  if (!is.data.frame(x)) {
    # Remove the t.test2 class temporarily to avoid infinite recursion
    class(x) <- setdiff(class(x), "t.test2")
    return(print(x, ...))
  }
  
  # Get attributes
  is_one_sample <- attr(x, "is_one_sample")
  is_paired <- attr(x, "is_paired")
  method_type <- attr(x, "method_type")
  NA1 <- attr(x, "NA1")
  NA2 <- attr(x, "NA2")
  group1 <- attr(x, "group1")
  group2 <- attr(x, "group2")
  name_1 <- attr(x, "name_1")
  name_2 <- attr(x, "name_2")
  
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
  col_names <- names(x)
  total_col_width <- sum(nchar(col_names))
  n_spaces_between_cols <- length(col_names) - 1
  total_df_width <- total_col_width + n_spaces_between_cols
  
  # Calculate spaces needed to center the title
  title_length <- nchar(title_text)
  spaces_needed <- max(0, floor((total_df_width - title_length) / 2))
  
  # Print centered title
  cat(paste0(paste(rep(" ", spaces_needed), collapse = ""), title_text, "\n\n"))
  
  # Get N column names before removing class
  name_N1 <- attr(x, "name_N1")
  name_N2 <- attr(x, "name_N2")
  
  # Show missing value notes if present
  NA1 <- if (is.null(NA1) || is.na(NA1)) 0L else NA1
  NA2 <- if (is.null(NA2) || is.na(NA2)) 0L else NA2
  
  # For one-sample tests, remove group2, diff, and N2 columns before formatting
  if (isTRUE(is_one_sample)) {
    # Get column names to remove
    name_2 <- attr(x, "name_2")
    name_N2 <- attr(x, "name_N2")
    cols_to_remove <- c("diff")
    if (!is.null(name_2) && name_2 %in% names(x)) {
      cols_to_remove <- c(cols_to_remove, name_2)
    }
    if (!is.null(name_N2) && name_N2 %in% names(x)) {
      cols_to_remove <- c(cols_to_remove, name_N2)
    }
    # Create display dataframe without these columns
    display_x <- x[, !names(x) %in% cols_to_remove, drop = FALSE]
  } else {
    display_x <- x
  }
  
  # Format numeric columns using group_decimals
  # Collect all numeric values (means, diff, ci.L, ci.H, t, df, corr) to determine decimals
  numeric_values <- c()
  for (col in names(display_x)) {
    if (col != "ci" && is.numeric(display_x[[col]])) {
      numeric_values <- c(numeric_values, display_x[[col]])
    }
  }
  
  # Determine number of decimals
  decimals <- group_decimals(numeric_values)
  
  # Format numeric columns (except N columns which are integers, and df which has 1 decimal)
  for (col in names(display_x)) {
    if (is.numeric(display_x[[col]])) {
      if (col == "df") {
        # df always has 1 decimal
        display_x[[col]] <- round(display_x[[col]], 1)
      } else if (grepl("^N", col) || grepl("^N ", col)) {
        # N columns are integers
        display_x[[col]] <- round(display_x[[col]], 0)
      } else if (col == "p.value") {
        # p.value is formatted separately using format_pvalue
        if (!is.na(display_x[[col]])) {
          formatted_p <- format_pvalue(display_x[[col]], include_p = FALSE)
          formatted_p <- gsub("= ", "", formatted_p)
          formatted_p <- gsub("< ", "<", formatted_p)
          formatted_p <- gsub("> ", ">", formatted_p)
          display_x[[col]] <- formatted_p
        }
      } else {
        # All other numeric columns use group_decimals
        display_x[[col]] <- round(display_x[[col]], decimals)
      }
    }
  }
  
  # Print the dataframe
  # Remove the t.test2 class temporarily to avoid infinite recursion
  class(display_x) <- setdiff(class(display_x), "t.test2")
  print.data.frame(display_x, row.names = FALSE, ...)
  
  # Check if this is a one-sample test
  if (isTRUE(is_one_sample)) {
    # One-sample test - only show missing values for the single variable
    if (NA1 > 0) {
      # Get N value from original dataframe (before formatting)
      N1_col <- names(x)[grepl("^N", names(x))][1]  # Find first column starting with "N"
      if (length(N1_col) > 0 && N1_col %in% names(x)) {
        N1 <- x[[N1_col]]
        total_1 <- if (!is.na(N1)) N1 + NA1 else NA_integer_
        
        if (!is.na(total_1)) {
          cat(paste0("\nnote: ", NA1, " of ", total_1, " values are missing\n"))
        } else {
          cat(paste0("\nnote: ", NA1, " values are missing\n"))
        }
      } else {
        cat(paste0("\nnote: ", NA1, " values are missing\n"))
      }
    }
  } else {
    # Two-sample test - show missing values for both variables
    has_any_missing <- (NA1 > 0 || NA2 > 0)
    
    if (has_any_missing) {
      # Get N values from original dataframe (before formatting)
      N1 <- if (!is.null(name_N1) && name_N1 %in% names(x)) x[[name_N1]] else NA_integer_
      N2 <- if (!is.null(name_N2) && name_N2 %in% names(x)) x[[name_N2]] else NA_integer_
      
      # Calculate totals (N + missing)
      total_1 <- if (!is.na(N1)) N1 + NA1 else NA_integer_
      total_2 <- if (!is.na(N2)) N2 + NA2 else NA_integer_
      
      # Use group names for the note
      col1_name <- if (!is.null(name_1) && !is.na(name_1)) name_1 else "Group 1"
      col2_name <- if (!is.null(name_2) && !is.na(name_2)) name_2 else "Group 2"
      
      # Warning about missing data
      if (!is.na(total_1) && !is.na(total_2)) {
        cat(paste0("\nnote: '", col1_name, "' is missing ", NA1, " of ", total_1, 
                   " values, and '", col2_name, "' is missing ", NA2, " of ", total_2, "\n"))
      } else if (!is.na(total_1)) {
        cat(paste0("\nnote: '", col1_name, "' is missing ", NA1, " of ", total_1, " values\n"))
      } else if (!is.na(total_2)) {
        cat(paste0("\nnote: '", col2_name, "' is missing ", NA2, " of ", total_2, " values\n"))
      }
    }
  }
  
  # Restore class before returning
  class(x) <- c("t.test2", class(x))
  
  # Return invisibly
  invisible(x)
}
