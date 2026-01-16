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
  
  # Print left-aligned title
  cat(paste0(title_text, "\n\n"))
  
  # Get N column names before removing class
  name_N1 <- attr(x, "name_N1")
  name_N2 <- attr(x, "name_N2")
  
  # Show missing value notes if present
  NA1 <- if (is.null(NA1) || is.na(NA1)) 0L else NA1
  NA2 <- if (is.null(NA2) || is.na(NA2)) 0L else NA2
  
  # For one-sample tests, remove group2, diff column, and N2 columns before formatting
  # For paired tests, remove N2 column (only show N)
  if (isTRUE(is_one_sample)) {
    # Get column names to remove
    name_2 <- attr(x, "name_2")
    name_N2 <- attr(x, "name_N2")
    name_1 <- attr(x, "name_1")
    cols_to_remove <- c()
    
    # Find diff column (format: "group1-group2" or "1-2" if using Group 1/Group 2)
    if (!is.null(name_1) && !is.null(name_2)) {
      if (name_1 == "Group 1" && name_2 == "Group 2") {
        diff_col_name <- "1-2"
      } else {
        diff_col_name <- paste0(name_1, "-", name_2)
      }
      if (diff_col_name %in% names(x)) {
        cols_to_remove <- c(cols_to_remove, diff_col_name)
      }
    }
    
    if (!is.null(name_2) && name_2 %in% names(x)) {
      cols_to_remove <- c(cols_to_remove, name_2)
    }
    if (!is.null(name_N2) && name_N2 %in% names(x)) {
      cols_to_remove <- c(cols_to_remove, name_N2)
    }
    # Create display dataframe without these columns
    display_x <- x[, !names(x) %in% cols_to_remove, drop = FALSE]
  } else if (isTRUE(is_paired)) {
    # For paired tests, remove N2 column (only show N)
    name_N2 <- attr(x, "name_N2")
    cols_to_remove <- c()
    if (!is.null(name_N2) && name_N2 %in% names(x)) {
      cols_to_remove <- c(cols_to_remove, name_N2)
    }
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
      } else if (col == "N" || grepl("^N", col) || grepl("^N ", col) || grepl("^N\\(", col)) {
        # N columns are integers (including plain "N" for paired tests)
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
  
  # Add APA formatting: t(df)=t.value, p=p.value
  t_value <- x$t
  df_value <- x$df
  p_value <- x$p.value
  
  # Format t-value to 2 decimal places
  t_formatted <- if (!is.na(t_value)) sprintf("%.2f", round(t_value, 2)) else "NA"
  
  # Format df to 1 decimal place (already rounded in display_x, but use original for APA)
  df_formatted <- if (!is.na(df_value)) sprintf("%.1f", round(df_value, 1)) else "NA"
  
  # Format p-value using format_pvalue (same approach as used elsewhere in this function)
  p_formatted <- if (!is.na(p_value)) {
    # format_pvalue returns "p = .05", "p < .0001", or "p > .9999" format
    format_pvalue(p_value, include_p = TRUE)
  } else {
    "p = NA"
  }
  
  # Create APA formatted string
  apa_string <- paste0("t(", df_formatted, ") = ", t_formatted, ", ", p_formatted)
  
  # Print left-aligned APA formatting
  cat(paste0("\nAPA Style:\n", apa_string, "\n"))
  
  # Show group mapping if group names were replaced (but not for one-sample tests)
  show_group_mapping <- attr(x, "show_group_mapping")
  if (isTRUE(show_group_mapping) && !isTRUE(is_one_sample)) {
    orig_group1 <- attr(x, "orig_group1")
    orig_group2 <- attr(x, "orig_group2")
    if (!is.null(orig_group1) && !is.null(orig_group2)) {
      cat(paste0("\nGroup 1: ", orig_group1, "\n"))
      cat(paste0("Group 2: ", orig_group2, "\n"))
    }
  }
  
  # Check if this is a paired test
  if (isTRUE(is_paired)) {
    # Paired test - show missing pairs
    NA_paired <- attr(x, "NA_paired")
    NA_paired <- if (is.null(NA_paired) || is.na(NA_paired)) 0L else NA_paired
    
    if (NA_paired > 0) {
      # Get N value from original dataframe (before formatting)
      N_col <- if ("N" %in% names(x)) "N" else names(x)[grepl("^N", names(x))][1]
      if (length(N_col) > 0 && N_col %in% names(x)) {
        N <- x[[N_col]]
        total_pairs <- if (!is.na(N)) N + NA_paired else NA_integer_
        
        if (!is.na(total_pairs)) {
          cat(paste0("\nnote: ", NA_paired, " of ", total_pairs, " pairs were dropped due to missing values\n"))
        } else {
          cat(paste0("\nnote: ", NA_paired, " pairs were dropped due to missing values\n"))
        }
      } else {
        cat(paste0("\nnote: ", NA_paired, " pairs were dropped due to missing values\n"))
      }
    }
  } else if (isTRUE(is_one_sample)) {
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
      
      # Use original group names for the note (not "Group 1" and "Group 2")
      orig_group1 <- attr(x, "orig_group1")
      orig_group2 <- attr(x, "orig_group2")
      show_group_mapping <- attr(x, "show_group_mapping")
      
      # If group names were replaced, use original names; otherwise use current names
      if (isTRUE(show_group_mapping) && !is.null(orig_group1) && !is.null(orig_group2)) {
        col1_name <- orig_group1
        col2_name <- orig_group2
      } else {
        col1_name <- if (!is.null(name_1) && !is.na(name_1)) name_1 else group1
        col2_name <- if (!is.null(name_2) && !is.na(name_2)) name_2 else group2
        # Fallback to "Group 1" and "Group 2" if still empty
        if (is.na(col1_name) || col1_name == "") col1_name <- "Group 1"
        if (is.na(col2_name) || col2_name == "") col2_name <- "Group 2"
      }
      
      # Check if group names are in "varname=value" format
      is_varname_format1 <- grepl("=", col1_name)
      is_varname_format2 <- grepl("=", col2_name)
      
      # Warning about missing data
      if (!is.na(total_1) && !is.na(total_2)) {
        if (is_varname_format1 && is_varname_format2) {
          # Format: "When 'x4=0' there are k out of N values missing, and when 'x4=1' there are k2 out of N2 values missing"
          cat(paste0("\nnote: When '", col1_name, "' there are ", NA1, " out of ", total_1, 
                     " values missing, and when '", col2_name, "' there are ", NA2, " out of ", total_2, " values missing\n"))
        } else {
          # Original format
          cat(paste0("\nnote: '", col1_name, "' is missing ", NA1, " of ", total_1, 
                     " values, while '", col2_name, "' is missing ", NA2, " of ", total_2, "\n"))
        }
      } else if (!is.na(total_1)) {
        if (is_varname_format1) {
          cat(paste0("\nnote: When '", col1_name, "' there are ", NA1, " out of ", total_1, " values missing\n"))
        } else {
          cat(paste0("\nnote: '", col1_name, "' is missing ", NA1, " of ", total_1, " values\n"))
        }
      } else if (!is.na(total_2)) {
        if (is_varname_format2) {
          cat(paste0("\nnote: When '", col2_name, "' there are ", NA2, " out of ", total_2, " values missing\n"))
        } else {
          cat(paste0("\nnote: '", col2_name, "' is missing ", NA2, " of ", total_2, " values\n"))
        }
      }
    }
  }
  
  # Print environment variable warning at the end if present
  env_warning <- attr(x, "env_warning")
  if (!is.null(env_warning)) {
    cat("\n")
    message2("t.test2() says:", col = "red", font = 2)
    message2(env_warning, col = "red")
  }
  
  # Restore class before returning
  class(x) <- c("t.test2", class(x))
  
  # Return invisibly
  invisible(x)
}
