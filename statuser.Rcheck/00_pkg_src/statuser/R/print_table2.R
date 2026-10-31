#' Print method for table2 output with centered column variable name
#'
#' @param x An object of class \code{table2}
#' @param ... Additional arguments passed to print
#'
#' @return Invisibly returns the input object \code{x}. Called for its side effect 
#'   of printing a formatted cross-tabulation table to the console. The output 
#'   includes frequencies, optional relative frequencies (row, column, or overall 
#'   proportions), and chi-squared test results when applicable.
#'
#' @export
print.table2 <- function(x, ...) {
  # Check if x is the new list format (has freq, prop, chisq elements)
  if (is.list(x) && "freq" %in% names(x) && "prop" %in% names(x) && "chisq" %in% names(x)) {
    # Print freq table (without chi-square - we'll print that at the end)
    freq_to_print <- x$freq
    # Mark as frequency (not proportion) to avoid triggering legacy freq+prop logic
    attr(freq_to_print, "is_frequency") <- TRUE
    class(freq_to_print) <- c("table2", class(freq_to_print))
    
    if (!is.null(x$prop)) {
      cat("\n1. Frequencies")
    }
    print.table2(freq_to_print, ...)
    
    # Print prop table if present
    if (!is.null(x$prop)) {
      prop_to_print <- x$prop
      # Mark as proportion for formatting, but no original_frequency to avoid legacy logic
      attr(prop_to_print, "is_proportion") <- TRUE
      attr(prop_to_print, "proportion_digits") <- 3
      class(prop_to_print) <- c("table2", class(prop_to_print))
      
      # Get prop_type from list attributes
      prop_type_list <- attr(x, "prop_type")
      
      # Pass prop_type to the proportion table for adding totals
      attr(prop_to_print, "prop_type") <- prop_type_list
      
      # Build proportion header based on prop type
      cat("\n2. ")
      if (!is.null(prop_type_list) && prop_type_list == 0) {
        cat("Relative frequencies: overall")
      } else if (!is.null(prop_type_list) && prop_type_list == 1) {
        cat("Relative frequencies: by row")
      } else if (!is.null(prop_type_list) && prop_type_list == 2) {
        cat("Relative frequencies: by column")
      } else {
        cat("Relative frequencies")
      }
      print.table2(prop_to_print, ...)
    }
    
    # Print chi-square test at the end if present
    if (!is.null(x$chisq)) {
      chi_test <- x$chisq
      n_dims <- length(dim(x$freq))
      # Print header based on dimension
      if (n_dims == 1) {
        cat("Chi-squared test, null: equality of proportions\n")
      } else {
        cat("Chi-squared test, null: independence\n")
      }
      
      chi_stat <- as.numeric(chi_test$statistic)
      chi_df <- as.numeric(chi_test$parameter)
      chi_p <- as.numeric(chi_test$p.value)
      
      chi_stat_formatted <- if (!is.na(chi_stat)) sprintf("%.2f", round(chi_stat, 2)) else "NA"
      chi_df_formatted <- if (!is.na(chi_df)) sprintf("%.0f", round(chi_df, 0)) else "NA"
      
      chi_p_formatted <- if (!is.na(chi_p)) {
        format_pvalue(chi_p, include_p = TRUE)
      } else {
        "p = NA"
      }
      
      apa_string <- paste0("\u03C7\u00B2(", chi_df_formatted, ") = ", chi_stat_formatted, ", ", chi_p_formatted)
      cat(paste0(apa_string, "\n"))
      
      # Show warning if expected counts are low
      if (isTRUE(attr(chi_test, "low_expected"))) {
        message2(col = "red", "Warning: some cells have less than 5 expected observations.\nThis makes the chi-square approximation less accurate; the p-value is questionable.")
      }
    }
    
    return(invisible(x))
  }
  
  # Legacy format: x is a table/matrix with attributes
  # Get dimension names
  dimn <- dimnames(x)
  dim_names <- names(dimn)
  n_dims <- length(dim(x))
  
  # Check if this is a proportion table (check once)
  # Use !is.null() instead of isTRUE() to be more robust
  is_proportion <- !is.null(attr(x, "is_proportion")) && isTRUE(attr(x, "is_proportion"))
  is_frequency <- !is.null(attr(x, "is_frequency")) && isTRUE(attr(x, "is_frequency"))
  proportion_digits <- attr(x, "proportion_digits")
  if (is.null(proportion_digits)) proportion_digits <- 3
  
  # Get original frequency table if this is a proportion table
  orig_freq <- attr(x, "original_frequency")
  prop_type <- attr(x, "prop_type")
  var1_name <- attr(x, "var1_name")
  var2_name <- attr(x, "var2_name")
  
  # If this is a proportion table, first print the frequency table
  if (is_proportion && !is.null(orig_freq)) {
    # Create a temporary table2 object from the original frequency table
    # This will have the same dimnames structure but without proportion formatting
    freq_table <- orig_freq
    # Copy dimnames from original (before totals were added)
    dimnames(freq_table) <- dimnames(orig_freq)
    # Mark this as a frequency table (not proportion) so it formats correctly
    attr(freq_table, "is_frequency") <- TRUE
    # Ensure it's NOT marked as proportion
    attr(freq_table, "is_proportion") <- FALSE
    # Copy chi_test attribute from the proportion table to freq_table so it prints after frequency table
    chi_test_from_prop <- attr(x, "chi_test")
    if (!is.null(chi_test_from_prop)) {
      attr(freq_table, "chi_test") <- chi_test_from_prop
    }
    # Add class for custom printing if it has the right structure
    if (length(dim(freq_table)) == 2 || length(dim(freq_table)) == 3) {
      freq_dimn <- dimnames(freq_table)
      if (!is.null(names(freq_dimn)) && any(nchar(names(freq_dimn)) > 0)) {
        class(freq_table) <- c("table2", class(freq_table))
      } else {
        # Even without dimension names, add class to ensure custom printing
        class(freq_table) <- c("table2", class(freq_table))
      }
    }
    # Print header for frequency table
    cat("\n1. Frequencies")
    # Print the frequency table (this will also print the chi-square test)
    print.table2(freq_table, ...)
    
    # Print header for proportion table
    cat("\n2. ")
    if (prop_type == 0) {
      cat("Relative frequencies: overall")
    } else if (prop_type == 1) {
      cat("Relative frequencies: by row")
    } else if (prop_type == 2) {
      cat("Relative frequencies: by column")
    }
    # Remove chi_test attribute from proportion table so it doesn't print again
    attr(x, "chi_test") <- NULL
  }
  
  # Handle 3D tables
  if (n_dims == 3) {
    if (is.null(dim_names) || length(dim_names) != 3) {
      # Use base print instead of NextMethod() to avoid issues with recursive calls
      x_for_print <- x
      class(x_for_print) <- setdiff(class(x_for_print), "table2")
      print(x_for_print)
      return(invisible(x))
    }
    
    third_var_name <- dim_names[3]
    third_labels <- dimn[[3]]
    
    # Print each slice of the 3D table
    for (k in seq_along(third_labels)) {
      # Print the third dimension header (e.g., "heat = high" instead of ", , heat = high")
      cat("\n")
      cat(third_var_name, " = ", third_labels[k], "\n", sep = "")
      
      # Extract the 2D slice
      slice <- x[, , k, drop = FALSE]
      # Remove the third dimension to make it a 2D table
      dim(slice) <- dim(slice)[1:2]
      dimnames(slice) <- dimn[1:2]
      
      # For proportion tables, add Total row/column based on prop_type
      if (is_proportion && !is.null(prop_type)) {
        slice_dimn <- dimnames(slice)
        dimn_names <- names(slice_dimn)
        
        if (prop_type == 0) {
          # Overall proportions: add both Total row and Total column
          n_rows <- nrow(slice)
          n_cols <- ncol(slice)
          
          # Add Total row
          col_totals <- colSums(slice, na.rm = TRUE)
          slice <- rbind(slice, col_totals)
          slice_dimn[[1]] <- c(slice_dimn[[1]], "Total")
          
          # Add Total column
          row_totals <- rowSums(slice, na.rm = TRUE)
          slice <- cbind(slice, row_totals)
          slice_dimn[[2]] <- c(slice_dimn[[2]], "Total")
          
          names(slice_dimn) <- dimn_names
          dimnames(slice) <- slice_dimn
        } else if (prop_type == 1) {
          # Row proportions: add Total column with 1.0 for each row
          n_rows <- nrow(slice)
          summary_col <- matrix(round(1.0, proportion_digits), nrow = n_rows, ncol = 1)
          slice <- cbind(slice, summary_col)
          slice_dimn[[2]] <- c(slice_dimn[[2]], "Total")
          names(slice_dimn) <- dimn_names
          dimnames(slice) <- slice_dimn
        } else if (prop_type == 2) {
          # Column proportions: add Total row with 1.0 for each column
          n_cols <- ncol(slice)
          summary_row <- matrix(round(1.0, proportion_digits), nrow = 1, ncol = n_cols)
          slice <- rbind(slice, summary_row)
          slice_dimn[[1]] <- c(slice_dimn[[1]], "Total")
          names(slice_dimn) <- dimn_names
          dimnames(slice) <- slice_dimn
        }
      }
      
      class(slice) <- c("table2", class(slice))
      # Copy attributes
      if (is_proportion) {
        attr(slice, "is_proportion") <- TRUE
        attr(slice, "proportion_digits") <- proportion_digits
        # Copy original frequency slice if available
        if (!is.null(orig_freq)) {
          freq_slice <- orig_freq[, , k, drop = FALSE]
          dim(freq_slice) <- dim(freq_slice)[1:2]
          attr(slice, "original_frequency") <- freq_slice
          attr(slice, "prop_type") <- prop_type
          attr(slice, "var1_name") <- var1_name
          attr(slice, "var2_name") <- var2_name
        }
      }
      
      # Print the 2D slice using the same print method
      print.table2(slice, ...)
    }
    
    return(invisible(x))
  }
  
  # Handle 1D tables
  if (n_dims == 1) {
    # For 1D tables, print using base table print method
    # We can't use NextMethod() here because it fails when called recursively
    # with the table2 class added. Instead, temporarily remove the table2 class.
    x_for_print <- x
    class(x_for_print) <- setdiff(class(x_for_print), "table2")
    cat("\n")
    print(x_for_print)
    
    # Check if we have chi_test attribute and print if available
    # BUT skip if this is being called from the list format handler (is_frequency or is_proportion set)
    # because the list handler prints chi-square at the end
    chi_test <- attr(x, "chi_test")
    if (!is.null(chi_test) && !is.null(chi_test$statistic) && !is.null(chi_test$p.value) &&
        !is_frequency && !is_proportion) {
      # Print chi-square test with label (left-aligned)
      cat("\nChi-squared test, null: equality of proportions\n")
      chi_stat <- as.numeric(chi_test$statistic)
      chi_df <- as.numeric(chi_test$parameter)
      chi_p <- as.numeric(chi_test$p.value)
      
      chi_stat_formatted <- if (!is.na(chi_stat)) sprintf("%.2f", round(chi_stat, 2)) else "NA"
      chi_df_formatted <- if (!is.na(chi_df)) sprintf("%.0f", round(chi_df, 0)) else "NA"
      
      # Format p-value using format_pvalue (same approach as t.test2)
      chi_p_formatted <- if (!is.na(chi_p)) {
        # format_pvalue returns "p = .05", "p < .0001", or "p > .9999" format
        format_pvalue(chi_p, include_p = TRUE)
      } else {
        "p = NA"
      }
      
      # Create APA formatted string: chi-squared(df) = value, p = value
      apa_string <- paste0("\u03c7\u00b2(", chi_df_formatted, ") = ", chi_stat_formatted, ", ", chi_p_formatted)
      
      # Print left-aligned APA formatting
      cat(paste0(apa_string, "\n"))
    }
    return(invisible(x))
  }
  
  # Handle 2D tables (original code)
  # If we don't have dimension names and it's not a proportion table and not a frequency table, fall back to default print
  # But if it's a frequency table (marked with is_frequency), we want to use our custom formatting
  if ((is.null(dim_names) || length(dim_names) != 2) && !is_proportion && !is_frequency) {
    # Use base print instead of NextMethod() to avoid issues with recursive calls
    x_for_print <- x
    class(x_for_print) <- setdiff(class(x_for_print), "table2")
    print(x_for_print)
    return(invisible(x))
  }
  
  # For proportion tables or frequency tables without dimension names, use default print but with custom formatting
  if (is.null(dim_names) || length(dim_names) != 2) {
    # Still use base print but the formatting should be handled by the proportion logic
    # Actually, we need dimension names to format properly, so fall back
    x_for_print <- x
    class(x_for_print) <- setdiff(class(x_for_print), "table2")
    print(x_for_print)
    return(invisible(x))
  }
  
  row_var_name <- dim_names[1]
  col_var_name <- dim_names[2]
  
  # Get row and column labels
  row_labels <- dimn[[1]]
  col_labels <- dimn[[2]]
  
  # Calculate widths for formatting
  # First, calculate width of column labels
  col_label_widths <- nchar(col_labels)
  max_col_label_width <- max(col_label_widths, na.rm = TRUE)
  
  # Helper function to format proportion values
  format_proportion <- function(val, digits) {
    if (is.na(val)) {
      return("NA")
    }
    # For values >= 1.0 (like totals), show with leading digit: 1.000
    if (val >= 1.0) {
      return(sprintf(paste0("%.", digits, "f"), val))
    }
    # For values < 1.0, show without leading zero: .100, .000, etc.
    val_rounded <- round(val * (10^digits))
    return(sprintf(paste0(".%0", digits, "d"), val_rounded))
  }
  
  # Calculate width of actual data values - format each value and check width
  max_data_width <- 0
  for (i in seq_along(row_labels)) {
    for (j in seq_along(col_labels)) {
      val <- x[i, j]
      # Format as integer if NOT a proportion table (frequencies should be integers)
      if (is_proportion) {
        val_str <- format_proportion(val, proportion_digits)
      } else {
        # For frequencies, format as integer (no decimals)
        if (is.na(val)) {
          val_str <- "NA"
        } else {
          # Force integer formatting - use %d which always shows integers
          val_int <- as.integer(round(val))
          val_str <- sprintf("%d", val_int)
        }
      }
      max_data_width <- max(max_data_width, nchar(val_str), na.rm = TRUE)
    }
  }
  
  # Use the maximum of label width and data width for column formatting
  max_col_width <- max(max_col_label_width, max_data_width)
  
  row_label_width <- max(nchar(row_labels), na.rm = TRUE)
  row_var_width <- nchar(row_var_name)
  
  # Column spacing (R typically uses 2 spaces between columns)
  col_spacing <- 2
  
  # Calculate total width of all columns
  n_cols <- length(col_labels)
  total_col_width <- n_cols * max_col_width + (n_cols - 1) * col_spacing
  
  # Calculate spacing to center column variable name over the data columns
  col_var_width <- nchar(col_var_name)
  col_var_spacing <- max(0, floor((total_col_width - col_var_width) / 2))
  
  # Total width needed for row label area (variable name + space + label + spacing)
  total_row_label_width <- row_var_width + 1 + row_label_width + col_spacing
  
  # Print column variable name centered over columns
  cat("\n")
  cat(strrep(" ", total_row_label_width + col_var_spacing))
  cat(col_var_name)
  cat("\n")
  
  # Print column labels
  cat(strrep(" ", total_row_label_width))
  # Build column labels as formatted values
  col_label_values <- character(length(col_labels))
  for (i in seq_along(col_labels)) {
    col_label_values[i] <- sprintf(paste0("%", max_col_width, "s"), col_labels[i])
  }
  # Join with spacing and print
  spacing_str <- strrep(" ", col_spacing)
  cat(paste(col_label_values, collapse = spacing_str))
  cat("\n")
  
  # Print rows with row variable name on left margin
  for (i in seq_along(row_labels)) {
    # Print row variable name and label on first row, or just label with spacing for alignment
    if (i == 1) {
      cat(sprintf(paste0("%-", row_var_width, "s"), row_var_name))
      cat(" ")
      cat(sprintf(paste0("%-", row_label_width + col_spacing, "s"), row_labels[i]))
    } else {
      cat(strrep(" ", row_var_width + 1))
      cat(sprintf(paste0("%-", row_label_width + col_spacing, "s"), row_labels[i]))
    }
    
    # Print values (right-aligned like R does)
    # Build the row as a vector of formatted values
    row_values <- character(length(col_labels))
    for (j in seq_along(col_labels)) {
      val <- x[i, j]
      # Format as integer if NOT a proportion table (frequencies should be integers)
      if (is_proportion) {
        val_str <- format_proportion(val, proportion_digits)
      } else {
        # For frequencies, format as integer (no decimals)
        if (is.na(val)) {
          val_str <- "NA"
        } else {
          # Force integer formatting - use %d which always shows integers
          val_int <- as.integer(round(val))
          val_str <- sprintf("%d", val_int)
        }
      }
      # Right-align the value within max_col_width
      row_values[j] <- sprintf(paste0("%", max_col_width, "s"), val_str)
    }
    # Join with spacing and print
    spacing_str <- strrep(" ", col_spacing)
    cat(paste(row_values, collapse = spacing_str))
    cat("\n")
  }
  
  cat("\n")
  
  # Print chi-square test result if available
  chi_test <- attr(x, "chi_test")
  if (!is.null(chi_test) && !is.null(chi_test$statistic) && !is.null(chi_test$p.value)) {
    # Print header for 2D tables (independence test)
    cat("Chi-squared test, null: independence\n")
    # Format chi-square statistic to 2 decimal places
    chi_stat <- as.numeric(chi_test$statistic)
    chi_df <- as.numeric(chi_test$parameter)
    chi_p <- as.numeric(chi_test$p.value)
    
    chi_stat_formatted <- if (!is.na(chi_stat)) sprintf("%.2f", round(chi_stat, 2)) else "NA"
    chi_df_formatted <- if (!is.na(chi_df)) sprintf("%.0f", round(chi_df, 0)) else "NA"
    
    # Format p-value using format_pvalue (same approach as t.test2)
    chi_p_formatted <- if (!is.na(chi_p)) {
      # format_pvalue returns "p = .05", "p < .0001", or "p > .9999" format
      format_pvalue(chi_p, include_p = TRUE)
    } else {
      "p = NA"
    }
    
    # Create APA formatted string: chi-squared(df) = value, p = value
    # Use Unicode chi-square symbol
    apa_string <- paste0("\u03c7\u00b2(", chi_df_formatted, ") = ", chi_stat_formatted, ", ", chi_p_formatted)
    
    # Print left-aligned APA formatting
    cat(paste0(apa_string, "\n"))
  }
  
  invisible(x)
}

