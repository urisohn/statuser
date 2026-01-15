#' Print method for table2 output with centered column variable name
#'
#' @param x An object of class \code{table2}
#' @param ... Additional arguments passed to print
#'
#' @export
print.table2 <- function(x, ...) {
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
    cat("\nTable 1. Frequencies\n")
    # Print the frequency table
    print.table2(freq_table, ...)
    
    # Print header for proportion table
    cat("\nTable 2. ")
    if (prop_type == 0) {
      cat("Relative frequencies\n")
    } else if (prop_type == 1) {
      cat("Relative frequencies by '", var1_name, "'\n", sep = "")
    } else if (prop_type == 2) {
      cat("Relative frequencies by '", var2_name, "'\n", sep = "")
    }
  }
  
  # Handle 3D tables
  if (n_dims == 3) {
    if (is.null(dim_names) || length(dim_names) != 3) {
      return(NextMethod())
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
  
  # Handle 2D tables (original code)
  # If we don't have dimension names and it's not a proportion table and not a frequency table, fall back to default print
  # But if it's a frequency table (marked with is_frequency), we want to use our custom formatting
  if ((is.null(dim_names) || length(dim_names) != 2) && !is_proportion && !is_frequency) {
    return(NextMethod())
  }
  
  # For proportion tables or frequency tables without dimension names, use default print but with custom formatting
  if (is.null(dim_names) || length(dim_names) != 2) {
    # Still use NextMethod but the formatting should be handled by the proportion logic
    # Actually, we need dimension names to format properly, so fall back
    return(NextMethod())
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
  
  invisible(x)
}

