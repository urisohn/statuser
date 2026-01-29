#' Enhanced alternative to table() 
#'
#'  The function \code{\link[base]{table}} does not show variable 
#'  names when tabulating from a dataframe, requires running another
#'  function, \code{\link[base]{prop.table}},  to tabulate proportions 
#'  and yet another function, \code{\link[stats]{chisq.test}} to test difference of 
#'  proportions.  \code{table2} does what those three functions do, producing easier to 
#'  read output, and always shows variable names. 
#' @param ... same arguments as \code{\link[base]{table}}, plus the arguments shown below
#' @param prop report a table with:
#'   \itemize{
#'     \item \code{prop="all"}: Proportions for full table (each cell / total)
#'     \item \code{prop="row"}: Proportions by row  ('rows' also accepted)
#'     \item \code{prop="col"}: Proportions by columns ('cols', 'column', 'columns' also accepted)
#'   }
#' @param digits Number of decimal values to show for proportions 
#' @param chi Logical. If \code{TRUE}, performs a chi-square test on frequency table,
#' reports results in APA format
#' @param correct Logical. If \code{TRUE}, applies Yates' continuity correction 
#' for 2x2 tables in the chi-square test. Default is \code{FALSE} (no correction).
#' 
#'
#' @return A list (object of class "table2") with the following components:
#'   \itemize{
#'     \item \code{freq}: frequency table 
#'     \item \code{prop}: proportions table 
#'     \item \code{chisq}: chi-square test  
#'   }
#'
#' @examples
#' # Create example data
#' df <- data.frame(
#'   group = c("A", "A", "B", "B", "A"),
#'   status = c("X", "Y", "X", "Y", "X")
#' )
#'
#' # Enhanced table with variable names (2 variables)
#' table2(df$group, df$status)
#'
#' # Enhanced table with variable names (3 variables)
#' df3 <- data.frame(
#'   x = c("A", "A", "B", "B"),
#'   y = c("X", "Y", "X", "Y"),
#'   z = c("high", "low", "high", "low")
#' )
#' table2(df3$x, df3$y, df3$z)
#'
#' # Table with proportions
#' table2(df$group, df$status, prop = 'all')  # Overall proportions
#' table2(df$group, df$status, prop = 'row')  # Row proportions
#' table2(df$group, df$status, prop = 'col')  # Column proportions
#'
#' # Table with chi-square test
#' table2(df$group, df$status, chi = TRUE,prop='all')
#'
#' @usage NULL
#' @export
table2 <- function(..., data = NULL, exclude = if (useNA == "no") c(NA, NaN), 
                  useNA = c("no", "ifany", "always"), 
                  dnn = NULL, deparse.level = 1, prop = NULL, digits = 3, 
                  chi = FALSE, correct = FALSE) {
  
  # FUNCTION OUTLINE:
  # 1. Validate and process useNA and exclude arguments
  # 2. Capture function call expressions to extract variable names
  # 3. Call base::table() to create the contingency table
  # 4. Extract variable names from dataframe column references (df$var, df[["var"]], etc.)
  # 5. FORMAT HEADERS: Set variable names as dimension names (appear in margins when printed)
  # 6. FORMAT HEADERS: Keep row/column labels as values only (not "var=value")
  # 7. Handle proportion calculations if prop argument is provided
  # 8. Add marginal totals for proportion tables (Total rows/columns)
  # 9. Return the enhanced table object
  
  # TASK 1: Validate and process useNA and exclude arguments
  # Match useNA argument to handle default properly
  useNA <- match.arg(useNA)
  
  # Set exclude default based on useNA
  if (missing(exclude)) {
    exclude <- if (useNA == "no") c(NA, NaN) else NULL
  }
  
  # TASK 2: Validate inputs and handle data argument
  validated <- validate_table2(..., data = data, func_name = "table2")
  dots <- validated$dots
  dot_expressions <- validated$dot_expressions
  data_name <- validated$data_name
  
  # Initialize return list elements
  freq <- NULL
  prop_out <- NULL
  chisq <- NULL
  
  # TASK 3: Call base table function
  # Let base::table handle dnn default (it uses list.names internally)
  if (is.null(dnn)) {
    result <- do.call(base::table, c(dots, list(exclude = exclude, useNA = useNA, 
                          deparse.level = deparse.level)))
  } else {
    result <- do.call(base::table, c(dots, list(exclude = exclude, useNA = useNA, 
                         dnn = dnn, deparse.level = deparse.level)))
  }
  
  # Store frequency table
  freq <- result
  
  # Helper function to extract variable name from an expression
  extract_var_name <- function(expr) {
    var_name <- ""
    # Check if it's a symbol (variable name) - works with or without data argument
    if (is.symbol(expr) || is.name(expr)) {
      var_name <- as.character(expr)
      return(var_name)
    }
    
    # Check if it's a dataframe column reference: df$var
    if (is.call(expr) && length(expr) >= 3) {
      op <- expr[[1]]
      # Handle df$var
      if (identical(op, quote(`$`)) || identical(op, as.name("$"))) {
        var_name <- as.character(expr[[3]])
      }
      # Handle df[["var"]] or df[, "var"] or df[, i]
      else if (identical(op, quote(`[`)) || identical(op, as.name("["))) {
        if (length(expr) >= 3) {
          col_expr <- expr[[3]]
          # Handle df[["var"]] - double bracket with character
          if (is.character(col_expr) && length(col_expr) == 1) {
            var_name <- col_expr
          }
          # Handle df[, "var"] - single bracket with character column name
          else if (is.call(col_expr) && identical(col_expr[[1]], quote(`[`)) && 
                   length(col_expr) >= 2 && is.character(col_expr[[2]])) {
            var_name <- col_expr[[2]]
          }
          # Handle df[, i] where i is a name or number
          else if (is.name(col_expr)) {
            # Try to evaluate to see if it's a character
            tryCatch({
              val <- eval(col_expr, parent.frame())
              if (is.character(val) && length(val) == 1) {
                var_name <- val
              }
            }, error = function(e) {})
          }
        }
      }
    }
    return(var_name)
  }
  
  # TASK 4: Extract variable names from dataframe column references
  # Enhance if we have 1, 2 or 3 dimensions matching the number of arguments
  n_dims <- length(dim(result))
  if ((n_dims == 1 && length(dots) == 1) || 
      (n_dims == 2 && length(dots) == 2) || 
      (n_dims == 3 && length(dots) == 3)) {
    var_names <- character(n_dims)
    
    # Try to extract variable names from expressions
    for (i in 1:n_dims) {
      expr <- dot_expressions[[i]]
      var_names[i] <- extract_var_name(expr)
    }
    
    # TASK 5 & 6: FORMAT HEADERS - Set variable names and labels
    # If we found variable names, enhance the dimnames
    if (any(nchar(var_names) > 0)) {
      dimn <- dimnames(result)
      
      # TASK 5: FORMAT HEADERS - Set variable names as dimension names
      # These appear in margins when printed (column var on top, row var on left)
      # For 3D tables, the third dimension name appears in the slice headers
      for (i in 1:n_dims) {
        if (nchar(var_names[i]) > 0) {
          names(dimn)[i] <- var_names[i]
        }
      }
      
      # TASK 6: FORMAT HEADERS - Keep row and column labels as just the values
      # Row labels: dimn[[1]] contains just values (e.g., "A", "B", "C")
      # Column labels: dimn[[2]] contains just values (e.g., "1", "2", "3")
      # Third dimension labels: dimn[[3]] contains just values (e.g., "high", "low")
      # The variable names will appear in the margins via names(dimnames)
      dimnames(result) <- dimn
      
      # Update freq to have the enhanced dimnames
      freq <- result
    }
  }
  
  # TASK 7: Perform chi-square test if requested
  # Initialize chi_test_attr to NULL
  chi_test_attr <- NULL
  
  if (isTRUE(chi)) {
    # Chi-square test can be performed on 1D or 2D frequency tables
    # For 1D: tests for given probabilities (goodness of fit)
    # For 2D: tests for independence
    n_dims_chi <- length(dim(result))
    if (n_dims_chi == 1 || n_dims_chi == 2) {
      # Perform chi-square test on the frequency table
      # Capture warning about small expected counts
      chi_warning <- NULL
      chi_test <- tryCatch({
        withCallingHandlers(
          stats::chisq.test(result, correct = correct),
          warning = function(w) {
            if (grepl("Chi-squared approximation may be incorrect", w$message)) {
              chi_warning <<- TRUE
              invokeRestart("muffleWarning")
            }
          }
        )
      }, error = function(e) {
        # If chi-square test fails (e.g., all zeros, insufficient data), return NULL
        NULL
      })
      # Store chi-square test result and warning flag
      attr(result, "chi_test") <- chi_test
      attr(chi_test, "low_expected") <- isTRUE(chi_warning)
      chi_test_attr <- chi_test
      chisq <- chi_test
    } else {
      # For 3D tables, chi-square test could be performed on each slice
      # For now, we'll skip it or perform on the first slice
      # Store NULL to indicate chi-square test not performed
      attr(result, "chi_test") <- NULL
      chi_test_attr <- NULL
    }
  }
  
  # TASK 8: Convert to proportions if requested
  # Initialize orig_freq to NULL (will be set if prop is requested)
  orig_freq <- NULL
  
  if (!is.null(prop)) {
    # Mark this as a proportion table
    attr(result, "is_proportion") <- TRUE
    attr(result, "proportion_digits") <- digits
    # Convert character values to numeric
    if (is.character(prop)) {
      prop_lower <- tolower(prop)
      if (prop_lower == "all") {
        prop <- 0
      } else if (prop_lower %in% c("row", "rows")) {
        prop <- 1
      } else if (prop_lower %in% c("col", "cols", "column", "columns")) {
        prop <- 2
      } else {
        stop("table2(): prop must be 0, 1, 2, 'all', 'row'/'rows', or 'col'/'cols'/'column'/'columns'", call. = FALSE)
      }
    }
    
    if (!prop %in% c(0, 1, 2)) {
      stop("table2(): prop must be 0, 1, 2, 'all', 'row', or 'column'", call. = FALSE)
    }
    
    # Get original dimnames before modification
    orig_dimn <- dimnames(result)
    n_dims_orig <- length(dim(result))
    
    # Save original frequency table before any modifications (for all prop types)
    orig_freq <- result
    
    # Track if original was 1D
    was_1d <- (n_dims_orig == 1)
    
    # Handle 1D tables: keep as 1D, just calculate proportions
    if (was_1d) {
      # For 1D tables, calculate proportions and return early
      total_sum <- sum(result, na.rm = TRUE)
      prop_result <- result / total_sum
      prop_result <- round(prop_result, digits = digits)
      
      # Keep the same dimnames as the frequency table
      dimnames(prop_result) <- dimnames(result)
      class(prop_result) <- "table"
      
      # Store proportion table
      prop_out <- prop_result
      
      # Build output list
      output <- list2(freq = freq, prop = prop_out, chisq = chisq)
      
      # Store proportion metadata
      attr(output, "prop_type") <- prop
      var1_name <- if (!is.null(names(orig_dimn)) && length(names(orig_dimn)) >= 1 && 
                       !is.na(names(orig_dimn)[1]) && nchar(names(orig_dimn)[1]) > 0) {
        names(orig_dimn)[1]
      } else {
        ""
      }
      attr(output, "var1_name") <- var1_name
      attr(output, "var2_name") <- ""
      
      class(output) <- c("table2", class(output))
      return(output)
    }
    
    # Get variable names from dimnames for cat messages
    var1_name <- if (length(dim(result)) == 2 && !is.null(names(orig_dimn)) && length(names(orig_dimn)) >= 1 && !is.na(names(orig_dimn)[1]) && nchar(names(orig_dimn)[1]) > 0) {
      names(orig_dimn)[1]
    } else {
      ""
    }
    var2_name <- if (length(dim(result)) == 2 && !is.null(names(orig_dimn)) && length(names(orig_dimn)) >= 2 && !is.na(names(orig_dimn)[2]) && nchar(names(orig_dimn)[2]) > 0) {
      names(orig_dimn)[2]
    } else {
      ""
    }
    
    if (prop == 0) {
      # Overall proportions: divide by sum of all cells
      total_sum <- sum(result, na.rm = TRUE)
      
      result <- result / total_sum
      # Round to specified number of digits
      result <- round(result, digits = digits)
      prop_type <- "overall proportions"
      
      # Add row and column with marginal proportions, 100% only in bottom right
      # Skip margins for 1D tables (they would just duplicate the single column)
      if (length(dim(result)) == 2 && !was_1d) {
        n_rows <- nrow(result)
        n_cols <- ncol(result)
        dimn <- dimnames(result)
        # Preserve names of dimnames
        dimn_names <- names(dimn)
        
        # Calculate column totals from original frequency table, then convert to proportions
        col_totals_freq <- colSums(orig_freq, na.rm = TRUE)
        col_totals <- col_totals_freq / total_sum
        col_totals <- round(col_totals, digits = digits)
        
        # TASK 8: Add summary row with column marginal proportions
        summary_row <- matrix(col_totals, nrow = 1, ncol = n_cols)
        result <- rbind(result, summary_row)
        dimn[[1]] <- c(dimn[[1]], "Total")  # Add "Total" to row labels
        
        # Calculate row totals from original frequency table, then convert to proportions
        row_totals_freq <- rowSums(orig_freq, na.rm = TRUE)
        row_totals <- row_totals_freq / total_sum
        row_totals <- round(row_totals, digits = digits)
        
        # TASK 8: Add summary column with row marginal proportions
        summary_col <- matrix(c(row_totals, round(1.0, digits = digits)), nrow = n_rows + 1, ncol = 1)
        # Bottom right corner is 1.0 - sum of all proportions
        result <- cbind(result, summary_col)
        dimn[[2]] <- c(dimn[[2]], "Total")  # Add "Total" to column labels
        # Restore names of dimnames
        names(dimn) <- dimn_names
        dimnames(result) <- dimn
      }
      
    } else if (prop == 1) {
      # Row proportions: each row sums to 1
      row_sums <- rowSums(result, na.rm = TRUE)
      # Avoid division by zero
      row_sums[row_sums == 0] <- 1
      result <- result / row_sums
      # Round to specified number of digits
      result <- round(result, digits = digits)
      prop_type <- "row proportions"
      
      # TASK 8: Add column with 1.0 for each row
      # Skip for 1D tables (would just duplicate the single column)
      if (length(dim(result)) == 2 && !was_1d) {
        n_rows <- nrow(result)
        dimn <- dimnames(result)
        # Preserve names of dimnames
        dimn_names <- names(dimn)
        summary_col <- matrix(round(1.0, digits = digits), nrow = n_rows, ncol = 1)  # 1.0 for each row
        result <- cbind(result, summary_col)
        dimn[[2]] <- c(dimn[[2]], "Total")  # Add "Total" to column labels
        # Restore names of dimnames
        names(dimn) <- dimn_names
        dimnames(result) <- dimn
      }
      
    } else if (prop == 2) {
      # Column proportions: each column sums to 1
      col_sums <- colSums(result, na.rm = TRUE)
      # Avoid division by zero
      col_sums[col_sums == 0] <- 1
      result <- sweep(result, 2, col_sums, "/")
      # Round to specified number of digits
      result <- round(result, digits = digits)
      prop_type <- "column proportions"
      
      # TASK 8: Add row with 1.0 for each column
      if (length(dim(result)) == 2) {
        n_cols <- ncol(result)
        dimn <- dimnames(result)
        # Preserve names of dimnames
        dimn_names <- names(dimn)
        summary_row <- matrix(round(1.0, digits = digits), nrow = 1, ncol = n_cols)  # 1.0 for each column
        result <- rbind(result, summary_row)
        dimn[[1]] <- c(dimn[[1]], "Total")  # Add "Total" to row labels
        # Restore names of dimnames
        names(dimn) <- dimn_names
        dimnames(result) <- dimn
      }
    }
    
    # Re-set attributes after all modifications (rbind/cbind may have created new objects)
    if (!is.null(prop)) {
      attr(result, "is_proportion") <- TRUE
      attr(result, "proportion_digits") <- digits
      attr(result, "original_frequency") <- orig_freq
      attr(result, "prop_type") <- prop
      attr(result, "var1_name") <- var1_name
      attr(result, "var2_name") <- var2_name
      # Restore chi_test attribute if it was saved before prop calculations
      if (!is.null(chi_test_attr)) {
        attr(result, "chi_test") <- chi_test_attr
      }
      # Store proportion table as plain table (no custom attributes)
      # Keep only dim, dimnames, class
      prop_out <- result
      attr(prop_out, "is_proportion") <- NULL
      attr(prop_out, "proportion_digits") <- NULL
      attr(prop_out, "original_frequency") <- NULL
      attr(prop_out, "prop_type") <- NULL
      attr(prop_out, "var1_name") <- NULL
      attr(prop_out, "var2_name") <- NULL
      attr(prop_out, "chi_test") <- NULL
      class(prop_out) <- "table"
    }
  }
  
  # TASK 9: Return as list with class table2
  output <- list2(freq = freq, prop = prop_out, chisq = chisq)
  
  # Store proportion metadata if prop was specified
  if (!is.null(prop)) {
    attr(output, "prop_type") <- prop
    attr(output, "var1_name") <- var1_name
    attr(output, "var2_name") <- var2_name
  }
  
  class(output) <- c("table2", class(output))
  return(output)
}











