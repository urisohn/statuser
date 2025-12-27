#' Enhanced table() by showing variable name and allowing proportions as results
#'
#' Identical to base R's \code{table()}, except that when tabulating two or three variables
#' from a dataframe, the variable names are displayed in the dimension names, and that
#' \code{prop} argument allows reporting proportions, bypassing need for \code{prop.table(table())}
#'
#' @param ... One or more objects which can be interpreted as factors (including
#'   character strings), or a list (or data frame) whose components can be so
#'   interpreted. For the default method, either all arguments are vectors of
#'   the same length, or one argument is a data frame and the rest are vectors
#'   that can be matched by name.
#' @param exclude Levels to remove from all factors in \code{...}.
#' @param useNA Whether to include NA values in the table.
#' @param dnn The names to be given to the dimensions in the result.
#' @param deparse.level Controls how the default \code{dnn} is constructed.
#' @param prop Numeric or character. If specified, converts table to proportions:
#'   \itemize{
#'     \item \code{prop=0} or \code{prop="all"}: Proportions of the whole table (each cell / total)
#'     \item \code{prop=1}, \code{prop="row"}, or \code{prop="rows"}: Proportions by rows (each row sums to 1)
#'     \item \code{prop=2}, \code{prop="col"}, \code{prop="column"}, or \code{prop="columns"}: Proportions by columns (each column sums to 1)
#'   }
#'   If \code{NULL} (default), returns frequency counts.
#' @param digits Number of decimal places to display when \code{prop} is specified.
#'   Default is 3. Values are displayed as proportions without leading zero (e.g., .100, .110, .111).
#'   Only applies when \code{prop} is not \code{NULL}.
#'
#' @return A contingency table (an object of class "table") with enhanced
#'   dimnames when variables come from a dataframe. If \code{prop} is specified,
#'   the table contains proportions (between 0 and 1) instead of counts, and
#'   values are displayed without leading zero (e.g., .100 instead of 0.100).
#'
#' @details
#' When tabulating two or three variables from a dataframe (e.g., \code{table2(df$x, df$y)} or
#' \code{table2(df$x, df$y, df$z)}), the variable names appear as dimension names in the table
#' margins, while row and column labels show only the values. This creates a cleaner cross-tabulation
#' format with variable names as headers. For 3D tables, the third variable name appears in the
#' slice headers (e.g., \code{, , var = value}).
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
#' # Regular table (no variable names)
#' x <- c("A", "A", "B")
#' y <- c("X", "Y", "X")
#' table2(x, y)
#'
#' # Table with proportions
#' table2(df$group, df$status, prop = 0)  # Overall proportions
#' table2(df$group, df$status, prop = 1)  # Row proportions
#' table2(df$group, df$status, prop = 2)  # Column proportions
#' table2(df$group, df$status, prop = "row")  # Row proportions (character)
#' table2(df$group, df$status, prop = 1, digits = 3)  # Row proportions with 3 decimals
#'
#' @export
table2 <- function(..., exclude = if (useNA == "no") c(NA, NaN), 
                  useNA = c("no", "ifany", "always"), 
                  dnn = NULL, deparse.level = 1, prop = NULL, digits = 3) {
  
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
  
  # TASK 2: Capture the original call to detect dataframe column references
  dots <- list(...)
  dot_expressions <- as.list(substitute(list(...)))[-1L]
  
  # TASK 3: Call base table function
  # Let base::table handle dnn default (it uses list.names internally)
  if (is.null(dnn)) {
    result <- base::table(..., exclude = exclude, useNA = useNA, 
                          deparse.level = deparse.level)
  } else {
    result <- base::table(..., exclude = exclude, useNA = useNA, 
                         dnn = dnn, deparse.level = deparse.level)
  }
  
  # TASK 4: Extract variable names from dataframe column references
  # Enhance if we have 2 or 3 dimensions matching the number of arguments
  n_dims <- length(dim(result))
  if ((n_dims == 2 && length(dots) == 2) || (n_dims == 3 && length(dots) == 3)) {
    var_names <- character(n_dims)
    
    # Helper function to extract variable name from an expression
    extract_var_name <- function(expr) {
      var_name <- ""
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
    }
  }
  
  # TASK 7: Convert to proportions if requested
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
      } else if (prop_lower %in% c("col", "column", "columns")) {
        prop <- 2
      } else {
        stop("prop must be 0, 1, 2, 'all', 'row'/'rows', or 'col'/'column'/'columns'")
      }
    }
    
    if (!prop %in% c(0, 1, 2)) {
      stop("prop must be 0, 1, 2, 'all', 'row', or 'column'")
    }
    
    # Get original dimnames before modification
    orig_dimn <- dimnames(result)
    
    # Get variable names from dimnames for cat messages
    var1_name <- if (length(dim(result)) == 2 && !is.null(names(orig_dimn)) && nchar(names(orig_dimn)[1]) > 0) {
      names(orig_dimn)[1]
    } else {
      ""
    }
    var2_name <- if (length(dim(result)) == 2 && !is.null(names(orig_dimn)) && nchar(names(orig_dimn)[2]) > 0) {
      names(orig_dimn)[2]
    } else {
      ""
    }
    
    # TASK 8: Add marginal totals for proportion tables
    if (prop == 0) {
      # Overall proportions: divide by sum of all cells
      cat("\nNote: Proportions for full data\n")
      total_sum <- sum(result, na.rm = TRUE)
      
      # Save original frequency table before converting to proportions
      orig_freq <- result
      
      result <- result / total_sum
      # Round to specified number of digits
      result <- round(result, digits = digits)
      prop_type <- "overall proportions"
      
      # Add row and column with marginal proportions, 100% only in bottom right
      if (length(dim(result)) == 2) {
        n_rows <- nrow(result)
        n_cols <- ncol(result)
        dimn <- dimnames(result)
        
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
        dimnames(result) <- dimn
      }
      
    } else if (prop == 1) {
      # Row proportions: each row sums to 1
      cat("\nNote: Proportions by each '", var1_name, "' row.\n", sep = "")
      row_sums <- rowSums(result, na.rm = TRUE)
      # Avoid division by zero
      row_sums[row_sums == 0] <- 1
      result <- result / row_sums
      # Round to specified number of digits
      result <- round(result, digits = digits)
      prop_type <- "row proportions"
      
      # TASK 8: Add column with 1.0 for each row
      if (length(dim(result)) == 2) {
        n_rows <- nrow(result)
        dimn <- dimnames(result)
        summary_col <- matrix(round(1.0, digits = digits), nrow = n_rows, ncol = 1)  # 1.0 for each row
        result <- cbind(result, summary_col)
        dimn[[2]] <- c(dimn[[2]], "Total")  # Add "Total" to column labels
        dimnames(result) <- dimn
      }
      
    } else if (prop == 2) {
      # Column proportions: each column sums to 1
      cat("\nNote: Proportions for each '", var2_name, "' column\n", sep = "")
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
        summary_row <- matrix(round(1.0, digits = digits), nrow = 1, ncol = n_cols)  # 1.0 for each column
        result <- rbind(result, summary_row)
        dimn[[1]] <- c(dimn[[1]], "Total")  # Add "Total" to row labels
        dimnames(result) <- dimn
      }
    }
    
    # Re-set attributes after all modifications (rbind/cbind may have created new objects)
    if (!is.null(prop)) {
      attr(result, "is_proportion") <- TRUE
      attr(result, "proportion_digits") <- digits
    }
  }
  
  # TASK 9: Return the enhanced table object
  # Add class for custom printing if we have 2D or 3D table with variable names or if it's a proportion table
  n_dims <- length(dim(result))
  if (n_dims == 2 || n_dims == 3) {
    dimn <- dimnames(result)
    # Add class if we have variable names OR if it's a proportion table
    # For 2D: check if we have 2 dots and variable names
    # For 3D: check if we have 3 dots and variable names
    has_var_names <- (n_dims == 2 && length(dots) == 2) || 
                     (n_dims == 3 && length(dots) == 3)
    if ((has_var_names && !is.null(names(dimn)) && any(nchar(names(dimn)) > 0)) ||
        isTRUE(attr(result, "is_proportion"))) {
      class(result) <- c("table2", class(result))
    }
  }
  
  return(result)
}

