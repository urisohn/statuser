#' Enhanced table function with variable name display
#'
#' Identical to base R's \code{table()}, except that when tabulating two variables
#' from a dataframe, the variable names are displayed in the row/column names.
#' Also includes a \code{prop} argument for computing proportions.
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
#'     \item \code{prop=1} or \code{prop="row"}: Proportions by rows (each row sums to 1)
#'     \item \code{prop=2} or \code{prop="column"}: Proportions by columns (each column sums to 1)
#'   }
#'   If \code{NULL} (default), returns frequency counts.
#'
#' @return A contingency table (an object of class "table") with enhanced
#'   dimnames when variables come from a dataframe. If \code{prop} is specified,
#'   the table contains proportions instead of counts.
#'
#' @details
#' When tabulating two variables from a dataframe (e.g., \code{table2(df$x, df$y)}),
#' the row and column names will include the variable names, formatted as
#' "varname=value" instead of just "value".
#'
#' @examples
#' # Create example data
#' df <- data.frame(
#'   group = c("A", "A", "B", "B", "A"),
#'   status = c("X", "Y", "X", "Y", "X")
#' )
#'
#' # Enhanced table with variable names
#' table2(df$group, df$status)
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
#'
#' @export
table2 <- function(..., exclude = if (useNA == "no") c(NA, NaN), 
                  useNA = c("no", "ifany", "always"), 
                  dnn = NULL, deparse.level = 1, prop = NULL) {
  
  # Match useNA argument to handle default properly
  useNA <- match.arg(useNA)
  
  # Set exclude default based on useNA
  if (missing(exclude)) {
    exclude <- if (useNA == "no") c(NA, NaN) else NULL
  }
  
  # Capture the original call to detect dataframe column references
  dots <- list(...)
  dot_expressions <- as.list(substitute(list(...)))[-1L]
  
  # Call base table function
  # Let base::table handle dnn default (it uses list.names internally)
  if (is.null(dnn)) {
    result <- base::table(..., exclude = exclude, useNA = useNA, 
                          deparse.level = deparse.level)
  } else {
    result <- base::table(..., exclude = exclude, useNA = useNA, 
                         dnn = dnn, deparse.level = deparse.level)
  }
  
  # Only enhance if we have exactly 2 dimensions and 2 arguments
  if (length(dim(result)) == 2 && length(dots) == 2) {
    var_names <- character(2)
    
    # Try to extract variable names from expressions
    for (i in 1:2) {
      expr <- dot_expressions[[i]]
      
      # Check if it's a dataframe column reference: df$var
      if (is.call(expr) && length(expr) >= 3) {
        op <- expr[[1]]
        # Handle df$var
        if (identical(op, quote(`$`)) || identical(op, as.name("$"))) {
          var_names[i] <- as.character(expr[[3]])
        }
        # Handle df[["var"]] or df[, "var"] or df[, i]
        else if (identical(op, quote(`[`)) || identical(op, as.name("["))) {
          if (length(expr) >= 3) {
            col_expr <- expr[[3]]
            # Handle df[["var"]] - double bracket with character
            if (is.character(col_expr) && length(col_expr) == 1) {
              var_names[i] <- col_expr
            }
            # Handle df[, "var"] - single bracket with character column name
            else if (is.call(col_expr) && identical(col_expr[[1]], quote(`[`)) && 
                     length(col_expr) >= 2 && is.character(col_expr[[2]])) {
              var_names[i] <- col_expr[[2]]
            }
            # Handle df[, i] where i is a name or number
            else if (is.name(col_expr)) {
              # Try to evaluate to see if it's a character
              tryCatch({
                val <- eval(col_expr, parent.frame())
                if (is.character(val) && length(val) == 1) {
                  var_names[i] <- val
                }
              }, error = function(e) {})
            }
          }
        }
      }
    }
    
    # If we found variable names, enhance the dimnames
    if (any(nchar(var_names) > 0)) {
      dimn <- dimnames(result)
      
      # Enhance row names (first dimension)
      if (nchar(var_names[1]) > 0 && !is.null(dimn[[1]])) {
        dimn[[1]] <- paste0(var_names[1], "=", dimn[[1]])
      }
      
      # Enhance column names (second dimension)
      if (nchar(var_names[2]) > 0 && !is.null(dimn[[2]])) {
        dimn[[2]] <- paste0(var_names[2], "=", dimn[[2]])
      }
      
      dimnames(result) <- dimn
    }
  }
  
  # Convert to proportions if requested
  if (!is.null(prop)) {
    # Convert character values to numeric
    if (is.character(prop)) {
      prop_lower <- tolower(prop)
      if (prop_lower == "all") {
        prop <- 0
      } else if (prop_lower == "row") {
        prop <- 1
      } else if (prop_lower == "column") {
        prop <- 2
      } else {
        stop("prop must be 0, 1, 2, 'all', 'row', or 'column'")
      }
    }
    
    if (!prop %in% c(0, 1, 2)) {
      stop("prop must be 0, 1, 2, 'all', 'row', or 'column'")
    }
    
    # Get original dimnames before modification
    orig_dimn <- dimnames(result)
    
    if (prop == 0) {
      # Overall proportions: divide by sum of all cells
      total_sum <- sum(result, na.rm = TRUE)
      result <- result / total_sum
      prop_type <- "overall proportions"
      
      # Add row and column with marginal proportions, 100% only in bottom right
      if (length(dim(result)) == 2) {
        n_rows <- nrow(result)
        n_cols <- ncol(result)
        dimn <- dimnames(result)
        
        # Calculate column totals (marginal proportions for columns)
        col_totals <- colSums(result, na.rm = TRUE)
        
        # Add summary row with column marginal proportions
        summary_row <- matrix(col_totals, nrow = 1, ncol = n_cols)
        result <- rbind(result, summary_row)
        dimn[[1]] <- c(dimn[[1]], "Total")
        rownames(result) <- dimn[[1]]
        
        # Calculate row totals (marginal proportions for rows)
        row_totals <- rowSums(result[1:n_rows, , drop = FALSE], na.rm = TRUE)
        
        # Add summary column with row marginal proportions
        summary_col <- matrix(c(row_totals, 1.0), nrow = n_rows + 1, ncol = 1)
        # Bottom right corner is 1.0 (100%) - sum of all proportions
        result <- cbind(result, summary_col)
        dimn[[2]] <- c(dimn[[2]], "Total")
        colnames(result) <- dimn[[2]]
      }
      
    } else if (prop == 1) {
      # Row proportions: each row sums to 1
      row_sums <- rowSums(result, na.rm = TRUE)
      # Avoid division by zero
      row_sums[row_sums == 0] <- 1
      result <- result / row_sums
      prop_type <- "row proportions"
      
      # Add column with 100% for each row
      if (length(dim(result)) == 2) {
        n_rows <- nrow(result)
        dimn <- dimnames(result)
        summary_col <- matrix(1, nrow = n_rows, ncol = 1)  # 100% for each row
        result <- cbind(result, summary_col)
        dimn[[2]] <- c(dimn[[2]], "Total")
        colnames(result) <- dimn[[2]]
      }
      
    } else if (prop == 2) {
      # Column proportions: each column sums to 1
      col_sums <- colSums(result, na.rm = TRUE)
      # Avoid division by zero
      col_sums[col_sums == 0] <- 1
      result <- sweep(result, 2, col_sums, "/")
      prop_type <- "column proportions"
      
      # Add row with 100% for each column
      if (length(dim(result)) == 2) {
        n_cols <- ncol(result)
        dimn <- dimnames(result)
        summary_row <- matrix(1, nrow = 1, ncol = n_cols)  # 100% for each column
        result <- rbind(result, summary_row)
        dimn[[1]] <- c(dimn[[1]], "Total")
        rownames(result) <- dimn[[1]]
      }
    }
    
    # Show message
    message.col("sohn::table2() computed ",prop_type, col = "blue")
  }
  
  return(result)
}

