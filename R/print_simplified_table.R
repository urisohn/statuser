#' Print method for simplified table output
#'
#' @param x An object of class \code{simplified_table}
#' @param ... Additional arguments passed to print
#'
#' @export
print.simplified_table <- function(x, ...) {
  # Get variable names if available
  var_names <- attr(x, "var_names")
  
  # Get dimensions
  dims <- dim(x)
  n_dims <- length(dims)
  
  # Convert to regular table for printing
  x_print <- unclass(x)
  
  # If we have variable names, add them as dimension names
  if (!is.null(var_names) && length(var_names) == n_dims) {
    # Set names of dimnames to show variable names in margins
    if (is.null(names(dimnames(x_print)))) {
      names(dimnames(x_print)) <- var_names
    } else {
      names(dimnames(x_print)) <- var_names
    }
  }
  
  # Print the table (R's print.table will show variable names in margins)
  print(x_print)
  
  invisible(x)
}







