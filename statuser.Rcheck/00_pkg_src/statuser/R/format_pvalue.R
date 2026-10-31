#' Format P-Values for Display 
#'
#' Formats p-values for clean display in figures and tables. e.g., p = .0231, p<.0001
#'
#' @param p A numeric vector of p-values to format.
#' @param digits Number of decimal places to round to. Default is 4.
#' @param include_p Logical. If TRUE, includes "p" prefix before the formatted
#'   value (e.g., "p = .05"). Default is FALSE.
#'
#' @return A character vector of formatted p-values.
#'
#' @examples
#' # Basic usage
#' format_pvalue(0.05)
#' format_pvalue(0.0001)
#' 
#' # More rounding
#' format_pvalue(0.0001,digits=2)
#'
#' # Vector input
#' format_pvalue(c(0.05, 0.001, 0.00001, 0.99))
#'
#' # With p prefix
#' format_pvalue(0.05, include_p = TRUE)
#'
#' @name format_pvalue
#' @export format_pvalue
format_pvalue <- function(p, digits = 4, include_p = FALSE) {
  # Handle NA values
  is_na <- is.na(p)
  result <- character(length(p))
  
  # Create p prefix if requested
  p_prefix <- if (include_p) "p " else ""
  
  # Handle edge cases first

  # Create dynamic threshold strings based on digits (e.g., ".01" for digits=2, ".001" for digits=3)
  min_threshold <- 10^(-digits)
  max_threshold <- 1 - 10^(-digits)
  min_str <- sub("^0\\.", ".", format(min_threshold, nsmall = digits, scientific = FALSE))
  max_str <- sub("^0\\.", ".", format(max_threshold, nsmall = digits, scientific = FALSE))
  
  result[p < min_threshold] <- paste0(p_prefix, "< ", min_str)
  result[p > max_threshold] <- paste0(p_prefix, "> ", max_str)
  
  # Handle regular p-values
  regular <- !is_na & p >= 10^(-digits) & p <= (1 - 10^(-digits))
  
  if (any(regular)) {
    p.clean <- round(p[regular], digits)
    p.clean <- format(p.clean, nsmall = digits, scientific = FALSE)
    # Remove leading zero
    p.clean <- sub("^0\\.", ".", p.clean)
    # Always add equals sign
    p.clean <- paste0(p_prefix, "= ", p.clean)
    result[regular] <- p.clean
  }
  
  # Handle NA values
  result[is_na] <- NA_character_
  
  return(result)
}

