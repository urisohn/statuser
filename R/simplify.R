#' Simplify Statistical Test Output
#'
#' A generic function that simplifies the output of various statistical tests
#' to make them more readable and informative.
#'
#' @param object The output object from a statistical test (e.g., from \code{t.test}).
#' @param digits Number of decimal places to display. Default is 3.
#' @param ... Additional arguments passed to specific simplify methods.
#'
#' @return A simplified version of the test output with enhanced formatting.
#'
#' @details
#' This function provides a unified interface for simplifying statistical test outputs.
#' It automatically detects the type of test object and routes it to the appropriate
#' simplification method.
#'
#' @examples
#' # Simplify t-test output
#' result <- t.test(rnorm(100), rnorm(100))
#' simplify(result)
#'
#' @export
simplify <- function(object, digits = 3, ...) {
  UseMethod("simplify")
}

#' Default method for simplify
#'
#' @param object The object to simplify
#' @param digits Number of decimal places
#' @param ... Additional arguments
#'
#' @export
simplify.default <- function(object, digits = 3, ...) {
  stop("No simplify method available for objects of class: ", 
       paste(class(object), collapse = ", "))
}

#' Simplify t-test output
#'
#' Takes the output from \code{\link[stats]{t.test}} and produces a simplified,
#' more readable output with variable names and formatted results.
#'
#' @param object An object of class \code{"htest"} from \code{\link[stats]{t.test}}.
#' @param digits Number of decimal places to display for means and difference
#'   of means. Default is 3.
#' @param ... Additional arguments (not currently used).
#'
#' @return The same object with enhanced printing (invisibly).
#'
#' @details
#' This function enhances t-test output by:
#' \itemize{
#'   \item Displaying actual variable names instead of "mean of x" and "mean of y"
#'   \item Adding the observed difference of means to the output
#'   \item Formatting results in a cleaner, more readable format
#' }
#'
#' The function extracts variable names from the \code{data.name} attribute of
#' the t-test object when possible.
#'
#' @examples
#' # Two-sample t-test
#' men <- rnorm(100, mean = 5, sd = 1)
#' women <- rnorm(100, mean = 4.8, sd = 1)
#' result <- t.test(men, women)
#' simplify(result)
#'
#' # Formula syntax
#' data <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
#' result <- t.test(y ~ group, data = data)
#' simplify(result)
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @export
simplify.htest <- function(object, digits = 3, ...) {
  # Check if it's a t-test
  if (!grepl("t-test", object$method, ignore.case = TRUE)) {
    stop("simplify() for htest objects currently only supports t-test results")
  }
  
  # Store the calling environment for simplify_ttest to access original variables
  # This allows us to extract group values for formula syntax
  calling_env <- parent.frame()
  
  # Route to simplify_ttest
  simplify_ttest(object, digits = digits, calling_env = calling_env, ...)
}
