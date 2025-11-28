#' Simplify t-test output
#'
#' Takes the output from \code{\link[stats]{t.test}} and produces simplified output.
#' This function extracts variable names from the t.test output and formats the results.
#'
#' @param object An htest object from t.test()
#' @param digits Number of decimal places to display for means and difference
#'   of means. Default is 3.
#' @param calling_env The environment where simplify() was called (used to extract
#'   original variables for formula syntax). Internal use only.
#' @param ... Additional arguments (not currently used).
#'
#' @return The same object with enhanced printing (invisibly).
#'
#' @details
#' This function parses the \code{data.name} attribute from the t.test output
#' to extract variable names and then formats the output in a simplified format.
#'
#' @examples
#' # Two-sample t-test
#' men <- rnorm(100, mean = 5, sd = 1)
#' women <- rnorm(100, mean = 4.8, sd = 1)
#' result <- t.test(men, women)
#' simplify_ttest(result)
#'
#' # Formula syntax
#' data <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
#' result <- t.test(y ~ group, data = data)
#' simplify_ttest(result)
#'
#' @seealso \code{\link[stats]{t.test}}, \code{\link{simplify}}
#'
#' @keywords internal
simplify_ttest <- function(object, digits = 3, calling_env = NULL, ...) {
  if (!inherits(object, "htest")) {
    stop("object must be of class 'htest'")
  }
  
  if (!grepl("t-test", object$method, ignore.case = TRUE)) {
    stop("simplify_ttest() only works with t.test() output")
  }
  
  # Parse data.name to extract variable names
  data_name <- object$data.name
  
  # Check if it's formula syntax (contains "by" or "~")
  is_formula <- grepl(" by | ~ ", data_name)
  
  if (is_formula) {
    # Formula syntax: "y by group" or "y ~ group"
    # Split on " by " or " ~ "
    parts <- strsplit(data_name, " by | ~ ")[[1]]
    if (length(parts) == 2) {
      y_var_name <- trimws(parts[1])
      group_var_name <- trimws(parts[2])
      
      object$y_var_name <- y_var_name
      object$group_var_name <- group_var_name
      object$is_formula <- TRUE
      
      # Try to get original data from the environment where simplify() was called
      # This allows us to show "When cond==0" format
      if (is.null(calling_env)) {
        # Fallback: try parent.frame() and .GlobalEnv
        env <- tryCatch(parent.frame(), error = function(e) .GlobalEnv)
      } else {
        env <- calling_env
      }
      
      tryCatch({
        y_var <- get(y_var_name, envir = env)
        group_var <- get(group_var_name, envir = env)
        if (length(y_var) > 0 && length(group_var) > 0 && length(y_var) == length(group_var)) {
          object$y_var <- y_var
          object$group_var <- group_var
        } else {
          object$y_var <- NULL
          object$group_var <- NULL
        }
      }, error = function(e) {
        object$y_var <- NULL
        object$group_var <- NULL
      })
      
      # Calculate difference
      if (length(object$estimate) == 2) {
        object$diff <- object$estimate[1] - object$estimate[2]
      }
    }
  } else {
    # Standard syntax: "x and y" or just "x"
    if (grepl(" and ", data_name)) {
      parts <- strsplit(data_name, " and ")[[1]]
      x_name <- trimws(parts[1])
      y_name <- trimws(parts[2])
      object$x.name <- x_name
      object$y.name <- y_name
      object$is_formula <- FALSE
      
      if (length(object$estimate) == 2) {
        object$diff <- object$estimate[1] - object$estimate[2]
      }
    } else {
      # One-sample test
      object$x.name <- trimws(data_name)
      object$y.name <- NULL
      object$is_formula <- FALSE
      object$diff <- NULL
    }
  }
  
  # Store digits
  object$digits <- digits
  
  # Change class to use our print method
  class(object) <- c("simplified_ttest", class(object))
  
  # Print the enhanced output
  print(object)
  
  invisible(object)
}
