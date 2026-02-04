#' Clear All: Environment, Console, and Graphics
#'
#' @param envir The environment to clear. Defaults to the calling environment.
#'   The global environment is not modified by this function.
#'
#' @return Invisibly returns NULL. Prints a colored confirmation message.
#'
#' @details
#' This function performs three cleanup operations:
#' \itemize{
#'   \item \strong{Environment}: Removes all objects from the specified environment
#'   \item \strong{Console}: Clears the console screen (only in interactive sessions)
#'   \item \strong{Graphics}: Closes all open graphics devices (except the null device)
#' }
#'
#' \strong{Warning}: This function deletes all objects in the specified
#' environment. Save anything that you wish to keep before running.
#'
#' @examples
#' \donttest{
#' # Clear a temporary environment (safe for examples)
#' tmp_env <- new.env()
#' tmp_env$x <- 1:10
#' tmp_env$y <- rnorm(10)
#' clear(tmp_env)
#' }
#'
#' @export
clear <- function(envir = parent.frame()) {
  if (identical(envir, .GlobalEnv)) {
    warning("clear() will not modify the global environment. Provide a different environment via `envir`.")
    return(invisible(NULL))
  }

  # Clear environment
  rm(list = ls(envir = envir), envir = envir)
  
  # Clear console (only in interactive sessions)
  if (interactive()) {
    cat("\014")
  }
  
  # Clear all plots (close all devices except null device)
  # Close devices one at a time, checking each iteration
  dev_list <- dev.list()
  if (!is.null(dev_list) && length(dev_list) > 0) {
    for (dev_num in dev_list) {
      if (dev_num != 1) {  # Don't close the null device
        tryCatch({
          dev.off(which = dev_num)
        }, error = function(e) {
          # Device may have been closed already, ignore error
        })
      }
    }
  }
  
  # Print confirmation message
  
  message2("statuser::clear()", font=2,col = "red4")
  message2("Cleared console, plot, and environment",col = "red4")
  
  invisible(NULL)
}
