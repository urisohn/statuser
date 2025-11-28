#' Simulation Counter
#'
#' Functoin that adaptively find the frequency with which to report simulation
#' progress so that it reports approximtaely within 5 seconds.
#'
#' @return Takes a single argument \code{simk} (the current
#'   iteration number) and prints progress updates.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Monitors execution time at checkpoints (iterations 2, 5, 10, 50)
#'   \item Automatically determines an optimal reporting interval based on a
#'     target time (5 seconds by default)
#'   \item Prints progress updates at the chosen interval
#'   \item Adapts to fast or slow iterations
#' }
#'
#' The reporter works in two phases:
#' \itemize{
#'   \item \strong{Discovery phase}: Checks execution time at checkpoints. If
#'     any checkpoint takes longer than the target time, it sets the reporting
#'     interval to that checkpoint value.
#'   \item \strong{Reporting phase}: Once an interval is chosen, it reports
#'     progress every N iterations, where N is the chosen interval.
#' }
#'
#' If all checkpoints complete quickly (before reaching iteration 50), the
#' function estimates an appropriate interval based on the average time per
#' iteration.
#'
#' @examples
#' # Create a progress reporter
#' report <- counter()
#'
#' # Use it in a simulation loop
#' for (i in 1:100) {
#'   # Your simulation code here
#'   Sys.sleep(0.1)  # Simulate work
#'   report(i)  # Report progress
#' }
#'
#' # The reporter will automatically determine when to print updates
#' # based on how long each iteration takes
#'
#' @export
counter <- function() {
  checkpoints <- c(2, 5, 10, 50)
  target_sec  <- 5
  chosen_int  <- NULL
  t_start     <- proc.time()[3]

  function(simk) {
    # If interval already chosen
    if (!is.null(chosen_int)) {
      if (simk %% chosen_int == 0) cat("sim", simk, "\n")
      return(invisible(NULL))
    }

    # We're still in the discovery phase
    if (simk %in% checkpoints) {
      elapsed <- proc.time()[3] - t_start
      if (elapsed >= target_sec) {
        chosen_int <<- simk   # print every 'simk' steps
        cat("[set interval =", chosen_int, "] sim", simk, "\n")
        return(invisible(NULL))
      }
    }

    # At simk = 50 we decide if none were slow
    if (simk == 50) {
      elapsed <- proc.time()[3] - t_start
      # estimated cost per iteration
      per_iter <- elapsed / simk
      chosen_int <<- max(1, round(target_sec / per_iter))
      cat("[estimated interval =", chosen_int, "] sim", simk, "\n")
      return(invisible(NULL))
    }

    # Default: print only discovery checkpoints
    if (simk %in% checkpoints) cat("sim", simk, "\n")
  }
}
