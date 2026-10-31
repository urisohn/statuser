#' Clear cache used by stimulus.plot() to reuse resampling results from identical previous calls.
#'
#' When `stimulus.plot()` with `plot.type = 'effects'` is run, resamples are saved in
#' package state. If an identical call is run again (same data, dv, condition, number of
#' simulations, etc.), stored results are loaded instead of re-calculated. Force
#' recalculation by clearing the cache with this function.
#'
#' @export
clear_stimulus_cache <- function() {
  .statuser_state$stimulus_cache <- list()
  message2("Cache for stimulus.plot() has been cleared.")
}
