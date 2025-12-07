# Utility functions (not exported)

#' Get default colors for plotting
#'
#' Returns a vector of colors based on the number of groups.
#' This is an internal utility function used by cdf.by() and density.by().
#'
#' @param k Number of colors to return
#' @return A character vector of color names
#'
#' @details
#' Color schemes:
#' \itemize{
#'   \item 2 groups: red4, dodgerblue
#'   \item 3 groups: red4, dodgerblue, green4
#'   \item 4 groups: orange1, orange3, red2, red4
#'   \item 5+ groups: extends the 4-group palette with additional colors (dodgerblue, blue, green, purple, gray)
#' }
get.colors <- function(k) {
  if (k == 2) {
    return(c("red4", "dodgerblue"))
  } else if (k == 3) {
    return(c("red4", "dodgerblue", "green4"))
  } else if (k == 4) {
    return(c("orange1", "orange3", "red2", "red4"))
  } else {
    # For 5+ groups, use a combination of colors that cycle
    # Start with the 4-group palette and add more colors
    base_colors <- c(  "orange1", "orange3", "red2", "red4",
                     "dodgerblue", "dodgerblue3", "blue1", "blue4",
                     "green", "darkgreen",  "darkorchid", "purple4","gray88", "gray11")
    return(base_colors[1:k])
  }
}

