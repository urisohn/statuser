#' text() wrapper: Adds intuitive text-alignment & background color
#'
#'
#' @param x Numeric vector of x coordinates where the text should be placed.
#' @param y Numeric vector of y coordinates where the text should be placed.
#' @param labels Character vector of text labels to display.
#' @param bg Character string or vector specifying the background color(s). Default is "yellow".
#'   Can be recycled if length is less than length of x.
#' @param cex Numeric character expansion factor. Default is 1.
#' @param pad Percentage of padding to add to background to left & right side of the text. Default is 0.03.
#' @param pad_v Percentage of padding to add to background to top & bottom of the text. Default is 0.03.
#' @param align Character string or vector specifying text alignment.
#'   Options are \code{"left"}, \code{"center"}, or \code{"right"}.
#'   \describe{
#'     \item{"left"}{Aligns text so the left edge is at the x coordinate.}
#'     \item{"center"}{Aligns text so the center is at the x coordinate (default).}
#'     \item{"right"}{Aligns text so the right edge is at the x coordinate.}
#'   }
#' @param ... Additional arguments passed to \code{\link[graphics]{text}}.
#'
#' @details
#' This function is designed for use with base R plots. It draws a colored background
#' rectangle behind text labels, making them more readable when overlaid on plots.
#' The background rectangle is automatically sized based on the text dimensions and padding.
#'
#' The \code{align} parameter controls how the text is positioned relative to the x coordinate:
#' \itemize{
#'   \item \code{"left"}: The left edge of the text aligns with x
#'   \item \code{"center"}: The center of the text aligns with x (default behavior)
#'   \item \code{"right"}: The right edge of the text aligns with x
#' }
#'
#' @examples
#' # Create a simple plot
#' plot(1:10, 1:10, type = "n", main = "text2 Examples")
#' 
#' # Left-aligned text
#' text2(2, 8, "Left", align = "left", bg = "lightblue")
#' 
#' # Center-aligned text (default)
#' text2(5, 8, "Center", align = "center", bg = "lightgreen")
#' 
#' # Right-aligned text
#' text2(8, 8, "Right", align = "right", bg = "lightyellow")
#' 
#' # Multiple labels with different alignments
#' text2(c(2, 5, 8), c(5, 5, 5), 
#'       labels = c("Left", "Center", "Right"),
#'       align = c("left", "center", "right"),
#'       bg = c("pink", "lightblue", "lightgreen"))
#' 
#' # Text with custom font color (passed through ...)
#' text2(5, 3, "Red Text", col = "red", bg = "white")
#'
#' @export
text2 <- function(x, y, labels, bg="papayawhip", cex=1, pad=0.03, pad_v=0.25, align="center", ...) {

  # Validate and recycle align argument
  valid_aligns <- c("left", "center", "right")
  if (length(align) == 1) {
    align <- match.arg(align, valid_aligns)
    align <- rep(align, length(x))
  } else {
    align <- match.arg(align, valid_aligns, several.ok = TRUE)
    if (length(align) != length(x)) {
      align <- rep(align, length.out = length(x))
    }
  }
  
  # compute text physical size in USER coordinates
  w <- strwidth(labels, cex = cex)
  h <- strheight(labels, cex = cex)

  # padding
  pad_w <- pad * w
  pad_h <- pad_v * h

  # Process each text label
  for (i in seq_along(x)) {
    # Determine horizontal adjustment based on alignment
    if (align[i] == "left") {
      adj_x <- 0
      rect_x1 <- x[i] - pad_w[i]
      rect_x2 <- x[i] + w[i] + pad_w[i]
    } else if (align[i] == "right") {
      adj_x <- 1
      rect_x1 <- x[i] - w[i] - pad_w[i]
      rect_x2 <- x[i] + pad_w[i]
    } else { # center
      adj_x <- 0.5
      rect_x1 <- x[i] - (w[i]/2 + pad_w[i])
      rect_x2 <- x[i] + (w[i]/2 + pad_w[i])
    }
    
    # Handle vectorized bg
    bg_i <- if (length(bg) > 1) bg[i] else bg
    
    # draw background rectangle
    rect(rect_x1,
         y[i] - (h[i]/2 + pad_h[i]),
         rect_x2,
         y[i] + (h[i]/2 + pad_h[i]),
         col = bg_i, border = NA)

    # draw text with appropriate alignment
    text(x[i], y[i], labels[i],
         cex = cex,
         adj = c(adj_x, 0.5),
         ...)
  }
}
