#' Enhanced alternative to text()
#' 
#' Adds to text() optional background color and verbal alignment (align='center')
#'
#' @param x,y coordinates for text placement 
#' @param labels text to display
#' @param align alignment in relation to x coordinate ('left','center','right')
#' @param bg background color
#' @param cex character expansion factor
#' @param pad left/right padding in percentage (e.g., .03)
#' @param pad_v top/bottom padding in percentage (e.g., .25)
#' @param ... Additional arguments passed to \code{\link[graphics]{text}}.
#'
#' @examples
#' # Create a simple plot
#' plot(1:10, 1:10, type = "n", main = "text2() - Alignment & Color")
#' 
#' # Alignment respect to x=5
#' text2(5, 8, "align='left' from 5", align = "left", bg = "yellow1")
#' text2(5, 7, "align='right' from 5", align = "right", bg = "blue", col = "white")
#' text2(5, 6, "align='center' from 5", align = "center", bg = "black", col = "white")
#' abline(v = 5, lty = 2)
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
#' # Padding examples
#' plot(1:10, 1:10, type = "n", main = "Padding Examples")
#' 
#' # Default padding (pad=0.03, pad_v=0.25)
#' text2(5, 8, "Default padding", bg = "lightblue")
#' 
#' # More horizontal padding
#' text2(5, 6, "Wide padding", pad = 0.2, bg = "lightgreen")
#' 
#' # More vertical padding
#' text2(5, 4, "Tall padding", pad_v = 0.8, bg = "lightyellow")
#' 
#' # Both padding increased
#' text2(5, 2, "Extra padding", pad = 0.15, pad_v = 0.6, bg = "pink")
#' @usage NULL
#' @export
text2 <- function(x, y, labels, align="center", bg="papayawhip", cex=1, pad=0.03, pad_v=0.25,  ...) {

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
