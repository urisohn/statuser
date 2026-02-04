# Utility functions (not exported)

# Package-level state (avoid global options)
.statuser_state <- new.env(parent = emptyenv())
#0 Set default values in a list
set_default <- function(x, name, value) {
  if (!name %in% names(x)) x[[name]] <- value
  x
}

# Helper function to initialize bottom plot with background
init_bottom_plot <- function(xlim, ylim, xlab, ylab, bg, cex.lab) {
  plot(NA, NA, 
       xlim = xlim, 
       ylim = ylim,
       xlab = xlab, 
       ylab = ylab,
       main = "", 
       xaxt = "n", 
       yaxt = "n",
       bty = "o",  # Show border
       font.lab = 2, 
       cex.lab = cex.lab)
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = bg, border = NA)
  box()
}
  


#1 Get colors

      get.colors <- function(k) {
                  if (k==1) return('steelblue')
          
          if (k == 2) {
            return(c("firebrick", "dodgerblue"))
          } else if (k == 3) {
            return( c('orange1','red1','red4'))
          } else if (k == 4) {
            return(c('orange1','red1', 'red4','black'))
          } else {
            # For 5+ groups, use a combination of colors that cycle
            # Start with the 4-group palette and add more colors
            base_colors <- c(  "orange1", "orange3", "red2", "red4",
                             "dodgerblue", "dodgerblue3", "blue1", "blue4",
                             "green", "darkgreen",  "darkorchid", "purple4","gray88", "gray11")
            return(base_colors[1:k])
          }
      }

      
      
#2 Nice exit
  exit <- function(msg, col='red4',font=2) {
    message2(msg,col=col,font=font)
    invokeRestart("abort") 
  }

#3 Determine decimal places for group values
  group_decimals <- function(v) {
    v <- abs(v[is.finite(v)])
    if (!length(v)) return(2)
    
    x <- min(v)
    if (x < 1)  return(3)
    if (x < 10) return(2)
    return(1)
  }

#3 Determine decimal places for group values
  group_decimals <- function(v) {
    v <- abs(v[is.finite(v)])
    if (!length(v)) return(2)
    
    x <- min(v)
    if (x < 1)  return(3)
    if (x < 10) return(2)
    return(1)
  }

