#' Plot empirical distribution of a variable (histogram without binning)
#'
#' Creates a frequency plot showing the frequency of every observed value,
#' displaying the full range from minimum to maximum value.
#'
#' @param x A numeric vector of values to plot frequencies for.
#' @param col Color for the frequency bars. Default is "dodgerblue".
#' @param lwd Line width for the frequency bars. Default is 9.
#' @param value.labels Logical. If TRUE, displays frequencies on top of each line. Default is TRUE
#' @param ... Pass on any other argument that's accepted by \code{plot()}.
#'
#' @return Invisibly returns a data frame with values and their frequencies.
#'
#' @details
#' This function creates a frequency plot where each observed value is shown
#' with its frequency. Unlike a standard histogram, there is no binning, unlike
#' a barplot, non-observed values of the variable are shown with 0 frequency 
#' instead of skipped.
#'
#' @examples
#' # Simple example
#' x <- c(1, 1, 2, 2, 2, 5, 5)
#' fhist(x)
#'
#' # Pass on some common \code{plot()} arguments
#' fhist(x, col = "steelblue", xlab = "Value", ylab = "Frequency",ylim=c(0,7))
#'

#' @export
fhist <- function(x, freq=TRUE, col='dodgerblue',lwd=9, value.labels=TRUE, ...) {
  # Capture x variable name for x-axis label (before potentially overwriting)
  x_name <- deparse(substitute(x))
  
  # Extract additional arguments
  dots <- list(...)
  
  # Calculate frequencies for each unique value
    freq_table <- table(x)
    xs <- as.numeric(names(freq_table))
    fs <- as.numeric(freq_table)
    fsp=fs
    
    if (freq==FALSE)
    {
      
      fsp   = paste0(round(100*(fs/sum(fs)),0),"%")
      
    }
    
  #########################################################
  #Default figure parameters if not set
    # xlim if not set
        if (!"xlim" %in% names(dots)) {
          dots$xlim <- c(min(xs), max(xs))
        }
    
    # Set default xlab if not provided
        if (!"xlab" %in% names(dots)) dots$xlab <- x_name
    
    # Set default ylab if not provided
        if (!"ylab" %in% names(dots) && freq) dots$ylab <- "Frequency"
        if (!"ylab" %in% names(dots) && !freq) dots$ylab <- "% of Observations"
        
    # Set default main title if not provided
       if (!"main" %in% names(dots)) dots$main <- paste("Distribution of", x_name)
    
    # Set default ylim to start at 0 if not provided
        if (!"ylim" %in% names(dots)) {
          y_max <- max(fs, na.rm = TRUE)
          # Add extra space at top if value labels are shown
          if (value.labels == TRUE && y_max > 0) {
            y_max <- y_max + max(1, y_max * 0.15)  # Add 15% or at least 1 unit
          }
          dots$ylim <- c(0, y_max)
        }
      
        # Remove default axis padding to eliminate gap at bottom
          if (!"yaxs" %in% names(dots)) dots$yaxs <- "i"
          
        # Only set xaxs = "i" if xlim is not provided (to allow padding when xlim is set)
          if (!"xaxs" %in% names(dots) && !"xlim" %in% names(dots)) dots$xaxs <- "i"
        
        # Add las=1 to dots if not provided
          if (!"las" %in% names(dots)) dots$las <- 1
        
        # Set axis label formatting (xlab/ylab, not tick labels)
          if (!"font.lab" %in% names(dots)) dots$font.lab <- 2
          if (!"cex.lab" %in% names(dots)) dots$cex.lab <- 1.2
        
        # Set type='n' to create plot frame without drawing points (unless user specifies type)
          if (!"type" %in% names(dots)) dots$type <- "n"
        
        # Track if user provided yaxt - if not, we'll draw custom axis
          user_provided_yaxt <- "yaxt" %in% names(dots)
          if (!user_provided_yaxt) dots$yaxt <- "n"
  #########################################################
  
          
                
  # Plot the frequencies (empty plot frame)
      plot_args <- c(list(x = xs, y = fs), dots)
      do.call(plot, plot_args)
    
          
  # Draw custom y-axis with tickmarks at 0, midpoint, and maximum (if we suppressed default)
    if (!user_provided_yaxt) {
      # Get y-axis range
      y_max <- max(fs, na.rm = TRUE)
      if ("ylim" %in% names(dots)) {
        y_max_plot <- dots$ylim[2]
      } else {
        y_max_plot <- y_max
      }
      
      if (freq == FALSE) {
        # When showing percentages, set ticks at multiples of 10%
        total <- sum(fs)
        y_max_pct <- (y_max_plot / total) * 100
        
        # Generate ticks at multiples of 10% that fit within the range
        pct_ticks <- seq(0, ceiling(y_max_pct / 10) * 10, by = 10)
        # Only keep ticks that are <= y_max_pct (or slightly above for rounding)
        pct_ticks <- pct_ticks[pct_ticks <= y_max_pct + 1]
        
        # Convert percentage ticks back to raw frequencies for positioning
        y_ticks <- (pct_ticks / 100) * total
        
        # Create labels with % sign
        y_labels <- paste0(pct_ticks, "%")
        
        axis(2, at = y_ticks, labels = y_labels, las = 1)
      } else {
        # Original behavior for frequency mode
        # Calculate midpoint and round all values to integers (since these are counts)
        y_max_rounded <- round(y_max_plot)
        y_mid <- round(y_max_rounded / 2)
        
        # Draw tickmarks at 0, midpoint, and maximum (all integers)
        y_ticks <- c(0, y_mid, y_max_rounded)
        axis(2, at = y_ticks, las = 1)
      }
    }
    
  # Identify non-zero frequencies (only draw segments and labels for these)
    non_zero <- fs > 0
  
  # Draw segments for each value (only non-zero frequencies)
    if (any(non_zero)) {
      segments(x0 = xs[non_zero], x1 = xs[non_zero], y0 = 0, y1 = fs[non_zero], 
               lwd = lwd, col = col, lend = 1)
    }
    
  # Add value labels if requested
    if (value.labels == TRUE && any(non_zero)) {
      text(x = xs[non_zero], y = fs[non_zero], labels = fsp[non_zero], 
           cex = 0.7, pos = 3)
    }
    
  # Return frequencies invisibly
  invisible(data.frame(value = xs, frequency = fs))
}

