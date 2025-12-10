#' Plot frequencies of a variable, optionally by group (histogram without binning)
#'
#' Creates a frequency plot showing the frequency of every observed value,
#' displaying the full range from minimum to maximum value.
#'
#' @param x A numeric vector of values to plot frequencies for.
#' @param col Color for the bars. 
#' @param lwd Line width for the frequency bars. Default is 9.
#' @param value.labels Logical. If TRUE, displays frequencies on top of each line. 
#' @param add Logical. If TRUE, adds to an existing plot instead of creating a new one. 
#' @param by A grouping variable (with 2 or 3 unique values). If specified, frequencies are computed separately for each group and plotted with different colors. 
#' @param ... Pass on any argument accepted by \code{plot()} e.g., \code{xlab='x-axis'} , \code{main='Distribution of X'}
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
#' plot_freq(x)
#'
#' # Pass on some common \code{plot()} arguments
#' plot_freq(x, col = "steelblue", xlab = "Value", ylab = "Frequency",ylim=c(0,7))
#'
#' # Add to an existing plot
#' plot_freq(x, col = "dodgerblue")
#' plot_freq(x + 1, col = "red", add = TRUE)
#'

#' @export
plot_freq <- function(x,by=NULL, freq=TRUE, col='dodgerblue',lwd=9, value.labels=TRUE, add=FALSE,  ...) {
  # Capture x variable name for x-axis label (before potentially overwriting)
  x_name <- deparse(substitute(x))
  
  # Extract additional arguments
  dots <- list(...)
  
  # Extract 'add' from dots if passed via ... and remove it from dots
  if ("add" %in% names(dots)) {
    add <- dots$add
    dots$add <- NULL
  }
  
  # Extract 'by' from dots if passed via ... and remove it from dots
  if ("by" %in% names(dots)) {
    by <- dots$by
    dots$by <- NULL
  }
  
  # Extract 'col' from dots if passed via ... and remove it from dots
  if ("col" %in% names(dots)) {
    col <- dots$col
    dots$col <- NULL
  }
  
  # Handle 'by' grouping if specified
  if (!is.null(by)) {
    # Validate by argument
    if (length(by) != length(x)) {
      stop("'by' must have the same length as 'x'")
    }
    
    unique_by <- unique(by)
    n_groups <- length(unique_by)
    if (n_groups < 2 || n_groups > 3) {
      stop("'by' must have 2 or 3 unique values")
    }
    
    # Use provided colors if valid, otherwise use default colors for groups
    if (length(col) == n_groups && is.character(col)) {
      group_cols <- col
    } else {
      group_cols <- sohn:::get.colors(n_groups)
    }
    
    # Compute frequencies for each group
      all_xs <- sort(unique(x))
      group_freqs <- list()
    
    for (i in 1:n_groups) {
      group_x <- x[by == unique_by[i]]
      freq_table <- table(group_x)
      group_xs <- as.numeric(names(freq_table))
      group_fs <- as.numeric(freq_table)
      
      # Create full frequency vector for all x values (0 for missing)
      full_fs <- numeric(length(all_xs))
      idx <- match(group_xs, all_xs)
      full_fs[idx] <- group_fs
      
      group_freqs[[i]] <- list(xs = all_xs, fs = full_fs)
    }
    
    # Find overall max frequency for ylim
    max_fs <- max(sapply(group_freqs, function(gf) max(gf$fs, na.rm = TRUE)), na.rm = TRUE)
    
    # Only set up plot if not adding to existing plot
    if (!add) {
      # Set default xlim if not set
      if (!"xlim" %in% names(dots)) {
        dots$xlim <- c(min(all_xs), max(all_xs))
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
        y_max <- max_fs
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
      
      # Plot the frequencies (empty plot frame)
      plot_args <- c(list(x = all_xs, y = rep(0, length(all_xs))), dots)
      do.call(plot, plot_args)
      
      # Calculate total sample size and add it under the main title
      tot <- length(x)
      mtext(paste0("(N=", tot, ")"), side = 3, line = 0.35, font = 3)
      
      # Draw custom y-axis with tickmarks at 0, midpoint, and maximum (if we suppressed default)
      if (!user_provided_yaxt) {
        # Use max_fs for tick calculation (actual data range, not expanded ylim)
        y_max_plot <- max_fs
        
        if (freq == FALSE) {
          # When showing percentages, use pretty() to generate reasonable tick marks
          total <- length(x)
          y_max_pct <- (y_max_plot / total) * 100
          
          # Use pretty() to generate nice tick intervals (typically 4-5 ticks)
          pct_range <- c(0, y_max_pct)
          pct_ticks <- pretty(pct_range, n = 5)
          pct_ticks <- pct_ticks[pct_ticks >= 0 & pct_ticks <= y_max_pct + 0.1]
          
          # Convert percentage ticks back to raw frequencies for positioning
          y_ticks <- (pct_ticks / 100) * total
          
          # Create labels with % sign
          y_labels <- paste0(pct_ticks, "%")
          
          axis(2, at = y_ticks, labels = y_labels, las = 1)
        } else {
          # Use pretty() to generate nice tick intervals for frequency mode
          freq_range <- c(0, y_max_plot)
          y_ticks <- pretty(freq_range, n = 5)
          # Only keep ticks that are >= 0 and <= y_max_plot (with small tolerance for rounding)
          y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= y_max_plot + 0.1]
          
          axis(2, at = y_ticks, las = 1)
        }
      }
    }
    
    # Calculate offset for side-by-side bars (touching with exactly 0 gap)
    # To make bars touch exactly, we need to position them so their edges meet
    # The offset is calculated based on bar width in user coordinates
    # Get plot dimensions to convert lwd (points) to user coordinates
    usr <- par("usr")
    pin <- par("pin")  # plot dimensions in inches
    x_range <- usr[2] - usr[1]
    
    # Convert lwd from points to inches, then to user coordinates
    # 1 point = 1/72 inch
    lwd_inches <- lwd / 72
    lwd_user <- lwd_inches * (x_range / pin[1])
    
    # For bars to touch exactly: distance between centers = bar width
    # For 2 groups: offset = lwd_user / 2 (so bars at -offset and +offset touch)
    # For 3 groups: offset = lwd_user / 2 (so adjacent bars touch)
    offset_amount <- lwd_user / 2
    
    # Calculate offsets for each group (centered around x value)
    # For 2 groups: bars at -offset and +offset (distance = 2*offset = lwd_user)
    # For 3 groups: bars at -offset, 0, +offset (adjacent bars distance = offset = lwd_user/2)
    if (n_groups == 2) {
      offsets <- c(-offset_amount, offset_amount)
    } else if (n_groups == 3) {
      offsets <- c(-offset_amount, 0, offset_amount)
    }
    
    # Draw segments for each group (side by side)
    for (i in 1:n_groups) {
      gf <- group_freqs[[i]]
      non_zero <- gf$fs > 0
      if (any(non_zero)) {
        x_positions <- gf$xs[non_zero] + offsets[i]
        segments(x0 = x_positions, x1 = x_positions, y0 = 0, y1 = gf$fs[non_zero], 
                 lwd = lwd, col = group_cols[i], lend = 1)
      }
    }
    
    # Add value labels with frequencies (colored by group)
    if (value.labels) {
      # For each x value and group, add label if frequency > 0
      for (j in 1:length(all_xs)) {
        x_val <- all_xs[j]
        for (i in 1:n_groups) {
          freq_val <- group_freqs[[i]]$fs[j]
          if (freq_val > 0) {
            x_label_pos <- x_val + offsets[i]
            text(x = x_label_pos, y = freq_val, labels = freq_val, 
                 cex = 0.7, pos = 3, col = group_cols[i])
          }
        }
      }
    }
    
    # Add legend showing groups and colors
    if (!add) {
      # Calculate sample sizes for each group
      group_ns <- sapply(1:n_groups, function(i) {
        length(x[by == unique_by[i]])
      })
      
      # Create legend labels with sample sizes
      legend_labels <- paste0(as.character(unique_by), " (N=", group_ns, ")")
      legend_cols <- group_cols[1:n_groups]
      
      # Add legend
      legend("topleft", legend = legend_labels, col = legend_cols, 
             lwd = lwd, bty = "n")
    }
    
    # Return frequencies invisibly (combined across groups)
    combined_fs <- rowSums(sapply(group_freqs, function(gf) gf$fs))
    return(invisible(data.frame(value = all_xs, frequency = combined_fs)))
  }
  
  # Calculate frequencies for each unique value (only if by is not used)
  freq_table <- table(x)
  xs <- as.numeric(names(freq_table))
  fs <- as.numeric(freq_table)
  fsp=fs
  
  if (freq==FALSE)
  {
    
    fsp   = paste0(round(100*(fs/sum(fs)),0),"%")
    
  }
    
  # Only set up plot if not adding to existing plot
  if (!add) {
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
      
    # Calculate total sample size and add it under the main title
        tot <- sum(fs)
        mtext(paste0("(N=", tot, ")"), side = 3, line = 0.35, font = 3)
            
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
          # When showing percentages, use pretty() to generate reasonable tick marks
          total <- sum(fs)
          y_max_pct <- (y_max_plot / total) * 100
          
          # Use pretty() to generate nice tick intervals (typically 4-5 ticks)
          # pretty() will choose appropriate intervals (e.g., 0, 5, 10, 15, 20 or 0, 10, 20, 30, etc.)
          pct_range <- c(0, y_max_pct)
          pct_ticks <- pretty(pct_range, n = 5)
          # Only keep ticks that are >= 0 and <= y_max_pct (with small tolerance for rounding)
          pct_ticks <- pct_ticks[pct_ticks >= 0 & pct_ticks <= y_max_pct + 0.1]
          
          # Convert percentage ticks back to raw frequencies for positioning
          y_ticks <- (pct_ticks / 100) * total
          
          # Create labels with % sign
          y_labels <- paste0(pct_ticks, "%")
          
          axis(2, at = y_ticks, labels = y_labels, las = 1)
        } else {
          # Use pretty() to generate nice tick intervals for frequency mode
          freq_range <- c(0, y_max_plot)
          y_ticks <- pretty(freq_range, n = 5)
          # Only keep ticks that are >= 0 and <= y_max_plot (with small tolerance for rounding)
          y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= y_max_plot + 0.1]
          
          axis(2, at = y_ticks, las = 1)
        }
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

# Alias for backward compatibility
#' @export
fhist <- function(...) {
  message.col("The function fhist() is deprecated, please use plot_freq() instead going forward", col = "red", font = 2)
  plot_freq(...)
}

