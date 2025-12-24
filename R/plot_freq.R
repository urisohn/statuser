#' Plot frequencies of a variable, optionally by group (histogram without binning)
#'
#' Creates a frequency plot showing the frequency of every observed value,
#' displaying the full range from minimum to maximum value.
#'
#' @param x A numeric vector of values to plot frequencies for, or a column name (character string) if \code{data} is provided.
#' @param col Color for the bars. 
#' @param lwd Line width for the frequency bars. Default is 9.
#' @param value.labels Logical. If TRUE, displays frequencies on top of each line. 
#' @param add Logical. If TRUE, adds to an existing plot instead of creating a new one. 
#' @param group A grouping variable (with 2 or 3 unique values). If specified, frequencies are computed separately for each group and plotted with different colors. Can be a vector or a column name (character string) if \code{data} is provided.
#' @param data Optional data frame containing the variables \code{x} and optionally \code{group}.
#' @param show.legend Logical. If TRUE (default), displays a legend when \code{group} is specified. If FALSE, no legend is shown.
#' @param legend.title Character string. Title for the legend when \code{group} is specified. If NULL (default), no title is shown.
#' @param col.text Color for the value labels. If not specified, uses \code{col} for non-grouped plots or group colors for grouped plots.
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
#' # Using a data frame
#' df <- data.frame(value = c(1, 1, 2, 2, 2, 5, 5), group = c("A", "A", "A", "B", "B", "A", "B"))
#' plot_freq(x = "value", data = df)
#' plot_freq(x = value, group = group, data = df)  # unquoted column names also work
#'

#' @export
plot_freq <- function(x, group=NULL, freq=TRUE, col='dodgerblue', lwd=9, width=NULL, value.labels=TRUE, add=FALSE, data=NULL, show.legend=TRUE, legend.title=NULL, col.text=NULL, ...) {
  # Handle group expressions (like c(1,2,3)) separately before validation
  group_expr <- substitute(group)
  group_was_provided <- !is.null(group_expr)
  group_is_expression <- FALSE
  
  # Check if group is an expression (not a simple column name)
  if (group_was_provided && !is.null(data)) {
    group_name_raw_temp <- if (is.character(group_expr) && length(group_expr) == 1) {
      group_expr
    } else {
      deparse(group_expr)
    }
    group_name_raw_temp <- gsub('^"|"$', '', group_name_raw_temp)
    
    # If it contains operators/parentheses, it's likely an expression
    if (grepl("[()\\[\\]\\+\\-\\*/]", group_name_raw_temp)) {
      group_is_expression <- TRUE
      # Evaluate the expression
      tryCatch({
        group <- eval(group_expr, envir = parent.frame())
      }, error = function(e) {
        stop(sprintf("plot_freq(): Could not evaluate 'group' expression '%s'", group_name_raw_temp), call. = FALSE)
      })
    }
  }
  
  # Extract additional arguments
  dots <- list(...)
  
  # Validate inputs using validation function shared with plot_density, plot_cdf, plot_freq (only if group is not an expression)
  if (!group_is_expression) {
    validated <- validate_plot(x, group, data, func_name = "plot_freq", require_group = FALSE)
    x <- validated$y  # Note: validate_plot uses 'y' but we pass 'x'
    group <- validated$group
    x_name <- validated$y_name
    group_name <- validated$group_name
    x_name_raw <- validated$y_name_raw
    group_name_raw <- validated$group_name_raw
  } else {
    # Group was an expression, validate x separately
    validated <- validate_plot(x, NULL, data, func_name = "plot_freq", require_group = FALSE)
    x <- validated$y
    x_name <- validated$y_name
    x_name_raw <- validated$y_name_raw
    # For group expressions, we need to capture the name differently
    group_name_raw <- if (is.character(group_expr) && length(group_expr) == 1) {
      group_expr
    } else {
      deparse(group_expr)
    }
    group_name_raw <- gsub('^"|"$', '', group_name_raw)
    group_name <- group_name_raw
  }
  
  # Handle 'group' grouping if specified
  if (!is.null(group)) {
    # Validate group argument
    if (length(group) != length(x)) {
      stop("'group' must have the same length as 'x'")
    }
    
    unique_by <- unique(group)
    n_groups <- length(unique_by)
    if (n_groups < 2 || n_groups > 3) {
      stop("'group' must have 2 or 3 unique values")
    }
    
    # Use provided colors if valid, otherwise use default colors for groups
    if (length(col) == n_groups && is.character(col)) {
      group_cols <- col
    } else {
      group_cols <- sohn:::get.colors(n_groups)
    }
    
    # Compute frequencies using cross-tabulation
    all_xs <- sort(unique(x))
    freq_table <- table(x, group)
    
    # Get column names from table (these are the unique values of group in table order)
    table_by_cols <- colnames(freq_table)
    # Match table columns to unique_by order to ensure correct group assignment
    col_indices <- match(unique_by, table_by_cols)
    
    # Convert to list format for each group
    group_freqs <- list()
    group_freqs_original <- list()  # Store original frequencies for return value
    total <- length(x)  # Total sample size
    for (i in 1:n_groups) {
      # Extract frequencies for this group, ensuring all x values are included
      group_fs <- numeric(length(all_xs))
      # Match table row names (x values) to all_xs
      # Handle both numeric and character/factor x values
      if (is.numeric(x)) {
        table_xs <- as.numeric(rownames(freq_table))
      } else {
        table_xs <- rownames(freq_table)
      }
      idx <- match(table_xs, all_xs)
      # Use the correct column index that matches unique_by[i]
      group_fs[idx] <- freq_table[, col_indices[i]]
      
      # Store original frequencies for return value
      group_freqs_original[[i]] <- list(xs = all_xs, fs = group_fs)
      
      # Convert to percentages if requested (relative to this group's sample size)
      if (freq == FALSE) {
        group_total <- sum(group_fs)  # Sample size for this group
        group_fs <- (group_fs / group_total) * 100
      }
      
      group_freqs[[i]] <- list(xs = all_xs, fs = group_fs)
    }
    
    # Find overall max frequency for ylim (already in percentages if freq=FALSE)
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
      if (!"main" %in% names(dots)) dots$main <- paste0("Distribution of ", x_name, "")
      
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
      
      # Track if user provided xaxt - if not, we'll draw custom axis with all x values
      user_provided_xaxt <- "xaxt" %in% names(dots)
      if (!user_provided_xaxt) dots$xaxt <- "n"
      
      # Plot the frequencies (empty plot frame)
      plot_args <- c(list(x = all_xs, y = rep(0, length(all_xs))), dots)
      do.call(plot, plot_args)
      
      # Calculate total sample size and add it under the main title
      tot <- length(x)
      mtext(paste0("(N=", tot, ")"), side = 3, line = 0.35, font = 3)
      
      # Draw custom x-axis with all x values (if we suppressed default)
      if (!user_provided_xaxt) {
        axis(1, at = all_xs, las = 1)
      }
      
      # Draw custom y-axis with tickmarks at 0, midpoint, and maximum (if we suppressed default)
      if (!user_provided_yaxt) {
        # Use max_fs for tick calculation (actual data range, not expanded ylim)
        # max_fs is already in percentages if freq=FALSE
        y_max_plot <- max_fs
        if ("ylim" %in% names(dots)) {
          y_max_plot <- dots$ylim[2]
        }
        
        if (freq == FALSE) {
          # When showing percentages, use pretty() to generate reasonable tick marks
          # max_fs is already in percentages
          pct_range <- c(0, y_max_plot)
          pct_ticks <- pretty(pct_range, n = 5)
          pct_ticks <- pct_ticks[pct_ticks >= 0 & pct_ticks <= y_max_plot + 0.1]
          
          # Create labels with % sign
          y_labels <- paste0(pct_ticks, "%")
          
          # Ticks are positioned at percentage values (since group_freqs$fs is in percentages)
          axis(2, at = pct_ticks, labels = y_labels, las = 1)
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
    
    # Calculate bar width if not provided
    if (is.null(width)) {
      if (length(all_xs) > 1) {
        min_spacing <- min(diff(sort(all_xs)))
        width <- min_spacing * 0.2  # 20% of minimum spacing
      } else {
        width <- 0.15  # fallback
      }
    }
    
    # Calculate offsets for each group so bars touch exactly
    # For 2 groups: bars at -width/2 and +width/2 (touching at center)
    # For 3 groups: bars at -width, 0, +width (each bar is width wide, touching)
    if (n_groups == 2) {
      offsets <- c(-width/2, width/2)
    } else if (n_groups == 3) {
      offsets <- c(-width, 0, width)
    }
    
    # Draw polygons for each group (side by side, touching exactly)
    for (i in 1:n_groups) {
      gf <- group_freqs[[i]]
      non_zero <- gf$fs > 0
      if (any(non_zero)) {
        for (j in which(non_zero)) {
          x_val <- gf$xs[j]
          freq_val <- gf$fs[j]
          x_center <- x_val + offsets[i]
          x_left <- x_center - width/2
          x_right <- x_center + width/2
          
          polygon(x = c(x_left, x_right, x_right, x_left),
                  y = c(0, 0, freq_val, freq_val),
                  col = group_cols[i], border = group_cols[i])
        }
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
            # Format label based on freq parameter
            if (freq == FALSE) {
              label_text <- paste0(round(freq_val, 0), "%")
            } else {
              label_text <- freq_val
            }
            # Use col.text if provided, otherwise use group color
            label_color <- if (!is.null(col.text)) col.text else group_cols[i]
            text(x = x_label_pos, y = freq_val, labels = label_text, 
                 cex = 0.8, pos = 3, col = label_color)
          }
        }
      }
    }
    
    # Add legend showing groups and colors
    if (!add && show.legend) {
      # Calculate sample sizes for each group
      group_ns <- sapply(1:n_groups, function(i) {
        length(x[group == unique_by[i]])
      })
      
      # Create legend labels with sample sizes, aligned so N=xxx is at same position
      group_names <- as.character(unique_by)
      # Calculate text width needed for alignment
      # Measure the width of group names plus some padding
      temp_cex <- 1.2  # Match the legend cex
      text_widths <- strwidth(group_names, cex = temp_cex, units = "user")
      max_name_width <- max(text_widths)
      # Calculate width needed for the full label format: "name (N=xxx)"
      # We'll set text.width to accommodate the longest name plus "(N=xxx)" part
      sample_n_text <- paste0(" (N=", max(group_ns), ")")
      sample_n_width <- strwidth(sample_n_text, cex = temp_cex, units = "user")
      total_text_width <- max_name_width + sample_n_width
      
      # Format labels with padding to align N=xxx
      padded_labels <- sapply(1:n_groups, function(i) {
        name_width <- text_widths[i]
        padding_needed <- max_name_width - name_width
        # Estimate number of spaces needed (using space width)
        space_width <- strwidth(" ", cex = temp_cex, units = "user")
        n_spaces <- max(1, round(padding_needed / space_width))
        paste0(group_names[i], strrep(" ", n_spaces), " (N=", group_ns[i], ")")
      })
      legend_labels <- padded_labels
      legend_cols <- group_cols[1:n_groups]
      
      # Add legend with text.width to ensure consistent alignment
      legend_args <- list("topleft", legend = legend_labels, fill = legend_cols, 
                          bty = "n", inset = 0.05, cex=1.2, text.width = total_text_width)
      if (!is.null(legend.title)) {
        legend_args$title <- legend.title
        legend_args$title.font <- 2
      }
      do.call(legend, legend_args)
    }
    
    # Return frequencies or percentages invisibly (full table with separate columns for each group)
    # Build data frame with value column and one column per group
    result_df <- data.frame(value = all_xs, stringsAsFactors = FALSE)
    for (i in 1:n_groups) {
      # Use percentages if freq=FALSE, otherwise use frequencies
      if (freq == FALSE) {
        result_df[[as.character(unique_by[i])]] <- group_freqs[[i]]$fs
      } else {
        result_df[[as.character(unique_by[i])]] <- group_freqs_original[[i]]$fs
      }
    }
    return(invisible(result_df))
  }
  
  # Calculate frequencies for each unique value (only if group is not used)
  freq_table <- table(x)
  xs <- as.numeric(names(freq_table))
  fs <- as.numeric(freq_table)
  fs_original <- fs  # Store original frequencies for return value
  fsp=fs
  
  # Convert frequencies to percentages if requested
  total <- sum(fs)
  if (freq==FALSE)
  {
    fs <- (fs / total) * 100  # Convert to percentages
    fsp <- paste0(round(fs, 0),"%")
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
            
          # Track if user provided xaxt - if not, we'll draw custom axis with all x values
            user_provided_xaxt <- "xaxt" %in% names(dots)
            if (!user_provided_xaxt) dots$xaxt <- "n"
    #########################################################
    
              
                  
    # Plot the frequencies (empty plot frame)
        plot_args <- c(list(x = xs, y = fs), dots)
        do.call(plot, plot_args)
      
    # Calculate total sample size and add it under the main title
        tot <- total  # Use the original total (before percentage conversion)
        mtext(paste0("(N=", tot, ")"), side = 3, line = 0.35, font = 3)
        
    # Draw custom x-axis with all x values (if we suppressed default)
      if (!user_provided_xaxt) {
        axis(1, at = xs, las = 1)
      }
            
    # Draw custom y-axis with tickmarks at 0, midpoint, and maximum (if we suppressed default)
      if (!user_provided_yaxt) {
        # Get y-axis range (fs is now in percentages if freq=FALSE)
        y_max <- max(fs, na.rm = TRUE)
        if ("ylim" %in% names(dots)) {
          y_max_plot <- dots$ylim[2]
        } else {
          y_max_plot <- y_max
        }
        
        if (freq == FALSE) {
          # When showing percentages, use pretty() to generate reasonable tick marks
          # fs is already in percentages, so y_max_plot is also in percentages
          pct_range <- c(0, y_max_plot)
          pct_ticks <- pretty(pct_range, n = 5)
          # Only keep ticks that are >= 0 and <= y_max_plot (with small tolerance for rounding)
          pct_ticks <- pct_ticks[pct_ticks >= 0 & pct_ticks <= y_max_plot + 0.1]
          
          # Create labels with % sign
          y_labels <- paste0(pct_ticks, "%")
          
          # Ticks are positioned at percentage values (since fs is in percentages)
          axis(2, at = pct_ticks, labels = y_labels, las = 1)
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
    
  # Calculate bar width if not provided (for non-grouped case)
  if (is.null(width)) {
    if (length(xs) > 1) {
      min_spacing <- min(diff(sort(xs)))
      width <- min_spacing * 0.2  # 20% of minimum spacing
    } else {
      width <- 0.15  # fallback
    }
  }
  
  # Identify non-zero frequencies (only draw polygons and labels for these)
    non_zero <- fs > 0
  
  # Draw polygons for each value (only non-zero frequencies)
    if (any(non_zero)) {
      for (j in which(non_zero)) {
        x_val <- xs[j]
        freq_val <- fs[j]
        x_left <- x_val - width/2
        x_right <- x_val + width/2
        
        polygon(x = c(x_left, x_right, x_right, x_left),
                y = c(0, 0, freq_val, freq_val),
                col = col, border = col)
      }
    }
    
  # Add value labels if requested
    if (value.labels == TRUE && any(non_zero)) {
      # Use col.text if provided, otherwise use col
      label_color <- if (!is.null(col.text)) col.text else col
      text(x = xs[non_zero], y = fs[non_zero], labels = fsp[non_zero], 
           cex = 0.7, pos = 3, col = label_color)
    }
    
  # Return frequencies or percentages invisibly
  if (freq == FALSE) {
    # Return percentages
    invisible(data.frame(value = xs, percentage = fs))
  } else {
    # Return frequencies
    invisible(data.frame(value = xs, frequency = fs_original))
  }
}

# Alias for backward compatibility
#' @export
fhist <- function(...) {
  message2("The function fhist() is deprecated, please use plot_freq() instead going forward", col = "red", font = 2)
  plot_freq(...)
}

