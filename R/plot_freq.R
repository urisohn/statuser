#' Plot frequencies of a variable, optionally by group (histogram without binning)
#'
#' Creates a frequency plot showing the frequency of every observed value,
#' displaying the full range from minimum to maximum value.
#'
#' @param formula A formula of the form \code{x ~ group} where \code{x} is the
#'   variable to plot frequencies for and \code{group} is an optional grouping variable
#'   (with 2 or 3 unique values). For single variable (no grouping), use \code{x ~ 1}.
#'   Alternatively, pass a single vector \code{x} for a simple frequency plot.
#' @param y An optional second vector to compare with \code{formula}. When provided,
#'   creates a comparison plot of two variables (like grouped plot but with separate variables).
#'   This allows syntax like \code{plot_freq(y1, y2)} to compare two vectors.
#' @param data An optional data frame containing the variables in the formula.
#'   If \code{data} is not provided, variables are evaluated from the calling environment.
#' @param freq Logical. If TRUE (default), displays frequencies. If FALSE, displays percentages.
#' @param order Controls the order in which groups appear in the plot and legend. 
#'   Use \code{-1} to reverse the default order. Alternatively, provide a vector specifying
#'   the exact order (e.g., \code{c("B", "A", "C")}). If \code{NULL} (default), groups are 
#'   ordered by their factor levels (if the grouping variable is a factor) or sorted 
#'   alphabetically/numerically. Only applies when using grouped plots or comparing two variables.
#' @param col Color for the bars. 
#' @param lwd Line width for the frequency bars. Default is 9.
#' @param width Numeric. Width of the frequency bars. If NULL (default), width is automatically calculated based on the spacing between values.
#' @param value.labels Logical. If TRUE, displays frequencies on top of each line. 
#' @param add Logical. If TRUE, adds to an existing plot instead of creating a new one. 
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
#' # Compare two vectors
#' y1 <- c(1, 1, 2, 2, 2, 5, 5)
#' y2 <- c(1, 2, 2, 3, 3, 3)
#' plot_freq(y1, y2)
#'
#' # Using a data frame with grouping
#' df <- data.frame(value = c(1, 1, 2, 2, 2, 5, 5), group = c("A", "A", "A", "B", "B", "A", "B"))
#' plot_freq(value ~ 1, data = df)  # single variable
#' plot_freq(value ~ group, data = df)  # with grouping
#'
#' # Control group order in legend and plot
#' plot_freq(value ~ group, data = df, order = c("B", "A"))  # B first, then A
#' plot_freq(value ~ group, data = df, order = -1)  # Reverse default order
#'

#' @export
plot_freq <- function(formula, y=NULL, data=NULL, freq=TRUE, order=NULL, col='dodgerblue', lwd=9, width=NULL, value.labels=TRUE, add=FALSE, show.legend=TRUE, legend.title=NULL, col.text=NULL, ...) {
  # Extract additional arguments
  dots <- list(...)
  
  # Check if we're in two-vector comparison mode (formula is vector, y is vector)
  if (!is.null(y) && !inherits(formula, "formula")) {
    # Two-vector comparison mode: plot_freq(y1, y2)
    # Capture variable names
    mc <- match.call()
    y1_name <- deparse(mc$formula)
    y1_name <- paste(y1_name, collapse = "")
    y1_name <- gsub('^"|"$', '', y1_name)
    
    y2_name <- deparse(mc$y)
    y2_name <- paste(y2_name, collapse = "")
    y2_name <- gsub('^"|"$', '', y2_name)
    
    # Validate inputs
    if (!is.numeric(formula) || !is.vector(formula)) {
      stop(sprintf("plot_freq(): First argument '%s' must be a numeric vector", y1_name), call. = FALSE)
    }
    if (!is.numeric(y) || !is.vector(y)) {
      stop(sprintf("plot_freq(): Second argument '%s' must be a numeric vector", y2_name), call. = FALSE)
    }
    
    # Create a data frame and recursively call with grouped syntax
    df <- data.frame(
      value = c(formula, y),
      group = c(rep(y1_name, length(formula)), rep(y2_name, length(y))),
      stringsAsFactors = FALSE
    )
    
    # Forward all arguments to the grouped version
    return(plot_freq(value ~ group, data = df, freq = freq, order = order, 
                     col = col, lwd = lwd, width = width, 
                     value.labels = value.labels, add = add, 
                     show.legend = show.legend, legend.title = legend.title, 
                     col.text = col.text, ...))
  }
  
  # Standard mode: formula syntax
  # Validate formula early if it is one
  validate_formula(formula, data, func_name = "plot_freq", calling_env = parent.frame())
  
  # Check if formula is actually a formula or a vector
  # If it's not a formula, capture the variable name before calling validate_plot
  is_formula_input <- tryCatch(inherits(formula, "formula"), error = function(e) FALSE)
  
  # Capture data name for error messages
  mc <- match.call()
  data_expr <- mc$data
  data_name <- if (!is.null(data_expr)) {
    data_name_val <- deparse(data_expr)
    gsub('^"|"$', '', data_name_val)
  } else {
    NULL
  }
  
  # Validate inputs using validation function shared with plot_density, plot_cdf, plot_freq
  # If not a formula, we need to pass it in a way that preserves the variable name
  if (is_formula_input) {
    validated <- validate_plot(formula, NULL, data, func_name = "plot_freq", require_group = FALSE, data_name = data_name)
  } else {
    # Not a formula - capture the actual variable name first
    formula_expr <- mc$formula
    actual_name <- if (!is.null(formula_expr)) {
      deparse(formula_expr)
    } else {
      deparse(substitute(formula))
    }
    # Collapse to single string if deparse returned multiple strings
    actual_name <- paste(actual_name, collapse = "")
    # Ensure it's a single character string
    actual_name <- as.character(actual_name)[1]
    # Remove quotes if present
    actual_name <- gsub('^"|"$', '', actual_name)
    
    # If data is provided, extract the variable from data frame first
    # This prevents validate_plot from looking for "formula" in the data frame
    if (!is.null(data)) {
      if (!is.data.frame(data)) {
        stop("plot_freq(): 'data' must be a data frame", call. = FALSE)
      }
      # Clean the variable name (remove $ prefix if present)
      clean_name <- if (grepl("\\$", actual_name)) {
        strsplit(actual_name, "\\$")[[1]][length(strsplit(actual_name, "\\$")[[1]])]
      } else {
        actual_name
      }
      # Check if variable exists in data
      if (!clean_name %in% names(data)) {
        stop(sprintf("plot_freq(): Column \"%s\" not found in dataset \"%s\"", clean_name, data_name), call. = FALSE)
      }
      # Extract variable from data frame
      formula <- data[[clean_name]]
      # Now call validate_plot without data (since we've already extracted the variable)
      validated <- validate_plot(formula, NULL, NULL, func_name = "plot_freq", require_group = FALSE, data_name = data_name)
      # Override the names with the actual variable name
      validated$y_name_raw <- clean_name
      validated$y_name <- clean_name
    } else {
      # No data - pass it to validation (will evaluate from environment)
      validated <- validate_plot(formula, NULL, data, func_name = "plot_freq", require_group = FALSE, data_name = data_name)
      # Override the name if it got "formula" instead of the actual variable name
      # Only override if actual_name looks like a valid variable name (not a deparsed vector)
      if (validated$y_name_raw == "formula" && 
          nchar(actual_name) < 100 && 
          !grepl("^c\\(|^structure\\(|^list\\(", actual_name)) {
        validated$y_name_raw <- actual_name
        validated$y_name <- if (grepl("\\$", actual_name)) {
          strsplit(actual_name, "\\$")[[1]][length(strsplit(actual_name, "\\$")[[1]])]
        } else {
          actual_name
        }
      }
    }
  }
  x <- validated$y  # Note: validate_plot uses 'y' but we use 'x'
  group <- validated$group
  x_name <- validated$y_name
  group_name <- validated$group_name
  x_name_raw <- validated$y_name_raw
  group_name_raw <- validated$group_name_raw
  
  # Store original group for factor level checking (before NA removal)
  group_original <- group
  
  # Drop missing data
  if (!is.null(group)) {
    isnagroup=is.na(group)
    isnax=is.na(x)
    group=group[!isnagroup & !isnax]
    x=x[!isnagroup & !isnax]
    
    n.nagroup = sum(isnagroup)
    n.nax = sum(isnax)
    
    if (n.nagroup>0) message2("plot_freq() says: dropped ",n.nagroup," observations with missing '",group_name_raw,"' values",col='red4')
    if (n.nax>0) message2("plot_freq() says: dropped ",n.nax," observations with missing '",x_name_raw,"' values",col='red4')
  } else {
    # No group variable - just drop missing x values
    isnax=is.na(x)
    x=x[!isnax]
    
    n.nax = sum(isnax)
    if (n.nax>0) message2("plot_freq() says: dropped ",n.nax," observations with missing '",x_name_raw,"' values",col='red4')
  }
  
  # Handle 'group' grouping if specified
  if (!is.null(group)) {
    # Validate group argument
    if (length(group) != length(x)) {
      stop("'group' must have the same length as 'x'")
    }
    
    # Determine group ordering
    unique_groups <- unique(group)
    n_groups <- length(unique_groups)
    if (n_groups < 2 || n_groups > 3) {
      stop("'group' must have 2 or 3 unique values")
    }
    
    # Check if order = -1 (reverse default order)
    reverse_order <- FALSE
    if (!is.null(order) && length(order) == 1 && is.numeric(order) && order == -1) {
      reverse_order <- TRUE
      order <- NULL  # Process as default, then reverse
    }
    
    if (!is.null(order)) {
      # User specified custom order
      # Validate that order contains all groups
      missing_groups <- setdiff(unique_groups, order)
      extra_groups <- setdiff(order, unique_groups)
      
      if (length(missing_groups) > 0) {
        stop(sprintf("plot_freq(): 'order' is missing group(s): %s", 
                     paste(missing_groups, collapse = ", ")), call. = FALSE)
      }
      if (length(extra_groups) > 0) {
        warning(sprintf("plot_freq(): 'order' contains group(s) not in data: %s", 
                       paste(extra_groups, collapse = ", ")))
      }
      
      # Use the specified order (only groups that exist in data)
      unique_by <- order[order %in% unique_groups]
    } else if (is.factor(group_original)) {
      # Respect factor levels
      factor_levels <- levels(group_original)
      # Only include levels that actually appear in the data
      unique_by <- factor_levels[factor_levels %in% unique_groups]
    } else {
      # Default: sort alphabetically/numerically
      unique_by <- sort(unique_groups)
    }
    
    # Reverse order if order = -1 was specified
    if (reverse_order) {
      unique_by <- rev(unique_by)
    }
    
    # Use provided colors if valid, otherwise use default colors for groups
    if (length(col) == n_groups && is.character(col)) {
      group_cols <- col
    } else {
      group_cols <- get.colors(n_groups)
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
    for (i in seq_len(n_groups)) {
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
      if (!"main" %in% names(dots)) {
        dots$main <- paste0("Distribution of ", x_name, "")
      }
      
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
      
      # Set main title formatting (15% bigger than default)
      if (!"cex.main" %in% names(dots)) dots$cex.main <- 1.38
      
      # Set type='n' to create plot frame without drawing points (unless user specifies type)
      if (!"type" %in% names(dots)) dots$type <- "n"
      
      # Track if user provided yaxt - if not, we'll draw custom axis
      user_provided_yaxt <- "yaxt" %in% names(dots)
      if (!user_provided_yaxt) dots$yaxt <- "n"
      
      # Track if user provided xaxt - if not, we'll draw custom axis with all x values
      user_provided_xaxt <- "xaxt" %in% names(dots)
      if (!user_provided_xaxt) dots$xaxt <- "n"
      
      # Ensure adequate top margin for main title and (N=...) text
      old_mar <- par("mar")
      on.exit(par(mar = old_mar), add = TRUE)
      if (!"mar" %in% names(dots)) {
        # Increase top margin if it's too small (less than 5 lines to accommodate title and N)
        if (old_mar[3] < 5) {
          par(mar = c(old_mar[1], old_mar[2], 5, old_mar[4]))
        }
      }
      
      # Plot the frequencies (empty plot frame)
      plot_args <- c(list(x = all_xs, y = rep(0, length(all_xs))), dots)
      do.call(plot, plot_args)
      
      # Calculate total sample size and add it below the main title
      # Main title is typically at line 3, so position (N=...) at line 0.75 (between positions used for grouped and non-grouped plots)
      tot <- length(x)
      mtext(paste0("(N=", tot, ")"), side = 3, line = 0.75, font = 3, cex = 0.9)
      
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
    for (i in seq_len(n_groups)) {
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
      for (j in seq_along(all_xs)) {
        x_val <- all_xs[j]
        for (i in seq_len(n_groups)) {
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
      group_ns <- sapply(seq_len(n_groups), function(i) {
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
      padded_labels <- sapply(seq_len(n_groups), function(i) {
        name_width <- text_widths[i]
        padding_needed <- max_name_width - name_width
        # Estimate number of spaces needed (using space width)
        space_width <- strwidth(" ", cex = temp_cex, units = "user")
        n_spaces <- max(1, round(padding_needed / space_width))
        paste0(group_names[i], strrep(" ", n_spaces), " (N=", group_ns[i], ")")
      })
      legend_labels <- padded_labels
      legend_cols <- group_cols[seq_len(n_groups)]
      
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
    for (i in seq_len(n_groups)) {
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
          
          # Set main title formatting (15% bigger than default)
          if (!"cex.main" %in% names(dots)) dots$cex.main <- 1.38
          
          # Set type='n' to create plot frame without drawing points (unless user specifies type)
            if (!"type" %in% names(dots)) dots$type <- "n"
          
          # Track if user provided yaxt - if not, we'll draw custom axis
            user_provided_yaxt <- "yaxt" %in% names(dots)
            if (!user_provided_yaxt) dots$yaxt <- "n"
            
          # Track if user provided xaxt - if not, we'll draw custom axis with all x values
            user_provided_xaxt <- "xaxt" %in% names(dots)
            if (!user_provided_xaxt) dots$xaxt <- "n"
    #########################################################
    
      # Ensure adequate top margin for main title and (N=...) text
      old_mar <- par("mar")
      on.exit(par(mar = old_mar), add = TRUE)
      if (!"mar" %in% names(dots)) {
        # Increase top margin if it's too small (less than 5 lines to accommodate title and N)
        if (old_mar[3] < 5) {
          par(mar = c(old_mar[1], old_mar[2], 5, old_mar[4]))
        }
      }
              
                  
    # Plot the frequencies (empty plot frame)
        plot_args <- c(list(x = xs, y = fs), dots)
        do.call(plot, plot_args)
      
    # Calculate total sample size and add it below the main title
    # Main title is typically at line 3, so position (N=...) at line 0.75 (between positions used for grouped and non-grouped plots)
        tot <- total  # Use the original total (before percentage conversion)
        mtext(paste0("(N=", tot, ")"), side = 3, line = 0.75, font = 3, cex = 0.9)
        
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

