#' Scatter Plot with GAM Smooth Line
#'
#' Creates a scatter plot with a GAM (Generalized Additive Model) smooth line,
#' with options to display data points and three-way spline summary points.
#'
#' @param x A numeric vector of x values, or a formula of the form \code{y ~ x}.
#' @param y A numeric vector of y values. Not used if \code{x} is a formula.
#' @param data.dots Logical. If TRUE, displays the original data points on the
#'   plot. Default is FALSE.
#' @param three.dots Logical. If TRUE, displays three summary points representing
#'   the mean x and y values for each tertile of x. Default is FALSE.
#' @param data An optional data frame containing the variables \code{x} and \code{y}.
#' @param k Optional integer specifying the basis dimension for the smooth term
#'   in the GAM model (passed to \code{s(x, k=k)}). If NULL (default), uses the
#'   default basis dimension.
  #' @param plot.dist Character string specifying how to plot the distribution of \code{x}
  #'   underneath the scatter plot. Options: \code{NULL} (default, auto-select based on
  #'   number of unique values), \code{"none"} (no distribution plot), \code{"plot_freq"}
  #'   (always use \code{plot_freq()}), or \code{"hist"} (always use \code{hist()}).
  #'   When \code{NULL}, uses \code{plot_freq()} if there are 25 or fewer unique values,
  #'   otherwise uses \code{hist()}.
#' @param dot.pch Plotting character for data points when \code{data.dots = TRUE}.
#'   Default is 16 (filled circle).
#' @param dot.col Color for data points when \code{data.dots = TRUE}. Default is
#'   \code{adjustcolor('gray', 0.7)} (semi-transparent gray).
#' @param jitter Logical. If TRUE, applies a small amount of jitter to data points
#'   to reduce overplotting. Default is FALSE.
#' @param ... Additional arguments passed to \code{plot()} and \code{gam()}.
#'
#' @return Invisibly returns the fitted GAM model object.
#'
#' @details
#' This function fits a GAM model with a smooth term for x and plots the fitted
#' smooth line. The function uses the \code{mgcv} package's \code{gam()} function.
#'
#' When \code{three.dots = TRUE}, the x variable is divided into three equal-sized
#' groups (tertiles), and the mean x and y values for each group are plotted as
#' points. This provides a simple summary of the relationship across the range of x.
#'
#' @examples
#' # Basic usage
#' x <- rnorm(100)
#' y <- 2*x + rnorm(100)
#' scatter.gam(x, y)
#'
#' # Using formula syntax
#' scatter.gam(y ~ x)
#'
#' # With data points
#' scatter.gam(x, y, data.dots = TRUE)
#'
#' # With three-way spline points
#' scatter.gam(x, y, three.dots = TRUE)
#'
#' # Both options
#' scatter.gam(x, y, data.dots = TRUE, three.dots = TRUE)
#'
#' # Using data frame
#' df <- data.frame(x = rnorm(100), y = 2*rnorm(100) + rnorm(100))
#' scatter.gam(x, y, data = df, data.dots = TRUE)
#'
#' # Using formula syntax with data frame
#' scatter.gam(y ~ x, data = df, data.dots = TRUE)
#'
#' # Custom styling
#' scatter.gam(x, y, data.dots = TRUE, col = "red", lwd = 2, main = "GAM Fit")
#'
#' # With custom basis dimension
#' scatter.gam(x, y, k = 10)
#'
#' @importFrom mgcv gam
#' @export
scatter.gam <- function(x, y, data.dots = TRUE, three.dots = FALSE, data = NULL, k = NULL, plot.dist = NULL, 
                        dot.pch = 16, dot.col = adjustcolor('gray', 0.7), jitter = FALSE, ...) {
  # Check if x is a formula (y ~ x syntax)
  is_formula <- tryCatch(inherits(x, "formula"), error = function(e) FALSE)
  
  if (is_formula) {
    # Formula syntax: y ~ x
    formula_vars <- all.vars(x)
    if (length(formula_vars) != 2) {
      stop("scatter.gam(): Formula must have exactly two variables: y ~ x", call. = FALSE)
    }
    
    y_var_name <- formula_vars[1]
    x_var_name <- formula_vars[2]
    
    # Get environment for evaluating variables
    formula_env <- environment(x)
    if (is.null(formula_env)) {
      calling_env <- parent.frame()
    } else {
      calling_env <- formula_env
    }
    
    if (!is.null(data)) {
      # Data provided: extract from data frame
      if (!is.data.frame(data)) {
        stop("scatter.gam(): 'data' must be a data frame", call. = FALSE)
      }
      
      # Check if variables exist in data
      if (!y_var_name %in% names(data)) {
        stop(sprintf("scatter.gam(): Variable \"%s\" not found in dataset", y_var_name), call. = FALSE)
      }
      if (!x_var_name %in% names(data)) {
        stop(sprintf("scatter.gam(): Variable \"%s\" not found in dataset", x_var_name), call. = FALSE)
      }
      
      # Extract variables from data
      y <- data[[y_var_name]]
      x <- data[[x_var_name]]
      
      # Set names for labels
      y_name <- y_var_name
      x_name <- x_var_name
    } else {
      # No data: evaluate variables from environment
      y_exists <- exists(y_var_name, envir = calling_env, inherits = TRUE)
      x_exists <- exists(x_var_name, envir = calling_env, inherits = TRUE)
      
      if (!y_exists) {
        stop(sprintf("scatter.gam(): Could not find variable '%s'", y_var_name), call. = FALSE)
      }
      if (!x_exists) {
        stop(sprintf("scatter.gam(): Could not find variable '%s'", x_var_name), call. = FALSE)
      }
      
      y <- eval(as.name(y_var_name), envir = calling_env)
      x <- eval(as.name(x_var_name), envir = calling_env)
      
      # Set names for labels
      y_name <- y_var_name
      x_name <- x_var_name
    }
  } else {
    # Standard syntax: x, y
    # Capture x and y names for labels (before potentially overwriting)
    x_name_raw <- deparse(substitute(x))
    y_name_raw <- deparse(substitute(y))
    # Remove quotes if present
    x_name_raw <- gsub('^"|"$', '', x_name_raw)
    y_name_raw <- gsub('^"|"$', '', y_name_raw)
    # Clean variable names: remove df$ prefix if present
    x_name <- if (grepl("\\$", x_name_raw)) {
      strsplit(x_name_raw, "\\$")[[1]][length(strsplit(x_name_raw, "\\$")[[1]])]
    } else {
      x_name_raw
    }
    y_name <- if (grepl("\\$", y_name_raw)) {
      strsplit(y_name_raw, "\\$")[[1]][length(strsplit(y_name_raw, "\\$")[[1]])]
    } else {
      y_name_raw
    }
    
    # Handle data frame if provided
    if (!is.null(data)) {
      if (!is.data.frame(data)) {
        stop("scatter.gam(): 'data' must be a data frame", call. = FALSE)
      }
      
      # Extract columns from data frame
      # Use raw names for column lookup (they may include df$ prefix)
      if (!x_name_raw %in% names(data)) {
        stop(sprintf("scatter.gam(): Column '%s' not found in data", x_name_raw), call. = FALSE)
      }
      if (!y_name_raw %in% names(data)) {
        stop(sprintf("scatter.gam(): Column '%s' not found in data", y_name_raw), call. = FALSE)
      }
      
      x <- data[[x_name_raw]]
      y <- data[[y_name_raw]]
    }
  }
  
  # Extract additional arguments
  dots <- list(...)
  
  # Extract plot.dist from dots if it was passed via ... (for backward compatibility)
  if ("plot.dist" %in% names(dots)) {
    plot.dist <- dots$plot.dist
    dots$plot.dist <- NULL
  }
  
  # Check for required package
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required. Please install it with: install.packages('mgcv')")
  }
  
  # Fit GAM model
  # Define GAM argument names (to avoid duplication)
  gam_arg_names <- c("family", "method", "optimizer", "control", 
                     "scale", "select", "knots", "sp", "min.sp",
                     "H", "gamma", "fit", "paraPen", "G", "in.out",
                     "drop.unused.levels", "drop.intercept", "nthreads",
                     "cluster", "mustart", "etastart", "offset", "subset",
                     "na.action", "start", "model", "x", "y")
  
  # Separate gam arguments from plot arguments
  gam_args <- dots[names(dots) %in% gam_arg_names]
  plot_args <- dots[!names(dots) %in% gam_arg_names]
  
  # Build GAM formula with optional k parameter
  if (!is.null(k)) {
    gam_formula <- y ~ s(x, k = k)
  } else {
    gam_formula <- y ~ s(x)
  }
  g1 <- do.call(mgcv::gam, c(list(formula = gam_formula), gam_args))
  
  # Create grid for smooth line prediction
  # Use user-provided xlim if available, otherwise use data range
  if ("xlim" %in% names(plot_args)) {
    x_range <- plot_args$xlim
  } else {
    x_range <- range(x, na.rm = TRUE)
  }
  newdat <- data.frame(x = seq(x_range[1], x_range[2], length.out = 400))
  yh <- predict(g1, newdata = newdat)
  
  # Calculate three-way spline points if requested
  if (three.dots == TRUE) {
    xq <- cut(x, 3)
    # aggregate returns data frame with Group.1 (factor levels) and x (aggregated values)
    x3_agg <- aggregate(x, list(xq), mean, na.rm = TRUE)
    y3_agg <- aggregate(y, list(xq), mean, na.rm = TRUE)
    x3_means <- x3_agg$x  # Mean x values for each tertile
    y3_means <- y3_agg$x  # Mean y values for each tertile
  }
  
  # Determine ylim
  if (data.dots == TRUE && three.dots == TRUE) {
    ylim <- range(c(y, yh, y3_means), na.rm = TRUE)
  } else if (data.dots == TRUE) {
    ylim <- range(y, na.rm = TRUE)
  } else if (three.dots == TRUE) {
    ylim <- range(c(yh, y3_means), na.rm = TRUE)
  } else {
    ylim <- range(yh, na.rm = TRUE)
  }
  
  # Determine if we need to plot distribution and which method to use
  plot_distribution <- is.null(plot.dist) || (plot.dist != "none")
  use_plot_freq <- FALSE
  use_hist <- FALSE
  
  if (plot_distribution) {
    if (!is.null(plot.dist) && plot.dist == "plot_freq") {
      use_plot_freq <- TRUE
    } else if (!is.null(plot.dist) && plot.dist == "hist") {
      use_hist <- TRUE
    } else {
      # Auto-select (when plot.dist is NULL): use plot_freq if 25 or fewer unique values
      n_unique <- length(unique(x))
      if (n_unique <= 25) {
        use_plot_freq <- TRUE
      } else {
        use_hist <- TRUE
      }
    }
  }
  
  # Set up layout for two panels if distribution plotting is requested
  if (plot_distribution) {
    # Save current par settings
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    # Set up two panels: top for scatter plot, bottom for distribution
    # Use layout to control spacing (no gap between panels)
    layout(matrix(c(1, 2), nrow = 2, ncol = 1), heights = c(2, 1))
    
    # Set margins: remove bottom margin from top plot, remove top margin from bottom plot
    # Use same left and right margins for both plots to ensure x-axes align
    # Left margin: 5.1 to accommodate y-axis label on top plot
    # Right margin: 4.1 to accommodate y-axis labels on bottom plot (axis 4)
    par(mar = c(0, 5.1, 3, 4.1))  # Top plot: no bottom margin, 3 lines top margin, aligned margins
  } else {
    # Set top margin to 3 lines when not plotting distribution
    current_mar <- par("mar")
    par(mar = c(current_mar[1], current_mar[2], 3, current_mar[4]))
  }
  
  # Set default labels if not provided
  if (!"xlab" %in% names(plot_args)) plot_args$xlab <- x_name
  if (!"ylab" %in% names(plot_args)) plot_args$ylab <- y_name
  
  # Set axis label formatting and orientation
  if (!"font.lab" %in% names(plot_args)) plot_args$font.lab <- 2
  if (!"cex.lab" %in% names(plot_args)) plot_args$cex.lab <- 1.2
  if (!"las" %in% names(plot_args)) plot_args$las <- 1
  
  # Suppress x-axis on top plot if distribution is plotted below
  if (plot_distribution) {
    plot_args$xaxt <- "n"
  }
  
  # Create plot frame first (without drawing the line yet)
  # We'll draw the line after the data points so it appears on top
  plot_args_frame <- c(list(x = newdat$x, y = yh, type = 'n', ylim = ylim), 
                       plot_args)
  do.call(plot, plot_args_frame)
  
  # Add title on top
  mtext(side = 3, text = paste0("Scatter GAM - ", x_name, " & ", y_name), 
        line = 1, font = 2, cex = 1.2)
  
  # Add data points if requested (draw these first, before the line)
  if (data.dots == TRUE) {
    # Detect dense data and adjust visualization accordingly
    n_points <- length(x)
    n_unique_x <- length(unique(x))
    density_ratio <- n_points / n_unique_x
    
    # Auto-adjust transparency and jitter for dense data
    auto_jitter <- jitter
    auto_dot_col <- dot.col
    auto_cex <- 0.8  # Default point size
    
    # If data is dense (many points per unique x value), use more transparency and auto-jitter
    if (density_ratio > 10 && !jitter) {
      auto_jitter <- TRUE
      # Make points more transparent for dense data
      if (identical(dot.col, adjustcolor('gray', 0.7))) {
        # Only adjust if using default color
        auto_dot_col <- adjustcolor('gray', 0.3)  # Much more transparent
        auto_cex <- 0.5  # Smaller points for dense data
      }
    } else if (density_ratio > 5 && !jitter) {
      # Moderate density - still use jitter
      auto_jitter <- TRUE
      if (identical(dot.col, adjustcolor('gray', 0.7))) {
        auto_dot_col <- adjustcolor('gray', 0.4)
        auto_cex <- 0.6
      }
    }
    
    # Apply jitter if requested or auto-enabled
    if (auto_jitter) {
      # Calculate jitter amount based on data range
      x_range <- diff(range(x, na.rm = TRUE))
      y_range <- diff(range(y, na.rm = TRUE))
      x_jitter <- x_range * 0.01  # 1% of x range
      y_jitter <- y_range * 0.01  # 1% of y range
      x_plot <- x + runif(length(x), -x_jitter, x_jitter)
      y_plot <- y + runif(length(y), -y_jitter, y_jitter)
    } else {
      x_plot <- x
      y_plot <- y
    }
    points(x_plot, y_plot, pch = dot.pch, col = auto_dot_col, cex = auto_cex)
  }
  
  # Add three-way spline points if requested
  if (three.dots == TRUE) {
    points(x3_means, y3_means, pch = 16)
  }
  
  # Draw smooth line on top of data points
  lines(newdat$x, yh, col = 'blue', lwd = 3)
  
  # Plot distribution in bottom panel if requested
  if (plot_distribution) {
    # Switch to bottom panel
    # Use same left and right margins as top plot to ensure x-axes align
    par(mar = c(5.1, 5.1, 0, 4.1))  # Bottom plot: no top margin, aligned margins with top plot
    
    # Determine xlim for distribution plot
    # Use user-provided xlim if available, otherwise calculate from data with buffer
    if ("xlim" %in% names(plot_args)) {
      # Use the same xlim as the scatter plot (user-specified)
      xlim_dist <- plot_args$xlim
    } else {
      # Calculate x-axis range with buffer (10% on each side for better visibility)
      x_range <- range(x, na.rm = TRUE)
      x_buffer <- diff(x_range) * 0.10
      xlim_dist <- c(x_range[1] - x_buffer, x_range[2] + x_buffer)
    }
    
    # Prepare distribution plot arguments - only pass essential arguments
    # plot_freq() and hist() have their own defaults, so we only override what's necessary
    dist_plot_args <- list(
      xlab = x_name,
      ylab = "",  # Will use mtext instead
      main = "",
      xlim = xlim_dist
    )
    
    # Optionally pass through some formatting arguments if they were specified
      # but let plot_freq/hist use their defaults otherwise
    if ("font.lab" %in% names(plot_args)) dist_plot_args$font.lab <- plot_args$font.lab
    if ("cex.lab" %in% names(plot_args)) dist_plot_args$cex.lab <- plot_args$cex.lab
    if ("las" %in% names(plot_args)) dist_plot_args$las <- plot_args$las
    
    if (use_plot_freq) {
      # Calculate ylim from frequencies for background plot
      freq_table <- table(x)
      max_freq <- max(freq_table, na.rm = TRUE)
      ylim_freq <- c(0, max_freq)
      # Add extra space at top if value labels might be shown (plot_freq default behavior)
      if (max_freq > 0) {
        ylim_freq[2] <- max_freq + max(1, max_freq * 0.15)  # Add 15% or at least 1 unit
      }
      
      # Get cex.lab for consistency
      cex_lab <- if ("cex.lab" %in% names(plot_args)) plot_args$cex.lab else 1.2
      
      # Initialize bottom plot with gray80 background
      init_bottom_plot(xlim = xlim_dist,
                       ylim = ylim_freq,
                       xlab = x_name,
                       ylab = "",  # Will use mtext instead
                       bg = "gray95",
                       cex.lab = cex_lab)
      
      # Use plot_freq() with add=TRUE to overlay on the background
      # Pass x as first positional argument (formula parameter) without naming it
      plot_freq_args <- list(x,
                             xlab = x_name,
                             main = "",
                             xlim = xlim_dist,
                             add = TRUE)
      if ("col" %in% names(dist_plot_args)) plot_freq_args$col <- dist_plot_args$col
      do.call(plot_freq, plot_freq_args)
      
      # Draw axes after plot_freq (since add=TRUE doesn't draw axes)
      axis(1)  # x-axis
      axis(2, las = 1, col = "gray40", col.axis = "gray40")  # y-axis on left side with gray40 color
      
      # Add Frequency label on left side next to the plot with gray40 color
      # Use line = 3 to match the default ylab position from plot() (mgp[1] = 3)
      mtext(side = 2, text = "Frequency", line = 3, font = 2, cex = cex_lab, col = "gray40")
    } else if (use_hist) {
      # Use hist() for distribution plot of x
      # Suppress default y-axis to draw custom one
      hist_args <- c(list(x = x, yaxt = "n"), dist_plot_args)
      do.call(hist, hist_args)
      axis(2, las = 1, col = "gray40", col.axis = "gray40")  # y-axis on left side with gray40 color
      
      # Add Frequency label on left side next to the plot with gray40 color
      # Use line = 3 to match the default ylab position from plot() (mgp[1] = 3)
      cex_lab <- if ("cex.lab" %in% names(plot_args)) plot_args$cex.lab else 1.2
      mtext(side = 2, text = "Frequency", line = 3, font = 2, cex = cex_lab, col = "gray40")
    }
  }
  
  # Return GAM model invisibly
  invisible(g1)
}

