#' Scatter Plot with GAM Smooth Line
#'
#' Creates a scatter plot with a GAM (Generalized Additive Model) smooth line,
#' with options to display data points and three-way spline summary points.
#'
#' @param x A numeric vector of x values.
#' @param y A numeric vector of y values.
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
#'   number of unique values), \code{"none"} (no distribution plot), \code{"fhist"}
#'   (always use \code{fhist()}), or \code{"hist"} (always use \code{hist()}).
#'   When \code{NULL}, uses \code{fhist()} if there are 25 or fewer unique values,
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
#' # Custom styling
#' scatter.gam(x, y, data.dots = TRUE, col = "red", lwd = 2, main = "GAM Fit")
#'
#' # With custom basis dimension
#' scatter.gam(x, y, k = 10)
#'
#' @importFrom mgcv gam
#' @export
scatter.gam <- function(x, y, data.dots = FALSE, three.dots = FALSE, data = NULL, k = NULL, plot.dist = NULL, 
                        dot.pch = 16, dot.col = adjustcolor('gray', 0.7), jitter = FALSE, ...) {
  # Capture x and y names for labels (before potentially overwriting)
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  # Extract additional arguments
  dots <- list(...)
  
  # Extract plot.dist from dots if it was passed via ... (for backward compatibility)
  if ("plot.dist" %in% names(dots)) {
    plot.dist <- dots$plot.dist
    dots$plot.dist <- NULL
  }
  
  # Handle data frame if provided
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame")
    }
    
    # Extract columns from data frame
    if (!x_name %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", x_name))
    }
    if (!y_name %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", y_name))
    }
    
    x <- data[[x_name]]
    y <- data[[y_name]]
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
  use_fhist <- FALSE
  use_hist <- FALSE
  
  if (plot_distribution) {
    if (!is.null(plot.dist) && plot.dist == "fhist") {
      use_fhist <- TRUE
    } else if (!is.null(plot.dist) && plot.dist == "hist") {
      use_hist <- TRUE
    } else {
      # Auto-select (when plot.dist is NULL): use fhist if 25 or fewer unique values
      n_unique <- length(unique(x))
      if (n_unique <= 25) {
        use_fhist <- TRUE
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
    par(mar = c(0, 4.1, 4.1, 2.1))  # Top plot: no bottom margin
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
  
  # Plot smooth line using grid predictions
  plot_args_line <- c(list(x = newdat$x, y = yh, type = 'l', col = 'blue', ylim = ylim), 
                      plot_args)
  do.call(plot, plot_args_line)
  
  # Add data points if requested
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
  
  # Plot distribution in bottom panel if requested
  if (plot_distribution) {
    # Switch to bottom panel
    par(mar = c(5.1, 4.1, 0, 2.1))  # Bottom plot: no top margin
    
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
    # fhist() and hist() have their own defaults, so we only override what's necessary
    dist_plot_args <- list(
      xlab = x_name,
      ylab = "Frequency",
      main = "",
      xlim = xlim_dist
    )
    
    # Optionally pass through some formatting arguments if they were specified
    # but let fhist/hist use their defaults otherwise
    if ("font.lab" %in% names(plot_args)) dist_plot_args$font.lab <- plot_args$font.lab
    if ("cex.lab" %in% names(plot_args)) dist_plot_args$cex.lab <- plot_args$cex.lab
    if ("las" %in% names(plot_args)) dist_plot_args$las <- plot_args$las
    
    if (use_fhist) {
      # Use fhist() for distribution plot of x
      do.call(fhist, c(list(x = x), dist_plot_args))
    } else if (use_hist) {
      # Use hist() for distribution plot of x
      do.call(hist, c(list(x = x), dist_plot_args))
    }
  }
  
  # Return GAM model invisibly
  invisible(g1)
}

