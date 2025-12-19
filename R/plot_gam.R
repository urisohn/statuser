#' Plot GAM Model 
#'
#' Plots fitted GAM values for focal predictor,
#' keeping any other predictors in thhe model at a specified quantile  (default: median)
#'
#' @param model A GAM model object fitted using \code{mgcv::gam()}.
#' @param predictor Character string specifying the name of the predictor variable
#'   to plot on the x-axis.
#' @param quantile.others Numeric value between 1 and 99 specifying the quantile
#'   at which to hold all other variables constant. Default is 50 (median).
#' @param col Color for the prediction line. Default is "blue4".
#' @param bg Background color for the confidence band. Default is
#'   \code{adjustcolor('dodgerblue', .2)}.
#' @param plot2 Character string specifying how to plot the distribution of the
#'   predictor variable below the main plot. Options: \code{'auto'} (default,
#'   auto-select based on number of unique values), \code{'freq'} (always use
#'   \code{plot_freq()}), \code{'density'} (always use \code{plot_density()}),
#'   or \code{'none'} (no distribution plot). When \code{'auto'}, uses
#'   \code{plot_freq()} if there are 30 or fewer unique values, otherwise uses
#'   \code{plot_density()}.
#' @param ... Additional arguments passed to \code{plot()} and \code{lines()}.
#'
#' @return Invisibly returns a list containing:
#' \itemize{
#'   \item \code{predictor_values}: The sequence of predictor values used
#'   \item \code{predicted}: The predicted values
#'   \item \code{se}: The standard errors
#'   \item \code{lower}: Lower confidence bound (predicted - 2*se)
#'   \item \code{upper}: Upper confidence bound (predicted + 2*se)
#' }
#'
#' @details
#' This function:
#' \itemize{
#'   \item Validates that the model is from \code{mgcv::gam()}
#'   \item Extracts all variables from the GAM model
#'   \item Sets all variables except \code{predictor} to their specified quantile
#'     (default: median)
#'   \item Generates 100 equally spaced values for \code{predictor} between its
#'     minimum and maximum values in the model data
#'   \item Computes predicted values and standard errors using \code{predict()}
#'   \item Plots the predicted values with confidence bands (2 standard errors)
#' }
#'
#' @examples
#' \dontrun{
#' library(mgcv)
#' # Fit a GAM model
#' data(mtcars)
#' model <- gam(mpg ~ s(hp) + s(wt) + factor(cyl), data = mtcars)
#'
#' # Plot effect of hp (with other variables at median)
#' plot_gam(model, "hp")
#'
#' # Plot effect of hp (with other variables at 25th percentile)
#' plot_gam(model, "hp", quantile.others = 25)
#'
#' # Customize plot
#' plot_gam(model, "hp", main = "Effect of Horsepower", col = "blue", lwd = 2)
#' }
#'
#' @importFrom mgcv gam
#' @export
plot_gam <- function(model, predictor, quantile.others = 50, 
                     col = "blue4", bg = adjustcolor('dodgerblue', .2), 
                     plot2 = 'auto', ...) {
  # Check if model is from mgcv::gam()
  if (!inherits(model, "gam")) {
    stop("'model' must be a GAM model object fitted using mgcv::gam()")
  }
  
  # Check if mgcv is available
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required. Please install it with: install.packages('mgcv')")
  }
  
  # Check if factor() is used in the model formula
  # This causes issues with predict() - variables should be converted to factor before gam()
  model_formula <- formula(model)
  formula_char <- paste(deparse(model_formula), collapse = " ")
  # Check for factor( in the formula (factor is lowercase in R)
  if (grepl("\\bfactor\\s*\\(", formula_char, ignore.case = FALSE)) {
    stop("A variable in the GAM formula was included with 'factor()'.\n ",
         " Please, instead, convert that variable to factor  before running the GAM model\n",
         "\n  Example: Instead of gam(y ~ factor(x), data = df), do:\n",
         "    df$x <- factor(df$x)\n",
         "    gam(y ~ x, data = df)")
  }
  
  # Validate quantile.others
  if (!is.numeric(quantile.others) || length(quantile.others) != 1) {
    stop("'quantile.others' must be a single numeric value")
  }
  if (quantile.others < 1 || quantile.others > 99) {
    stop("'quantile.others' must be between 1 and 99")
  }
  
  # Validate predictor
  if (!is.character(predictor) || length(predictor) != 1) {
    stop("'predictor' must be a single character string")
  }
  
  # Validate plot2
  if (!is.character(plot2) || length(plot2) != 1) {
    stop("'plot2' must be a single character string")
  }
  if (!plot2 %in% c('auto', 'freq', 'density', 'none')) {
    stop("'plot2' must be one of: 'auto', 'freq', 'density', 'none'")
  }
  
  # Extract model data
  # Try model$model first, then model.frame() as fallback
  model_data <- model$model
  if (is.null(model_data)) {
    # Try to get model frame from the model
    tryCatch({
      model_data <- model.frame(model)
    }, error = function(e) {
      stop("Model data not found. Please refit the model with 'keepData = TRUE' or ensure model data is available.")
    })
  }
  
  # Get the original data if available (from model call)
  # This helps ensure we have the right variable structure
  original_data <- NULL
  if (!is.null(model$call$data)) {
    tryCatch({
      original_data <- eval(model$call$data, envir = environment(formula(model)))
    }, error = function(e) {
      # If we can't get original data, use model_data
      original_data <- model_data
    })
  } else {
    original_data <- model_data
  }
  
  # Get all variable names from the model frame
  all_vars <- names(model_data)
  
  # Remove response variable (first column is typically the response)
  response_var <- all_vars[1]
  predictor_vars <- all_vars[-1]
  
  # Check if predictor exists in model data
  if (!predictor %in% predictor_vars) {
    stop(sprintf("Predictor '%s' not found in model variables. Available variables: %s",
                 predictor, paste(predictor_vars, collapse = ", ")))
  }
  
  # Get other variables (all predictors except the one we're plotting)
  other_vars <- setdiff(predictor_vars, predictor)
  
  # Create new dataset for prediction
  # Start with the predictor variable: 100 equally spaced values between min and max
  predictor_values <- model_data[[predictor]]
  predictor_min <- min(predictor_values, na.rm = TRUE)
  predictor_max <- max(predictor_values, na.rm = TRUE)
  predictor_seq <- seq(predictor_min, predictor_max, length.out = 100)
  n_rows <- length(predictor_seq)
  
  # CRITICAL: Replicate the entire model frame structure first
  # This preserves ALL attributes, factor structures, and internal mgcv requirements
  new_data <- model_data[rep(1, n_rows), , drop = FALSE]
  
  # Now modify the predictor variable
  new_data[[predictor]] <- predictor_seq
  
  # Set other variables to their quantile/default values
  for (var in other_vars) {
    # Get variable from model_data (this is what mgcv sees)
    var_values <- model_data[[var]]
    
    # Handle different variable types
    if (is.factor(var_values)) {
      # For factors, use the first/lowest factor level
      first_level <- levels(var_values)[1]
      # Find a row with this level to preserve exact factor structure
      idx <- which(var_values == first_level)[1]
      if (length(idx) == 0) idx <- 1
      # Replicate the exact factor value from model_data
      new_data[[var]] <- var_values[rep(idx, n_rows)]
    } else if (is.character(var_values)) {
      # For character vectors, use the most common value
      var_levels <- table(var_values)
      most_common <- names(var_levels)[which.max(var_levels)]
      new_data[[var]] <- rep(most_common, n_rows)
    } else if (is.numeric(var_values)) {
      # For numeric variables, use the specified quantile
      quantile_value <- quantile(var_values, probs = quantile.others / 100, na.rm = TRUE)
      new_data[[var]] <- rep(as.numeric(quantile_value), n_rows)
    } else {
      # For other types, try to use the most common value
      warning(sprintf("Variable '%s' has unsupported type. Using most common value.", var))
      var_levels <- table(var_values)
      most_common <- names(var_levels)[which.max(var_levels)]
      new_data[[var]] <- rep(most_common, n_rows)
    }
  }
  
  # Extract only predictor variables (exclude response) for prediction
  new_data <- new_data[, predictor_vars, drop = FALSE]
  
  # Make predictions with standard errors
  pred_result <- predict(model, newdata = new_data, se.fit = TRUE)
  predicted <- pred_result$fit
  se <- pred_result$se.fit
  
  # Calculate confidence bounds (2 standard errors)
  lower <- predicted - 2 * se
  upper <- predicted + 2 * se
  
  # Extract additional arguments for plotting
  dots <- list(...)
  
  # Remove col and bg from dots if present (use formal arguments instead)
  if ("col" %in% names(dots)) {
    col <- dots$col
    dots$col <- NULL
  }
  if ("bg" %in% names(dots)) {
    bg <- dots$bg
    dots$bg <- NULL
  }
  
  # Set default labels if not provided
  if (!"xlab" %in% names(dots)) dots$xlab <- predictor
  if (!"ylab" %in% names(dots)) dots$ylab <- response_var
  
  # Set default main title if not provided
  main_title_text <- if ("main" %in% names(dots)) dots$main else paste0("GAM Predicting '", response_var, "' with '", predictor,"'")
  
  # Extract model formula for subtitle (will be added after plot)
  model_formula_text <- paste(deparse(formula(model)), collapse = " ")
  
  # Set main='' so we can use mtext() for both titles
  dots$main <- ""
  
  # Set default formatting
  if (!"font.lab" %in% names(dots)) dots$font.lab <- 2
  if (!"cex.lab" %in% names(dots)) dots$cex.lab <- 1.2
  if (!"las" %in% names(dots)) dots$las <- 1
  
  # Set default main title formatting if not provided
  if (!"font.main" %in% names(dots)) dots$font.main <- 2  # bold
  if (!"cex.main" %in% names(dots)) dots$cex.main <- 1.2
  
  # Determine ylim if not provided
  if (!"ylim" %in% names(dots)) {
    dots$ylim <- range(c(lower, upper), na.rm = TRUE)
  }
  
  # Set default xlim if not provided
  if (!"xlim" %in% names(dots)) {
    dots$xlim <- range(predictor_seq, na.rm = TRUE)
  }
  
  # Determine if we need to plot distribution and which method to use
  plot_distribution <- plot2 != 'none'
  use_plot_freq <- FALSE
  use_plot_density <- FALSE
  
  if (plot_distribution) {
    # Get predictor values from model data to count unique values
    predictor_data <- model_data[[predictor]]
    n_unique <- length(unique(predictor_data))
    
    if (plot2 == 'freq') {
      use_plot_freq <- TRUE
    } else if (plot2 == 'density') {
      use_plot_density <- TRUE
    } else if (plot2 == 'auto') {
      # Auto-select: use plot_freq if <= 30 unique values, else plot_density
      if (n_unique <= 30) {
        use_plot_freq <- TRUE
      } else {
        use_plot_density <- TRUE
      }
    }
  }
  
  # Set up layout for two panels if distribution plotting is requested
  if (plot_distribution) {
    # Save current par settings
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    # Set up two panels: top for GAM plot (75%), bottom for distribution (25%)
    # Use layout to control spacing (no gap between panels)
    layout(matrix(c(1, 2), nrow = 2, ncol = 1), heights = c(0.75, 0.25))
    
    # Set margins: remove bottom margin from top plot, remove top margin from bottom plot
    # Top margin needs to accommodate main title and subtitle
    par(mar = c(0, 4.1, 5.1, 2.1))  # Top plot: no bottom margin, extra top margin for title
  } else {
    # When no bottom plot, ensure we have enough top margin for title and subtitle
    if (par("mar")[3] < 5) {
      current_mar <- par("mar")
      par(mar = c(current_mar[1], current_mar[2], 5.1, current_mar[4]))
    }
  }
  
  # Suppress x-axis on top plot if distribution is plotted below
  if (plot_distribution) {
    dots$xaxt <- "n"
  }
  
  # Plot predicted values
  plot_args <- c(list(x = predictor_seq, y = predicted, type = "l"), dots)
  do.call(plot, plot_args)
  
  # Add main title and subtitle using mtext()
  # Get main title cex to calculate subtitle size
  main_cex <- if ("cex.main" %in% names(dots)) dots$cex.main else 1.2
  main_font <- if ("font.main" %in% names(dots)) dots$font.main else 2
  subtitle_cex <- main_cex * 0.9  # 10% smaller (90% of main size)
  
  # Add main title at line=2
  mtext(main_title_text, 
        side = 3, 
        line = 2, 
        cex = main_cex, 
        font = main_font)
  
  # Add subtitle with model formula at line=1 (below main title)
  mtext(model_formula_text, 
        side = 3, 
        line = 1, 
        cex = subtitle_cex, 
        font = 3,  # italic
        col = "gray70")
  
  # Add confidence bands
  polygon(c(predictor_seq, rev(predictor_seq)), 
          c(lower, rev(upper)),
          col = bg,
          border = NA)
  
  # Redraw the prediction line on top
  lines(predictor_seq, predicted, col = col, ...)
  
  # Plot distribution in bottom panel if requested
  if (plot_distribution) {
    # Switch to bottom panel
    par(mar = c(5.1, 4.1, 0, 2.1))  # Bottom plot: no top margin
    
    # Get predictor values from model data
    predictor_data <- model_data[[predictor]]
    
    # Determine xlim for distribution plot (use same as main plot)
    if ("xlim" %in% names(dots)) {
      xlim_dist <- dots$xlim
    } else {
      xlim_dist <- range(predictor_seq, na.rm = TRUE)
    }
    
    if (use_plot_freq) {
      # For plot_freq, determine ylim from frequencies
      freq_table <- table(predictor_data)
      max_freq <- max(freq_table)
      ylim_dist <- c(0, max_freq * 1.20)  # Add 20% space at top (15% + 5% buffer)
      
      # Get ylab cex from GAM plot to match size
      ylab_cex <- if ("cex.lab" %in% names(dots)) dots$cex.lab else 1.2
      
      # Create empty plot frame (suppress axes initially, we'll draw them after)
      plot(NA, NA, 
           xlim = xlim_dist, 
           ylim = ylim_dist,
           xlab = predictor,  # Predictor variable name
           ylab = "Freq.",
           main = "",
           xaxt = "n",
           yaxt = "n",
           bty = "o",  # Show border
           font.lab = 2,  # Bold font to match GAM plot ylab
           cex.lab = ylab_cex)  # Match size of GAM plot ylab
      
      # Draw gray90 background covering the entire plot area
      usr <- par("usr")
      rect(usr[1], usr[3], usr[2], usr[4], col = "gray90", border = NA)
      
      # Redraw border box on top of background
      box()
      
      # Use plot_freq() with add=TRUE to overlay on the background
      plot_freq(x = predictor_data, 
                xlab = predictor,  # Predictor variable name
                main = "",
                xlim = xlim_dist,
                add = TRUE)
      
      # Draw axes after plot_freq (since add=TRUE doesn't draw axes)
      axis(1)  # x-axis
      
      # Draw y-axis with rounded integer labels
      y_ticks <- pretty(c(0, ylim_dist[2]), n = 5)
      y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= ylim_dist[2]]
      y_labels <- round(y_ticks, 0)  # Round to 0 decimals (integers)
      axis(2, at = y_ticks, labels = y_labels, las = 1)  # y-axis with horizontal labels
    } else if (use_plot_density) {
      # For plot_density, compute density manually to determine ylim
      density_obj <- density(predictor_data)
      ylim_density <- c(0, max(density_obj$y) * 1.3)
      
      # Get ylab cex from GAM plot to match size
      ylab_cex <- if ("cex.lab" %in% names(dots)) dots$cex.lab else 1.2
      
      # Create empty plot frame (suppress axes initially, we'll draw them after)
      plot(NA, NA, 
           xlim = xlim_dist, 
           ylim = ylim_density,
           xlab = predictor,  # Predictor variable name
           ylab = "Density",
           main = "",
           xaxt = "n",
           yaxt = "n",
           bty = "o",  # Show border
           font.lab = 2,  # Bold font to match GAM plot ylab
           cex.lab = ylab_cex)  # Match size of GAM plot ylab
      
      # Draw gray90 background covering the entire plot area
      usr <- par("usr")
      rect(usr[1], usr[3], usr[2], usr[4], col = "gray90", border = NA)
      
      # Redraw border box on top of background
      box()
      
      # Manually plot the density curve
      lines(density_obj, col = "dodgerblue", lwd = 4)
      
      # Add axes
      axis(1)  # x-axis
      axis(2, las = 1)  # y-axis with horizontal labels
    }
  }
  
  # Return results invisibly
  result <- list(
    predictor_values = predictor_seq,
    predicted = predicted,
    se = se,
    lower = lower,
    upper = upper
  )
  
  invisible(result)
}







