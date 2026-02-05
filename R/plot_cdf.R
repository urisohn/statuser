#' Plot Empirical Cumulative Distribution Functions by Group
#'
#' Plots empirical cumulative distribution functions (ECDFs) separately for
#' each unique value of a grouping variable, with support for vectorized
#' plotting parameters. If no grouping variable is provided, plots a single ECDF.
#'
#' @param formula A formula of the form \code{y ~ group} where \code{y} is the
#'   response variable and \code{group} is the grouping variable. Alternatively,
#'   can be just \code{y} (without a grouping variable) to plot a single ECDF.
#' @param data An optional data frame containing the variables in the formula.
#'   If \code{data} is not provided, variables are evaluated from the calling environment.
#' @param show.ks Logical. If TRUE (default), shows Kolmogorov-Smirnov test results
#'   when there are exactly 2 groups. If FALSE, KS test results are not displayed.
#' @param show.quantiles Logical. If TRUE (default), shows horizontal lines and results
#'   at 25th, 50th, and 75th percentiles when there are exactly 2 groups. If FALSE,
#'   quantile lines and results are not displayed.
#' @param ... Additional arguments passed to plotting functions. Can be single values
#'   (applied to all groups) or vectors (applied element-wise to each group).
#'   Common parameters include \code{col}, \code{lwd}, \code{lty}, \code{pch},
#'   \code{type}, etc.
#'
#' @return Invisibly returns a list containing:
#'   \itemize{
#'     \item \code{ecdfs}: A list of ECDF function objects, one per group. Each can be
#'       called as a function to compute cumulative probabilities (e.g., \code{result$ecdfs[[1]](5)}
#'       returns P(X <= 5) for group 1).
#'     \item \code{ks_test}: (Only when exactly 2 groups) The Kolmogorov-Smirnov test result
#'       comparing the two distributions. Access p-value with \code{result$ks_test$p.value}.
#'     \item \code{quantile_regression_25}: (Only when exactly 2 groups) Quantile regression
#'       model for the 25th percentile.
#'     \item \code{quantile_regression_50}: (Only when exactly 2 groups) Quantile regression
#'       model for the 50th percentile (median).
#'     \item \code{quantile_regression_75}: (Only when exactly 2 groups) Quantile regression
#'       model for the 75th percentile.
#'     \item \code{warnings}: Any warnings captured during execution (if any).
#'   }
#'
#' @examples
#' # Basic usage with single variable (no grouping)
#' y <- rnorm(100)
#' plot_cdf(y)
#' 
#' # Basic usage with formula syntax and grouping
#' group <- rep(c("A", "B", "C"), c(30, 40, 30))
#' plot_cdf(y ~ group)
#'
#' # With custom colors (scalar - same for all)
#' plot_cdf(y ~ group, col = "blue")
#'
#' # With custom colors (vector - different for each group)
#' plot_cdf(y ~ group, col = c("red", "green", "blue"))
#'
#' # Multiple parameters
#' plot_cdf(y ~ group, col = c("red", "green", "blue"), lwd = c(1, 2, 3))
#'
#' # With line type and point character
#' plot_cdf(y ~ group, col = c("red", "green", "blue"), lty = c(1, 2, 3), lwd = 2)
#'
#' # Using data frame
#' df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
#' plot_cdf(value ~ group, data = df)
#' plot_cdf(value ~ group, data = df, col = c("red", "blue"))
#' 
#' # Formula syntax without data (variables evaluated from environment)
#' widgetness <- rnorm(100)
#' gender <- rep(c("M", "F"), 50)
#' plot_cdf(widgetness ~ gender)
#'
#' # Using the returned object
#' df <- data.frame(value = c(rnorm(50, 0), rnorm(50, 1)), group = rep(c("A", "B"), each = 50))
#' result <- plot_cdf(value ~ group, data = df)
#' 
#' # Use ECDF to find P(X <= 0.5) for group A
#' result$ecdfs[[1]](0.5)
#' 
#' # Access KS test p-value
#' result$ks_test$p.value
#' 
#' # Summarize median quantile regression
#' summary(result$quantile_regression_50)

#' @export
plot_cdf <- function(formula, data = NULL, show.ks = TRUE, show.quantiles = TRUE, ...) {
  # Extract plotting parameters from ...
    dots <- list(...)
    
    # Remove show.ks and show.quantiles from dots if passed (they're formal parameters)
    dots$show.ks <- NULL
    dots$show.quantiles <- NULL
  
  # Validate formula early if it is one
  validate_formula(formula, data, func_name = "plot_cdf", calling_env = parent.frame())
  
  # Check if input is a formula or a variable
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
    validated <- validate_plot(formula, NULL, data, func_name = "plot_cdf", require_group = FALSE, data_name = data_name)
  } else {
    # Not a formula - capture the actual variable name first
    formula_expr <- mc$formula
    actual_name <- if (!is.null(formula_expr)) {
      deparse(formula_expr)
    } else {
      deparse(substitute(formula))
    }
    # Remove quotes if present
    actual_name <- gsub('^"|"$', '', actual_name)
    
    # If data is provided, extract the variable from data frame first
    # This prevents validate_plot from looking for "formula" in the data frame
    if (!is.null(data)) {
      if (!is.data.frame(data)) {
        stop("plot_cdf(): 'data' must be a data frame", call. = FALSE)
      }
      # Clean the variable name (remove $ prefix if present)
      clean_name <- if (grepl("\\$", actual_name)) {
        strsplit(actual_name, "\\$")[[1]][length(strsplit(actual_name, "\\$")[[1]])]
      } else {
        actual_name
      }
      # Check if variable exists in data
      if (!clean_name %in% names(data)) {
        stop(sprintf("plot_cdf(): Column \"%s\" not found in dataset \"%s\"", clean_name, data_name), call. = FALSE)
      }
      # Extract variable from data frame
      formula <- data[[clean_name]]
      # Now call validate_plot without data (since we've already extracted the variable)
      validated <- validate_plot(formula, NULL, NULL, func_name = "plot_cdf", require_group = FALSE, data_name = data_name)
      # Override the names with the actual variable name
      validated$y_name_raw <- clean_name
      validated$y_name <- clean_name
    } else {
      # No data - pass it to validation (will evaluate from environment)
      validated <- validate_plot(formula, NULL, data, func_name = "plot_cdf", require_group = FALSE, data_name = data_name)
      # Override the name if it got "formula" instead of the actual variable name
      if (validated$y_name_raw == "formula") {
        validated$y_name_raw <- actual_name
        validated$y_name <- if (grepl("\\$", actual_name)) {
          strsplit(actual_name, "\\$")[[1]][length(strsplit(actual_name, "\\$")[[1]])]
        } else {
          actual_name
        }
      }
    }
  }
  y <- validated$y
  group <- validated$group
  y_name <- validated$y_name
  group_name <- validated$group_name
  y_name_raw <- validated$y_name_raw
  group_name_raw <- validated$group_name_raw
  data_name <- validated$data_name
  
  # Check if group is provided
  has_group <- !is.null(group)
  
  # Drop missing data
  if (has_group) {
    isnagroup=is.na(group)
    isnay=is.na(y)
    group=group[!isnagroup & !isnay]
    y=y[!isnagroup & !isnay]
    
    n.nagroup = sum(isnagroup)
    n.nay = sum(isnay)
    
    if (n.nagroup>0) message2("plot_cdf() says: dropped ",n.nagroup," observations with missing '",group_name_raw,"' values",col='red4')
    if (n.nay>0) message2("plot_cdf() says: dropped ",n.nay," observations with missing '",y_name_raw,"' values",col='red4')
    
    # Get unique groups and sort them alphabetically
    unique_x <- sort(unique(group))
    n_groups <- length(unique_x)
  } else {
    # No group variable - just drop missing y values
    isnay=is.na(y)
    y=y[!isnay]
    
    n.nay = sum(isnay)
    if (n.nay>0) message2("plot_cdf() says: dropped ",n.nay," observations with missing '",y_name_raw,"' values",col='red4')
    
    unique_x <- NULL
    n_groups <- 1
  }
  
  # Initialize return values for tests (when 2 groups)
  ks_test_result <- NULL
  quantile_regression_25 <- NULL
  quantile_regression_50 <- NULL
  quantile_regression_75 <- NULL
  warnings_list <- list()
  
  # Get default colors based on number of groups
  default_colors <- get.colors(n_groups)
  
  # Helper function to extract parameter value for a group
  get_param <- function(param_name, group_idx) {
    if (param_name %in% names(dots)) {
      param_val <- dots[[param_name]]
      if (length(param_val) == 1) {
        return(param_val)
      } else if (length(param_val) >= group_idx) {
        return(param_val[group_idx])
      } else {
        return(param_val[1])  # Recycle if shorter
      }
    }
    return(NULL)
  }
  
  # Compute ECDFs for each group (or single ECDF if no group)
  ecdf_list <- list()
  y_ranges <- list()
  
  if (has_group) {
    for (i in seq_along(unique_x)) {
      group_val <- unique_x[i]
      y_group <- y[group == group_val]
      if (length(y_group) > 0) {
        ecdf_list[[i]] <- ecdf(y_group)
        y_ranges[[i]] <- range(y_group)
      }
    }
  } else {
    # No group - single ECDF for all data
    if (length(y) > 0) {
      ecdf_list[[1]] <- ecdf(y)
      y_ranges[[1]] <- range(y)
    }
  }
  
  # Determine overall range for plotting
  all_y <- unlist(y_ranges)
  y_min <- min(all_y, na.rm = TRUE)
  y_max <- max(all_y, na.rm = TRUE)
  y_range <- y_max - y_min
  y_lim <- c(y_min - 0.05 * y_range, y_max + 0.05 * y_range)

  # Create sequence for plotting ECDF
  y_seq <- seq(y_min, y_max, length.out = 1000)

  # Helper function for NULL coalescing
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # Plot first ECDF to set up the plot
  if (length(ecdf_list) > 0) {
    first_ecdf <- ecdf_list[[1]]
    first_y_vals <- first_ecdf(y_seq)
    
    # Get parameters for first group
    col1 <- get_param("col", 1) %||% default_colors[1]
    lwd1 <- get_param("lwd", 1) %||% 4  # Default lwd=4
    lty1 <- get_param("lty", 1) %||% 1
    type1 <- get_param("type", 1) %||% "s"
    pch1 <- get_param("pch", 1)
    
    # Build plot arguments
    # Set main title if not provided
      if (!"main" %in% names(dots)) {
        if (has_group) {
          main_title <- paste0("Comparing Distribution of '", y_name, "' by '", group_name, "'")
        } else {
          main_title <- paste0("Distribution of '", y_name, "'")
        }
      } else {
        main_title <- dots$main
      }
    # Set font and size for main title if not provided
      font_main <- if ("font.main" %in% names(dots)) dots$font.main else 2
      cex_main <- if ("cex.main" %in% names(dots)) dots$cex.main else 1.38
    
    # Set xlab if not provided
      xlab_title <- if ("xlab" %in% names(dots)) dots$xlab else y_name
    
    # Set ylab if not provided
      ylab_title <- if ("ylab" %in% names(dots)) dots$ylab else "% of observations"
    
    # Set default ylim if not provided (extend to 1.15 to accommodate legend above plot if groups exist)
      if (!"ylim" %in% names(dots)) {
        if (has_group && n_groups > 1) {
          default_ylim <- c(0, 1.15)
        } else {
          default_ylim <- c(0, 1)
        }
      } else {
        default_ylim <- dots$ylim
      }
    
    # Ensure adequate top margin for main title and legend
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    if (!"mar" %in% names(dots)) {
      # Increase top margin if it's too small (less than 3 lines to accommodate title and legend)
      # Add 1 line to the left margin
      new_mar <- c(old_par$mar[1], old_par$mar[2] + 1, old_par$mar[3], old_par$mar[4])
      if (new_mar[3] < 3) {
        new_mar[3] <- 3
      }
      par(mar = new_mar)
    }
    # Move ylab 0.75 lines to the left
    if (!"mgp" %in% names(dots)) {
      new_mgp <- c(old_par$mgp[1], old_par$mgp[2] - 0.75, old_par$mgp[3])
      par(mgp = new_mgp)
    }
    
    # Remove vectorized parameters and data from dots for plot()
    # Also remove xlab, ylab, main since we handle them separately
    plot_dots <- dots
    vectorized_params <- c("col", "lwd", "lty", "type", "pch", "data")
    plot_params_to_remove <- c(vectorized_params, "xlab", "ylab", "main")
    plot_dots[plot_params_to_remove] <- NULL
    
    plot_args <- list(x = y_seq, y = first_y_vals, 
                      type = type1, col = col1, lwd = lwd1, lty = lty1,
                      xlab = xlab_title, 
                      ylab = ylab_title,
                      main = main_title,
                      font.main = font_main,
                      cex.main = cex_main,
                      ylim = default_ylim,
                      font.lab = 2, cex.lab = 1.2, las = 1,
                      yaxt = "n",  # Suppress default y-axis to draw custom percentage axis
                      xaxt = "n")  # Suppress default x-axis to redraw with adjusted label position
    if (!is.null(pch1)) plot_args$pch <- pch1
    
    # Set up plot
    do.call(plot, c(plot_args, plot_dots))
    
    # Draw custom y-axis with percentage labels
    y_ticks <- seq(0, 1, by = 0.25)
    y_labels <- paste0(y_ticks * 100, "%")
    axis(2, at = y_ticks, labels = y_labels, las = 1)
    
    # Draw x-axis with labels moved lower (padj moves labels down)
    axis(1, padj = 0.5)
    
    # Add remaining ECDFs
    if (length(ecdf_list) > 1) {
      for (i in 2:length(ecdf_list)) {
        ecdf_fun <- ecdf_list[[i]]
        y_vals <- ecdf_fun(y_seq)
        
        # Get parameters for this group
        coli <- get_param("col", i) %||% default_colors[i]
        lwdi <- get_param("lwd", i) %||% 4  # Default lwd=4
        ltyi <- get_param("lty", i) %||% 1
        typei <- get_param("type", i) %||% "s"
        pchi <- get_param("pch", i)
        
        # Build lines arguments
        lines_args <- list(x = y_seq, y = y_vals, type = typei, 
                          col = coli, lwd = lwdi, lty = ltyi)
        if (!is.null(pchi)) lines_args$pch <- pchi
        
        do.call(lines, lines_args)
      }
    }
    
    # Add legend on top (only if there are groups)
    if (has_group && n_groups > 1) {
      legend_cols <- sapply(1:length(ecdf_list), function(i) get_param("col", i) %||% default_colors[i])
      legend_lwds <- sapply(1:length(ecdf_list), function(i) get_param("lwd", i) %||% 4)
      legend_ltys <- sapply(1:length(ecdf_list), function(i) get_param("lty", i) %||% 1)
      
      # Calculate sample sizes for each group and add to legend labels
      group_ns <- sapply(1:length(ecdf_list), function(i) {
        length(y[group == unique_x[i]])
      })
      
      # Create legend labels with sample sizes (newline before sample size, lowercase n)
      legend_labels <- paste0(as.character(unique_x), "\n(n=", group_ns, ")")
      
      legend("top", legend = legend_labels, 
             col = legend_cols, lwd = legend_lwds, lty = legend_ltys,
             horiz = TRUE, bty = "n")
    }
    
    # If exactly 2 groups, perform KS test and quantile regression tests
      if (n_groups == 2) {
        # Get data for both groups
        y1 <- y[group == unique_x[1]]
        y2 <- y[group == unique_x[2]]
        
        # Kolmogorov-Smirnov test (capture warnings about ties)
        ks_test <- withCallingHandlers(
          ks.test(y1, y2),
          warning = function(w) {
            if (grepl("p-value will be approximate in the presence of ties", w$message, ignore.case = TRUE)) {
              warnings_list$ks_ties <<- w$message
              invokeRestart("muffleWarning")
            }
          }
        )
        ks_test_result <- ks_test
        
        # Quantile probabilities (used for both computation and display)
        quantile_probs <- c(0.25, 0.50, 0.75)
        
        # Add horizontal lines at 25%, 50%, and 75% of cumulative probability (only if show.quantiles is TRUE)
        if (show.quantiles) {
          abline(h = quantile_probs, lty = 2, col = "gray80")
        }
        
        # Quantile regression tests at 25th, 50th, and 75th percentiles
        # Always compute quantile regressions (for return value), but only display if show.quantiles is TRUE
        if (requireNamespace("quantreg", quietly = TRUE)) {
        # Show message about independence assumption (only once per session)
        if (!exists("plot_cdf_message_shown", envir = .statuser_state, inherits = FALSE)) {
          message2("The p-values are based on quantile regressions that assume all observations are independent", col = "red")
          assign("plot_cdf_message_shown", TRUE, envir = .statuser_state)
        }
        
        # Create data frame for quantile regression
        df_qr <- data.frame(y = y, x_group = as.numeric(group == unique_x[2]))
        
        # Calculate quantiles for each group (only needed if show.quantiles is TRUE for display)
        if (show.quantiles) {
          q1_25 <- quantile(y1, probs = 0.25, na.rm = TRUE)
          q1_50 <- quantile(y1, probs = 0.50, na.rm = TRUE)
          q1_75 <- quantile(y1, probs = 0.75, na.rm = TRUE)
          q2_25 <- quantile(y2, probs = 0.25, na.rm = TRUE)
          q2_50 <- quantile(y2, probs = 0.50, na.rm = TRUE)
          q2_75 <- quantile(y2, probs = 0.75, na.rm = TRUE)
        }
        
        # Get ECDF functions for finding intersections
        ecdf1 <- ecdf_list[[1]]
        ecdf2 <- ecdf_list[[2]]
        
        # Test quantiles and get p-values, store models as separate objects
        quantile_pvals <- character(length(quantile_probs))
        quantile_regression_25 <- NULL
        quantile_regression_50 <- NULL
        quantile_regression_75 <- NULL
        
        for (i in seq_along(quantile_probs)) {
          tau <- quantile_probs[i]
          tryCatch({
            # Capture warnings about non-uniqueness in quantile regression
            qr_model <- withCallingHandlers(
              quantreg::rq(y ~ x_group, data = df_qr, tau = tau),
              warning = function(w) {
                if (grepl("non.*unique|nonunique", w$message, ignore.case = TRUE)) {
                  warning_key <- paste0("quantile_regression_", tau, "_nonunique")
                  warnings_list[[warning_key]] <<- w$message
                  invokeRestart("muffleWarning")
                }
              }
            )
            # Store model first (before summary, so it's stored even if summary fails)
            qr_summary <- withCallingHandlers(
              summary(qr_model, se = "iid"),
              warning = function(w) {
                if (grepl("non.*unique|nonunique", w$message, ignore.case = TRUE)) {
                  warning_key <- paste0("quantile_regression_", tau, "_summary_nonunique")
                  warnings_list[[warning_key]] <<- w$message
                  invokeRestart("muffleWarning")
                }
              }
            )
            qr_p <- qr_summary$coefficients[2, 4]  # p-value for x_group coefficient
            quantile_pvals[i] <- format_pvalue(qr_p, include_p = TRUE)
            
            # Store model with appropriate name
            if (tau == 0.25) {
              quantile_regression_25 <- qr_model
            } else if (tau == 0.50) {
              quantile_regression_50 <- qr_model
            } else if (tau == 0.75) {
              quantile_regression_75 <- qr_model
            }
          }, error = function(e) {
            quantile_pvals[i] <- "NA"
          })
        }
        
        # Get plot boundaries
        usr <- par("usr")
        x_range <- usr[2] - usr[1]
        y_range <- usr[4] - usr[3]
        
        # Add p-values just above the horizontal lines (only if show.quantiles is TRUE)
        if (show.quantiles) {
          for (i in seq_along(quantile_probs)) {
            text(x = usr[1] + 0.02 * x_range, y = quantile_probs[i] + 0.02, 
                 labels = quantile_pvals[i],
                 adj = c(0, 0.5), cex = 0.8, font = 2)
          }
          
          # Add value labels next to CDF lines
          # Position labels based on which CDF is to the left/right at each quantile
          # Use adj to control alignment and add offset to separate labels
          usr <- par("usr")
          x_range <- usr[2] - usr[1]
          label_offset <- 0.05 / 3 * x_range  # ~1.67% of plot width for separation
          
          # 25th percentile
          if (q1_25 < q2_25) {
            # Group 1 is to the left, right-align its label and offset left
            text(x = q1_25 - label_offset, y = 0.25 + 0.02, labels = round(q1_25, 2),
                 adj = c(1, 0.5), cex = 0.8, font = 2, col = legend_cols[1])
            # Group 2 is to the right, left-align its label and offset right
            text(x = q2_25 + label_offset, y = 0.25 + 0.02, labels = round(q2_25, 2),
                 adj = c(0, 0.5), cex = 0.8, font = 2, col = legend_cols[2])
          } else {
            # Group 2 is to the left, right-align its label and offset left
            text(x = q2_25 - label_offset, y = 0.25 + 0.02, labels = round(q2_25, 2),
                 adj = c(1, 0.5), cex = 0.8, font = 2, col = legend_cols[2])
            # Group 1 is to the right, left-align its label and offset right
            text(x = q1_25 + label_offset, y = 0.25 + 0.02, labels = round(q1_25, 2),
                 adj = c(0, 0.5), cex = 0.8, font = 2, col = legend_cols[1])
          }
          
          # 50th percentile
          if (q1_50 < q2_50) {
            # Group 1 is to the left, right-align its label and offset left
            text(x = q1_50 - label_offset, y = 0.50 + 0.02, labels = round(q1_50, 2),
                 adj = c(1, 0.5), cex = 0.8, font = 2, col = legend_cols[1])
            # Group 2 is to the right, left-align its label and offset right
            text(x = q2_50 + label_offset, y = 0.50 + 0.02, labels = round(q2_50, 2),
                 adj = c(0, 0.5), cex = 0.8, font = 2, col = legend_cols[2])
          } else {
            # Group 2 is to the left, right-align its label and offset left
            text(x = q2_50 - label_offset, y = 0.50 + 0.02, labels = round(q2_50, 2),
                 adj = c(1, 0.5), cex = 0.8, font = 2, col = legend_cols[2])
            # Group 1 is to the right, left-align its label and offset right
            text(x = q1_50 + label_offset, y = 0.50 + 0.02, labels = round(q1_50, 2),
                 adj = c(0, 0.5), cex = 0.8, font = 2, col = legend_cols[1])
          }
          
          # 75th percentile
          if (q1_75 < q2_75) {
            # Group 1 is to the left, right-align its label and offset left
            text(x = q1_75 - label_offset, y = 0.75 + 0.02, labels = round(q1_75, 2),
                 adj = c(1, 0.5), cex = 0.8, font = 2, col = legend_cols[1])
            # Group 2 is to the right, left-align its label and offset right
            text(x = q2_75 + label_offset, y = 0.75 + 0.02, labels = round(q2_75, 2),
                 adj = c(0, 0.5), cex = 0.8, font = 2, col = legend_cols[2])
          } else {
            # Group 2 is to the left, right-align its label and offset left
            text(x = q2_75 - label_offset, y = 0.75 + 0.02, labels = round(q2_75, 2),
                 adj = c(1, 0.5), cex = 0.8, font = 2, col = legend_cols[2])
            # Group 1 is to the right, left-align its label and offset right
            text(x = q1_75 + label_offset, y = 0.75 + 0.02, labels = round(q1_75, 2),
                 adj = c(0, 0.5), cex = 0.8, font = 2, col = legend_cols[1])
          }
        }
      }
      
      # Add KS test results in bottom right corner (only if show.ks is TRUE)
        if (show.ks) {
          ks_d <- round(ks_test$statistic, 3)
          ks_p <- format_pvalue(ks_test$p.value, include_p = TRUE)
          
          usr <- par("usr")
          x_range <- usr[2] - usr[1]
          y_range <- usr[4] - usr[3]
          # Format KS p-value (format_pvalue with include_p=TRUE gives "p = .05")
          ks_p_formatted <- format_pvalue(ks_test$p.value, include_p = TRUE)
          # Format KS results: "Kolmogorov-Smirnov\nD = xx\np = p"
          # format_pvalue already includes "p = ", so we use it directly
          ks_values <- paste0("Kolmogorov-Smirnov\nD = ", ks_d, "\n", ks_p_formatted)
          # Position in bottom right corner
          text(x = usr[2] - 0.02 * x_range, y = usr[3] + 0.02 * y_range, 
               labels = ks_values,
               adj = c(1, 0), cex = 0.8, font = 1)
        }
    }
  }
  
  # Return ECDFs and test results
  if (has_group) {
    names(ecdf_list) <- as.character(unique_x)
  } else {
    names(ecdf_list) <- "all"
  }
  
  # Build return list
  result <- list(ecdfs = ecdf_list)
  
  # Add test results if 2 groups
  if (has_group && n_groups == 2) {
    result$ks_test <- ks_test_result
    # Add quantile regression models as separate named objects
    if (!is.null(quantile_regression_25)) {
      result$quantile_regression_25 <- quantile_regression_25
    }
    if (!is.null(quantile_regression_50)) {
      result$quantile_regression_50 <- quantile_regression_50
    }
    if (!is.null(quantile_regression_75)) {
      result$quantile_regression_75 <- quantile_regression_75
    }
  }
  
  # Add warnings if any were captured
  if (length(warnings_list) > 0) {
    result$warnings <- warnings_list
  }
  
  invisible(result)
}

