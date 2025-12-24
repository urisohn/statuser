#' Plot Empirical Cumulative Distribution Functions by Group
#'
#' Plots empirical cumulative distribution functions (ECDFs) separately for
#' each unique value of a grouping variable, with support for vectorized
#' plotting parameters.
#'
#' @param y A numeric vector of values to compute ECDFs for, a column name
#'   (character string or unquoted) if \code{data} is provided, or a formula
#'   of the form \code{y ~ group} where \code{y} is the response variable and
#'   \code{group} is the grouping variable.
#' @param group A vector (factor, character, or numeric) used to group the data,
#'   or a column name (character string or unquoted) if \code{data} is provided.
#'   Ignored if \code{y} is a formula.
#' @param data An optional data frame containing the variables \code{y} and \code{group}.
#'   If \code{y} is a formula and \code{data} is not provided, variables are
#'   evaluated from the calling environment.
#' @param show.ks Logical. If TRUE (default), shows Kolmogorov-Smirnov test results
#'   when there are exactly 2 groups. If FALSE, KS test results are not displayed.
#' @param show.quantiles Logical. If TRUE (default), shows horizontal lines and results
#'   at 25th, 50th, and 75th percentiles when there are exactly 2 groups. If FALSE,
#'   quantile lines and results are not displayed.
#' @param ... Additional arguments passed to plotting functions. Can be scalars
#'   (applied to all groups) or vectors (applied element-wise to each group).
#'   Common parameters include \code{col}, \code{lwd}, \code{lty}, \code{pch},
#'   \code{type}, etc.
#'
#' @return Invisibly returns a list containing:
#' \itemize{
#'   \item \code{ecdfs}: A list of ECDF functions, one for each group
#'   \item \code{ks_test}: (when 2 groups) The Kolmogorov-Smirnov test result
#'   \item \code{quantile_regression_25}: (when 2 groups and quantreg available) Quantile regression model for tau = 0.25
#'   \item \code{quantile_regression_50}: (when 2 groups and quantreg available) Quantile regression model for tau = 0.50
#'   \item \code{quantile_regression_75}: (when 2 groups and quantreg available) Quantile regression model for tau = 0.75
#'   \item \code{warnings}: (when warnings occur) A list of captured warnings, including:
#'     \itemize{
#'       \item \code{ks_ties}: Warning about ties in KS test (if present)
#'       \item \code{quantile_regression_*_nonunique}: Warnings about non-uniqueness in quantile regression (if present)
#'     }
#' }
#'
#' @details
#' This function:
#' \itemize{
#'   \item Splits \code{y} by unique values of \code{group}
#'   \item Computes an ECDF for each group
#'   \item Plots all ECDFs on the same graph
#'   \item Handles plotting parameters: scalars apply to all groups, vectors
#'     apply element-wise to groups (in order of unique \code{group} values)
#' }
#'
#' The ECDFs are plotted as step functions with vertical lines. Parameters like
#' \code{col}, \code{lwd}, \code{lty}, and \code{pch} can be specified as:
#' \itemize{
#'   \item A single value: applied to all groups
#'   \item A vector: applied to groups in order of unique \code{group} values
#' }
#'
#' Default colors are automatically assigned based on the number of groups:
#' \itemize{
#'   \item 2 groups: red4, dodgerblue
#'   \item 3 groups: red4, dodgerblue, green4
#'   \item 4 groups: orange1, orange3, red2, red4
#'   \item 5+ groups: extends with additional colors
#' }
#'
#' When there are exactly 2 groups, the function automatically performs:
#' \itemize{
#'   \item Kolmogorov-Smirnov test for distribution equality
#'   \item Quantile regression tests at 25th, 50th, and 75th percentiles (requires \code{quantreg} package)
#'   \item Displays test results in the bottom right corner
#'   \item Adds vertical dashed lines at the 25th, 50th, and 75th percentiles
#' }
#'
#' @examples
#' # Basic usage
#' y <- rnorm(100)
#' group <- rep(c("A", "B", "C"), c(30, 40, 30))
#' plot_cdf(y, group)
#'
#' # With custom colors (scalar - same for all)
#' plot_cdf(y, group, col = "blue")
#'
#' # With custom colors (vector - different for each group)
#' plot_cdf(y, group, col = c("red", "green", "blue"))
#'
#' # Multiple parameters
#' plot_cdf(y, group, col = c("red", "green", "blue"), lwd = c(1, 2, 3))
#'
#' # With line type and point character
#' plot_cdf(y, group, col = c("red", "green", "blue"), lty = c(1, 2, 3), lwd = 2)
#'
#' # Using data frame
#' df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
#' plot_cdf(value, group, data = df)
#' plot_cdf("value", "group", data = df)  # quoted column names also work
#' plot_cdf(value, group, data = df, col = c("red", "blue"))
#'
#' # Using formula syntax
#' plot_cdf(value ~ group, data = df)
#' plot_cdf(value ~ group, data = df, col = c("red", "blue"))
#' 
#' # Formula syntax without data (variables evaluated from environment)
#' widgetness <- rnorm(100)
#' gender <- rep(c("M", "F"), 50)
#' plot_cdf(widgetness ~ gender)
#'
#' @export
plot_cdf <- function(y, group, data = NULL, show.ks = TRUE, show.quantiles = TRUE, ...) {
  # Extract plotting parameters from ...
    dots <- list(...)
    
    # Remove show.ks and show.quantiles from dots if passed (they're formal parameters)
    dots$show.ks <- NULL
    dots$show.quantiles <- NULL
  
  # Validate inputs using validation function shared with plot_density, plot_cdf, plot_freq
  validated <- validate_plot(y, group, data, func_name = "plot_cdf", require_group = TRUE)
  y <- validated$y
  group <- validated$group
  y_name <- validated$y_name
  group_name <- validated$group_name
  y_name_raw <- validated$y_name_raw
  group_name_raw <- validated$group_name_raw
  data_name <- validated$data_name
  
  # Drop missing data
  isnagroup=is.na(group)
  isnay=is.na(y)
  group=group[!isnagroup & !isnay]
  y=y[!isnagroup & !isnay]
  
  n.nagroup = sum(isnagroup)
  n.nay = sum(isnay)
  
  if (n.nagroup>0) message2("sohn::plot_cdf() says: dropped ",n.nagroup," observations with missing '",group_name_raw,"' values",col='red4')
  if (n.nay>0) message2("sohn::plot_cdf() says: dropped ",n.nay," observations with missing '",y_name_raw,"' values",col='red4')
  
  # Get unique groups and their order
  unique_x <- unique(group)
  n_groups <- length(unique_x)
  
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
  
  # Compute ECDFs for each group
  ecdf_list <- list()
  y_ranges <- list()
  
  for (i in seq_along(unique_x)) {
    group_val <- unique_x[i]
    y_group <- y[group == group_val]
    if (length(y_group) > 0) {
      ecdf_list[[i]] <- ecdf(y_group)
      y_ranges[[i]] <- range(y_group)
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
        main_title <- paste0("Comparing Distribution of '", y_name, "' by '", group_name, "'")
      } else {
        main_title <- dots$main
      }
    # Set font and size for main title if not provided
      font_main <- if ("font.main" %in% names(dots)) dots$font.main else 2
      cex_main <- if ("cex.main" %in% names(dots)) dots$cex.main else 1.3
    
    # Set xlab if not provided
      xlab_title <- if ("xlab" %in% names(dots)) dots$xlab else y_name
    
    # Set ylab if not provided
      ylab_title <- if ("ylab" %in% names(dots)) dots$ylab else "% of observations"
    
    # Set default ylim if not provided (extend to 1.15 to accommodate legend above plot)
      if (!"ylim" %in% names(dots)) {
        default_ylim <- c(0, 1.15)
      } else {
        default_ylim <- dots$ylim
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
                      yaxt = "n")  # Suppress default y-axis to draw custom percentage axis
    if (!is.null(pch1)) plot_args$pch <- pch1
    
    # Set up plot
    do.call(plot, c(plot_args, plot_dots))
    
    # Draw custom y-axis with percentage labels
    y_ticks <- seq(0, 1, by = 0.25)
    y_labels <- paste0(y_ticks * 100, "%")
    axis(2, at = y_ticks, labels = y_labels, las = 1)
    
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
    
    # Add legend on top with title showing x variable name
    legend_cols <- sapply(1:length(ecdf_list), function(i) get_param("col", i) %||% default_colors[i])
    legend_lwds <- sapply(1:length(ecdf_list), function(i) get_param("lwd", i) %||% 4)
    legend_ltys <- sapply(1:length(ecdf_list), function(i) get_param("lty", i) %||% 1)
    legend("top", legend = as.character(unique_x), 
           col = legend_cols, lwd = legend_lwds, lty = legend_ltys,
           horiz = TRUE, bty = "n", title = group_name)
    
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
        if (is.null(getOption("sohn.plot_cdf.message.shown"))) {
          message("The p-values are done with quantile regressions that assume all observations are independent")
          options(sohn.plot_cdf.message.shown = TRUE)
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
  names(ecdf_list) <- as.character(unique_x)
  
  # Build return list
  result <- list(ecdfs = ecdf_list)
  
  # Add test results if 2 groups
  if (n_groups == 2) {
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

