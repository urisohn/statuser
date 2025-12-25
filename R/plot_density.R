#' Plot density of a variable, by another variable
#'
#' Plots density functions separately for each unique value of a grouping
#' variable.
#'
#' @param y A numeric vector of values to compute densities for, or a column name
#'   (character string or unquoted) if \code{data} is provided.
#' @param group A vector (factor, character, or numeric) used to group the data,
#'   or a column name (character string or unquoted) if \code{data} is provided.
#' @param data An optional data frame containing the variables \code{y} and \code{group}.
#' @param show.t Logical. If TRUE (default), shows points at means, vertical segments,
#'   and mean labels. If FALSE, none of these are displayed.
#' @param ... Additional arguments passed to plotting functions. Can be scalars
#'   (applied to all groups) or vectors (applied element-wise to each group).
#'   Common parameters include \code{col}, \code{lwd}, \code{lty}, \code{pch},
#'   \code{type}, etc. Arguments can also be passed to \code{density()} such as
#'   \code{bw}, \code{kernel}, etc. Additionally, \code{show.means} can be passed
#'   via \code{...}: Logical. If TRUE (default), shows vertical segments at mean values
#'   for 2-3 groups. For 2 groups, low mean uses pos=2 and high mean uses pos=4.
#'   For 3 groups, mid mean uses pos=3. For 4+ groups, vertical segments are not shown.
#'
#' @return Invisibly returns a list containing:
#' \itemize{
#'   \item \code{densities}: A list of density objects, one for each group
#' }
#'
#' @details
#' This function:
#' \itemize{
#'   \item Splits \code{y} by unique values of \code{group}
#'   \item Computes a density estimate for each group
#'   \item Plots all densities on the same graph
#'   \item Handles plotting parameters: scalars apply to all groups, vectors
#'     apply element-wise to groups (in order of unique \code{group} values)
#' }
#'
#' The densities are plotted as smooth curves. Parameters like
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
#' @examples
#' # Basic usage
#' y <- rnorm(100)
#' group <- rep(c("A", "B", "C"), c(30, 40, 30))
#' plot_density(y, group)
#'
#' # With custom colors (scalar - same for all)
#' plot_density(y, group, col = "blue")
#'
#' # With custom colors (vector - different for each group)
#' plot_density(y, group, col = c("red", "green", "blue"))
#'
#' # Multiple parameters
#' plot_density(y, group, col = c("red", "green", "blue"), lwd = c(1, 2, 3))
#'
#' # With line type
#' plot_density(y, group, col = c("red", "green", "blue"), lty = c(1, 2, 3), lwd = 2)
#'
#' # Using data frame
#' df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
#' plot_density(df$value, df$group)
#' plot_density(value ~ group, data = df)  # formula syntax
#' plot_density(value ~ group, data = df, col = c("red", "blue"))
#'
#' @export
plot_density <- function(y, group, data = NULL, show.t = TRUE, ...) {
  #OUTLINE
  #1. Capture variable names for labels
  #2. Extract and handle parameters
  #3. Handle data frame input
  #4. Drop missing data
  #5. Get unique groups
  #6. Initialize return values
  #7. Get default colors based on number of groups
  #8. Helper function: extract parameter value for a group
  #9. Separate density() arguments from plotting arguments
  #10. Compute densities for each group
  #11. Determine overall range for plotting
  #12. Helper function: NULL coalescing
  #13. Plot first density to set up the plot
  #14. Get parameters for first group
  #15. Build plot arguments
  #16. Set up plot
  #17. Add remaining densities
  #18. Add points at y=0 and x=mean with labels
  #19. Add vertical segments at means (if show.means=TRUE and 2-3 groups)
  #20. Add legend
  #21. Return densities
  
  #1. Extract and handle parameters
  # Extract plotting parameters from ...
    dots <- list(...)
    
    # Extract show.means parameter (from dots, as it's not a formal parameter)
    show_means <- if ("show.means" %in% names(dots)) dots$show.means else TRUE
    dots$show.means <- NULL  # Remove from dots so it doesn't get passed to plot functions
  
  #2. Validate inputs using validation function shared with plot_density, plot_cdf, plot_freq
  validated <- validate_plot(y, group, data, func_name = "plot_density", require_group = TRUE)
  y <- validated$y
  group <- validated$group
  # Use names from validate_plot (it handles both formula and standard syntax)
  y_name <- validated$y_name
  group_name <- validated$group_name
  y_name_raw <- validated$y_name_raw
  group_name_raw <- validated$group_name_raw
  
  #3. Drop missing data
    isnagroup=is.na(group)
    isnay=is.na(y)
    group=group[!isnagroup & !isnay]
    y=y[!isnagroup & !isnay]
    
    n.nagroup = sum(isnagroup)
    n.nay = sum(isnay)
    
    if (n.nagroup>0) message2("sohn::plot_density() says: dropped ",n.nagroup," observations with missing '",group_name_raw,"' values",col='red4')
    if (n.nay>0) message2("sohn::plot_density() says: dropped ",n.nay," observations with missing '",y_name_raw,"' values",col='red4')
  
  #5. Get unique groups
    unique_x <- unique(group)
    n_groups <- length(unique_x)
  
  #6. Initialize return values
  
  #7. Get default colors based on number of groups
    default_colors <- get.colors(n_groups) #See utils.R
  
  #8. Helper function: extract parameter value for a group
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
    
  #9. Separate density() vs plotting arguments
    density_args <- c("bw", "adjust", "kernel", "n", "from", "to", "na.rm", "weights")
    density_params <- list()
    plot_params <- dots
    
    for (arg in density_args) {
      if (arg %in% names(dots)) {
        density_params[[arg]] <- dots[[arg]]
        plot_params[[arg]] <- NULL
      }
    }
  
  #10. Compute densities for each group
    density_list <- list()
    y_ranges <- list()
    y_density_max <- list()
    
    for (i in seq_along(unique_x)) {
      group_val <- unique_x[i]
      y_group <- y[group == group_val]
      if (length(y_group) > 0) {
        # Compute density with any density-specific arguments
        density_obj <- do.call(density, c(list(x = y_group), density_params))
        density_list[[i]] <- density_obj
        y_ranges[[i]] <- range(density_obj$x)
        y_density_max[[i]] <- max(density_obj$y)
      }
    }
  
  #11. Determine overall range for plotting
    all_x <- unlist(lapply(density_list, function(d) range(d$x)))
    all_y_density <- unlist(y_density_max)
    x_min <- min(all_x, na.rm = TRUE)
    x_max <- max(all_x, na.rm = TRUE)
    x_range <- x_max - x_min
    x_lim <- c(x_min - 0.05 * x_range, x_max + 0.05 * x_range)
    y_max_density <- max(all_y_density, na.rm = TRUE)
    y_lim_density <- c(0, y_max_density * 1.3)  # Add 30% space for legend

  #12. Helper function: NULL coalescing
    `%||%` <- function(x, y) if (is.null(x)) y else x
  
  #13. Plot first density to set up the plot
    if (length(density_list) > 0) {
      first_density <- density_list[[1]]
    
    #14. Get parameters for first group
      col1 <- get_param("col", 1) %||% default_colors[1]
      lwd1 <- get_param("lwd", 1) %||% 4  # Default lwd=4
      lty1 <- get_param("lty", 1) %||% 1
      type1 <- get_param("type", 1) %||% "l"
      pch1 <- get_param("pch", 1)
      
    #15. Build plot arguments
    # Build plot arguments
    # Set main title if not provided
      if (!"main" %in% names(plot_params)) {
        # Use the captured variable names (e.g., "value" and "cond" instead of "y" and "group")
        main_title <- paste0("Comparing Distribution of '", y_name, "' by '", group_name, "'")
      } else {
        main_title <- plot_params$main
      }
    # Set font and size for main title if not provided
      font_main <- if ("font.main" %in% names(plot_params)) plot_params$font.main else 2
      cex_main <- if ("cex.main" %in% names(plot_params)) plot_params$cex.main else 1.3
    
    # Set xlab if not provided
      xlab_title <- if ("xlab" %in% names(plot_params)) plot_params$xlab else y_name
    
    # Set ylab if not provided
      ylab_title <- if ("ylab" %in% names(plot_params)) plot_params$ylab else "Density"
    
    # Set default ylim if not provided
      if (!"ylim" %in% names(plot_params)) {
        default_ylim <- y_lim_density
        default_ylim[2]=default_ylim[2]*1.15
      } else {
        default_ylim <- plot_params$ylim
        # Ensure ylim always starts at 0
        default_ylim[1] <- 0
      }
    
    # Set default xlim if not provided
      if (!"xlim" %in% names(plot_params)) {
        default_xlim <- x_lim
      } else {
        default_xlim <- plot_params$xlim
      }
    
    # Remove vectorized parameters and data from plot_params for plot()
    # Also remove xlab, ylab, main, xlim, ylim since we handle them separately
    vectorized_params <- c("col", "lwd", "lty", "type", "pch", "data")
    plot_params_to_remove <- c(vectorized_params, "xlab", "ylab", "main", "xlim", "ylim", "font.main", "cex.main")
    plot_params[plot_params_to_remove] <- NULL
    
    plot_args <- list(x = first_density, 
                      col = col1, lwd = lwd1, lty = lty1, type = type1,
                      xlab = xlab_title, 
                      ylab = ylab_title,
                      main = main_title,
                      font.main = font_main,
                      cex.main = cex_main,
                      xlim = default_xlim,
                      ylim = default_ylim,
                      yaxs = "i",  # Prevent padding below 0
                      font.lab = 2, cex.lab = 1.2, las = 1)
    if (!is.null(pch1)) plot_args$pch <- pch1
    
    #16. Set up plot
      # Set up plot
      do.call(plot, c(plot_args, plot_params))
    
    #17. Add remaining densities
      # Add remaining densities
      if (length(density_list) > 1) {
        for (i in 2:length(density_list)) {
          density_obj <- density_list[[i]]
          
          # Get parameters for this group
          coli <- get_param("col", i) %||% default_colors[i]
          lwdi <- get_param("lwd", i) %||% 4  # Default lwd=4
          ltyi <- get_param("lty", i) %||% 1
          typei <- get_param("type", i) %||% "l"
          pchi <- get_param("pch", i)
          
          # Build lines arguments
          lines_args <- list(x = density_obj, 
                            col = coli, lwd = lwdi, lty = ltyi, type = typei)
          if (!is.null(pchi)) lines_args$pch <- pchi
          
          do.call(lines, lines_args)
        }
      }
    
    #18. Add points at y=0 and x=mean
      # Add points at y=0 and x=mean for each group (only if show.t is TRUE)
      if (show.t) {
        for (i in seq_along(density_list)) {
          group_val <- unique_x[i]
          y_group <- y[group == group_val]
          group_mean <- mean(y_group, na.rm = TRUE)
          
          # Get color for this group
          coli <- get_param("col", i) %||% default_colors[i]
          
          # Add point at (mean, 0)
          points(x = group_mean, y = 0, pch = 16, col = coli)
        }
      }
    
    #19. Report means and t-test
      
      # Add vertical segments at mean values (only if show.t and show.means are TRUE)
        if (show.t && show_means && n_groups >= 2 && n_groups <= 3) {
          # Calculate all means
          all_means <- numeric(n_groups)
          for (i in seq_along(density_list)) {
            group_val <- unique_x[i]
            y_group <- y[group == group_val]
            all_means[i] <- mean(y_group, na.rm = TRUE)
          }
        
        # Sort means to determine low/mid/high
          sorted_indices <- order(all_means)
          sorted_means <- all_means[sorted_indices]
        
        # Get maximum density value for segment height (1.1 times highest density)
          y_max_segment <- y_max_density * 1.1
          
        # Add segments and labels based on number of groups
          if (n_groups == 2) {
            # Low mean: pos=2 (left), High mean: pos=4 (right)
            low_idx <- sorted_indices[1]
            high_idx <- sorted_indices[2]
            
          # Low mean segment
            coli_low <- get_param("col", low_idx) %||% default_colors[low_idx]
            segments(x0 = sorted_means[1], y0 = 0, x1 = sorted_means[1], y1 = y_max_segment,
                     col = coli_low, lwd = 2)
            text(x = sorted_means[1], y = y_max_segment, 
                 labels = paste0("M=", round(sorted_means[1], 2)),
                 pos = 2, col = coli_low, cex = 0.8, font = 2)
          
          # High mean segment
            coli_high <- get_param("col", high_idx) %||% default_colors[high_idx]
            segments(x0 = sorted_means[2], y0 = 0, x1 = sorted_means[2], y1 = y_max_segment,
                     col = coli_high, lwd = 2,lty=2)
            
            text(x = sorted_means[2], y = y_max_segment, 
                 labels = paste0("M=", round(sorted_means[2], 2)),
                 pos = 4, col = coli_high, cex = 0.8, font = 2)
            
        } else if (n_groups == 3) {
          # Show all three means: Low (pos=2), Mid (pos=3), High (pos=4)
          low_idx <- sorted_indices[1]
          mid_idx <- sorted_indices[2]
          high_idx <- sorted_indices[3]
          
          # Low mean segment
            coli_low <- get_param("col", low_idx) %||% default_colors[low_idx]
            segments(x0 = sorted_means[1], y0 = 0, x1 = sorted_means[1], y1 = y_max_segment,
                     col = coli_low, lwd = 2)
            text(x = sorted_means[1], y = y_max_segment, 
                 labels = paste0("M=", round(sorted_means[1], 2)),
                 pos = 2, col = coli_low, cex = 0.8, font = 2)
            
          # Mid mean segment
            coli_mid <- get_param("col", mid_idx) %||% default_colors[mid_idx]
            segments(x0 = sorted_means[2], y0 = 0, x1 = sorted_means[2], y1 = y_max_segment,
                     col = coli_mid, lwd = 2)
            text(x = sorted_means[2], y = y_max_segment, 
                 labels = paste0("M=", round(sorted_means[2], 2)),
                 pos = 3, col = coli_mid, cex = 0.8, font = 2)
            
          # High mean segment
            coli_high <- get_param("col", high_idx) %||% default_colors[high_idx]
            segments(x0 = sorted_means[3], y0 = 0, x1 = sorted_means[3], y1 = y_max_segment,
                     col = coli_high, lwd = 2, lty = 2)
            text(x = sorted_means[3], y = y_max_segment, 
                 labels = paste0("M=", round(sorted_means[3], 2)),
                 pos = 4, col = coli_high, cex = 0.8, font = 2)
          
          }
        }
    
    #20. Add legend
      # Add legend on top with title showing x variable name
      # Calculate means and sample sizes for each group
      legend_cols <- sapply(1:length(density_list), function(i) get_param("col", i) %||% default_colors[i])
      legend_lwds <- sapply(1:length(density_list), function(i) get_param("lwd", i) %||% 4)
      legend_ltys <- sapply(1:length(density_list), function(i) get_param("lty", i) %||% 1)
      
      # Create legend labels with group value and sample size
      legend_labels <- character(length(density_list))
      for (i in seq_along(density_list)) {
        group_val <- unique_x[i]
        y_group <- y[group == group_val]
        group_n <- length(y_group)
        
        # Format: 'value'\nN=sample_size
        legend_labels[i] <- paste0("'", as.character(group_val), "'\n",
                                   "N=", group_n)
      }
      
      legend("top", legend = legend_labels, 
             col = legend_cols, lwd = legend_lwds, lty = legend_ltys,
             horiz = TRUE, bty = "n")
  }
  
  #21. Return densities
  # Return densities
    names(density_list) <- as.character(unique_x)
  
  # Build return list
    result <- list(densities = density_list)
  
  invisible(result)
}

