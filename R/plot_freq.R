#' Plot frequencies for a variable (histogram without binning)
#'
#' Creates a frequency plot showing the frequency of every observed value, optionaly by group. 
#' Most frequent values are labeled by default.
#'
#' @param formula Two possible uses (similar to \code{t.test()}):
#'   \itemize{
#'     \item {Single Variable (possibly by subgroup)}: \code{plot_freq(y)} or \code{plot_freq(y~x)}
#'     \item {Contrast Two Variables}: \code{plot_freq(y1, y2)}
#'   }
#' @param y2 optional second variable when contrasting two variables \code{plot_freq(y1,y2)}
#' @param data An optional data frame containing the variables in the formula.
#' @param freq Logical. If TRUE (default), displays frequencies. If FALSE, displays percentages.
#' @param order Controls the order in which groups appear in the plot and legend. 
#'   Use \code{-1} to reverse the default order. Alternatively, provide a vector specifying
#'   the exact order (e.g., \code{c("B", "A", "C")}). If \code{NULL} (default), groups are 
#'   ordered by their factor levels (if the grouping variable is a factor) or sorted 
#'   alphabetically/numerically. Only applies when using grouped plots or comparing two variables.
#' @param col Color for the bars. 
#' @param lwd Line width for the frequency bars. Default is 9.
#' @param width Numeric. Width of the frequency bars. If NULL (default), width is automatically calculated based on the spacing between values.
#' @param value.labels Controls value labeling. If numeric, shows labels for the \code{value.labels}
#'   highest-frequency values (including ties). Use \code{-1} or \code{"all"} to show all labels and
#'   \code{0} to show none. Use \code{"auto"} to label all values when there are 30 or fewer
#'   unique values; otherwise label the single most frequent value. For backward compatibility, \code{TRUE}
#'   is treated as \code{-1} and \code{FALSE} as \code{0}.
#' @param ticks.max Integer. Maximum number of unique x values to label on the x-axis. If there are more
#'   than \code{ticks.max} unique values, \code{pretty()} ticks are used instead of labeling every value.
#' @param show.x.value Either \code{"auto"} (default), \code{TRUE}, or \code{FALSE}. If enabled, draws a
#'   small \code{x=<value>} label just above each frequency value label. When \code{"auto"}, x labels are
#'   shown only when plot_freq is not already labeling all x values on the x-axis.
#' 
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
#'
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
#'
#: 1 plot_freq: capture call (NSE-safe) -> normalize args -> dispatch -> compute grid -> draw -> return
#: 2 get_plot_freq_x_ticks: choose x-axis tick strategy (all unique vs pretty())
#: 3 topk_with_ties_mask: choose which bars receive frequency labels (top-k with ties)
#: 4 compute_show_x_value: decide whether to draw x=<value> labels above freq labels
#: 5 draw_freq_bars_and_labels: shared base-graphics drawing loop (bars + labels)
#: 6 grouped path: build (n_unique x n_groups) matrices + legend, then draw via helper
#: 7 non-grouped path: build single-group matrices, then draw via helper
#----------------------------------
# plot_freq (exported) ----
plot_freq <- function(formula, y2=NULL, data=NULL, freq=TRUE, order=NULL, col='dodgerblue', lwd=9, width=NULL, value.labels='auto', ticks.max=30, show.x.value="auto", show.legend=TRUE, legend.title=NULL, col.text=NULL, ...) {
  # 0. Capture the call before any evaluation (NSE-safe)
  # We intentionally use sys.call() in addition to match.call(): match.call() will
  # rename partial matches to formal argument names, which breaks detection of
  # user-supplied argument names (notably: ylim= can partially match y2=).
  mc <- match.call()
  # sys.call() preserves original supplied names; match.call() renames partial
  # matches to the formal name (e.g. ylim= becomes y2= in mc, never ylim=).
  sc <- sys.call()
  sc_names <- names(as.list(sc))[-1]
  # If the user wrote ylim= and did not explicitly pass y2=, R may have already
  # bound ylim's value to formal y2 via partial matching. We detect that case so
  # we can (a) avoid evaluating mc$y2 as an NSE variable and (b) forward ylim to
  # plot() via dots later.
  ylim_matched_to_y2 <- "ylim" %in% sc_names && !"y2" %in% sc_names
  
  # Resolve first argument (formula or first vector)
  formula_resolved <- evaluate_variable_arguments(
    arg_expr = mc$formula,
    arg_name = "formula",
    data = data,
    calling_env = parent.frame(),
    func_name = "plot_freq",
    allow_null = FALSE
  )
  
  # Resolve second argument if present (for two-vector mode).
  # Skip when ylim_matched_to_y2: mc$y2 holds a ..N symbol in wrapper contexts
  # which must not be evaluated as a column name.
  y2_resolved <- if (!is.null(mc$y2) && !ylim_matched_to_y2) {
    evaluate_variable_arguments(
      arg_expr = mc$y2,
      arg_name = "y2",
      data = data,
      calling_env = parent.frame(),
      func_name = "plot_freq",
      allow_null = TRUE
    )
  } else {
    list(value = NULL, name = NULL, name_raw = NULL, was_symbol = FALSE)
  }
  
  # Now overwrite the arguments with resolved values.
  # IMPORTANT: Do not force evaluation of y2 unless we are in the special case
  # where R partially matched ylim= into y2. In normal two-vector usage with
  # data=, y2 may be a bare symbol that must be resolved from data.
  y2_before_resolved <- if (isTRUE(ylim_matched_to_y2)) y2 else NULL
  formula <- formula_resolved$value
  y2 <- y2_resolved$value

  # Extract additional arguments
  dots <- list(...)
  
  # Internal-only: allow overlaying on an existing plot frame
  # Passed via ... as .overlay by internal callers; we remove it from dots so it
  # is never forwarded to plot().
    overlay <- isTRUE(dots$.overlay)
    dots$.overlay <- NULL

  # 1. Normalize and validate user-facing arguments
  # value.labels accepts numeric (k), 0 (none), -1/"all" (all), or "auto".
  # Internally we normalize it to a single integer in {-1, 0, 1, 2, ...} and keep
  # a separate flag to remember whether the user requested "auto" (for messaging).
    value_labels_auto <- FALSE
    if (is.character(value.labels)) {
      if (length(value.labels) != 1 || is.na(value.labels)) {
        stop("plot_freq(): 'value.labels' must be numeric, TRUE/FALSE, or one of: \"all\", \"auto\"", call. = FALSE)
      }
      if (value.labels == "all") {
        value.labels <- -1
      } else if (value.labels == "auto") {
        value_labels_auto <- TRUE
        value.labels <- 1
      } else {
        stop("plot_freq(): 'value.labels' must be numeric, TRUE/FALSE, or one of: \"all\", \"auto\"", call. = FALSE)
      }
    }
    if (isTRUE(value.labels)) value.labels <- -1
    if (identical(value.labels, FALSE)) value.labels <- 0
    if (length(value.labels) != 1 || is.na(value.labels) || !is.numeric(value.labels)) {
      stop("plot_freq(): 'value.labels' must be numeric, TRUE/FALSE, or one of: \"all\", \"auto\"", call. = FALSE)
    }
    if (value.labels < -1) {
      stop("plot_freq(): 'value.labels' must be -1 (all), 0 (none), or a positive integer", call. = FALSE)
    }
    if (value.labels > 0) {
      value.labels <- as.integer(value.labels)
    }

  # ticks.max controls when we stop labeling every unique x on the x-axis and
  # switch to a smaller pretty() tick set.
    if (length(ticks.max) != 1 || is.na(ticks.max) || !is.numeric(ticks.max)) {
      stop("plot_freq(): 'ticks.max' must be a single positive numeric value", call. = FALSE)
    }
    if (ticks.max < 1) {
      stop("plot_freq(): 'ticks.max' must be >= 1", call. = FALSE)
    }
    ticks.max <- as.integer(ticks.max)

  # show.x.value controls whether we annotate x=<value> above frequency labels.
  # "auto" means: show only when the x-axis is not already labeling all unique x.
    if (is.character(show.x.value)) {
      if (length(show.x.value) != 1 || is.na(show.x.value) || show.x.value != "auto") {
        stop("plot_freq(): 'show.x.value' must be \"auto\", TRUE, or FALSE", call. = FALSE)
      }
    } else if (is.logical(show.x.value)) {
      if (length(show.x.value) != 1 || is.na(show.x.value)) {
        stop("plot_freq(): 'show.x.value' must be \"auto\", TRUE, or FALSE", call. = FALSE)
      }
    } else {
      stop("plot_freq(): 'show.x.value' must be \"auto\", TRUE, or FALSE", call. = FALSE)
    }

  # 2. Internal constants/helpers (keep behavior, improve readability)
    VALUE_LABELS_AUTO_UNIQUE_CUTOFF <- 30
    
    # Emit the standard message shown when value.labels="auto" and the plot has
    # more unique values than the auto cutoff.
    warn_value_labels_auto_many <- function() {
      invisible(NULL)
    }
    
    # Return a logical mask selecting the bars that should receive frequency
    # labels when value.labels is a positive integer k (top-k, including ties).
    topk_with_ties_mask <- function(fs, k) {
      # fs: numeric vector, k: positive integer
      non_zero <- fs > 0
      if (!any(non_zero)) return(rep(FALSE, length(fs)))
      if (sum(non_zero) <= k) return(non_zero)
      cutoff <- sort(fs[non_zero], decreasing = TRUE)[k]
      non_zero & (fs >= cutoff)
    }
    
    # Decide whether to draw x=<value> above the frequency labels.
    # user_provided_xaxt is TRUE when the user directly controls x-axis ticks.
    compute_show_x_value <- function(show_x_value, user_provided_xaxt, n_unique, ticks_max) {
      if (is.character(show_x_value) && show_x_value == "auto") {
        return((!user_provided_xaxt) && (n_unique > ticks_max))
      }
      if (isTRUE(show_x_value)) {
        return((user_provided_xaxt) || (n_unique > ticks_max))
      }
      FALSE
    }

    # Shared drawing helper for grouped and non-grouped plots.
    # Expects a common (n_unique x n_groups) representation plus per-group offsets.
    draw_freq_bars_and_labels <- function(x_unique,
                                          heights_by_group,
                                          offsets,
                                          width,
                                          bar_cols,
                                          label_masks_by_group,
                                          label_text_by_group,
                                          show_x_value,
                                          col_text = NULL) {
      n_groups <- ncol(heights_by_group)
      # Draw bars
        for (i in seq_len(n_groups)) {
          fs_i <- heights_by_group[, i]
          non_zero <- fs_i > 0
          if (any(non_zero)) {
            for (j in which(non_zero)) {
              x_val <- x_unique[j]
              freq_val <- fs_i[j]
              x_center <- x_val + offsets[i]
              x_left <- x_center - width/2
              x_right <- x_center + width/2
              
              polygon(x = c(x_left, x_right, x_right, x_left),
                      y = c(0, 0, freq_val, freq_val),
                      col = bar_cols[i], border = bar_cols[i])
            }
          }
        }
      
      # Draw labels
        for (i in seq_len(n_groups)) {
          mask_i <- label_masks_by_group[, i]
          if (any(mask_i)) {
            x_vals <- x_unique[mask_i] + offsets[i]
            y_vals <- heights_by_group[mask_i, i]
            
            label_color <- if (!is.null(col_text)) col_text else bar_cols[i]
            text(x = x_vals, y = y_vals, labels = label_text_by_group[mask_i, i],
                 cex = 0.8, pos = 3, col = label_color)
            
            if (show_x_value) {
              text(x = x_vals, y = y_vals,
                   labels = paste0("x=", x_unique[mask_i]),
                   cex = 0.7, pos = 3, offset = 1.2, col = "gray77")
            }
          }
        }
    }

  # R's partial matching binds ylim= to formal y (since "ylim" begins with "y").
  # Move it to dots so it reaches plot() correctly.
  if (ylim_matched_to_y2 && !"ylim" %in% names(dots)) {
    dots$ylim <- y2_before_resolved
    y2 <- NULL
  }

  # Decide x-axis tick positions for plot_freq
  # - If there are <= max_unique_ticks unique x values, we label them all.
  # - Otherwise we use pretty() on the plotting x-range and then clip.
    get_plot_freq_x_ticks <- function(x_values, dots, max_unique_ticks = 30) {
      # Prefer user-supplied xlim (or computed dots$xlim) when available
        xlim_used <- if ("xlim" %in% names(dots)) dots$xlim else range(x_values, finite = TRUE)
        xlim_used <- as.numeric(xlim_used)
        if (length(xlim_used) != 2 || any(!is.finite(xlim_used))) {
          xlim_used <- range(x_values, finite = TRUE)
        }

      # If few unique values, label them all; otherwise use pretty() on the range
        x_ticks <- if (length(x_values) <= max_unique_ticks) {
          x_values
        } else {
          pretty(xlim_used, n = 10)
        }

      # Clip ticks to the plotting x-range (avoid drawing far outside)
        x_ticks <- x_ticks[x_ticks >= min(xlim_used) & x_ticks <= max(xlim_used)]
        x_ticks
    }
  
  #----------------------------------
  # Dispatch: two-vector vs formula mode ----
  # Two-vector mode (plot_freq(y1, y2)) is implemented by converting to a grouped
  # data.frame and delegating to the formula path to reuse the same plotting code.
  #----------------------------------
  # Check if we're in two-vector comparison mode (formula is vector, y is vector)
  if (!is.null(y2) && !inherits(formula, "formula")) {
    # Two-vector comparison mode: plot_freq(y1, y2)
    
    y1_name <- formula_resolved$name
    y2_name <- y2_resolved$name
    
    # Validate inputs
    if (!is.numeric(formula) || !is.vector(formula)) {
      stop(sprintf("plot_freq(): First argument must be a numeric vector"), call. = FALSE)
    }
    if (!is.numeric(y2) || !is.vector(y2)) {
      stop(sprintf("plot_freq(): Second argument must be a numeric vector"), call. = FALSE)
    }
    
    # Create a data frame and recursively call with grouped syntax
    df <- data.frame(
      value = c(formula, y2),
      group = c(rep(y1_name, length(formula)), rep(y2_name, length(y2))),
      stringsAsFactors = FALSE
    )
    
    # Forward all arguments to the grouped version
    return(plot_freq(value ~ group, data = df, freq = freq, order = order, 
                     col = col, lwd = lwd, width = width, 
                     value.labels = value.labels, ticks.max = ticks.max, show.x.value = show.x.value, 
                     show.legend = show.legend, legend.title = legend.title, 
                     col.text = col.text, ...))
  }
  
  #----------------------------------
  # Standard mode: formula syntax ----
  #----------------------------------
  # Validate formula early if it is one
  validate_formula(formula, data, func_name = "plot_freq", calling_env = parent.frame())
  
  # Check if formula is actually a formula or a vector
  is_formula_input <- inherits(formula, "formula")
  
  # Capture data name for error messages
  data_expr <- mc$data
  data_name <- if (!is.null(data_expr)) {
    data_name_val <- deparse(data_expr)
    gsub('^"|"$', '', data_name_val)
  } else {
    NULL
  }
  
  # Capture original group before validation (to preserve factor levels)
  group_original_before_validation <- NULL
  if (is_formula_input && inherits(formula, "formula")) {
    f <- formula
    if (length(f) >= 3L && identical(f[[1L]], quote(`~`))) {
      rhs <- f[[3L]]
      rhs_is_intercept_only <- is.numeric(rhs) && length(rhs) == 1L &&
        !is.na(rhs[1]) && rhs[1] == 1
      if (!rhs_is_intercept_only) {
        group_original_before_validation <- tryCatch({
          if (!is.null(data)) {
            eval(rhs, envir = data, enclos = parent.frame())
          } else {
            fe <- environment(f)
            if (is.null(fe)) fe <- parent.frame()
            eval(rhs, envir = fe)
          }
        }, error = function(e) NULL)
      }
    }
  }
  
  # Validate inputs using validation function shared with plot_density, plot_cdf, plot_freq
  # If formula input, use validate_plot
  if (is_formula_input) {
    validated <- validate_plot(formula, NULL, data, func_name = "plot_freq", require_group = FALSE, data_name = data_name)
  } else {
    # Not a formula - use resolved names from evaluate_variable_arguments
    validated <- list(
      y = formula,
      group = NULL,
      y_name = formula_resolved$name,
      group_name = NULL,
      y_name_raw = formula_resolved$name_raw,
      group_name_raw = NULL,
      data_name = data_name
    )
  }
  x <- validated$y  # Note: validate_plot uses 'y' but we use 'x'
  group <- validated$group
  x_name <- validated$y_name
  group_name <- validated$group_name
  x_name_raw <- validated$y_name_raw
  group_name_raw <- validated$group_name_raw
  
  # Store original group for factor level checking (before NA removal)
  # Use the version captured before validation if available (to preserve factor levels)
  group_original <- if (!is.null(group_original_before_validation)) {
    group_original_before_validation
  } else {
    group
  }
  
  # Drop missing data
  if (!is.null(group)) {
    isnagroup=is.na(group)
    isnax=is.na(x)
    group=group[!isnagroup & !isnax]
    x=x[!isnagroup & !isnax]
    
    n.nagroup = sum(isnagroup)
    n.nax = sum(isnax)
    
    if (n.nagroup>0) message2("plot_freq() says: dropped ",n.nagroup," observations with missing '",group_name_raw,"' values",col='red2')
    if (n.nax>0) message2("plot_freq() says: dropped ",n.nax," observations with missing '",x_name_raw,"' values",col='red2')
  } else {
    # No group variable - just drop missing x values
    isnax=is.na(x)
    x=x[!isnax]
    
    n.nax = sum(isnax)
    if (n.nax>0) message2("plot_freq() says: dropped ",n.nax," observations with missing '",x_name_raw,"' values",col='red2')
  }
  
  #----------------------------------
  # Grouped path (x ~ group) ----
  #----------------------------------
  if (!is.null(group)) {
    # Validate group argument
    if (length(group) != length(x)) {
      stop("'group' must have the same length as 'x'")
    }
    
    # Determine group ordering
    unique_groups <- unique(group)
    n_groups <- length(unique_groups)
    if (n_groups < 2 || n_groups > 4) {
      stop("'group' must have 2, 3, or 4 unique values")
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
    
    # Check for any NA values in col_indices (shouldn't happen but defensive programming)
    if (any(is.na(col_indices))) {
      stop(sprintf("plot_freq(): Internal error - group matching failed. unique_by: %s, table_by_cols: %s",
                   paste(unique_by, collapse=", "), paste(table_by_cols, collapse=", ")), call. = FALSE)
    }
    
    # Convert to list format for each group
    group_freqs <- list()
    group_freqs_original <- list()  # Store original frequencies for return value
    total <- length(x)  # Total sample size
    for (i in seq_len(n_groups)) {
      # Extract frequencies for this group, ensuring all x values are included
      group_fs <- numeric(length(all_xs))
      x_in_group <- x[group == unique_by[i]]
      counts <- table(x_in_group)
      idx <- match(names(counts), all_xs)
      group_fs[idx] <- as.numeric(counts)
      
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
    
    # Calculate bar width if not provided (needed for xlim calculation and later use)
    if (is.null(width)) {
      if (length(all_xs) > 1) {
        min_spacing <- min(diff(sort(all_xs)))
        width_calc <- min_spacing * 0.2  # 20% of minimum spacing
      } else {
        width_calc <- 0.15  # fallback
      }
    } else {
      width_calc <- width
    }
    
    # Default for overlay mode: treat x-axis labeling as user-controlled
    user_provided_xaxt <- TRUE
    
    # Only set up plot if not overlaying on an existing frame
    if (!overlay) {
      
      # Set default xlim if not set, with padding for bar width
      if (!"xlim" %in% names(dots)) {
        # Bars are drawn with group-specific center offsets, so the outer edge can extend
        # beyond min/max(all_xs) by more than width/2.
        #
        # - 2 groups: outer edge at x ± 1.0*width
        # - 3 groups: outer edge at x ± 1.5*width
        # - 4 groups: outer edge at x ± 2.0*width
        outer_extent <- (n_groups / 2) * width_calc
        padding <- outer_extent * 1.1  # Add 10% extra padding
        dots$xlim <- c(min(all_xs) - padding, max(all_xs) + padding)
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
        # Reserve top 20% for legend if legend will be shown
        if (show.legend) {
          y_max <- y_max * 1.25  # Increase by 25% to make room for legend
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
        existing_ticks <- get_plot_freq_x_ticks(all_xs, dots, max_unique_ticks = ticks.max)
        axis(1, at = existing_ticks, las = 1)
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

    # Decide whether to show x=<value> labels above frequencies
      do_show_x_value <- compute_show_x_value(show.x.value, user_provided_xaxt, length(all_xs), ticks.max)
    
    # Use the width calculated earlier (or user-provided width)
    if (is.null(width)) {
      width <- width_calc
    }
    
    # Calculate offsets for each group so bars touch exactly
    # Centers are equally spaced by `width`, symmetric around 0.
    offsets <- (seq_len(n_groups) - (n_groups + 1) / 2) * width

    # Prepare the common representation used by draw_freq_bars_and_labels():
    # heights_by_group, label masks, and label text are all (n_unique x n_groups).
      heights_by_group <- do.call(cbind, lapply(group_freqs, function(gf) gf$fs))

    # Determine value-label masks and label text (n_unique x n_groups)
      label_masks_by_group <- matrix(FALSE, nrow = length(all_xs), ncol = n_groups)
      label_text_by_group <- matrix("", nrow = length(all_xs), ncol = n_groups)
      if (value.labels != 0) {
        value_labels_effective <- value.labels
        if (isTRUE(value_labels_auto) && length(all_xs) <= VALUE_LABELS_AUTO_UNIQUE_CUTOFF) {
          value_labels_effective <- -1
        }
        if (isTRUE(value_labels_auto) && length(all_xs) > VALUE_LABELS_AUTO_UNIQUE_CUTOFF) {
          warn_value_labels_auto_many()
        }

        for (i in seq_len(n_groups)) {
          fs_i <- heights_by_group[, i]
          if (value_labels_effective == -1) {
            label_masks_by_group[, i] <- fs_i > 0
          } else if (value_labels_effective > 0) {
            label_masks_by_group[, i] <- topk_with_ties_mask(fs_i, value_labels_effective)
          }
          if (any(label_masks_by_group[, i])) {
            if (freq == FALSE) {
              label_text_by_group[label_masks_by_group[, i], i] <- paste0(round(fs_i[label_masks_by_group[, i]], 0), "%")
            } else {
              label_text_by_group[label_masks_by_group[, i], i] <- as.character(fs_i[label_masks_by_group[, i]])
            }
          }
        }
      }

    # Draw bars and labels
      draw_freq_bars_and_labels(
        x_unique = all_xs,
        heights_by_group = heights_by_group,
        offsets = offsets,
        width = width,
        bar_cols = group_cols,
        label_masks_by_group = label_masks_by_group,
        label_text_by_group = label_text_by_group,
        show_x_value = do_show_x_value,
        col_text = col.text
      )
    
    # Add legend showing groups and colors
    if (!overlay && show.legend) {
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
      # Position at top center since we reserved 20% of space
      legend_args <- list("top", legend = legend_labels, fill = legend_cols, 
                          bty = "n", inset = 0.02, cex=1.2, text.width = total_text_width, horiz = FALSE)
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
  
  #----------------------------------
  # Non-grouped path (x only) ----
  #----------------------------------
  # Calculate frequencies for each unique value
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
  
  # Calculate bar width if not provided (needed for xlim calculation and later use)
  if (is.null(width)) {
    if (length(xs) > 1) {
      min_spacing <- min(diff(sort(xs)))
      width_calc <- min_spacing * 0.2  # 20% of minimum spacing
    } else {
      width_calc <- 0.15  # fallback
    }
  } else {
    width_calc <- width
  }
    
  # Default for overlay mode: treat x-axis labeling as user-controlled
  user_provided_xaxt <- TRUE
  
  # Only set up plot if not overlaying on an existing frame
  if (!overlay) {
    #########################################################
    #Default figure parameters if not set
      
      # xlim if not set - add padding for bar width
          if (!"xlim" %in% names(dots)) {
            # For single variable, bars are centered, so add half-width padding
            padding <- width_calc * 0.6  # 60% of width for padding
            dots$xlim <- c(min(xs) - padding, max(xs) + padding)
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
            # Add a small buffer so the tallest bar doesn't touch the top border
            if (is.finite(y_max) && y_max > 0) {
              y_max <- y_max + max(1, y_max * 0.10)
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
        existing_ticks <- get_plot_freq_x_ticks(xs, dots, max_unique_ticks = ticks.max)
        axis(1, at = existing_ticks, las = 1)
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

  # Decide whether to show x=<value> labels above frequencies
    do_show_x_value <- compute_show_x_value(show.x.value, user_provided_xaxt, length(xs), ticks.max)
    
  # Use the width calculated earlier (or user-provided width)
  if (is.null(width)) {
    width <- width_calc
  }
  
  # Prepare (n_unique x 1) matrix for shared drawing
    heights_by_group <- matrix(fs, ncol = 1)
    bar_cols <- rep(col, 1)
    offsets <- 0

  # Determine value-label mask and label text (n_unique x 1)
    label_masks_by_group <- matrix(FALSE, nrow = length(xs), ncol = 1)
    label_text_by_group <- matrix("", nrow = length(xs), ncol = 1)
    if (value.labels != 0) {
      value_labels_effective <- value.labels
      if (isTRUE(value_labels_auto) && length(xs) <= VALUE_LABELS_AUTO_UNIQUE_CUTOFF) {
        value_labels_effective <- -1
      }
      if (isTRUE(value_labels_auto) && length(xs) > VALUE_LABELS_AUTO_UNIQUE_CUTOFF) {
        warn_value_labels_auto_many()
      }

      if (value_labels_effective == -1) {
        label_masks_by_group[, 1] <- fs > 0
      } else if (value_labels_effective > 0) {
        # Use fs_rank for selection (percent vs counts), but show fsp as the label
        fs_rank <- if (freq == FALSE) fs else fs_original
        label_masks_by_group[, 1] <- topk_with_ties_mask(fs_rank, value_labels_effective)
      }

      if (any(label_masks_by_group[, 1])) {
        label_text_by_group[label_masks_by_group[, 1], 1] <- as.character(fsp[label_masks_by_group[, 1]])
      }
    }

  # Draw bars and labels
    draw_freq_bars_and_labels(
      x_unique = xs,
      heights_by_group = heights_by_group,
      offsets = offsets,
      width = width,
      bar_cols = bar_cols,
      label_masks_by_group = label_masks_by_group,
      label_text_by_group = label_text_by_group,
      show_x_value = do_show_x_value,
      col_text = col.text
    )
    
  # Return frequencies or percentages invisibly
  if (freq == FALSE) {
    # Return percentages
    invisible(data.frame(value = xs, percentage = fs))
  } else {
    # Return frequencies
    invisible(data.frame(value = xs, frequency = fs_original))
  }
}

