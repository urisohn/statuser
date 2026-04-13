#' Plot means (barplot)
#'
#' Plots group means as a barplot and returns the summary table used.
#'
#' @param formula A formula like \code{y ~ x1}, \code{y ~ x1 + x2}, or
#'   \code{y ~ x1 + x2 + x3}. The left-hand side (y) must be numeric. Up to three
#'   grouping variables are supported. Bars for different \code{x1} values are
#'   shown side-by-side within each \code{(x2, x3)} block, with larger gaps
#'   separating \code{x2} and \code{x3} blocks.
#' @param data An optional data frame containing the variables in the formula.
#' @param order Controls the order of \code{x1} groups (bar order and colors).
#'   Use \code{-1} to reverse the default order, or provide a character vector
#'   with the desired order.
#' @param add Logical. Reserved for future plotting (currently unused).
#' @param legend.title Character string. Title for the legend. If \code{NULL},
#'   defaults to \code{x1} variable name.
#' @param col Color(s) for \code{x1} bars. If \code{NULL}, colors are chosen
#'   automatically using \code{get.colors(k)} where \code{k} is the number of
#'   unique \code{x1} values.
#' @param col.text Reserved for future plotting (currently unused). Default \code{NULL}.
#' @param cluster Reserved for future clustering support (currently unused). Default \code{NULL}.
#' @param ... Additional arguments passed to \code{plot()} (e.g., \code{main},
#'   \code{ylim}, \code{ylab}).
#'
#' @return A \code{desc_var} object (a data frame with attributes), returned visibly.
#'
#' @examples
#' df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
#' plot_means(y ~ group, data = df)
#'
#' df2 <- data.frame(
#'   y = rnorm(200),
#'   x1 = rep(c("A", "B"), 100),
#'   x2 = rep(c("X", "Y"), each = 100)
#' )
#' plot_means(y ~ x1 + x2, data = df2)
#'
#' @export
plot_means <- function(formula,
                       data = NULL,
                       order = NULL,
                       add = FALSE,
                       legend.title = NULL,
                       col = NULL,
                       col.text = NULL,
                       cluster = NULL,
                       ...) {
  #0. CAPTURE UNEVALUATED ARGUMENTS FIRST (before ANY evaluation!)
    mc <- match.call()

  #1. Resolve and validate formula input (NSE-safe)
    formula_resolved <- evaluate_variable_arguments(
      arg_expr = mc$formula,
      arg_name = "formula",
      data = data,
      calling_env = parent.frame(),
      func_name = "plot_means",
      allow_null = FALSE
    )
    formula <- formula_resolved$value

    validate_formula(formula, data, func_name = "plot_means", calling_env = parent.frame())
    if (!inherits(formula, "formula")) {
      stop("plot_means(): First argument must be a formula like y ~ x1 + x2", call. = FALSE)
    }

  #2. Compute descriptives (means) using desc_var()
    # IMPORTANT: desc_var() detects formula syntax using the *unevaluated* call.
    # Passing a formula through an intermediate object (e.g., argument named
    # 'formula') would make desc_var() treat it as non-formula. So we call it
    # with the original expression captured in match.call().
    result <- eval(call("desc_var", mc$formula, data = data), envir = parent.frame())

  #3. Determine grouping variables (up to 3)
    vars <- all.vars(formula)
    if (length(vars) < 2) {
      stop("plot_means(): Formula must have at least one grouping variable: y ~ x1", call. = FALSE)
    }
    y_name <- vars[1]
    x_names <- vars[-1]
    if (length(x_names) > 3) {
      stop("plot_means(): Currently supports up to 3 grouping variables: y ~ x1 + x2 + x3", call. = FALSE)
    }
    x1_name <- x_names[1]
    x2_name <- if (length(x_names) >= 2) x_names[2] else NULL
    x3_name <- if (length(x_names) >= 3) x_names[3] else NULL

  #4. Determine levels for x1/x2/x3 (respect factor levels when possible)
    get_levels <- function(var_name, fallback_values) {
      if (!is.null(data) && is.data.frame(data) && var_name %in% names(data) && is.factor(data[[var_name]])) {
        return(levels(data[[var_name]]))
      }
      vals <- fallback_values
      if (is.null(vals) || !length(vals)) return(character(0))
      vals_chr <- as.character(vals)
      if (suppressWarnings(all(!is.na(as.numeric(vals_chr))))) {
        return(as.character(sort(unique(as.numeric(vals_chr)))))
      }
      sort(unique(vals_chr))
    }

    x1_levels <- get_levels(x1_name, if (x1_name %in% names(result)) result[[x1_name]] else NULL)
    x2_levels <- if (is.null(x2_name)) "All" else get_levels(x2_name, if (x2_name %in% names(result)) result[[x2_name]] else NULL)
    x3_levels <- if (is.null(x3_name)) "All" else get_levels(x3_name, if (x3_name %in% names(result)) result[[x3_name]] else NULL)

  #5. Apply ordering to x1 only (controls bar order/colors)
    if (!is.null(order)) {
      if (length(order) == 1 && is.numeric(order) && order == -1) {
        x1_levels <- rev(x1_levels)
      } else {
        missing_groups <- setdiff(x1_levels, order)
        extra_groups <- setdiff(order, x1_levels)

        if (length(missing_groups) > 0) {
          stop(
            sprintf("plot_means(): 'order' is missing x1 group(s): %s", paste(missing_groups, collapse = ", ")),
            call. = FALSE
          )
        }
        if (length(extra_groups) > 0) {
          warning(
            sprintf("plot_means(): 'order' contains x1 group(s) not in data: %s", paste(extra_groups, collapse = ", ")),
            call. = FALSE
          )
        }
        x1_levels <- order[order %in% x1_levels]
      }
    }

  #5b. If there is a single grouping variable, reorder the returned table too
    if (length(x_names) == 1 && x1_name %in% names(result)) {
      x1_in_result <- as.character(result[[x1_name]])
      row_idx <- match(x1_levels, x1_in_result)
      if (all(!is.na(row_idx))) {
        result <- result[row_idx, , drop = FALSE]
      }
    }

  #6. Resolve colors for x1 levels
    k <- length(x1_levels)
    if (is.null(col)) {
      col <- get.colors(k)
    } else {
      if (!is.character(col)) stop("plot_means(): 'col' must be a character vector (color name(s))", call. = FALSE)
      if (length(col) == 1) col <- rep(col, k)
      if (length(col) != k) {
        stop(sprintf("plot_means(): 'col' must have length 1 or %d (number of x1 levels)", k), call. = FALSE)
      }
    }

  #7. Build complete grid of combinations, keeping empty slots
    result_key <- result
    if (x1_name %in% names(result_key)) result_key[[x1_name]] <- as.character(result_key[[x1_name]])
    if (!is.null(x2_name) && x2_name %in% names(result_key)) result_key[[x2_name]] <- as.character(result_key[[x2_name]])
    if (!is.null(x3_name) && x3_name %in% names(result_key)) result_key[[x3_name]] <- as.character(result_key[[x3_name]])

    grid_df <- expand.grid(
      x1 = x1_levels,
      x2 = x2_levels,
      x3 = x3_levels,
      stringsAsFactors = FALSE
    )
    names(grid_df) <- c(x1_name, if (is.null(x2_name)) ".x2" else x2_name, if (is.null(x3_name)) ".x3" else x3_name)
    if (is.null(x2_name)) grid_df$.x2 <- "All"
    if (is.null(x3_name)) grid_df$.x3 <- "All"

    merge_by <- c(x1_name, if (is.null(x2_name)) ".x2" else x2_name, if (is.null(x3_name)) ".x3" else x3_name)
    if (is.null(x2_name)) {
      result_key$.x2 <- "All"
    }
    if (is.null(x3_name)) {
      result_key$.x3 <- "All"
    }
    merged <- merge(grid_df, result_key, by = merge_by, all.x = TRUE, sort = FALSE)

  #8. Prepare bar heights (x1 within each (x2,x3) block)
    gap_x2 <- 1
    gap_x3 <- 2
    bar_width <- 1
    bar_step <- 1
    
    # Decide when to prefix labels (disambiguate overlaps across x1/x2/x3)
    # Default: show just the value label. If a label appears in more than one
    # grouping variable's levels, prefix with "x2=" or "x3=" accordingly.
      label_overlap_set <- character(0)
      if (!is.null(x2_name) || !is.null(x3_name)) {
        x1_lab <- as.character(x1_levels)
        x2_lab <- if (is.null(x2_name)) character(0) else as.character(x2_levels)
        x3_lab <- if (is.null(x3_name)) character(0) else as.character(x3_levels)
        
        present <- list(x1 = unique(x1_lab), x2 = unique(x2_lab), x3 = unique(x3_lab))
        all_labels <- unique(c(present$x1, present$x2, present$x3))
        label_overlap_set <- all_labels[sapply(all_labels, function(lbl) {
          sum(c(lbl %in% present$x1, lbl %in% present$x2, lbl %in% present$x3)) >= 2
        })]
      }
      
      format_level_label <- function(var_name, level) {
        level_chr <- as.character(level)
        if (!is.null(var_name) && nzchar(var_name) && level_chr %in% label_overlap_set) {
          return(paste0(var_name, "=", level_chr))
        }
        level_chr
      }

    block_centers <- numeric(0)
    block_labels <- character(0)
    x3_section_centers <- numeric(0)
    x3_section_labels <- character(0)

    x_lefts <- numeric(0)
    x_rights <- numeric(0)
    heights <- numeric(0)
    cols <- character(0)

    block_index <- 0
    x_pos <- 1

    for (x3_val in x3_levels) {
      x3_start <- x_pos
      for (x2_val in x2_levels) {
        block_index <- block_index + 1
        centers_block <- numeric(k)

        for (i in seq_len(k)) {
          x1_val <- x1_levels[i]
          row_sel <- merged[
            as.character(merged[[x1_name]]) == as.character(x1_val) &
              as.character(merged[[if (is.null(x2_name)) ".x2" else x2_name]]) == as.character(x2_val) &
              as.character(merged[[if (is.null(x3_name)) ".x3" else x3_name]]) == as.character(x3_val),
            ,
            drop = FALSE
          ]
          mean_val <- if (nrow(row_sel) >= 1) row_sel$mean[1] else NA_real_

          x_center <- x_pos + (i - 1) * bar_step
          centers_block[i] <- x_center

          if (!is.na(mean_val)) {
            x_lefts <- c(x_lefts, x_center - bar_width / 2)
            x_rights <- c(x_rights, x_center + bar_width / 2)
            heights <- c(heights, mean_val)
            cols <- c(cols, col[i])
          }
        }

        block_centers <- c(block_centers, mean(centers_block))
        if (is.null(x2_name)) {
          block_labels <- c(block_labels, "")
        } else {
          block_labels <- c(block_labels, format_level_label(x2_name, x2_val))
        }

        x_pos <- x_pos + k * bar_step
        if (x2_val != tail(x2_levels, 1)) {
          x_pos <- x_pos + gap_x2
        }
      }
      x3_end <- x_pos - bar_step
      x3_section_centers <- c(x3_section_centers, (x3_start + x3_end) / 2)
      x3_section_labels <- c(x3_section_labels, if (is.null(x3_name)) "" else format_level_label(x3_name, x3_val))

      if (x3_val != tail(x3_levels, 1)) {
        x_pos <- x_pos + gap_x3
      }
    }

  #9. Draw plot
    dots <- list(...)
    y_max <- max(heights, na.rm = TRUE)
    if (!is.finite(y_max)) y_max <- 1

    if (!"xlab" %in% names(dots)) dots$xlab <- ""
    if (!"ylab" %in% names(dots)) dots$ylab <- "Mean"
    if (!"main" %in% names(dots)) dots$main <- paste0("Means of ", y_name)
    if (!"ylim" %in% names(dots)) dots$ylim <- c(0, y_max * 1.25)
    if (!"xlim" %in% names(dots)) dots$xlim <- c(0, x_pos)
    if (!"las" %in% names(dots)) dots$las <- 1
    if (!"font.lab" %in% names(dots)) dots$font.lab <- 2
    if (!"cex.lab" %in% names(dots)) dots$cex.lab <- 1.2
    if (!"cex.main" %in% names(dots)) dots$cex.main <- 1.38

    dots$type <- "n"
    dots$xaxt <- "n"

    plot_args <- c(list(x = 0, y = 0), dots)
    do.call(plot, plot_args)

    if (length(heights)) {
      rect(x_lefts, 0, x_rights, heights, col = cols, border = cols)
    }

    axis(1, at = block_centers, labels = block_labels, las = 1)

    if (!is.null(x3_name) && nzchar(x3_name)) {
      mtext(x3_section_labels, side = 3, at = x3_section_centers, line = 0.5, font = 2, cex = 0.9)
    }

    if (k > 1) {
      legend_args <- list(
        "top",
        legend = x1_levels,
        fill = col,
        bty = "n",
        inset = 0.02,
        cex = 1.1,
        horiz = TRUE
      )
      legend_args$title <- if (!is.null(legend.title)) legend.title else x1_name
      legend_args$title.font <- 2
      do.call(legend, legend_args)
    }

  #10. Return (visible) result table
    result
}

