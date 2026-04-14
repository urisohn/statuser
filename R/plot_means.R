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
#' @param legend.title Character string. Title for the legend. If \code{NULL},
#'   no title is shown.
#' @param col Color(s) for \code{x1} bars. If \code{NULL}, colors are chosen
#'   automatically using \code{get.colors(k)} where \code{k} is the number of
#'   unique \code{x1} values.
#' @param col.text Reserved for future plotting (currently unused). Default \code{NULL}.
#' @param cluster Reserved for future clustering support (currently unused). Default \code{NULL}.
#' @param buffer.top Either \code{"auto"} (default) or a numeric value. Extra
#'   vertical headroom (as a fraction of the data y-range) added above the
#'   maximum y value to make room for annotations. When \code{"auto"}, uses 0.35
#'   when an interaction p-value is shown (scenario 2) and 0.25 otherwise.
#' @param save.as File path to save plot (\code{.png} or \code{.svg}). Default
#'   is \code{"plot_means.svg"}. If no folder is provided, the file is saved in
#'   \code{tempdir()}.
#' @param ... Additional arguments passed to \code{plot()} (e.g., \code{main},
#'   \code{ylim}, \code{ylab}).
#'
#' @return A minimal list returned invisibly (see Details). The main table is
#'   \code{mean_results}.
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
#'
#: 1 plot_means: validate -> descriptives -> params -> compute -> draw -> output/export
#: 2 plot_means_validate: NSE-safe arg evaluation + input validation/normalization
#: 3 plot_means_params: derive factor levels/order, colors, legend layout, buffer.top
#: 4 plot_means_compute: build full grid, compute CIs, and bar/layout vectors used for plotting
#: 5 plot_means_draw: render the plot in base graphics (bars, CIs, labels, legend, p-values)
#: 6 plot_means_compute_pvalues: compute and format p-values shown on the plot (scenario-specific)

# plot_means (exported) ----
plot_means <- function(formula,
                       data = NULL,
                       order = NULL,
                       legend.title = NULL,
                       col = NULL,
                       col.text = NULL,
                       cluster = NULL,
                       values.cex = 1,
                       values.pos = "top",
                       values.round = 1,
                       tests = "auto",
                       pvalue.cex = 0.9,
                       pvalue.col = "gray50",
                       buffer.top = "auto",
                       save.as = "plot_means.svg",
                       ...) {
  # 2. Validate + normalize inputs (NSE-safe)
  #   (mc must be captured before anything is evaluated)
    mc <- match.call()
    calling_env <- parent.frame()

  # 2.1 Validate and normalize inputs
    v <- plot_means_validate(
      mc = mc,
      data = data,
      order = order,
      legend.title = legend.title,
      col = col,
      col.text = col.text,
      cluster = cluster,
      values.cex = values.cex,
      values.pos = values.pos,
      values.round = values.round,
      tests = tests,
      pvalue.cex = pvalue.cex,
      pvalue.col = pvalue.col,
      buffer.top = buffer.top,
      save.as = save.as,
      calling_env = calling_env
    )

    formula <- v$formula_eval
    y_name <- v$y_name
    x_names <- v$x_names
    x1_name <- v$x1_name
    x2_name <- v$x2_name
    x3_name <- v$x3_name
    values.cex <- v$values.cex
    values.pos <- v$values.pos
    values.round <- v$values.round
    tests <- v$tests
    pvalue.cex <- v$pvalue.cex
    pvalue.col <- v$pvalue.col
    buffer.top <- v$buffer.top
    save.as <- v$save.as
    save_as_is_default <- v$save_as_is_default

  # 3. Compute descriptives (means) using desc_var()
  #   Non-obvious: desc_var() needs the original unevaluated expression.
    result <- eval(call("desc_var", v$mc$formula, data = v$data), envir = v$calling_env)

  # 3.1 Normalize desc_var output for single grouping variable
    result_plot <- result
    if (length(x_names) == 1 && "group" %in% names(result_plot) && !(x1_name %in% names(result_plot))) {
      result_plot[[x1_name]] <- as.character(result_plot$group)
    }

  # 3.2 CI settings (always computed)
    ci_level <- 0.95

  # 4. Levels/order/colors/buffer/legend layout (derived from data + args)
    params <- plot_means_params(v, result = result, result_plot = result_plot)
    result <- params$result
    x1_levels <- params$x1_levels
    x2_levels <- params$x2_levels
    x3_levels <- params$x3_levels
    k <- params$k
    col <- params$col_vec
    legend_horiz <- params$legend_horiz
    buffer_top_effective <- params$buffer_top_effective
    format_level_label <- params$format_level_label

  # 5. Grid/CI computation and bar layout
    comp <- plot_means_compute(v, params = params, result_plot = result_plot, ci_level = ci_level)
    merged <- comp$merged
    ci_map <- comp$ci_map
    x_lefts <- comp$x_lefts
    x_rights <- comp$x_rights
    x_centers_drawn <- comp$x_centers_drawn
    cell_keys_drawn <- comp$cell_keys_drawn
    n_total_drawn <- comp$n_total_drawn
    n_missing_drawn <- comp$n_missing_drawn
    heights <- comp$heights
    cols <- comp$cols
    block_centers <- comp$block_centers
    block_labels <- comp$block_labels
    x3_section_centers <- comp$x3_section_centers
    x3_section_labels <- comp$x3_section_labels
    x_pos <- comp$x_pos
    bar_width <- comp$bar_width
    bar_step <- comp$bar_step

  # 6. Draw plot
    plot_means_draw(
      v = v,
      params = params,
      comp = comp,
      y_name = y_name,
      x1_levels = x1_levels,
      x2_name = x2_name,
      x3_name = x3_name,
      x1_name = x1_name,
      col = col,
      legend.title = legend.title,
      col.text = col.text,
      values.cex = values.cex,
      values.pos = values.pos,
      values.round = values.round,
      tests = tests,
      pvalue.cex = pvalue.cex,
      pvalue.col = pvalue.col,
      buffer_top_effective = buffer_top_effective,
      ...
    )


  # 7. Prepare minimal output (returned invisibly)
    # Build one output table aligned with the plotting grid (keeps missing combos)
      means <- merged
      means$ciL <- NA_real_
      means$ciH <- NA_real_
      if (!is.null(ci_map) && is.data.frame(ci_map) && nrow(ci_map) > 0) {
        # 7.1 Join CIs onto the grid via the same stable cell key used in CI model
        x2_col <- if (is.null(x2_name)) ".x2" else x2_name
        x3_col <- if (is.null(x3_name)) ".x3" else x3_name
        
        if (!(x2_col %in% names(means))) means[[x2_col]] <- "All"
        if (!(x3_col %in% names(means))) means[[x3_col]] <- "All"
        
        means$cell_key <- paste(
          as.character(means[[x1_name]]),
          as.character(means[[x2_col]]),
          as.character(means[[x3_col]]),
          sep = "|"
        )
        
        ci_idx <- match(means$cell_key, ci_map$cell_key)
        means$ciL <- ifelse(is.na(ci_idx), NA_real_, ci_map$lwr[ci_idx])
        means$ciH <- ifelse(is.na(ci_idx), NA_real_, ci_map$upr[ci_idx])
      }
      
      group_cols <- character(0)
      if (length(x_names) == 1) {
        group_cols <- x1_name
      } else {
        group_cols <- c(x1_name, x2_name, x3_name)
      }
      group_cols <- group_cols[!is.null(group_cols) & nzchar(group_cols)]
      group_cols <- intersect(group_cols, names(means))
      
      keep_cols <- c(group_cols, "mean", "sd", "n.total", "n.missing", "ciL", "ciH")
      keep_cols <- keep_cols[keep_cols %in% names(means)]
      means <- means[, keep_cols, drop = FALSE]
    
  # 7.2 Compute mean comparisons to report (matches what is drawn)
    means_comparisons <- plot_means_compute_pvalues(v = v, params = params, mean_results = means)
    
    out <- list2(
      means,
      means_comparisons
    )
  
  # 7.3 Optional export (plot is still shown on screen)
    if (!is.null(save.as)) {
      headless_dev <- (grDevices::dev.cur() == 1L)
      tmp_dev_file <- NULL
      if (headless_dev) {
        tmp_dev_file <- tempfile(fileext = ".png")
        grDevices::png(tmp_dev_file, width = 1200, height = 800, res = 120)
        on.exit({
          grDevices::dev.off()
          unlink(tmp_dev_file)
        }, add = TRUE)
      }
      
      p <- tryCatch(recordPlot(), error = function(e) NULL)
      if (!is.null(p)) {
        extension <- tools::file_ext(save.as)
        
        # Export sizing: base 6x6 for up to 4 bars; add 1 inch of width
        # per additional pair of bars.
          bars <- length(x_centers_drawn)
          extra_pairs <- ceiling(max(bars - 4, 0) / 2)
          w_in <- 6 + 1 * extra_pairs
          h_in <- 6
          w_px <- as.integer(round(w_in * 1000))
          h_px <- as.integer(round(h_in * 1000))
        if (extension == "svg") grDevices::svg(save.as, width = w_in, height = h_in)
        if (extension == "png") grDevices::png(save.as, width = w_px, height = h_px, res = 1000)
        on.exit(grDevices::dev.off(), add = TRUE)
        replayPlot(p)
        
        save_as_print <- save.as
        save_as_print <- tryCatch(normalizePath(save_as_print, winslash = "/", mustWork = FALSE), error = function(e) save_as_print)
        save_as_print <- gsub("\\\\", "/", save_as_print)
        if (isTRUE(save_as_is_default)) {
          msg <- paste0(
            "plot_means() says: The figure was saved with pre-set dimensions to \n",
            save_as_print, " (set `save.as` to change default location)"
          )
          message2(msg, col = "gray")
        }
        if (!isTRUE(save_as_is_default)) {
          message2("plot_means() says: The figure was saved to `", save_as_print,"`" ,col = "gray")
        }
      }
    }
    invisible(out)
}

# plot_means_validate ----
plot_means_validate <- function(mc,
                                data,
                                order,
                                legend.title,
                                col,
                                col.text,
                                cluster,
                                values.cex,
                                values.pos,
                                values.round,
                                tests,
                                pvalue.cex,
                                pvalue.col,
                                buffer.top,
                                save.as,
                                calling_env) {
  # Resolve and validate formula input (NSE-safe)
    formula_resolved <- evaluate_variable_arguments(
      arg_expr = mc$formula,
      arg_name = "formula",
      data = data,
      calling_env = calling_env,
      func_name = "plot_means",
      allow_null = FALSE
    )
    formula_eval <- formula_resolved$value
    
    validate_formula(formula_eval, data, func_name = "plot_means", calling_env = calling_env)
    if (!inherits(formula_eval, "formula")) {
      stop("plot_means(): First argument must be a formula like y ~ x1 + x2", call. = FALSE)
    }
  
  # Determine grouping variables (up to 3)
    vars <- all.vars(formula_eval)
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
  
  # Validate label sizing argument
    if (!is.numeric(values.cex) || length(values.cex) != 1 || is.na(values.cex) || values.cex <= 0) {
      stop("plot_means(): 'values.cex' must be a single positive number", call. = FALSE)
    }
  
  # Validate value label position
    values.pos <- match.arg(values.pos, c("top", "middle", "bottom", "none"))
  
  # Validate rounding argument for mean labels
    if (!is.numeric(values.round) || length(values.round) != 1 || is.na(values.round) || values.round < 0) {
      stop("plot_means(): 'values.round' must be a single non-negative number", call. = FALSE)
    }
    values.round <- as.integer(values.round)
  
  # Validate tests argument
    tests <- match.arg(tests, c("auto", "none"))
  
  # Validate p-value styling arguments
    if (!is.numeric(pvalue.cex) || length(pvalue.cex) != 1 || is.na(pvalue.cex) || pvalue.cex <= 0) {
      stop("plot_means(): 'pvalue.cex' must be a single positive number", call. = FALSE)
    }
    if (!is.character(pvalue.col) || length(pvalue.col) != 1 || is.na(pvalue.col) || !nzchar(pvalue.col)) {
      stop("plot_means(): 'pvalue.col' must be a single color name", call. = FALSE)
    }
  
  # Validate buffer.top argument
    if (is.character(buffer.top) && length(buffer.top) == 1 && identical(buffer.top, "auto")) {
      buffer.top <- "auto"
    } else {
      if (!is.numeric(buffer.top) || length(buffer.top) != 1 || is.na(buffer.top) || buffer.top < 0) {
        stop("plot_means(): 'buffer.top' must be 'auto' or a single non-negative number (e.g., 0.3)", call. = FALSE)
      }
    }
  
  # Validate save.as argument
    save_as_is_default <- FALSE
    if (!is.null(save.as)) {
      if (!is.character(save.as) || length(save.as) != 1 || is.na(save.as) || !nzchar(save.as)) {
        stop("plot_means(): 'save.as' must be a single file path ('.png' or '.svg')", call. = FALSE)
      }
      save_as_is_default <- identical(save.as, "plot_means.svg")
      
      # If user provided only a filename (no folder), write into tempdir()
        if (identical(dirname(save.as), ".")) {
          save.as <- file.path(tempdir(), save.as)
        }
      
      extension <- tools::file_ext(save.as)
      if (!extension %in% c("svg", "png")) {
        stop("plot_means(): 'save.as' must be either a .png or .svg format.", call. = FALSE)
      }
    }
  
  # Resolve cluster argument (optional; for lm2(..., clusters=))
    cluster_vec <- NULL
    if (!is.null(cluster)) {
      cluster_resolved <- evaluate_variable_arguments(
        arg_expr = mc$cluster,
        arg_name = "cluster",
        data = data,
        calling_env = calling_env,
        func_name = "plot_means",
        allow_null = TRUE
      )
      cluster_vec <- cluster_resolved$value
      if (!is.null(cluster_vec)) {
        if (!is.atomic(cluster_vec) || length(cluster_vec) == 0) {
          stop("plot_means(): 'cluster' must be a vector (or a column name in 'data')", call. = FALSE)
        }
      }
    }
  
  list2(
    mc,
    formula_eval,
    y_name,
    x_names,
    x1_name,
    x2_name,
    x3_name,
    data,
    order,
    legend.title,
    col,
    col.text,
    cluster,
    cluster_vec,
    values.cex,
    values.pos,
    values.round,
    tests,
    pvalue.cex,
    pvalue.col,
    buffer.top,
    save.as,
    save_as_is_default,
    calling_env
  )
}

# plot_means_params ----
plot_means_params <- function(v, result, result_plot) {
  get_levels <- function(var_name, fallback_values) {
    if (!is.null(v$data) && is.data.frame(v$data) && var_name %in% names(v$data) && is.factor(v$data[[var_name]])) {
      return(levels(v$data[[var_name]]))
    }
    vals <- fallback_values
    if (is.null(vals) || !length(vals)) return(character(0))
    vals_chr <- as.character(vals)
    if (suppressWarnings(all(!is.na(as.numeric(vals_chr))))) {
      return(as.character(sort(unique(as.numeric(vals_chr)))))
    }
    sort(unique(vals_chr))
  }
  
  x1_levels <- get_levels(v$x1_name, if (v$x1_name %in% names(result_plot)) result_plot[[v$x1_name]] else NULL)
  x2_levels <- if (is.null(v$x2_name)) "All" else get_levels(v$x2_name, if (v$x2_name %in% names(result_plot)) result_plot[[v$x2_name]] else NULL)
  x3_levels <- if (is.null(v$x3_name)) "All" else get_levels(v$x3_name, if (v$x3_name %in% names(result_plot)) result_plot[[v$x3_name]] else NULL)
  
  # Apply ordering to x1 only (controls bar order/colors)
    if (!is.null(v$order)) {
      if (length(v$order) == 1 && is.numeric(v$order) && v$order == -1) {
        x1_levels <- rev(x1_levels)
      } else {
        missing_groups <- setdiff(x1_levels, v$order)
        extra_groups <- setdiff(v$order, x1_levels)
        
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
        x1_levels <- v$order[v$order %in% x1_levels]
      }
    }
  
  # If there is a single grouping variable, reorder the returned table too
    if (length(v$x_names) == 1) {
      x1_in_result <- if (v$x1_name %in% names(result)) {
        as.character(result[[v$x1_name]])
      } else if ("group" %in% names(result)) {
        as.character(result$group)
      } else {
        NULL
      }
      
      if (!is.null(x1_in_result)) {
        row_idx <- match(x1_levels, x1_in_result)
        if (all(!is.na(row_idx))) {
          result <- result[row_idx, , drop = FALSE]
        }
      }
    }
  
  # Resolve colors for x1 levels
    k <- length(x1_levels)
    col_vec <- v$col
    if (is.null(col_vec)) {
      col_vec <- get.colors(k)
    } else {
      if (!is.character(col_vec)) stop("plot_means(): 'col' must be a character vector (color name(s))", call. = FALSE)
      if (length(col_vec) == 1) col_vec <- rep(col_vec, k)
      if (length(col_vec) != k) {
        stop(sprintf("plot_means(): 'col' must have length 1 or %d (number of x1 levels)", k), call. = FALSE)
      }
    }
  
  # Legend layout (used for auto buffer sizing too)
    max_legend_chars <- suppressWarnings(max(nchar(as.character(x1_levels))))
    if (!is.finite(max_legend_chars)) max_legend_chars <- 0
    legend_horiz <- isTRUE(max_legend_chars <= 10)
  
  # Resolve buffer.top when set to 'auto'
    show_interaction <- identical(v$tests, "auto") &&
      length(v$x_names) == 2 &&
      length(x1_levels) == 2 &&
      !is.null(v$x2_name) &&
      length(x2_levels) == 2 &&
      is.null(v$x3_name)
    
    buffer_top_effective <- if (identical(v$buffer.top, "auto")) {
      base <- if (show_interaction) 0.4 else 0.2
      if (!legend_horiz) base <- base + 0.05
      base
    } else {
      v$buffer.top
    }
  
  # Label disambiguation overlap set and formatter
    label_overlap_set <- character(0)
    if (!is.null(v$x2_name) || !is.null(v$x3_name)) {
      x1_lab <- as.character(x1_levels)
      x2_lab <- if (is.null(v$x2_name)) character(0) else as.character(x2_levels)
      x3_lab <- if (is.null(v$x3_name)) character(0) else as.character(x3_levels)
      
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
  
  list2(
    result = result,
    x1_levels,
    x2_levels,
    x3_levels,
    k,
    col_vec,
    legend_horiz,
    buffer_top_effective,
    format_level_label
  )
}

# plot_means_compute ----
plot_means_compute <- function(v, params, result_plot, ci_level = 0.95) {
  x1_name <- v$x1_name
  x2_name <- v$x2_name
  x3_name <- v$x3_name
  x1_levels <- params$x1_levels
  x2_levels <- params$x2_levels
  x3_levels <- params$x3_levels
  k <- params$k
  col <- params$col_vec
  format_level_label <- params$format_level_label
  
  # Build complete grid of combinations, keeping empty slots
    result_key <- result_plot
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
    if (is.null(x2_name)) result_key$.x2 <- "All"
    if (is.null(x3_name)) result_key$.x3 <- "All"
    
    merged <- merge(grid_df, result_key, by = merge_by, all.x = TRUE, sort = FALSE)
  
  # Confidence intervals via dummy-coded lm2() (always)
    ci_map <- NULL
    mf <- tryCatch(
      model.frame(v$formula_eval, data = v$data, na.action = na.pass),
      error = function(e) NULL
    )
    if (!is.null(mf) && is.data.frame(mf) && nrow(mf) > 0) {
      df_m <- mf
      names(df_m)[1] <- ".__y"
      df_m[[x1_name]] <- as.character(df_m[[x1_name]])
      if (is.null(x2_name)) df_m$.x2 <- "All" else df_m[[x2_name]] <- as.character(df_m[[x2_name]])
      if (is.null(x3_name)) df_m$.x3 <- "All" else df_m[[x3_name]] <- as.character(df_m[[x3_name]])
      
      if (!is.null(v$cluster_vec)) {
        if (!is.null(v$data) && is.data.frame(v$data) && length(v$cluster_vec) == nrow(v$data)) {
          idx <- suppressWarnings(as.integer(rownames(df_m)))
          if (anyNA(idx)) idx <- seq_len(nrow(df_m))
          df_m$.__cluster <- v$cluster_vec[idx]
        } else if (length(v$cluster_vec) == nrow(df_m)) {
          df_m$.__cluster <- v$cluster_vec
        } else {
          stop("plot_means(): 'cluster' length must match rows in 'data'", call. = FALSE)
        }
      }
      
      x2_col <- if (is.null(x2_name)) ".x2" else x2_name
      x3_col <- if (is.null(x3_name)) ".x3" else x3_name
      
      complete_mask <- !is.na(df_m$.__y) &
        !is.na(df_m[[x1_name]]) &
        !is.na(df_m[[x2_col]]) &
        !is.na(df_m[[x3_col]])
      df_m <- df_m[complete_mask, , drop = FALSE]
      
      if (nrow(df_m) > 0) {
        df_m$cell_key <- paste(df_m[[x1_name]], df_m[[x2_col]], df_m[[x3_col]], sep = "|")
        df_m$cell_factor <- factor(df_m$cell_key)
        
        fit <- if (is.null(v$cluster_vec)) {
          lm2(.__y ~ 0 + cell_factor, data = df_m)
        } else {
          lm2(.__y ~ 0 + cell_factor, data = df_m, clusters = df_m$.__cluster)
        }
        
        newdata <- data.frame(cell_factor = levels(df_m$cell_factor))
        pred <- predict(fit, newdata = newdata, interval = "confidence", level = ci_level)
        pred_mat <- pred
        if (is.list(pred) && "fit" %in% names(pred)) pred_mat <- pred$fit
        
        if (is.matrix(pred_mat) && all(c("fit", "lwr", "upr") %in% colnames(pred_mat))) {
          ci_map <- data.frame(
            cell_key = levels(df_m$cell_factor),
            lwr = pred_mat[, "lwr"],
            upr = pred_mat[, "upr"],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  
  # Bar positions and block labels
    gap_x2 <- 1
    gap_x3 <- 2
    bar_width <- 1
    bar_step <- 1
    
    block_centers <- numeric(0)
    block_labels <- character(0)
    x3_section_centers <- numeric(0)
    x3_section_labels <- character(0)
    
    x_lefts <- numeric(0)
    x_rights <- numeric(0)
    x_centers_drawn <- numeric(0)
    cell_keys_drawn <- character(0)
    n_total_drawn <- numeric(0)
    n_missing_drawn <- numeric(0)
    heights <- numeric(0)
    cols <- character(0)
    
    x_pos <- 1
    
    for (x3_val in x3_levels) {
      x3_start <- x_pos
      for (x2_val in x2_levels) {
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
          n_total <- if (nrow(row_sel) >= 1 && "n.total" %in% names(row_sel)) row_sel$n.total[1] else NA_real_
          n_missing <- if (nrow(row_sel) >= 1 && "n.missing" %in% names(row_sel)) row_sel$n.missing[1] else NA_real_
          
          x_center <- x_pos + (i - 1) * bar_step
          centers_block[i] <- x_center
          
          if (!is.na(mean_val)) {
            cell_keys_drawn <- c(cell_keys_drawn, paste0(as.character(x1_val), "|", as.character(x2_val), "|", as.character(x3_val)))
            x_centers_drawn <- c(x_centers_drawn, x_center)
            n_total_drawn <- c(n_total_drawn, n_total)
            n_missing_drawn <- c(n_missing_drawn, n_missing)
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
        if (x2_val != tail(x2_levels, 1)) x_pos <- x_pos + gap_x2
      }
      x3_end <- x_pos - bar_step
      x3_section_centers <- c(x3_section_centers, (x3_start + x3_end) / 2)
      x3_section_labels <- c(x3_section_labels, if (is.null(x3_name)) "" else format_level_label(x3_name, x3_val))
      if (x3_val != tail(x3_levels, 1)) x_pos <- x_pos + gap_x3
    }
  
  list2(
    merged,
    ci_map,
    x_lefts,
    x_rights,
    x_centers_drawn,
    cell_keys_drawn,
    n_total_drawn,
    n_missing_drawn,
    heights,
    cols,
    block_centers,
    block_labels,
    x3_section_centers,
    x3_section_labels,
    x_pos,
    bar_width,
    bar_step
  )
}

# plot_means_draw ----
plot_means_draw <- function(v,
                            params,
                            comp,
                            y_name,
                            x1_levels,
                            x2_name,
                            x3_name,
                            x1_name,
                            col,
                            legend.title,
                            col.text,
                            values.cex,
                            values.pos,
                            values.round,
                            tests,
                            pvalue.cex,
                            pvalue.col,
                            buffer_top_effective,
                            ...) {
  heights <- comp$heights
  cols <- comp$cols
  x_lefts <- comp$x_lefts
  x_rights <- comp$x_rights
  x_centers_drawn <- comp$x_centers_drawn
  cell_keys_drawn <- comp$cell_keys_drawn
  n_total_drawn <- comp$n_total_drawn
  block_centers <- comp$block_centers
  block_labels <- comp$block_labels
  x3_section_centers <- comp$x3_section_centers
  x3_section_labels <- comp$x3_section_labels
  x_pos <- comp$x_pos
  bar_width <- comp$bar_width
  ci_map <- comp$ci_map
  legend_horiz <- params$legend_horiz
  
  dots <- list(...)
  y_max <- max(heights, na.rm = TRUE)
  if (!is.finite(y_max)) y_max <- 1
  y_min <- min(0, min(heights, na.rm = TRUE))
  if (!is.finite(y_min)) y_min <- 0
  
  if (!is.null(ci_map) && nrow(ci_map) > 0) {
    y_min_ci <- suppressWarnings(min(ci_map$lwr, na.rm = TRUE))
    y_max_ci <- suppressWarnings(max(ci_map$upr, na.rm = TRUE))
    if (is.finite(y_min_ci)) y_min <- min(y_min, y_min_ci)
    if (is.finite(y_max_ci)) y_max <- max(y_max, y_max_ci)
  }
  
  if (!"xlab" %in% names(dots)) dots$xlab <- ""
  
  # When x2 is present but x3 is not, show x2 name as x-axis label (centered)
    if (is.null(x3_name) && !is.null(x2_name) && nzchar(x2_name) && identical(dots$xlab, "")) {
      dots$xlab <- x2_name
    }
  if (!"ylab" %in% names(dots)) dots$ylab <- "Mean"
  if (!"main" %in% names(dots)) dots$main <- paste0("Means of ", y_name)
  
  y_span_data <- (y_max - y_min)
  if (!is.finite(y_span_data) || y_span_data <= 0) y_span_data <- abs(y_max)
  if (!is.finite(y_span_data) || y_span_data <= 0) y_span_data <- 1
  ylim_top <- y_max + buffer_top_effective * y_span_data
  if (!"ylim" %in% names(dots)) dots$ylim <- c(y_min, ylim_top)
  if (!"xlim" %in% names(dots)) dots$xlim <- c(0, x_pos)
  if (!"las" %in% names(dots)) dots$las <- 1
  if (!"font.lab" %in% names(dots)) dots$font.lab <- 2
  if (!"cex.lab" %in% names(dots)) dots$cex.lab <- 1.2
  if (!"cex.main" %in% names(dots)) dots$cex.main <- 1.38
  
  dots$type <- "n"
  dots$xaxt <- "n"
  
  user_provided_yaxt <- "yaxt" %in% names(dots)
  if (!user_provided_yaxt) dots$yaxt <- "n"
  
  plot_args <- c(list(x = 0, y = 0), dots)
  do.call(plot, plot_args)
  
  if (!user_provided_yaxt && (is.null(dots$axes) || isTRUE(dots$axes))) {
    y_ticks <- pretty(c(y_min, y_max), n = 5)
    y_ticks <- y_ticks[y_ticks >= y_min & y_ticks <= y_max + 1e-9]
    axis(2, at = y_ticks, las = 1)
  }
  
  if (length(heights)) rect(x_lefts, 0, x_rights, heights, col = cols, border = cols)
  
  lum <- function(col_one) {
    rgb <- grDevices::col2rgb(col_one)
    as.numeric((0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) / 255)
  }
  text_cols <- ifelse(sapply(cols, lum) < 0.5, "white", "black")
  
  if (!is.null(ci_map) && nrow(ci_map) > 0 && length(x_centers_drawn) == length(cell_keys_drawn)) {
    eb_col <- if (!is.null(col.text)) col.text else "gray20"
    cap <- bar_width * 0.08
    match_idx <- match(cell_keys_drawn, ci_map$cell_key)
    ok <- !is.na(match_idx)
    if (any(ok)) {
      lwr <- ci_map$lwr[match_idx[ok]]
      upr <- ci_map$upr[match_idx[ok]]
      x_ok <- x_centers_drawn[ok]
      
      ok2 <- is.finite(lwr) & is.finite(upr)
      if (any(ok2)) {
        x_ok <- x_ok[ok2]
        lwr <- lwr[ok2]
        upr <- upr[ok2]
        
        segments(x_ok, lwr, x_ok, upr, col = eb_col)
        segments(x_ok - cap, lwr, x_ok + cap, lwr, col = eb_col)
        segments(x_ok - cap, upr, x_ok + cap, upr, col = eb_col)
      }
    }
  }
  
  if (identical(tests, "auto")) {
    pvalue_line <- function(x0, x1, y, p) {
      n <- max(length(x0), length(x1), length(y), length(p))
      x0 <- rep_len(x0, n)
      x1 <- rep_len(x1, n)
      y <- rep_len(y, n)
      p <- rep_len(p, n)
      
      y_span <- diff(par("usr")[3:4])
      tick <- 0.015 * y_span
      
      segments(x0, y, x1, y, col = pvalue.col)
      segments(x0, y - tick, x0, y, col = pvalue.col)
      segments(x1, y - tick, x1, y, col = pvalue.col)
      
      x_mid <- (x0 + x1) / 2
      for (i in seq_len(n)) {
        p_txt <- format_p_expr(p[i], digits = 3)
        if (!is.null(p_txt)) {
          text2(x_mid[i], y[i] + 0.03 * y_span, p_txt, bg = "white", cex = pvalue.cex, col = pvalue.col, pad = 0, pad_v = 0)
        }
      }
      
      invisible(NULL)
    }
    
    format_p_expr <- function(p, digits = 3) {
      if (!is.finite(p)) return(NULL)
      min_threshold <- 10^(-digits)
      max_threshold <- 1 - 10^(-digits)
      min_str <- format(min_threshold, nsmall = digits, scientific = FALSE)
      max_str <- format(max_threshold, nsmall = digits, scientific = FALSE)
      
      if (p < min_threshold) return(as.expression(parse(text = paste0("italic(p) < ", min_str))))
      if (p > max_threshold) return(as.expression(parse(text = paste0("italic(p) > ", max_str))))
      
      p_clean <- round(p, digits)
      p_str <- format(p_clean, nsmall = digits, scientific = FALSE)
      as.expression(parse(text = paste0("italic(p) == ", p_str)))
    }
    
    get_p_for_term <- function(fit, var_a, var_b = NULL, want_interaction = FALSE) {
      tab <- attr(fit, "statuser_table")
      if (is.null(tab) || !is.data.frame(tab) || !"term" %in% names(tab) || !"p.value" %in% names(tab)) return(NA_real_)
      terms <- as.character(tab$term)
      if (want_interaction) {
        idx <- which(grepl(":", terms) & grepl(var_a, terms) & grepl(var_b, terms))
      } else {
        idx <- which(grepl(paste0("^", var_a), terms))
      }
      if (!length(idx)) return(NA_real_)
      as.numeric(tab$p.value[idx[1]])
    }
    
    p_lm2_x1 <- function(df_sub) {
      if (nrow(df_sub) == 0) return(NA_real_)
      fit <- if (is.null(v$cluster_vec)) {
        lm2(.__y ~ .__x1, data = df_sub)
      } else {
        lm2(.__y ~ .__x1, data = df_sub, clusters = df_sub$.__cluster)
      }
      get_p_for_term(fit, ".__x1")
    }
    
    x1_is_binary <- length(x1_levels) == 2
    mf_tests <- tryCatch(model.frame(v$formula_eval, data = v$data, na.action = na.omit), error = function(e) NULL)
    if (!is.null(mf_tests) && is.data.frame(mf_tests) && nrow(mf_tests) > 0) {
      names(mf_tests)[1] <- ".__y"
      mf_tests$.__x1 <- as.factor(mf_tests[[x1_name]])
      if (!is.null(v$x2_name)) mf_tests$.__x2 <- as.factor(mf_tests[[v$x2_name]])
      if (!is.null(v$cluster_vec)) {
        if (!is.null(v$data) && is.data.frame(v$data) && length(v$cluster_vec) == nrow(v$data)) {
          idx <- suppressWarnings(as.integer(rownames(mf_tests)))
          if (anyNA(idx)) idx <- seq_len(nrow(mf_tests))
          mf_tests$.__cluster <- v$cluster_vec[idx]
        } else if (length(v$cluster_vec) == nrow(mf_tests)) {
          mf_tests$.__cluster <- v$cluster_vec
        } else {
          stop("plot_means(): 'cluster' length must match rows in 'data'", call. = FALSE)
        }
      }
      
      if (length(v$x_names) == 1 && x1_is_binary) {
        p1 <- p_lm2_x1(mf_tests[, c(".__y", ".__x1"), drop = FALSE])
        
        k1 <- paste0(x1_levels[1], "|All|All")
        k2 <- paste0(x1_levels[2], "|All|All")
        i1 <- match(k1, cell_keys_drawn)
        i2 <- match(k2, cell_keys_drawn)
        if (is.finite(i1) && is.finite(i2)) {
          xA <- x_centers_drawn[i1]
          xB <- x_centers_drawn[i2]
          
          y_base <- max(heights[c(i1, i2)], na.rm = TRUE)
          if (!is.null(ci_map) && nrow(ci_map) > 0) {
            mi <- match(c(k1, k2), ci_map$cell_key)
            if (all(!is.na(mi))) y_base <- max(y_base, ci_map$upr[mi], na.rm = TRUE)
          }
          y_span <- diff(par("usr")[3:4])
          y <- y_base + 0.06 * y_span
          
          pvalue_line(x0 = xA, x1 = xB, y = y, p = p1)
        }
      }
      
      x2_is_binary <- !is.null(v$x2_name) && length(params$x2_levels) == 2
      x3_is_null <- is.null(v$x3_name)
      
      if (length(v$x_names) == 2 && x1_is_binary && x2_is_binary && x3_is_null) {
        x2_levels <- params$x2_levels
        p_by_x2 <- sapply(x2_levels, function(x2v) {
          df_sub <- mf_tests[mf_tests$.__x2 == x2v, c(".__y", ".__x1"), drop = FALSE]
          p_lm2_x1(df_sub)
        })
        
        fit_int <- if (is.null(v$cluster_vec)) {
          lm2(.__y ~ .__x1 * .__x2, data = mf_tests[, c(".__y", ".__x1", ".__x2"), drop = FALSE])
        } else {
          lm2(.__y ~ .__x1 * .__x2, data = mf_tests[, c(".__y", ".__x1", ".__x2", ".__cluster"), drop = FALSE], clusters = mf_tests$.__cluster)
        }
        p_int <- get_p_for_term(fit_int, ".__x1", ".__x2", want_interaction = TRUE)
        p_int_txt <- format_p_expr(p_int, digits = 3)
        
        y_span <- diff(par("usr")[3:4])
        y_tops <- rep(NA_real_, length(x2_levels))
        x_mids <- rep(NA_real_, length(x2_levels))
        
        for (j in seq_along(x2_levels)) {
          x2v <- x2_levels[j]
          k1 <- paste0(x1_levels[1], "|", x2v, "|All")
          k2 <- paste0(x1_levels[2], "|", x2v, "|All")
          i1 <- match(k1, cell_keys_drawn)
          i2 <- match(k2, cell_keys_drawn)
          if (!is.finite(i1) || !is.finite(i2)) next
          
          xA <- x_centers_drawn[i1]
          xB <- x_centers_drawn[i2]
          x_mids[j] <- mean(c(xA, xB))
          
          y_base <- max(heights[c(i1, i2)], na.rm = TRUE)
          if (!is.null(ci_map) && nrow(ci_map) > 0) {
            mi <- match(c(k1, k2), ci_map$cell_key)
            if (all(!is.na(mi))) y_base <- max(y_base, ci_map$upr[mi], na.rm = TRUE)
          }
          
          y <- y_base + 0.06 * y_span
          y_tops[j] <- y
          pvalue_line(x0 = xA, x1 = xB, y = y, p = p_by_x2[[j]])
        }
        
        if (all(is.finite(x_mids)) && all(is.finite(y_tops)) && !is.null(p_int_txt)) {
          x_int <- mean(x_mids)
          y_line <- max(y_tops) + 0.06 * y_span
          y_lab <- y_line + 0.025 * y_span
          
          y_line_from <- max(y_tops) + 0.045 * y_span
          segments(x_mids[1], y_line_from, x_mids[1], y_line, col = pvalue.col)
          segments(x_mids[2], y_line_from, x_mids[2], y_line, col = pvalue.col)
          segments(x_mids[1], y_line, x_mids[2], y_line, col = pvalue.col)
          text2(x_int, y_lab, p_int_txt, bg = "white", cex = pvalue.cex, col = pvalue.col, pad = 0, pad_v = 0)
        }
      }
      
      if (length(v$x_names) == 2 && x1_is_binary && !is.null(v$x2_name) && length(params$x2_levels) > 2 && x3_is_null) {
        x2_levels <- params$x2_levels
        p_by_x2 <- sapply(x2_levels, function(x2v) {
          df_sub <- mf_tests[mf_tests$.__x2 == x2v, c(".__y", ".__x1"), drop = FALSE]
          p_lm2_x1(df_sub)
        })
        
        y_span <- diff(par("usr")[3:4])
        
        for (j in seq_along(x2_levels)) {
          x2v <- x2_levels[j]
          k1 <- paste0(x1_levels[1], "|", x2v, "|All")
          k2 <- paste0(x1_levels[2], "|", x2v, "|All")
          i1 <- match(k1, cell_keys_drawn)
          i2 <- match(k2, cell_keys_drawn)
          if (!is.finite(i1) || !is.finite(i2)) next
          
          xA <- x_centers_drawn[i1]
          xB <- x_centers_drawn[i2]
          
          y_base <- max(heights[c(i1, i2)], na.rm = TRUE)
          if (!is.null(ci_map) && nrow(ci_map) > 0) {
            mi <- match(c(k1, k2), ci_map$cell_key)
            if (all(!is.na(mi))) y_base <- max(y_base, ci_map$upr[mi], na.rm = TRUE)
          }
          
          y <- y_base + 0.06 * y_span
          pvalue_line(x0 = xA, x1 = xB, y = y, p = p_by_x2[[j]])
        }
      }
    }
  }
  
  if (length(x_centers_drawn) > 0 && length(n_total_drawn) == length(x_centers_drawn)) {
    usr <- par("usr")
    pad <- 0.02 * (usr[4] - usr[3])
    
    labels <- character(length(x_centers_drawn))
    y_labs <- numeric(length(x_centers_drawn))
    adjs <- vector("list", length(x_centers_drawn))
    for (i in seq_along(x_centers_drawn)) {
      n_i <- n_total_drawn[i]
      if (!is.finite(n_i)) n_i <- NA
      
      labels[i] <- paste0("n=", n_i)
      
      if (is.finite(heights[i]) && heights[i] < 0) {
        y_labs[i] <- 0 - pad
        adjs[[i]] <- c(0.5, 1)
      } else {
        y_labs[i] <- 0 + pad
        adjs[[i]] <- c(0.5, 0)
      }
    }
    
    for (i in seq_along(x_centers_drawn)) {
      graphics::text(
        x = x_centers_drawn[i],
        y = y_labs[i],
        labels = labels[i],
        col = text_cols[i],
        cex = values.cex,
        adj = adjs[[i]]
      )
    }
  }
  
  if (!identical(values.pos, "none") && length(x_centers_drawn) > 0) {
    usr <- par("usr")
    pad_top <- 0.03 * (usr[4] - usr[3])
    pad_bot <- 0.06 * (usr[4] - usr[3])
    
    mean_labels <- character(length(x_centers_drawn))
    y_mean <- numeric(length(x_centers_drawn))
    for (i in seq_along(x_centers_drawn)) {
      if (identical(values.pos, "bottom")) {
        mean_labels[i] <- paste0("M=", formatC(heights[i], format = "f", digits = values.round))
      } else {
        mean_labels[i] <- formatC(heights[i], format = "f", digits = values.round)
      }
      
      if (identical(values.pos, "top")) {
        if (is.finite(heights[i]) && heights[i] < 0) {
          y_mean[i] <- 0 - pad_top
        } else {
          y_mean[i] <- heights[i] - pad_top
        }
      } else if (identical(values.pos, "middle")) {
        y_mean[i] <- heights[i] / 2
      } else if (identical(values.pos, "bottom")) {
        if (is.finite(heights[i]) && heights[i] < 0) {
          y_mean[i] <- 0 - pad_top
        } else {
          y_mean[i] <- 0 + pad_bot
        }
      }
    }
    
    for (i in seq_along(x_centers_drawn)) {
      text2(
        x = x_centers_drawn[i],
        y = y_mean[i],
        labels = mean_labels[i],
        bg = cols[i],
        cex = values.cex,
        col = text_cols[i],
        pad = 0,
        pad_v = 0
      )
    }
  }
  
  usr <- par("usr")
  x_left <- usr[1] - 0.03 * (usr[2] - usr[1])
  old_xpd <- par("xpd")
  par(xpd = NA)
  on.exit(par(xpd = old_xpd), add = TRUE)
  
  if (is.null(x2_name) || !nzchar(x2_name)) {
    axis(1, at = block_centers, labels = block_labels, las = 1)
  } else {
    if (is.null(x3_name) || !nzchar(x3_name)) {
      axis(1, at = block_centers, labels = block_labels, las = 1)
    } else {
      axis(1, at = block_centers, labels = FALSE, las = 1)
      mtext(x2_name, side = 1, at = x_left, adj = 0, line = 1, font = 2, cex = 1.17)
      mtext(block_labels, side = 1, at = block_centers, line = 1, cex = 1.17)
      
      mtext(x3_name, side = 1, at = x_left, adj = 0, line = 2.5, font = 2, cex = 1.17)
      mtext(x3_section_labels, side = 1, at = x3_section_centers, line = 2.5, font = 2, cex = 1.17)
    }
  }
  
  if (params$k > 1) {
    tw <- suppressWarnings(max(strwidth(x1_levels, cex = 1.3), na.rm = TRUE))
    if (!is.finite(tw)) tw <- 0
    gap_w <- suppressWarnings(strwidth("      ", cex = 1.3))
    if (!is.finite(gap_w)) gap_w <- 0
    
    legend_args <- list(
      "top",
      legend = x1_levels,
      pch = 15,
      col = col,
      bty = "n",
      inset = 0.01,
      cex = 1.15,
      pt.cex = 2,
      x.intersp = 1.2,
      text.width = tw + gap_w,
      horiz = legend_horiz
    )
    if (!legend_horiz) {
      legend_args$text.width <- NULL
      legend_args$x.intersp <- 1.2
    }
    if (!is.null(legend.title)) {
      legend_args$title <- legend.title
      legend_args$title.font <- 2
    }
    do.call(legend, legend_args)
  }
  
  invisible(NULL)
}

# plot_means_compute_pvalues ----
plot_means_compute_pvalues <- function(v, params, mean_results) {
  if (!identical(v$tests, "auto")) return(NULL)
  
  get_term_row <- function(fit, var_a, var_b = NULL, want_interaction = FALSE) {
    tab <- attr(fit, "statuser_table")
    if (is.null(tab) || !is.data.frame(tab) || !"term" %in% names(tab)) return(NULL)
    terms <- as.character(tab$term)
    if (want_interaction) {
      idx <- which(grepl(":", terms) & grepl(var_a, terms) & grepl(var_b, terms))
    } else {
      idx <- which(grepl(paste0("^", var_a), terms))
    }
    if (!length(idx)) return(NULL)
    tab[idx[1], , drop = FALSE]
  }
  
  mf_tests <- tryCatch(model.frame(v$formula_eval, data = v$data, na.action = na.omit), error = function(e) NULL)
  if (is.null(mf_tests) || !is.data.frame(mf_tests) || nrow(mf_tests) == 0) return(NULL)
  
  names(mf_tests)[1] <- ".__y"
  mf_tests$.__x1 <- as.factor(mf_tests[[v$x1_name]])
  if (!is.null(v$x2_name)) mf_tests$.__x2 <- as.factor(mf_tests[[v$x2_name]])
  if (!is.null(v$cluster_vec)) {
    if (!is.null(v$data) && is.data.frame(v$data) && length(v$cluster_vec) == nrow(v$data)) {
      idx <- suppressWarnings(as.integer(rownames(mf_tests)))
      if (anyNA(idx)) idx <- seq_len(nrow(mf_tests))
      mf_tests$.__cluster <- v$cluster_vec[idx]
    } else if (length(v$cluster_vec) == nrow(mf_tests)) {
      mf_tests$.__cluster <- v$cluster_vec
    } else {
      stop("plot_means(): 'cluster' length must match rows in 'data'", call. = FALSE)
    }
  }
  
  p_lm2_x1 <- function(df_sub) {
    if (nrow(df_sub) == 0) return(NA_real_)
    fit <- if (is.null(v$cluster_vec)) {
      lm2(.__y ~ .__x1, data = df_sub)
    } else {
      lm2(.__y ~ .__x1, data = df_sub, clusters = df_sub$.__cluster)
    }
    fit
  }
  
  x1_levels <- params$x1_levels
  x2_levels <- params$x2_levels
  
  x1_is_binary <- length(x1_levels) == 2
  x2_is_binary <- !is.null(v$x2_name) && length(x2_levels) == 2
  x3_is_null <- is.null(v$x3_name)

  group_label <- function(x1, x2 = NULL, x3 = NULL) {
    parts <- character(0)
    if (!is.null(x3) && nzchar(x3)) parts <- c(parts, x3)
    if (!is.null(x2) && nzchar(x2)) parts <- c(parts, x2)
    parts <- c(parts, x1)
    paste(parts, collapse = "_")
  }
  
  mean_lookup <- function(x1, x2 = NULL, x3 = NULL) {
    df <- mean_results
    keep <- rep(TRUE, nrow(df))
    if (!is.null(v$x1_name) && v$x1_name %in% names(df)) keep <- keep & as.character(df[[v$x1_name]]) == as.character(x1)
    if (!is.null(v$x2_name) && v$x2_name %in% names(df) && !is.null(x2)) keep <- keep & as.character(df[[v$x2_name]]) == as.character(x2)
    if (!is.null(v$x3_name) && v$x3_name %in% names(df) && !is.null(x3)) keep <- keep & as.character(df[[v$x3_name]]) == as.character(x3)
    val <- df$mean[keep]
    if (!length(val)) return(NA_real_)
    as.numeric(val[1])
  }
  
  rows <- data.frame(
    group1 = character(0),
    group2 = character(0),
    mean1 = numeric(0),
    mean2 = numeric(0),
    diff = numeric(0),
    t.value = numeric(0),
    p.value = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Scenario 1: x1 only, binary
    if (length(v$x_names) == 1 && x1_is_binary) {
      fit <- p_lm2_x1(mf_tests[, c(".__y", ".__x1"), drop = FALSE])
      tr <- get_term_row(fit, ".__x1")
      if (!is.null(tr)) {
        g1 <- group_label(x1_levels[1])
        g2 <- group_label(x1_levels[2])
        m1 <- mean_lookup(x1_levels[1])
        m2 <- mean_lookup(x1_levels[2])
        rows <- rbind(rows, data.frame(
          group1 = g1,
          group2 = g2,
          mean1 = m1,
          mean2 = m2,
          diff = m2 - m1,
          t.value = as.numeric(tr$t),
          p.value = as.numeric(tr$p.value),
          stringsAsFactors = FALSE
        ))
      }
      return(rows)
    }
  
  # Scenario 2: x1 and x2, both binary, no x3
    if (length(v$x_names) == 2 && x1_is_binary && x2_is_binary && x3_is_null) {
      for (x2v in x2_levels) {
        df_sub <- mf_tests[mf_tests$.__x2 == x2v, c(".__y", ".__x1"), drop = FALSE]
        fit <- p_lm2_x1(df_sub)
        tr <- get_term_row(fit, ".__x1")
        if (is.null(tr)) next
        
        g1 <- group_label(x1_levels[1], x2 = x2v)
        g2 <- group_label(x1_levels[2], x2 = x2v)
        m1 <- mean_lookup(x1_levels[1], x2 = x2v)
        m2 <- mean_lookup(x1_levels[2], x2 = x2v)
        rows <- rbind(rows, data.frame(
          group1 = g1,
          group2 = g2,
          mean1 = m1,
          mean2 = m2,
          diff = m2 - m1,
          t.value = as.numeric(tr$t),
          p.value = as.numeric(tr$p.value),
          stringsAsFactors = FALSE
        ))
      }
      
      fit_int <- if (is.null(v$cluster_vec)) {
        lm2(.__y ~ .__x1 * .__x2, data = mf_tests[, c(".__y", ".__x1", ".__x2"), drop = FALSE])
      } else {
        lm2(.__y ~ .__x1 * .__x2, data = mf_tests[, c(".__y", ".__x1", ".__x2", ".__cluster"), drop = FALSE], clusters = mf_tests$.__cluster)
      }
      tr_int <- get_term_row(fit_int, ".__x1", ".__x2", want_interaction = TRUE)
      if (!is.null(tr_int)) {
        rows <- rbind(rows, data.frame(
          group1 = paste0("interaction(", v$x1_name, ":", v$x2_name, ")"),
          group2 = "",
          mean1 = NA_real_,
          mean2 = NA_real_,
          diff = as.numeric(tr_int$estimate),
          t.value = as.numeric(tr_int$t),
          p.value = as.numeric(tr_int$p.value),
          stringsAsFactors = FALSE
        ))
      }
      
      return(rows)
    }
  
  # Scenario 3: x1 binary, x2 has >2 levels, no x3
    if (length(v$x_names) == 2 && x1_is_binary && !is.null(v$x2_name) && length(x2_levels) > 2 && x3_is_null) {
      for (x2v in x2_levels) {
        df_sub <- mf_tests[mf_tests$.__x2 == x2v, c(".__y", ".__x1"), drop = FALSE]
        fit <- p_lm2_x1(df_sub)
        tr <- get_term_row(fit, ".__x1")
        if (is.null(tr)) next
        
        g1 <- group_label(x1_levels[1], x2 = x2v)
        g2 <- group_label(x1_levels[2], x2 = x2v)
        m1 <- mean_lookup(x1_levels[1], x2 = x2v)
        m2 <- mean_lookup(x1_levels[2], x2 = x2v)
        rows <- rbind(rows, data.frame(
          group1 = g1,
          group2 = g2,
          mean1 = m1,
          mean2 = m2,
          diff = m2 - m1,
          t.value = as.numeric(tr$t),
          p.value = as.numeric(tr$p.value),
          stringsAsFactors = FALSE
        ))
      }
      
      return(rows)
    }
  
  NULL
}

