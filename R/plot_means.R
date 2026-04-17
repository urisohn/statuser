#' Barplot of means
#'
#' Plots means, with confidence intervals, and (optionally) p-values for differences of means and interactions 
#'
#' @param formula A formula, e.g., \code{y ~ x1+x2}, where x1 & x2 are
#' grouping variables (e.g., condition indicators in a 2x2 experiment). 
#' The formula can include up to three grouping variables. The plot will 
#'  show contiguous bars for x1, and sets of bars for x2 and x3
#' @param data Optional data frame containing variables in the formula.
#' @param cluster Optional clustering variable when there are repeated 
#' observations per cluster 
#' e.g., \code{cluster="participant_ID"}. 
#' When provided, inference for reported tests is based on regressions with
#' clustered standard errors (via \code{lm2(..., clusters=...)}).
#' @param tests specifies which comparisons of means to report. Syntax involves 
#' putting column numbers in a character string, with a - to symbolize a 
#' comparison and a + symbolizing combination. For example tests="1-2" reports 
#' t-test comparing columns 1 and 2. tests="1-2,3-4" t-tests comparing columns 
#' 1 & 2 and another 3 & 4. To run an interaction use parentheses:
#' tests=\code{"(4-3)-(2-1)"} and can also be combined with simple tests. 
#' Main effects can be specified using `+`, for example (1+2)-(3+4) compares 
#' all observations in the first two columns with all observations in 
#' the next two columns.
#' @param save.as Optional file path to save plot (\code{.png} or \code{.svg}).
#'   When \code{NULL} (default), the plot is not saved.
#' @param quiet Logical. When \code{TRUE}, suppresses console messages from \code{plot_means()}.
#' @param order Controls the order of \code{x1} groups (bar order and colors).
#'   Use \code{-1} to reverse the default order (e.g., if plot shows 'male' first and 'female' second, order=-1 will flip that).
#' @param legend.title Character string. Title for the legend. If \code{NULL},
#'   no title is shown.
#' @param col Color(s) for \code{x1} bars. If \code{NULL}, colors are chosen
#'   automatically.
#' @param col.text Color for confidence intervals and other non-bar annotations.
#'   If \code{NULL}, defaults to a dark gray.
#' @param values.cex Numeric scalar controlling text size for mean value labels
#'   (and related annotations).
#' @param values.align Where within the bars to put the mean value labels: \code{"top"}, \code{"middle"},
#'   \code{"bottom"}, or \code{"none"}.
#' @param values.round Non-negative integer. Number of decimal places for mean
#'   value labels.
#' @param pvalue.cex Numeric scalar controlling p-value label size.
#' @param pvalue.col Color for p-value brackets/labels.
#' @param ci.level Confidence interval level for \code{ciL}/\code{ciH} in \code{$means}.
#'   Default is \code{95}. You can also pass a proportion (e.g., \code{0.95}).
#' @param buffer.top Either \code{"auto"} (default) or a numeric value. Extra
#'   vertical headroom (as a fraction of the data y-range) added above the
#'   maximum y value to make room for annotations. When \code{"auto"}, uses 0.35
#'   when an interaction p-value is shown (scenario 2) and 0.25 otherwise.
#' @param ... Additional arguments passed to \code{plot()} (e.g., \code{main},
#'   \code{ylim}, \code{ylab}).
#'
#' @details
#' When \code{tests="auto"}, the function reports a small default set of
#' differences-of-means tests (when applicable) and, in 2x2 designs, an
#' interaction test:
#' \describe{
#'   \item{Differences in means}{If \code{cluster} is \code{NULL}, these are Welch
#'     two-sample t-tests computed with \code{t.test(..., var.equal=FALSE)}. If
#'     \code{cluster} is provided, these comparisons are computed from a
#'     regression using \code{lm2()} with clustered standard errors.}
#'   \item{Interaction}{The interaction is tested using a linear regression fit
#'     with \code{lm2()}, even when \code{cluster} is \code{NULL}; when
#'     \code{cluster} is provided, the interaction test uses clustered standard
#'     errors.}
#' }
#' The regression-based tests use heteroskedasticity-robust inference (HC3) when
#' \code{cluster} is \code{NULL}. HC3 is a common small-sample adjustment to
#' White-type robust standard errors and is used to reduce sensitivity to
#' heteroskedasticity. When \code{cluster} is provided, \code{plot_means()}
#' instead uses clustered standard errors (robust to within-cluster correlation).
#'
#' In the returned \code{$means} table, \code{ciL} and \code{ciH} are the lower and
#' upper bounds of a \code{ci.level}\% confidence interval for the mean (when
#' available). The same confidence level is used for the confidence-interval
#' whiskers drawn in the figure.
#'
#' @return A minimal list returned invisibly with two elements:
#' \describe{
#'   \item{\code{means}}{A data frame of means (and, when available, confidence intervals)
#'     aligned to the plotting grid.}
#'   \item{\code{tests}}{A data frame of comparisons used for p-value
#'     annotation (or \code{NULL} if not applicable).}
#' }
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
#' df3 <- data.frame(
#'   y = rnorm(600),
#'   x1 = rep(c("control", "treatment"), times = 300),
#'   x2 = rep(rep(c("low", "high"), each = 150), times = 2),
#'   x3 = rep(c("online", "lab"), each = 300)
#' )
#' plot_means(y ~ x1 + x2 + x3, data = df3)
#'
#' @export
#' @importFrom grDevices recordPlot replayPlot
#' @importFrom stats na.pass
#'
#: 1 plot_means: validate -> descriptives -> params -> compute -> draw -> output/export
#: 2 plot_means_validate: NSE-safe arg evaluation + input validation/normalization
#: 3 plot_means_params: derive factor levels/order, colors, legend layout, buffer.top
#: 4 plot_means_compute: build full grid, compute CIs, and bar/layout vectors used for plotting
#: 5 plot_means_draw: render the plot in base graphics (bars, CIs, labels, legend, p-values)
#: 6 plot_means_compute_pvalues: compute and format p-values shown on the plot (scenario-specific)


#----------------------------------
# plot_means (exported) ----
plot_means <- function(formula,
                       data = NULL,
                       cluster = NULL,
                       tests = "auto",
                       save.as = NULL,
                       quiet = FALSE,
                       order = NULL,
                       # Graphics / annotation options
                       legend.title = NULL,
                       col = NULL,
                       col.text = NULL,
                       values.cex = 1,
                       values.align = "top",
                       values.round = 1,
                       pvalue.cex = 0.9,
                       pvalue.col = "gray50",
                       ci.level = 95,
                       buffer.top = "auto",
                       ...) {
  # 2. Validate + normalize inputs (NSE-safe)
  #   (mc must be captured before anything is evaluated)
    mc <- match.call()
    calling_env <- parent.frame()
    
  # Track whether any console output was emitted by plot_means()
    printed_any <- FALSE
    
    msg2 <- function(...) {
      if (isTRUE(quiet)) return(invisible(NULL))
      printed_any <<- TRUE
      message2(...)
    }

  # 1. Validate and normalize inputs
  # Convert user-facing arguments into a single normalized list (`v`) used throughout.
  #   This is where we resolve NSE inputs (formula/data/cluster), validate types and
  #   lengths, and set defaults so downstream helpers can assume consistent fields.
    v <- plot_means_validate(
      mc = mc,
      data = data,
      order = order,
      legend.title = legend.title,
      col = col,
      col.text = col.text,
      cluster = cluster,
      values.cex = values.cex,
      values.align = values.align,
      values.round = values.round,
      tests = tests,
      pvalue.cex = pvalue.cex,
      pvalue.col = pvalue.col,
      ci.level = ci.level,
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
    values.align <- v$values.align
    values.round <- v$values.round
    tests <- v$tests
    pvalue.cex <- v$pvalue.cex
    pvalue.col <- v$pvalue.col
    ci.level <- v$ci.level
    buffer.top <- v$buffer.top
    save.as <- v$save.as

  # 2. Compute descriptives (means) using statuser::desc_var()
    result <- eval(call("desc_var", v$mc$formula, data = v$data), envir = v$calling_env)

  # 2.1 Normalize desc_var output for single grouping variable
    result_plot <- result
    if (length(x_names) == 1 && "group" %in% names(result_plot) && !(x1_name %in% names(result_plot))) {
      result_plot[[x1_name]] <- as.character(result_plot$group)
    }

  # 2.2 CI settings (always computed)
    ci_level <- as.numeric(ci.level) / 100

  # 3. Levels/order/colors/buffer/legend layout (derived from data + args)
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

  # 4. Results table with CI computation and bar layout
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

  # 5. Prepare comparison table used for p-value drawing
    # Build one output table aligned with the plotting grid (keeps missing combos)
      means <- merged
      means$ciL <- NA_real_
      means$ciH <- NA_real_
      if (nrow(ci_map) > 0) {
        # Join CIs onto the grid via the same stable cell key used in CI model
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
      rownames(means) <- NULL
    
    means_comparisons <- plot_means_compute_pvalues(v = v, params = params, mean_results = means, comp = comp)
    means_comparisons__out <- means_comparisons
    if (!is.null(means_comparisons__out) && is.data.frame(means_comparisons__out)) {
      drop_cols <- intersect(c("col1", "col2", "col3", "col4"), names(means_comparisons__out))
      if (length(drop_cols)) means_comparisons__out[drop_cols] <- NULL
    }
    
    if (!isTRUE(quiet) && !is.null(means_comparisons) && is.data.frame(means_comparisons) && nrow(means_comparisons) > 0) {
      fmt_p <- function(p) {
        if (!is.finite(p)) return("NA")
        if (p < 0.001) return("<0.001")
        format(round(p, 3), nsmall = 3, scientific = FALSE)
      }
      fmt_df <- function(df) {
        if (!is.finite(df)) return("NA")
        format(round(df, 1), nsmall = 1, scientific = FALSE)
      }
      fmt_df_int <- function(df) {
        if (!is.finite(df)) return("NA")
        format(round(df, 0), nsmall = 0, scientific = FALSE)
      }
      fmt_t <- function(t) {
        if (!is.finite(t)) return("NA")
        format(round(t, 2), nsmall = 2, scientific = FALSE)
      }
      
      method_fallback <- function(row) {
        # When not provided by compute_pvalues(), infer from cluster + test type.
        if (!is.null(v$cluster_vec)) return("regression (clustered SE)")
        if ("test_type" %in% names(row) && identical(as.character(row$test_type[1]), "interaction")) return("regression interaction")
        "Welch t-test"
      }
      
      lines <- character(0)
      any_welch <- FALSE
      any_interaction <- FALSE
      for (i in seq_len(nrow(means_comparisons))) {
        r <- means_comparisons[i, , drop = FALSE]
        
        label <- ""
        if (all(c("test_type", "col1", "col2") %in% names(r))) {
          tt <- as.character(r$test_type[1])
          if (identical(tt, "simple")) {
            label <- paste0(r$col1[1], " vs ", r$col2[1])
          } else if (identical(tt, "pooled") && all(c("cols_left", "cols_right") %in% names(r))) {
            left <- gsub(",", ",", as.character(r$cols_left[1]), fixed = TRUE)
            right <- gsub(",", ",", as.character(r$cols_right[1]), fixed = TRUE)
            label <- paste0(left, " vs ", right)
          } else if (identical(tt, "interaction") && all(c("col3", "col4") %in% names(r))) {
            label <- paste0("(", r$col1[1], "-", r$col2[1], ")-(", r$col3[1], "-", r$col4[1], ")")
          } else {
            label <- as.character(r$group1[1])
          }
        } else if (all(c("group1", "group2") %in% names(r)) && nzchar(as.character(r$group2[1]))) {
          label <- paste0(as.character(r$group1[1]), " vs ", as.character(r$group2[1]))
        } else if ("group1" %in% names(r)) {
          label <- as.character(r$group1[1])
        } else {
          label <- paste0("test ", i)
        }
        
      is_interaction_row <- (("test_type" %in% names(r) && identical(as.character(r$test_type[1]), "interaction")) ||
        ("group1" %in% names(r) && grepl("^interaction\\(", as.character(r$group1[1]))))
      
        method <- if ("method" %in% names(r) && nzchar(as.character(r$method[1]))) as.character(r$method[1]) else method_fallback(r)
        df_txt <- if ("df" %in% names(r)) {
          if (isTRUE(is_interaction_row)) fmt_df_int(as.numeric(r$df[1])) else fmt_df(as.numeric(r$df[1]))
        } else {
          "NA"
        }
        t_txt <- fmt_t(as.numeric(r$t.value[1]))
        p_txt <- fmt_p(as.numeric(r$p.value[1]))
        
        any_welch <- any_welch || identical(method, "Welch t-test")
        any_interaction <- any_interaction || isTRUE(is_interaction_row)
        
        if (identical(method, "Welch t-test")) {
          lines <- c(lines, paste0(i, ") ", label, ": t(", df_txt, ")=", t_txt, ", p=", p_txt))
        } else {
          lines <- c(lines, paste0(i, ") ", label, ": ", method, ": t(", df_txt, ")=", t_txt, ", p=", p_txt))
        }
      }
      
      footer <- character(0)
      if (identical(tests, "auto")) {
        footer <- c(footer, "You can specify which statistical tests to report with the `tests` argument.")
      }
      msg2(paste0("tests:\n", paste(lines, collapse = "\n"), if (length(footer)) paste0("\n", paste(footer, collapse = "\n")) else ""), col = "gray")
    }

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
      values.align = values.align,
      values.round = values.round,
      tests = tests,
      means_comparisons = means_comparisons,
      pvalue.cex = pvalue.cex,
      pvalue.col = pvalue.col,
      buffer_top_effective = buffer_top_effective,
      ...
    )


  # 7. Prepare minimal output (returned invisibly)
    tests_out <- means_comparisons__out
    if (is.null(tests_out)) {
      tests_out <- "Use `tests` argument to request tests contrasting specific means"
    }
    out <- list(
      means = means,
      tests = tests_out
    )
  
  # 7.1 Optional export (plot is still shown on screen)
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
    # 7.1.1 Capture the current plot for replay on a file device.
    #   recordPlot() can fail on some devices; in that case we skip saving and keep the on-screen plot.
      p <- NULL
      p_err <- NULL
      p <- tryCatch(recordPlot(), error = function(e) { p_err <<- e; NULL })
      if (is.null(p)) {
        msg2("plot_means() says: could not record plot for export (skipping save): ", conditionMessage(p_err), col = "gray")
      } else {
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
        
      # 7.1.2 Normalize path for user-facing message (platform-independent separators).
        save_as_print <- save.as
        save_as_print <- tryCatch(normalizePath(save_as_print, winslash = "/", mustWork = FALSE), error = function(e) save_as_print)
        save_as_print <- gsub("\\\\", "/", save_as_print)
        msg2("plot_means() says: The figure was saved to `", save_as_print,"`" ,col = "gray")
      }
    }
    
    if (isTRUE(printed_any) && !isTRUE(quiet)) {
      msg2("\nSet `quiet=TRUE` to suppress this output.", col = "gray")
    }
    invisible(out)
}



#----------------------------------
# plot_means_validate: evaluate NSE inputs, validate args, and normalize to a single list `v` ----
plot_means_validate <- function(mc,
                                data,
                                order,
                                legend.title,
                                col,
                                col.text,
                                cluster,
                                values.cex,
                                values.align,
                                values.round,
                                tests,
                                pvalue.cex,
                                pvalue.col,
                                ci.level,
                                buffer.top,
                                save.as,
                                calling_env) {
  # 1. Resolve and validate formula input (NSE-safe)
  #   \"Resolve\" here means: take what the user typed for `formula` (which may be a name in the
  #   calling environment) and evaluate it into an actual `formula` object we can inspect and use.
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
  
  # 2. Determine grouping variables (up to 3)
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
  
  # 3. Validate display and testing options
  # 3.1 Validate label sizing argument
    if (!is.numeric(values.cex) || length(values.cex) != 1 || is.na(values.cex) || values.cex <= 0) {
      stop("plot_means(): 'values.cex' must be a single positive number", call. = FALSE)
    }
  
  # 3.2 Validate value label position
    if (!is.null(mc$values.pos) && !identical(mc$values.pos, quote(NULL))) {
      stop("plot_means(): `values.pos` was renamed to `values.align`", call. = FALSE)
    }
    values_align_effective <- match.arg(values.align, c("top", "middle", "bottom", "none"))
  
  # 3.3 Validate rounding argument for mean labels
    if (!is.numeric(values.round) || length(values.round) != 1 || is.na(values.round) || values.round < 0) {
      stop("plot_means(): 'values.round' must be a single non-negative number", call. = FALSE)
    }
    values.round <- as.integer(values.round)
  
  # 3.4 Validate tests argument
  #   Accept "auto", "none", or a custom comparison string like "3-1,4-2,(4-3)-(2-1)".
    if (is.character(tests) && length(tests) == 1 && !is.na(tests) && nzchar(tests)) {
      if (!(tests %in% c("auto", "none"))) {
        tests <- as.character(tests)
      } else {
        tests <- match.arg(tests, c("auto", "none"))
      }
    } else {
      stop("plot_means(): 'tests' must be a single string: \"auto\", \"none\", or custom comparisons like \"3-1, 4-2\"", call. = FALSE)
    }
  
  # 3.5 Validate p-value styling arguments
    if (!is.numeric(pvalue.cex) || length(pvalue.cex) != 1 || is.na(pvalue.cex) || pvalue.cex <= 0) {
      stop("plot_means(): 'pvalue.cex' must be a single positive number", call. = FALSE)
    }
    if (!is.character(pvalue.col) || length(pvalue.col) != 1 || is.na(pvalue.col) || !nzchar(pvalue.col)) {
      stop("plot_means(): 'pvalue.col' must be a single color name", call. = FALSE)
    }
  
  # 3.6 Validate ci.level argument (confidence interval level for ciL/ciH)
    if (!is.numeric(ci.level) || length(ci.level) != 1 || is.na(ci.level) || ci.level <= 0) {
      stop("plot_means(): 'ci.level' must be a single positive number (e.g., 95 or 0.95)", call. = FALSE)
    }
    if (ci.level < 1) ci.level <- ci.level * 100
    if (!is.finite(ci.level) || ci.level <= 0) {
      stop("plot_means(): 'ci.level' must be a single positive number (e.g., 95 or 0.95)", call. = FALSE)
    }
    if (ci.level >= 100) {
      stop("plot_means(): confidence can never be equal to or greater than 100%", call. = FALSE)
    }
  
  # 3.6 Validate buffer.top argument
    if (is.character(buffer.top) && length(buffer.top) == 1 && identical(buffer.top, "auto")) {
      buffer.top <- "auto"
    } else {
      if (!is.numeric(buffer.top) || length(buffer.top) != 1 || is.na(buffer.top) || buffer.top < 0) {
        stop("plot_means(): 'buffer.top' must be 'auto' or a single non-negative number (e.g., 0.3)", call. = FALSE)
      }
    }
  
  # 3.7 Validate save.as argument
    if (!is.null(save.as)) {
      if (!is.character(save.as) || length(save.as) != 1 || is.na(save.as) || !nzchar(save.as)) {
        stop("plot_means(): 'save.as' must be a single file path ('.png' or '.svg')", call. = FALSE)
      }
      
      extension <- tools::file_ext(save.as)
      if (!extension %in% c("svg", "png")) {
        stop("plot_means(): 'save.as' must be either a .png or .svg format.", call. = FALSE)
      }
    }
  
  # 4. Resolve cluster argument (optional; for lm2(..., clusters=))
  #   Accept both `cluster=` (documented) and `clusters=` (alias) in the user call.
  #   If both are provided, error to avoid ambiguity.
    cluster_vec <- NULL
    has_cluster_expr <- !is.null(mc$cluster) && !identical(mc$cluster, quote(NULL))
    has_clusters_expr <- !is.null(mc$clusters) && !identical(mc$clusters, quote(NULL))
    if (isTRUE(has_cluster_expr) && isTRUE(has_clusters_expr)) {
      stop("plot_means(): use only one of `cluster=` or `clusters=` (not both)", call. = FALSE)
    }
    
    cluster_expr <- if (isTRUE(has_cluster_expr)) mc$cluster else if (isTRUE(has_clusters_expr)) mc$clusters else NULL
    if (!is.null(cluster_expr)) {
      cluster_resolved <- evaluate_variable_arguments(
        arg_expr = cluster_expr,
        arg_name = if (isTRUE(has_clusters_expr)) "clusters" else "cluster",
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
    values.align = values_align_effective,
    values.round,
    tests,
    pvalue.cex,
    pvalue.col,
    ci.level,
    buffer.top,
    save.as,
    calling_env
  )
}



#----------------------------------
# plot_means_params: derive plotting parameters (levels/order, colors, legend layout, buffer.top) ----
plot_means_params <- function(v, result, result_plot) {
  get_levels <- function(var_name, fallback_values) {
    if (!is.null(v$data) && is.data.frame(v$data) && var_name %in% names(v$data) && is.factor(v$data[[var_name]])) {
      return(levels(v$data[[var_name]]))
    }
    vals <- fallback_values
    if (is.null(vals) || !length(vals)) return(character(0))
    vals_chr <- as.character(vals)
    is_numeric_like <- grepl("^\\s*-?\\d+(\\.\\d+)?\\s*$", vals_chr)
    if (all(is_numeric_like)) {
      return(as.character(sort(unique(as.numeric(trimws(vals_chr))))))
    }
    sort(unique(vals_chr))
  }
  
  x1_levels <- get_levels(v$x1_name, if (v$x1_name %in% names(result_plot)) result_plot[[v$x1_name]] else NULL)
  x2_levels <- if (is.null(v$x2_name)) "All" else get_levels(v$x2_name, if (v$x2_name %in% names(result_plot)) result_plot[[v$x2_name]] else NULL)
  x3_levels <- if (is.null(v$x3_name)) "All" else get_levels(v$x3_name, if (v$x3_name %in% names(result_plot)) result_plot[[v$x3_name]] else NULL)
  
  # Apply ordering to x1 only (controls bar order/colors)
  #   `order` is interpreted as an instruction for the *within-block* series (x1):
  #   it changes the left-to-right bar order and, by extension, the mapping of
  #   `col_vec[i]` to the i-th x1 level. We do not reorder x2/x3 here because
  #   those define block layout on the x-axis (reordering them would change the
  #   overall panel/block arrangement rather than the series inside each block).
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
    max_legend_chars <- max(nchar(as.character(x1_levels)))
    if (!is.finite(max_legend_chars)) max_legend_chars <- 0
    legend_horiz <- isTRUE(max_legend_chars <= 10)
  
  # Resolve buffer.top when set to 'auto'
    is_default_interaction_scenario <- length(v$x_names) == 2 &&
      length(x1_levels) == 2 &&
      !is.null(v$x2_name) &&
      length(x2_levels) == 2 &&
      is.null(v$x3_name)
    
    # Treat custom diff-in-diff requests like the default interaction scenario for auto buffer sizing.
    # This ensures identical tests produce identical headroom regardless of whether they are requested
    # via tests="auto" or an explicit string like "(4-3)-(2-1),2-1,4-3".
    tests_str <- if (is.character(v$tests) && length(v$tests) == 1 && !is.na(v$tests)) gsub("\\s+", "", v$tests) else ""
    has_interaction_test <- nzchar(tests_str) && grepl("\\([0-9]+-[0-9]+\\)-\\([0-9]+-[0-9]+\\)", tests_str)
    
    show_interaction <- (identical(v$tests, "auto") && is_default_interaction_scenario) || has_interaction_test
    
    buffer_top_effective <- if (identical(v$buffer.top, "auto")) {
      base <- if (show_interaction) 0.3 else 0.2
      if (!legend_horiz) base <- base + 0.05
      base
    } else {
      v$buffer.top
    }
  
  # Label disambiguation overlap set and formatter
  #   When x2 and/or x3 are present, the same label text can appear in more than
  #   one role (e.g., an x1 level "A" and an x2 level "A"). If we print raw levels
  #   everywhere, axis labels become ambiguous. We precompute the set of labels
  #   that overlap across x1/x2/x3 and, only for those, format as "var=level"
  #   (e.g., "x2=A") to make the plot self-explanatory without being verbose.
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



#----------------------------------
# Internal helpers used by multiple plot_means_* functions ----
plot_means_model_frame <- function(formula_eval, data, na.action, context = "model.frame") {
  mf <- tryCatch(
    model.frame(formula_eval, data = data, na.action = na.action),
    error = function(e) e
  )
  if (inherits(mf, "error")) {
    stop("plot_means(): failed to build model frame for ", context, ": ", conditionMessage(mf), call. = FALSE)
  }
  mf
}

plot_means_align_cluster <- function(v, mf) {
  if (is.null(v$cluster_vec)) return(NULL)
  if (length(v$cluster_vec) == nrow(mf)) return(v$cluster_vec)
  if (!is.null(v$data) && is.data.frame(v$data) && length(v$cluster_vec) == nrow(v$data)) {
    idx <- as.integer(rownames(mf))
    if (anyNA(idx)) idx <- seq_len(nrow(mf))
    return(v$cluster_vec[idx])
  }
  stop("plot_means(): 'cluster' length must match rows in 'data'", call. = FALSE)
}

plot_means_span <- function(y_min, y_max) {
  span <- (y_max - y_min)
  if (!is.finite(span) || span <= 0) span <- abs(y_max)
  if (!is.finite(span) || span <= 0) span <- 1
  span
}

plot_means_parse_tests <- function(tests_str) {
  if (!is.character(tests_str) || length(tests_str) != 1 || is.na(tests_str) || !nzchar(tests_str)) {
    stop("plot_means(): 'tests' must be a single non-empty string", call. = FALSE)
  }
  tests_str <- gsub("\\s+", "", tests_str)
  parts <- strsplit(tests_str, ",", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    stop("plot_means(): 'tests' is empty. Example: tests=\"3-1,4-2,(1+2)-(3+4),(4-3)-(2-1)\"", call. = FALSE)
  }
  
  parse_pair <- function(s) {
    m <- regexec("^([0-9]+)-([0-9]+)$", s)
    mm <- regmatches(s, m)[[1]]
    if (!length(mm)) return(NULL)
    a <- as.integer(mm[2])
    b <- as.integer(mm[3])
    if (!is.finite(a) || !is.finite(b) || a <= 0L || b <= 0L) return(NULL)
    list(a = a, b = b)
  }
  
  parse_sum <- function(s) {
    # one-or-more indices joined by '+', e.g. "1+3" or "2+1+4"
    if (!grepl("^[0-9]+(\\+[0-9]+)+$", s)) return(NULL)
    ss <- strsplit(s, "\\+", fixed = FALSE)[[1]]
    idx <- suppressWarnings(as.integer(ss))
    if (!length(idx) || any(!is.finite(idx)) || any(idx <= 0L)) return(NULL)
    idx
  }
  
  out <- vector("list", length(parts))
  for (i in seq_along(parts)) {
    p <- parts[i]
    # interaction: (a-b)-(c-d)
    m_int <- regexec("^\\(([0-9]+)-([0-9]+)\\)-\\(([0-9]+)-([0-9]+)\\)$", p)
    mm <- regmatches(p, m_int)[[1]]
    if (length(mm)) {
      a <- as.integer(mm[2]); b <- as.integer(mm[3]); c <- as.integer(mm[4]); d <- as.integer(mm[5])
      if (any(!is.finite(c(a, b, c, d))) || any(c(a, b, c, d) <= 0L)) {
        stop("plot_means(): invalid interaction test in 'tests': ", parts[i], call. = FALSE)
      }
      out[[i]] <- list(type = "interaction", a = a, b = b, c = c, d = d, expr = parts[i])
      next
    }
    # pooled: (a+b+...)-(c+d+...)
    m_pool <- regexec("^\\(([0-9\\+]+)\\)-\\(([0-9\\+]+)\\)$", p)
    mm <- regmatches(p, m_pool)[[1]]
    if (length(mm)) {
      left <- parse_sum(mm[2])
      right <- parse_sum(mm[3])
      if (is.null(left) || is.null(right)) {
        stop("plot_means(): invalid pooled test in 'tests': ", parts[i], call. = FALSE)
      }
      out[[i]] <- list(type = "pooled", left = left, right = right, expr = parts[i])
      next
    }
    pr <- parse_pair(p)
    if (!is.null(pr)) {
      out[[i]] <- list(type = "simple", a = pr$a, b = pr$b, expr = parts[i])
      next
    }
    stop("plot_means(): could not parse 'tests' entry: ", parts[i], ". Example: tests=\"3-1,4-2,(1+2)-(3+4),(4-3)-(2-1)\"", call. = FALSE)
  }
  out
}



#----------------------------------
# plot_means_compute: expand to a full x1/x2/x3 grid, compute CIs, and build bar/layout vectors ----
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
  #   Intent: preserve (x1,x2,x3) combinations with no observations so spacing and axes remain stable.
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
  
  # Confidence intervals for each plotted mean
  #   Default: per-cell t interval (one-sample t CI within each cell).
  #   Exceptions:
  #   - When clustering is used: regression-based CI via lm2(..., clusters=...)
  #   - 2x2 interaction scenario (tests="auto", x1/x2 both binary, no x3): regression-based CI
    ci_map <- data.frame(
      cell_key = character(0),
      lwr = numeric(0),
      upr = numeric(0),
      stringsAsFactors = FALSE
    )
    mf <- plot_means_model_frame(v$formula_eval, data = v$data, na.action = na.pass, context = "confidence intervals")
    if (nrow(mf) > 0) {
      df_m <- mf
      names(df_m)[1] <- ".__y"
      df_m[[x1_name]] <- as.character(df_m[[x1_name]])
      if (is.null(x2_name)) df_m$.x2 <- "All" else df_m[[x2_name]] <- as.character(df_m[[x2_name]])
      if (is.null(x3_name)) df_m$.x3 <- "All" else df_m[[x3_name]] <- as.character(df_m[[x3_name]])
      
      cluster_mf <- plot_means_align_cluster(v, df_m)
      if (!is.null(cluster_mf)) df_m$.__cluster <- cluster_mf
      
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

      # Determine when to use regression-based CIs for the bars.
        is_2x2_interaction_scenario <- identical(v$tests, "auto") &&
          length(v$x_names) == 2 &&
          length(params$x1_levels) == 2 &&
          !is.null(v$x2_name) &&
          length(params$x2_levels) == 2 &&
          is.null(v$x3_name)
        
        use_regression_ci <- !is.null(v$cluster_vec) || isTRUE(is_2x2_interaction_scenario)
        
        if (!isTRUE(use_regression_ci)) {
        # Per-cell t interval for the mean (one-sample t CI within each cell)
          levels_cf <- levels(df_m$cell_factor)
          lwr <- rep(NA_real_, length(levels_cf))
          upr <- rep(NA_real_, length(levels_cf))
          alpha <- 1 - ci_level
          for (j in seq_along(levels_cf)) {
            key <- levels_cf[j]
            yy <- df_m$.__y[df_m$cell_key == key]
            yy <- yy[is.finite(yy)]
            n <- length(yy)
            if (n < 2) next
            m <- mean(yy)
            s <- stats::sd(yy)
            if (!is.finite(m) || !is.finite(s) || s < 0) next
            tcrit <- stats::qt(1 - alpha / 2, df = n - 1)
            half <- tcrit * s / sqrt(n)
            lwr[j] <- m - half
            upr[j] <- m + half
          }
          
          ci_map <- data.frame(
            cell_key = levels_cf,
            lwr = lwr,
            upr = upr,
            stringsAsFactors = FALSE
          )
        }
        
        if (isTRUE(use_regression_ci)) {
        # Regression-based CI from lm2 coef/vcov (robust or clustered SE)
          fit <- if (is.null(v$cluster_vec)) {
            lm2(.__y ~ 0 + cell_factor, data = df_m)
          } else {
            lm2(.__y ~ 0 + cell_factor, data = df_m, clusters = df_m$.__cluster)
          }
          
          beta <- stats::coef(fit)
          V <- stats::vcov(fit)
          if (!is.null(beta) && length(beta) > 0 && is.matrix(V) && nrow(V) == length(beta)) {
            levels_cf <- levels(df_m$cell_factor)
            newdata <- data.frame(cell_factor = factor(levels_cf, levels = levels_cf))
            X <- stats::model.matrix(~ 0 + cell_factor, data = newdata)
            
            b_idx <- match(colnames(X), names(beta))
            if (anyNA(b_idx)) {
              X <- X[, !is.na(b_idx), drop = FALSE]
              b_idx <- b_idx[!is.na(b_idx)]
            }
            beta_use <- beta[b_idx]
            V_use <- V[b_idx, b_idx, drop = FALSE]
            
            fit_hat <- as.numeric(X %*% beta_use)
            se_hat <- sqrt(pmax(0, diag(X %*% V_use %*% t(X))))
            
            df_fit <- fit$df
            if (length(df_fit) != 1) df_fit <- suppressWarnings(min(df_fit, na.rm = TRUE))
            if (!is.finite(df_fit) || df_fit <= 0) df_fit <- 1
            
            alpha <- 1 - ci_level
            tcrit <- stats::qt(1 - alpha / 2, df = df_fit)
            lwr <- fit_hat - tcrit * se_hat
            upr <- fit_hat + tcrit * se_hat
            
            ci_map <- data.frame(
              cell_key = levels_cf,
              lwr = lwr,
              upr = upr,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  
  # Bar positions and block labels
  #   Intent: compute x-positions once and return vectors used by the draw step.
  #   Performance: use a pre-keyed `merged` lookup + pre-allocation to avoid O(n^2) scans and repeated vector growth.
    gap_x2 <- 1
    gap_x3 <- 2
    bar_width <- 1
    bar_step <- 1
    
    x2_col <- if (is.null(x2_name)) ".x2" else x2_name
    x3_col <- if (is.null(x3_name)) ".x3" else x3_name
    
    # Pre-key merged once by "x1|x2|x3" for O(1) lookups inside the loop
    merged$cell_key <- paste(
      as.character(merged[[x1_name]]),
      as.character(merged[[x2_col]]),
      as.character(merged[[x3_col]]),
      sep = "|"
    )
    rownames(merged) <- merged$cell_key
    
    n_x2 <- length(x2_levels)
    n_x3 <- length(x3_levels)
    n_blocks <- n_x2 * n_x3
    n_cells  <- k * n_blocks
    
    block_centers <- numeric(n_blocks)
    block_labels  <- character(n_blocks)
    x3_section_centers <- numeric(n_x3)
    x3_section_labels  <- character(n_x3)
    
    # Pre-allocate per-bar vectors to max possible size; trim after
    x_lefts        <- numeric(n_cells)
    x_rights       <- numeric(n_cells)
    x_centers_drawn <- numeric(n_cells)
    cell_keys_drawn <- character(n_cells)
    n_total_drawn   <- numeric(n_cells)
    n_missing_drawn <- numeric(n_cells)
    heights         <- numeric(n_cells)
    cols            <- character(n_cells)
    drawn_idx       <- 0L
    
    x_pos    <- 1
    blk_idx  <- 0L
    
    for (x3_idx in seq_along(x3_levels)) {
      x3_val   <- x3_levels[x3_idx]
      x3_start <- x_pos
      
      for (x2_idx in seq_along(x2_levels)) {
        x2_val      <- x2_levels[x2_idx]
        blk_idx     <- blk_idx + 1L
        centers_block <- numeric(k)
        
        for (i in seq_len(k)) {
          x1_val  <- x1_levels[i]
          key     <- paste(as.character(x1_val), as.character(x2_val), as.character(x3_val), sep = "|")
          row_sel <- merged[key, , drop = FALSE]
          
          mean_val  <- if (!is.na(row_sel$mean[1])) row_sel$mean[1] else NA_real_
          n_total   <- if ("n.total"   %in% names(row_sel)) row_sel$n.total[1]   else NA_real_
          n_missing <- if ("n.missing" %in% names(row_sel)) row_sel$n.missing[1] else NA_real_
          
          x_center        <- x_pos + (i - 1L) * bar_step
          centers_block[i] <- x_center
          
          if (!is.na(mean_val)) {
            drawn_idx <- drawn_idx + 1L
            cell_keys_drawn[drawn_idx] <- key
            x_centers_drawn[drawn_idx] <- x_center
            n_total_drawn[drawn_idx]   <- n_total
            n_missing_drawn[drawn_idx] <- n_missing
            x_lefts[drawn_idx]         <- x_center - bar_width / 2
            x_rights[drawn_idx]        <- x_center + bar_width / 2
            heights[drawn_idx]         <- mean_val
            cols[drawn_idx]            <- col[i]
          }
        }
        
        block_centers[blk_idx] <- mean(centers_block)
        block_labels[blk_idx]  <- if (is.null(x2_name)) "" else format_level_label(x2_name, x2_val)
        
        x_pos <- x_pos + k * bar_step
        if (x2_idx < n_x2) x_pos <- x_pos + gap_x2
      }
      
      x3_end <- x_pos - bar_step
      x3_section_centers[x3_idx] <- (x3_start + x3_end) / 2
      x3_section_labels[x3_idx]  <- if (is.null(x3_name)) "" else format_level_label(x3_name, x3_val)
      if (x3_idx < n_x3) x_pos <- x_pos + gap_x3
    }
    
    # Trim pre-allocated vectors to actually drawn bars
    if (drawn_idx < n_cells) {
      keep <- seq_len(drawn_idx)
      cell_keys_drawn <- cell_keys_drawn[keep]
      x_centers_drawn <- x_centers_drawn[keep]
      n_total_drawn   <- n_total_drawn[keep]
      n_missing_drawn <- n_missing_drawn[keep]
      x_lefts         <- x_lefts[keep]
      x_rights        <- x_rights[keep]
      heights         <- heights[keep]
      cols            <- cols[keep]
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



#----------------------------------
# plot_means_draw: render bars, CIs, labels, legend, and p-value annotations in base graphics ----
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
                            values.align,
                            values.round,
                            tests,
                            means_comparisons = NULL,
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
  # Establish plotting range.
  #   Bars are drawn from y=0 to `heights`, so we always include 0 in the y-limits.
  y_max_raw <- max(heights, na.rm = TRUE)
  if (!is.finite(y_max_raw)) y_max_raw <- 1
  y_max <- max(0, y_max_raw)
  y_min <- min(0, min(heights, na.rm = TRUE))
  if (!is.finite(y_min)) y_min <- 0
  
  if (nrow(ci_map) > 0) {
    lwr_f <- ci_map$lwr[is.finite(ci_map$lwr)]
    upr_f <- ci_map$upr[is.finite(ci_map$upr)]
    if (length(lwr_f)) y_min <- min(y_min, min(lwr_f))
    if (length(upr_f)) y_max <- max(y_max, max(upr_f), 0)
  }
  
  if (!"xlab" %in% names(dots)) dots$xlab <- ""
  
  # When x2 is present but x3 is not, show x2 name as x-axis label (centered)
    if (is.null(x3_name) && !is.null(x2_name) && nzchar(x2_name) && identical(dots$xlab, "")) {
      dots$xlab <- x2_name
    }
  if (!"ylab" %in% names(dots)) dots$ylab <- "Mean"
  if (!"main" %in% names(dots)) dots$main <- paste0("Means of ", y_name)
  
  y_span_data <- plot_means_span(y_min, y_max)

  # If everything is below 0, p-value brackets should be drawn "from below".
  #   Reserve extra bottom space so brackets/labels don't overlap bars (and so the
  #   interaction bracket, when present, isn't clipped at the bottom).
  all_below_zero <- (is.finite(y_max_raw) && y_max_raw <= 0)
  if (isTRUE(all_below_zero) && !identical(tests, "none") && !"ylim" %in% names(dots)) {
    is_interaction_scenario <- length(v$x_names) == 2 &&
      length(params$x1_levels) == 2 &&
      !is.null(v$x2_name) &&
      length(params$x2_levels) == 2 &&
      is.null(v$x3_name)
    extra_bottom <- if (isTRUE(is_interaction_scenario)) 0.28 else 0.18
    y_min <- y_min - extra_bottom * y_span_data
    y_span_data <- plot_means_span(y_min, y_max)
  }
  
  # Reserve a band below the lowest error whisker for n= labels.
  if (length(x_centers_drawn) && !"yaxt" %in% names(dots) && !"ylim" %in% names(dots)) {
    min_whisker <- y_min
    if (nrow(ci_map) > 0 && length(cell_keys_drawn) == length(x_centers_drawn)) {
      mi <- match(cell_keys_drawn, ci_map$cell_key)
      ok <- !is.na(mi)
      if (any(ok)) {
        lwr <- ci_map$lwr[mi[ok]]
        lwr <- lwr[is.finite(lwr)]
        if (length(lwr)) min_whisker <- min(min_whisker, min(lwr))
      }
    }
    # Minimal expansion: just enough room to place n= slightly below the lowest whisker.
    #   If all CIs are above 0, we still want n= below the bars (below 0), not inside them.
    min_anchor <- min(0, min_whisker)
    pad_n <- 0.04 * y_span_data
    y_n_target <- min_anchor - pad_n
    y_min_target <- y_n_target - pad_n
    y_min <- min(y_min, y_min_target)
    y_span_data <- plot_means_span(y_min, y_max)
  }

  # Extra headroom only when stacked p-value brackets require it.
  extra_top <- 0
  if (!identical(tests, "none") && !is.null(means_comparisons) && is.data.frame(means_comparisons) && nrow(means_comparisons) > 0) {
    assign_overlap_tier <- function(x0, x1) {
      n <- length(x0)
      if (!n) return(integer(0))
      left <- pmin(x0, x1)
      right <- pmax(x0, x1)
      ord <- order(left, right)
      tiers <- integer(n)
      tier_rights <- numeric(0)
      
      for (ii in ord) {
        placed <- FALSE
        if (length(tier_rights)) {
          for (t in seq_along(tier_rights)) {
            if (left[ii] > tier_rights[t]) {
              tiers[ii] <- t
              tier_rights[t] <- right[ii]
              placed <- TRUE
              break
            }
          }
        }
        if (!placed) {
          tiers[ii] <- length(tier_rights) + 1L
          tier_rights <- c(tier_rights, right[ii])
        }
      }
      tiers
    }
    
    if (all(c("col1", "col2") %in% names(means_comparisons))) {
      simple_rows <- means_comparisons[is.na(means_comparisons$col3) & is.finite(means_comparisons$p.value), , drop = FALSE]
      
      x0 <- rep(NA_real_, nrow(simple_rows))
      x1 <- rep(NA_real_, nrow(simple_rows))
      for (r in seq_len(nrow(simple_rows))) {
        is_pooled <- ("test_type" %in% names(simple_rows)) && identical(as.character(simple_rows$test_type[r]), "pooled")
        if (isTRUE(is_pooled) && all(c("cols_left", "cols_right") %in% names(simple_rows))) {
          left <- suppressWarnings(as.integer(strsplit(as.character(simple_rows$cols_left[r]), ",", fixed = TRUE)[[1]]))
          right <- suppressWarnings(as.integer(strsplit(as.character(simple_rows$cols_right[r]), ",", fixed = TRUE)[[1]]))
          idx <- c(left, right)
          idx <- idx[is.finite(idx)]
          if (length(idx)) {
            xx <- x_centers_drawn[idx]
            x0[r] <- min(xx, na.rm = TRUE)
            x1[r] <- max(xx, na.rm = TRUE)
          }
        } else {
          i1 <- as.integer(simple_rows$col1[r])
          i2 <- as.integer(simple_rows$col2[r])
          if (is.finite(i1) && is.finite(i2)) {
            x0[r] <- x_centers_drawn[i1]
            x1[r] <- x_centers_drawn[i2]
          }
        }
      }
    } else {
      simple_rows <- means_comparisons[is.character(means_comparisons$group2) & nzchar(means_comparisons$group2) & is.finite(means_comparisons$p.value), , drop = FALSE]
      # Build a temporary label -> bar index map (idx_by_label is created later for drawing).
      group_label_tmp <- function(x1, x2 = NULL, x3 = NULL) {
        parts <- character(0)
        if (!is.null(x3) && nzchar(x3)) parts <- c(parts, x3)
        if (!is.null(x2) && nzchar(x2)) parts <- c(parts, x2)
        parts <- c(parts, x1)
        paste(parts, collapse = "_")
      }
      key_parts <- strsplit(cell_keys_drawn, "\\|")
      x1_vals <- vapply(key_parts, function(z) z[1], character(1))
      x2_vals <- vapply(key_parts, function(z) z[2], character(1))
      x3_vals <- vapply(key_parts, function(z) z[3], character(1))
      x2_use <- if (is.null(v$x2_name)) rep(list(NULL), length(x1_vals)) else as.list(x2_vals)
      x3_use <- if (is.null(v$x3_name)) rep(list(NULL), length(x1_vals)) else as.list(x3_vals)
      bar_labels <- mapply(group_label_tmp, x1 = x1_vals, x2 = x2_use, x3 = x3_use, USE.NAMES = FALSE)
      idx_by_label_tmp <- seq_along(bar_labels)
      names(idx_by_label_tmp) <- bar_labels
      
      i1 <- idx_by_label_tmp[as.character(simple_rows$group1)]
      i2 <- idx_by_label_tmp[as.character(simple_rows$group2)]
      x0 <- x_centers_drawn[as.integer(i1)]
      x1 <- x_centers_drawn[as.integer(i2)]
    }
    ok <- is.finite(x0) & is.finite(x1)
    if (any(ok)) {
      tiers <- assign_overlap_tier(x0[ok], x1[ok])
      step <- 0.05 * y_span_data
      extra_top <- (max(tiers, na.rm = TRUE) - 1L) * step
      if (!is.finite(extra_top) || extra_top < 0) extra_top <- 0
    }
  }
  
  ylim_top <- y_max + buffer_top_effective * y_span_data + extra_top
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

  abline(h = 0, col = "gray80")
  
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
  
  if (nrow(ci_map) > 0 && length(x_centers_drawn) == length(cell_keys_drawn)) {
    eb_col <- "black"
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
        
        segments(x_ok, lwr, x_ok, upr, col = eb_col, lwd = 1)
        segments(x_ok - cap, lwr, x_ok + cap, lwr, col = eb_col, lwd = 1)
        segments(x_ok - cap, upr, x_ok + cap, upr, col = eb_col, lwd = 1)
      }
    }
  }
  
  if (!identical(tests, "none") && !is.null(means_comparisons) && is.data.frame(means_comparisons) && nrow(means_comparisons) > 0) {
    pvalue_line <- function(x0, x1, y, p, from_below = FALSE) {
      n <- max(length(x0), length(x1), length(y), length(p))
      x0 <- rep_len(x0, n)
      x1 <- rep_len(x1, n)
      y <- rep_len(y, n)
      p <- rep_len(p, n)
      
      y_span <- diff(par("usr")[3:4])
      tick <- 0.015 * y_span
      
      segments(x0, y, x1, y, col = pvalue.col)
      if (isTRUE(from_below)) {
        segments(x0, y, x0, y + tick, col = pvalue.col)
        segments(x1, y, x1, y + tick, col = pvalue.col)
      } else {
        segments(x0, y - tick, x0, y, col = pvalue.col)
        segments(x1, y - tick, x1, y, col = pvalue.col)
      }
      
      x_mid <- (x0 + x1) / 2
      for (i in seq_len(n)) {
        p_txt <- format_p_expr(p[i], digits = 3)
        if (!is.null(p_txt)) {
          y_txt <- if (isTRUE(from_below)) y[i] - 0.03 * y_span else y[i] + 0.03 * y_span
          text2(x_mid[i], y_txt, p_txt, bg = "white", cex = pvalue.cex, col = pvalue.col, pad = 0, pad_v = 0)
        }
      }
      
      invisible(NULL)
    }

    pvalue_line_pooled <- function(xL0, xL1, xR0, xR1, y, p, from_below = FALSE) {
      # Draw a two-level bracket for pooled comparisons:
      # - a small bracket over the pooled-left bars, and one over pooled-right bars (y_sub)
      # - a main bracket connecting the two pooled groups (y)
      y_span <- diff(par("usr")[3:4])
      sub_drop <- 0.03 * y_span
      y_sub <- if (isTRUE(from_below)) y + sub_drop else y - sub_drop
      
      # Small grouping lines for each pooled group (flat: no end ticks)
      segments(xL0, y_sub, xL1, y_sub, col = pvalue.col)
      segments(xR0, y_sub, xR1, y_sub, col = pvalue.col)
      
      # Connect pooled-group midpoints up to the main bracket
      xLm <- (xL0 + xL1) / 2
      xRm <- (xR0 + xR1) / 2
      if (isTRUE(from_below)) {
        segments(xLm, y_sub, xLm, y, col = pvalue.col)
        segments(xRm, y_sub, xRm, y, col = pvalue.col)
      } else {
        segments(xLm, y, xLm, y_sub, col = pvalue.col)
        segments(xRm, y, xRm, y_sub, col = pvalue.col)
      }
      
      # Main bracket + p-value label
      pvalue_line(x0 = xLm, x1 = xRm, y = y, p = p, from_below = from_below)
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
    
    # Map drawn bars to group labels used by plot_means_compute_pvalues().
    group_label <- function(x1, x2 = NULL, x3 = NULL) {
      parts <- character(0)
      if (!is.null(x3) && nzchar(x3)) parts <- c(parts, x3)
      if (!is.null(x2) && nzchar(x2)) parts <- c(parts, x2)
      parts <- c(parts, x1)
      paste(parts, collapse = "_")
    }
    
    if (!length(cell_keys_drawn)) return(invisible(NULL))
    key_parts <- strsplit(cell_keys_drawn, "\\|")
    x1_vals <- vapply(key_parts, function(z) z[1], character(1))
    x2_vals <- vapply(key_parts, function(z) z[2], character(1))
    x3_vals <- vapply(key_parts, function(z) z[3], character(1))
    x2_use <- if (is.null(v$x2_name)) rep(list(NULL), length(x1_vals)) else as.list(x2_vals)
    x3_use <- if (is.null(v$x3_name)) rep(list(NULL), length(x1_vals)) else as.list(x3_vals)
    bar_labels <- mapply(group_label, x1 = x1_vals, x2 = x2_use, x3 = x3_use, USE.NAMES = FALSE)
    idx_by_label <- seq_along(bar_labels)
    names(idx_by_label) <- bar_labels
    
    from_below <- (is.finite(max(heights, na.rm = TRUE)) && max(heights, na.rm = TRUE) <= 0)
    y_span <- diff(par("usr")[3:4])
    
    assign_overlap_tier <- function(x0, x1) {
      n <- length(x0)
      if (!n) return(integer(0))
      left <- pmin(x0, x1)
      right <- pmax(x0, x1)
      ord <- order(left, right)
      tiers <- integer(n)
      tier_rights <- numeric(0)
      
      for (ii in ord) {
        placed <- FALSE
        if (length(tier_rights)) {
          for (t in seq_along(tier_rights)) {
            if (left[ii] > tier_rights[t]) {
              tiers[ii] <- t
              tier_rights[t] <- right[ii]
              placed <- TRUE
              break
            }
          }
        }
        if (!placed) {
          tiers[ii] <- length(tier_rights) + 1L
          tier_rights <- c(tier_rights, right[ii])
        }
      }
      tiers
    }

    has_custom_cols <- all(c("col1", "col2") %in% names(means_comparisons))
    if (isTRUE(has_custom_cols)) {
      simple_rows <- means_comparisons[is.na(means_comparisons$col3) & is.finite(means_comparisons$p.value), , drop = FALSE]
      int_rows <- means_comparisons[is.finite(means_comparisons$col3) & is.finite(means_comparisons$col4) & is.finite(means_comparisons$p.value), , drop = FALSE]
      
      # 1) Compute a shared baseline y-position for all custom tests.
        involved_idx <- integer(0)
        if (nrow(simple_rows) > 0) {
          involved_idx <- unique(c(involved_idx, as.integer(simple_rows$col1), as.integer(simple_rows$col2)))
          if ("test_type" %in% names(simple_rows) && "cols_left" %in% names(simple_rows) && "cols_right" %in% names(simple_rows)) {
            pooled_idx <- which(as.character(simple_rows$test_type) == "pooled")
            if (length(pooled_idx)) {
              for (rr in pooled_idx) {
                left <- suppressWarnings(as.integer(strsplit(as.character(simple_rows$cols_left[rr]), ",", fixed = TRUE)[[1]]))
                right <- suppressWarnings(as.integer(strsplit(as.character(simple_rows$cols_right[rr]), ",", fixed = TRUE)[[1]]))
                involved_idx <- unique(c(involved_idx, left, right))
              }
            }
          }
        }
        if (nrow(int_rows) > 0) {
          involved_idx <- unique(c(involved_idx, as.integer(int_rows$col1), as.integer(int_rows$col2), as.integer(int_rows$col3), as.integer(int_rows$col4)))
        }
        involved_idx <- involved_idx[is.finite(involved_idx)]
        if (!length(involved_idx)) return(invisible(NULL))
        if (any(involved_idx < 1L) || any(involved_idx > length(x_centers_drawn))) {
          stop("plot_means(): invalid column index in custom tests (out of range)", call. = FALSE)
        }
        
        y_extreme <- if (isTRUE(from_below)) min(heights[involved_idx], na.rm = TRUE) else max(heights[involved_idx], na.rm = TRUE)
        if (nrow(ci_map) > 0) {
          k_all <- cell_keys_drawn[involved_idx]
          mi_all <- match(k_all, ci_map$cell_key)
          mi_all <- mi_all[!is.na(mi_all)]
          if (length(mi_all)) {
            if (isTRUE(from_below)) {
              y_extreme <- min(y_extreme, ci_map$lwr[mi_all], na.rm = TRUE)
            } else {
              y_extreme <- max(y_extreme, ci_map$upr[mi_all], na.rm = TRUE)
            }
          }
        }
        y_base <- if (isTRUE(from_below)) y_extreme - 0.02 * y_span else y_extreme + 0.02 * y_span
        
      # 2) Tier all custom tests together based on their x-spans (keep current x definitions).
        x0 <- rep(NA_real_, nrow(simple_rows))
        x1 <- rep(NA_real_, nrow(simple_rows))
        if (nrow(simple_rows) > 0) {
          for (r in seq_len(nrow(simple_rows))) {
            is_pooled <- ("test_type" %in% names(simple_rows)) && identical(as.character(simple_rows$test_type[r]), "pooled")
            if (isTRUE(is_pooled) && all(c("cols_left", "cols_right") %in% names(simple_rows))) {
              left <- suppressWarnings(as.integer(strsplit(as.character(simple_rows$cols_left[r]), ",", fixed = TRUE)[[1]]))
              right <- suppressWarnings(as.integer(strsplit(as.character(simple_rows$cols_right[r]), ",", fixed = TRUE)[[1]]))
              idx <- c(left, right)
              idx <- idx[is.finite(idx)]
              if (length(idx)) {
                xx <- x_centers_drawn[idx]
                x0[r] <- min(xx, na.rm = TRUE)
                x1[r] <- max(xx, na.rm = TRUE)
              }
            } else {
              i1 <- as.integer(simple_rows$col1[r])
              i2 <- as.integer(simple_rows$col2[r])
              if (is.finite(i1) && is.finite(i2)) {
                x0[r] <- x_centers_drawn[i1]
                x1[r] <- x_centers_drawn[i2]
              }
            }
          }
        }
        
        x0_int <- rep(NA_real_, nrow(int_rows))
        x1_int <- rep(NA_real_, nrow(int_rows))
        if (nrow(int_rows) > 0) {
          for (r in seq_len(nrow(int_rows))) {
            a <- as.integer(int_rows$col1[r]); b <- as.integer(int_rows$col2[r])
            c <- as.integer(int_rows$col3[r]); d <- as.integer(int_rows$col4[r])
            if (any(!is.finite(c(a, b, c, d)))) next
            x_ab <- mean(c(x_centers_drawn[a], x_centers_drawn[b]))
            x_cd <- mean(c(x_centers_drawn[c], x_centers_drawn[d]))
            x0_int[r] <- min(x_ab, x_cd, na.rm = TRUE)
            x1_int[r] <- max(x_ab, x_cd, na.rm = TRUE)
          }
        }
        
        x0_all <- c(x0, x0_int)
        x1_all <- c(x1, x1_int)
        tiers_all <- assign_overlap_tier(x0_all, x1_all)
        step <- 0.05 * y_span
        
      # 3) Draw simple/pooled tests at y determined solely by tier.
        if (nrow(simple_rows) > 0) {
          for (r in seq_len(nrow(simple_rows))) {
            off <- (tiers_all[r] - 1L) * step
            y <- if (isTRUE(from_below)) y_base - off else y_base + off
            if (!is.finite(x0[r]) || !is.finite(x1[r])) next
            
            is_pooled <- ("test_type" %in% names(simple_rows)) && identical(as.character(simple_rows$test_type[r]), "pooled")
            if (isTRUE(is_pooled) && all(c("cols_left", "cols_right") %in% names(simple_rows))) {
              left <- suppressWarnings(as.integer(strsplit(as.character(simple_rows$cols_left[r]), ",", fixed = TRUE)[[1]]))
              right <- suppressWarnings(as.integer(strsplit(as.character(simple_rows$cols_right[r]), ",", fixed = TRUE)[[1]]))
              left <- left[is.finite(left)]
              right <- right[is.finite(right)]
              if (!length(left) || !length(right)) next
              
              xL <- x_centers_drawn[left]
              xR <- x_centers_drawn[right]
              xL0 <- min(xL, na.rm = TRUE); xL1 <- max(xL, na.rm = TRUE)
              xR0 <- min(xR, na.rm = TRUE); xR1 <- max(xR, na.rm = TRUE)
              pvalue_line_pooled(xL0 = xL0, xL1 = xL1, xR0 = xR0, xR1 = xR1, y = y, p = simple_rows$p.value[r], from_below = from_below)
            } else {
              pvalue_line(x0 = x0[r], x1 = x1[r], y = y, p = simple_rows$p.value[r], from_below = from_below)
            }
          }
        }
      
      # 4) Draw interaction tests at y determined solely by tier (aligned with simple/pooled).
        if (nrow(int_rows) > 0) {
          n_simple <- nrow(simple_rows)
          for (r in seq_len(nrow(int_rows))) {
            a <- as.integer(int_rows$col1[r]); b <- as.integer(int_rows$col2[r])
            c <- as.integer(int_rows$col3[r]); d <- as.integer(int_rows$col4[r])
            if (any(!is.finite(c(a, b, c, d)))) next
            x_ab <- mean(c(x_centers_drawn[a], x_centers_drawn[b]))
            x_cd <- mean(c(x_centers_drawn[c], x_centers_drawn[d]))
            p_txt <- format_p_expr(int_rows$p.value[r], digits = 3)
            if (is.null(p_txt)) next
            
            tier_idx <- n_simple + r
            off <- (tiers_all[tier_idx] - 1L) * step
            y_anchor <- if (isTRUE(from_below)) y_base - off else y_base + off
            
            if (isTRUE(from_below)) {
              y_line <- y_anchor - 0.06 * y_span
              y_lab <- y_line - 0.025 * y_span
              y_line_from <- y_anchor - 0.045 * y_span
              segments(x_ab, y_line, x_ab, y_line_from, col = pvalue.col)
              segments(x_cd, y_line, x_cd, y_line_from, col = pvalue.col)
            } else {
              y_line <- y_anchor + 0.06 * y_span
              y_lab <- y_line + 0.025 * y_span
              y_line_from <- y_anchor + 0.045 * y_span
              segments(x_ab, y_line_from, x_ab, y_line, col = pvalue.col)
              segments(x_cd, y_line_from, x_cd, y_line, col = pvalue.col)
            }
            segments(x_ab, y_line, x_cd, y_line, col = pvalue.col)
            text2(mean(c(x_ab, x_cd)), y_lab, p_txt, bg = "white", cex = pvalue.cex, col = pvalue.col, pad = 0, pad_v = 0)
          }
        }
      
      invisible(NULL)
    }
    
    # Simple-effect comparisons: rows with a non-empty group2 and non-missing p.value
    simple_rows <- means_comparisons[is.character(means_comparisons$group2) & nzchar(means_comparisons$group2), , drop = FALSE]
    simple_rows <- simple_rows[is.finite(simple_rows$p.value), , drop = FALSE]
    if (nrow(simple_rows) > 0) {
      # Shared y-position: 2% beyond the most extreme CI among all involved bars
      involved <- unique(c(as.character(simple_rows$group1), as.character(simple_rows$group2)))
      idx_all <- idx_by_label[involved]
      idx_all <- idx_all[is.finite(idx_all)]
      y_simple <- NA_real_
      if (length(idx_all)) {
        y_extreme <- if (isTRUE(from_below)) min(heights[idx_all], na.rm = TRUE) else max(heights[idx_all], na.rm = TRUE)
        if (nrow(ci_map) > 0) {
          k_all <- cell_keys_drawn[idx_all]
          mi_all <- match(k_all, ci_map$cell_key)
          mi_all <- mi_all[!is.na(mi_all)]
          if (length(mi_all)) {
            if (isTRUE(from_below)) {
              y_extreme <- min(y_extreme, ci_map$lwr[mi_all], na.rm = TRUE)
            } else {
              y_extreme <- max(y_extreme, ci_map$upr[mi_all], na.rm = TRUE)
            }
          }
        }
        y_simple <- if (isTRUE(from_below)) y_extreme - 0.02 * y_span else y_extreme + 0.02 * y_span
      }
      
      if (is.finite(y_simple)) {
        x0 <- x_centers_drawn[as.integer(idx_by_label[as.character(simple_rows$group1)])]
        x1 <- x_centers_drawn[as.integer(idx_by_label[as.character(simple_rows$group2)])]
        tiers <- assign_overlap_tier(x0, x1)
        step <- 0.05 * y_span
        for (r in seq_len(nrow(simple_rows))) {
          i1 <- idx_by_label[as.character(simple_rows$group1[r])]
          i2 <- idx_by_label[as.character(simple_rows$group2[r])]
          if (!is.finite(i1) || !is.finite(i2)) next
          off <- (tiers[r] - 1L) * step
          y <- if (isTRUE(from_below)) y_simple - off else y_simple + off
          pvalue_line(x0 = x_centers_drawn[i1], x1 = x_centers_drawn[i2], y = y, p = simple_rows$p.value[r], from_below = from_below)
        }
      }
    }
    
    # Interaction comparison: a single row labeled interaction(x1:x2)
    int_rows <- means_comparisons[is.character(means_comparisons$group1) & grepl("^interaction\\(", means_comparisons$group1), , drop = FALSE]
    int_rows <- int_rows[is.finite(int_rows$p.value), , drop = FALSE]
    if (nrow(int_rows) > 0 && !is.null(v$x2_name) && length(params$x2_levels) == 2 && length(x1_levels) == 2) {
      p_int_txt <- format_p_expr(int_rows$p.value[1], digits = 3)
      if (!is.null(p_int_txt)) {
        x2_levels <- params$x2_levels
        x_mids <- rep(NA_real_, length(x2_levels))
        for (j in seq_along(x2_levels)) {
          g1 <- group_label(x1_levels[1], x2 = x2_levels[j])
          g2 <- group_label(x1_levels[2], x2 = x2_levels[j])
          i1 <- idx_by_label[g1]
          i2 <- idx_by_label[g2]
          if (!is.finite(i1) || !is.finite(i2)) next
          x_mids[j] <- mean(c(x_centers_drawn[i1], x_centers_drawn[i2]))
        }
        if (all(is.finite(x_mids))) {
          # Place interaction bracket offset from the simple-effect line, if present; otherwise anchor on CI extreme
          y_anchor <- if (exists("y_simple", inherits = FALSE) && is.finite(y_simple)) y_simple else {
            if (nrow(ci_map) > 0) {
              y_extreme <- if (isTRUE(from_below)) min(ci_map$lwr, na.rm = TRUE) else max(ci_map$upr, na.rm = TRUE)
              if (is.finite(y_extreme)) {
                if (isTRUE(from_below)) y_extreme - 0.02 * y_span else y_extreme + 0.02 * y_span
              } else {
                0
              }
            } else {
              0
            }
          }
          if (exists("tiers", inherits = FALSE) && length(tiers)) {
            max_off <- (max(tiers, na.rm = TRUE) - 1L) * (0.05 * y_span)
            y_anchor <- if (isTRUE(from_below)) y_anchor - max_off else y_anchor + max_off
          }
          if (isTRUE(from_below)) {
            y_line <- y_anchor - 0.06 * y_span
            y_lab <- y_line - 0.025 * y_span
            y_line_from <- y_anchor - 0.045 * y_span
            segments(x_mids[1], y_line, x_mids[1], y_line_from, col = pvalue.col)
            segments(x_mids[2], y_line, x_mids[2], y_line_from, col = pvalue.col)
          } else {
            y_line <- y_anchor + 0.06 * y_span
            y_lab <- y_line + 0.025 * y_span
            y_line_from <- y_anchor + 0.045 * y_span
            segments(x_mids[1], y_line_from, x_mids[1], y_line, col = pvalue.col)
            segments(x_mids[2], y_line_from, x_mids[2], y_line, col = pvalue.col)
          }
          segments(x_mids[1], y_line, x_mids[2], y_line, col = pvalue.col)
          text2(mean(x_mids), y_lab, p_int_txt, bg = "white", cex = pvalue.cex, col = pvalue.col, pad = 0, pad_v = 0)
        }
      }
    }
  }
  
  if (length(x_centers_drawn) > 0 && length(n_total_drawn) == length(x_centers_drawn)) {
    usr <- par("usr")
    y_span <- (usr[4] - usr[3])
    
    # Place sample sizes just below the lowest CI whisker (with a small pad), so we
    # don't have to create a large empty band under the plot.
    min_whisker <- 0
    if (nrow(ci_map) > 0 && length(cell_keys_drawn) == length(x_centers_drawn)) {
      mi <- match(cell_keys_drawn, ci_map$cell_key)
      ok <- !is.na(mi)
      if (any(ok)) {
        lwr <- ci_map$lwr[mi[ok]]
        lwr <- lwr[is.finite(lwr)]
        if (length(lwr)) min_whisker <- min(lwr, na.rm = TRUE)
      }
    }
    pad_n <- 0.04 * y_span
    min_anchor <- min(0, min_whisker)
    y_n <- min_anchor - pad_n
    y_n <- max(y_n, usr[3] + pad_n)
    
    n_vals <- ifelse(is.finite(n_total_drawn), n_total_drawn, NA)
    labels <- paste0("n=", n_vals)
    n_cex <- 0.9 * values.cex
    graphics::text(x_centers_drawn, y_n, labels, col = pvalue.col, cex = n_cex, adj = c(0.5, 0))
  }
  
  if (!identical(values.align, "none") && length(x_centers_drawn) > 0) {
    usr <- par("usr")
    pad_top <- 0.03 * (usr[4] - usr[3])
    pad_bot <- 0.06 * (usr[4] - usr[3])
    
    fmt <- formatC(heights, format = "f", digits = values.round)
    mean_labels <- if (identical(values.align, "bottom")) paste0("M=", fmt) else fmt
    
    neg <- is.finite(heights) & heights < 0
    y_mean <- if (identical(values.align, "top")) {
      # Place labels near the mean endpoint of each bar (not near the baseline at 0).
      # Positive bars end at `heights` above 0; negative bars end at `heights` below 0.
      ifelse(neg, heights + pad_top, heights - pad_top)
    } else if (identical(values.align, "middle")) {
      heights / 2
    } else if (identical(values.align, "bottom")) {
      # "bottom" still means the label is pulled toward the bar body, but anchored
      # near the mean endpoint rather than near 0 for negative bars.
      ifelse(neg, heights + pad_bot, heights - pad_bot)
    } else {
      rep(0, length(heights))
    }
    
    text2(
      x      = x_centers_drawn,
      y      = y_mean,
      labels = mean_labels,
      bg     = cols,
      cex    = values.cex,
      col    = text_cols,
      pad    = 0,
      pad_v  = 0
    )
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
    w <- strwidth(x1_levels, cex = 1.3)
    w <- w[is.finite(w)]
    tw <- if (length(w)) max(w) else 0
    gap_w <- strwidth("      ", cex = 1.3)
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



#----------------------------------
# plot_means_compute_pvalues: run the default inferential tests and return p-values for annotation ----
#   This is only used when `tests = "auto"`. It returns a small table with the exact
#   comparisons needed by the plotting code (group labels, means, diffs, and p-values).
plot_means_compute_pvalues <- function(v, params, mean_results, comp = NULL) {
  if (identical(v$tests, "none")) return(NULL)

  # Custom tests: parse and compute from bar indices (left-to-right draw order)
  if (!identical(v$tests, "auto")) {
    if (is.null(comp) || !is.list(comp) || is.null(comp$cell_keys_drawn)) {
      stop("plot_means(): internal error: custom 'tests' requires computed bar layout", call. = FALSE)
    }
    specs <- plot_means_parse_tests(v$tests)
    cell_keys_drawn <- comp$cell_keys_drawn
    n_cols <- length(cell_keys_drawn)
    
    all_idx <- unlist(lapply(specs, function(s) {
      if (identical(s$type, "pooled")) {
        c(s$left, s$right)
      } else {
        unlist(s[c("a", "b", "c", "d")], use.names = FALSE)
      }
    }), use.names = FALSE)
    all_idx <- all_idx[is.finite(all_idx)]
    if (any(all_idx < 1L) || any(all_idx > n_cols)) {
      stop("plot_means(): invalid column index in 'tests'. There are ", n_cols, " plotted columns.", call. = FALSE)
    }
    
    mf_tests <- plot_means_model_frame(v$formula_eval, data = v$data, na.action = na.omit, context = "custom tests")
    if (nrow(mf_tests) == 0) return(NULL)
    names(mf_tests)[1] <- ".__y"
    
    x1_name <- v$x1_name
    x2_name <- v$x2_name
    x3_name <- v$x3_name
    x2_col <- if (is.null(x2_name)) ".x2" else x2_name
    x3_col <- if (is.null(x3_name)) ".x3" else x3_name
    
    mf_tests[[x1_name]] <- as.character(mf_tests[[x1_name]])
    if (is.null(x2_name)) mf_tests$.x2 <- "All" else mf_tests[[x2_name]] <- as.character(mf_tests[[x2_name]])
    if (is.null(x3_name)) mf_tests$.x3 <- "All" else mf_tests[[x3_name]] <- as.character(mf_tests[[x3_name]])
    
    complete_mask <- !is.na(mf_tests$.__y) &
      !is.na(mf_tests[[x1_name]]) &
      !is.na(mf_tests[[x2_col]]) &
      !is.na(mf_tests[[x3_col]])
    mf_tests <- mf_tests[complete_mask, , drop = FALSE]
    if (nrow(mf_tests) == 0) return(NULL)
    
    mf_tests$cell_key <- paste(mf_tests[[x1_name]], mf_tests[[x2_col]], mf_tests[[x3_col]], sep = "|")
    cluster_mf <- plot_means_align_cluster(v, mf_tests)
    if (!is.null(cluster_mf)) mf_tests$.__cluster <- cluster_mf
    
    calc_simple <- function(a, b) {
      kA <- cell_keys_drawn[a]
      kB <- cell_keys_drawn[b]
      df_sub <- mf_tests[mf_tests$cell_key %in% c(kA, kB), c(".__y", "cell_key", if (!is.null(cluster_mf)) ".__cluster" else NULL), drop = FALSE]
      if (nrow(df_sub) == 0) stop("plot_means(): no data for columns ", a, " and ", b, call. = FALSE)
      df_sub$.__g <- factor(ifelse(df_sub$cell_key == kA, "A", "B"), levels = c("A", "B"))
      if (length(unique(df_sub$.__g)) < 2) stop("plot_means(): not enough data to compare columns ", a, " and ", b, call. = FALSE)
      
      if (is.null(cluster_mf)) {
        tt <- stats::t.test(.__y ~ .__g, data = df_sub, var.equal = FALSE)
        meanA <- as.numeric(tt$estimate[[1]])
        meanB <- as.numeric(tt$estimate[[2]])
        list(
          mean1 = meanA,
          mean2 = meanB,
          diff = meanB - meanA,
          # Align sign with diff (B - A). t.test reports t for (A - B).
          t.value = -as.numeric(tt$statistic[[1]]),
          p.value = as.numeric(tt$p.value),
          df = as.numeric(tt$parameter[[1]]),
          method = "Welch t-test"
        )
      } else {
        fit <- lm2(.__y ~ .__g, data = df_sub, clusters = df_sub$.__cluster)
        tab <- attr(fit, "statuser_table")
        if (is.null(tab) || !is.data.frame(tab) || !"term" %in% names(tab)) stop("plot_means(): failed to extract lm2 table for custom test", call. = FALSE)
        idx <- which(grepl("^\\.__g", as.character(tab$term)))
        if (!length(idx)) stop("plot_means(): failed to find group term for custom test", call. = FALSE)
        tr <- tab[idx[1], , drop = FALSE]
        meanA <- mean(df_sub$.__y[df_sub$.__g == "A"], na.rm = TRUE)
        meanB <- mean(df_sub$.__y[df_sub$.__g == "B"], na.rm = TRUE)
        list(
          mean1 = as.numeric(meanA),
          mean2 = as.numeric(meanB),
          diff = as.numeric(tr$estimate),
          t.value = as.numeric(tr$t),
          p.value = as.numeric(tr$p.value),
          df = if ("df" %in% names(tr)) as.numeric(tr$df) else NA_real_,
          method = "regression (clustered SE)"
        )
      }
    }
    
    calc_interaction <- function(a, b, c, d) {
      keys <- unique(cell_keys_drawn[c(a, b, c, d)])
      df_sub <- mf_tests[mf_tests$cell_key %in% keys, c(".__y", "cell_key", if (!is.null(cluster_mf)) ".__cluster" else NULL), drop = FALSE]
      if (nrow(df_sub) == 0) stop("plot_means(): no data for interaction test", call. = FALSE)
      df_sub$.__g <- factor(df_sub$cell_key, levels = keys)
      
      fit <- if (is.null(cluster_mf)) {
        lm2(.__y ~ 0 + .__g, data = df_sub)
      } else {
        lm2(.__y ~ 0 + .__g, data = df_sub, clusters = df_sub$.__cluster)
      }
      beta <- stats::coef(fit)
      V <- stats::vcov(fit)
      if (is.null(beta) || is.null(V)) stop("plot_means(): failed to extract coef/vcov for interaction test", call. = FALSE)
      
      coef_names <- names(beta)
      name_for <- function(key) {
        target <- paste0(".__g", key)
        idx <- match(target, coef_names)
        if (is.na(idx)) stop("plot_means(): missing coefficient for bar in interaction test", call. = FALSE)
        coef_names[idx]
      }
      nA <- name_for(cell_keys_drawn[a])
      nB <- name_for(cell_keys_drawn[b])
      nC <- name_for(cell_keys_drawn[c])
      nD <- name_for(cell_keys_drawn[d])
      
      L <- rep(0, length(beta))
      names(L) <- coef_names
      L[nA] <-  1
      L[nB] <- -1
      L[nC] <- -1
      L[nD] <-  1
      
      est <- sum(L * beta)
      se <- sqrt(as.numeric(t(L) %*% V %*% L))
      if (!is.finite(se) || se <= 0) stop("plot_means(): could not compute SE for interaction test", call. = FALSE)
      t_val <- est / se
      df_fit <- fit$df
      if (length(df_fit) != 1) df_fit <- suppressWarnings(min(df_fit, na.rm = TRUE))
      if (!is.finite(df_fit)) df_fit <- suppressWarnings(min(fit$df, na.rm = TRUE))
      if (!is.finite(df_fit)) df_fit <- 1
      p_val <- 2 * stats::pt(-abs(t_val), df = df_fit)
      list(
        diff = as.numeric(est),
        t.value = as.numeric(t_val),
        p.value = as.numeric(p_val),
        df = as.numeric(df_fit),
        method = if (is.null(cluster_mf)) "regression interaction" else "regression interaction (clustered SE)"
      )
    }
    
    calc_pooled <- function(left, right) {
      kL <- unique(cell_keys_drawn[left])
      kR <- unique(cell_keys_drawn[right])
      keys <- unique(c(kL, kR))
      df_sub <- mf_tests[mf_tests$cell_key %in% keys, c(".__y", "cell_key", if (!is.null(cluster_mf)) ".__cluster" else NULL), drop = FALSE]
      if (nrow(df_sub) == 0) stop("plot_means(): no data for pooled test", call. = FALSE)
      df_sub$.__g <- factor(ifelse(df_sub$cell_key %in% kL, "A", "B"), levels = c("A", "B"))
      if (length(unique(df_sub$.__g)) < 2) stop("plot_means(): not enough data for pooled test", call. = FALSE)
      
      if (is.null(cluster_mf)) {
        tt <- stats::t.test(.__y ~ .__g, data = df_sub, var.equal = FALSE)
        meanA <- as.numeric(tt$estimate[[1]])
        meanB <- as.numeric(tt$estimate[[2]])
        list(
          mean1 = meanA,
          mean2 = meanB,
          diff = meanB - meanA,
          # Align sign with diff (B - A). t.test reports t for (A - B).
          t.value = -as.numeric(tt$statistic[[1]]),
          p.value = as.numeric(tt$p.value),
          df = as.numeric(tt$parameter[[1]]),
          method = "Welch t-test"
        )
      } else {
        fit <- lm2(.__y ~ .__g, data = df_sub, clusters = df_sub$.__cluster)
        tab <- attr(fit, "statuser_table")
        if (is.null(tab) || !is.data.frame(tab) || !"term" %in% names(tab)) stop("plot_means(): failed to extract lm2 table for pooled test", call. = FALSE)
        idx <- which(grepl("^\\.__g", as.character(tab$term)))
        if (!length(idx)) stop("plot_means(): failed to find group term for pooled test", call. = FALSE)
        tr <- tab[idx[1], , drop = FALSE]
        meanA <- mean(df_sub$.__y[df_sub$.__g == "A"], na.rm = TRUE)
        meanB <- mean(df_sub$.__y[df_sub$.__g == "B"], na.rm = TRUE)
        list(
          mean1 = as.numeric(meanA),
          mean2 = as.numeric(meanB),
          diff = as.numeric(tr$estimate),
          t.value = as.numeric(tr$t),
          p.value = as.numeric(tr$p.value),
          df = if ("df" %in% names(tr)) as.numeric(tr$df) else NA_real_,
          method = "regression (clustered SE)"
        )
      }
    }
    
    rows <- data.frame(
      group1 = character(0),
      group2 = character(0),
      mean1 = numeric(0),
      mean2 = numeric(0),
      diff = numeric(0),
      t.value = numeric(0),
      p.value = numeric(0),
      df = numeric(0),
      method = character(0),
      test_type = character(0),
      cols_left = character(0),
      cols_right = character(0),
      col1 = integer(0),
      col2 = integer(0),
      col3 = integer(0),
      col4 = integer(0),
      stringsAsFactors = FALSE
    )
    
    for (s in specs) {
      if (identical(s$type, "simple")) {
        res <- calc_simple(s$a, s$b)
        rows <- rbind(rows, data.frame(
          group1 = paste0("col", s$a),
          group2 = paste0("col", s$b),
          mean1 = res$mean1,
          mean2 = res$mean2,
          diff = res$diff,
          t.value = res$t.value,
          p.value = res$p.value,
          df = res$df,
          method = res$method,
          test_type = "simple",
          cols_left = "",
          cols_right = "",
          col1 = s$a,
          col2 = s$b,
          col3 = NA_integer_,
          col4 = NA_integer_,
          stringsAsFactors = FALSE
        ))
      } else if (identical(s$type, "pooled")) {
        res <- calc_pooled(s$left, s$right)
        involved <- c(s$left, s$right)
        rows <- rbind(rows, data.frame(
          group1 = paste0("(", paste(s$left, collapse = "+"), ")-(", paste(s$right, collapse = "+"), ")"),
          group2 = "",
          mean1 = res$mean1,
          mean2 = res$mean2,
          diff = res$diff,
          t.value = res$t.value,
          p.value = res$p.value,
          df = res$df,
          method = res$method,
          test_type = "pooled",
          cols_left = paste(s$left, collapse = ","),
          cols_right = paste(s$right, collapse = ","),
          col1 = min(involved),
          col2 = max(involved),
          col3 = NA_integer_,
          col4 = NA_integer_,
          stringsAsFactors = FALSE
        ))
      } else if (identical(s$type, "interaction")) {
        res <- calc_interaction(s$a, s$b, s$c, s$d)
        m_a <- mean(mf_tests$.__y[mf_tests$cell_key == cell_keys_drawn[s$a]], na.rm = TRUE)
        m_b <- mean(mf_tests$.__y[mf_tests$cell_key == cell_keys_drawn[s$b]], na.rm = TRUE)
        m_c <- mean(mf_tests$.__y[mf_tests$cell_key == cell_keys_drawn[s$c]], na.rm = TRUE)
        m_d <- mean(mf_tests$.__y[mf_tests$cell_key == cell_keys_drawn[s$d]], na.rm = TRUE)
        d_first <- as.numeric(m_a - m_b)
        d_second <- as.numeric(m_c - m_d)
        rows <- rbind(rows, data.frame(
          group1 = paste0("interaction(", s$a, "-", s$b, ")-(", s$c, "-", s$d, ")"),
          group2 = "",
          mean1 = d_first,
          mean2 = d_second,
          diff = res$diff,
          t.value = res$t.value,
          p.value = res$p.value,
          df = res$df,
          method = res$method,
          test_type = "interaction",
          cols_left = "",
          cols_right = "",
          col1 = s$a,
          col2 = s$b,
          col3 = s$c,
          col4 = s$d,
          stringsAsFactors = FALSE
        ))
      }
    }
    return(rows)
  }
  
  # Pull the relevant term row from the lm2() summary table.
  #   Non-obvious: lm2() stores the coefficient table in an attribute ("statuser_table"),
  #   and interaction term naming can vary, so we match by pattern.
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
  
  # Build the model frame once, applying the same NA filtering policy as the tests.
  #   This keeps p-values consistent with the data actually available for inference.
  mf_tests <- plot_means_model_frame(v$formula_eval, data = v$data, na.action = na.omit, context = "plot_means_compute_pvalues()")
  if (nrow(mf_tests) == 0) return(NULL)
  
  names(mf_tests)[1] <- ".__y"
  mf_tests$.__x1 <- as.factor(mf_tests[[v$x1_name]])
  if (!is.null(v$x2_name)) mf_tests$.__x2 <- as.factor(mf_tests[[v$x2_name]])
  cluster_mf <- plot_means_align_cluster(v, mf_tests)
  if (!is.null(cluster_mf)) mf_tests$.__cluster <- cluster_mf
  
  # Fit helper that keeps the clustered and non-clustered paths in one place.
  #   Returns the fitted model (or NA) so callers can extract the term row they need.
  p_lm2_x1 <- function(df_sub) {
    if (nrow(df_sub) == 0) return(NA_real_)
    fit <- if (is.null(v$cluster_vec)) {
      lm2(.__y ~ .__x1, data = df_sub)
    } else {
      lm2(.__y ~ .__x1, data = df_sub, clusters = df_sub$.__cluster)
    }
    fit
  }

  ttest_x1 <- function(df_sub) {
    out <- list(t.value = NA_real_, p.value = NA_real_, df = NA_real_, method = "Welch t-test")
    if (nrow(df_sub) == 0) return(out)
    if (!(".__y" %in% names(df_sub)) || !(".__x1" %in% names(df_sub))) return(out)
    if (length(unique(df_sub$.__x1)) < 2) return(out)
    tt <- tryCatch(
      stats::t.test(.__y ~ .__x1, data = df_sub, var.equal = FALSE),
      error = function(e) NULL
    )
    if (is.null(tt)) return(out)
    out$t.value <- as.numeric(tt$statistic[[1]])
    out$p.value <- as.numeric(tt$p.value)
    out$df <- as.numeric(tt$parameter[[1]])
    out
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
  
  # Output table: one row per displayed comparison
  #   group1/group2 are plot labels (used to map to specific bars), and mean/diff are included
  #   so the draw step can position p-value brackets relative to the bars.
  rows <- data.frame(
    group1 = character(0),
    group2 = character(0),
    mean1 = numeric(0),
    mean2 = numeric(0),
    diff = numeric(0),
    t.value = numeric(0),
    p.value = numeric(0),
    df = numeric(0),
    method = character(0),
    stringsAsFactors = FALSE
  )
  
  # Scenario 1: x1 only, binary
    if (length(v$x_names) == 1 && x1_is_binary) {
      df_sub <- mf_tests[, c(".__y", ".__x1"), drop = FALSE]
      if (is.null(v$cluster_vec)) {
        tt <- ttest_x1(df_sub)
        t_val <- tt$t.value
        p_val <- tt$p.value
        df_val <- tt$df
        method <- tt$method
      } else {
        fit <- p_lm2_x1(df_sub)
        tr <- get_term_row(fit, ".__x1")
        t_val <- if (is.null(tr)) NA_real_ else as.numeric(tr$t)
        p_val <- if (is.null(tr)) NA_real_ else as.numeric(tr$p.value)
        df_val <- if (is.null(tr) || !"df" %in% names(tr)) NA_real_ else as.numeric(tr$df)
        method <- "regression (clustered SE)"
      }
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
        t.value = t_val,
        p.value = p_val,
        df = df_val,
        method = method,
        stringsAsFactors = FALSE
      ))
      return(rows)
    }
  
  # Scenario 2: x1 and x2, both binary, no x3
    if (length(v$x_names) == 2 && x1_is_binary && x2_is_binary && x3_is_null) {
      for (x2v in x2_levels) {
        df_sub <- mf_tests[mf_tests$.__x2 == x2v, c(".__y", ".__x1"), drop = FALSE]
        if (is.null(v$cluster_vec)) {
          tt <- ttest_x1(df_sub)
          t_val <- tt$t.value
          p_val <- tt$p.value
          df_val <- tt$df
          method <- tt$method
        } else {
          fit <- p_lm2_x1(df_sub)
          tr <- get_term_row(fit, ".__x1")
          t_val <- if (is.null(tr)) NA_real_ else as.numeric(tr$t)
          p_val <- if (is.null(tr)) NA_real_ else as.numeric(tr$p.value)
          df_val <- if (is.null(tr) || !"df" %in% names(tr)) NA_real_ else as.numeric(tr$df)
          method <- "regression (clustered SE)"
        }
        
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
          t.value = t_val,
          p.value = p_val,
          df = df_val,
          method = method,
          stringsAsFactors = FALSE
        ))
      }
      
      # Interaction: always use lm2(), even when cluster is NULL
      fit_int <- if (is.null(v$cluster_vec)) {
        lm2(.__y ~ .__x1 * .__x2, data = mf_tests[, c(".__y", ".__x1", ".__x2"), drop = FALSE])
      } else {
        lm2(.__y ~ .__x1 * .__x2, data = mf_tests[, c(".__y", ".__x1", ".__x2", ".__cluster"), drop = FALSE], clusters = mf_tests$.__cluster)
      }
      tr_int <- get_term_row(fit_int, ".__x1", ".__x2", want_interaction = TRUE)
      if (!is.null(tr_int)) {
        d_first <- mean_lookup(x1_levels[2], x2 = x2_levels[1]) - mean_lookup(x1_levels[1], x2 = x2_levels[1])
        d_second <- mean_lookup(x1_levels[2], x2 = x2_levels[2]) - mean_lookup(x1_levels[1], x2 = x2_levels[2])
        rows <- rbind(rows, data.frame(
          group1 = paste0("interaction(", v$x1_name, ":", v$x2_name, ")"),
          group2 = "",
          mean1 = as.numeric(d_first),
          mean2 = as.numeric(d_second),
          diff = as.numeric(tr_int$estimate),
          t.value = as.numeric(tr_int$t),
          p.value = as.numeric(tr_int$p.value),
          df = if ("df" %in% names(tr_int)) as.numeric(tr_int$df) else NA_real_,
          method = if (is.null(v$cluster_vec)) "regression interaction" else "regression interaction (clustered SE)",
          stringsAsFactors = FALSE
        ))
      }
      
      return(rows)
    }
  
  # Scenario 3: x1 binary, x2 has >2 levels, no x3
    if (length(v$x_names) == 2 && x1_is_binary && !is.null(v$x2_name) && length(x2_levels) > 2 && x3_is_null) {
      for (x2v in x2_levels) {
        df_sub <- mf_tests[mf_tests$.__x2 == x2v, c(".__y", ".__x1"), drop = FALSE]
        if (is.null(v$cluster_vec)) {
          tt <- ttest_x1(df_sub)
          t_val <- tt$t.value
          p_val <- tt$p.value
          df_val <- tt$df
          method <- tt$method
        } else {
          fit <- p_lm2_x1(df_sub)
          tr <- get_term_row(fit, ".__x1")
          t_val <- if (is.null(tr)) NA_real_ else as.numeric(tr$t)
          p_val <- if (is.null(tr)) NA_real_ else as.numeric(tr$p.value)
          df_val <- if (is.null(tr) || !"df" %in% names(tr)) NA_real_ else as.numeric(tr$df)
          method <- "regression (clustered SE)"
        }
        
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
          t.value = t_val,
          p.value = p_val,
          df = df_val,
          method = method,
          stringsAsFactors = FALSE
        ))
      }
      
      return(rows)
    }
  
  NULL
}

