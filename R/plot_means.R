#' Plot means with 95% CI error bars (barplot)
#'
#' Runs \code{desc_var()} on the formula/data to get mean and SE per group,
#' then draws a barplot with 95% CI error bars extending upward from each bar.
#' Uses \code{get.colors()} for bar colors (same as other plot functions).
#'
#' @param formula Formula of the form \code{y ~ x} (one grouping variable) or
#'   \code{y ~ x1 + x2} (two grouping variables). For a single mean use \code{y ~ 1}.
#' @param data Optional data frame containing the variables in the formula.
#'   If \code{NULL}, variables are taken from the environment of the formula.
#'
#' @return Invisibly returns the \code{desc_var} result data frame with an
#'   added column \code{ci_upper} (mean + half-width of 95% CI).
#'
#' @details
#' Standard error comes from \code{desc_var} (\code{sd / sqrt(n)}). The 95% CI
#' half-width is \code{qt(0.975, n - 1) * se} with \code{n} the non-missing count.
#' Error bars are drawn only for groups with \code{n >= 2}. For two grouping
#' variables, bars are drawn with \code{beside = TRUE} (first factor = groups
#' of bars, second = legend).
#'
#' @examples
#' # One factor
#' d <- data.frame(condition = rep(c("A", "B"), each = 25),
#'                 dv = c(rnorm(25, 10, 2), rnorm(25, 14, 2)))
#' plot_means(dv ~ condition, data = d)
#'
#' # Two factors (2x2)
#' d2 <- expand.grid(x1 = c("low", "high"), x2 = c("low", "high"))
#' d2 <- d2[rep(seq_len(nrow(d2)), each = 25), ]
#' d2$dv <- rnorm(100, mean = rep(c(8, 10, 11, 15), each = 25), sd = 1.2)
#' plot_means(dv ~ x1 + x2, data = d2)
#'
#' @importFrom graphics barplot
#' @export
plot_means <- function(formula, data = NULL) {
  if (!inherits(formula, "formula")) {
    message2("plot_means() says: first argument must be a formula (e.g. y ~ x)", col = "red", stop = TRUE)
  }
  if (!is.null(data) && !is.data.frame(data)) {
    message2("plot_means() says: 'data' must be a data frame or NULL", col = "red", stop = TRUE)
  }

  # Same inputs as desc_var: run desc_var to get mean, se, n per group
  d <- desc_var(formula, data = data)
  # Strip class so we can add column and treat as plain data frame
  class(d) <- setdiff(class(d), "desc_var")

  n_valid <- d$n.total - d$n.missing
  df_val <- n_valid - 1L
  df_val[n_valid < 2L] <- NA_integer_
  # 95% CI half-width = t * se (t from qt(0.975, n-1))
  ci_half <- ifelse(n_valid < 2L, NA_real_, stats::qt(0.975, df_val) * d$se)
  d$ci_upper <- d$mean + ci_half

  n_groups <- nrow(d)
  cols <- get.colors(n_groups)

  # One factor (or no factor): "group" column present
  if ("group" %in% names(d)) {
    means <- setNames(d$mean, d$group)
    y_max <- max(d$ci_upper, na.rm = TRUE)
    if (!is.finite(y_max)) y_max <- max(d$mean, na.rm = TRUE)
    bp <- graphics::barplot(
      means,
      ylim = c(0, y_max * 1.05),
      names.arg = d$group,
      col = cols,
      border = cols
    )
    draw_ci <- n_valid >= 2L & is.finite(ci_half)
    if (any(draw_ci)) {
      graphics::arrows(
        x0 = bp[draw_ci],
        y0 = d$mean[draw_ci],
        y1 = d$ci_upper[draw_ci],
        angle = 90,
        code = 2L,
        length = 0.1
      )
    }
  } else {
    # Two factors: columns before "mean" are the grouping variables
    idx_mean <- which(names(d) == "mean")[1L]
    n_group_cols <- idx_mean - 1L
    if (n_group_cols < 2L) {
      message2("plot_means() says: desc_var result has no 'group' and fewer than 2 grouping columns", col = "red", stop = TRUE)
    }
    if (n_group_cols > 2L) {
      message2("plot_means() says: only one or two grouping variables are supported", col = "red", stop = TRUE)
    }
    f1_name <- names(d)[1L]
    f2_name <- names(d)[2L]
    means_mat <- reshape(
      d[, c(f1_name, f2_name, "mean")],
      idvar = f1_name,
      timevar = f2_name,
      direction = "wide"
    )
    rownames(means_mat) <- means_mat[[f1_name]]
    means_mat <- as.matrix(means_mat[, -1L, drop = FALSE])
    colnames(means_mat) <- sub("^mean\\.", "", colnames(means_mat))

    ci_mat <- reshape(
      d[, c(f1_name, f2_name, "ci_upper")],
      idvar = f1_name,
      timevar = f2_name,
      direction = "wide"
    )
    ci_mat <- as.matrix(ci_mat[, -1L, drop = FALSE])
    colnames(ci_mat) <- sub("^ci_upper\\.", "", colnames(ci_mat))

    n_valid_mat <- matrix(n_valid, nrow = nrow(means_mat), ncol = ncol(means_mat), byrow = TRUE)

    # Same color within each set of bars: one color per level of second factor (legend)
    n_legend_levels <- ncol(means_mat)
    cols <- rep(get.colors(n_legend_levels), nrow(means_mat))

    y_max <- max(ci_mat, na.rm = TRUE)
    if (!is.finite(y_max)) y_max <- max(means_mat, na.rm = TRUE)
    # Reserve 25% space at top for legend (like plot_freq)
    old_mar <- par("mar")
    on.exit(par(mar = old_mar), add = TRUE)
    if (old_mar[3] < 5) par(mar = c(old_mar[1], old_mar[2], 5, old_mar[4]))
    bp <- graphics::barplot(
      means_mat,
      beside = TRUE,
      legend.text = TRUE,
      ylim = c(0, y_max * 1.25),
      col = cols,
      border = cols
    )
    draw_ci <- n_valid_mat >= 2L & is.finite(ci_mat - means_mat)
    if (any(draw_ci)) {
      graphics::arrows(
        x0 = bp[draw_ci],
        y0 = means_mat[draw_ci],
        y1 = ci_mat[draw_ci],
        angle = 90,
        code = 2L,
        length = 0.1
      )
    }
  }

  invisible(d)
}
