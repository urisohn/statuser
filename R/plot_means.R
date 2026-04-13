#' Plot means (skeleton)
#'
#' Computes the means (and related descriptives) that will be used for a future
#' barplot of group means. For now, this function simply returns the output of
#' \code{\link{desc_var}} for the supplied formula.
#'
#' @param formula A formula like \code{y ~ x} or \code{y ~ x1 + x2}. The left-hand
#'   side (y) must be numeric. The right-hand side contains one or more grouping
#'   variables. (Formula-only for now.)
#' @param data An optional data frame containing the variables in the formula.
#' @param order Controls the order of groups in the returned table when a single
#'   grouping variable is used. Use \code{-1} to reverse the default order, or
#'   provide a character vector with the desired order. Ignored when multiple
#'   grouping variables are used (i.e., when \code{desc_var()} returns separate
#'   grouping columns rather than a single \code{group} column).
#' @param add Logical. Reserved for future plotting (currently unused).
#' @param legend.title Character. Reserved for future plotting (currently unused).
#' @param col Reserved for future plotting (currently unused). Default \code{NULL}.
#' @param col.text Reserved for future plotting (currently unused). Default \code{NULL}.
#' @param cluster Reserved for future clustering support (currently unused). Default \code{NULL}.
#' @param ... Reserved for future plotting parameters (currently unused).
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

  #3. Apply ordering when there is a single group column
    if (!is.null(order) && "group" %in% names(result)) {
      # Handle order = -1 as reverse of current order
        if (length(order) == 1 && is.numeric(order) && order == -1) {
          result <- result[rev(seq_len(nrow(result))), , drop = FALSE]
        } else {
          # User provided an explicit order
            groups <- as.character(result$group)
            unique_groups <- unique(groups)

            missing_groups <- setdiff(unique_groups, order)
            extra_groups <- setdiff(order, unique_groups)

            if (length(missing_groups) > 0) {
              stop(
                sprintf(
                  "plot_means(): 'order' is missing group(s): %s",
                  paste(missing_groups, collapse = ", ")
                ),
                call. = FALSE
              )
            }
            if (length(extra_groups) > 0) {
              warning(
                sprintf(
                  "plot_means(): 'order' contains group(s) not in data: %s",
                  paste(extra_groups, collapse = ", ")
                ),
                call. = FALSE
              )
            }

            order_use <- order[order %in% unique_groups]
            row_idx <- match(order_use, groups)
            result <- result[row_idx, , drop = FALSE]
        }
    }

  #4. Return (visible) result table for now
    result
}

