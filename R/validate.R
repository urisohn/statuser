# =============================================================================
# validate.R overview
# =============================================================================
# 1) Formula + NSE hygiene
#    - Validate formula inputs early and provide helpful errors.
#    - Avoid common NSE traps where `all.vars()` and `deparse()` can mislead.
# 2) `$` formulas (df$y ~ df$x)
#    - Detect `$` inside formulas.
#    - Enforce a package policy: **do not** allow `$` formulas together with `data=`.
#    - When `data=` is absent, normalize `$` formulas into an internal `(formula, data)`
#      via `model.frame()` so downstream code behaves like standard `y ~ x, data=df`.
# 3) Shared validators used across exported functions
#    - `validate_formula()`: lightweight early validation used by many functions.
#    - `validate_plot()`, `validate_table2()`, `validate_t.test2()`, `validate_lm2()`:
#      argument validation + safe evaluation patterns shared across the package.
#
# =============================================================================
# Formula evaluation helpers
# =============================================================================
# Many user-facing functions accept formulas, and users often write either:
#   - `y ~ x` with `data = df`
#   - `df$y ~ df$x` with no `data=`
#
# Base helpers like `all.vars()` treat column names in `$` calls (the RHS symbol
# of `df$col`) as if they were free variables. This is correct for “symbol-only”
# evaluation, but it breaks validation / data-construction when we mistakenly
# require `col` to exist as a binding in an environment.
#
# The helpers below do two things:
#   1) Decide which symbols must truly exist as bindings (env variables) vs.
#      which are “just column names” inside `$`.
#   2) For `$`-formulas, normalize them into a clean `(formula, data)` pair
#      using `model.frame()`, so downstream code can operate like `y ~ x` + data.
#' @noRd
.collect_dollar_rhs_symbols <- function(expr) {
  acc <- character()
  walk <- function(e) {
    if (is.call(e)) {
      op <- e[[1L]]
      if (identical(op, quote(`$`)) || identical(op, as.name("$"))) {
        if (length(e) >= 3L) {
          rh <- e[[3L]]
          if (is.symbol(rh) || is.name(rh)) {
            acc <<- c(acc, as.character(rh))
          }
        }
      }
      if (length(e) >= 2L) {
        for (i in 2L:length(e)) walk(e[[i]])
      }
    }
  }
  walk(expr)
  unique(acc)
}

#' @noRd
formula_symbols_required_in_env <- function(formula) {
  av <- all.vars(formula)
  setdiff(av, .collect_dollar_rhs_symbols(formula))
}

.extract_formula_side_label <- function(expr) {
  # Produce a human-friendly label for plotting/tables.
  # For `$` expressions, we strip the data prefix: `df$y` -> `y`.
  if (is.symbol(expr) || is.name(expr)) {
    return(as.character(expr))
  }
  es <- deparse(expr, width.cutoff = 500L)
  if (grepl("\\$", es)) {
    return(trimws(sub(".*\\$", "", es)))
  }
  trimws(es)
}

#' @noRd
.expr_contains_dollar <- function(expr) {
  # Walk an expression tree and return TRUE if it contains a `$` call.
  found <- FALSE
  walk <- function(e) {
    if (isTRUE(found)) return(invisible(NULL))
    if (is.call(e)) {
      op <- e[[1L]]
      if (identical(op, quote(`$`)) || identical(op, as.name("$"))) {
        found <<- TRUE
        return(invisible(NULL))
      }
      if (length(e) >= 2L) {
        for (i in 2L:length(e)) walk(e[[i]])
      }
    }
    invisible(NULL)
  }
  walk(expr)
  isTRUE(found)
}

#' @noRd
.formula_contains_dollar <- function(formula) {
  # TRUE when either side of the formula uses `$` (e.g., df$y ~ df$x).
  tryCatch({
    if (!inherits(formula, "formula")) return(FALSE)
    if (length(formula) >= 2L && .expr_contains_dollar(formula[[2L]])) return(TRUE)
    if (length(formula) >= 3L && .expr_contains_dollar(formula[[3L]])) return(TRUE)
    FALSE
  }, error = function(e) FALSE)
}

#' @noRd
.assert_no_dollar_with_data <- function(formula, data, func_name = "function") {
  # Package policy: users should choose *one* evaluation style.
  # If they use `$` in the formula, we forbid also passing `data=`, because it is
  # ambiguous and can mask mistakes (is `$` pointing at the same data as `data=`?).
  if (!is.null(data) && .formula_contains_dollar(formula)) {
    stop(
      sprintf(
        "%s(): Do not combine `$` formulas with `data=`.\nUse either `%s(y ~ x, data = df)` or `%s(df$y ~ df$x)`.",
        func_name, func_name, func_name
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' @noRd
.normalize_dollar_formula_to_data_and_formula <- function(formula, calling_env, func_name = "function") {
  # Convert `df$y ~ df$x` into:
  #   - a temporary data.frame containing the evaluated vectors
  #   - a rewritten formula `y ~ x` that refers to those temp columns
  #
  # This lets downstream code use standard formula workflows (`model.frame`,
  # `terms`, `reformulate`, etc.) without special-casing `$` everywhere.
  if (!inherits(formula, "formula") || !.formula_contains_dollar(formula)) {
    return(list(formula = formula, data = NULL, mapping = NULL))
  }

  # Determine evaluation environment (prefer formula env if present).
  formula_env <- environment(formula)
  if (is.null(formula_env)) formula_env <- calling_env

  # Evaluate into a model.frame to respect formula semantics and NA handling.
  # NOTE: we pass `data = <env>` so expressions like `df$y` can resolve `df`
  # from the environment (no data.frame needed at this stage).
  mf <- tryCatch(
    stats::model.frame(formula, data = formula_env, na.action = stats::na.pass),
    error = function(e) {
      stop(sprintf("%s(): %s", func_name, e$message), call. = FALSE)
    }
  )
  if (!is.data.frame(mf) || ncol(mf) < 1) {
    stop(sprintf("%s(): Could not evaluate formula", func_name), call. = FALSE)
  }

  # Build clean, syntactic column names for the evaluated vectors.
  # We prefer labels like `y`/`x` (not `df$y`/`df$x`) and make them syntactic so
  # `reformulate()` and data.frame access are safe.
  response_expr <- formula[[2L]]
  response_label <- .extract_formula_side_label(response_expr)
  response_name <- make.names(response_label)
  if (!nzchar(response_name)) response_name <- "y"

  # `term.labels` reflects the RHS terms; for `$` calls this will include `df$x`
  # as a term label string, which we strip for display/column naming.
  predictor_terms <- attr(stats::terms(formula), "term.labels")
  if (length(predictor_terms) == 0) {
    stop(sprintf("%s(): Formula must include at least one predictor", func_name), call. = FALSE)
  }
  predictor_labels <- vapply(
    predictor_terms,
    function(s) {
      s2 <- trimws(s)
      if (grepl("\\$", s2)) sub(".*\\$", "", s2) else s2
    },
    character(1)
  )
  predictor_names <- make.names(predictor_labels)
  predictor_names[predictor_names == ""] <- paste0("x", which(predictor_names == ""))
  predictor_names <- make.unique(predictor_names, sep = "_")

  # Rename model.frame columns and rebuild a formula that no longer needs `df$...`.
  # `model.frame()` returns response first, then predictors in formula order.
  new_names <- c(response_name, predictor_names)
  if (ncol(mf) != length(new_names)) {
    # Defensive fallback: keep original mf names but make them syntactic.
    new_names <- make.unique(make.names(names(mf)), sep = "_")
  }
  names(mf) <- new_names

  new_formula <- stats::reformulate(termlabels = new_names[-1L], response = new_names[1L])
  list(
    formula = new_formula,
    data = as.data.frame(mf, stringsAsFactors = FALSE),
    mapping = list(
      # Mapping is for debugging / future UX; not currently shown to users.
      response_raw = deparse(response_expr, width.cutoff = 500L),
      response = new_names[1L],
      predictors_raw = predictor_terms,
      predictors = new_names[-1L]
    )
  )
}

#' Validate Formula Variables
#'
#' Checks if the input is a formula and validates that all variables mentioned
#' in the formula exist either in the provided data frame or in the environment.
#' This is a lightweight validation function that should be called early in functions
#' that accept formula syntax.
#'
#' @param formula A potential formula object to validate (can be any object).
#' @param data An optional data frame containing the variables.
#' @param func_name Character string. Name of the calling function (for error messages).
#' @param calling_env The environment in which to look for variables if data is not provided.
#'   Defaults to parent.frame().
#'
#' @return Returns NULL invisibly. Stops with an error if validation fails.
#'
#' @keywords internal
validate_formula <- function(formula, data = NULL, func_name = "function", calling_env = parent.frame()) {
  # Capture the call to get the actual expression passed (for better error messages)
  parent_call <- sys.call(-1)
  formula_expr <- if (!is.null(parent_call) && length(parent_call) >= 2) {
    parent_call[[2]]  # Get the formula argument expression
  } else {
    NULL
  }
  
  # First, check if we can even access the object
  # This catches cases like df$var where df doesn't exist
  obj_result <- tryCatch(
    {
      obj <- force(formula)
      list(success = TRUE, obj = obj)
    },
    error = function(e) {
      list(success = FALSE, error = e$message)
    }
  )
  
  if (!obj_result$success) {
    stop(obj_result$error, call. = FALSE)
  }
  
  # Check if the result is NULL (which happens when df$column where column doesn't exist)
  if (is.null(obj_result$obj)) {
    # Try to extract df name and column name from the expression
    if (!is.null(formula_expr) && is.call(formula_expr) && length(formula_expr) >= 3) {
      if (as.character(formula_expr[[1]]) == "$") {
        df_name <- as.character(formula_expr[[2]])
        col_name <- as.character(formula_expr[[3]])
        stop(sprintf("%s(): Column '%s' not found in '%s'", func_name, col_name, df_name), call. = FALSE)
      }
    }
    # Fallback error if we can't parse the expression
    stop(sprintf("%s(): The input evaluated to NULL", func_name), call. = FALSE)
  }
  
  # Now check if input is actually a formula
  if (!inherits(obj_result$obj, "formula")) {
    # Not a formula, return silently (allow non-formula inputs)
    return(invisible(NULL))
  }
  
  # From here on, use obj_result$obj instead of formula
  formula <- obj_result$obj
  
  formula_env <- environment(formula)
  if (is.null(formula_env)) {
    formula_env <- calling_env
  }
  
  # With data=: verify by evaluation in data context (supports df$col ~ df$col2).
  # With env only: require bindings only for symbols that are not `$` column names.
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop(sprintf("%s(): 'data' must be a data frame", func_name), call. = FALSE)
    }
    parent_call <- sys.call(-1)
    data_name <- if (!is.null(parent_call) && "data" %in% names(parent_call)) {
      deparse(parent_call$data)
    } else {
      "data"
    }
    data_name <- gsub('^"|"$', '', data_name)
    tmp_env <- list2env(as.list(data, use.names = TRUE), parent = calling_env)
    # Prefer evaluation (handles df$col). If evaluation fails (e.g. x1+x2 with factors),
    # fall back to: bare symbols in the expression must be columns of data.
    eval_formula_side <- function(expr) {
      ev <- tryCatch(
        list(ok = TRUE, val = suppressWarnings(eval(expr, envir = tmp_env))),
        error = function(e) list(ok = FALSE, err = e)
      )
      if (isTRUE(ev$ok)) {
        return(invisible(ev$val))
      }
      vars_in <- all.vars(expr)
      miss <- character(0)
      for (v in vars_in) {
        if (!v %in% names(data) && !exists(v, envir = calling_env, inherits = TRUE)) {
          miss <- c(miss, v)
        }
      }
      if (length(miss) > 0) {
        stop(sprintf('%s(): Variable "%s" not found in dataset "%s"', func_name, miss[1], data_name), call. = FALSE)
      }
      invisible(NULL)
    }
    if (length(formula) >= 2L) eval_formula_side(formula[[2L]])
    if (length(formula) >= 3L) eval_formula_side(formula[[3L]])
  } else {
    must_exist <- formula_symbols_required_in_env(formula)
    missing_vars <- character(0)
    for (var in must_exist) {
      if (!exists(var, envir = formula_env, inherits = TRUE)) {
        missing_vars <- c(missing_vars, var)
      }
    }
    if (length(missing_vars) > 0) {
      if (length(missing_vars) == 1) {
        stop(sprintf("%s(): Could not find variable '%s'", func_name, missing_vars[1]), call. = FALSE)
      } else {
        missing_list <- paste0("'", missing_vars, "'", collapse = ", ")
        stop(sprintf("%s(): Could not find variables: %s", func_name, missing_list), call. = FALSE)
      }
    }
  }
  
  invisible(NULL)
}

#' Validate Inputs for Plotting Functions
#'
#' Validates inputs for plotting functions that accept either formula syntax
#' (y ~ group) or standard syntax (y, group), with optional data frame.
#'
#' @param y A numeric vector, column name, or formula of the form y ~ group.
#' @param group A vector used to group the data, or a column name if data is provided.
#'   Can be NULL for functions where group is optional.
#' @param data An optional data frame containing the variables.
#' @param func_name Character string. Name of the calling function (for error messages).
#' @param require_group Logical. If TRUE, group is required. If FALSE, group can be NULL.
#' @param data_name Character string. Name of the data argument (for error messages). 
#'   If NULL, will attempt to infer from the call.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{y}: Validated y variable (numeric vector)
#'   \item \code{group}: Validated group variable (if provided, otherwise NULL)
#'   \item \code{y_name}: Clean variable name for y (for labels)
#'   \item \code{group_name}: Clean variable name for group (for labels)
#'   \item \code{y_name_raw}: Raw variable name for y (for error messages)
#'   \item \code{group_name_raw}: Raw variable name for group (for error messages)
#'   \item \code{data_name}: Name of data argument (for error messages)
#' }
#'
#' @keywords internal
validate_plot <- function(y, group = NULL, data = NULL, func_name = "plot", require_group = TRUE, data_name = NULL) {
  # Capture data name for error messages (before potentially overwriting)
  # If data_name not provided, try to infer it from the call
  if (is.null(data_name)) {
    # Try to get it from parent frame (the calling function)
    parent_call <- sys.call(-1)
    parent_func <- sys.function(-1)
    if (!is.null(parent_call) && !is.null(parent_func)) {
      # Wrap in tryCatch in case match.call fails (e.g., in test contexts)
      parent_mc <- tryCatch({
        match.call(definition = parent_func, call = parent_call)
      }, error = function(e) NULL)
      if (!is.null(parent_mc) && "data" %in% names(parent_mc)) {
        data_expr <- parent_mc$data
        if (!is.null(data_expr)) {
          data_name <- deparse(data_expr)
          # Remove quotes if present
          data_name <- gsub('^"|"$', '', data_name)
        }
      }
    }
    # Fallback to "data" if we couldn't determine it
    if (is.null(data_name)) {
      data_name <- "data"
    }
  }
  
  # Check if y is a formula
  # Use tryCatch to avoid "object not found" error if y is a symbol in data
  is_formula <- tryCatch(inherits(y, "formula"), error = function(e) FALSE)
  
  if (is_formula) {
    # Formula syntax: evaluate lhs/rhs (supports df$col ~ df$col2).
    fo <- y
    if (length(fo) < 2L || !identical(fo[[1L]], quote(`~`))) {
      stop(sprintf("%s(): Invalid formula", func_name), call. = FALSE)
    }
    if (require_group && length(fo) < 3L) {
      stop(sprintf("%s(): Formula must have exactly two variables: response ~ group", func_name), call. = FALSE)
    }
    formula_env <- environment(fo)
    if (is.null(formula_env)) {
      formula_env <- parent.frame()
    }
    enc <- parent.frame()
    if (length(fo) == 2L) {
      lhs_expr <- fo[[2L]]
      y_name_raw <- deparse(lhs_expr, width.cutoff = 500L)
      y_name <- .extract_formula_side_label(lhs_expr)
      group <- NULL
      group_name_raw <- NULL
      group_name <- NULL
      if (!is.null(data)) {
        if (!is.data.frame(data)) {
          stop(sprintf("%s(): 'data' must be a data frame", func_name), call. = FALSE)
        }
        y <- tryCatch(
          eval(lhs_expr, envir = data, enclos = enc),
          error = function(e) stop(sprintf("%s(): %s", func_name, e$message), call. = FALSE)
        )
      } else {
        y <- tryCatch(
          eval(lhs_expr, envir = formula_env),
          error = function(e) stop(sprintf("%s(): %s", func_name, e$message), call. = FALSE)
        )
      }
    } else {
      lhs_expr <- fo[[2L]]
      rhs_expr <- fo[[3L]]
      y_name_raw <- deparse(lhs_expr, width.cutoff = 500L)
      y_name <- .extract_formula_side_label(lhs_expr)
      rhs_is_intercept_only <- is.numeric(rhs_expr) && length(rhs_expr) == 1L &&
        !is.na(rhs_expr[1]) && rhs_expr[1] == 1
      if (!require_group && rhs_is_intercept_only) {
        group <- NULL
        group_name_raw <- NULL
        group_name <- NULL
        if (!is.null(data)) {
          if (!is.data.frame(data)) {
            stop(sprintf("%s(): 'data' must be a data frame", func_name), call. = FALSE)
          }
          y <- tryCatch(
            eval(lhs_expr, envir = data, enclos = enc),
            error = function(e) stop(sprintf("%s(): %s", func_name, e$message), call. = FALSE)
          )
        } else {
          y <- tryCatch(
            eval(lhs_expr, envir = formula_env),
            error = function(e) stop(sprintf("%s(): %s", func_name, e$message), call. = FALSE)
          )
        }
      } else {
        group_name_raw <- deparse(rhs_expr, width.cutoff = 500L)
        group_name <- .extract_formula_side_label(rhs_expr)
        if (!is.null(data)) {
          if (!is.data.frame(data)) {
            stop(sprintf("%s(): 'data' must be a data frame", func_name), call. = FALSE)
          }
          y <- tryCatch(
            eval(lhs_expr, envir = data, enclos = enc),
            error = function(e) stop(sprintf("%s(): %s", func_name, e$message), call. = FALSE)
          )
          group <- tryCatch(
            eval(rhs_expr, envir = data, enclos = enc),
            error = function(e) stop(sprintf("%s(): %s", func_name, e$message), call. = FALSE)
          )
        } else {
          y <- tryCatch(
            eval(lhs_expr, envir = formula_env),
            error = function(e) stop(sprintf("%s(): %s", func_name, e$message), call. = FALSE)
          )
          group <- tryCatch(
            eval(rhs_expr, envir = formula_env),
            error = function(e) stop(sprintf("%s(): %s", func_name, e$message), call. = FALSE)
          )
        }
      }
    }
  } else {
    # Standard syntax: y, group
    # Use match.call() to capture the actual expressions passed, not parameter names
    mc <- match.call()
    y_expr <- mc$y
    group_expr <- mc$group
    
    # Capture y name for xlab (before potentially overwriting y)
    if (!is.null(y_expr)) {
      y_name_raw <- deparse(y_expr)
    } else {
      y_name_raw <- deparse(substitute(y))
    }
    # Remove quotes if present (handles both y = "col" and y = col)
    y_name_raw <- gsub('^"|"$', '', y_name_raw)
    y_name <- if (grepl("\\$", y_name_raw)) {
      strsplit(y_name_raw, "\\$")[[1]][length(strsplit(y_name_raw, "\\$")[[1]])]
    } else {
      y_name_raw
    }
    
    # Capture group name for legend title (before potentially overwriting group)
    if (!is.null(group_expr)) {
      group_name_raw <- deparse(group_expr)
      # Remove quotes if present (handles both group = "col" and group = col)
      group_name_raw <- gsub('^"|"$', '', group_name_raw)
      group_name <- if (grepl("\\$", group_name_raw)) {
        strsplit(group_name_raw, "\\$")[[1]][length(strsplit(group_name_raw, "\\$")[[1]])]
      } else {
        group_name_raw
      }
    } else {
      group_name_raw <- NULL
      group_name <- NULL
    }
    
    # Handle data frame if provided
    if (!is.null(data)) {
      if (!is.data.frame(data)) {
        stop(sprintf("%s(): 'data' must be a data frame", func_name), call. = FALSE)
      }
      
      # Extract columns from data frame
      # Use raw names for column lookup (they may include df$ prefix)
      if (!y_name_raw %in% names(data)) {
        stop(sprintf("%s(): Column \"%s\" not found in dataset \"%s\"", func_name, y_name_raw, data_name), call. = FALSE)
      }
      if (!is.null(group_name_raw) && !group_name_raw %in% names(data)) {
        stop(sprintf("%s(): Column \"%s\" not found in dataset \"%s\"", func_name, group_name_raw, data_name), call. = FALSE)
      }
      
      y <- data[[y_name_raw]]
      if (!is.null(group_name_raw)) {
        group <- data[[group_name_raw]]
      }
    }
  }
  
  # Validate that y is a numeric vector
  if (!is.numeric(y) || !is.vector(y)) {
    stop(sprintf("%s(): '%s' must be a numeric vector, and '%s' is not", func_name, ifelse(is_formula, "y", y_name_raw), y_name_raw), call. = FALSE)
  }
  
  # Validate group if required
  if (require_group && is.null(group)) {
    stop(sprintf("%s(): 'group' argument is required", func_name), call. = FALSE)
  }
  
  # Convert factor to character if needed
  if (!is.null(group) && is.factor(group)) {
    group <- as.character(group)
  }
  
  # Validate group type and length if provided
  if (!is.null(group)) {
    if (!is.vector(group)) {
      stop(sprintf("%s(): 'group' must be a vector", func_name), call. = FALSE)
    }
    if (length(y) != length(group)) {
      stop(sprintf("%s(): 'y' and 'group' must have the same length (y has %d, group has %d)", func_name, length(y), length(group)), call. = FALSE)
    }
  }
  
  # Return validated inputs
  list(
    y = y,
    group = group,
    y_name = y_name,
    group_name = group_name,
    y_name_raw = y_name_raw,
    group_name_raw = group_name_raw,
    data_name = data_name
  )
}

#' Validate Inputs for table2() Function
#'
#' Validates inputs for table2() function that accepts multiple variables via ...
#' with optional data frame.
#'
#' @param ... One or more variables to be tabulated.
#' @param data An optional data frame containing the variables.
#' @param func_name Character string. Name of the calling function (for error messages).
#'   Default is "table2".
#' @param data_name Character string. Name of the data argument (for error messages).
#'   If NULL, will attempt to infer from the call.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{dots}: List of evaluated variables (ready for base::table)
#'   \item \code{dot_expressions}: List of expressions (for variable name extraction)
#'   \item \code{data_name}: Name of data argument (for error messages)
#' }
#'
#' @keywords internal
validate_table2 <- function(..., data = NULL, func_name = "table2", data_name = NULL) {
  # Capture data name for error messages
  # If data_name not provided, try to infer it from the call
  if (is.null(data_name)) {
    # Try to get it from parent frame (the calling function)
    parent_call <- sys.call(-1)
    parent_func <- sys.function(-1)
    if (!is.null(parent_call) && !is.null(parent_func)) {
      # Wrap in tryCatch in case match.call fails (e.g., in test contexts)
      parent_mc <- tryCatch({
        match.call(definition = parent_func, call = parent_call)
      }, error = function(e) NULL)
      if (!is.null(parent_mc) && "data" %in% names(parent_mc)) {
        data_expr <- parent_mc$data
        if (!is.null(data_expr)) {
          data_name <- deparse(data_expr)
          # Remove quotes if present
          data_name <- gsub('^"|"$', '', data_name)
        }
      }
    }
    # Fallback to "data" if we couldn't determine it
    if (is.null(data_name)) {
      data_name <- "data"
    }
  }
  
  # Handle data argument and capture expressions
  # If data is provided, evaluate unquoted variable names in the data context
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop(sprintf("%s(): 'data' must be a data frame", func_name), call. = FALSE)
    }
    
    # Capture expressions before evaluation
    dot_expressions <- as.list(substitute(list(...)))[-1L]
    
    # Evaluate each expression in the data context
    dots_list <- lapply(dot_expressions, function(expr) {
      # Check if it's a symbol (unquoted variable name)
      if (is.symbol(expr) || is.name(expr)) {
        var_name <- as.character(expr)
        if (var_name %in% names(data)) {
          return(data[[var_name]])
        } else {
          stop(sprintf("%s(): Column '%s' not found in dataset '%s'", func_name, var_name, data_name), call. = FALSE)
        }
      } else {
        # It's an expression (like df$var), evaluate it with data in context
        tryCatch({
          return(eval(expr, envir = data, enclos = parent.frame()))
        }, error = function(e) {
          # Try to extract variable name from expression for better error message
          var_name <- tryCatch({
            if (is.call(expr) && length(expr) >= 3) {
              op <- expr[[1]]
              if (identical(op, quote(`$`)) || identical(op, as.name("$"))) {
                as.character(expr[[3]])
              } else {
                deparse(expr)
              }
            } else {
              deparse(expr)
            }
          }, error = function(e2) "variable")
          stop(sprintf("%s(): Could not evaluate '%s' in dataset '%s': %s", func_name, var_name, data_name, e$message), call. = FALSE)
        })
      }
    })
    
    # Convert to list for base::table
    dots <- dots_list
  } else {
    # No data argument: use dots as-is
    dots <- list(...)
    dot_expressions <- as.list(substitute(list(...)))[-1L]
  }
  
  # Validate that all variables have the same length
  if (length(dots) > 1) {
    lengths <- sapply(dots, length)
    if (length(unique(lengths)) > 1) {
      # Extract variable names from expressions for better error message
      var_names <- sapply(dot_expressions, function(expr) {
        # Check if it's a symbol (variable name)
        if (is.symbol(expr) || is.name(expr)) {
          return(as.character(expr))
        }
        # Check if it's a dataframe column reference: df$var
        if (is.call(expr) && length(expr) >= 3) {
          op <- expr[[1]]
          if (identical(op, quote(`$`)) || identical(op, as.name("$"))) {
            return(as.character(expr[[3]]))
          }
        }
        # Fallback to deparsed expression
        return(deparse(expr))
      })
      
      # Build informative message showing each variable and its length
      var_info <- paste(sprintf("  %s (n=%d)", var_names, lengths), collapse = "\n")
      msg <- paste0(func_name, "(): All variables must have the same length, but these variables have different lengths:\n", var_info)
      message2(msg, col = "red", stop = TRUE)
    }
  }
  
  # Return validated inputs
  list(
    dots = dots,
    dot_expressions = dot_expressions,
    data_name = data_name
  )
}

#' Validate Grouping Variable for t.test2()
#'
#' Validates that a grouping variable exists and has exactly 2 levels for t-test.
#'
#' @param group_var_name Character string. Name of the grouping variable (for error messages).
#' @param data An optional data frame containing the variable.
#' @param calling_env The environment in which to look for the variable if data is not provided.
#' @param data_name Character string. Name of the data argument (for error messages).
#'   If NULL, will attempt to infer from the call.
#'
#' @return The validated grouping variable (numeric or factor vector). Stops execution with an error message if validation fails.
#'
#' @keywords internal
validate_t.test2 <- function(group_var_name, data = NULL, calling_env = parent.frame(), data_name = NULL) {
  # Capture data name for error messages (before potentially overwriting)
  # If data_name not provided, try to infer it from the call
  if (is.null(data_name)) {
    # Try to get it from parent frame (the calling function)
    parent_call <- sys.call(-1)
    parent_func <- sys.function(-1)
    if (!is.null(parent_call) && !is.null(parent_func)) {
      # Wrap in tryCatch in case match.call fails (e.g., in test contexts)
      parent_mc <- tryCatch({
        match.call(definition = parent_func, call = parent_call)
      }, error = function(e) NULL)
      if (!is.null(parent_mc) && "data" %in% names(parent_mc)) {
        data_expr <- parent_mc$data
        if (!is.null(data_expr)) {
          data_name <- deparse(data_expr)
          # Remove quotes if present
          data_name <- gsub('^"|"$', '', data_name)
        }
      }
    }
    # Fallback to "data" if we couldn't determine it
    if (is.null(data_name)) {
      data_name <- "data"
    }
  }
  
  # Check if variable exists in data or environment
  if (!is.null(data)) {
    # Data provided: check if variable exists in data frame
    if (!is.data.frame(data)) {
      message2("t.test2() says: 'data' must be a data frame", col = "red", stop = TRUE)
    }
    
    # Check if variable exists in data
    if (!group_var_name %in% names(data)) {
      message2(sprintf("t.test2() says: '%s' is not a variable in %s", group_var_name, data_name), col = "red", stop = TRUE)
    }
    
    # Extract variable from data
    group_var <- data[[group_var_name]]
  } else {
    # No data: check if variable exists before evaluating
    group_exists <- exists(group_var_name, envir = calling_env, inherits = TRUE)
    
    if (!group_exists) {
      message2(sprintf("t.test2() says: Could not find variable '%s'", group_var_name), col = "red", stop = TRUE)
    }
    
    # Variable exists, now evaluate
    group_var <- eval(as.name(group_var_name), envir = calling_env)
  }
  
  # Validate grouping variable has exactly 2 levels
  unique_groups <- unique(group_var)
  n_levels <- length(unique_groups[!is.na(unique_groups)])
  if (n_levels != 2) {
    message2(sprintf("t.test2() says: The grouping variable '%s' has %d unique values, cannot run t-test (requires exactly 2 levels).", group_var_name, n_levels), col = "red", stop = TRUE)
  }
  
  return(group_var)
}

#' Validate Inputs for lm2() Function
#'
#' Validates se_type and clusters arguments for lm2().
#' Also handles creating a data frame from vectors if data is not provided.
#'
#' @param formula A formula specifying the model.
#' @param data An optional data frame containing the variables.
#' @param se_type The type of standard error to use.
#' @param se_type_missing Logical. Whether se_type was not explicitly provided by user.
#' @param dots Additional arguments passed to lm_robust (to check for clusters).
#' @param calling_env The environment in which to look for variables if data is not provided.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{data}: The data frame to use (either provided or constructed from vectors)
#'   \item \code{se_type}: The validated/adjusted se_type
#'   \item \code{has_clusters}: Logical indicating if clusters are being used
#' }
#'
#' @keywords internal
validate_lm2 <- function(formula, data = NULL, se_type = "HC3", se_type_missing = TRUE, 
                         dots = list(), calling_env = parent.frame()) {
  
  # Check if clusters are specified (and not NULL)
  has_clusters <- "clusters" %in% names(dots) && !is.null(dots$clusters)
  
  # Validate se_type based on whether clusters are used
  if (has_clusters) {
    # If clusters specified, se_type cannot be specified (we hardcode CR2)
    if (!se_type_missing && toupper(se_type) != "HC3") {
      message2("lm2() says: When using clusters, se_type cannot be specified. lm2() uses CR2 for clustered standard errors.", col = "red")
      invokeRestart("abort")
    }
    se_type <- "CR2"
  } else {
    # Without clusters, only HC0-HC3 are valid
    valid_se_types <- c("HC0", "HC1", "HC2", "HC3")
    if (!toupper(se_type) %in% valid_se_types) {
      message2(paste0("lm2() says: se_type must be one of: ", paste(valid_se_types, collapse = ", "), 
           ". Got: '", se_type, "'"), col = "red")
      invokeRestart("abort")
    }
    # Normalize to uppercase
    se_type <- toupper(se_type)
  }
  
  # If data is not provided, try to construct it from vectors in the environment
  if (is.null(data)) {
    # If the formula uses `$`, evaluate it and rewrite to a data+formula pair.
    # This avoids treating `$` column names as required free variables.
    normalized <- .normalize_dollar_formula_to_data_and_formula(
      formula = formula,
      calling_env = calling_env,
      func_name = "lm2"
    )
    if (!is.null(normalized$data)) {
      formula <- normalized$formula
      data <- normalized$data
    }

    # If normalization produced data, we are done (skip env-based construction).
    if (is.null(data)) {
      # Extract variable names from formula (omit `$` RHS column names; see t.test2 / validate_formula)
      formula_vars <- all.vars(formula)
      must_exist <- formula_symbols_required_in_env(formula)
      
      # Check if all required bindings exist in the calling environment
      all_exist <- all(sapply(must_exist, function(v) exists(v, envir = calling_env, inherits = TRUE)))
      
      if (!all_exist) {
        missing_vars <- must_exist[!sapply(must_exist, function(v) exists(v, envir = calling_env, inherits = TRUE))]
        message2(paste0("lm2() says: Could not find variable(s): ", paste(missing_vars, collapse = ", "), 
             ". Either provide a 'data' argument or ensure variables exist in the environment."), col = "red")
        invokeRestart("abort")
      }
      
      # Get all variables from environment
      var_list <- lapply(formula_vars, function(v) {
        eval(as.name(v), envir = calling_env)
      })
      names(var_list) <- formula_vars
      
      # Check all have the same length
      lengths <- sapply(var_list, length)
      if (length(unique(lengths)) > 1) {
        length_info <- paste(sapply(seq_along(formula_vars), function(i) {
          paste0(formula_vars[i], " (", lengths[i], ")")
        }), collapse = ", ")
        message2(paste0("lm2() says: All variables must have the same length. Lengths: ", length_info), col = "red")
        invokeRestart("abort")
      }
      
      # Create data frame
      data <- as.data.frame(var_list, stringsAsFactors = FALSE)
    }
    
    # Also check clusters variable if specified
    if (has_clusters) {
      cluster_var <- dots$clusters
      # If clusters is a symbol/name, evaluate it
      if (is.symbol(cluster_var) || is.name(cluster_var)) {
        cluster_name <- as.character(cluster_var)
        if (!exists(cluster_name, envir = calling_env, inherits = TRUE)) {
          message2(paste0("lm2() says: Could not find clusters variable: ", cluster_name), col = "red")
          invokeRestart("abort")
        }
        cluster_values <- eval(cluster_var, envir = calling_env)
        if (length(cluster_values) != nrow(data)) {
          message2("lm2() says: clusters variable must have the same length as other variables", col = "red")
          invokeRestart("abort")
        }
        data[[cluster_name]] <- cluster_values
      }
    }
  }
  
  list(
    data = data,
    formula = formula,
    se_type = se_type,
    has_clusters = has_clusters
  )
}
