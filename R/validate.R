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
    # Formula syntax: y ~ group
    # Extract variable names from formula
    formula_vars <- all.vars(y)
    
    # Check if group is required
    if (require_group) {
      if (length(formula_vars) != 2) {
        stop(sprintf("%s(): Formula must have exactly two variables: response ~ group", func_name), call. = FALSE)
      }
    } else {
      if (length(formula_vars) < 1 || length(formula_vars) > 2) {
        stop(sprintf("%s(): Formula must have one or two variables: response or response ~ group", func_name), call. = FALSE)
      }
    }
    
    y_var_name <- formula_vars[1]
    group_var_name <- if (length(formula_vars) >= 2) formula_vars[2] else NULL
    
    # Get environment for evaluating variables
    # Use the formula's environment if available (where the formula was created),
    # otherwise fall back to parent.frame() (the calling function's environment)
    formula_env <- environment(y)
    if (is.null(formula_env)) {
      calling_env <- parent.frame()
    } else {
      calling_env <- formula_env
    }
    
    if (!is.null(data)) {
      # Data provided: extract from data frame
      if (!is.data.frame(data)) {
        stop(sprintf("%s(): 'data' must be a data frame", func_name), call. = FALSE)
      }
      
      # Check if variables exist in data
      if (!y_var_name %in% names(data)) {
        stop(sprintf("%s(): Variable \"%s\" not found in dataset \"%s\"", func_name, y_var_name, data_name), call. = FALSE)
      }
      if (!is.null(group_var_name) && !group_var_name %in% names(data)) {
        stop(sprintf("%s(): Variable \"%s\" not found in dataset \"%s\"", func_name, group_var_name, data_name), call. = FALSE)
      }
      
      # Extract variables from data
      y <- data[[y_var_name]]
      if (!is.null(group_var_name)) {
        group <- data[[group_var_name]]
      } else {
        group <- NULL
      }
    } else {
      # No data: check if variables exist before evaluating
      y_exists <- exists(y_var_name, envir = calling_env, inherits = TRUE)
      
      if (!y_exists) {
        stop(sprintf("%s(): Could not find variable '%s'", func_name, y_var_name), call. = FALSE)
      }
      
      if (!is.null(group_var_name)) {
        group_exists <- exists(group_var_name, envir = calling_env, inherits = TRUE)
        if (!group_exists) {
          stop(sprintf("%s(): Could not find variable '%s'", func_name, group_var_name), call. = FALSE)
        }
        group <- eval(as.name(group_var_name), envir = calling_env)
      } else {
        group <- NULL
      }
      
      # Variables exist, now evaluate y
      y <- eval(as.name(y_var_name), envir = calling_env)
    }
    
    # Set names for labels and raw names (used in error messages)
    y_name <- y_var_name
    group_name <- group_var_name
    y_name_raw <- y_var_name
    group_name_raw <- group_var_name
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
      length_str <- paste(sprintf("%d", lengths), collapse = ", ")
      stop(sprintf("%s(): All variables must have the same length. Lengths: %s", func_name, length_str), call. = FALSE)
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

