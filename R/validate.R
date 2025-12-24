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
validate_plot <- function(y, group = NULL, data = NULL, func_name = "plot", require_group = TRUE) {
  # Capture data name for error messages (before potentially overwriting)
  data_name <- deparse(substitute(data))
  # Remove quotes if present
  data_name <- gsub('^"|"$', '', data_name)
  
  # Check if y is a formula
  is_formula <- inherits(y, "formula")
  
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
    
    # Get calling environment for evaluating variables
    calling_env <- parent.frame()
    
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
    # Capture y name for xlab (before potentially overwriting y)
    y_name_raw <- deparse(substitute(y))
    # Remove quotes if present (handles both y = "col" and y = col)
    y_name_raw <- gsub('^"|"$', '', y_name_raw)
    y_name <- if (grepl("\\$", y_name_raw)) {
      strsplit(y_name_raw, "\\$")[[1]][length(strsplit(y_name_raw, "\\$")[[1]])]
    } else {
      y_name_raw
    }
    
    # Capture group name for legend title (before potentially overwriting group)
    group_expr <- substitute(group)
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

