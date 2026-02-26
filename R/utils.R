# Utility functions (not exported)

# Package-level state (avoid global options)
.statuser_state <- new.env(parent = emptyenv())
#0 Set default values in a list
set_default <- function(x, name, value) {
  if (!name %in% names(x)) x[[name]] <- value
  x
}

# Helper function to initialize bottom plot with background
init_bottom_plot <- function(xlim, ylim, xlab, ylab, bg, cex.lab) {
  plot(NA, NA, 
       xlim = xlim, 
       ylim = ylim,
       xlab = xlab, 
       ylab = ylab,
       main = "", 
       xaxt = "n", 
       yaxt = "n",
       bty = "o",  # Show border
       font.lab = 2, 
       cex.lab = cex.lab)
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = bg, border = NA)
  box()
}
  


#1 Get colors

      get.colors <- function(k) {
                  if (k==1) return('steelblue')
          
          if (k == 2) {
            return(c("firebrick", "dodgerblue"))
          } else if (k == 3) {
            return( c('orange1','red1','red4'))
          } else if (k == 4) {
            return(c('orange1','red1', 'red4','black'))
          } else {
            # For 5+ groups, use a combination of colors that cycle
            # Start with the 4-group palette and add more colors
            base_colors <- c(  "orange1", "orange3", "red2", "red4",
                             "dodgerblue", "dodgerblue3", "blue1", "blue4",
                             "green", "darkgreen",  "darkorchid", "purple4","gray88", "gray11")
            return(base_colors[1:k])
          }
      }

      
      
#2 Nice exit
  exit <- function(msg, col='red2',font=2) {
    message2(msg,col=col,font=font)
    invokeRestart("abort") 
  }

#3 Determine decimal places for group values
  group_decimals <- function(v) {
    v <- abs(v[is.finite(v)])
    if (!length(v)) return(2)
    
    x <- min(v)
    if (x < 1)  return(3)
    if (x < 10) return(2)
    return(1)
  }

#3 Determine decimal places for group values
  group_decimals <- function(v) {
    v <- abs(v[is.finite(v)])
    if (!length(v)) return(2)
    
    x <- min(v)
    if (x < 1)  return(3)
    if (x < 10) return(2)
    return(1)
  }

#4 Evaluate variable arguments with NSE support
#' Evaluate Variable Arguments with NSE Support
#'
#' Resolves bare symbols (unquoted names) or quoted strings to actual data,
#' either from a data frame or the calling environment. This enables both
#' \code{plot_density(DV1, data=df)} and \code{plot_density("DV1", data=df)}
#' to work identically.
#'
#' @param arg_expr Unevaluated expression (from match.call() or substitute())
#' @param arg_name Character string. Name of the argument (for error messages)
#' @param data Optional data frame to look up columns
#' @param calling_env Environment to evaluate symbols if data is NULL
#' @param func_name Character string. Name of calling function (for error messages)
#' @param allow_null Logical. If TRUE, NULL is a valid input. Default FALSE.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{value}: The actual data (vector, formula, or NULL)
#'   \item \code{name}: Clean name for labels (e.g., "DV1")
#'   \item \code{name_raw}: Raw name for error messages (e.g., "df$DV1")
#'   \item \code{was_symbol}: Logical. TRUE if input was an unquoted name
#' }
#'
#' @keywords internal
evaluate_variable_arguments <- function(arg_expr, 
                                        arg_name = "arg",
                                        data = NULL, 
                                        calling_env = parent.frame(),
                                        func_name = "function",
                                        allow_null = FALSE) {
  
  # Handle NULL input
  if (is.null(arg_expr)) {
    if (allow_null) {
      return(list(
        value = NULL,
        name = NULL,
        name_raw = NULL,
        was_symbol = FALSE
      ))
    } else {
      stop(sprintf("%s(): '%s' is required", func_name, arg_name), call. = FALSE)
    }
  }
  
  # Get the expression as a string for name extraction
  expr_str <- deparse(arg_expr, width.cutoff = 500L)
  expr_str <- paste(expr_str, collapse = "")
  
  # Determine what type of expression this is
  is_symbol <- is.symbol(arg_expr)
  # Check if it's a character constant (quoted string like "DV1")
  is_string <- is.character(arg_expr) || (is.call(arg_expr) && length(arg_expr) == 1 && is.character(arg_expr[[1]]))
  # For character constants captured by match.call, they appear as actual character values
  if (!is_symbol && is.character(arg_expr) && length(arg_expr) == 1) {
    is_string <- TRUE
    string_val <- arg_expr
  } else {
    string_val <- NULL
  }
  
  # Initialize return values
  var_value <- NULL
  var_name_raw <- expr_str
  var_name <- expr_str
  was_symbol <- is_symbol
  
  # Case 1: Bare symbol (unquoted name like DV1)
  if (is_symbol) {
    symbol_name <- as.character(arg_expr)
    
    if (!is.null(data)) {
      # Look up in data frame
      if (symbol_name %in% names(data)) {
        var_value <- data[[symbol_name]]
        var_name <- symbol_name
        var_name_raw <- symbol_name
      } else {
        stop(sprintf('%s(): Column "%s" not found in data', func_name, symbol_name), call. = FALSE)
      }
    } else {
      # Evaluate in calling environment
      if (exists(symbol_name, envir = calling_env, inherits = TRUE)) {
        var_value <- get(symbol_name, envir = calling_env, inherits = TRUE)
        var_name <- symbol_name
        var_name_raw <- symbol_name
      } else {
        stop(sprintf("%s(): Could not find variable '%s'", func_name, symbol_name), call. = FALSE)
      }
    }
  }
  
  # Case 2: Quoted string (like "DV1")
  else if (is_string && !is.null(string_val)) {
    string_name <- string_val
    
    if (!is.null(data)) {
      # Look up in data frame
      if (string_name %in% names(data)) {
        var_value <- data[[string_name]]
        var_name <- string_name
        var_name_raw <- string_name
      } else {
        stop(sprintf('%s(): Column "%s" not found in data', func_name, string_name), call. = FALSE)
      }
    } else {
      # String without data frame - look up in environment
      if (exists(string_name, envir = calling_env, inherits = TRUE)) {
        var_value <- get(string_name, envir = calling_env, inherits = TRUE)
        var_name <- string_name
        var_name_raw <- string_name
      } else {
        stop(sprintf("%s(): Could not find variable '%s'", func_name, string_name), call. = FALSE)
      }
    }
  }
  
  # Case 3: Complex expression (formula, vector, df$col, etc.)
  else {
    # Evaluate the expression
    var_value <- tryCatch({
      eval(arg_expr, envir = calling_env)
    }, error = function(e) {
      stop(sprintf("%s(): Error evaluating '%s': %s", func_name, expr_str, e$message), call. = FALSE)
    })
    
    # Extract clean name if it's df$col format
    var_name <- if (grepl("\\$", expr_str)) {
      parts <- strsplit(expr_str, "\\$")[[1]]
      parts[length(parts)]
    } else {
      expr_str
    }
    var_name_raw <- expr_str
  }
  
  
  return(list(
    value = var_value,
    name = var_name,
    name_raw = var_name_raw,
    was_symbol = was_symbol
  ))
}

