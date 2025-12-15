#' Colored Message Function
#'
#' Prints a colored message with optional bold formatting. 
#'
#' @param ... One or more R objects that can be coerced to character and pasted together.
#' @param col Character string specifying the color. Can be any color name (e.g., "cyan", 
#'   "red", "blue", "dodgerblue", etc.). Default is "cyan". For ANSI terminal colors, 
#'   standard colors (black, red, green, yellow, blue, magenta, cyan, white) will be used 
#'   directly; other colors will default to "cyan".
#' @param font Integer. 1 for plain text (default), 2 for bold text.
#' @param domain See \code{\link[base]{gettext}}. If \code{NA}, messages will not be translated.
#' @param appendLF Logical. Should a newline be appended to the message?
#' @param quiet Logical. If TRUE, the function returns invisibly without printing.
#'
#' @details
#' This function prints colored messages to the console. If ANSI color codes are supported
#' by the terminal, the message will be colored. Otherwise, it will be printed as plain text.
#'
#' @examples
#' message.col("This is a plain cyan message", col = "cyan", font = 1)
#' message.col("This is a bold cyan message", col = "cyan", font = 2)
#' message.col("This is a bold red message", col = "red", font = 2)
#'
#' @export
message.col <- function(..., col = "cyan", font = 1, domain = NULL, appendLF = TRUE, 
                        quiet = FALSE) {
  
  if (quiet) {
    return(invisible())
  }
  
  # Check if ANSI is supported
  supportsANSI <- isTRUE(getOption("crayon.enabled", default = TRUE)) && 
                  (Sys.getenv("TERM") != "" || .Platform$OS.type == "windows")
  
  # If crayon is available, use it for better ANSI support
  if (requireNamespace("crayon", quietly = TRUE)) {
    supportsANSI <- crayon::has_color()
  }
  
  # Map standard ANSI color names to ANSI codes
  color_codes <- list(
    black = "30",
    red = "31",
    green = "32",
    yellow = "33",
    blue = "34",
    magenta = "35",
    cyan = "36",
    white = "37",
    gray = "90",
    grey = "90",
    darkgray = "90",
    darkgrey = "90"
  )
  
  # Find matching ANSI color - check if color starts with any known ANSI color name
  color_lower <- tolower(col)
  color_code <- NULL
  
  # Try exact match first
  if (color_lower %in% names(color_codes)) {
    color_code <- color_codes[[color_lower]]
  } else {
    # Check if color starts with any ANSI color name (handles "red4", "blue2", etc.)
    for (ansi_color in names(color_codes)) {
      if (startsWith(color_lower, ansi_color)) {
        color_code <- color_codes[[ansi_color]]
        break
      }
    }
  }
  
  # Default to cyan if no match found
  if (is.null(color_code)) {
    color_code <- color_codes[["cyan"]]
  }
  
  # Build ANSI escape sequence
  if (supportsANSI) {
    if (font == 2) {
      # Bold: \033[1;COLORm
      ansi_start <- paste0("\033[1;", color_code, "m")
    } else {
      # Plain: \033[COLORm
      ansi_start <- paste0("\033[", color_code, "m")
    }
    ansi_end <- "\033[0m"
  } else {
    ansi_start <- ""
    ansi_end <- ""
  }
  
  # Create message
  msg <- list(...)
  
  # Combine message parts with ANSI codes if supported
  if (supportsANSI) {
    # Wrap message with ANSI codes, start on new line
    msg_text <- paste(msg, collapse = "")
    msg <- paste0("\n", ansi_start, msg_text, ansi_end)
  } else {
    # Start on new line even without ANSI
    msg <- paste0("\n", paste(msg, collapse = ""))
  }
  
  # Handle domain translation (similar to .makeMessage behavior)
  if (!is.null(domain)) {
    msg <- gettext(msg, domain = domain)
  }
  
  # Create message object
  msg_obj <- simpleMessage(msg)
  
  # Print message with appendLF handling
  if (appendLF) {
    message(msg_obj)
  } else {
    message(msg_obj, appendLF = FALSE)
  }
  
  invisible()
}

