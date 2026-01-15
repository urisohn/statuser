#' Improves message() by adding color, font weight, and ability to stop execution
#'
#' @param ... One or more R objects that can be coerced to character and pasted together.
#' @param col Character string specifying the color. Can be any R color name (e.g., "dodgerblue", 
#'   "red4", "red1"). Default is "cyan". 
#'   Supports ALL named R colors by converting them to RGB. If the terminal supports 256-color 
#'   mode, colors are mapped directly to 256-color ANSI codes (allowing distinct colors like 
#'   red1 vs red4). Otherwise, colors are mapped to the closest of 16 ANSI colors (black, red, 
#'   green, yellow, blue, magenta, cyan, white, with both standard and bright variants). 
#'   If conversion fails, defaults to "cyan".
#' @param font Integer. 1 for plain text (default), 2 for bold text.
#' @param domain See \code{\link[base]{gettext}}. If \code{NA}, messages will not be translated.
#' @param appendLF Logical. Should a newline be appended to the message?
#' @param quiet Logical. If TRUE, the function returns invisibly without printing.
#' @param stop Logical. If TRUE, stops execution (like \code{stop()}) but without printing "Error:".
#'
#' @details
#' This function prints colored messages to the console. If ANSI color codes are supported
#' by the terminal, the message will be colored. Otherwise, it will be printed as plain text.
#' If \code{stop = TRUE}, execution will be halted after printing the message.
#'
#' @examples
#' message2("This is a plain cyan message", col = "cyan", font = 1)
#' message2("This is a bold cyan message", col = "cyan", font = 2)
#' message2("This is a bold red message", col = "red", font = 2)
#' \dontrun{
#' cat('this will be shown')
#' message2("This stops execution", stop = TRUE)
#' cat('this will not')
#' }
#'
#' @importFrom grDevices col2rgb
#' @export
message2 <- function(..., col = "cyan", font = 1, domain = NULL, appendLF = TRUE, 
                     quiet = FALSE, stop = FALSE) {
  
  if (quiet) {
    return(invisible())
  }
  
  # Check if ANSI is supported
  supportsANSI <- isTRUE(getOption("crayon.enabled", default = TRUE)) && 
                  (Sys.getenv("TERM") != "" || .Platform$OS.type == "windows")
  
  # Check for 256-color support
  supports256 <- FALSE
  if (requireNamespace("crayon", quietly = TRUE)) {
    supportsANSI <- crayon::has_color()
    # Check if terminal supports 256 colors
    # Most modern terminals support 256 colors if they support colors at all
    if (supportsANSI) {
      term <- Sys.getenv("TERM", "")
      # Most xterm-compatible terminals support 256 colors
      supports256 <- term != "" && (
        grepl("256", term) || 
        grepl("xterm", term, ignore.case = TRUE) ||
        grepl("screen", term, ignore.case = TRUE) ||
        grepl("tmux", term, ignore.case = TRUE) ||
        .Platform$OS.type == "windows"  # Windows Terminal supports 256 colors
      )
    }
  } else if (supportsANSI) {
    # Basic check without crayon
    term <- Sys.getenv("TERM", "")
    supports256 <- term != "" && (
      grepl("256", term) || 
      grepl("xterm", term, ignore.case = TRUE) ||
      .Platform$OS.type == "windows"
    )
  }
  
  # Helper function to convert RGB to 256-color ANSI code
  rgb_to_256color <- function(rgb_vals) {
    r <- rgb_vals[1]
    g <- rgb_vals[2]
    b <- rgb_vals[3]
    
    # If all components are equal and close, use grayscale (232-255)
    if (abs(r - g) < 3 && abs(g - b) < 3 && abs(r - b) < 3) {
      gray_val <- round((r + g + b) / 3)
      # Map to grayscale range 232-255
      gray_index <- round((gray_val / 255) * 23) + 232
      return(paste0("38;5;", gray_index))
    }
    
    # Otherwise, use 6x6x6 color cube (16-231)
    # Each component is quantized to 6 levels: 0, 95, 135, 175, 215, 255
    r_level <- round((r / 255) * 5)
    g_level <- round((g / 255) * 5)
    b_level <- round((b / 255) * 5)
    
    # Clamp to valid range
    r_level <- max(0, min(5, r_level))
    g_level <- max(0, min(5, g_level))
    b_level <- max(0, min(5, b_level))
    
    # Calculate index: 16 + 36*r + 6*g + b
    color_index <- 16 + 36 * r_level + 6 * g_level + b_level
    return(paste0("38;5;", color_index))
  }
  
  # Map basic ANSI color names to ANSI codes (for exact matches)
  basic_ansi_colors <- list(
    black = "30",
    red = "31",
    green = "32",
    yellow = "33",
    blue = "34",
    magenta = "35",
    cyan = "36",
    white = "37",
    gray = "90",
    grey = "90"
  )
  
  # RGB values for ANSI colors (standard and bright variants)
  # Standard colors (30-37)
  ansi_rgb_standard <- list(
    black = c(0, 0, 0),
    red = c(187, 0, 0),      # Dark red
    green = c(0, 187, 0),    # Dark green
    yellow = c(187, 187, 0), # Dark yellow
    blue = c(0, 0, 187),     # Dark blue
    magenta = c(187, 0, 187), # Dark magenta
    cyan = c(0, 187, 187),   # Dark cyan
    white = c(187, 187, 187) # Dark white/gray
  )
  
  # Bright colors (90-97)
  ansi_rgb_bright <- list(
    black = c(85, 85, 85),   # Dark gray
    red = c(255, 85, 85),     # Bright red
    green = c(85, 255, 85),   # Bright green
    yellow = c(255, 255, 85), # Bright yellow
    blue = c(85, 85, 255),    # Bright blue
    magenta = c(255, 85, 255), # Bright magenta
    cyan = c(85, 255, 255),   # Bright cyan
    white = c(255, 255, 255)  # White
  )
  
  # ANSI codes for standard and bright colors
  ansi_codes_standard <- c("30", "31", "32", "33", "34", "35", "36", "37")
  ansi_codes_bright <- c("90", "91", "92", "93", "94", "95", "96", "97")
  ansi_color_names <- names(ansi_rgb_standard)
  
  color_lower <- tolower(col)
  color_code <- NULL
  use_256color <- FALSE
  
  # Try exact match with basic ANSI colors first
  if (color_lower %in% names(basic_ansi_colors)) {
    color_code <- basic_ansi_colors[[color_lower]]
  } else {
    # Try to convert any R color name to RGB
    tryCatch({
      # Convert color name to RGB (handles all R color names)
      rgb_vals <- col2rgb(col)[, 1]
      
      # If 256-color mode is supported, use it for better color accuracy
      if (supports256) {
        color_code <- rgb_to_256color(rgb_vals)
        use_256color <- TRUE
      } else {
        # Fall back to 16-color mode: find closest ANSI color
        min_dist <- Inf
        closest_code <- "36"  # Default to cyan
        
        # Check standard colors
        for (i in seq_along(ansi_color_names)) {
          ansi_rgb <- ansi_rgb_standard[[i]]
          # Euclidean distance in RGB space
          dist <- sqrt(sum((rgb_vals - ansi_rgb)^2))
          if (dist < min_dist) {
            min_dist <- dist
            closest_code <- ansi_codes_standard[i]
          }
        }
        
        # Check bright colors
        for (i in seq_along(ansi_color_names)) {
          ansi_rgb <- ansi_rgb_bright[[i]]
          dist <- sqrt(sum((rgb_vals - ansi_rgb)^2))
          if (dist < min_dist) {
            min_dist <- dist
            closest_code <- ansi_codes_bright[i]
          }
        }
        
        color_code <- closest_code
      }
    }, error = function(e) {
      # If col2rgb fails, default to cyan
      if (supports256) {
        color_code <- "38;5;51"  # Cyan in 256-color mode
        use_256color <- TRUE
      } else {
        color_code <- "36"
      }
    })
  }
  
  # Fallback to cyan if still null
  if (is.null(color_code)) {
    if (supports256) {
      color_code <- "38;5;51"  # Cyan in 256-color mode
      use_256color <- TRUE
    } else {
      color_code <- "36"
    }
  }
  
  # Build ANSI escape sequence
  if (supportsANSI) {
    if (use_256color) {
      # 256-color mode: \033[38;5;COLORm (or \033[1;38;5;COLORm for bold)
      if (font == 2) {
        ansi_start <- paste0("\033[1;", color_code, "m")
      } else {
        ansi_start <- paste0("\033[", color_code, "m")
      }
    } else {
      # 16-color mode: \033[COLORm (or \033[1;COLORm for bold)
      if (font == 2) {
        ansi_start <- paste0("\033[1;", color_code, "m")
      } else {
        ansi_start <- paste0("\033[", color_code, "m")
      }
    }
    ansi_end <- "\033[0m"
  } else {
    ansi_start <- ""
    ansi_end <- ""
  }
  
  # Create message
  msg_parts <- list(...)
  msg_text <- paste(msg_parts, collapse = "")
  
  # Combine message parts with ANSI codes if supported
  if (supportsANSI) {
    # Wrap message with ANSI codes, start on new line
    msg <- paste0("\n", ansi_start, msg_text, ansi_end)
  } else {
    # Start on new line even without ANSI
    msg <- paste0("\n", msg_text)
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
    # Use cat to stderr for no newline (message() always adds newline)
    cat(msg, file = stderr())
  }
  
  # Stop execution if requested (without printing "Error:")
  if (stop) {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop(msg_text)
  }
  
  invisible()
}




