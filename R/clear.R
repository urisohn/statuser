#' Clear Plot, Global Environment, and Console
#'
#' Clears plot, global environment, and console. On first use the user is
#' prompted to authorize clearing the environment to comply with CRAN rules.
#'
#' @return Invisibly returns NULL. Prints a colored confirmation message.
#'
#' @details
#' This function performs three cleanup operations:
#' \itemize{
#'   \item \strong{Plot}: Closes all open graphics devices (except the null device)
#'   \item \strong{Global environment}: Removes all objects from the global environment
#'   \item \strong{Console}: Clears the console screen (only in interactive sessions)
#' }
#'
#' \code{clear()} will not modify the global environment unless you have
#' previously typed "yes" when prompted. If you do not type "yes", you are asked
#' again next time; only "yes" is remembered for future sessions.
#'
#' \strong{Warning}: This function deletes all objects in the global environment.
#' Save anything that you wish to keep before running.
#'
#' @examples
#' \donttest{
#' # Interactive use: clear workspace, console, and plots
#' # First run may prompt; once you type "yes", your preference is saved.
#' clear()
#' }
#'
#' @export
clear <- function() {
  pref <- clear_allow_global_preference()
  if (!isTRUE(pref)) {
    if (interactive()) {
      msg <- "To allow clear() to clear your environment, console, and plot, type \"yes\"\n(you will not be asked again if you say \"yes\").\n"
      message2(msg, col = "red4")
      ans <- trimws(tolower(readline(prompt = "")))
      if (!identical(ans, "yes")) {
        message2("You did not write 'yes', so clear() will not work.", col = "red4")
        return(invisible(NULL))  # End without clearing; will prompt again next time
      }
      clear_save_allow_global_preference(TRUE)
    } else {
      warning("clear() will not modify the global environment. Set the preference interactively first.")
      return(invisible(NULL))
    }
  }

  # Clear environment
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  
  # Clear console (only in interactive sessions)
  if (interactive()) {
    cat("\014")
  }
  
  # Clear all plots (close all devices except null device)
  # Close devices one at a time, checking each iteration
  dev_list <- dev.list()
  if (!is.null(dev_list) && length(dev_list) > 0) {
    for (dev_num in dev_list) {
      if (dev_num != 1) {  # Don't close the null device
        tryCatch({
          dev.off(which = dev_num)
        }, error = function(e) {
          # Device may have been closed already, ignore error
        })
      }
    }
  }
  
  # Print confirmation message
  
  message2("statuser::clear()", font=2,col = "red4")
  message2("Cleared plot, global environment, and console",col = "red4")
  
  invisible(NULL)
}

# --- Persistent preference for clearing .GlobalEnv (internal) ---

clear_config_dir <- function() {
  tryCatch(
    tools::R_user_dir("statuser", "config"),
    error = function(e) file.path(Sys.getenv("HOME", "~"), ".config", "R", "statuser")
  )
}

clear_allow_global_preference <- function() {
  dir <- clear_config_dir()
  path <- file.path(dir, "clear_allow_global")
  if (!file.exists(path)) return(NA)
  out <- trimws(readLines(path, n = 1L, warn = FALSE))
  if (identical(out, "TRUE")) return(TRUE)
  if (identical(out, "FALSE")) return(FALSE)
  NA
}

clear_save_allow_global_preference <- function(value) {
  dir <- clear_config_dir()
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "clear_allow_global")
  writeLines(as.character(value), path)
}
