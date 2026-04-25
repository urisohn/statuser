# Variable labels (exported)
#
# 1. Get/set variable labels
#' Get or set variable labels
#'
#' Assign labels to describe variables in a data frame.
#' Can be called on a single variable or full data, to assign or to read existing
#' labels.
#'
#' For a vector, this reads/writes a base-R \code{"label"} attribute.
#' For a data frame, this reads/writes the \code{"label"} attribute of each
#' column.
#'
#' @param x A vector or a data frame.
#' @param value Character label(s) to assign.
#'
#' @return
#' - If \code{x} is a data frame: a named character vector with one entry per
#'   column (missing labels are returned as \code{NA_character_}).
#' - Otherwise: a single character string (or \code{NA_character_}).
#'
#' @examples
#' df <- data.frame(x = 1:3, y = 4:6)
#'
#' # Set labels for all columns
#' var_labels(df) <- c("this is x", "this is y")
#' var_labels(df)
#'
#' # Set a label for a single column
#' var_labels(df$x) <- "this is x"
#' var_labels(df$x)
#'
#' @export
  var_labels <- function(x) {
    if (is.data.frame(x)) {
      out <- vapply(x, function(col) {
        lab <- attr(col, "label", exact = TRUE)
        if (is.null(lab)) NA_character_ else as.character(lab)[1]
      }, character(1))
      return(stats::setNames(out, names(x)))
    }
    
    lab <- attr(x, "label", exact = TRUE)
    if (is.null(lab)) {
      return(NA_character_)
    }
    as.character(lab)[1]
  }

#' @rdname var_labels
#' @export
  `var_labels<-` <- function(x, value) {
    # 1. Validate value
      if (is.list(value)) {
        value <- unlist(value, recursive = FALSE, use.names = TRUE)
      }
      if (!is.atomic(value) || (!is.character(value) && !is.factor(value))) {
        stop("var_labels<-(): 'value' must be a character vector", call. = FALSE)
      }
      
      value_names <- names(value)
      value <- as.character(value)
      names(value) <- value_names
    
    # 2. Data frame case
      if (is.data.frame(x)) {
        n <- ncol(x)
        if (!n) {
          return(x)
        }
        
        if (!is.null(names(value)) && any(nzchar(names(value)))) {
          # 2a. Named assignment: match by name (ignore unknown names)
            common <- intersect(names(x), names(value))
            for (nm in common) {
              v <- value[[nm]]
              if (is.na(v) || identical(v, "")) {
                attr(x[[nm]], "label") <- NULL
              } else {
                attr(x[[nm]], "label") <- v
              }
            }
            return(x)
        }
        
        # 2b. Positional assignment: names()-like length rules
          if (length(value) == 1L && n > 1L) {
            value <- rep(value, n)
            names(value) <- NULL
          } else if (length(value) != n) {
            stop(sprintf("var_labels<-(): 'value' must have length 1 or %d", n), call. = FALSE)
          }
          
          for (i in seq_len(n)) {
            v <- value[[i]]
            if (is.na(v) || identical(v, "")) {
              attr(x[[i]], "label") <- NULL
            } else {
              attr(x[[i]], "label") <- v
            }
          }
          return(x)
      }
    
    # 3. Vector-like case
      v <- value[[1]]
      if (is.na(v) || identical(v, "")) {
        attr(x, "label") <- NULL
      } else {
        attr(x, "label") <- v
      }
      x
  }

