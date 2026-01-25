#' Enhanced alternative to list()
#' 
#' List with objects that are automatically named. 
#' 
#' list2(x , y)      is equivalent to list(x = x , y = y)
#' 
#' list2(x , y2 = y) is equivalent to list(x = x , y2 = y)
#' 
#' Based on: \url{https://stackoverflow.com/questions/16951080/can-lists-be-created-that-name-themselves-based-on-input-object-names}
#'
#' @param ... Objects to include in the list. Objects are automatically named 
#'   based on their variable names unless explicit names are provided.
#'
#' @examples
#' x <- 1:5
#' y <- letters[1:3]
#' z <- matrix(1:4, nrow = 2)
#'
#' # Create named list from objects
#' my_list <- list2(x, y, z)
#' names(my_list)  # "x" "y" "z"
#'
#' # Works with explicit names too
#' my_list2 <- list2(a = x, b = y)
#' names(my_list2)  # "a" "b"
#'
#' @export
list2 <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)), deparse)[-1]
  
  if (is.null(nm <- names(L))) nm <- snm
  
  if (any(nonames <- nm == "")) nm[nonames] <- snm[nonames]
  
  setNames(L, nm)
}

