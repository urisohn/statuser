#' Round with Fixed Decimals
#'
#' Like \code{round()}, but always returns a character vector with exactly
#' \code{digits} decimal places. By default, drops leading zeros before the
#' decimal point when every finite value has absolute value less than 1
#' (e.g. \code{0.34} becomes \code{.34}).
#'
#' @param x A numeric vector.
#' @param digits Number of decimal places. Default is 0.
#' @param leading.zero Logical. If \code{FALSE} (default), drop leading zeros
#'   before the decimal when every finite value satisfies \code{|x| < 1}.
#'   If any finite value has \code{|x| >= 1}, leading zeros are kept for the
#'   whole vector. If \code{TRUE}, leading zeros are always kept.
#'
#' @return A character vector of the same length as \code{x}.
#'
#' @examples
#' round2(0.34, 2)
#' round2(0.34, 2, leading.zero = TRUE)
#' round2(c(0.34, 0.5), 2)
#' round2(c(0.34, 1.25), 2)
#' round2(1.2, 2)
#'
#' @export
round2 <- function(x, digits = 0, leading.zero = FALSE) {

  # 1. Validate input
    if (!is.numeric(x)) {
      stop("round2(): 'x' must be numeric", call. = FALSE)
    }

  # 2. Round
    xr <- round(x, digits)

  # 3. Format with fixed decimals
    nsmall <- max(0, digits)
    out <- format(xr, nsmall = nsmall, scientific = FALSE, trim = TRUE)

  # 4. Drop leading zeros only if all finite |x| < 1
    finite <- is.finite(xr)
    drop_lz <- isFALSE(leading.zero) && length(xr) > 0L &&
               all(!finite | abs(xr) < 1)
    if (drop_lz) {
      out <- sub("^(-?)0\\.", "\\1.", out)
    }

  # 5. Preserve NA as NA_character_ (leave NaN / Inf as formatted)
    out[is.na(xr) & !is.nan(xr)] <- NA_character_

  out
}
