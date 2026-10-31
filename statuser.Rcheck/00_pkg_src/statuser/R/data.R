#' Simulated nonlinear interaction for interprobe demos
#'
#' A small simulated dataset with a binary treatment, a continuous moderator,
#' and an outcome whose treatment effect rises nonlinearly with the moderator
#' and then flattens: \eqn{2(1 - e^{-mod/3})}. Useful for examples and for
#' checking that GAM probing recovers a curved interaction that a linear
#' \code{y ~ treat * mod} model would misshape.
#'
#' @format A data frame with 400 rows and 3 variables:
#' \describe{
#'   \item{treat}{Factor with levels \code{Control}, \code{Treated}}
#'   \item{mod}{Numeric moderator on \eqn{[0, 10]}}
#'   \item{y}{Numeric outcome}
#' }
#'
#' @source Simulated in \file{data-raw/interprobe_sim.R} with seed 20241030.
#'
#' @examples
#' data(interprobe_sim)
#' head(interprobe_sim)
#' \donttest{
#' interprobe(x = treat, z = mod, y = y, data = interprobe_sim, quiet = TRUE)
#' }
#'
#' @name interprobe_sim
#' @docType data
#' @keywords datasets
NULL
