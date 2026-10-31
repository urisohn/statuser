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

#' Fictitious taco spiciness study for interprobe examples
#'
#' Simulated data inspired by the taco vignette in Montealegre and Simonsohn
#' (2026). Respondents vary in spice \code{tolerance}; salsa \code{spicy}ness
#' ranges from 1 (mild) to 7 (hot); \code{enjoyment} is highest when spiciness
#' roughly matches tolerance. Hot vs mild salsa is disliked at low tolerance and
#' preferred at high tolerance (nonlinear interaction).
#'
#' @format A data frame with 1000 rows and 6 variables:
#' \describe{
#'   \item{spicy}{Numeric salsa spiciness on \eqn{[1, 7]}}
#'   \item{salsa}{Factor with levels \code{Mild}, \code{Medium}, \code{Hot}
#'     (cut from \code{spicy}: \eqn{[1,3]}, \eqn{(3,5]}, \eqn{(5,7]})}
#'   \item{spicy_bin}{Factor \code{Mild}/\code{Hot} when \code{spicy} is near
#'     1 (\eqn{\le 1.5}) or near 7 (\eqn{\ge 6.5}); \code{NA} otherwise
#'     (use \code{!is.na(spicy_bin)} for the binary design)}
#'   \item{tolerance}{Numeric spice tolerance on about 1--7}
#'   \item{enjoyment}{Numeric taco enjoyment on about 15--100 (kept well above 0)}
#'   \item{age}{Integer age (nuisance covariate for fitted-model examples)}
#' }
#'
#' @source Simulated in \file{data-raw/tacos_data.R} with seed 20260727.
#'
#' @examples
#' data(tacos_data)
#' head(tacos_data)
#' \donttest{
#' tacos_bin <- tacos_data[!is.na(tacos_data$spicy_bin), ]
#' interprobe(x = spicy_bin, z = tolerance, y = enjoyment, data = tacos_bin)
#' }
#'
#' @name tacos_data
#' @docType data
#' @keywords datasets
NULL
