#' Linear Model with Robust and/or Cluster Standard Errors
#'
#' Fits a linear model using ordinary least squares (OLS) and computes robust
#' standard errors. Supports both heteroskedasticity-robust (HC) and
#' cluster-robust standard errors. The resulting model object is modified so
#' that \code{predict()} uses robust standard errors for prediction intervals.
#'
#' @param formula Mirrors lm() formula: symbolic description of the model to be
#'   fitted.
#' @param data An optional data frame, list or environment (or object coercible
#'   by \code{\link[base]{as.data.frame}} to a data frame) containing the
#'   variables in the model. If not found in \code{data}, the variables are
#'   taken from \code{environment(formula)}, typically the environment from
#'   which \code{lmr} is called.
#' @param se_type Character string specifying the type of robust standard
#'   errors. Default is \code{"HC3"}. For heteroskedasticity-robust errors,
#'   options include \code{"HC0"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"},
#'   \code{"HC4"}, \code{"HC4m"}, and \code{"HC5"}. For cluster-robust errors
#'   (when \code{cluster} is specified), options include \code{"HC0"},
#'   \code{"HC1"}, \code{"HC2"}, and \code{"HC3"}. See
#'   \code{\link[sandwich]{vcovHC}} and \code{\link[sandwich]{vcovCL}} for
#'   details.
#' @param cluster An optional vector or variable name (as character string)
#'   specifying the clustering variable for cluster-robust standard errors. If
#'   \code{NULL} (default), heteroskedasticity-robust standard errors are used.
#' @param ... Additional arguments passed to \code{\link[stats]{lm}}.
#'
#' @return An object of class \code{c("lmr", "lm")} containing the fitted model
#'   with robust standard errors. The object includes all standard \code{lm}
#'   components plus:
#'   \itemize{
#'     \item \code{vcov.robust}: The robust variance-covariance matrix
#'     \item \code{se_type}: The type of robust standard errors used
#'     \item \code{cluster}: The clustering variable (if specified)
#'   }
#'
#'   The object is modified internally so that \code{predict()} uses robust
#'   standard errors for computing prediction intervals.
#'
#' @details
#' This function fits a linear model using \code{\link[stats]{lm}} and then
#' computes robust standard errors using the \code{sandwich} package. The
#' function modifies the internal structure of the fitted model object so that
#' \code{predict()} will use robust standard errors when computing prediction
#' intervals.
#'
#' Two types of robust standard errors are supported:
#' \itemize{
#'   \item \strong{Heteroskedasticity-robust (HC)}: Used when \code{cluster =
#'     NULL}. These standard errors are robust to heteroskedasticity but assume
#'     independence of observations.
#'   \item \strong{Cluster-robust}: Used when \code{cluster} is specified.
#'     These standard errors account for both heteroskedasticity and clustering
#'     within groups.
#' }
#'
#'
#' @examples
#' # Example data
#' set.seed(123)
#' n <- 100
#' x <- rnorm(n)
#' y <- 2 + 3*x + rnorm(n, sd = 1 + abs(x))  # heteroskedastic errors
#' data <- data.frame(x = x, y = y)
#'
#' # Fit model with robust standard errors
#' fit <- lmr(y ~ x, data = data)
#' summary(fit)
#'
#' # Use HC1 standard errors instead of HC3
#' fit_hc1 <- lmr(y ~ x, data = data, se_type = "HC1")
#'
#' # Cluster-robust standard errors
#' data$cluster <- rep(1:10, each = 10)
#' fit_cluster <- lmr(y ~ x, data = data, cluster = "cluster")
#'
#' # Predictions use robust SEs
#' newdata <- data.frame(x = c(0, 1, 2))
#' predict(fit, newdata, interval = "prediction")
#'
#' # Verify results by hand using sandwich package
#' fit_lm <- lm(y ~ x, data = data)
#' vcov_robust <- sandwich::vcovHC(fit_lm, type = "HC3")
#' se_robust <- sqrt(diag(vcov_robust))
#' 
#' # Compare: lmr stores robust VCOV in fit$vcov.robust
#' all.equal(vcov_robust, fit$vcov.robust)
#' all.equal(se_robust, sqrt(diag(fit$vcov.robust)))
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link[sandwich]{vcovHC}},
#'   \code{\link[sandwich]{vcovCL}}, \code{\link[stats]{predict.lm}}
#'
#' @export
lmr <- function(formula, data, se_type = "HC3", cluster = NULL, ...) {
  # Fit OLS normally
  fit <- lm(formula, data = data, ...)
  
  # Compute robust VCOV
  if (!is.null(cluster)) {
    # clustered vcov
    cluster_var <- cluster
    V.robust <- sandwich::vcovCL(fit, cluster = cluster_var, type = se_type)
  } else {
    # standard HCx vcov
    V.robust <- sandwich::vcovHC(fit, type = se_type)
  }

  # --- overwrite internals so predict() uses robust SE ---
  fit$cov.unscaled <- V.robust   # full robust VCOV used directly
  fit$sigma <- 1                 # so predict.lm(): Var = X V X^T * 1^2

  # store metadata
  fit$vcov.robust <- V.robust
  fit$se_type <- se_type
  fit$cluster <- cluster
  
  class(fit) <- c("lmr", class(fit))
  return(fit)
}
