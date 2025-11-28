#' Miscellaneous Functions for Papers and Blogposts
#'
#' @description
#' The \pkg{sohn} package provides miscellaneous functions used in papers and
#' blogposts by Uri Simonsohn. It includes functions for data visualization,
#' statistical analysis, and data formatting.
#'
#' @details
#' This package contains various utility functions organized into the following categories:
#'
#' @section Graphing:
#' Functions for data visualization and plotting:
#' \itemize{
#'   \item \code{\link{scatter.gam}}: Scatter plots with GAM smooth lines
#'   \item \code{\link{cdf.by}}: Plot empirical cumulative distribution functions by group
#'   \item \code{\link{fhist}}: Frequency histograms without binning
#' }
#'
#' @section Statistical Analyses:
#' Functions for statistical modeling:
#' \itemize{
#'   \item \code{\link{lmr}}: Linear models with robust standard errors
#' }
#'
#' @section Formatting:
#' Functions for formatting output and results:
#' \itemize{
#'   \item \code{\link{format.pvalue}}: Format p-values for display
#'   \item \code{\link{message.col}}: Print colored messages to console
#' }
#'
#' @section Simulations:
#' Functions for running and monitoring simulations:
#' \itemize{
#'   \item \code{\link{counter}}: Adaptive progress reporter for simulations
#' }
#'
#' @section Data Management:
#' Functions for data manipulation and utilities:
#' \itemize{
#'   \item \code{\link{namedList}}: Create lists with automatic naming
#'   \item \code{\link{convert_to_sql}}: Convert CSV files to SQL INSERT statements
#' }
#'
#' @author
#' Uri Simonsohn \email{urisohn@gmail.com}
#'
#' @references
#' Data Colada blog: \url{https://datacolada.org/}
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/urisohn/sohn}
#' }
#'
#' @keywords internal
"_PACKAGE"
