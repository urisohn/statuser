#' R functions I wished existed, and now do
#'
#' @description
#' The \pkg{statuser} package provides miscellaneous functions used in papers and
#' blogposts by Uri Simonsohn. It includes functions for data visualization,
#' statistical analysis, and data formatting.
#'
#' @details
#' This package contains various utility functions organized into the following categories:
#'
#' @section Graphing:
#' \itemize{
#'   \item \code{\link{scatter.gam}}: Scatter plots with GAM smooth lines
#'   \item \code{\link{plot_cdf}}: Plot empirical cumulative distribution functions by group
#'   \item \code{\link{plot_density}}: Plot density functions by group
#'   \item \code{\link{plot_freq}}: Frequency histograms without binning
#' }
#'
#' @section Statistical Analyses:
#' \itemize{
#'   \item \code{\link{t.test2}}: Enhanced t-test function returning dataframes
#'   \item \code{\link{desc_var}}: Descriptive statistics by group (or full dataset)
#' }
#'
#' @section Formatting:
#' \itemize{
#'   \item \code{\link{format_pvalue}}: Format p-values for display
#'   \item \code{\link{message2}}: Print colored messages to console
#' }
#'
#' @section Data Management:
#' \itemize{
#'   \item \code{\link{list2}}: Create lists with automatic naming
#'   \item \code{\link{convert_to_sql}}: Convert CSV files to SQL INSERT statements
#'   \item \code{\link{clear}}: Clear environment, console, and all graphics devices
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
#'   \item \url{https://github.com/urisohn/statuser}
#' }
#'
#' @importFrom grDevices adjustcolor dev.list dev.off png
#' @importFrom graphics abline arrows axis box hist layout legend lines mtext par points polygon rect segments strheight strwidth text
#' @importFrom stats aggregate as.formula complete.cases density drop.terms ecdf formula glm ks.test lm median model.frame na.omit predict quantile runif sd setNames terms update
#' @importFrom utils packageVersion read.csv
#' @keywords internal
"_PACKAGE"
