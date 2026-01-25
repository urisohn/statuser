#' Stat Tools for End Users 
#'
#' @description
#' Basic and custom statistical tools designed with end users in mind. 
#' Functions have optimized defaults, produce decluttered and informative output 
#' that is self-explanatory, and generate publication-ready results in 1 line of code. 
#' 
#' @section Basic Stats (improved):
#' \itemize{
#'   \item \code{\link{lm2}}: like lm(), with robust SE and much more informative output
#'   \item \code{\link{t.test2}}: like t.test(), decluttered, and more informative output 
#'   \item \code{\link{table2}}: like table(), showing variable names, and with proportions & chi2 built in
#'   \item \code{\link{desc_var}}: Descriptive statistics for variables (optional, by group(s))
#' }
#' @section Custom Stats (new):
#' \itemize{
#'   \item \code{\link{twolines}}: Two-lines test for U-shapes (Simonsohn 2018)
#' }
#'
#' @section Graphing:
#' \itemize{
#'   \item \code{\link{scatter.gam}}: Makes scatter plot for x & y, with fitted GAM line y=f(x)
#'   \item \code{\link{plot_cdf}}: Plot empirical cumulative distribution functions (optional, by group)
#'   \item \code{\link{plot_density}}: Plot density functions (optional, by group)
#'   \item \code{\link{plot_freq}}: Plot frequency of observed values (optional, by group)
#'   \item \code{\link{plot_gam}}: Plot fitted GAM values for a focal predictor
#'   \item \code{\link{text2}}: like text() adding text-alignment and background color
#' }
#'
#' @section Formatting:
#' \itemize{
#'   \item \code{\link{format_pvalue}}: Format p-values for display
#'   \item \code{\link{message2}}: Print colored messages to console
#'   \item \code{\link{resize_images}}: Resize images (SVG, PDF, EPS, JPG, PNG, etc.) to PNG with specified width
#' }
#'
#' @section Miscellaneous:
#' \itemize{
#'   \item \code{\link{list2}}: Like list(), but unnamed objects are automatically named
#'   \item \code{\link{convert_to_sql}}: Convert CSV files to SQL INSERT statements
#'   \item \code{\link{clear}}: Clear environment, console, and all graphics devices
#' }
#'
#' @author
#' Uri Simonsohn \email{urisohn@gmail.com}
#'
#' @references
#' Simonsohn, U. (2018). Two lines: A valid alternative to the invalid testing of 
#' U-shaped relationships with quadratic regressions. \emph{Advances in Methods and 
#' Practices in Psychological Science}, 1(4), 538-555. 
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
