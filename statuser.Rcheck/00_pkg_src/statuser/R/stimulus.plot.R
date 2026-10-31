#' Make stimulus plots as in "Stimulus Sampling Reimagined"
#'
#' Build stimulus plots comparing results for individual stimuli in an experiment
#' (Simonsohn, Montealegre, & Evangelidis, 2025).
#'
#' @param plot.type Either `"means"` or `"effects"`; determines what is plotted on the y-axis.
#' @param data Data frame containing variables to be analyzed.
#' @param dv Name of the dependent variable (e.g., `dv = 'y'`); quotes are not required.
#' @param condition Name of the variable containing the condition indicator.
#' @param stimulus Name of the variable containing the stimulus ID.
#' @param participant Name of the variable containing participant IDs; necessary for valid
#'   inference when `plot.type = 'effects'` and each participant provided more than one observation.
#' @param save.as File path for saving the figure (`.svg` or `.png`, optional).
#' @param svg.width Optional width when saving to SVG/PNG.
#' @param svg.height Optional height when saving to SVG/PNG.
#' @param sort.by Variable to sort stimuli by. Defaults to sorting by observed effect size.
#' @param flip.conditions If `TRUE`, subtract the first condition from the second instead of the default.
#' @param model Method used to compute overall average: `'regression'`, `'intercepts'`,
#'   `'slopes'`, and/or `'all'`.
#' @param overall.estimate Scalar or vector of overall average effect computed outside statuser.
#' @param overall.ci Confidence interval bounds for `overall.estimate`.
#' @param overall.p P-value for overall average effect computed outside statuser.
#' @param overall.label Label for overall averages on the x-axis.
#' @param ylab1,ylab2 Labels on the y-axis (optional).
#' @param xlab1,xlab2 Labels on the x-axis (optional).
#' @param decimals Number of decimals for value labels (`"auto"` by default).
#' @param null.method Null method for effects plots: `"shuffle"` or `"demean"`.
#' @param dv.is.percentage If `TRUE`, format the dependent variable as percentages.
#' @param legend.title Text with title above legend (optional).
#' @param simtot Number of resamples for heterogeneity under null (effects plots).
#' @param watermark If `TRUE`, display package version watermark.
#' @param seed Seed used when resampling for effects plots.
#' @param ylim Optional y-axis limits.
#' @param main Plot title.
#' @param ... Additional arguments passed to [graphics::plot.default()].
#'
#' @return Invisibly, a data frame (means plot) or list (effects plot).
#' @importFrom digest digest
#' @importFrom grDevices dev.off png svg
#' @importFrom graphics mtext
#' @importFrom utils packageVersion
#' @importFrom stats ave qt residuals t.test
#' @import lmerTest
#' @export
stimulus.plot <- function(
  plot.type = 'means',
  data, dv, condition, stimulus,
  participant = '',
  save.as = '',
  svg.width = '',
  svg.height = '',
  sort.by = '',
  flip.conditions = FALSE,
  model = c(),
  overall.estimate = c(),
  overall.ci = c(),
  overall.p = c(),
  overall.label = c(),
  ylab1 = '',
  ylab2 = '',
  xlab1 = 'Stimuli',
  xlab2 = '',
  decimals = 'auto',
  null.method = 'shuffle',
  dv.is.percentage = FALSE,
  legend.title = '',
  simtot = 1000,
  watermark = TRUE,
  seed = 2024,
  ylim = c(),
  main = '',
  ...
) {

#: 1 stimulus.plot: validate -> prepare -> plot -> watermark
  args_passed <- as.list(match.call())[-1]

  if (!"data.frame" %in% class(data)) {
    exit(paste0("stimulus.plot() says: the argument data must be a data.frame, but '", deparse(substitute(data)), "' is not a dataframe."))
  }
  data <- data.frame(data)

  dataname <- clean_string(deparse(substitute(data)))

  if (null.method == 'demeans') null.method <- 'demean'

  validate.stimulus.plot(
    plot.type, data, dv, condition, stimulus,
    save.as, svg.width, svg.height, sort.by, flip.conditions,
    model, overall.estimate, overall.ci, overall.p, overall.label,
    ylab1, ylab2, xlab1, xlab2, decimals, null.method,
    dv.is.percentage, legend.title, simtot, watermark, seed, ylim,
    args_passed
  )

  f <- 'statuser::stimulus.plot'
  validate.dots(f, ...)

  dv <- clean_string(deparse(substitute(dv)))
  condition <- clean_string(deparse(substitute(condition)))
  stimulus <- clean_string(deparse(substitute(stimulus)))
  sort.by <- clean_string(deparse(substitute(sort.by)))

  validate.data(f, data, dv, condition, stimulus, sort.by, participant, dataname)

  data <- data[, names(data) %in% c('r', dv, condition, stimulus, participant), drop = FALSE]

  n1 <- nrow(data)
  data <- data[!is.na(data[, stimulus]) & !is.na(data[, dv]) & !is.na(data[, condition]), , drop = FALSE]
  if (participant != '') data <- data[!is.na(data[, participant]), , drop = FALSE]
  n2 <- nrow(data)
  if (n2 < n1) message2('stimulus.plot() says: ', n1 - n2, ' observations were dropped because of missing values.')

  t <- table(data[, stimulus], data[, condition])
  drop.stim <- t[t[, 1] == 0 | t[, 2] == 0, , drop = FALSE]
  if (nrow(drop.stim) > 0) {
    drop.stim.rows <- data[, stimulus] %in% rownames(drop.stim)
    data <- data[!drop.stim.rows, , drop = FALSE]
    n3 <- nrow(data)
    message2('stimulus.plot() says: ', n2 - n3, ' observations were dropped because their value of "', stimulus, '" appears in only one condition')
  }

  if (save.as != '') {
    filename <- save.as
    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
    extension <- tools::file_ext(filename)
    ns <- length(unique(data[, stimulus]))
    nm <- length(model)
    w <- 5 + (ns + nm * 1.5) * .4
    h <- 5 * (1 + max(nchar(unique(data[, stimulus]))) / 40)
    if (svg.width != '') w <- svg.width
    if (svg.height != '') h <- svg.height
    if (extension == 'svg') grDevices::svg(filename, w, h)
    if (extension == 'png') grDevices::png(filename, w * 1000, h * 1000, res = 1000)
  }

  if (plot.type == 'means') {
    res <- stimulus.plot.means(
      data = data, dv = dv, condition = condition, stimulus = stimulus,
      participant = participant, sort.by = sort.by, flip.conditions = flip.conditions,
      ylab1 = ylab1, ylab2 = ylab2, xlab1 = xlab1, xlab2 = xlab2,
      decimals = decimals, dv.is.percentage = dv.is.percentage,
      legend.title = legend.title, ylim = ylim, main = main, ...
    )
  }

  if (plot.type == 'effects') {
    res <- stimulus.plot.effects(
      data = data, dv = dv, condition = condition, stimulus = stimulus,
      dataname = dataname, model = model,
      overall.estimate = overall.estimate, overall.ci = overall.ci,
      overall.p = overall.p, overall.label = overall.label,
      participant = participant, sort.by = sort.by,
      flip.conditions = flip.conditions, null.method = null.method,
      ylab1 = ylab1, ylab2 = ylab2, xlab1 = xlab1, xlab2 = xlab2,
      dv.is.percentage = dv.is.percentage, simtot = simtot,
      decimals = decimals, ylim = ylim, seed = seed, main = main, ...
    )
  }

  if (watermark == TRUE) {
    stim_vrs <- paste0("{statuser v", utils::packageVersion('statuser'), "}")
    graphics::mtext(side = 1, line = -1, cex = .7, stim_vrs, col = 'gray66', adj = 0, outer = TRUE)
  }

  if (save.as != '') grDevices::dev.off()

  invisible(res)
}
