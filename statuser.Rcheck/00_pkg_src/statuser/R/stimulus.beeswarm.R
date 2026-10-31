#' Make beeswarm plots for compared-stimulus designs
#'
#' Beeswarm plot for compared-stimulus designs from "Stimulus Sampling Reimagined"
#' (Simonsohn, Montealegre, & Evangelidis, 2025).
#'
#' @param data Data frame containing variables to be analyzed.
#' @param dv Name of the dependent variable.
#' @param stimulus Name of the variable containing the stimulus ID.
#' @param condition Name of the variable containing the condition indicator.
#' @param flip.conditions If `TRUE`, reverse the order of condition labels.
#' @param dv.is.percentage If `TRUE`, format values as percentages.
#' @param simtot Number of bootstraps for the confidence band under homogeneity.
#' @param confidence Confidence level for the band (default 95).
#' @param ylim Optional y-axis limits.
#' @param ylab1,ylab2 Labels on the y-axis (optional).
#' @param xlab1,xlab2 Labels on the x-axis (optional).
#' @param dot.spacing Horizontal distance between stimulus labels (`"auto"` by default).
#' @param col1,col2 Colors for the two conditions.
#' @param main Plot title.
#' @param watermark If `TRUE`, display package version watermark.
#' @param save.as File path for saving the figure (`.svg` or `.png`, optional).
#' @param svg.width Optional width when saving.
#' @param svg.height Optional height when saving.
#' @param ... Additional arguments passed to [graphics::plot.default()].
#'
#' @return Invisibly, a two-column matrix of beeswarm coordinates.
#' @importFrom beeswarm beeswarm
#' @importFrom grDevices adjustcolor dev.off png svg
#' @importFrom graphics axis legend mtext par plot segments text
#' @importFrom stats aggregate
#' @importFrom utils packageVersion
#' @export
stimulus.beeswarm <- function(
  data, dv, stimulus, condition,
  flip.conditions = FALSE,
  dv.is.percentage = FALSE,
  simtot = 500,
  confidence = 95,
  ylim = c(),
  ylab1 = '',
  ylab2 = '',
  xlab1 = '',
  xlab2 = NULL,
  dot.spacing = 'auto',
  col1 = 'blue4',
  col2 = 'red4',
  main = '',
  watermark = TRUE,
  save.as = '',
  svg.width = '',
  svg.height = '',
  ...
) {

#: 1 stimulus.beeswarm: validate -> means -> bootstrap -> plot
  args_passed <- as.list(match.call())[-1]
  args <- list(...)

  validate.beeswarm(
    data, dv, stimulus, condition, flip.conditions, dv.is.percentage,
    simtot, confidence, ylim, ylab1, ylab2, xlab1, xlab2, dot.spacing,
    col1, col2, main, watermark, save.as, svg.width, svg.height, args_passed
  )

  uc <- sort(as.character(unique(data[, condition])), decreasing = flip.conditions)
  dc1 <- data[, condition] == uc[1]
  dc2 <- data[, condition] == uc[2]

  ms1 <- stats::aggregate(data[dc1, dv], list(data[dc1, stimulus]), mean)
  ms2 <- stats::aggregate(data[dc2, dv], list(data[dc2, stimulus]), mean)
  ms1$condition <- uc[1]
  ms2$condition <- uc[2]
  names(ms1) <- names(ms2) <- c("stimulus", 'mean', 'condition')
  ms <- rbind(ms1, ms2)

  dataname <- clean_string(deparse(substitute(data)))
  md5k <- get.md5(list(dataname, dv, stimulus, condition, simtot))
  cache <- sp_stimulus_cache()

  if (does.cache.d.exist(md5k)) {
    maxmin_boot <- cache[[md5k]]
    if (sys.parent() == 0) {
      message2(
        "*Recycled results*:\n",
        "You had run this same analysis before with all the same variables and options.\n",
        "(data='", dataname, "' | dv='", dv, "' | stimulus='", stimulus, "' | condition='", condition, "' | simtot='", simtot, "')\n",
        "To save time, we are re-using saved results. To force new calculations\n",
        "change one of those parameters or clear your cache running: 'clear_stimulus_cache()'"
      )
    }
  } else {
    maxmin_boot <- get.maxmin.confidence(data, dv, stimulus, condition, simtot, confidence, ms1, ms2, dc1, dc2)
    cache[[md5k]] <- maxmin_boot
    .statuser_state$stimulus_cache <- cache
  }

  if (dot.spacing == 'auto') {
    stimulus.length <- mean(nchar(ms$stimulus))
    dot.spacing <- stimulus.length / 3 + 2
  }

  col1a <- grDevices::adjustcolor(col1, .75)
  col2a <- grDevices::adjustcolor(col2, .75)

  b <- beeswarm::beeswarm(ms$mean ~ ms$condition, spacing = dot.spacing, do.plot = FALSE, method = 'swarm')

  if (save.as != '') {
    filename <- save.as
    extension <- tools::file_ext(filename)
    w <- 9
    h <- 7
    if (svg.width != '') w <- svg.width
    if (svg.height != '') h <- svg.height
    if (extension == 'svg') grDevices::svg(filename, w, h)
    if (extension == 'png') grDevices::png(filename, w * 1000, h * 1000, res = 1000)
  }

  mar.before <- graphics::par("mar")
  mar.after <- mar.before
  custom_mar <- getOption("graphics.par")$mar
  if (!is.null(custom_mar)) mar.default <- custom_mar
  if (is.null(custom_mar)) mar.default <- c(5.1, 4.1, 4.1, 2.1)

  if (all(mar.before == mar.default)) {
    mar.after[3] <- ifelse(main == '', 1, 3)
    width.y.label <- nchar(max(pretty(ms$mean)))
    mar.after[2] <- max(width.y.label / 3.5, 5)
    if (ylab2 != '') mar.after[2] <- mar.after[2] + 1
    if (dv.is.percentage == TRUE) mar.after[2] <- mar.after[2] + 1
    graphics::par(mar = mar.after)
    on.exit(graphics::par(mar = mar.before), add = TRUE)
  }

  if (length(ylim) < 2) {
    ylim <- range(ms$mean)
    dy <- diff(ylim)
    ylim[2] <- ylim[2] + .25 * dy
  }

  graphics::plot(
    b$x, b$y, pch = NA,
    xlim = c(min(.75, b$x), max(2.25, b$x)),
    xlab = '', ylab = '', las = 1,
    yaxt = ifelse(dv.is.percentage, 'n', 's'),
    xaxt = 'n', ylim = ylim, ...
  )

  b1 <- b[b$x <= 1.5, , drop = FALSE]
  b2 <- b[b$x > 1.5, , drop = FALSE]
  graphics::text(b1$x, b1$y, ms[ms$mean == b1$y.orig, 'stimulus'], cex = .65, col = col1a)
  graphics::text(b2$x, b2$y, ms[ms$mean == b2$y.orig, 'stimulus'], cex = .65, col = col2a)

  if (xlab1 == '') xlab1 <- condition
  if (is.null(xlab2)) xlab2 <- uc
  graphics::mtext(side = 1, font = 2, xlab1, line = 3, cex = 1.65)
  graphics::mtext(side = 1, at = c(1, 2), line = 1.5, xlab2, cex = 1.25, font = 2)

  xt1 <- min(b1$x, 0.85)
  xt2 <- max(b1$x, 1.15)
  xt3 <- min(b2$x, 1.85)
  xt4 <- max(b2$x, 2.15)
  mean1 <- mean(data[data[, condition] == uc[1], dv], na.rm = TRUE)
  mean2 <- mean(data[data[, condition] == uc[2], dv], na.rm = TRUE)
  graphics::segments(x0 = xt1, x1 = xt2, y0 = mean1, y1 = mean1, col = col1, lwd = 3)
  graphics::segments(x0 = xt3, x1 = xt4, y0 = mean2, y1 = mean2, col = col2, lwd = 3)

  if (dv.is.percentage == FALSE) {
    rm1 <- round_smart(mean1)
    rm2 <- round_smart(mean2)
  } else {
    rm1 <- format_percent(mean1, 'auto')
    rm2 <- format_percent(mean2, 'auto')
  }
  graphics::text(xt2, mean1, paste0("M=", rm1), col = col1, cex = 1, pos = 4)
  graphics::text(xt3, mean2, paste0("M=", rm2), col = col2, cex = 1, pos = 2)

  b1L <- maxmin_boot$b1L
  b1H <- maxmin_boot$b1H
  b2L <- maxmin_boot$b2L
  b2H <- maxmin_boot$b2H
  graphics::polygon(x = c(xt1, xt1, xt2, xt2), y = c(b1L, b1H, b1H, b1L), col = grDevices::adjustcolor(col1, .1), border = NA)
  graphics::polygon(x = c(xt3, xt3, xt4, xt4), y = c(b2L, b2H, b2H, b2L), col = grDevices::adjustcolor(col2, .1), border = NA)

  if (ylab1 == '') ylab1 <- dv
  if (!"yaxt" %in% names(args)) {
    graphics::mtext(side = 2, line = mar.after[2] - 1.8, font = 2, cex = 1.65, ylab1)
    graphics::mtext(side = 2, line = mar.after[2] - 2.8, font = 3, cex = 1.25, ylab2, col = 'gray30')
  }
  if (dv.is.percentage == TRUE) {
    ys <- pretty(ms$mean)
    graphics::axis(side = 2, at = ys, paste0(ys * 100, "%"), las = 1)
  }

  if (main != '') graphics::mtext(side = 3, line = 1, font = 2, cex = 1.65, main)

  if (watermark == TRUE) {
    stim_vrs <- paste0("{statuser v", utils::packageVersion('statuser'), "}")
    graphics::mtext(side = 1, line = -1, cex = .7, stim_vrs, col = 'gray66', adj = 0, outer = TRUE)
  }

  kmax <- which.max(ms1$mean)
  stimulus.max <- ms1$stimulus[kmax]
  graphics::legend(
    'top',
    pch = c(21, NA, NA, NA),
    bty = 'n',
    lty = c(NA, 1, 1, NA),
    lwd = c(NA, 3, 20, NA),
    legend = c(
      paste0('Stimulus label, e.g., "', stimulus.max, '"'),
      'Mean across stimuli',
      paste0(confidence, "% confidence band for Max-to-Min stimulus range"),
      paste0("Based on ", simtot, " resamples under null of equal distributions")
    ),
    cex = 1,
    col = c(col1a, col1, grDevices::adjustcolor(col1, .1))
  )

  if (save.as != '') {
    message2("\nFigure was saved as '", save.as, "'")
    if (svg.width == "" & svg.height == "") {
      message2(paste0("NOTE: We used default width=", w, ", and height=", h, ", customize with svg.width & svg.height arguments."))
    }
    grDevices::dev.off()
  }

  if (save.as != '') {
    call.original <- match.call()
    call.original$save.as <- ''
    eval(call.original)
  }

  invisible(b[, 1:2])
}
