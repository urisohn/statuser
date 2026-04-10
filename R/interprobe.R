#' Probe interactions robustly to nonlinearities
#'
#' Probes an interaction by estimating (or accepting) a model and computing:
#' - simple slopes ("spotlights") using predicted values
#' - Johnson-Neyman ("jn") curves using marginal effects
#'
#' Designed for GAM models but works with any model supported by `marginaleffects`
#' (including `lm`, `glm`, `mgcv::gam`, and `lm2` / `estimatr::lm_robust`).
#'
#' @param x The focal predictor. Can be a name (bare or quoted) when `data` or `model`
#'   is provided, or a numeric/factor vector when probing from vectors.
#' @param z The moderator. Same options as `x`.
#' @param y The dependent variable. Same options as `x`. Not required when `model` is supplied.
#' @param model By default `interprobe` estimates a GAM model predicting `y` with `x` and `z`.
#' You can instead probe a linear interaction by setting model=linear. You can also probe a 
#' model of your choice by running it separately, saving the output, and submitting it as the model 
#' argument to  interprobe. This is the way to include covariates for a probed interaction.
#' @param data Optional data frame containing `x`, `z`, and `y`.
#' @param moderator.on.x.axis Logical. If TRUE (default), moderator (`z`) is shown on the x-axis.
#' @param k Integer. Smoothness parameter passed to `mgcv::gam()` when estimating with the default
#'   GAM engine. 
#' @param spotlights Numeric vector of length 3. Values at which curves are computed.
#' @param spotlight.labels Character vector of length 3. Labels for the legend.
#' @param histogram Logical. If TRUE (default), show sample size distribution under the plot.
#' @param max.unique Integer. Threshold for treating a variable as continuous vs discrete.
#' @param n.bin.continuous Integer. Number of bins used in histogram when binning continuous values.
#' @param n.max Integer. Sample size at which line darkness/width saturates.
#' @param xlab Character. X-axis label.
#' @param cols Character vector of length 3. Colors for the three curves.
#' @param ylab1 Character. Y-axis label for simple slopes panel.
#' @param ylab2 Character. Y-axis label for JN panel.
#' @param main1 Character. Title for simple slopes panel.
#' @param main2 Character. Title for JN panel.
#' @param legend.round Integer vector length 2. Min/max decimals in legend.
#' @param draw Which plots to draw: `"both"` (default), `"simple slopes"` (or legacy `"simple.slopes"`), or `"jn"`.
#' @param save.as Optional file path to save plot (`.png` or `.svg`).
#' @param xlim Numeric vector length 2. X-axis limits.
#' @param ylim1 Numeric vector length 2. Y-axis limits for simple slopes.
#' @param ylim2 Numeric vector length 2. Y-axis limits for JN.
#' @param legend.simple.slopes Optional legend title for simple slopes.
#' @param legend.jn Optional legend title for JN.
#' @param x.ticks Optional custom x-axis ticks.
#' @param y1.ticks Optional custom y-axis ticks for panel 1.
#' @param y2.ticks Optional custom y-axis ticks for panel 2.
#' @param quiet Logical. If TRUE, reduces console output.
#' @param probe.bins Integer. Resolution for probing curves (larger = smoother/slower).
#'
#' @return Invisibly returns a list with:
#' \itemize{
#'   \item \code{simple.slopes}: data.frame of predicted values and confidence intervals
#'   \item \code{johnson.neyman}: data.frame of marginal effects and confidence intervals
#'   \item \code{frequencies}: data.frame with bin frequencies used for shading/histogram
#'   \item \code{model}: the fitted model when estimated inside \code{interprobe()}
#'     (e.g. a \code{mgcv::gam} or \code{lm2} object)
#' }
#'
#' @export
interprobe <- function( x = NULL, z = NULL, y = NULL,
  model = NULL,  data = NULL,
  moderator.on.x.axis = TRUE,
  k = NULL,
  spotlights = NULL, spotlight.labels = NULL,
  histogram = TRUE, max.unique = 11,n.bin.continuous = 10,n.max = 50,
  xlab = "", ylab1 = "", ylab2 = "",
  cols = c("red4", "dodgerblue", "green4"),
  main1 = "GAM Simple Slopes", main2 = "GAM Johnson-Neyman",
  legend.round = c(1, 4), draw = "both",
  save.as = NULL,
  xlim = NULL, ylim1 = NULL, ylim2 = NULL,x.ticks = NULL,   y1.ticks = NULL, y2.ticks = NULL, 
  legend.simple.slopes = NULL, legend.jn = NULL,
  quiet = FALSE,
  probe.bins = 100
) {
  xvar <- ip_clean_string(deparse(substitute(x)))
  zvar <- ip_clean_string(deparse(substitute(z)))
  yvar <- ip_clean_string(deparse(substitute(y)))

  mc <- match.call(expand.dots = FALSE)
  model_label <- NULL
  if ("model" %in% names(mc)) {
    model_label <- paste0(deparse(mc[["model"]]), collapse = " ")
  }
  rs <- ip_interprobe_resolve_bare_model_symbol(mc, parent.frame(1))
  if (isTRUE(rs$handled)) {
    model <- rs$model
    estimate_linear <- rs$estimate_linear
  } else {
    pm <- ip_interprobe_parse_model_arg(model)
    model <- pm$model
    estimate_linear <- pm$estimate_linear
  }
  if (!is.null(model) && !is.null(model_label)) {
    attr(model, "interprobe_modelname") <- model_label
  }

  if (!is.null(data)) {
    if (is.null(x) | is.null(z) | is.null(y)) exit("interprobe says(): you must specify 'x', 'z' and 'y'")
    x <- xvar
    z <- zvar
    y <- yvar
  }

  # Run before ip_validate_arguments() so missing x/z with model=... hits the
  # intended message instead of failing on focal variable ('NULL') not in model.
  v <- ip_validate_input_combinations(data, model, x, y, z)
  if (v$input.model == TRUE) yvar <- all.vars(terms(model))[1]

  ip_validate_arguments(
    x, z, y,
    model, data,
    k,
    spotlights, spotlight.labels,
    histogram,
    max.unique, n.bin.continuous, n.max,
    xlab, ylab1, ylab2, main1, main2,
    cols,
    draw,
    legend.round,
    xlim,
    save.as,
    xvar, zvar, yvar,
    x.ticks, y1.ticks, y2.ticks,
    moderator.on.x.axis
  )

  if (!is.null(data)) {
    x <- xvar
    z <- zvar
    y <- yvar
  }

  if (quiet == FALSE) {
    cat(paste0("Probing the interaction of '", xvar, "' * '", zvar, "'\n"))
  }

  if (v$input.data == FALSE & v$input.xyz == TRUE) {
    data.text <- paste0("data = data.frame(", xvar, ",", zvar, ",", yvar, ")")
    data <- eval(parse(text = data.text), envir = parent.frame())
  }

  if (v$input.model == TRUE) {
    data <- ip_model_frame(model)
    if (is.null(data)) {
      exit("interprobe() says: could not recover the model frame from the fitted model (try stats::model.frame()).")
    }
  }

  ux <- sort(unique(data[, xvar]))
  uz <- sort(unique(data[, zvar]))
  nux <- length(ux)
  nuz <- length(uz)

  if (nux == 1) exit(paste0("interprobe says: there is only one observed value for the focal (x) variable, '", xvar, "'"))
  if (nuz == 1) exit(paste0("interprobe says: there is only one observed value for the moderator (z) variable, '", zvar, "'"))

  if (nux > max.unique) focal <- "continuous"
  if (nux <= max.unique & nux > 3) focal <- "discrete"
  if (nux <= 3) focal <- "categorical"

  if (nuz > max.unique) moderation <- "continuous"
  if (nuz <= max.unique) moderation <- "discrete"

  if (nux <= 3 & moderator.on.x.axis == FALSE) {
    message(
      paste0(
        "interprobe() says: Less than 3 unique values for x variable ('", xvar, "').\n",
        "Will ignore request to have it on the x-axis"
      )
    )
    moderator.on.x.axis <- TRUE
  }

  if (moderation == "continuous") zs <- seq(min(data[, zvar]), max(data[, zvar]), length.out = probe.bins)
  if (moderation == "discrete") zs <- uz

  if (focal == "continuous") xs <- seq(min(data[, xvar]), max(data[, xvar]), length.out = probe.bins)
  if (focal != "continuous") xs <- ux

  if (v$input.model == FALSE) {
    engine <- if (estimate_linear) "lm2" else "gam"
    model <- ip_estimate_model(nux, data, k, xvar, zvar, yvar, engine = engine)
    if (!is.null(model) && !is.null(model_label)) {
      attr(model, "interprobe_modelname") <- model_label
    }
  }

  if (is.null(spotlights) & focal != "categorical") {
    if (moderator.on.x.axis == FALSE) spotvar <- data[, zvar]
    if (moderator.on.x.axis == TRUE) spotvar <- data[, xvar]

    spotlights <- stats::quantile(spotvar, c(0.15, 0.5, 0.85), type = 3)

    if (is.null(spotlight.labels)) {
      spotlight.labels <- paste0(
        c("15th percentile (", "50th percentile (", "85th percentile ("),
        c(ip_round2(as.numeric(spotlights), max.d = legend.round[2], min.d = legend.round[1])),
        c(")", ")", ")")
      )
    }
  }

  if (is.null(spotlight.labels)) spotlight.labels <- as.numeric(spotlights)

  if (nux <= 3) simple.slopes <- ip_compute_slopes_discrete(ux, zs, model, xvar, zvar)
  if (nux > 3) simple.slopes <- ip_compute_slopes_continuous(spotlights, data, xs, zs, model, xvar, zvar, moderator.on.x.axis)

  if (nux <= 3) jn <- ip_compute_jn_discrete(ux, zs, model, xvar, zvar)
  if (nux > 3) jn <- ip_compute_jn_continuous(spotlights, data, xs, zs, model, xvar, zvar, moderator.on.x.axis)

  fxz.list <- ip_make_fxz(data, n.bin.continuous, moderation, nux, max.unique, spotlights, xvar, zvar, moderator.on.x.axis)
  fxz <- fxz.list$fxz

  gr <- fxz
  for (j in 1:ncol(fxz)) gr[, j] <- pmin(fxz[, j] / n.max, 1)

  df1 <- data.frame(do.call(rbind, simple.slopes))
  df2 <- data.frame(do.call(rbind, jn))

  df1 <- df1[, !names(df1) %in% c("rowid", "y", "s.value", "p.value", "statistic", yvar)]
  df2 <- df2[, !names(df2) %in% c("rowid", "y", "s.value", "statistic", "term", "predicted_lo", "predicted_hi", "predicted", yvar)]

  names(df1)[names(df1) == "estimate"] <- "y.hat"
  names(df2)[names(df2) == "estimate"] <- "marginal.effect"

  if (ncol(fxz) == 2) frequencies <- data.frame(bin = rownames(fxz), f1 = fxz[, 1], f2 = fxz[, 2], row.names = NULL)
  if (ncol(fxz) == 3) frequencies <- data.frame(bin = rownames(fxz), f1 = fxz[, 1], f2 = fxz[, 2], f3 = fxz[, 3], row.names = NULL)

  output <- list(simple.slopes = df1, johnson.neyman = df2, frequencies = frequencies)
  if (v$input.model == FALSE) {
    output$model <- model
  }

  if (v$input.model == TRUE) {
    if (!inherits(model, "gam")) {
      linear.st <- ""
      if (inherits(model, "lm")) linear.st <- "Linear "
      if (main1 == "GAM Simple Slopes") main1 <- paste0(linear.st, "Simple Slopes")
      if (main2 == "GAM Johnson-Neyman") main2 <- paste0(linear.st, "Johnson-Neyman")
    }
  }

  if (ylab1 == "") ylab1 <- yvar
  if (ylab2 == "") ylab2 <- paste0("Marginal effect of ", xvar)

  draw2 <- draw
  if (identical(draw2, "simple.slopes")) draw2 <- "simple slopes"

  if (!is.null(save.as)) {
    extension <- tools::file_ext(save.as)

    if (draw2 == "both") {
      if (extension == "svg") grDevices::svg(save.as, width = 14, height = 7)
      if (extension == "png") grDevices::png(save.as, width = 14000, height = 7000, res = 1000)
      par(mfrow = c(1, 2))
      par(oma = c(0, 1, 0, 0))
    } else {
      if (extension == "svg") grDevices::svg(save.as, width = 7, height = 7)
      if (extension == "png") grDevices::png(save.as, width = 7000, height = 7000, res = 1000)
      par(oma = c(0, 1, 0, 0))
    }

    if (draw2 %in% c("both", "simple slopes")) {
      ip_make_plot(
        type = "simple.slopes",
        xlab, ylab1, main1, simple.slopes, histogram, data, xs, zs, gr, spotlights, cols, spotlight.labels,
        focal, moderation, max.unique, fxz.list, nux, nuz, xvar, zvar, xlim, ylim1, legend.title = legend.simple.slopes,
        x.ticks, y1.ticks, moderator.on.x.axis
      )
    }

    if (draw2 %in% c("both", "jn")) {
      ip_make_plot(
        type = "jn",
        xlab, ylab2, main2, jn, histogram, data, xs, zs, gr, spotlights, cols, spotlight.labels,
        focal, moderation, max.unique, fxz.list, nux, nuz, xvar, zvar, xlim, ylim2, legend.title = legend.jn,
        x.ticks, y2.ticks, moderator.on.x.axis
      )
    }

    message(paste0("The figures have been saved to '", save.as, "'"))
    dev.off()
  }

  old_par <- par(no.readonly = TRUE)
  par(oma = c(0, 1, 0, 0))

  if (draw2 == "both") {
    par(mfrow = c(1, 2))
    on.exit(par(old_par))
  }

  breaks <- NULL

  if (draw2 %in% c("simple slopes", "both")) {
    breaks <- ip_make_plot(
      type = "simple.slopes",
      xlab, ylab1, main1, simple.slopes, histogram, data, xs, zs, gr, spotlights, cols, spotlight.labels,
      focal, moderation, max.unique, fxz.list, nux, nuz, xvar, zvar, xlim, ylim1, legend.title = legend.simple.slopes,
      x.ticks, y1.ticks, moderator.on.x.axis
    )
  }

  if (draw2 %in% c("jn", "both")) {
    breaks <- ip_make_plot(
      type = "jn",
      xlab, ylab2, main2, jn, histogram, data, xs, zs, gr, spotlights, cols, spotlight.labels,
      focal, moderation, max.unique, fxz.list, nux, nuz, xvar, zvar, xlim, ylim2, legend.title = legend.jn,
      x.ticks, y2.ticks, moderator.on.x.axis
    )
  }

  regions.jn <- "N/A"
  if (draw2 != "simple slopes" & quiet == FALSE & focal == "categorical") {
    regions.jn <- ip_get_regions_jn(df2, xvar, zvar, focal, probe.bins)
    cat(regions.jn)
  }

  if (!is.null(breaks)) {
    output$frequencies$bin_from <- breaks$from
    output$frequencies$bin_to <- breaks$to
  }

  invisible(output)
}

