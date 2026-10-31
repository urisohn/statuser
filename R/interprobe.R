#' Test and probe possibly nonlinear interactions
#'
#' @description
#' Tests and probes 2-way regression interactions (i.e., between two predictors, x & z)
#' on their association with the dependent variable, y. When the argument `model` is
#' not specified, interprobe() estimates a Generalized Additive Model (`mgcv::gam`)
#' and thus does not impose linearity on any of the associations (the optional argument `model`
#' can be used to apply interprobe to other models, e.g., linear regression). 
#' In terms of testing the interaction,
#' when `model` is not specified, the interaction p-value comes from the null that the
#' association between x and y is independent of z. For legacy purposes, the p-value 
#' for the linear model's x*z interaction is also reported.
#' In case of substantive differences between the GAM and linear p-values for the interaction,
#' the plots `interprobe` generates (see below) are likely to help users identify
#' the origin of the disagreement (e.g., a blatantly nonlinear association between
#' x & y could produce a spurious (or eliminate a real) interaction between x & z).
#' In terms of probing the interaction (describing the estimated association
#' between the focal predictor, x, and the dependent variable, y, for a given
#' set of x, z values), `interprobe` reports two traditional analyses:
#' simple slopes and the Johnson-Neyman procedure. Simple slopes map the predicted y
#' for combinations of x & z. Simple slopes are also known as "spotlight analysis".
#' Johnson-Neyman analysis, in turn, maps the estimated marginal effect of x on y,
#' for sets of x & z values. Both simple slopes and Johnson-Neyman keep all other
#' predictors constant, which is necessary to conduct the analysis ceteris paribus
#' (see https://datacolada.org/123).
#'
#' @section Input options:
#' Call `interprobe()` in one of three ways:
#' 1. `data` plus names `x`, `z`, and `y`
#' 2. vectors `x`, `z`, and `y` (no `data`)
#' 3. a fitted `model` plus names `x` and `z` (`y` is taken from the model)
#'
#' You cannot pass both `data` and `model`. If both `model` and `y` are
#' supplied, `y` is ignored.
#'
#' @section Model estimated:
#' By default `interprobe()` fits a GAM with `mgcv::gam(..., method = "REML")`.
#' GAM probing avoids the linear-interaction-effect assumption of the classical
#' `y ~ x * z` model: the conditional effect of `x` can bend freely with `z`
#' (and vice versa). The frequency panel under each plot shows where the data
#' support the curves (common support across the moderator range).
#'
#' Default formulas:
#' - If `x` has 3 or fewer unique values: `y ~ s(z, by = x) + x` (with `x` as a factor)
#' - If `x` has 4 or more unique values: `y ~ s(z) + s(x) + ti(x, z)`
#'
#' Set `model = "linear"` (or `"lm"`) to fit `lm2(y ~ x * z)` instead. Pass a
#' fitted model (e.g. `lm()`, `lm2()`, `glm()`, `mgcv::gam()`) to include
#' covariates; other predictors are held at their means on the prediction grid.
#' Any model supported by `marginaleffects` can be probed.
#'
#' @section How x and z are classified:
#' Classification uses the number of unique observed values:
#' - `x` with 2-3 unique values: **categorical** (one curve per level; spotlights unused)
#' - `x` with 4 to `max.unique` unique values: **discrete** (still uses three spotlights)
#' - `x` with more than `max.unique` unique values: **continuous**
#' - `z` at or below `max.unique`: evaluated at observed values
#' - `z` above `max.unique`: evaluated on a grid of `probe.bins` points from min to max
#'
#' If `x` has 3 or fewer unique values, `moderator.on.x.axis = FALSE` is ignored
#' and the moderator is forced onto the x-axis.
#'
#' @section Spotlights:
#' When `spotlights` is `NULL` and `x` is not categorical, curves are drawn at
#' the 15th, 50th, and 85th percentiles (`stats::quantile(..., type = 3)`) of
#' `x` if the moderator is on the x-axis, otherwise of `z`. Default legend
#' labels are built from those percentiles and `legend.round`.
#'
#' @section Console output:
#' Unless `quiet = TRUE`, prints the pair being probed. When a GAM is estimated
#' internally, also prints an HC3 linear interaction test and a GAM interaction
#' F test. Johnson-Neyman regions of significance are printed only for a
#' categorical focal predictor when the JN panel is drawn.
#'
#' @param x Focal predictor. A name (bare or quoted) when `data` or `model` is
#'   provided, or a numeric/factor vector when probing from vectors.
#' @param z Moderator. Same options as `x`.
#' @param y Dependent variable. Same options as `x`. Not required (and ignored)
#'   when `model` is supplied.
#' @param model Model to probe. Default `NULL` fits a GAM of `y` on `x` and `z`
#'   (see Model estimated). Use `model = "linear"` or `model = "lm"` for an
#'   internal linear interaction via `lm2()`. Pass a fitted object to probe a
#'   model of your choice (the usual path for covariates).
#' @param data Optional data frame containing `x`, `z`, and `y`. Cannot be
#'   combined with a fitted `model`.
#' @param moderator.on.x.axis Logical. If `TRUE` (default), moderator `z` is on
#'   the x-axis. Ignored (forced to `TRUE`) when `x` has 3 or fewer unique values.
#' @param k Integer smoothness parameter passed to `mgcv::gam()` when estimating
#'   with the default GAM engine. Default `NULL` omits `k` (mgcv default), except
#'   the binary-`x` testing GAM which uses `k = 10`.
#' @param spotlights Optional numeric vector of length 3. Values at which curves
#'   are computed. Default `NULL` uses the 15th/50th/85th percentiles (see Spotlights).
#' @param spotlight.labels Optional character vector of length 3. Legend labels.
#'   Default `NULL` builds labels from `spotlights` and `legend.round`.
#' @param histogram Logical. If `TRUE` (default), show a frequency panel under
#'   each curve plot (common support).
#' @param max.unique Integer. Threshold for continuous vs discrete treatment of
#'   a variable (default `11`). Separately, `x` with 2-3 unique values is always
#'   categorical (see How x and z are classified).
#' @param n.bin.continuous Integer. Number of bins in the histogram when binning
#'   continuous values (default `10`).
#' @param n.max Integer. Sample size at which line darkness/width saturates
#'   (default `50`).
#' @param xlab Character. X-axis label. Default `""` uses the variable name.
#' @param cols Character vector of length 2 or 3. Colors for the curves.
#'   Length 2 is allowed when the focal predictor has exactly two levels;
#'   otherwise length 3 is required (three spotlights or up to three categorical levels).
#'   Default `c("red4", "dodgerblue", "green4")`.
#' @param ylab1 Character. Y-axis label for simple slopes. Default `""` becomes
#'   the name of `y`.
#' @param ylab2 Character. Y-axis label for the JN panel. Default `""` becomes
#'   `"Marginal effect of <x>"`.
#' @param main1 Character. Title for simple slopes. Default `"GAM Simple Slopes"`;
#'   rewritten to drop `"GAM"` when a non-GAM fitted model is supplied.
#' @param main2 Character. Title for JN. Default `"GAM Johnson-Neyman"`; rewritten
#'   like `main1` for non-GAM fitted models.
#' @param legend.round Integer vector length 2. Min/max decimals in legend labels
#'   (default `c(1, 4)`).
#' @param draw Which plots to draw: `"both"` (default), `"simple slopes"` (or
#'   legacy `"simple.slopes"`), `"jn"`, or `"none"` (no plot).
#' @param save.as Optional file path to save the plot (`.png` or `.svg`).
#' @param xlim Numeric vector length 2. X-axis limits. Default `NULL` uses the data range.
#' @param ylim1 Numeric vector length 2. Y-axis limits for simple slopes. Default `NULL`.
#' @param ylim2 Numeric vector length 2. Y-axis limits for JN. Default `NULL`.
#' @param legend.simple.slopes Optional legend title for simple slopes. Default `NULL`.
#' @param legend.jn Optional legend title for JN. Default `NULL`.
#' @param x.ticks Optional custom x-axis ticks. Default `NULL`.
#' @param y1.ticks Optional custom y-axis ticks for panel 1. Default `NULL`.
#' @param y2.ticks Optional custom y-axis ticks for panel 2. Default `NULL`.
#' @param quiet Logical. If `TRUE`, suppresses the probing message, interaction
#'   tests, and JN region printout (default `FALSE`).
#' @param probe.bins Integer. Grid resolution for continuous probing (default
#'   `100`; larger = smoother/slower).
#'
#' @return Invisibly returns a list with:
#' \itemize{
#'   \item \code{simple.slopes}: data.frame of predicted values (\code{y.hat}) and
#'     confidence limits, plus the evaluation grid for \code{x} and \code{z}
#'   \item \code{johnson.neyman}: data.frame of marginal effects
#'     (\code{marginal.effect}) and confidence limits on the same grid
#'   \item \code{frequencies}: data.frame with bin frequencies used for
#'     shading/histogram (\code{bin}, \code{f1}--\code{f3}; after a plot is drawn,
#'     also \code{bin_from} / \code{bin_to})
#'   \item \code{gam_results}: the model fitted inside \code{interprobe()} when
#'     none was supplied. Despite the name, this is an \code{lm2} fit when
#'     \code{model = "linear"} / \code{"lm"}
#'   \item \code{gam_results_testing}: when a GAM is estimated internally and
#'     \code{x} has exactly 2 unique values, a separate GAM used for interaction
#'     testing (with a \code{ti()} term and numeric coding of \code{x})
#'   \item \code{lm2_results}: when a GAM is estimated internally, the
#'     corresponding linear fit \code{lm2(y ~ x * z)} (or \code{NULL} if
#'     \code{estimatr} is not installed). Not set on the \code{model = "linear"} path
#' }
#'
#' @references
#' Simonsohn, U. (2024). Interacting with curves: How to validly test and probe
#' interactions in the real (nonlinear) world. \emph{Advances in Methods and
#' Practices in Psychological Science}, 7(1), Article 25152459231207787.
#' \doi{10.1177/25152459231207787}
#'
#' Montealegre, D., & Simonsohn, U. (2026). \emph{Johnson Neyman 2.0} (working paper).
#'
#' Run \code{citation("statuser")} for ready-to-use citation entries.
#'
#' @seealso
#' \code{vignette("interprobe", package = "statuser")}, \code{\link{lm2}},
#' \code{\link[mgcv]{gam}}, \code{\link{tacos_data}}
#'
#' @examples
#' \donttest{
#' # This is a stylized dataset inspired by Montealegre & Simonsohn (2026).
#' # It depicts how salsa spiciness relates to taco enjoyment,
#' # moderated by respondents' tolerance for spicy food.
#' data(tacos_data)
#'
#' # Case A: experiment with Mild vs Hot salsa (binary focal predictor)
#' tacos_bin <- tacos_data[!is.na(tacos_data$spicy_bin), ]
#' interprobe(x = spicy_bin, z = tolerance, y = enjoyment, data = tacos_bin)
#'
#' # Case B: three salsa levels (Mild / Medium / Hot)
#' interprobe(x = salsa, z = tolerance, y = enjoyment, data = tacos_data)
#'
#' # Case C: continuous spiciness rating (1-7) by continuous tolerance
#' interprobe(x = spicy, z = tolerance, y = enjoyment, data = tacos_data)
#'
#' # Same as Case A, but force a classical linear interaction
#' interprobe(
#'   x = spicy_bin, z = tolerance, y = enjoyment,
#'   data = tacos_bin, model = "linear"
#' )
#' # Probe a user-fitted model (here with age as a covariate)
#' lm1 <- lm(enjoyment ~ spicy_bin * tolerance + age, data = tacos_bin)
#' interprobe(model = lm1, x = spicy_bin, z = tolerance)
#' }
#'
#' @usage
#' interprobe(
#'   x = NULL, z = NULL, y = NULL, model = NULL, data = NULL,
#'   moderator.on.x.axis = TRUE, k = NULL, spotlights = NULL, spotlight.labels = NULL,
#'   histogram = TRUE, max.unique = 11, n.bin.continuous = 10, n.max = 50,
#'   xlab = "", ylab1 = "", ylab2 = "", cols = c("red4", "dodgerblue", "green4"),
#'   main1 = "GAM Simple Slopes", main2 = "GAM Johnson-Neyman",
#'   legend.round = c(1, 4), draw = "both", save.as = NULL,
#'   xlim = NULL, ylim1 = NULL, ylim2 = NULL,
#'   x.ticks = NULL, y1.ticks = NULL, y2.ticks = NULL,
#'   legend.simple.slopes = NULL, legend.jn = NULL, quiet = FALSE, probe.bins = 100
#' )
#' @md
#' @export
interprobe <- function(
  x = NULL, z = NULL, y = NULL, model = NULL, data = NULL,
  moderator.on.x.axis = TRUE, k = NULL, spotlights = NULL, spotlight.labels = NULL,
  histogram = TRUE, max.unique = 11, n.bin.continuous = 10, n.max = 50,
  xlab = "", ylab1 = "", ylab2 = "", cols = c("red4", "dodgerblue", "green4"),
  main1 = "GAM Simple Slopes", main2 = "GAM Johnson-Neyman",
  legend.round = c(1, 4), draw = "both", save.as = NULL,
  xlim = NULL, ylim1 = NULL, ylim2 = NULL,
  x.ticks = NULL, y1.ticks = NULL, y2.ticks = NULL,
  legend.simple.slopes = NULL, legend.jn = NULL, quiet = FALSE, probe.bins = 100
) {
  # Capture names without evaluating x/z/y (bare names may exist only in data=/model)
  mc <- match.call(expand.dots = FALSE)
  provided_x <- "x" %in% names(mc)
  provided_z <- "z" %in% names(mc)
  provided_y <- "y" %in% names(mc)
  xvar <- if (provided_x) ip_clean_string(deparse(substitute(x))) else NULL
  zvar <- if (provided_z) ip_clean_string(deparse(substitute(z))) else NULL
  yvar <- if (provided_y) ip_clean_string(deparse(substitute(y))) else NULL
  dataname <- if (!is.null(data)) paste(deparse(substitute(data)), collapse = "") else NULL

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

  # With data= or model=, treat x/z/y as names — do not force env evaluation
  if (!is.null(data) || !is.null(model)) {
    if (!is.null(data) && (!provided_x || !provided_z || !provided_y)) {
      exit("interprobe() says: you must specify 'x', 'z' and 'y'")
    }
    if (provided_x) x <- xvar
    if (provided_z) z <- zvar
    if (provided_y) y <- yvar
  }

  # Run before ip_validate_arguments() so missing x/z with model=... hits the
  # intended message instead of failing on focal variable ('NULL') not in model.
  v <- ip_validate_input_combinations(
    data, model,
    x = if (provided_x) xvar else NULL,
    y = if (provided_y) yvar else NULL,
    z = if (provided_z) zvar else NULL
  )
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
    moderator.on.x.axis,
    dataname = dataname
  )

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

  # cols length: categorical needs one color per x level; spotlights need 3
  if (focal == "categorical") {
    if (length(cols) < nux) {
      exit(
        paste0(
          "interprobe() says: with ", nux, " levels of the focal predictor '",
          xvar, "', 'cols' must have length at least ", nux,
          " (got ", length(cols), ")"
        )
      )
    }
  } else if (length(cols) < 3) {
    exit(
      paste0(
        "interprobe() says: for continuous/discrete focal predictors, 'cols' must ",
        "have length 3 (got ", length(cols), ")"
      )
    )
  }

  if (nuz > max.unique) moderation <- "continuous"
  if (nuz <= max.unique) moderation <- "discrete"

  if (nux <= 3 & moderator.on.x.axis == FALSE) {
    message(
      paste0(
        "interprobe() says: 3 or fewer unique values for x variable ('", xvar, "').\n",
        "Will ignore the request to put it on the x-axis."
      )
    )
    moderator.on.x.axis <- TRUE
  }

  if (moderation == "continuous") zs <- seq(min(data[, zvar]), max(data[, zvar]), length.out = probe.bins)
  if (moderation == "discrete") zs <- uz

  if (focal == "continuous") xs <- seq(min(data[, xvar]), max(data[, xvar]), length.out = probe.bins)
  if (focal != "continuous") xs <- ux

  gam_results_testing <- NULL
  lm2_results_override <- NULL

  if (v$input.model == FALSE) {
    engine <- if (estimate_linear) "lm2" else "gam"
    model <- ip_estimate_model(nux, data, k, xvar, zvar, yvar, engine = engine)
    if (!is.null(model) && !is.null(model_label)) {
      attr(model, "interprobe_modelname") <- model_label
    }
    if (identical(engine, "gam") && nux == 2) {
      # Separate GAM for interaction testing: keep x numeric (0/1) and use ti(x,z)
      xF <- factor(data[[xvar]])
      xNum <- as.numeric(xF == levels(xF)[2])
      data_testing <- data
      data_testing[[xvar]] <- xNum

      kz <- if (is.null(k)) 10 else k
      gam_testing_formula <- stats::as.formula(
        paste0(
          yvar, " ~ ", xvar,
          " + s(", zvar, ",k=", kz, ")",
          " + ti(", zvar, ",by=", xvar, ",k=", kz, ")"
        )
      )
      gam_results_testing <- tryCatch(
        mgcv::gam(gam_testing_formula, data = data_testing, method = "REML"),
        error = function(e) NULL
      )

      if (requireNamespace("estimatr", quietly = TRUE)) {
        lm2_formula <- stats::as.formula(paste(yvar, "~", xvar, "*", zvar))
        lm2_results_override <- tryCatch(
          {
            fit_lm2 <- lm2(lm2_formula, data = data_testing)
            attr(fit_lm2, "lm2_call") <- as.call(list(as.name("lm2"), lm2_formula))
            fit_lm2
          },
          error = function(e) NULL
        )
      }
    }

    if (quiet == FALSE && identical(engine, "gam")) {
      cat("p-value for the interaction:\n")
      linear_txt <- if (!is.null(lm2_results_override)) {
        ip_get_linear_interaction_test_apa_from_lm2(lm2_results_override, xvar, zvar)
      } else {
        ip_get_linear_interaction_test_apa(data, xvar, zvar, yvar)
      }
      cat("  ", linear_txt, "\n", sep = "")
      gam_for_print <- if (!is.null(gam_results_testing)) gam_results_testing else model
      cat("  ", ip_get_gam_interaction_test_apa(gam_for_print, xvar, zvar), "\n\n", sep = "")
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
    output$gam_results <- model
    if (!is.null(gam_results_testing)) {
      output$gam_results_testing <- gam_results_testing
    }
    if (identical(engine, "gam")) {
      output$lm2_results <- if (!is.null(lm2_results_override)) {
        lm2_results_override
      } else {
        tryCatch(
          {
            if (!requireNamespace("estimatr", quietly = TRUE)) {
              NULL
            } else {
              fo_lm2 <- stats::as.formula(paste(yvar, "~", xvar, "*", zvar))
              fit_lm2 <- lm2(fo_lm2, data = data)
              # Make printed Call reflect the actual interaction formula.
              attr(fit_lm2, "lm2_call") <- as.call(list(as.name("lm2"), fo_lm2))
              fit_lm2
            }
          },
          error = function(e) NULL
        )
      }
    }
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

  breaks <- NULL

  if (draw2 != "none") {
    # Device height: extra room when frequency panels are stacked under the curves
    fig_h <- if (isTRUE(histogram)) 8.5 else 7
    fig_h_px <- if (isTRUE(histogram)) 8500 else 7000

    if (!is.null(save.as)) {
      extension <- tools::file_ext(save.as)

      if (draw2 == "both") {
        if (extension == "svg") grDevices::svg(save.as, width = 14, height = fig_h)
        if (extension == "png") grDevices::png(save.as, width = 14000, height = fig_h_px, res = 1000)
      } else {
        if (extension == "svg") grDevices::svg(save.as, width = 7, height = fig_h)
        if (extension == "png") grDevices::png(save.as, width = 7000, height = fig_h_px, res = 1000)
      }

      graphics::par(oma = c(0, 1, 0, 0))
      ip_setup_plot_layout(draw2, histogram)

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
      grDevices::dev.off()
    }

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(oma = c(0, 1, 0, 0))
    ip_setup_plot_layout(draw2, histogram)

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
  }

  regions.jn <- "N/A"
  if (draw2 != "simple slopes" & quiet == FALSE & focal == "categorical") {
    regions.jn <- ip_get_regions_jn(df2, xvar, zvar, focal, probe.bins)
    cat(regions.jn)
  }

  if (!is.null(breaks) && is.data.frame(breaks) && nrow(breaks) > 0) {
    output$frequencies$bin_from <- breaks$from
    output$frequencies$bin_to <- breaks$to
  }

  invisible(output)
}

