# Internal helpers for interprobe()
# (Vendored from the deprecated `interacting` package, adapted for statuser.)

# lm/glm store the model frame in $model; estimatr::lm_robust / lm2 does not — use model.frame()
ip_model_frame <- function(model) {
  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (!is.null(mf) && is.data.frame(mf)) {
    return(mf)
  }
  m <- model[["model"]]
  if (!is.null(m) && is.data.frame(m)) {
    return(m)
  }
  NULL
}

# Distinguish model = fitted object vs model = "lm"/"linear"/stats::lm (estimate with lm2 inside interprobe).
ip_interprobe_parse_model_arg <- function(model) {
  if (is.null(model)) {
    return(list(model = NULL, estimate_linear = FALSE))
  }
  if (is.function(model) && identical(model, stats::lm)) {
    return(list(model = NULL, estimate_linear = TRUE))
  }
  if (is.character(model) && length(model) == 1L) {
    s <- trimws(model[[1L]])
    if (nzchar(s) && tolower(s) %in% c("lm", "linear")) {
      return(list(model = NULL, estimate_linear = TRUE))
    }
  }
  if (inherits(model, "factor")) {
    s <- trimws(as.character(model)[[1L]])
    if (nzchar(s) && tolower(s) %in% c("lm", "linear")) {
      return(list(model = NULL, estimate_linear = TRUE))
    }
  }
  list(model = model, estimate_linear = FALSE)
}

# Bare model = linear / model = lm: resolve in caller env (fitted object vs lm2 keyword) without forcing promise.
ip_interprobe_resolve_bare_model_symbol <- function(mc, env) {
  default <- list(handled = FALSE, model = NULL, estimate_linear = FALSE)
  if (!"model" %in% names(mc)) {
    return(default)
  }
  ma <- mc[["model"]]
  if (!is.name(ma)) {
    return(default)
  }
  nm <- as.character(ma)
  if (!nm %in% c("linear", "lm")) {
    return(default)
  }
  fit_classes <- c("lm", "glm", "gam", "lm2", "lm_robust")
  if (nm == "linear") {
    if (!exists(nm, envir = env, inherits = TRUE)) {
      return(list(handled = TRUE, model = NULL, estimate_linear = TRUE))
    }
    val <- tryCatch(get(nm, envir = env, inherits = TRUE), error = function(e) NULL)
    if (is.null(val)) {
      return(list(handled = TRUE, model = NULL, estimate_linear = TRUE))
    }
    if (inherits(val, fit_classes)) {
      return(list(handled = TRUE, model = val, estimate_linear = FALSE))
    }
    return(list(handled = TRUE, model = NULL, estimate_linear = TRUE))
  }
  if (!exists(nm, envir = env, inherits = TRUE)) {
    return(list(handled = TRUE, model = NULL, estimate_linear = TRUE))
  }
  val <- tryCatch(get(nm, envir = env, inherits = TRUE), error = function(e) NULL)
  if (is.null(val)) {
    return(list(handled = TRUE, model = NULL, estimate_linear = TRUE))
  }
  if (is.function(val) && identical(val, stats::lm)) {
    return(list(handled = TRUE, model = NULL, estimate_linear = TRUE))
  }
  if (inherits(val, fit_classes)) {
    return(list(handled = TRUE, model = val, estimate_linear = FALSE))
  }
  list(handled = TRUE, model = NULL, estimate_linear = TRUE)
}

ip_adjustcolor2 <- function(col, dark) {
  new_cols <- c()
  for (dj in dark) {
    rgb_val <- grDevices::col2rgb(col)
    new_rgb_val <- rgb_val * dj + (1 - dj) * 255
    new_rgb_val <- pmax(0, pmin(255, new_rgb_val))
    new_col <- grDevices::rgb(new_rgb_val[1], new_rgb_val[2], new_rgb_val[3], maxColorValue = 255)
    new_cols <- c(new_cols, new_col)
  }
  new_cols
}

ip_line_seg <- function(x, y, lwd, col, g, lty = 1, type = "l") {
  g <- pmax(g, 0.15)
  lwd <- pmax(lwd, 0.75)
  n <- length(x)
  for (k in 1:(n - 1)) {
    graphics::lines(
      x = c(x[k], x[k + 1]),
      y = c(y[k], y[k + 1]),
      type = type,
      lwd = lwd[k],
      col = ip_adjustcolor2(col, (g[k] + g[k + 1]) / 2),
      lty = lty
    )
  }
}

ip_get_breaks <- function(cut_var) {
  interval_matrix <- do.call(
    rbind,
    strsplit(gsub("\\[|\\]|\\(|\\)", "", levels(cut_var)), ",", fixed = TRUE)
  )
  df_intervals <- as.data.frame(interval_matrix, stringsAsFactors = FALSE)
  df_intervals$V1 <- as.numeric(df_intervals$V1)
  df_intervals$V2 <- as.numeric(df_intervals$V2)
  names(df_intervals) <- c("from", "to")
  df_intervals
}

ip_sync_factors <- function(data, ndj) {
  for (var in names(data)) {
    if (is.factor(data[[var]])) {
      ndj[[var]] <- factor(ndj[[var]], levels = levels(data[[var]]))
    }
  }
  ndj
}

ip_add_covariates_at_mean <- function(newdata, data) {
  missing_vars <- setdiff(names(data), names(newdata))
  mean_values <- list()

  for (var in missing_vars) {
    if (is.numeric(data[[var]])) {
      mean_values[[var]] <- mean(data[[var]], na.rm = TRUE)
    } else {
      mean_values[[var]] <- as.character(data[[var]][1])
    }
  }

  for (var in names(mean_values)) {
    newdata[[var]] <- mean_values[[var]]
  }

  newdata
}

ip_round2 <- function(x, min.d = 2, max.d = 3) {
  x_char <- as.character(x)
  fnz <- regexpr("[1-9]", sub(".*\\.", "", x_char))
  d_round <- max(min(max.d, fnz), min.d)
  rounded <- round(x, d_round)
  formatC(rounded, format = "f", digits = min.d)
}

ip_clean_string <- function(input_string) {
  gsub("[^A-Za-z0-9_]", "", input_string)
}

ip_is_integer2 <- function(x) all(floor(x) == x)

ip_check1 <- function(var, varname, length.check, type.check) {
  if (length(var) != length.check) {
    exit(
      paste0(
        "interprobe() says the argument '", varname, "' must be of length '",
        length.check, "'\n",
        "but it is of length '", length(var), "'"
      )
    )
  }

  if (type.check == "integer") {
    if (ip_is_integer2(var) == FALSE) {
      exit(paste0("interprobe() says the argument '", varname, "' must be an integer, but '", var, "' isn't."))
    }
  }

  if (type.check == "character") {
    if (is.character(var) == FALSE) {
      exit(paste0("interprobe() says the argument '", varname, "' must be a character variable but '", var, "' isn't."))
    }
  }

  if (type.check == "numeric") {
    if (is.numeric(var) == FALSE) {
      exit(paste0("interprobe() says the argument '", varname, "' must be a numeric, but '", var, "' isn't."))
    }
  }

  if (type.check == "logical") {
    if (is.logical(var) == FALSE) {
      exit(paste0("interprobe() says the argument '", varname, "' must be a logical, but '", var, "' isn't."))
    }
  }
}

ip_check_gam_error <- function(model) {
  if ("try-error" %in% class(model)) {
    message2(
      paste0(
        "interprobe() says: The GAM model could not be estimated. ",
        "This often occurs when predictors have few possible values. ",
        "Try setting the argument 'k' to a low value like k=3 or k=2."
      ),
      col = "red",
      stop = FALSE
    )
    exit("interprobe() stopped due to GAM estimation failure.")
  }
}

ip_validate_input_combinations <- function(data, model, x, y, z) {
  input.xz <- input.data <- input.xyz <- input.model <- FALSE

  if (!is.null(model) & !is.null(y)) {
    y <- NULL
    message("interprobe() says: You specified both 'model' and 'y', will ignore 'y' and use DV in the model.")
  }

  if (!is.null(data)) input.data <- TRUE
  if (!is.null(model)) input.model <- TRUE
  if (!is.null(x) & !is.null(z) & !is.null(y)) input.xyz <- TRUE
  if (!is.null(x) & !is.null(z) & is.null(y)) input.xz <- TRUE

  if (input.data + input.model == 2) {
    exit("interprobe() says: You may include either a data or a model argument, but you included both.")
  }

  if (input.data == TRUE & input.xyz == FALSE) {
    exit(
      paste0(
        "interprobe says: you specified a dataset but not x,y,z, recall that:\n",
        "x: focal predictor\n",
        "z: moderator\n",
        "y: dependent variable"
      )
    )
  }

  if (input.model == TRUE & input.xz == FALSE) {
    message(
      paste0(
        "interprobe says: you specified a model but not x and z, recall that:\n",
        "x: focal predictor\n",
        "z: moderator"
      )
    )
    exit("interprobe() stopped due to missing x/z variable names.")
  }

  if (input.model == TRUE & input.xyz == TRUE) {
    message("interprobe says: you specified both a model and the y argument.\nPlease specify only one of them")
    exit("interprobe() stopped due to incompatible inputs.")
  }

  if (input.data + input.xyz + input.model == 0) {
    message(
      paste0(
        "interprobe says:\nYou must specify the variables to be used through one of these 3 combinations:\n",
        "1) 'data' and variable names for 'x', 'z', 'y'\n",
        "2) vectors 'x', 'z', and 'y'\n",
        "3) 'model' and variable names 'x' and 'z'"
      )
    )
    exit("interprobe() stopped due to missing inputs.")
  }

  list2(input.data, input.xyz, input.model, input.xz)
}

ip_validate_arguments <- function(
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
) {
  if (!is.null(data)) {
    dataname <- deparse(substitute(data))
    if (!inherits(data, "data.frame")) {
      exit(paste0("interprobe says(): the 'data' argument must be a data.frame, but '", dataname, "' is not a data.frame."))
    }

    n1 <- names(data)
    if (!xvar %in% n1) exit(paste0("interprobe() says the focal variable ('", xvar, "') is not in the dataset '", dataname, "'."))
    if (!zvar %in% n1) exit(paste0("interprobe() says the moderator variable ('", zvar, "') is not in the dataset '", dataname, "'."))
    if (!yvar %in% n1) exit(paste0("interprobe() says the dependent variable ('", yvar, "') is not in the dataset '", dataname, "'."))
  }

  if (
    any(class(model) %in% c("logical", "integer", "numeric", "data.frame", "factor")) |
      any(class(x) %in% c("lm", "glm", "gam", "lm2", "lm_robust")) |
      any(class(z) %in% c("lm", "glm", "gam", "lm2", "lm_robust")) |
      any(class(y) %in% c("lm", "glm", "gam", "lm2", "lm_robust"))
  ) {
    exit(
      paste0(
        "interprobe() says:\n",
        "There is a problem with the set of arguments provided to the function.\n",
        "If you are providing a model as input, make sure to reference it explictly\n",
        "and to enter the variable names in quotes.\n",
        "\n   For example:\n      lm1=lm(y~cond*age)\n      interprobe(model=lm1,x='cond',z='age') "
      )
    )
  }

  if (is.null(model) & is.null(data)) {
    if (length(x) != length(z)) exit("interprobe says(): x and z must have the same length")
    if (length(x) != length(y)) exit("interprobe says(): x and y must have the same length")
  }

  if (!is.null(model)) {
    modelname <- attr(model, "interprobe_modelname")
    if (is.null(modelname) || !is.character(modelname) || length(modelname) != 1) {
      modelname <- deparse(substitute(model))
    }
    if (!inherits(model, c("lm", "glm", "gam", "lm2", "lm_robust"))) {
      exit("interprobe() says you provided a model but it is not lm, glm, gam, lm2, or lm_robust")
    }
    vars <- all.vars(terms(model))[-1]
    if (!xvar %in% vars) exit(paste0("interprobe() says the focal variable x ('", xvar, "') is not in the model '", modelname, "'"))
    if (!zvar %in% vars) exit(paste0("interprobe() says the moderator variable z ('", zvar, "') is not in the model '", modelname, "'"))
  }

  if (!is.null(k)) ip_check1(k, "k", 1, "integer")

  if (!is.null(spotlights) && length(spotlights) != 3) {
    exit("interprobe() says the argument 'spotlights' must be of length 3")
  }
  if (!is.null(spotlight.labels) & length(spotlight.labels) != 3) {
    exit("interprobe() says the argument 'spotlight.labels' must be  of length 3")
  }

  if (!is.logical(histogram) && length(histogram) == 1) {
    exit("interprobe() says, The argument 'histogram' must be TRUE or FALSE and of length 1")
  }

  ip_check1(max.unique, "max.unique", 1, "integer")
  ip_check1(n.bin.continuous, "n.bin.continuous", 1, "integer")
  ip_check1(n.max, "n.max", 1, "integer")

  ip_check1(xlab, "xlab", 1, "character")
  ip_check1(ylab1, "ylab1", 1, "character")
  ip_check1(ylab2, "ylab2", 1, "character")
  ip_check1(main1, "main1", 1, "character")
  ip_check1(main2, "main2", 1, "character")
  ip_check1(legend.round, "legend.round", 2, "integer")
  ip_check1(cols, "cols", 3, "character")

  draw2 <- draw
  if (identical(draw2, "simple.slopes")) draw2 <- "simple slopes"
  if (!draw2 %in% c("both", "simple slopes", "jn")) {
    exit(
      paste0(
        "interprobe() says that the argument 'draw' must be one of:\n",
        "  - 'both'\n",
        "  - 'simple slopes' (or legacy: 'simple.slopes')\n",
        "  - 'jn'"
      )
    )
  }

  if (!is.null(xlim)) ip_check1(xlim, "xlim", 2, "numeric")

  if (!is.null(save.as)) {
    extension <- tools::file_ext(save.as)
    if (!extension %in% c("svg", "png")) exit("interprobe() says 'save.as' must be either a .png or .svg format.")
  }

  if (!is.null(model)) {
    mf <- ip_model_frame(model)
    if (is.null(mf)) {
      exit("interprobe() says: could not recover the model frame from the fitted model (try stats::model.frame()).")
    }
    x_model <- mf[, xvar]
    nux <- length(unique(x_model))
    model_txt <- ip_clean_string(deparse(substitute(model)))

    if (nux <= 3 & !inherits(x_model, "factor")) {
      exit(
        paste0(
          "ERROR.\n   ",
          "interprobe() says the focal predictor x ('", xvar, "') has only \n   ",
          nux, " possible values. Make sure to define it as a factor before\n   ",
          "estimating the model '", model_txt, "'. You can do that by running\n   ",
          "df$", xvar, " <- factor(df$", xvar, "), where df is the name of the \n   ",
          "data frame containing  '", xvar, "'."
        )
      )
    }

    formula.txt <- paste0(as.character(model$call), collapse = " ")
    if (as.numeric(regexpr("factor\\(", formula.txt)) > 0) {
      exit(
        paste0(
          "interprobe() says: you defined a variable as factor\n   ",
          "within the '", model_txt, "' call. This creates problems.\n   ",
          "Please define factor variables as factors in \n   ",
          "the data before estimating the model \n   e.g., df$x <- factor(df$x))."
        )
      )
    }
  }

  if (!is.null(x.ticks) && !class(x.ticks) %in% c("numeric", "data.frame", "integer")) {
    exit(paste0("interprobe() says: the argument 'x.ticks' must be either a numeric vector or a dataframe, but it is '", class(x.ticks), "'."))
  }
  if (!is.null(y1.ticks) && !class(y1.ticks) %in% c("numeric", "data.frame", "integer")) {
    exit(paste0("interprobe() says: the argument 'y1.ticks' must be either a numeric vector or a dataframe, but it is '", class(y1.ticks), "'."))
  }
  if (!is.null(y2.ticks) && !class(y2.ticks) %in% c("numeric", "data.frame", "integer")) {
    exit(paste0("interprobe() says: the argument 'y2.ticks' must be either a numeric vector or a dataframe, but it is '", class(y2.ticks), "'."))
  }

  if (!is.null(y1.ticks) && inherits(y1.ticks, "data.frame") && ncol(y1.ticks) != 2) {
    exit(paste0("interprobe() says: the argument 'y1.ticks' must be a dataframe with 2 columns, but it has '", ncol(y1.ticks), "' columns."))
  }
  if (!is.null(y2.ticks) && inherits(y2.ticks, "data.frame") && ncol(y2.ticks) != 2) {
    exit(paste0("interprobe() says: the argument 'y2.ticks' must be a dataframe with 2 columns, but it has '", ncol(y2.ticks), "' columns."))
  }

  ip_check1(moderator.on.x.axis, "moderator.on.x.axis", 1, "logical")

  if (length(unique(c(xvar, zvar, yvar))) < 3) {
    exit("interprobe() says: you seem to have entered the same variable twice")
  }
}

ip_estimate_model <- function(nux, data, k, xvar, zvar, yvar, engine = c("gam", "lm2")) {
  engine <- match.arg(engine)

  if (engine == "lm2") {
    if (nux <= 3) {
      data[, xvar] <- factor(data[, xvar])
    }
    fo <- stats::as.formula(paste(yvar, "~", xvar, "*", zvar))
    return(lm2(fo, data = data, notes = FALSE))
  }

  k_if_specified <- ifelse(is.null(k), "", paste0(",k=", k))

  if (nux <= 3) {
    model.text <- paste0(
      'try(mgcv::gam(',
      yvar, "~",
      "s(", zvar, ",by=", xvar, k_if_specified, ")+",
      xvar,
      ', data=data,method="REML"),silent=TRUE)'
    )
    data[, xvar] <- factor(data[, xvar])
    model <- eval(parse(text = model.text))
    ip_check_gam_error(model)
  }

  if (nux >= 4) {
    model.text <- paste0(
      'try(mgcv::gam(',
      yvar, "~",
      "s(", zvar, k_if_specified, ")+",
      "s(", xvar, k_if_specified, ")+",
      "ti(", xvar, ",", zvar, k_if_specified, ')',
      ',method="REML", data=data),silent=TRUE)'
    )
    model <- eval(parse(text = model.text))
    ip_check_gam_error(model)
  }

  model
}

ip_format_p_apa <- function(p, digits = 3, eps = 1e-3) {
  if (!is.finite(p)) return("p = NA")
  if (p < eps) return("p < .001")
  paste0("p = ", formatC(p, format = "f", digits = digits))
}

ip_get_linear_interaction_test_apa <- function(data, xvar, zvar, yvar) {
  fo <- stats::as.formula(paste(yvar, "~", xvar, "*", zvar))
  fit <- tryCatch(lm2(fo, data = data, notes = FALSE), error = function(e) NULL)
  if (is.null(fit)) return("linear model: unavailable")
  tbl <- attr(fit, "statuser_table")
  if (is.null(tbl)) return("linear model: unavailable")
  term <- paste0(xvar, ":", zvar)
  idx <- which(tbl$term == term)
  if (length(idx) != 1) return("linear model: unavailable")
  tval <- as.numeric(tbl$t[idx])
  df <- as.numeric(tbl$df[idx])
  p <- as.numeric(tbl$p.value[idx])
  if (!is.finite(tval) || !is.finite(df)) return("linear model: unavailable")
  paste0(
    "linear model: t(", formatC(df, format = "f", digits = 0), ") = ",
    formatC(tval, format = "f", digits = 2), ", ",
    ip_format_p_apa(p), " (robust errors: HC3)"
  )
}

ip_get_linear_interaction_test_apa_from_lm2 <- function(fit, xvar, zvar) {
  if (is.null(fit)) return("linear model: unavailable")
  tbl <- attr(fit, "statuser_table")
  if (is.null(tbl)) return("linear model: unavailable")
  term <- paste0(xvar, ":", zvar)
  idx <- which(tbl$term == term)
  if (length(idx) != 1) return("linear model: unavailable")
  tval <- as.numeric(tbl$t[idx])
  df <- as.numeric(tbl$df[idx])
  p <- as.numeric(tbl$p.value[idx])
  if (!is.finite(tval) || !is.finite(df)) return("linear model: unavailable")
  paste0(
    "linear model: t(", formatC(df, format = "f", digits = 0), ") = ",
    formatC(tval, format = "f", digits = 2), ", ",
    ip_format_p_apa(p), " (robust errors: HC3)"
  )
}

ip_get_gam_interaction_test_apa <- function(model, xvar, zvar) {
  sm <- tryCatch(summary(model), error = function(e) NULL)
  if (is.null(sm) || is.null(sm$s.table)) return("GAM: unavailable")
  st <- sm$s.table
  rn <- rownames(st)
  if (is.null(rn)) return("GAM: unavailable")

  preferred <- c(
    paste0("ti(", xvar, ",", zvar, ")"),
    paste0("ti(", zvar, ",", xvar, ")")
  )
  idx <- match(preferred, rn)
  idx <- idx[!is.na(idx)]
  if (length(idx) < 1) {
    hits <- which(grepl(xvar, rn, fixed = TRUE) & grepl(zvar, rn, fixed = TRUE))
    if (length(hits) == 0) return("GAM: unavailable")
    idx <- hits
  }

  row <- idx[1]
  p <- as.numeric(st[row, "p-value"])
  edf <- if ("edf" %in% colnames(st)) as.numeric(st[row, "edf"]) else NA_real_
  refdf <- if ("Ref.df" %in% colnames(st)) as.numeric(st[row, "Ref.df"]) else NA_real_
  fval <- if ("F" %in% colnames(st)) as.numeric(st[row, "F"]) else NA_real_

  if (is.finite(fval) && is.finite(edf) && is.finite(refdf)) {
    return(paste0(
      "GAM: F(", formatC(edf, format = "f", digits = 2), ", ",
      formatC(refdf, format = "f", digits = 2), ") = ",
      formatC(fval, format = "f", digits = 2), ", ",
      ip_format_p_apa(p)
    ))
  }

  paste0("GAM: ", ip_format_p_apa(p))
}

ip_compute_slopes_continuous <- function(spotlights, data, xs, zs, model, xvar, zvar, moderator.on.x.axis) {
  simple.slopes <- list()
  j <- 1

  if (moderator.on.x.axis == FALSE) {
    for (zj in spotlights) {
      ndj <- expand.grid(z = zj, x = xs)
      names(ndj) <- c(zvar, xvar)
      ndj <- ip_add_covariates_at_mean(ndj, data)
      options(warn = -1)
      simple.slopes[[j]] <- marginaleffects::predictions(model, newdata = ndj)
      options(warn = 0)
      j <- j + 1
    }
    return(simple.slopes)
  }

  if (moderator.on.x.axis == TRUE) {
    for (xj in spotlights) {
      ndj <- expand.grid(z = zs, x = xj)
      names(ndj) <- c(zvar, xvar)
      ndj <- ip_add_covariates_at_mean(ndj, data)
      options(warn = -1)
      simple.slopes[[j]] <- marginaleffects::predictions(model, newdata = ndj)
      options(warn = 0)
      j <- j + 1
    }
    return(simple.slopes)
  }
}

ip_compute_slopes_discrete <- function(ux, zs, model, xvar, zvar) {
  simple.slopes <- list()
  j <- 1

  for (xj in ux) {
    ndj <- expand.grid(z = zs, x = xj)
    names(ndj) <- c(zvar, xvar)
    data <- ip_model_frame(model)
    ndj <- ip_add_covariates_at_mean(ndj, data)
    options(warn = -1)
    simple.slopes[[j]] <- marginaleffects::predictions(model, newdata = ndj, by = zvar)
    options(warn = 0)
    j <- j + 1
  }

  simple.slopes
}

ip_compute_jn_continuous <- function(spotlights, data, xs, zs, model, xvar, zvar, moderator.on.x.axis) {
  if (moderator.on.x.axis == FALSE) {
    jn <- list()
    j <- 1
    for (zj in spotlights) {
      ndj <- expand.grid(z = zj, x = xs)
      ndj <- ip_add_covariates_at_mean(ndj, data)
      names(ndj)[1:2] <- c(zvar, xvar)
      options(warn = -1)
      jn[[j]] <- data.frame(marginaleffects::slopes(model, newdata = ndj, var = xvar))
      jn[[j]][, zvar] <- zj
      options(warn = 0)
      j <- j + 1
    }
    return(jn)
  }

  if (moderator.on.x.axis == TRUE) {
    jn <- list()
    j <- 1
    for (xj in spotlights) {
      ndj <- expand.grid(x = xj, z = zs)
      ndj <- ip_add_covariates_at_mean(ndj, data)
      names(ndj)[1:2] <- c(xvar, zvar)
      options(warn = -1)
      jn[[j]] <- data.frame(marginaleffects::slopes(model, newdata = ndj, var = xvar))
      jn[[j]][, xvar] <- xj
      options(warn = 0)
      j <- j + 1
    }
    return(jn)
  }
}

ip_compute_jn_discrete <- function(ux, zs, model, xvar, zvar) {
  floodlight <- list()
  j <- 1

  for (xj in ux[-1]) {
    ndj <- expand.grid(z = zs, x = c(as.character(ux[1]), xj))
    names(ndj)[1:2] <- c(zvar, xvar)
    data <- ip_model_frame(model)
    ndj <- ip_add_covariates_at_mean(ndj, data)
    options(warn = -1)
    floodlight[[j]] <- data.frame(marginaleffects::slopes(model = model, newdata = ndj, by = zvar))
    floodlight[[j]][, xvar] <- xj
    options(warn = 0)
    floodlight[[j]] <- floodlight[[j]][floodlight[[j]]$term == xvar, , drop = FALSE]
    j <- j + 1
  }

  floodlight
}

ip_make_fxz <- function(data, n.bin.continuous, moderation, nux, max.unique, spotlights, xvar, zvar, moderator.on.x.axis) {
  uz <- sort(unique(data[, zvar]))
  nuz <- length(unique(data[, zvar]))

  if (nux > max.unique) {
    if (moderator.on.x.axis == FALSE) {
      xbins <- cut(
        data[, xvar],
        n.bin.continuous,
        include.lowest = TRUE,
        labels = paste0("xbin_", 1:(n.bin.continuous))
      )
      x1bins <- cut(data[, xvar], n.bin.continuous, include.lowest = TRUE)

      cuts <- c()
      cuts[1] <- (spotlights[1] + spotlights[2]) / 2
      cuts[2] <- (spotlights[3] + spotlights[2]) / 2
      zbins <- cut(data[, zvar], breaks = c(-Inf, cuts, Inf), labels = paste0("zbin_", 1:3), include.lowest = TRUE)
      fxz <- table(xbins, zbins)
      return(list2(fxz, x1bins))
    }

    if (moderator.on.x.axis == TRUE) {
      if (nuz > max.unique) {
        xbins <- cut(
          data[, zvar],
          n.bin.continuous,
          include.lowest = TRUE,
          labels = paste0("xbin_", 1:(n.bin.continuous))
        )
        x1bins <- cut(data[, zvar], n.bin.continuous, include.lowest = TRUE)
      }
      if (nuz <= max.unique) {
        xbins <- data[, zvar]
        x1bins <- data[, zvar]
      }

      cuts <- c()
      cuts[1] <- (spotlights[1] + spotlights[2]) / 2
      cuts[2] <- (spotlights[3] + spotlights[2]) / 2
      zbins <- cut(data[, xvar], breaks = c(-Inf, cuts, Inf), labels = paste0("zbin_", 1:3), include.lowest = TRUE)
      fxz <- table(xbins, zbins)
      return(list2(fxz, x1bins))
    }
  }

  if (nux > 3 & nux <= max.unique) {
    cuts <- c()
    cuts[1] <- (spotlights[1] + spotlights[2]) / 2
    cuts[2] <- (spotlights[3] + spotlights[2]) / 2
    zbins <- cut(data[, zvar], breaks = c(-Inf, cuts, Inf), labels = paste0("zbin_", 1:3), include.lowest = TRUE)
    fxz <- table(data[, xvar], zbins)
    return(list2(fxz))
  }

  if (nux <= 3 & moderation == "continuous") {
    zbins <- cut(
      data[, zvar],
      n.bin.continuous,
      include.lowest = TRUE,
      labels = paste0("zbin_", 1:(n.bin.continuous))
    )
    x1bins <- cut(data[, zvar], n.bin.continuous, include.lowest = TRUE)
    fxz <- table(zbins, data[, xvar])
    return(list2(fxz, x1bins))
  }

  if (nux <= 3 & moderation == "discrete") {
    fxz <- t(table(data[, xvar], data[, zvar]))
    return(list(fxz = fxz))
  }
}

ip_draw_histogram <- function(fxz.list, focal, moderation, x1s, nux1, cols, ylim, xlim, max.unique) {
  breaks <- NULL

  fxz <- fxz.list$fxz
  rs <- rowSums(fxz)
  rs <- ifelse(rs > 1000000, paste0(round(rs / 1000000, 1), "M"), rs)
  rs <- ifelse(rs > 1000, paste0(round(rs / 1000, 1), "k"), rs)

  x.axis.bins <- nrow(fxz)
  lines.total <- ncol(fxz)

  y0 <- par("usr")[3]
  x0 <- par("usr")[1]
  x1 <- par("usr")[2]
  yd <- y0 + (0.09 + lines.total * 0.07) * diff(ylim) * 0.85

  graphics::polygon(
    x = c(x0, x0, x1, x1),
    border = NA,
    y = c(y0, yd, yd, y0),
    col = grDevices::adjustcolor("gray77", 0.25)
  )
  xc <- (x1 + x0) / 2
  yd2 <- (yd - y0) * 1.05 + y0
  graphics::text(xc, yd2, "Number of Observations", font = 3, cex = 1.1, pos = 1)

  if (nux1 > max.unique) {
    breaks <- ip_get_breaks(fxz.list$x1bins)
    y0 <- par("usr")[3]
    y1 <- y0 + 0.05 * lines.total * diff(ylim)

    fxz2 <- apply(fxz, 1, cumsum)
    fxz2 <- fxz2 / max(fxz2[nrow(fxz2), ])
    fxz2 <- rbind(rep(0, ncol(fxz2)), fxz2)
    fxz2 <- fxz2 * (y1 - y0) + y0

    for (j in 1:x.axis.bins) {
      for (m in 1:lines.total) {
        x1s2 <- c(breaks$from[j], breaks$from[j], breaks$to[j], breaks$to[j])
        ys <- c(fxz2[m, j], fxz2[m + 1, j], fxz2[m + 1, j], fxz2[m, j])
        graphics::polygon(x = x1s2, y = ys, col = ip_adjustcolor2(cols[m], 0.6))
      }
    }

    graphics::text(rowMeans(breaks), y1, rs, pos = 3, cex = 0.8, font = 3, col = "gray38")
  }

  if (nux1 <= max.unique) {
    y0 <- par("usr")[3]
    y1 <- y0 + 0.05 * lines.total * diff(ylim)
    fxz2 <- (fxz / max(fxz)) * (y1 - y0) + y0

    x.width <- (x1s[2] - x1s[1])
    for (j in 1:lines.total) {
      xj <- x1s + x.width * (j - 2) * 0.1
      graphics::segments(x0 = xj, x1 = xj, y0 = y0, y1 = fxz2[, j], lwd = 4, col = cols[j])
    }
    graphics::text(x1s, y1, rs, pos = 3, cex = 0.8, font = 3, col = "gray38")
  }

  breaks
}

ip_make_plot <- function(
  type, xlab, ylab, main, res, histogram, data, xs, zs, gr, spotlights, cols, spotlight.labels,
  focal, moderation, max.unique, fxz.list, nux, nuz, xvar, zvar, xlim, ylim, legend.title,
  x.ticks, y.ticks, moderator.on.x.axis
) {
  ylim.set.by.user <- !is.null(ylim)

  if (moderator.on.x.axis == FALSE) {
    if (xlab == "") xlab <- xvar
    n.lines <- length(spotlights)
    x1.range <- range(data[, xvar])
    x1s <- xs
    nux1 <- nux
    x1.axis <- "x"
  }

  if (moderator.on.x.axis == TRUE) {
    if (xlab == "") xlab <- zvar

    if (type == "simple.slopes" & focal != "categorical") n.lines <- length(spotlights)
    if (type == "simple.slopes" & focal == "categorical") n.lines <- nux

    if (type == "jn" & focal == "categorical") n.lines <- nux - 1
    if (type == "jn" & focal != "categorical") n.lines <- length(spotlights)

    x1.range <- range(data[, zvar])
    x1s <- zs
    nux1 <- nuz
    x1.axis <- "z"
  }

  if (!is.null(y.ticks) & inherits(y.ticks, "data.frame")) {
    yline.adj <- max(nchar(y.ticks[, 2])) / 3.25
    m0 <- par("mar")
    m1 <- m0
    m1[2] <- m1[2] + yline.adj
    par(mar = m1)
  }

  res.df <- do.call(rbind, res)

  if (is.null(ylim)) ylim <- range(res.df[, c("conf.low", "conf.high")])
  ylim1 <- ylim[1]
  ylim2 <- ylim[2]

  if (type == "jn" & !ylim.set.by.user) {
    if (ylim1 * ylim2 > 0) {
      if (ylim1 > 0) ylim[1] <- 0 - 0.02 * diff(ylim)
      if (ylim1 < 0) ylim[2] <- 0 + 0.02 * diff(ylim)
    }
  }

  if (!ylim.set.by.user) ylim[2] <- ylim[2] + 0.3 * diff(ylim)
  if (histogram == TRUE & !ylim.set.by.user) ylim[1] <- ylim[1] - (0.15 + n.lines * 0.07) * diff(ylim)

  if (is.null(xlim)) xlim <- x1.range

  xaxt <- "s"
  yaxt <- "s"
  if (!is.null(x.ticks)) xaxt <- "n"
  if (!is.null(y.ticks)) yaxt <- "n"

  ylab.plotted <- ylab
  if (!is.null(y.ticks)) ylab.plotted <- ""

  graphics::plot(
    x1s, res[[1]]$estimate,
    type = "n",
    xlab = xlab,
    ylab = ylab.plotted,
    las = 1,
    ylim = ylim,
    xlim = xlim,
    yaxt = "n",
    cex.lab = 1.3,
    font.lab = 2,
    xaxt = xaxt,
    yaxt = yaxt
  )

  if (yaxt == "s") graphics::axis(2, at = pretty(ylim)[c(-1, -2)], las = 1)

  if (!is.null(x.ticks)) {
    if (!is.data.frame(x.ticks)) graphics::axis(side = 1, at = x.ticks)
    if (is.data.frame(x.ticks)) graphics::axis(side = 1, at = x.ticks[, 1], x.ticks[, 2])
  }

  if (!is.null(y.ticks)) {
    if (!is.data.frame(y.ticks)) graphics::axis(side = 2, at = y.ticks, las = 1)
    if (is.data.frame(y.ticks)) graphics::axis(side = 2, at = y.ticks[, 1], y.ticks[, 2], las = 1)
  }

  if (type == "jn") {
    graphics::axis(2, at = 0, las = 1)
    graphics::abline(h = 0, lty = 2, col = "gray77")
  }

  col.seg <- cols
  if (focal == "categorical" & type == "jn") col.seg <- col.seg[-1]

  for (j in 1:n.lines) {
    g1 <- as.numeric(gr[, j])
    if (nux1 > max.unique) g <- rep(g1, each = length(x1s) / length(g1))
    if (nux1 <= max.unique) g <- g1

    ip_line_seg(x1s, res[[j]]$estimate, lwd = 4 * g, col = col.seg[j], g = g)

    graphics::polygon(
      x = c(x1s, rev(x1s)),
      y = c(res[[j]]$conf.high, rev(res[[j]]$conf.low)),
      col = grDevices::adjustcolor(col.seg[j], 0.1),
      border = NA
    )

    if (nux1 <= max.unique) {
      graphics::points(x1s, res[[j]]$estimate, col = ip_adjustcolor2(col.seg[j], pmax(g, 0.2)), pch = 16, cex = 1.5)
    }
  }

  graphics::mtext(side = 3, line = 1.5, font = 2, cex = 1.5, main)

  if (x1.axis == "x") {
    if (is.null(legend.title)) legend.title <- zvar
    graphics::legend(
      "top",
      inset = 0.01,
      bty = "n",
      lwd = 8,
      col = cols[1:n.lines],
      title = legend.title,
      title.font = 2,
      legend = spotlight.labels
    )
  }

  if (x1.axis == "z") {
    if (is.null(legend.title)) legend.title <- paste0("Focal predictor ('", xvar, "')")

    if (type == "simple.slopes" & focal == "categorical") {
      graphics::legend("top", inset = 0.01, bty = "n", lwd = 5, col = cols[1:n.lines], title = legend.title, title.font = 2, legend = xs)
    }

    if (type == "simple.slopes" & focal != "categorical") {
      graphics::legend("top", inset = 0.01, bty = "n", lwd = 5, col = cols[1:n.lines], title = legend.title, title.font = 2, legend = spotlight.labels)
    }

    if (type == "jn" & focal == "categorical") {
      graphics::legend("top", inset = 0.01, bty = "n", lwd = 5, col = cols[-1], title = legend.title, title.font = 2, legend = paste0(xs[-1], " - ", xs[1]))
    }

    if (type == "jn" & focal == "continuous") {
      graphics::legend("top", inset = 0.01, bty = "n", lwd = 5, col = cols, title = legend.title, title.font = 2, legend = spotlight.labels)
    }
  }

  if (!is.null(y.ticks)) graphics::mtext(side = 2, line = par("mar")[2] - 1, font = 2, cex = 1.5, ylab)

  breaks <- NULL
  if (histogram == TRUE) {
    breaks <- ip_draw_histogram(fxz.list, focal, moderation = "continuous", x1s, nux1, cols, ylim, xlim, max.unique)
  }

  breaks
}

ip_get_regions_jn <- function(j, xvar, zvar, focal, probe.bins) {
  j$k <- 1:nrow(j)
  js <- j[j$conf.low * j$conf.high > 0, ]
  output <- "Johnson-Neyman Regions of Significance\n"

  if (nrow(js) == 0) {
    output <- paste0(output, "The effect of ", xvar, " is not significant for any value of the moderator considered")
    return(output)
  }

  if (focal != "categorical") {
    breaks <- c(TRUE, diff(js$k) != 1 | diff(js$z) != 0 | diff(sign(js$marginal.effect)) != 0)
  }

  if (focal == "categorical") {
    same.contrast <- c(js$contrast[-1] == js$contrast[-nrow(js)])
    breaks <- c(TRUE, diff(js$k) != 1 | !same.contrast | diff(sign(js$marginal.effect)) != 0)
  }

  js$batch <- cumsum(breaks)
  j2 <- do.call(
    rbind,
    by(js, js$batch, function(sub_j) {
      sub_j[c(1, nrow(sub_j)), ]
    })
  )

  if (focal != "categorical") {
    uz <- unique(j2$z)
    k <- 0
    for (zk in uz) {
      j3 <- j2[j2[, zvar] == zk, ]
      xs <- j3[, xvar]
      xs <- data.frame(matrix(xs, ncol = 2, byrow = TRUE))
      names(xs) <- c("from.x", "to.x")
      sk <- sign(j3$marginal.effect)
      signk <- sk[seq(1, length(sk), by = 2)]
      xs$sign.text <- ifelse(signk == 1, "positive", "negative")

      if (nrow(xs) > 0) {
        for (rowk in 1:nrow(xs)) {
          k <- k + 1
          xr <- xs[rowk, ]
          output <- paste0(
            output,
            k, ") When '", zvar, "' = ", round(zk, 2), ", the effect of '", xvar, "' is ", xr$sign.text,
            " in the range of '", xvar, "': [", round(xr$from.x, 2), " to ", round(xr$to.x, 2), "]\n"
          )
        }
      }
    }
  }

  if (focal == "categorical") {
    ucon <- unique(j2$contrast)
    k <- 0
    for (conk in ucon) {
      j3 <- j2[j2[, "contrast"] == conk, ]
      zs <- j3[, zvar]
      zs <- data.frame(matrix(zs, ncol = 2, byrow = TRUE))
      names(zs) <- c("from.z", "to.z")
      sk <- sign(j3$marginal.effect)
      signk <- sk[seq(1, length(sk), by = 2)]
      zs$sign.text <- ifelse(signk == 1, "positive", "negative")

      conk.clean <- gsub("mean\\(", "", conk)
      conk.clean <- gsub("\\)", "", conk.clean)

      if (nrow(zs) > 0) {
        for (rowk in 1:nrow(zs)) {
          k <- k + 1
          zr <- zs[rowk, ]
          output <- paste0(
            output,
            k, ") The contrast for '", xvar, "' of ", conk.clean, " is ", zr$sign.text,
            " in the range of '", zvar, "': [", round(zr$from.z, 2), " to ", round(zr$to.z, 2), "]\n"
          )
        }
      }
    }
  }

  output <- paste0(
    output,
    "\n\nNotes:\n",
    "1) Regions of significance reported by interprobe() never include values outside\n",
    "range of observed data.\n",
    "2) These regions are precise to 1/", probe.bins, " of the range of x-values. For \n",
    "greater precision (and slower estimation) set 'probe.bins' to a value greater than '", probe.bins, "'."
  )

  output
}

