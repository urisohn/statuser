# Two-line test (same as https://webstimate.org/twolines 0.52)
# Last update 2025 12 23
# Written by Uri Simonsohn (urisohn@gmail.com)
#


#
# FUNCTIONS DEFINED IN THIS SCRIPT:
# 1. twolines() - Main function: Tests for u-shaped relationships using two-line regression
# 2. reg2() - Helper function: Performs interrupted regression with robust standard errors


#' Two-Lines Test of U-Shapes
#'
#'Implements the two-lines test for U-shaped (or inverted U-shaped) relationships 
#'introduced by Simonsohn (2018).
#'
#' Reference: 
#' Simonsohn, Uri (2018) "Two lines: A valid alternative to the invalid testing of 
#' U-shaped relationships with quadratic regressions." AMPPS, 538-555. 
#' \doi{10.1177/2515245918805755}
#'
#' @param f A formula object specifying the model (e.g., y ~ x1 + x2 + x3).
#'   The first predictor is the one tested for a u-shaped relationship.
#' @param graph Integer. If 1 (default), produces a plot. If 0, no plot is generated.
#' @param link Character string specifying the link function for the GAM model.
#'   Default is "gaussian".
#' @param data An optional data frame containing the variables in the formula.
#'   If not provided, variables are evaluated from the calling environment.
#' @param pngfile Optional character string. If provided, saves the plot to a PNG file
#'   with the specified filename.
#' @param quiet Logical. If TRUE, suppresses the Robin Hood details messages.
#'   Default is FALSE.
#'
#' @return A list containing:
#' \itemize{
#'   \item All elements from \code{reg2()}: \code{b1}, \code{b2}, \code{p1}, \code{p2},
#'     \code{z1}, \code{z2}, \code{u.sig}, \code{xc}, \code{glm1}, \code{glm2}, \code{rob1},
#'     \code{rob2}, \code{msg}, \code{yhat.smooth}
#'   \item \code{yobs}: Observed y values (adjusted for covariates if present)
#'   \item \code{y.hat}: Fitted values from GAM
#'   \item \code{y.ub}, \code{y.lb}: Upper and lower bounds for fitted values
#'   \item \code{y.most}: Most extreme fitted value
#'   \item \code{x.most}: x-value associated with most extreme fitted value
#'   \item \code{f}: Formula as character string
#'   \item \code{bx1}, \code{bx2}: Linear and quadratic coefficients from preliminary quadratic regression
#'   \item \code{minx}: Minimum x value
#'   \item \code{midflat}: Median of flat region
#'   \item \code{midz1}, \code{midz2}: Z-statistics at midpoint
#' }
#'
#' @details
#' The test beings fitting a GAM model, predicting y with a smooth of x, and optionally with covariates. 
#' It identifies the interior most extreme value of fitted y, and adjusts from the matching x-value 
#' to set the breakpoint relying on the Robin Hood procedure introduced also by Simonsohn (2018).
#' It then estimates the (once) interrupted regression using that breakpoint,
#' and reports the slope and significance of the average slopes at either side of it. A U-shape 
#' is significant if the slopes are of opposite sign and are both individually significant.
#' 

#'
#' @examples
#' \donttest{
#' # Simple example with simulated data
#' set.seed(123)
#' x <- rnorm(100)
#' y <- -x^2 + rnorm(100)
#' data <- data.frame(x = x, y = y)
#' result <- twolines(y ~ x, data = data)
#'
#' # With covariates
#' z <- rnorm(100)
#' y <- -x^2 + 0.5*z + rnorm(100)
#' data <- data.frame(x = x, y = y, z = z)
#' result <- twolines(y ~ x + z, data = data)
#' 
#' # Without data argument (variables evaluated from environment)
#' x <- rnorm(100)
#' y <- -x^2 + rnorm(100)
#' result <- twolines(y ~ x)
#' }
#'
#' @importFrom mgcv gam
#' @export
twolines <- function(f, graph = 1, link = "gaussian", data = NULL, pngfile = "", quiet = FALSE) {
  #OUTLINE
  #1. Validate inputs
  #2. Extract variable names from formula
  #3. Drop missing values
  #4. Grab key variables
  #5. Set up GAM formula for smoothing
  #6. Run GAM smoother
  #7. Generate yobs (adjusted for covariates if present)
  #8. Get fitted values and standard errors from GAM
  #9. Determine shape (u-shaped vs inverted u-shaped) using quadratic regression
  #10. Find middle 80% of data and restrict analysis to that range
  #11. Find most extreme fitted value and flat regions
  #12. Run preliminary two-line regression at midpoint
  #13. Adjust breakpoint using Robin Hood procedure
  #14. Run final two-line regression
  #15. Combine and return results
  
  #1. Validate inputs and handle data
    # Get calling environment for evaluating variables
    calling_env <- parent.frame()
    
    # Extract variable names from formula
    vars <- all.vars(f)
    y.f <- vars[1]  # DV
    x.f <- vars[2]  # Variable on which the u-shape shall be tested
    
    # Number of variables
    var.count <- length(vars)  # How many predictors in addition to the key predictor?
    
    # If data is NULL, create it from the calling environment
    if (is.null(data)) {
      # Check if variables exist before evaluating
      missing_vars <- c()
      for (var in vars) {
        if (!exists(var, envir = calling_env, inherits = TRUE)) {
          missing_vars <- c(missing_vars, var)
        }
      }
      
      if (length(missing_vars) > 0) {
        if (length(missing_vars) == 1) {
          stop(sprintf("twolines(): Could not find variable '%s'", missing_vars[1]), call. = FALSE)
        } else {
          stop(sprintf("twolines(): Could not find variables: %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
        }
      }
      
      # Variables exist, create data frame from environment
      # Create a list first, then convert to data frame
      data_list <- list()
      for (var in vars) {
        data_list[[var]] <- eval(as.name(var), envir = calling_env)
      }
      data <- as.data.frame(data_list)
    } else {
      # Data provided: validate it's a data frame
      if (!is.data.frame(data)) {
        stop("twolines(): 'data' must be a data frame", call. = FALSE)
      }
    }
    
    # Entire model, except the first predictor
    if (var.count > 2) {
      nox.f <- drop.terms(terms(f), dropx = 1, keep.response = TRUE)
    }
  
  #3. Drop missing values
    # Vector with columns associated with those variable names in the dataset
    cols <- c()
    for (var in vars) {
      if (!var %in% names(data)) {
        stop(sprintf("twolines(): Variable '%s' not found in dataset", var), call. = FALSE)
      }
      cols <- c(cols, which(names(data) == var))
    }
    
    # Set of complete observations
    full.rows <- complete.cases(data[, cols, drop = FALSE])
    
    # Drop missing rows
    data <- data[full.rows, , drop = FALSE]
  
  #4. Grab key variables
    xu <- data[[x.f]]  # xu is the key predictor predicted to be u-shaped
    yu <- data[[y.f]]  # yu is the dv
  
  #5. Set up GAM formula for smoothing
    # Count number of unique x values
    unique.x <- length(unique(xu))   # How many unique values x has
    
    # New function segment for x
    sx.f <- paste0("s(", x.f, ",bs='cr', k=min(10,", unique.x, "))")
  
  #6. Run GAM smoother
    # Define the formula to be run based on whether there are covariates
    if (var.count > 2) {
      gam.f <- paste0(format(nox.f), "+", sx.f)      # with covariates
    }
    if (var.count == 2) {
      gam.f <- paste0(y.f, " ~", sx.f)                  # without
    }
    
    # Now run it
    gams <- mgcv::gam(as.formula(gam.f), data = data, family = link)  # so this is a general additive model with the main specification entered
    # but we make the first predictor, the one that will be tested for having a u-shaped effect
    # be estimated with a completely flexible functional form.
  
  #7. Generate yobs (adjusted for covariates if present)
    # If no covariates, yobs is the actually observed data
    if (var.count == 2) {
      yobs <- yu
    }
    
    # If covariates present, yobs is the fitted value with u(x) at mean, need new.data() with variables at means
    if (var.count > 2) {
      # Put observed data into data frame
      data.obs <- data[, all.vars(f), drop = FALSE]
      
      # Drop observations with missing values on any of the variables
      data.obs <- na.omit(data.obs)
      
      # Create data where xu is at sample means to get residuals based on rest of models to act as yobs
      data.xufixed <- data.obs
      data.xufixed[[x.f]] <- mean(data.obs[[x.f]])   # Note, the 1st predictor is always the one hypothesized to be u-shaped
      # replace it with the mean value of the predictor
      
      # Get yobs with covariates
      # First the fitted value
      yhat.xufixed <- mgcv::predict.gam(gams, newdata = data.xufixed)        # get fitted values at means for covariates
      
      # Subtract fitted value from observed y
      yobs <- yu - yhat.xufixed
      
      # Create data where u(x) is obs, and all else at sample means
      data.otherfixed <- data.obs     # start with original value
      
      # Replace all covariates with their mean for fitting data at sample means
      for (i in 3:var.count) {
        var.name <- all.vars(f)[i]
        data.otherfixed[[var.name]] <- mean(data.obs[[var.name]])
      }
    }  # End if covariates are present to compute yobs
  
  #8. Get fitted values and standard errors from GAM
    # Get predicted values into list
    if (var.count > 2) {
      g.fit <- mgcv::predict.gam(gams, newdata = data.otherfixed, se.fit = TRUE)  # predict with covariates at means
    }
    if (var.count == 2) {
      g.fit <- mgcv::predict.gam(gams, se.fit = TRUE)
    }
    
    # Take out the fitted itself
    y.hat <- g.fit$fit
    
    # Now the SE
    y.se <- g.fit$se.fit
  
  #9. Determine shape (u-shaped vs inverted u-shaped) using quadratic regression
    # Determine if function is at first decreasing (potential u-shape) vs. increasing (potentially inverted U)
    # using quadratic regression to know if we are looking for max or min
    xu2 <- xu^2                                                  # Square x term, xu is the 1st predictor
    # Create data frame for quadratic regression
    qdata <- data.frame(xu = xu, xu2 = xu2)
    qdata[[y.f]] <- yu
    if (var.count > 2) {
      # Add covariates
      for (var in all.vars(f)[-(1:2)]) {
        qdata[[var]] <- data[[var]]
      }
      lmq.f <- update(nox.f, ~ xu + xu2 + .)           # Add to function with covariates (put first, before covariates)
    }
    if (var.count == 2) {
      lmq.f <- as.formula(paste0(y.f, " ~ xu + xu2"))
    }
    lmq <- lm(lmq.f, data = qdata)            # Estimate the quadratic regression
    bqs <- lmq$coefficients                   # Get the point estimates
    bx1 <- bqs[2]                             # point estimate for effect of x
    bx2 <- bqs[3]                             # point estimate for effect of x^2
    x0 <- min(xu)                             # lowest x-value
    s0 <- bx1 + 2 * bx2 * x0                  # estimated slope at the lowest x-value
    if (s0 > 0) {
      shape <- 'inv-ushape'                   # if the quadratic is increasing at the lowest point, the could be inverted u-shape
    }
    if (s0 <= 0) {
      shape <- 'ushape'                       # if it is decreasing, then it could be a regular u-shape
    }
  
  #10. Find middle 80% of data and restrict analysis to that range
    # Get the middle 80% of data to avoid an extreme cutoff
    x10 <- quantile(xu, .1)
    x90 <- quantile(xu, .9)
    middle <- (xu > x10 & xu < x90)       # Don't consider extreme values for cutoff
    x.middle <- xu[middle]
    
    # Restrict y.hat to middle
    y.hat <- y.hat[middle]
    y.se <- y.se[middle]
  
  #11. Find most extreme fitted value and flat regions
    # Find upper and lower band
    y.ub <- y.hat + y.se            # +SE is for flat max
    y.lb <- y.hat - y.se            # -SE is for flat min
    
    # Find most extreme y-hat
    if (shape == 'inv-ushape') {
      y.most <- max(y.hat)   # if potentially inverted u-shape, use the highest y-hat as the most extreme
    }
    if (shape == 'ushape') {
      y.most <- min(y.hat)   # if potential u-shaped, then the lowest instead
    }
    
    # x-value associated with the most extreme value
    x.most <- x.middle[match(y.most, y.hat)]
    
    # Find flat regions
    if (shape == 'inv-ushape') {
      flat <- (y.ub > y.most)
    }
    if (shape == 'ushape') {
      flat <- (y.lb < y.most)
    }
    xflat <- x.middle[flat]
  
  #12. Run preliminary two-line regression at midpoint
    # First an interrupted regression at the midpoint of the flat region
    rmid <- reg2(f, xc = median(xflat), graph = 0, data = data)  # Two line regression at the median point of flat maximum
    
    # Get z1 and z2, statistical strength of both lines at the midpoint
    z1 <- abs(rmid$z1)
    z2 <- abs(rmid$z2)
  
  #13. Adjust breakpoint using Robin Hood procedure
    # Adjust breakpoint based on z1, z2
    xc <- quantile(xflat, z2 / (z1 + z2))
    
    # Print Robin Hood details (unless quiet = TRUE)
    if (!quiet) {
      xcn <- round(as.numeric(xc), 2)
      qp <- z2 / (z1 + z2)
      
      # Header message with font=2
      message2("Robin Hood calculations (see Simonsohn, 2018)", col = "blue4", font = 2)
      
      # Combine all details into a single message
          msg <- paste0(
            "Explaining why ", x.f, " = ", xcn, " is used as the breakpoint\n\n",
            "Most extreme value of fitted '", y.f, "' with GAM obtained at '", x.f, "' = ", round(x.most, 2), "\n",
            "Local range of values considered for breakpoint '", x.f, "': [", round(min(xflat), 2), ", ", round(max(xflat), 2), "]\n",
            "t-values for two lines at '", x.f, "' = ", round(x.most, 2), ":\n",
            "   t1 = ", round(z1, 2), "\n",
            "   t2 = ", round(z2, 2), "\n",
            "We compute t2/(t1+t2) = ", round(qp, 2), "\n",
            "We then lookup the value of '", x.f, "' at that quantile, so we run\n",
            "quantile(", x.f, ", ", round(qp, 2), ") = ", round(as.numeric(xc), 2), 
            " and set that value, ", round(as.numeric(xc), 2), ", as the breakpoint."
          )
          message2(msg, col = "blue4")
      
      message2("\nNote: you may turn off this message by setting `quiet=TRUE`", col = "red4")
      
    }
  
  #14. Run final two-line regression
    # Save to png? (option set at the beginning by giving png a name)
    if (pngfile != "") {
      png(pngfile, width = 2000, height = 1500, res = 300)
    }
    
    # Run the two lines
    res <- reg2(f, xc = xc, graph = graph, data = data)
    
    # Save to png? (close)
    if (pngfile != "") {
      dev.off()
    }
  
  #15. Combine and return results
    # Add other results obtained before to the output (some of these are read by the server and included in the app)
    res$yobs <- yobs
    res$y.hat <- y.hat
    res$y.ub <- y.ub
    res$y.lb <- y.lb
    res$y.most <- y.most
    res$x.most <- x.most
    res$f <- format(f)
    res$bx1 <- bx1           # linear effect in quadratic regression
    res$bx2 <- bx2           # quadratic
    res$minx <- min(xu)       # lowest x value
    res$midflat <- median(xflat)
    res$midz1 <- abs(rmid$z1)
    res$midz2 <- abs(rmid$z2)
    
    invisible(res)
}  # End function


#' Two-Line Interrupted Regression
#'
#' Performs an interrupted regression analysis with heteroskedastic robust standard errors.
#' This function fits two regression lines with a breakpoint at a specified value of the
#' first predictor variable.
#'
#' @param f A formula object specifying the model (e.g., y ~ x1 + x2 + x3).
#'   The first predictor is the one on which the u-shape is tested.
#' @param xc Numeric value specifying where to set the breakpoint.
#' @param graph Integer. If 1 (default), produces a plot. If 0, no plot is generated.
#' @param family Character string specifying the family for the GLM model.
#'   Default is "gaussian" for OLS regression. Use "binomial" for probit models.
#' @param data A data frame containing the variables in the formula.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{b1}, \code{b2}: Slopes of the two regression lines
#'   \item \code{p1}, \code{p2}: P-values for the slopes
#'   \item \code{z1}, \code{z2}: Z-statistics for the slopes
#'   \item \code{u.sig}: Indicator (0/1) for whether u-shape is significant
#'   \item \code{xc}: The breakpoint value
#'   \item \code{glm1}, \code{glm2}: The fitted GLM models
#'   \item \code{rob1}, \code{rob2}: Robust coefficient test results
#'   \item \code{msg}: Messages about standard error computation
#'   \item \code{yhat.smooth}: Fitted smooth values (if graph=1)
#' }
#'
#' @details
#' This function fits two interrupted regression lines with heteroskedastic robust
#' standard errors using the sandwich package. The first predictor variable is split
#' at the breakpoint \code{xc}, creating separate slopes before and after the breakpoint.
#'
#' @importFrom mgcv gam
#' @importFrom sandwich vcovHC
#' @importFrom lmtest coeftest
#' @keywords internal
reg2 <- function(f, xc, graph = 1, family = "gaussian", data = NULL) {
  #OUTLINE
  #1. Validate data
  #2. Extract variable names from formula
  #3. Grab key variables
  
  #1. Validate data
    if (is.null(data)) {
      stop("reg2(): 'data' argument is required", call. = FALSE)
    }
    if (!is.data.frame(data)) {
      stop("reg2(): 'data' must be a data frame", call. = FALSE)
    }
  
  #2. Extract variable names from formula
  #3. Set up GAM formula for smoothing (accommodates discrete values)
  #4. Create interrupted regression variables (xlow1, xhigh1, high1, xlow2, xhigh2, high2)
  #5. Generate formulas for two regression lines
  #6. Run GLM regressions
  #7. Compute robust standard errors (HC3, fallback to HC1 if needed)
  #8. Extract slopes, z-statistics, and p-values
  #9. Determine if u-shape is significant
  #10. Plot results (if graph=1)
  #11. Return results
  
  #1. Extract variable names from formula
    y.f <- all.vars(f)[1]  # DV
    x.f <- all.vars(f)[2]  # Variable on which the u-shape shall be tested
    
    # Number of variables
    var.count <- length(all.vars(f))  # How many total variables, including y and x
    
    # Entire model, except the first predictor
    if (var.count > 2) {
      nox.f <- drop.terms(terms(f), dropx = 1, keep.response = TRUE)
    }
  
  #2. Grab key variables
    xu <- data[[x.f]]  # xu is the key predictor predicted to be u-shaped
    yu <- data[[y.f]]  # yu is the dv
  
  #3. Set up GAM formula for smoothing (accommodates discrete values)
    # Replace formula for key predictor so that it accommodates possibly discrete values
    # gam() breaks down if x has few possible values unless one restricts it, done automatically
    unique.x <- length(unique(xu))  # How many unique values x has
    sx.f <- paste0("s(", x.f, ",bs='cr', k=min(10,", unique.x, "))")
  
  #4. Create interrupted regression variables
    # xc is included in the first line
    xlow1 <- ifelse(xu <= xc, xu - xc, 0)   # xlow = x - xc when x < xc, 0 otherwise
    xhigh1 <- ifelse(xu > xc, xu - xc, 0)    # xhigh = x - xc when x > xc, 0 otherwise
    high1 <- ifelse(xu > xc, 1, 0)           # high dummy, allows interruption
    
    # Now include xc in second line
    xlow2 <- ifelse(xu < xc, xu - xc, 0)
    xhigh2 <- ifelse(xu >= xc, xu - xc, 0)
    high2 <- ifelse(xu >= xc, 1, 0)
  
  #5. Generate formulas for two regression lines
    # Create data frame with interrupted regression variables
    reg.data <- data.frame(
      xlow1 = xlow1, xhigh1 = xhigh1, high1 = high1,
      xlow2 = xlow2, xhigh2 = xhigh2, high2 = high2
    )
    reg.data[[y.f]] <- yu
    
    # Add covariates if present
    if (var.count > 2) {
      for (var in all.vars(f)[-(1:2)]) {
        reg.data[[var]] <- data[[var]]
      }
      glm1.f <- update(nox.f, ~ xlow1 + xhigh1 + high1 + .)
      glm2.f <- update(nox.f, ~ xlow2 + xhigh2 + high2 + .)
    }
    
    # If there were no covariates, just run the 3 variable model
    if (var.count == 2) {
      glm1.f <- as.formula(paste0(y.f, " ~ xlow1 + xhigh1 + high1"))
      glm2.f <- as.formula(paste0(y.f, " ~ xlow2 + xhigh2 + high2"))
    }
  
  #6. Run GLM regressions
    glm1 <- glm(glm1.f, data = reg.data, family = family)
    glm2 <- glm(glm2.f, data = reg.data, family = family)
  
  #7. Compute robust standard errors (HC3, fallback to HC1 if needed)
    # Compute robust standard errors
    rob1 <- lmtest::coeftest(glm1, vcov = sandwich::vcovHC(glm1, "HC3"))
    rob2 <- lmtest::coeftest(glm2, vcov = sandwich::vcovHC(glm2, "HC3"))
    
    # Sometimes HC3 gives NA values (for very sparse or extreme data), check and if that's the case change method
    msg <- ""
    if (is.na(rob1[2, 4])) {
      rob1 <- lmtest::coeftest(glm1, vcov = sandwich::vcovHC(glm1, "HC1"))
      msg <- paste0(msg, "\nFor line 1 the heteroskedastic standard errors HC3 resulted in an error thus we used HC1 instead.")
    }
    if (is.na(rob2[2, 4])) {
      rob2 <- lmtest::coeftest(glm2, vcov = sandwich::vcovHC(glm2, "HC1"))
      msg <- paste0(msg, "\nFor line 2 the heteroskedastic standard errors HC3 resulted in an error thus we used HC1 instead.")
    }
  
  #8. Extract slopes, z-statistics, and p-values
    # Slopes
    b1 <- as.numeric(rob1[2, 1])
    b2 <- as.numeric(rob2[3, 1])
    
    # Test statistics, z-values
    z1 <- as.numeric(rob1[2, 3])
    z2 <- as.numeric(rob2[3, 3])
    
    # p-values
    p1 <- as.numeric(rob1[2, 4])
    p2 <- as.numeric(rob2[3, 4])
  
  #9. Determine if u-shape is significant
    u.sig <- ifelse(b1 * b2 < 0 & p1 < .05 & p2 < .05, 1, 0)
  
  #10. Plot results (if graph=1)
    if (graph == 1) {
      # Set up colors and parameters
      pch.dot <- 1          # Dot for scatterplot (data)
      col.l1 <- 'dodgerblue3'
      col.l2 <- 'firebrick'      # Color of straight line 2
      col.fit <- 'gray50'   # Color of fitted smooth line
      col.div <- "green3"   # Color of vertical line
      lty.l1 <- 1           # Type of line 1
      lty.l2 <- 1           # Type of line 2
      lty.fit <- 2          # Type of smoothed line
      
      # Estimate smoother
      if (var.count > 2) {
        gam.f <- paste0(format(nox.f), "+", sx.f)   # add the modified smoother version of x into the formula
        gams <- mgcv::gam(as.formula(gam.f), data = data, family = family)  # now actually run the smoother
      }
      
      if (var.count == 2) {
        gams <- mgcv::gam(as.formula(paste0(y.f, " ~", sx.f)), data = data, family = family)  # now actually run the smoother
      }
      
      # Get dots of raw data
      # If no covariates, there are two variables, and y.dots is the y values
      if (var.count == 2) {
        yobs <- yu
      }
      
      # If covariates present, yobs is the fitted value with u(x) at mean, need new.data() with variables at means
      if (var.count > 2) {
        # Put observed data into data frame
        data.obs <- data[, all.vars(f), drop = FALSE]
        
        # Drop observations with missing values on any of the variables
        data.obs <- na.omit(data.obs)
        
        # Create data where u(x) is at sample means to get residuals based on rest of models to act as yobs
        data.xufixed <- data.obs
        data.xufixed[[x.f]] <- mean(data.obs[[x.f]])   # Note, the 1st predictor is always the one hypothesized to be u-shaped
        # replace it with the mean value of the predictor
        
        # Create data where u(x) is obs, and all else at sample means
        data.otherfixed <- data.obs     # start with original value
        
        # Replace all RHS with mean, except the u(x)
        # changed on 2018 11 23 to allow having factors() as predictors, their "midpoint" value is used
        for (i in 3:var.count) {  # loop over covariates
          var.name <- all.vars(f)[i]
          xt <- sort(data.obs[[var.name]])  # create auxiliary variable that has those values sorted
          n <- length(xt)          # see how many observations there are
          xm <- xt[round(n/2, 0)]   # take the midpoint value (this will work with ordinal and factor data, but with factor it can be arbitrary)
          data.otherfixed[[var.name]] <- xm  # Replace with that value in the dataset used for fitted value
        }
        
        # Get yobs with covariates
        # First the fitted value
        yhat.xufixed <- mgcv::predict.gam(gams, newdata = data.xufixed)
        
        # Subtract fitted value from observed y, and shift it with constant so that it has same mean as original y
        yobs <- yu - yhat.xufixed
        yobs <- yobs + mean(yu) - mean(yobs)  # Adjust to have the same mean
      }  # End if for covariates that requires computes y.obs instead of using real y.
      
      # Get yhat.smooth
      # Without covariates, just fit the observed data
      if (var.count == 2) {
        yhat.smooth <- mgcv::predict.gam(gams)
      }
      
      # With covariates, fit at observed means
      if (var.count > 2) {
        yhat.smooth <- mgcv::predict.gam(gams, newdata = data.otherfixed)
      }
      
      # Subtract fitted value from observed y
      offset3 <- mean(yobs - yhat.smooth)
      yhat.smooth <- yhat.smooth + offset3
      
      # Coordinates for top and bottom end of chart
      y1 <- max(yobs, yhat.smooth)  # highest point
      y0 <- min(yobs, yhat.smooth)  # lowest point
      yr <- y1 - y0                  # range
      y0 <- y0 - .3 * yr               # new lowest. 30% lower
      
      # xs
      x1 <- max(xu)
      x0 <- min(xu)
      xr <- x1 - x0
      
      # Plot
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(mar = c(5.4, 4.1, .5, 2.1))
      plot(xu[xu < xc], yobs[xu < xc], cex = .75, col = col.l1, pch = pch.dot, las = 1,
           ylim = c(y0, y1),
           xlim = c(min(xu), max(xu)),
           xlab = "",
           ylab = "")  # Range of y has extra 30% to add labels
      
      points(xu[xu > xc], yobs[xu > xc], cex = .75, col = col.l2)
      
      # Axis labels
      mtext(side = 1, line = 2.75, x.f, font = 2)
      mtext(side = 2, line = 2.75, y.f, font = 2)
      
      # Smoothed line
      lines(xu[order(xu)], yhat.smooth[order(xu)], col = col.fit, lty = 2, lwd = 2)
      
      # Arrow 1
      xm1 <- (xc + x0) / 2
      x0.arrow.1 <- xm1 - .1 * xr
      x1.arrow.1 <- xm1 + .1 * xr
      y0.arrow.1 <- y0 + .1 * yr
      y1.arrow.1 <- y0 + .1 * yr + b1 * (x1.arrow.1 - x0.arrow.1)
      
      # Move arrow if it is too short
      if (x0.arrow.1 < x0 + .1 * xr) {
        x0.arrow.1 <- x0
      }
      
      # Move arrow if it covers text
      gap.1 <- (min(y0.arrow.1, y1.arrow.1) - (y0 + .1 * yr))
      if (gap.1 < 0) {
        y0.arrow.1 <- y0.arrow.1 - gap.1
        y1.arrow.1 <- y1.arrow.1 - gap.1
      }
      
      arrows(x0 = x0.arrow.1, x1 = x1.arrow.1, y0 = y0.arrow.1, y1 = y1.arrow.1, col = col.l1, lwd = 2)
      
      # Text under arrow 1
      xm1 <- max(xm1, x0 + .20 * xr)
      text(xm1, y0 + .025 * yr,
           paste0("Average slope 1:\nb = ", round(b1, 2), ", z = ", round(z1, 2), ", ", format_pvalue(p1, include_p = TRUE)),
           col = col.l1)
      
      # Arrow 2
      x0.arrow.2 <- xc + (x1 - xc) / 2 - .1 * xr
      x1.arrow.2 <- xc + (x1 - xc) / 2 + .1 * xr
      y0.arrow.2 <- y1.arrow.1
      y1.arrow.2 <- y0.arrow.2 + b2 * (x1.arrow.2 - x0.arrow.2)
      
      gap.2 <- (min(y0.arrow.2, y1.arrow.2) - (y0 + .1 * yr))
      if (gap.2 < 0) {
        y0.arrow.2 <- y0.arrow.2 - gap.2
        y1.arrow.2 <- y1.arrow.2 - gap.2
      }
      
      # Shorten arrow if it is too close to the end
      x1.arrow.2 <- min(x1.arrow.2, x1)
      if (x0.arrow.2 < xc) {
        x0.arrow.2 <- xc
      }
      
      xm2 <- xc + (x1 - xc) / 2
      xm2 <- min(xm2, x1 - .2 * xr)
      
      arrows(x0 = x0.arrow.2, x1 = x1.arrow.2, y0 = y0.arrow.2, y1 = y1.arrow.2, col = col.l2, lwd = 2)
      text(xm2, y0 + .025 * yr,
           paste0("Average slope 2:\nb = ", round(b2, 2), ", z = ", round(z2, 2), ", ", format_pvalue(p2, include_p = TRUE)),
           col = col.l2)
      
      # Division line
      lines(c(xc, xc), c(y0 + .35 * yr, y1), col = col.div, lty = lty.fit)
      text(xc, y0 + .3 * yr, round(xc, 2), col = col.div)
    }  # End: if graph == 1
  
  #11. Return results
    # list with results
    res <- list(b1 = b1, p1 = p1, b2 = b2, p2 = p2, u.sig = u.sig, xc = xc, z1 = z1, z2 = z2,
                glm1 = glm1, glm2 = glm2, rob1 = rob1, rob2 = rob2, msg = msg)
    
    if (graph == 1) {
      res$yhat.smooth <- yhat.smooth
    }
    
    # output it
    res
}  # End of reg2() function


