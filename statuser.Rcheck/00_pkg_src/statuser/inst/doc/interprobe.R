## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,
  fig.height = 6,
  fig.align = "center"
)
library(statuser)

## ----sim-demo-----------------------------------------------------------------
data(interprobe_sim)
# True effect at a few moderator values
mod_grid <- c(1, 3, 5, 9)
true_eff <- 2 * (1 - exp(-mod_grid / 3))
cbind(mod = mod_grid, true_effect = round(true_eff, 2))

interprobe(
  x = treat, z = mod, y = y,
  data = interprobe_sim,
  quiet = TRUE
)

## ----sim-linear---------------------------------------------------------------
interprobe(
  x = treat, z = mod, y = y,
  data = interprobe_sim,
  model = "linear",
  quiet = TRUE
)

## ----birthwt------------------------------------------------------------------
bw <- MASS::birthwt
bw$smoke <- factor(bw$smoke, levels = c(0, 1), labels = c("No", "Yes"))
bw$race <- factor(bw$race, levels = c(1, 2, 3),
                  labels = c("White", "Black", "Other"))

## ----case-a-------------------------------------------------------------------
res_a <- interprobe(x = smoke, z = age, y = bwt, data = bw)

## ----case-b-------------------------------------------------------------------
res_b <- interprobe(x = race, z = age, y = bwt, data = bw)

## ----case-c-------------------------------------------------------------------
res_c <- interprobe(x = age, z = lwt, y = bwt, data = bw)

## ----case-c-spotlights--------------------------------------------------------
interprobe(
  x = age, z = lwt, y = bwt, data = bw,
  spotlights = c(110, 130, 160),
  spotlight.labels = c("Light (110)", "Typical (130)", "Heavy (160)"),
  quiet = TRUE
)

## ----case-c-axis--------------------------------------------------------------
interprobe(
  x = age, z = lwt, y = bwt, data = bw,
  moderator.on.x.axis = FALSE,
  quiet = TRUE
)

## ----covariates---------------------------------------------------------------
fit <- lm(bwt ~ smoke * age + lwt, data = bw)
interprobe(model = fit, x = smoke, z = age, quiet = TRUE)

## ----gam-vs-linear, fig.show = "hold"-----------------------------------------
interprobe(x = smoke, z = age, y = bwt, data = bw, quiet = TRUE)
interprobe(x = smoke, z = age, y = bwt, data = bw, model = "linear", quiet = TRUE)

## ----returned-list------------------------------------------------------------
res <- interprobe(
  x = smoke, z = age, y = bwt, data = bw,
  draw = "none", quiet = TRUE
)
names(res)
head(res$johnson.neyman)
head(res$simple.slopes)

## ----inspect-model------------------------------------------------------------
summary(res$gam_results)

## ----customize, eval = FALSE--------------------------------------------------
# interprobe(
#   x = smoke, z = age, y = bwt, data = bw,
#   draw = "jn",                    # or "simple slopes", "both", "none"
#   cols = c("black", "steelblue"),
#   save.as = "smoke_age_jn.png",
#   xlim = c(15, 40),
#   quiet = TRUE
# )

