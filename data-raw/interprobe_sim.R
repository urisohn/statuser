# 1. Setup
  set.seed(20241030)
  n <- 400

# 2. Simulate nonlinear interaction
  # treat effect near 0 at low mod, rises then flattens: 2*(1-exp(-mod/3))
  treat <- rbinom(n, 1, 0.5)
  mod <- runif(n, 0, 10)
  true_effect <- 2 * (1 - exp(-mod / 3))
  y <- 5 + 0.3 * mod + treat * true_effect + rnorm(n, 0, 1.2)

# 3. Package as data frame and save
  interprobe_sim <- data.frame(
    treat = factor(treat, levels = c(0, 1), labels = c("Control", "Treated")),
    mod = mod,
    y = y
  )
  dir.create("data", showWarnings = FALSE)
  save(
    interprobe_sim,
    file = file.path("data", "interprobe_sim.rda"),
    compress = "xz",
    version = 2
  )
