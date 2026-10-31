# 1. Setup
  set.seed(20260727)
  n <- 1000L

# 2. Predictors
  # spicy: continuous on [1, 7] (paper Case 2 / package Case C)
  spicy <- stats::runif(n, min = 1, max = 7)
  # tolerance: roughly 1-7, slightly more mass near the middle
  tolerance <- pmin(7, pmax(1, round(stats::rnorm(n, mean = 4, sd = 1.35), 1)))
  # nuisance covariate for fitted-model examples
  age <- sample(18:65, n, replace = TRUE)

# 3. Mean enjoyment (paper-like match: prefer spicy ≈ tolerance - 1)
  # At tolerance = 5, preferred spiciness = 4 (midway between Mild=1 and Hot=7),
  # so Hot−Mild effect crosses zero near 5 (JN2 tutorial pattern).
  # Soft capped mismatch keeps means well above 0 so plots are not dominated
  # by the enjoyment floor (negative y-hat looks pathological).
  preferred <- tolerance - 1
  mismatch <- pmin(1, ((spicy - preferred) / 6)^2)
  # match → ~88; strong mismatch → ~35 (well above 0, still visibly low)
  mu <- 35 + 53 * (1 - mismatch)
  # small age nuisance (does not drive the interaction)
  mu <- mu + 0.05 * (age - mean(age))
  enjoyment <- mu + stats::rnorm(n, mean = 0, sd = 7)
  enjoyment <- pmin(100, pmax(15, enjoyment))

# 4. Derived factors for Cases A and B
  salsa <- cut(
    spicy,
    breaks = c(1, 3, 5, 7),
    include.lowest = TRUE,
    labels = c("Mild", "Medium", "Hot")
  )
  # Binary ends: near Mild (≤1.5) vs near Hot (≥6.5), matching paper 1 vs 7
  spicy_bin <- factor(
    ifelse(spicy <= 1.5, "Mild", ifelse(spicy >= 6.5, "Hot", NA_character_)),
    levels = c("Mild", "Hot")
  )

# 5. Assemble and save
  tacos_data <- data.frame(
    spicy = spicy,
    salsa = salsa,
    spicy_bin = spicy_bin,
    tolerance = tolerance,
    enjoyment = enjoyment,
    age = as.integer(age)
  )
  dir.create("data", showWarnings = FALSE)
  save(
    tacos_data,
    file = file.path("data", "tacos_data.rda"),
    compress = "xz",
    version = 2
  )
