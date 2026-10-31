# Examples: categorical plot_freq() with a larger data frame
# 1. Setup
# 2. Build a larger categorical data frame
# 3. plot_freq() statements for each x.order / usage

# 1. Setup
  if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(quiet = TRUE)
  } else {
    library(statuser)
  }

  if (interactive()) par(ask = TRUE)

# 2. Larger categorical data frame
  set.seed(26)
  n <- 500
  df <- data.frame(
    party = sample(c("Dem", "Rep", "Ind"), n, replace = TRUE, prob = c(0.40, 0.35, 0.25)),
    region = sample(c("Northeast", "South", "Midwest", "West"), n, replace = TRUE,
                    prob = c(0.20, 0.35, 0.25, 0.20)),
    approve = sample(c("Approve", "Disapprove", "DK"), n, replace = TRUE, prob = c(0.45, 0.40, 0.15)),
    stringsAsFactors = FALSE
  )
  df$party_f <- factor(df$party, levels = c("Rep", "Dem", "Ind"))
  df$region_f <- factor(df$region, levels = c("West", "Midwest", "South", "Northeast"))

# 3. plot_freq() statements
  # Character default (alphabetical)
  plot_freq(party, data = df)

  # Factor default (level order)
  plot_freq(party_f, data = df)

  # Force alphabetical on a factor
  plot_freq(party_f, data = df, x.order = "alpha")

  # Most -> least frequent
  plot_freq(region, data = df, x.order = "freq")

  # Least -> most frequent
  plot_freq(region, data = df, x.order = "freq.asc")

  # Reverse factor-level default
  plot_freq(party_f, data = df, x.order = -1)

  # Custom category order
  plot_freq(approve, data = df, x.order = c("Disapprove", "DK", "Approve"))

  # Formula syntax
  plot_freq(approve ~ 1, data = df, x.order = "freq")

  # Grouped categorical
  plot_freq(approve ~ party, data = df)

  # Grouped + x.order + group order
  plot_freq(region ~ party, data = df, x.order = "freq", order = c("Dem", "Rep", "Ind"))

  # Factor outcome with level order, grouped
  plot_freq(region_f ~ party_f, data = df)

  # Two-vector comparison
  plot_freq(df$party[df$region == "South"], df$party[df$region == "West"],
            x.order = "freq")
