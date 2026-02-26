# Demo: NSE (Non-Standard Evaluation) Support for Plot Functions
# Now you can reference columns with or without quotes!

library(statuser)

# Create example data
set.seed(123)
df <- data.frame(
  score = rnorm(100, mean = 75, sd = 10),
  grade = sample(c("A", "B", "C"), 100, replace = TRUE),
  pass = sample(c("Pass", "Fail"), 100, replace = TRUE)
)

cat("=== Both syntaxes work identically! ===\n\n")

# Old way (quoted) - still works
cat("1. Quoted column names (old way):\n")
plot_density('score', data = df)
plot_cdf('score', data = df)  
plot_freq('pass', data = df)

# New way (unquoted) - now works!
cat("\n2. Unquoted column names (new way):\n")
plot_density(score, data = df)
plot_cdf(score, data = df)
plot_freq(pass, data = df)

# Two-vector comparison
cat("\n3. Two-vector comparison with unquoted names:\n")
score_a <- df$score[df$grade == "A"]
score_b <- df$score[df$grade == "B"]
plot_density(score_a, score_b)

# Formula syntax still works as before
cat("\n4. Formula syntax (unchanged):\n")
plot_density(score ~ grade, data = df)

cat("\nAll done! NSE support is working.\n")
