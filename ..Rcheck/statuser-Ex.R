pkgname <- "statuser"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "statuser-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('statuser')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("clear")
### * clear

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: clear
### Title: Clear Plot, Global Environment, and Console
### Aliases: clear

### ** Examples

## No test: 
# Interactive use: clear workspace, console, and plots
# First run may prompt; once you type "yes", your preference is saved.
clear()
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("clear", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_to_sql")
### * convert_to_sql

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_to_sql
### Title: Convert CSV file to SQL INSERT statements
### Aliases: convert_to_sql

### ** Examples

# Convert a CSV file to SQL (INSERT statements only)
tmp_csv <- tempfile(fileext = ".csv")
tmp_sql <- tempfile(fileext = ".sql")
write.csv(
  data.frame(id = 1:2, value = c("a", "b"), date = c("2024-01-01", "2024-02-02")),
  tmp_csv,
  row.names = FALSE
)
convert_to_sql(tmp_csv, tmp_sql)

# Convert a CSV file to SQL with CREATE TABLE statement
convert_to_sql(tmp_csv, tmp_sql, create_table = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_to_sql", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("desc_var")
### * desc_var

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: desc_var
### Title: Describe a variable, optionally by groups
### Aliases: desc_var

### ** Examples

# With grouping
df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
desc_var(y, group, data = df)

# Without grouping (full dataset)
desc_var(y, data = df)

# Direct vectors
y <- rnorm(100)
group <- rep(c("A", "B"), 50)
desc_var(y, group)

# With custom decimal places
desc_var(y, group, data = df, digits = 2)

# Using formula syntax: y ~ x
desc_var(y ~ group, data = df)

# Using formula syntax with multiple grouping variables: y ~ x1 + x2
df2 <- data.frame(y = rnorm(200), x1 = rep(c("A", "B"), 100), x2 = rep(c("X", "Y"), each = 100))
desc_var(y ~ x1 + x2, data = df2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("desc_var", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("format_pvalue")
### * format_pvalue

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: format_pvalue
### Title: Format P-Values for Display
### Aliases: format_pvalue

### ** Examples

# Basic usage
format_pvalue(0.05)
format_pvalue(0.0001)

# More rounding
format_pvalue(0.0001,digits=2)

# Vector input
format_pvalue(c(0.05, 0.001, 0.00001, 0.99))

# With p prefix
format_pvalue(0.05, include_p = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("format_pvalue", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("list2")
### * list2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: list2
### Title: Enhanced alternative to list()
### Aliases: list2

### ** Examples

x <- 1:5
y <- letters[1:3]
z <- matrix(1:4, nrow = 2)

# Create named list from objects
my_list <- list2(x, y, z)
names(my_list)  # "x" "y" "z"

# Works with explicit names too
my_list2 <- list2(a = x, b = y)
names(my_list2)  # "a" "b"




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("list2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lm2")
### * lm2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lm2
### Title: Enhanced alternative to lm()
### Aliases: lm2

### ** Examples

# Basic usage with data argument
lm2(mpg ~ wt + hp, data = mtcars)

# Without data argument (variables from environment)
y <- mtcars$mpg
x1 <- mtcars$wt
x2 <- mtcars$hp
lm2(y ~ x1 + x2)

# RED FLAG EXAMPLES

# Example 1: red flag catches a nonlinearity
# True model is quadratic: y = x^2
set.seed(123)
x <- runif(200, -3, 3)
y <- x^2 + rnorm(200, sd = 2)

# lm2() shows red flag due to misspecification
lm2(y ~ x)

# Follow up with scatter.gam() to diagnose it
scatter.gam(x, y)

# Example 2: red flag catches an outlier in y
# True model is y = x, but one observation has a very large y value
set.seed(123)
x <- sort(rnorm(200))
y <- round(x + rnorm(200, sd = 2), 1)
y[200] <- 100  # Outlier

# lm2() flags x
lm2(y ~ x)

# Look at distribution of y to spot the outlier
plot_freq(y)

# Example 3: red flag catches an outlier in one predictor
# True model is y = x1 + x2, but x2 has an extreme value
set.seed(123)
x1 <- round(rnorm(200),.1)
x2 <- round(rnorm(200),.1)
y <- x1 + x2 + rnorm(200, sd = 0.5)
x2[200] <- 50  # Outlier in x2

# lm2() flags x2 (but not x1)
lm2(y ~ x1 + x2)

# Look at distribution of x2 to spot the outlier
plot_freq(x2)

# CLUSTERED STANDARD ERRORS
# When observations are grouped (e.g., students within schools),
# use clusters to account for within-group correlation
set.seed(123)
n_clusters <- 20
n_per_cluster <- 15
cluster_id <- rep(1:n_clusters, each = n_per_cluster)
cluster_effect <- rnorm(n_clusters, sd = 2)[cluster_id]
x <- rnorm(n_clusters * n_per_cluster)
y <- 1 + 0.5 * x + cluster_effect + rnorm(n_clusters * n_per_cluster)
mydata <- data.frame(y = y, x = x, cluster_id = cluster_id)

# Clustered SE (CR2) - note the SE.cluster column in output
lm2(y ~ x, data = mydata, clusters = cluster_id)

# FIXED EFFECTS
# Use fixed_effects to absorb group-level variation (e.g., firm or year effects)
# This is useful for panel data or when you have many fixed effect levels
set.seed(456)
n_firms <- 30
n_years <- 5
firm_id <- rep(1:n_firms, each = n_years)
year <- rep(2018:2022, times = n_firms)
firm_effect <- rnorm(n_firms, sd = 3)[firm_id]
x <- rnorm(n_firms * n_years)
y <- 2 + 0.8 * x + firm_effect + rnorm(n_firms * n_years)
panel <- data.frame(y = y, x = x, firm_id = factor(firm_id), year = factor(year))

# Absorb firm fixed effects (coefficient on x is estimated, firm dummies are not shown)
lm2(y ~ x, data = panel, fixed_effects = ~ firm_id)

# Two-way fixed effects (firm and year)
lm2(y ~ x, data = panel, fixed_effects = ~ firm_id + year)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lm2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("message2")
### * message2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: message2
### Title: Enhanced alternative to message()
### Aliases: message2

### ** Examples

message2("This is a plain cyan message", col = "cyan", font = 1)
message2("This is a bold cyan message", col = "cyan", font = 2)
message2("This is a bold red message", col = "red", font = 2)
## No test: 
cat("this will be shown")
try(message2("This stops execution", stop = TRUE), silent = TRUE)
cat("this will be shown after the try")
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("message2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_cdf")
### * plot_cdf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_cdf
### Title: Plot Empirical Cumulative Distribution Functions by Group
### Aliases: plot_cdf

### ** Examples

# Basic usage with single variable (no grouping)
y <- rnorm(100)
plot_cdf(y)

# Basic usage with formula syntax and grouping
group <- rep(c("A", "B", "C"), c(30, 40, 30))
plot_cdf(y ~ group)

# With custom colors (scalar - same for all)
plot_cdf(y ~ group, col = "blue")

# With custom colors (vector - different for each group)
plot_cdf(y ~ group, col = c("red", "green", "blue"))

# Multiple parameters
plot_cdf(y ~ group, col = c("red", "green", "blue"), lwd = c(1, 2, 3))

# With line type and point character
plot_cdf(y ~ group, col = c("red", "green", "blue"), lty = c(1, 2, 3), lwd = 2)

# Using data frame
df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
plot_cdf(value ~ group, data = df)
plot_cdf(value ~ group, data = df, col = c("red", "blue"))

# Compare two vectors
y1 <- rnorm(50)
y2 <- rnorm(50, mean = 1)
plot_cdf(y1, y2)

# Formula syntax without data (variables evaluated from environment)
widgetness <- rnorm(100)
gender <- rep(c("M", "F"), 50)
plot_cdf(widgetness ~ gender)

# Using the returned object
df <- data.frame(value = c(rnorm(50, 0), rnorm(50, 1)), group = rep(c("A", "B"), each = 50))
result <- plot_cdf(value ~ group, data = df)

# Use ECDF to find P(X <= 0.5) for group A
result$ecdfs[[1]](0.5)

# Access KS test p-value
result$ks_test$p.value

# Summarize median quantile regression
summary(result$quantile_regression_50)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_cdf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_density")
### * plot_density

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_density
### Title: Plot density of a variable, optionally by another variable
### Aliases: plot_density

### ** Examples

# Basic usage with formula syntax (no grouping)
y <- rnorm(100)
plot_density(y)

# With grouping variable
group <- rep(c("A", "B", "C"), c(30, 40, 30))
plot_density(y ~ group)

# With custom colors (scalar - same for all)
plot_density(y ~ group, col = "blue")

# With custom colors (vector - different for each group)
plot_density(y ~ group, col = c("red", "green", "blue"))

# Multiple parameters
plot_density(y ~ group, col = c("red", "green", "blue"), lwd = c(1, 2, 3))

# With line type
plot_density(y ~ group, col = c("red", "green", "blue"), lty = c(1, 2, 3), lwd = 2)

# Using data frame
df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
plot_density(value ~ group, data = df)
plot_density(value ~ group, data = df, col = c("red", "blue"))

# Compare two vectors
y1 <- rnorm(50)
y2 <- rnorm(50, mean = 1)
plot_density(y1, y2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_density", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_freq")
### * plot_freq

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_freq
### Title: Plot frequencies for a variable (histogram without binning)
### Aliases: plot_freq

### ** Examples

# Simple example
x <- c(1, 1, 2, 2, 2, 5, 5)
plot_freq(x)

# Pass on some common \code{plot()} arguments
plot_freq(x, col = "steelblue", xlab = "Value", ylab = "Frequency",ylim=c(0,7))

# Add to an existing plot
plot_freq(x, col = "dodgerblue")


# Compare two vectors
y1 <- c(1, 1, 2, 2, 2, 5, 5)
y2 <- c(1, 2, 2, 3, 3, 3)
plot_freq(y1, y2)

# Using a data frame with grouping
df <- data.frame(value = c(1, 1, 2, 2, 2, 5, 5), group = c("A", "A", "A", "B", "B", "A", "B"))
plot_freq(value ~ 1, data = df)  # single variable
plot_freq(value ~ group, data = df)  # with grouping

# Control group order in legend and plot
plot_freq(value ~ group, data = df, order = c("B", "A"))  # B first, then A
plot_freq(value ~ group, data = df, order = -1)  # Reverse default order




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_freq", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_gam")
### * plot_gam

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_gam
### Title: Plot GAM Model
### Aliases: plot_gam

### ** Examples

## No test: 
library(mgcv)
# Fit a GAM model
data(mtcars)
mtcars$cyl <- factor(mtcars$cyl)  # Convert to factor before fitting GAM
model <- gam(mpg ~ s(hp) + s(wt) + cyl, data = mtcars)

# Plot effect of hp (with other variables at median)
plot_gam(model, "hp")

# Plot effect of hp (with other variables at 25th percentile)
plot_gam(model, "hp", quantile.others = 25)

# Customize plot
plot_gam(model, "hp", main = "Effect of Horsepower", col = "blue", lwd = 2)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_gam", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_means")
### * plot_means

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_means
### Title: Barplot of means
### Aliases: plot_means

### ** Examples

df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
plot_means(y ~ group, data = df)

df2 <- data.frame(
  y = rnorm(200),
  x1 = rep(c("A", "B"), 100),
  x2 = rep(c("X", "Y"), each = 100)
)
plot_means(y ~ x1 + x2, data = df2)

df3 <- data.frame(
  y = rnorm(600),
  x1 = rep(c("control", "treatment"), times = 300),
  x2 = rep(rep(c("low", "high"), each = 150), times = 2),
  x3 = rep(c("online", "lab"), each = 300)
)
plot_means(y ~ x1 + x2 + x3, data = df3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_means", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("resize_images")
### * resize_images

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: resize_images
### Title: Resize Images
### Aliases: resize_images

### ** Examples

## No test: 
# Create a temporary PNG file and resize it
tmp_png <- tempfile(fileext = ".png")
grDevices::png(tmp_png, width = 400, height = 300)
old_par <- graphics::par(no.readonly = TRUE)
graphics::par(mar = c(2, 2, 1, 1))
graphics::plot(1:2, 1:2, type = "n")
grDevices::dev.off()
graphics::par(old_par)
resize_images(tmp_png, width = 80)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("resize_images", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("scatter.gam")
### * scatter.gam

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: scatter.gam
### Title: Scatter Plot with GAM Smooth Line
### Aliases: scatter.gam

### ** Examples

# Generate sample data for examples
x <- rnorm(100)
y <- 2*x + rnorm(100)

# Plot GAM smooth line only
scatter.gam(x, y)

# Equivalent call using formula syntax (y ~ x)
scatter.gam(y ~ x)

# Include scatter plot with underlying data points behind the GAM line
scatter.gam(x, y, data.dots = TRUE)

# Include summary points showing mean x and y for each tertile bin
scatter.gam(x, y, three.dots = TRUE)

# Customize the plot with a custom title, line color, and line width
scatter.gam(x, y, data.dots = TRUE, col = "red", lwd = 2, main = "GAM Fit")

# Control smoothness of the GAM line by specifying the basis dimension
scatter.gam(x, y, k = 10)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("scatter.gam", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("t.test2")
### * t.test2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: t.test2
### Title: Enhanced alternative to t.test()
### Aliases: t.test2

### ** Examples

# Two-sample t-test
men <- rnorm(100, mean = 5, sd = 1)
women <- rnorm(100, mean = 4.8, sd = 1)
t.test2(men, women)

# Paired t-test
x <- rnorm(50, mean = 5, sd = 1)
y <- rnorm(50, mean = 5.2, sd = 1)
t.test2(x, y, paired = TRUE)

# One-sample t-test
data <- rnorm(100, mean = 5, sd = 1)
t.test2(data, mu = 0)

# Formula syntax
data <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
t.test2(y ~ group, data = data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("t.test2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("table2")
### * table2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: table2
### Title: Enhanced alternative to table()
### Aliases: table2

### ** Examples

# Create example data
df <- data.frame(
  group = c("A", "A", "B", "B", "A"),
  status = c("X", "Y", "X", "Y", "X")
)

# Enhanced table with variable names (2 variables)
table2(df$group, df$status)

# Enhanced table with variable names (3 variables)
df3 <- data.frame(
  x = c("A", "A", "B", "B"),
  y = c("X", "Y", "X", "Y"),
  z = c("high", "low", "high", "low")
)
table2(df3$x, df3$y, df3$z)

# Table with proportions
table2(df$group, df$status, prop = 'all')  # Overall proportions
table2(df$group, df$status, prop = 'row')  # Row proportions
table2(df$group, df$status, prop = 'col')  # Column proportions

# Table with chi-square test
table2(df$group, df$status, chi = TRUE,prop='all')




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("table2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("text2")
### * text2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: text2
### Title: Enhanced alternative to text()
### Aliases: text2

### ** Examples

# Create a simple plot
plot(1:10, 1:10, type = "n", main = "text2() - Alignment & Color")

# Alignment respect to x=5
text2(5, 8, "align='left' from 5", align = "left", bg = "yellow1")
text2(5, 7, "align='right' from 5", align = "right", bg = "blue", col = "white")
text2(5, 6, "align='center' from 5", align = "center", bg = "black", col = "white")
abline(v = 5, lty = 2)

# Multiple labels with different alignments
text2(c(2, 5, 8), c(5, 5, 5), 
      labels = c("Left", "Center", "Right"),
      align = c("left", "center", "right"),
      bg = c("pink", "lightblue", "lightgreen"))

# Text with custom font color (passed through ...)
text2(5, 3, "Red Text", col = "red", bg = "white")

# Padding examples
plot(1:10, 1:10, type = "n", main = "Padding Examples")

# Default padding (pad=0.03, pad_v=0.25)
text2(5, 8, "Default padding", bg = "lightblue")

# More horizontal padding
text2(5, 6, "Wide padding", pad = 0.2, bg = "lightgreen")

# More vertical padding
text2(5, 4, "Tall padding", pad_v = 0.8, bg = "lightyellow")

# Both padding increased
text2(5, 2, "Extra padding", pad = 0.15, pad_v = 0.6, bg = "pink")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("text2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("twolines")
### * twolines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: twolines
### Title: Two-Lines Test of U-Shapes
### Aliases: twolines

### ** Examples

## No test: 
# Simple example with simulated data
set.seed(123)
x <- rnorm(100)
y <- -x^2 + rnorm(100)
data <- data.frame(x = x, y = y)
result <- twolines(y ~ x, data = data)

# With covariates
z <- rnorm(100)
y <- -x^2 + 0.5*z + rnorm(100)
data <- data.frame(x = x, y = y, z = z)
result <- twolines(y ~ x + z, data = data)

# Without data argument (variables evaluated from environment)
x <- rnorm(100)
y <- -x^2 + rnorm(100)
result <- twolines(y ~ x)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("twolines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("var_labels")
### * var_labels

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: var_labels
### Title: Get or set variable labels
### Aliases: var_labels var_labels<-

### ** Examples

df <- data.frame(x = 1:3, y = 4:6)

# Set labels for all columns
var_labels(df) <- c("this is x", "this is y")
var_labels(df)

# Set a label for a single column
var_labels(df$x) <- "this is x"
var_labels(df$x)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("var_labels", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
