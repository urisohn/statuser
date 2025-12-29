# sohn

Miscellaneous R functions for papers, blogposts, & teaching by Uri Simonsohn.

## Installation

```r
# Install from GitHub with `groundhog` (for version control)
groundhog::groundhog.library("urisohn/sohn", date)   #date used for version control 

# Or install from GitHub with `devtools`
devtools::install_github("urisohn/sohn")
```

## Overview

Functions I often use and are not (sufficiently?) available in existing packages.

## Functions

### 📄 Uri's Papers

<details>
<summary><code>twolines()</code>: Two-Lines Test of U-Shapes</summary>

Implements the two-lines test for U-shaped (or inverted U-shaped) relationships introduced by Simonsohn (2018).

**Reference:** Simonsohn, Uri (2018) "Two lines: A valid alternative to the invalid testing of U-shaped relationships with quadratic regressions." AMPPS, 538-555. https://doi.org/10.1177/2515245918805755

```r
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

# Suppress Robin Hood details
result <- twolines(y ~ x, data = data, quiet = TRUE)

# Save plot to PNG
result <- twolines(y ~ x, data = data, pngfile = "twolines_plot.png")
```
</details>

### 📊 Graphing

<details>
<summary><code>plot_cdf()</code>: CDF for multiple groups in one plot</summary>

```r
y <- rnorm(100)
x <- rep(c("A", "B"), 50)
plot_cdf(y, x)  # Uses default colors (red4, dodgerblue for 2 groups)
plot_cdf(y, x, col = c("red", "blue"))  # Custom colors
```
</details>

<details>
<summary><code>plot_density()</code>: Density for multiple groups in one plot</summary>

```r
y <- rnorm(100)
x <- rep(c("A", "B"), 50)
plot_density(y, x)  # Uses default colors (red4, dodgerblue for 2 groups)
plot_density(y, x, col = c("red", "blue"))  # Custom colors
plot_density(y, x, show.means = FALSE)  # Hide mean segments
```
</details>

<details>
<summary><code>plot_freq()</code>: Frequency distribution without binning, with value labels</summary>

```r
x <- c(1, 1, 2, 2, 2, 5, 5)
plot_freq(x)

# Grouped frequency plot
df <- data.frame(value = c(1, 1, 2, 2, 2, 5, 5), group = c("A", "A", "A", "B", "B", "A", "B"))
plot_freq(value, by = group, data = df)

# Show percentages instead of frequencies
plot_freq(value, by = group, data = df, freq = FALSE)

# Customize legend and labels
plot_freq(value, by = group, data = df, legend.title = "Group", col.text = "black")
```
</details>

<details>
<summary><code>plot_gam()</code>: Plot GAM model predictions with optional distribution plot</summary>

```r
library(mgcv)
# Fit a GAM model
data(mtcars)
model <- gam(mpg ~ s(hp) + s(wt) + factor(cyl), data = mtcars)

# Basic plot
plot_gam(model, "hp")

# With distribution plot below (auto-selects plot_freq or plot_density)
plot_gam(model, "hp", plot2 = "auto")

# Always use frequency plot
plot_gam(model, "hp", plot2 = "freq")

# Always use density plot
plot_gam(model, "hp", plot2 = "density")

# Customize colors for main plot and bottom plot
plot_gam(model, "hp", plot2 = "auto", 
         col = "blue4", bg = adjustcolor('dodgerblue', .2),  # Main plot colors
         col2 = "steelblue", bg2 = "gray95")  # Bottom plot colors

# Hold other variables at different quantile
plot_gam(model, "hp", quantile.others = 25)
```
</details>

<details>
<summary><code>scatter.gam()</code>: Scatter plots with GAM smooth lines</summary>

```r
x <- rnorm(100)
y <- 2*x + rnorm(100)
scatter.gam(x, y)
```
</details>

<details>
<summary><code>text2()</code>: Adds to text(): align='center' , bg='yellow'</summary>

```r
plot(1:10, 1:10, type = "n")
   text2(2, 8, "Left", align = "left", bg = "lightblue")
   text2(5, 8, "Center", align = "center", bg = "lightgreen")
   text2(8, 8, "Right",    align = "right", bg = "lightyellow")
   text2(5, 5, "Red Text", col = "red", bg = "white")
```
</details>

<details>
<summary><code>resize_images()</code>: Saves any image (or all in folder) as PNG with set width.</summary>

```r
# Resize a single image file
   resize_images("path/to/image.svg", width = 800)

# Resize all images in a folder to 800px width
   resize_images("path/to/images", width = 800)

# Resize images to different widths
   resize_images("path/to/images", width = c(800, 1200, 600))
```
</details>

### 📈 Statistical Analyses

<details>
<summary><code>table2()</code>: Enhances base table(): (1) variable names are shown, (2) proportions are an option</summary>

```r
df <- data.frame(
  group = c("A", "A", "B", "B", "A"),
  status = c("X", "Y", "X", "Y", "X")
)

# table() does not show var names, table2() does
table (df$group, df$status)
table2(df$group, df$status)

# can report proportinos (building in prop.table() )
table2(df$group, df$status, prop = "all")    # Overall proportions
table2(df$group, df$status, prop = "row")    # Row proportions
table2(df$group, df$status, prop = "column") # Column proportions
```
</details>

<details>
<summary><code>desc_var()</code>: Comprehensive variable summary stats, optionally by grouping variable(s)</summary>

```r
# Why use desc_var() instead of psych::describeBy()?
# - Returns a single dataframe (not a list) - easier to export, filter, merge
# - Shows mode statistics (most frequent values) - useful for discrete data
# - Counts missing values automatically
# - Columns are labeled for easy interpretation
# - Supports multiple grouping variables with formula syntax
# - Results are sorted by grouping variables

# With grouping - compare groups side-by-side
df <- data.frame(score = rnorm(100), condition = rep(c("Control", "Treatment"), 50))
desc_var(score, condition, data = df)

# Formula syntax (single grouping variable)
desc_var(score ~ condition, data = df)

# Multiple grouping variables - results sorted by all grouping variables
df2 <- data.frame(
  score = rnorm(200),
  x1 = rep(c("A", "B"), 100),
  x2 = rep(c("men", "women"), each = 100),
  x3 = sample(1:3, replace = TRUE, size = 200)
)
desc_var(score ~ x1 + x2 + x3, data = df2)

# Without grouping - get stats for full dataset
desc_var(score, data = df)

# Direct vectors (no data frame needed)
scores <- rnorm(100)
groups <- rep(c("A", "B"), 50)
desc_var(scores, groups)

# Custom decimal places for cleaner output
desc_var(score, condition, data = df, decimals = 2)
```
</details>

<details>
<summary><code>t.test2()</code>: Enhances base t.test: (1) console shows mean diff & var names, (2) output is dataframe, not list</summary>

```r
# Data for example
	men <- rnorm(100, mean = 5, sd = 1)
	women <- rnorm(100, mean = 4.8, sd = 1)

# t.test() is harder to interpret, does not show difference of means(!) or indicate which mean is subtraced from which

	t.test(men, women) 
	t.test2(men, women)

# Formula syntax
	data <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
	t2 = t.test2(y ~ group, data = data)  # Columns: A, B, A-B, SE_A-B, conf.intL, conf.intH, level, t, df, p.value, method, se_A, se_B
	t2

# Formula syntax without data argument
	x1=rnorm(100)
	x2=rnorm(100)
	y <- c(x1, x2)
	condition <- rep(c('A', 'B'), c(length(x1), length(x2)))
	t.test2(y ~ condition)  
```
</details>

### ✨ Formatting

<details>
<summary><code>format_pvalue()</code>: Format p-values for clean display in figures and tables (e.g., p<.0001)</summary>

```r
format_pvalue(0.05)
format_pvalue(0.0001, include_p = TRUE)
```
</details>

<details>
<summary><code>message2()</code>: Print colored messages to console</summary>

```r
message2("This is a red message", col = "red", font = 2)
message2("This is a cyan message", col = "cyan")
```
</details>

### 🗂️ Data Management

<details>
<summary><code>list2()</code>: Create lists with objects without having to name them</summary>

```r
	x <- 1:5
	y <- letters[1:3]
	z <- matrix(1:4, nrow = 2)
	list2(x, y, z)
```
</details>

<details>
<summary><code>convert_to_sql()</code>: Convert CSV to SQL</summary>

```r
	convert_to_sql("data.csv", "data.sql")
```
</details>

<details>
<summary><code>clear()</code>: Clear environment, console, and all graphics devices</summary>

```r
# Create some objects
	x <- 1:10
	y <- rnorm(10)
	plot(x, y)

# Clear everything
	clear()
```
</details> 

## Dependencies

- `mgcv` (for `scatter.gam()`, `plot_gam()`, and `twolines()`)
- `rsvg` (for `resize_images()`)
- `magick` (for `resize_images()`)
- `sandwich` (for `twolines()`)
- `lmtest` (for `twolines()`)

## Author

**Uri Simonsohn**  
Email: urisohn@gmail.com

## License

GPL-3

## Version

0.1.4

