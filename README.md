# statuser

**Stat Tools for End Users**

Basic and custom statistical tools designed with end users in mind. Functions have optimized defaults, produce decluttered and informative output that is self-explanatory, and generate publication-ready results in one line of code.

## Installation

```r
# CRAN (when available)
install.packages("statuser")

# Or pin a version with groundhog
groundhog::groundhog.library("statuser", date)
```

## Overview

Same organization as `help(statuser)`:

### Basic Stats (improved)

- `lm2()` — like `lm()`, with robust SE and much more informative output
- `t.test2()` — like `t.test()`, decluttered, and more informative output
- `table2()` — like `table()`, showing variable names, and with proportions & chi2 built in
- `desc_var()` — descriptive statistics for variables (optional, by group(s))

### Custom tools from papers by Uri Simonsohn

- `twolines()` — two-lines test for U-shapes (Simonsohn 2018)
- `interprobe()` — probe and visualize nonlinear interactions (Simonsohn 2024; Montealegre & Simonsohn 2026)
- `stimulus.plot()` — stimulus plots for matched/treated-stimulus designs (Simonsohn, Montealegre, & Evangelidis 2025)
- `stimulus.beeswarm()` — beeswarm plots for compared-stimulus designs (Simonsohn, Montealegre, & Evangelidis 2025)

### Graphing

- `scatter.gam()` — scatter plot for x & y, with fitted GAM line y = f(x)
- `plot_cdf()` — empirical cumulative distribution functions (optional, by group)
- `plot_density()` — density functions (optional, by group)
- `plot_freq()` — frequency of observed values (optional, by group)
- `plot_means()` — barplot of means with confidence intervals and (optionally) tests; see also `stimulus.plot()` for per-stimulus plots
- `plot_gam()` — fitted GAM values for a focal predictor
- `text2()` — like `text()` with horizontal alignment and background color

### Formatting

- `format_pvalue()` — format p-values for display
- `round2()` — like `round()`, but fixed decimals as character; optional drop of leading zeros
- `var_labels()` — get/set variable labels (as base-R `"label"` attributes)
- `message2()` — print colored messages to console
- `resize_images()` — resize images (SVG, PDF, EPS, JPG, PNG, etc.) to PNG with specified width

### Miscellaneous

- `clear()` — clear environment, console, and all graphics devices
- `list2()` — like `list()`, but unnamed objects are automatically named
- `convert_to_sql()` — convert CSV files to SQL INSERT statements

## Examples

### Basic Stats (improved)

<details>
<summary><code>lm2()</code>: like <code>lm()</code>, with robust SE and much more informative output</summary>

Uses **estimatr** for robust and clustered errors. Notes via `lm2_notes()` after printing.

```r
lm2(mpg ~ wt + hp, data = mtcars)
lm2(mpg ~ wt + hp, data = mtcars, se_type = "HC2")
lm2_notes()
```
</details>

<details>
<summary><code>t.test2()</code>: like <code>t.test()</code>, decluttered, and more informative output</summary>

```r
men <- rnorm(100, mean = 5, sd = 1)
women <- rnorm(100, mean = 4.8, sd = 1)
t.test2(men, women)

data <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
t.test2(y ~ group, data = data)
```
</details>

<details>
<summary><code>table2()</code>: like <code>table()</code>, showing variable names, with proportions & chi2 built in</summary>

```r
df <- data.frame(
  group = c("A", "A", "B", "B", "A"),
  status = c("X", "Y", "X", "Y", "X")
)
table2(df$group, df$status)
table2(df$group, df$status, prop = "row", chi = TRUE)
```
</details>

<details>
<summary><code>desc_var()</code>: descriptive statistics for variables (optional, by group(s))</summary>

```r
df <- data.frame(score = rnorm(100), condition = rep(c("Control", "Treatment"), 50))
desc_var(score ~ condition, data = df)
```
</details>

### Custom tools from papers by Uri Simonsohn

<details>
<summary><code>twolines()</code>: two-lines test for U-shapes (Simonsohn 2018)</summary>

Implements the two-lines test (Simonsohn, 2018).

**Reference:** Simonsohn, U. (2018). Two lines: A valid alternative to the invalid testing of U-shaped relationships with quadratic regressions. *Advances in Methods and Practices in Psychological Science*, 1(4), 538–555. https://doi.org/10.1177/2515245918805755

```r
set.seed(123)
x <- rnorm(100)
y <- -x^2 + rnorm(100)
data <- data.frame(x = x, y = y)
twolines(y ~ x, data = data)

# With covariates, suppress Robin Hood details, or save PNG
twolines(y ~ x + z, data = data, quiet = TRUE)
twolines(y ~ x, data = data, pngfile = "twolines_plot.png")
```
</details>

<details>
<summary><code>interprobe()</code>: probe and visualize nonlinear interactions</summary>

Estimates or accepts a model, then plots **simple slopes** and **Johnson–Neyman** curves. Default engine is GAM (robust to nonlinearities); also supports linear models, supplied fits (including covariates), and `lm2()`. Axis placement defaults to `"auto"`: moderator on the x-axis when the focal predictor has ≤3 levels; focal predictor on the x-axis otherwise.

**References:** Simonsohn, U. (2024). *AMPPS*. https://doi.org/10.1177/25152459231207787; Montealegre & Simonsohn (2026).

**Full walkthrough** (three study designs): after installing from source, run `vignette("interprobe", package = "statuser")`.

```r
# Examples use MASS::birthwt (see vignette for prep of smoke/race factors)
# Case A: binary x × continuous moderator
interprobe(x = smoke, z = age, y = bwt, data = bw)

# Case B: 3-level categorical x × continuous z
interprobe(x = race, z = age, y = bwt, data = bw)

# Case C: continuous x × continuous z
interprobe(x = age, z = lwt, y = bwt, data = bw)

# Linear probe, or probe a fitted model (path for covariates)
interprobe(x = smoke, z = age, y = bwt, data = bw, model = "linear")
fit <- lm(bwt ~ smoke * age + lwt, data = bw)
interprobe(model = fit, x = smoke, z = age)
```
</details>

<details>
<summary><code>stimulus.plot()</code>: stimulus plots for matched/treated-stimulus designs</summary>

**Reference:** Simonsohn, U., Montealegre, A., & Evangelidis, I. (2025). *JPSP*, 129(1), 71–90. https://doi.org/10.1037/pspa0000449

```r
stimulus.plot(
  data = df, dv = "score", stimulus = "item", condition = "cond",
  plot.type = "means", watermark = FALSE
)

stimulus.plot(
  data = df, dv = "score", stimulus = "item", condition = "cond",
  plot.type = "effects", participant = "id", simtot = 500, seed = 2024
)

clear_stimulus_cache()
```
</details>

<details>
<summary><code>stimulus.beeswarm()</code>: beeswarm plots for compared-stimulus designs</summary>

```r
stimulus.beeswarm(
  data = df, dv = "score", stimulus = "stim_id", condition = "cond",
  simtot = 500, watermark = FALSE
)
```
</details>

### Graphing

<details>
<summary><code>scatter.gam()</code>: scatter plot for x & y, with fitted GAM line</summary>

```r
x <- rnorm(100)
y <- 2 * x + rnorm(100)
scatter.gam(x, y)
scatter.gam(y ~ x, data = data.frame(x, y), data.dots = TRUE)
```
</details>

<details>
<summary><code>plot_cdf()</code>: ECDFs by group</summary>

```r
y <- rnorm(100)
x <- rep(c("A", "B"), 50)
plot_cdf(y ~ x)
plot_cdf(y ~ x, col = c("red", "blue"), order = c("B", "A"))
```
</details>

<details>
<summary><code>plot_density()</code>: Densities by group</summary>

```r
plot_density(y ~ x)
plot_density(y ~ x, col = c("red", "blue"), show.means = FALSE, order = -1)
```
</details>

<details>
<summary><code>plot_freq()</code>: Frequency plot without binning</summary>

```r
plot_freq(c(1, 1, 2, 2, 2, 5, 5))

df <- data.frame(
  value = c(1, 1, 2, 2, 2, 5, 5),
  group = c("A", "A", "A", "B", "B", "A", "B")
)
plot_freq(value ~ group, data = df, order = c("B", "A"))
plot_freq(value ~ group, data = df, freq = FALSE, legend.title = "Treatment")
```
</details>

<details>
<summary><code>plot_means()</code>: barplot of means with CIs and (optionally) tests</summary>

Up to three grouping factors; optional clustered SEs and custom comparisons. See `stimulus.plot()` for per-stimulus plots.

```r
df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
plot_means(y ~ group, data = df, quiet = TRUE)

df2 <- data.frame(
  y = rnorm(200),
  x1 = rep(c("A", "B"), 100),
  x2 = rep(c("X", "Y"), each = 100)
)
plot_means(y ~ x1 + x2, data = df2)
```
</details>

<details>
<summary><code>plot_gam()</code>: GAM partial effect with optional distribution panel</summary>

```r
library(mgcv)
model <- gam(mpg ~ s(hp) + s(wt) + factor(cyl), data = mtcars)
plot_gam(model, "hp", plot2 = "auto")
plot_gam(model, "hp", plot2 = "freq", quantile.others = 25)
```
</details>

<details>
<summary><code>text2()</code>: like <code>text()</code> with horizontal alignment and background color</summary>

```r
plot(1:10, 1:10, type = "n")
text2(2, 8, "Left", align = "left", bg = "lightblue")
text2(5, 8, "Center", align = "center", bg = "lightgreen")
text2(8, 8, "Right", align = "right", bg = "lightyellow")
text2(5, 5, "Red text", col = "red", bg = "white")
```
</details>

### Formatting

<details>
<summary><code>format_pvalue()</code>: format p-values for display</summary>

```r
format_pvalue(0.05)
format_pvalue(0.0001, include_p = TRUE)
```
</details>

<details>
<summary><code>round2()</code>: round with fixed decimals (optional drop of leading zeros)</summary>

```r
round2(0.34, 2)
round2(0.34, 2, leading.zero = TRUE)
round2(c(0.34, 1.25), 2)
```
</details>

<details>
<summary><code>var_labels()</code>: get/set variable labels</summary>

```r
df <- data.frame(x = 1:3, y = 4:6)
var_labels(df) <- c("Variable X", "Variable Y")
var_labels(df)
```
</details>

<details>
<summary><code>message2()</code>: print colored messages to console</summary>

```r
message2("Note", col = "cyan", font = 2)
```
</details>

<details>
<summary><code>resize_images()</code>: resize images to PNG at a set width</summary>

```r
resize_images("path/to/image.svg", width = 800)
resize_images("path/to/images", width = c(800, 1200, 600))
```
</details>

### Miscellaneous

<details>
<summary><code>clear()</code>: clear environment, console, and graphics devices</summary>

```r
clear()  # first run may prompt for one-time permission
```
</details>

<details>
<summary><code>list2()</code>: like <code>list()</code>, auto-named objects</summary>

```r
list2(x, y, z)
```
</details>

<details>
<summary><code>convert_to_sql()</code>: CSV to SQL INSERT statements</summary>

```r
convert_to_sql("data.csv", "data.sql")
```
</details>

## Dependencies

**Imports:** `mgcv`, `marginaleffects`, `rsvg`, `magick`, `sandwich`, `lmtest`, `lmerTest`, `digest`, `beeswarm`, `utils`

**Suggested (optional features):** `estimatr` (`lm2()`), `testthat`, `crayon`, `quantreg`, `broom`, `modelsummary`

## Author

**Uri Simonsohn** — urisohn@gmail.com — https://github.com/urisohn/statuser

## License

GPL-3

## Version

CRAN release: **0.4.0**.
