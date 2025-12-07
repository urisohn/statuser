# sohn

Miscellaneous R functions for papers & blogposts by Uri Simonsohn.

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

### 📊 Graphing

<details>
<summary><code>cdf.by()</code>: CDF for multiple groups in one plot</summary>

```r
y <- rnorm(100)
x <- rep(c("A", "B"), 50)
cdf.by(y, x)  # Uses default colors (red4, dodgerblue for 2 groups)
cdf.by(y, x, col = c("red", "blue"))  # Custom colors
```
</details>

<details>
<summary><code>density.by()</code>: Density for multiple groups in one plot</summary>

```r
y <- rnorm(100)
x <- rep(c("A", "B"), 50)
density.by(y, x)  # Uses default colors (red4, dodgerblue for 2 groups)
density.by(y, x, col = c("red", "blue"))  # Custom colors
density.by(y, x, show.means = FALSE)  # Hide mean segments
```
</details>

<details>
<summary><code>fhist()</code>: Frequency distribution without binning, with value labels</summary>

```r
x <- c(1, 1, 2, 2, 2, 5, 5)
fhist(x)
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
<summary><code>text2()</code>: Adds to text(): align='center' , bg='yellow'

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
<summary><code>simplify()</code>: Simplify statistical test output (e.g., t-tests) for cleaner, more readable results</summary>

```r
men <- rnorm(100, mean = 5, sd = 1)
women <- rnorm(100, mean = 4.8, sd = 1)
result <- t.test(men, women)
simplify(result)
```
</details>

### ✨ Formatting

<details>
<summary><code>format.pvalue()</code>: Format p-values for clean display in figures and tables (e.g., p<.0001)</summary>

```r
format.pvalue(0.05)
format.pvalue(0.0001, include_p = TRUE)
```
</details>

<details>
<summary><code>message.col()</code>: Print colored messages to console</summary>

```r
message.col("This is a red message", col = "red", font = 2)
message.col("This is a cyan message", col = "cyan")
```
</details>

### 🔄 Simulations

<details>
<summary><code>counter()</code>: Show # of simulations run so far inside a monte carlo loop</summary>

```r
for (i in 1:100) {
  # Your simulation code here
  Sys.sleep(0.1)  # Simulate work
  counter(i)  # Report progress
}
```
</details>

### 🗂️ Data Management

<details>
<summary><code>namedList()</code>: Create lists with objects without having to name them</summary>

```r
	x <- 1:5
	y <- letters[1:3]
	z <- matrix(1:4, nrow = 2)
	namedList(x, y, z)
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

- `mgcv` (for `scatter.gam()`)
- `rsvg` (for `resize_images()`)
- `magick` (for `resize_images()`)

## Author

**Uri Simonsohn**  
Email: urisohn@gmail.com

## License

GPL-3

## Version

0.1.4

