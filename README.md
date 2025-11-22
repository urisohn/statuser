# sohn

Miscellaneous R functions for papers & blogposts by Uri Simonsohn.

## Installation

```r
# Install from GitHub with `groundhog` (for version control)
groundhog::groundhog.library("urisohn/sohn", date)   #date is a date to load with version control 

# Or install from GitHub with `devtools`
devtools::install_github("urisohn/sohn")
```

## Overview

Common data visualization and formatting I have used in papers & Data Colada blog posts.
Created with help from Cursor (AI)

## Functions

### `scatter.gam()`

Creates a scatter plot with a GAM (Generalized Additive Model) smooth line, with options to display data points and three-way spline summary points.

**Example:**
```r
x <- rnorm(100)
y <- 2*x + rnorm(100)
scatter.gam(x, y, data.dots = TRUE, three.dots = TRUE)
```



### `fhist()`

Plots distribution of a variable.  
Unlike histograms, no binning.  
Unlike barplots, all possible values included in x-axis.

**Example:**
```r
x <- c(1, 1, 2, 2, 2, 5, 5)
fhist(x, col = "steelblue", xlab = "Value", ylab = "Frequency")
```



### `cdf.by()`

Plots CDFs as multiple lines in the same plot, plotting a given y by different values of x.

**Example:**
```r
y <- rnorm(100)
x <- rep(c("A", "B", "C"), c(30, 40, 30))
cdf.by(y, x, col = c("red", "green", "blue"), lwd = 2)
```



### `format.pvalue()`

Formats p-values for clean display in figures and tables, adds p= or p<, and rounds to four decimal points.

**Example:**
```r
format.pvalue(c(0.05, 0.001, 0.00001))
# [1] "= .05"    "= .001"   "< .0001"

format.pvalue(0.05, include_p = TRUE)
# [1] "p = .05"
```



### `namedList()`

Creates a  list where objects are automatically named based on their variable names.  
The existing name is a default, you can also set new names with it.

**Example:**
```r
x <- 1:5
y <- letters[1:3]
z_MAT <- matrix(1:4, nrow = 2)

my_list <- namedList(x, y, z=z_MAT)
names(my_list)  # "x" "y" "z"
```



## Dependencies

- `mgcv` (for `scatter.gam()`)

## Author

**Uri Simonsohn**  
Email: urisohn@gmail.com

## License

GPL-3

## Version

0.1.0

