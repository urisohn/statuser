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

- `cdf.by()`: CDF plots for multiple variables in one plot
- `fhist()`: Frequency distribution without binning, with value labels
- `scatter.gam()`: Scatter plots with GAM smooth lines

### 📈 Statistical Analyses

- `lmr()`: lm() with robust & clustered standard errors

### ✨ Formatting

- `format.pvalue()`: Format p-values for clean display in figures and tables (e.g., p<.0001)
- `message.col()`: Print colored messages to console

### 🔄 Simulations

- `counter()`: Show # of simulations run so far inside a monte carlo loop

### 🗂️ Data Management

- `namedList()`: Create lists with objects without having to name them
- `convert_to_sql()`: Convert CSV to SQL 



## Dependencies

- `mgcv` (for `scatter.gam()`)
- `sandwich` (for `lmr()`)

## Author

**Uri Simonsohn**  
Email: urisohn@gmail.com

## License

GPL-3

## Version

0.1.1

