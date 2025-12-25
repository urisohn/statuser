# Unit Test Summary for sohn Package

This document provides a comprehensive list of all unit tests conducted for the sohn package, organized by test file.

## Test Files Overview

### 1. test-clear.R
Tests for the `clear()` function that clears environment, console, and graphics.
- **Basic functionality**: Verifies function runs without errors
- **Return value**: Confirms function returns NULL invisibly
- **Note**: Full environment clearing cannot be tested without affecting test execution

### 2. test-desc_var.R
Tests for the `desc_var()` function that computes descriptive statistics.
- **Basic statistics**: Verifies computation of mean, SD, median, quantiles, mode
- **Data frame input**: Tests column extraction from data frames
- **No grouping**: Tests statistics for full dataset without groups
- **Missing values**: Verifies NA handling and counting
- **Decimals parameter**: Tests rounding to specified decimal places
- **Column structure**: Verifies all expected columns are present
- **Empty groups**: Tests handling of groups with all missing values
- **Quantiles**: Verifies correct computation and ordering of percentiles

### 3. test-format_pvalue.R
Tests for the `format_pvalue()` function that formats p-values for display.
- **Basic formatting**: Tests formatting of common p-values (e.g., 0.05, 0.001)
- **Edge cases**: Tests very small (< 0.0001) and very large (> 0.9999) p-values
- **NA handling**: Verifies graceful handling of missing values
- **include_p parameter**: Tests addition of "p" prefix
- **Vector input**: Tests formatting of multiple p-values
- **digits parameter**: Verifies rounding to specified decimal places
- **Boundary values**: Tests values at formatting thresholds

### 4. test-fhist.R
Tests for the deprecated `fhist()` alias function.
- **Alias functionality**: Verifies fhist() calls plot_freq() correctly
- **Argument passing**: Tests that arguments are passed through correctly
- **Deprecation message**: Confirms deprecation warning is displayed

### 5. test-gam_functions.R
Tests for GAM-related functions: `scatter.gam()`, `plot_gam()`, and `twolines()`.
- **scatter.gam**: Tests basic functionality, data frame input, data.dots and three.dots parameters, k parameter, error handling
- **plot_gam**: Tests validation of non-GAM models, predictor parameter validation, quantile.others parameter validation, return structure
- **twolines**: Tests U-shape testing functionality, graph parameter, quiet parameter, covariates handling, return structure
- **Note**: Requires mgcv package (tests skip if not installed)

### 6. test-message2.R
Tests for the `message2()` colored message function.
- **Basic printing**: Verifies messages print without errors
- **quiet parameter**: Tests suppression of output
- **Color handling**: Tests various color options (red, blue, cyan, green, etc.)
- **Font parameter**: Tests plain (font=1) and bold (font=2) text
- **stop parameter**: Tests execution stopping functionality
- **appendLF parameter**: Tests newline handling (with expected warning)
- **Multiple parts**: Tests combining multiple message components
- **Return value**: Verifies function returns invisibly

### 7. test-namedList.R
Tests for the `namedList()` function that creates named lists.
- **Automatic naming**: Tests automatic naming from object names
- **Explicit names**: Tests when names are explicitly provided
- **Mixed naming**: Tests combination of named and unnamed objects
- **Single object**: Tests handling of single object input
- **Empty call**: Tests behavior with no arguments
- **Value preservation**: Verifies object values are preserved correctly
- **Complex expressions**: Tests handling of expressions in arguments

### 8. test-plot_cdf.R
Tests for the `plot_cdf()` function that plots empirical cumulative distribution functions.
- **Basic functionality**: Verifies function runs and returns ECDF functions
- **Data frame input**: Tests using df$column syntax
- **Formula syntax**: Tests formula syntax (value ~ group)
- **Missing values**: Verifies NA handling
- **Custom parameters**: Tests custom colors, line widths, line types
- **show.ks parameter**: Tests Kolmogorov-Smirnov test display
- **show.quantiles parameter**: Tests quantile line display
- **Multiple groups**: Tests with 2, 3, and 4 groups
- **Return structure**: Verifies ECDF functions are returned and are monotonic

### 9. test-plot_density.R
Tests for the `plot_density()` function that plots density functions by group.
- **Basic functionality**: Verifies function runs and returns density objects
- **Data frame input**: Tests using df$column syntax
- **Missing values**: Verifies NA handling
- **Custom parameters**: Tests colors, line widths, line types, multiple parameters
- **show.t parameter**: Tests mean point and label display
- **show.means parameter**: Tests vertical segment display at means
- **Multiple groups**: Tests with 2, 3, and 4 groups
- **density() arguments**: Tests custom bandwidth and kernel parameters
- **Return structure**: Verifies density objects are returned correctly

### 10. test-plot_freq.R
Tests for the `plot_freq()` function that plots frequencies without binning.
- **Basic functionality**: Verifies function runs and returns data frame
- **Data frame input**: Tests using df$column syntax
- **Grouping**: Tests frequency plotting by group
- **freq parameter**: Tests frequency vs proportion display
- **Custom parameters**: Tests colors, line widths, width parameter
- **value.labels parameter**: Tests display of frequency labels
- **add parameter**: Tests adding to existing plots
- **show.legend parameter**: Tests legend display
- **Return structure**: Verifies correct data frame structure
- **Missing values**: Verifies NA handling

### 11. test-print_functions.R
Tests for print methods: `print.simplified_ttest()` and `print.table2()`.
- **print.simplified_ttest**: Tests two-sample tests, one-sample tests, small p-values, Welch vs Student tests
- **print.table2**: Tests basic tables, proportion tables, three-way tables, tables without dimension names
- **Return values**: Verifies print methods return invisibly

### 12. test-resize_images.R
Tests for the `resize_images()` function that resizes image files.
- **Non-existent folder**: Tests error handling for invalid paths
- **Empty folder**: Tests error handling when no images found
- **Width parameter**: Tests single and multiple width values
- **Note**: Full functionality requires actual image files

### 13. test-t.test2.R
Tests for the `t.test2()` enhanced t-test function.
- **Two-sample test**: Tests basic two-sample t-test functionality
- **Data frame input**: Tests formula syntax with data frames
- **Formula syntax**: Tests value ~ group syntax
- **Two vectors**: Tests standard x, y vector input
- **Return columns**: Verifies all expected columns are present
- **digits parameter**: Tests different decimal place specifications
- **Welch vs Student**: Tests detection of test type
- **One-sample test**: Tests one-sample t-test functionality

### 14. test-table2.R
Tests for the `table2()` enhanced table function.
- **Basic table**: Tests creation of standard contingency tables
- **Data frame columns**: Tests variable name extraction from df$column syntax
- **Three-way tables**: Tests 3D contingency tables
- **prop parameter**: Tests overall, row, and column proportions
- **digits parameter**: Tests decimal place specification for proportions
- **useNA parameter**: Tests handling of missing values (no, ifany, always)
- **exclude parameter**: Tests exclusion of specific levels
- **Return structure**: Verifies correct table structure and value sums
- **Single vector**: Tests one-way frequency tables

### 15. test-text2.R
Tests for the `text2()` function that adds text with background and alignment.
- **Basic functionality**: Tests text placement on plots
- **Alignment**: Tests left, center, and right alignment
- **Vectorized alignments**: Tests multiple labels with different alignments
- **Background colors**: Tests single and multiple background colors
- **cex parameter**: Tests character expansion (size)
- **pad parameters**: Tests horizontal and vertical padding
- **Additional arguments**: Tests passing through col, font, etc.
- **Multiple labels**: Tests handling of multiple text labels
- **Color recycling**: Tests recycling of background colors

### 16. test-validate_plot.R
Tests for the `validate_plot()` internal validation function.
- **Standard syntax**: Tests y, group vector input
- **Data frame input**: Tests column name extraction
- **Formula syntax**: Tests formula syntax with and without data
- **df$var syntax**: Tests dataframe column reference syntax
- **Error handling**: Tests validation of invalid inputs (wrong length, non-numeric, missing group, missing columns)
- **Optional group**: Tests behavior when group is optional

### 17. test-convert_to_sql.R
Tests for the `convert_to_sql()` function that converts CSV to SQL.
- **SQL file creation**: Tests creation of output SQL file
- **INSERT statements**: Verifies INSERT statements are generated
- **CREATE TABLE**: Tests optional CREATE TABLE statement generation
- **Date columns**: Tests detection and handling of DATE columns
- **Numeric columns**: Tests detection and handling of REAL columns
- **Quote escaping**: Tests escaping of single quotes in text values
- **Return value**: Verifies function returns invisibly
- **Note**: Uses temporary files for testing

## Test Statistics

- **Total test files**: 17
- **Total test cases**: ~350+ individual test assertions
- **Functions covered**: All exported functions in the package
- **Test framework**: testthat (>= 3.0.0)

## Test Coverage

The test suite covers:
- ✅ All exported functions
- ✅ Basic functionality
- ✅ Edge cases (NA values, empty inputs, boundary conditions)
- ✅ Error handling
- ✅ Parameter variations
- ✅ Return value structure and types
- ✅ Data frame input handling
- ✅ Formula syntax where applicable

## Running Tests

To run all tests:
```r
devtools::test()
# or
testthat::test_dir("tests/testthat")
```

To run a specific test file:
```r
testthat::test_file("tests/testthat/test-format_pvalue.R")
```

## Notes

- Tests use `skip_if_not_installed()` for optional dependencies (mgcv)
- File I/O tests use temporary files that are cleaned up
- Plotting tests verify return values rather than visual output
- Some tests expect warnings (e.g., message2 appendLF parameter)
- Tests are designed to run automatically during `R CMD check`

