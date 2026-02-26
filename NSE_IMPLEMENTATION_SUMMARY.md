# NSE Support Implementation Summary

## What Was Changed

Added Non-Standard Evaluation (NSE) support to plotting functions so that column names can be referenced with or without quotes when using the `data=` argument.

## Functions Updated

1. **plot_density()**
2. **plot_cdf()**
3. **plot_freq()**

## New Helper Function

Created `evaluate_variable_arguments()` in `R/utils.R` - a reusable utility that:
- Detects whether an argument is a bare symbol (unquoted) or a quoted string
- Looks up the column in the data frame if `data=` is provided
- Evaluates from the calling environment if `data=` is not provided
- Handles formulas and complex expressions correctly
- Provides consistent error messages

## Usage Examples

### Before (only this worked):
```r
plot_density('DV1', data = df)
plot_density('x', 'y', data = df)
plot_cdf('score', data = df)
```

### Now (both work):
```r
# Unquoted (NEW)
plot_density(DV1, data = df)
plot_density(x, y, data = df)
plot_cdf(score, data = df)

# Quoted (still works)
plot_density('DV1', data = df)
plot_density('x', 'y', data = df)
plot_cdf('score', data = df)

# Formula syntax (unchanged)
plot_density(DV1 ~ group, data = df)
```

## Implementation Details

### How It Works

1. **Early Capture**: At the very start of each function, before any argument evaluation, we call `match.call()` to capture unevaluated expressions

2. **Resolve Arguments**: Pass the unevaluated expressions to `evaluate_variable_arguments()` which:
   - Checks if it's a symbol (bare name like `DV1`)
   - If symbol + `data` provided → looks up in `data[[symbol_name]]`
   - If symbol + no `data` → evaluates in calling environment
   - If quoted string → treats as column name
   - If complex expression → evaluates normally

3. **Overwrite Parameters**: Replace the function parameters with the resolved values

4. **Continue Normally**: Rest of the function works as before with evaluated values

### Key Code Pattern

```r
plot_density <- function(formula, y = NULL, data = NULL, ...) {
  # Capture unevaluated arguments FIRST
  mc <- match.call()
  
  # Resolve with NSE support
  formula_resolved <- evaluate_variable_arguments(
    arg_expr = mc$formula,
    arg_name = "formula",
    data = data,
    calling_env = parent.frame(),
    func_name = "plot_density",
    allow_null = FALSE
  )
  
  # Overwrite with resolved values
  formula <- formula_resolved$value
  
  # Rest of function continues as before...
}
```

## Testing

- All existing 757 tests pass
- Updated one test that had expectations about error messages
- Tested both quoted and unquoted syntax manually

## Files Modified

1. `R/utils.R` - Added `evaluate_variable_arguments()` function
2. `R/plot_density.R` - Integrated NSE support
3. `R/plot_cdf.R` - Integrated NSE support
4. `R/plot_freq.R` - Integrated NSE support
5. `tests/testthat/test-plot_freq.R` - Updated one test for new error message
6. `examples/nse_demo.R` - Created demonstration script

## Benefits

1. **Consistency**: Matches tidyverse/dplyr-style NSE that users expect
2. **Convenience**: Less typing, cleaner code
3. **Backward Compatible**: All existing code with quoted names still works
4. **Extensible**: The `evaluate_variable_arguments()` helper can be easily reused for other functions

## Future Enhancements

The same pattern can be applied to other functions that accept column names:
- `desc_var()`
- `t.test2()`
- `table2()`
- `scatter.gam()`
- Any other functions that use `data=` argument with column references
