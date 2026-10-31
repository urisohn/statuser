# statuser 0.4.0.9001

### Improvements
- `plot_freq()`: character and factor inputs are supported (previously bare character vectors failed with an obscure `xlim` error). Thanks to Daniel Bankin for the report. New `x.order` controls category order on the x-axis: factor levels (default for factors), alphabetical, frequency (`"freq"` / `"freq.asc"`), `-1` to reverse the default, or a custom name vector.
- `interprobe()`: `draw = "none"` skips plotting (and `save.as`) while still returning simple slopes, Johnson–Neyman results, and related output.
- `interprobe()`: when `histogram = TRUE`, the sample-size distribution is drawn as its own panel under each curve plot (shared x-axis) instead of being carved out of the GAM y-axis, so frequency labels no longer collide with axis ticks.

### Bug fixes
- `interprobe()`: discrete focal predictors with a continuous moderator on the x-axis now build frequency bins along the moderator (and return `bin_from` / `bin_to` only when breaks exist), fixing empty-break assignment errors.

# statuser 0.4.0 (2026-10-09)

### New functions
- Added `round2()`: like `round()`, but always returns a character vector with the requested number of decimals. By default (`leading.zero = FALSE`), drops leading zeros when every finite value has `|x| < 1` (e.g. `0.34` → `.34`); mixed vectors keep leading zeros for all elements.

### Improvements
- `interprobe()`: Johnson–Neyman regions of significance now report what share of the sample falls in each range, e.g. `[2.50 to 4.80] (23.4% of the sample)`.
- `interprobe()`: `moderator.on.x.axis` now defaults to `"auto"` (moderator on the x-axis for binary focal predictors; focal predictor on the x-axis when it has more than 3 values).

### Bug fixes
- `print.lm2()`: undefined SE / t / p (exact fits under estimatr 2.0.1+, or dropped coefficients) now print as `--` instead of erroring in column alignment. Thanks to Alex Coppock for the report.
- `stimulus.plot()`: `null.method = "demean"` now uses the two condition-mean columns from `get.means.condition()` (previously selected the stimulus-label column and one mean, which errored with character stimulus IDs). Thanks to ceo@unicupcompany.com for discovering this.
- `stimulus.plot()`: with `flip.conditions = TRUE`, mean columns are named from the factor levels actually used in the t-tests (previously names always followed sorted condition order while contents followed the reversed levels).
- `stimulus.plot()`: `null.method = "demean"` heterogeneity p-values now use the same flipped observed effects as the null resamples (previously compared unflipped observed effects to flipped null draws).
- `interprobe()`: continuous Johnson–Neyman grids rename `x`/`z` to the real variable names before adding covariates at their means (avoids duplicate-name `newdata` columns).
- `interprobe()`: bare (unquoted) names for `x`, `z`, and `y` now work with `data=` and `model=` even when those names exist only as columns (not in the calling environment). Missing columns report e.g. `'value' is not a variable in dataframe 'df'`.
- `interprobe()`: `cols` of length 2 is allowed when the focal predictor has two levels (previously required length 3 even for binary `x`).

# statuser 0.3.1 (2026-06-24)

### Improvements
- `lm2()`: improved display rounding for printed regression tables. Near-zero numerical noise (e.g. from near-perfect fits) displays as `.000` instead of long scientific-notation strings; values with `|x| < 1` always omit the leading zero (including zero). Extreme `t` statistics display as `>999`. New optional `round` argument on `lm2()` and `print.lm2()`: `NULL` uses magnitude-based rules, `round = -1` uses R default `format()`, and non-negative values fix decimal places.
- `plot_cdf()`: `xlim` passed via `...` is now fully honored — ECDF curves are drawn over the requested range, not only the data range.
- `table2()`: `chi2=` is accepted as an alias for `chi=` (if both are passed, they must agree).

### Bug fixes
- `plot_freq()`, `plot_density()`, `plot_cdf()`, `plot_means()`, `lm2()`, and related helpers: automatic titles and axis labels now strip data-frame prefixes from `$` expressions while keeping the rest of the expression.
- `plot_means()`: user-supplied `ylim` is respected — the axis is no longer pulled down to 0 for `n=` labels; the lower limit may extend slightly below your requested minimum only to make room for sample-size labels.
- `table2()`: chi-square test again prints in APA format (`χ²(df) = ..., p = ...`) when printing results with `chi=TRUE`.

# statuser 0.3.0 (2026-05-20)
### New functions
- Added `stimulus.plot()` for stimulus plots in matched/treated-stimulus designs (means and effects plots with null resampling).
- Added `stimulus.beeswarm()` for compared-stimulus designs.
- Added `clear_stimulus_cache()` to reset resampling cache used by stimulus plotting functions.

### Improvements
- `t.test2()`: now reports Cohen's d for two-sample independent tests (returned as `d` and printed after sample sizes).
- `t.test2()`: missing-value note no longer double-counts rows where the grouping variable is `NA`.
- `lm2()`: explanatory notes are no longer printed with the regression table; they are stored when the model is printed and shown via the new `lm2_notes()` function. The print output ends with a reminder to run `lm2_notes()`.

### Deprecated
- `lm2()` and `print.lm2()`: the `notes` argument is deprecated (ignored). Passing `notes = TRUE` or `notes = FALSE` emits a message directing users to `lm2_notes()`.

# statuser 0.2.1 (2026-04-25)
### changes to pass CRAN inspection
- `plot_means()`: removed the `save.as` argument (no longer saves plots to PNG/SVG).
- `t.test2()`: documented S3 registration as `t.test2` for `base::t` to avoid recurring CRAN S3 consistency notes while keeping direct `t.test2(...)` usage unchanged.
- `desc_var()`: removed dependency on `labelled`; now relies on new function `var_labels`

### New function
- Added `var_labels()` to get/set these labels for vectors and data frames. This avoids depending on labelled 

# statuser 0.2.0 (never on CRAN)

### New functions
- Added `plot_means()` for barplots of group means with confidence intervals, optional comparisons and interaction-style contrasts, up to three grouping variables in the formula, optional clustered standard errors, and saving to PNG or SVG.
- Added `interprobe()` for probing interactions: simple slopes and Johnson–Neyman style marginal-effect curves for `x` × `z`, using GAM or linear models. 

### Improvements 
- `plot_freq()`: improved behavior when there are many distinct values; grouped plots support up to four levels of the grouping variable.
- `lm2()`: mean column shows percentages for factor levels and factor×factor interactions as intended; baseline factor level row shows the percentage of observations like other levels.
- `desc_var()`: when the response variable is missing, only the intended error is shown (no spurious “restarting interrupted promise evaluation” warning).
- `scatter.gam()`: supports direct coordinates and formula syntax (including `df$y ~ df$x`); axis labels use short variable names for `$` formulas; `ylim` and other `...` arguments no longer conflict with internal plot arguments; small vertical buffer on the lower frequency panel.
- Messages that used `red4` now use `red2` for readability in dark terminals (including messages from `plot_freq()`, `plot_density()`, `plot_cdf()`, `clear()`, `twolines()`, and `exit()`).
- `t.test2()` performance improvements.

### Bug fixes
- `desc_var()` evaluates calls such as `df$x` correctly when `data` is not supplied, instead of mistakenly using a same-named object from the search path.
- `text2()` applies vector arguments passed in `...` (e.g. `col`) per label, fixing wrong colors when labels differ (notably for `plot_means()` value labels).
- `plot_cdf()` restores only selected graphics parameters on exit (`mar`, `mgp`) so layouts using `par(mfrow = ...)` are not reset. Similar graphics-state cleanup was applied in other plotting functions where appropriate.

# statuser 0.1.9 (2026-02-26)
- `plot_freq()`, `plot_density()`, and `plot_cdf()`: optional comparison of two vectors (in addition to formula syntax); legend placed higher on the plot.
- `table2()`: clearer dimension labels for expressions such as `df$var > 10`; very long labels are truncated.

