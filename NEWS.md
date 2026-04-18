# statuser 0.2.0

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

# statuser 0.1.9
- `plot_freq()`, `plot_density()`, and `plot_cdf()`: optional comparison of two vectors (in addition to formula syntax); legend placed higher on the plot.
- `table2()`: clearer dimension labels for expressions such as `df$var > 10`; very long labels are truncated.

