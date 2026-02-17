# Changelog

All notable changes to this project will be documented in this file.

## Development version (0.1.8.9001)

Changes in the current development version (install from GitHub):

### Changed

- **Speedup `t.test2()`** – Performance improvements.
- **`clear()`** – Now works with a one-time permission authorization: on first use you are prompted to type "yes" to allow clearing the global environment, console, and plot; that choice is saved for future sessions. If you do not type "yes", you are asked again next time.
- **`plot_freq()`, `plot_density()`, `plot_cdf()`** – (1) Allow comparing two vectors (in addition to formula syntax). (2) Legend placed in the top 25% of the plot.
- **`table2()`** – Now supports comparison operators in variable names. When using expressions like `table2(df$var>10)`, the dimension name displays the full expression (e.g., "var>10") instead of just the variable name. Long expressions (>30 characters) are automatically truncated with "..." for readability.

## [0.1.8]

- First public release.
