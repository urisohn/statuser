# Changelog

All notable changes to this project will be documented in this file.

## Development version (0.1.8.9001)

Changes in the current development version (install from GitHub):

### Changed

- **`desc_var()`** – When the variable does not exist (e.g. `desc_var(x)` with no `x`), only the error is shown; the uninformative warning "restarting interrupted promise evaluation" is no longer emitted.
- **`scatter.gam()`** – (1) Accepts both direct and formula syntax: `scatter.gam(x, y)`, `scatter.gam(y ~ x, data = df)`, and `scatter.gam(df$y ~ df$x)` all work. (2) User-supplied `ylim` (and other plot args in `...`) no longer triggers "formal argument matched by multiple actual arguments". (3) With formula syntax like `mtcars$mpg ~ mtcars$hp`, axis labels and title show only the variable name (e.g. `xlab = "hp"`) instead of the full expression. (4) Lower (frequency) panel gets a 10% buffer on the y-axis so the top bar is not flush with the top of the plot.
- **Message colors** – All console messages that used `red4` now use `red2` for better visibility in dark-mode terminals (e.g. `plot_freq()`, `plot_density()`, `plot_cdf()` dropped-observation messages, `clear()`, `twolines()`, and `exit()` default).
- **Speedup `t.test2()`** – Performance improvements.
- **`clear()`** – Now works with a one-time permission authorization: on first use you are prompted to type "yes" to allow clearing the global environment, console, and plot; that choice is saved for future sessions. If you do not type "yes", you are asked again next time.
- **`plot_freq()`, `plot_density()`, `plot_cdf()`** – (1) Allow comparing two vectors (in addition to formula syntax). (2) Legend placed in the top 25% of the plot.
- **`table2()`** – Now supports comparison operators in variable names. When using expressions like `table2(df$var>10)`, the dimension name displays the full expression (e.g., "var>10") instead of not displaying the var name at all. Long expressions (>30 characters) are automatically truncated with "..." for readability.

## [0.1.8]

- First public release.
