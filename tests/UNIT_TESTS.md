# Unit test index

This document lists **testthat** files under `tests/testthat/`, the **R source files** they primarily exercise, and short notes on what **groups of tests** cover. Test titles are quoted as in code (`test_that("...", { ... })`).

### Test IDs

Each `test_that()` block is preceded by a comment of the form `#<function>_<NNN>` (for example `#t.test2_001`). The name is the **R function** under test (`t.test2`, `plot_freq`, `scatter.gam`, `print.table2`, …). Numbers are **unique per function across the whole suite** (assigned in sorted file order, with `test-backend_equivalence.R` processed **last** so primary test files keep the lowest IDs). To refresh IDs after edits, run `Rscript tests/testthat/_annotate_test_ids.R` from the package root.

---

## `test-backend_equivalence.R` → cross-cutting (`R/t.test2.R`, `R/lm2.R`, `R/interprobe.R`, …)

Regression-style checks that **statuser** outputs stay aligned with reference implementations (base **stats**, **estimatr**, **marginaleffects**).

| Subset | What it checks |
|--------|----------------|
| **t.test2 vs `stats::t.test`** | Default and formula interfaces agree on core two-sample scenarios. |
| **lm2 vs `estimatr::lm_robust`** | Coefficients and inference match for a representative linear model. |
| **interprobe** | Johnson–Neyman / regions of significance and internal interaction **p**-values agree with **marginaleffects**-based references (binary **x** case). |

Helper code: `tests/testthat/helper-backend_equivalence.R`.

---

## `test-convert_to_sql.R` → `R/convert_to_sql.R`

| Subset | What it checks |
|--------|----------------|
| **File output** | Writes a SQL file; optional `INSERT` generation. |
| **Schema** | `create_table` behaviour. |
| **Types** | Date and numeric column handling. |
| **Safety** | Escaping single quotes in string data. |
| **Return value** | Invisible return. |

---

## `test-desc_var.R` → `R/desc_var.R`

| Subset | What it checks |
|--------|----------------|
| **Basics** | Correct summary stats; `digits`; expected column set. |
| **Inputs** | Data frames; columns named like arguments (`y`, `group`); formula and multi-way grouping; sorting of results. |
| **Missing / empty** | `NA` handling; empty groups; all-**NA** **y**; single observation per group. |
| **Diagnostics** | Missing factor combinations; perfectly overlapping groupers. |
| **Errors** | Non-numeric **y**; truly missing variables; evaluation order for `df$col` vs validation. |

---

## `test-format_pvalue.R` → `R/format_pvalue.R`

| Subset | What it checks |
|--------|----------------|
| **Formatting** | Ordinary **p**-values; vector input; `digits`; `include_p` / “includes_p” variants. |
| **Rules** | Threshold vs `digits`; boundary values; values outside \([0,1]\); negative inputs. |
| **Edge cases** | `NA`; empty vectors; single `NA`. |

---

## `test-gam_functions.R` → `R/scatter.gam.R`, `R/plot_gam.R`, `R/twolines.R`

| Subset | What it checks |
|--------|----------------|
| **scatter.gam** | Runs with data-frame and formula inputs; `data.dots`, `three.dots`, `k`; invalid data errors; `ylim` via `...` without duplicate-argument issues; sensible fitted values. |
| **plot_gam** | Errors for non-GAM objects; `predictor` and `quantile.others` validation; structure; reasonable predictions; multiple predictors. |
| **twolines** | `graph`, `quiet`, covariates; structure; U-shaped vs inverted-U detection. |

---

## `test-interprobe.R` → `R/interprobe.R` (and helpers in `R/interprobe_helpers.R`)

| Subset | What it checks |
|--------|----------------|
| **`lm` input** | Structure; fitted model returned when estimated internally; **APA** printing unless `quiet=TRUE`. |
| **Binary x** | **ti**-based GAM testing path (`gam_results_testing`). |
| **`lm2` input** | Works with **lm2** fits. |
| **`model` / `data` routing** | Fits **lm2** when `model = linear` (or sentinel) with data; vector interface with `model = linear`. |
| **Keyword vs object** | `linear` in calling env vs bare keyword; `lm` as keyword when appropriate. |
| **Errors** | Messages preserve intended model name. |

---

## `test-list2.R` → `R/list2.R`

| Subset | What it checks |
|--------|----------------|
| **Naming** | Names from symbols; explicit names; mixed named/unnamed. |
| **Edge cases** | Single argument; empty call. |
| **Values** | Preserved values; non-trivial expressions. |

---

## `test-lm2.R` → `R/lm2.R`

| Subset | What it checks |
|--------|----------------|
| **Defaults** | Statuser-flavoured output; **HC3** SEs by default. |
| **Coefficients** | Standardized betas; agreement with **`lm`** / **`lm_robust`**; classical vs robust SE differences. |
| **`...`** | Extra args passed through to **`lm_robust`**. |
| **Predict** | With/without `newdata`; with/without clustering; `se.fit`; alignment with **`lm_robust`**. |
| **Ecosystem** | **marginaleffects** (`predict`, `avg_slopes`, comparisons); **broom** (`tidy`, `glance`, `augment`); **modelsummary** (single and multi-model). |
| **Diagnostics in print** | Heteroskedasticity flags (! / !!!); collinearity on interactions (X); **SE.cluster** when clustered; cluster SE &gt; robust when clustering matters. |
| **`print.lm2`** | `notes` toggles; Call line; summary stats; interaction notes. |
| **Mean column** | Numeric vs factor level percentages. |
| **Data** | Missing values; single/many predictors; with/without `data` (env vs explicit). |
| **Snapshots** | Stable print output for representative cases. |

---

## `test-message2.R` → `R/message2.R`

| Subset | What it checks |
|--------|----------------|
| **Rendering** | Default message; colours; `font`; `stop`; multi-part messages. |
| **Return** | Invisible return. |

---

## `test-plot_cdf.R` → `R/plot_cdf.R`

| Subset | What it checks |
|--------|----------------|
| **Smoke / inputs** | Runs; `data` frame; formula; `df$col` on LHS/RHS without bare names in env; missing values. |
| **Options** | Custom args; `show.ks`; `show.quantiles`; group count variants. |
| **Return / errors** | Object structure; dataset name in errors. |
| **Edge cases** | Single group; all-**NA**; very small **n** per group. |
| **Order / factors** | `order`, `order = -1`, factor levels when `order` is `NULL`. |
| **Two-vector mode** | Basic and custom params; `order`; unequal lengths; legend margin; `xlim` padding. |

---

## `test-plot_density.R` → `R/plot_density.R`

| Subset | What it checks |
|--------|----------------|
| **Smoke / inputs** | Same themes as **plot_cdf**: runs, `data`, formula, `df$col`, missing. |
| **Options** | Custom parameters; `show.means`; passthrough to **`density()`**. |
| **Groups** | Different counts; single vs many groups. |
| **Return / errors** | Structure; dataset name in errors. |
| **Order / factors** | `order`, `-1`, factor levels. |
| **Two-vector mode** | Custom params; `order`; length mismatch; legend space. |

---

## `test-plot_freq.R` → `R/plot_freq.R`

| Subset | What it checks |
|--------|----------------|
| **Smoke / inputs** | Runs; `data`; grouping; formula / `df$col`; `freq` and general custom args; `value.labels`, `show.x.value`, `ticks.max`, `show.legend`. |
| **Return / errors** | Structure; dataset name in errors; missing values. |
| **Order** | Named order, validation, three groups, default sort, `order = -1` (including with factors). |
| **Two-vector mode** | Structure, params, `order`, missing, validation, name deduction, `data` columns, continuous **x**, `col`/`freq`. |
| **Layout** | `xlim` covers bars; pretty ticks for many levels; legend space. |

---

## `test-plot_means.R` → `R/plot_means.R`

| Subset | What it checks |
|--------|----------------|
| **Integration** | Formula + `data` returns **`desc_var`**-like object; multi-way formulas (`+`); `order`; three-way grouping with sparse cells. |
| **`tests = "auto"`** | Three scenarios (binary **x1** only; binary **x1** and **x2**; **x2** with &gt;2 levels). |
| **Output** | `save.as` writes PNG. |
| **Numerical agreement** | Cell means/CIs vs **`t.test`** (no clusters); **Welch** **t** in scenario 1; 2×2 interaction **p** vs **`lm_robust`** **HC3**; clustered CIs follow `ci.level`. |

---

## `test-print_functions.R` → `R/print_table2.R` (methods for **table2** objects)

| Subset | What it checks |
|--------|----------------|
| **print.table2** | Frequency tables; proportion tables; three-way tables; tables without dimension names. |

---

## `test-resize_images.R` → `R/resize_images.R`

| Subset | What it checks |
|--------|----------------|
| **Robustness** | Missing folder; empty folder. |
| **Parameters** | `width` accepted. |

---

## `test-table2.R` → `R/table2.R`

| Subset | What it checks |
|--------|----------------|
| **Core** | One- and multi-way tables; `data` columns; `prop`, `digits`, `useNA`, `exclude`. |
| **Chi-square** | `chi` output; correction; match **`chisq.test`** / **`prop.test`** where applicable. |
| **Proportions** | Row/column/`ALL` vs **`prop.table`**; row/column sums. |
| **Single vector** | Structure; print; `prop`+`chi` prints chi once. |
| **Comparisons in cells** | Logical expressions in 1D/2D; truncation; with `prop` and `chi`. |
| **Equivalence** | Frequencies vs **`table()`**; **`freq`** snapshot stability for several print styles. |

---

## `test-text2.R` → `R/text2.R`

| Subset | What it checks |
|--------|----------------|
| **Layout** | Alignments (incl. vectorized); padding; `cex`; extra **`graphics::text`** args. |
| **Styling** | Per-label `col`; recycled `bg`; multiple labels. |
| **Edge cases** | Small/special cases covered in the last block. |

---

## `test-t.test2.R` → `R/t.test2.R` (and printing in `R/print_t.test2.R`)

| Subset | What it checks |
|--------|----------------|
| **API** | Export; class; column set. |
| **Inputs** | Formula with/without `data`; `df$col` formula; two vectors; paired; one-sample. |
| **Column labels** | Short names; long names; 0/1 as `var=value`; long names with 0/1. |
| **Missing data** | Reporting for two-sample and paired. |
| **Equivalence** | Matches **`stats::t.test`** (incl. paired, one-sample, formula, small **n**, extremes, factor/numeric grouping). |
| **Environment** | Variables resolved from env when `data` omitted. |
| **Print snapshots** | Stable printed output across designs (incl. missing and long names). |

---

## `test-validate_plot.R` → `R/validate.R` (`validate_plot()`)

| Subset | What it checks |
|--------|----------------|
| **Happy paths** | Standard `(y, group)`; `data`; formula with/without `data`; optional `group`; `df$var`. |
| **Errors** | Invalid inputs; messages include variable name, function name, and length mismatch detail. |
| **Edge cases** | `y ~ 1`; explicit `data_name`. |

---

## `tests/testthat.R`

Loads **testthat** and the package; not a test script itself.

---

## Maintenance

After adding or renaming tests, regenerate this index (or extend the table for new `test-*.R` files). The canonical list of test files is:

`list.files("tests/testthat", pattern = "^test-.*\\.R$")`.
