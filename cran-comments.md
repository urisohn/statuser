## Resubmission

This is a resubmission following an error produced in Debian testing.

## R CMD check results

0 errors | 0 warnings | 3 notes
(only one note is relevant, explained below)


## Test environments

* local Windows 11, R 4.5.2

## Changes in this resubmission

* Removed the `save.as` argument and file-export code path from `plot_means()` to fix the Debian testing error.
* Registered `t.test2` as an S3 method for `t` (`S3method(t,test2)`) and aligned the function signature to `function(x, ...)` to address the recurring S3 generic/method consistency note triggered by the dot-name heuristic.
* Updated `t.test2` documentation and `NEWS.md` accordingly.
* Removed the dependency on `labelled` to avoid CRAN macOS dependency failures; `desc_var()` now stores variable labels as base-R `"label"` attributes on output columns.

## Notes

* Previous submissions showed a NOTE from `checking S3 generic/method consistency` because of the function name `t.test2`.
  In this resubmission, `t.test2` is explicitly S3-registered for `t` and uses a compatible method signature.
  This preserves the public API (`t.test2(...)`) while making method registration explicit to avoid recurring false-positive mismatch notes.
