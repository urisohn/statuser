## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission addressing reviewer feedback.

## Test environments

* local Windows 11, R 4.5.2
* win-builder (R-devel)

## Changes in this resubmission

* Added or confirmed \value tags and return structure descriptions for exported functions.
* Replaced \dontrun examples with \donttest or runnable examples where appropriate.
* Avoided modifying the global environment in `clear()` and removed user option changes in `plot_cdf()`.

## Notes

The `clear()` function is a convenience for interactive use that performs three
cleanup operations: (1) removes objects from a user-specified environment (using
`parent.frame()` by default, but it will not modify `.GlobalEnv`), (2) clears the
console, and (3) closes open graphics devices. This function is never called
automatically by the package—users must explicitly invoke `clear()` to perform
these actions. The example now operates on a temporary environment and is wrapped
in `\donttest{}` to avoid side effects during R CMD check.

The `plot_cdf()` function previously used `options()` to record that a one-time
message had been shown. This now uses an internal package state environment instead,
so no user options are modified.

Note: The remaining NOTE is "checking for future file timestamps ... unable to verify current time"
on the local Windows build.

The S3 generic/method consistency check produces a NOTE for `t.test2`:

    checking S3 generic/method consistency ... NOTE
    t:
      function(x)
    t.test2:
      function(..., digits)

`t.test2` is an intentional standalone function name — a user-facing wrapper
around `stats::t.test`. It is not an S3 method of `base::t`. The NOTE is a
false positive from R CMD check's dot-in-name heuristic, which treats any
`foo.bar` function as an apparent method of generic `foo`. Registering it as
`S3method(t, test2)` was considered but rejected because it would create
incorrect dispatch semantics (objects of class `"test2"` calling `t()` would
be routed to a t-test function). We are not sure why this note was not triggered
with previous versions of the pkg.
