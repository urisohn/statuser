## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission addressing reviewer feedback.

## Test environments

* local Windows 11, R 4.5.2
* win-builder (R-devel)

## Changes in this resubmission

* Added \value tags to all exported function documentation.

## Notes

### Regarding `clear()` function

The `clear()` function is a convenience for interactive use that performs three 
cleanup operations: (1) removes objects from the calling environment (using 
`parent.frame()`), (2) clears the console, and (3) closes open graphics devices. 
This function is never called automatically by the package—users must explicitly 
invoke `clear()` to perform these actions. The example is wrapped in `\dontrun{}` 
to prevent any environment modification during R CMD check.

The environment-clearing behavior is analogous to `base::rm()`, which also allows 
users to remove objects from their environment.
