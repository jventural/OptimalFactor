## Submission summary

Update of OptimalFactor from 1.5.0 to 1.8.0. The main reasons are:

* The default model of the optional AI analysis was shut down by its provider on
  2026-10-23, so that feature no longer worked with the default settings.
* Two exported functions, optimal_cfa_with_ai() and optimal_efa_with_ai(), and
  the print_cfa_results() function that only served the former, are removed. The
  package has no reverse dependencies.
* Several bug fixes (see NEWS.md), among them items that stayed in the final model
  after being removed.

It also adds cfa_item_stability() and a decision table for cross-loadings
(1.6.0 and 1.7.0, released on GitHub only).

## Test environments

* Local: Windows 11 Pro (x86_64), R 4.4.1
  (R CMD check --as-cran --run-donttest)

## R CMD check results

0 errors | 0 warnings | 2 notes

* NOTE: "checking for future file timestamps ... unable to verify current
  time". It comes from the local machine's time check, not from the package.
* NOTE: examples with elapsed time > 5s. They are wrapped in \donttest{} because
  they fit many WLSMV models on resampled or simulated data.

## Notes for the reviewer

* No example, test or vignette calls an external service. The examples that need an
  API key are wrapped in \dontrun{}.
* No function sets a seed unless the user supplies one, changes options(), par()
  or the working directory, or writes files unless a path is given.

## Downstream dependencies

There are currently no downstream dependencies.
