## Submission summary

Update of OptimalFactor from 1.5.0 to 1.6.0. It adds one exported function,
cfa_item_stability() (with its print method), which evaluates by subsampling
whether each item of a confirmatory model stays on its assigned factor. No
existing function changes behaviour.

## Test environments

* Local: Windows 11 Pro (x86_64), R 4.4.1 (R CMD check --as-cran)

## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: "checking for future file timestamps ... unable to verify current
  time". It comes from the local machine's time check, not from the package.

## Notes for the reviewer

* The new example is self-contained (package dataset, one core, five
  subsamples) and wrapped in \donttest{} because it fits several WLSMV models.
* The new function does not set a seed (seed = NULL by default), does not
  change options(), par() or the working directory, and writes no files.

## Downstream dependencies

There are currently no downstream dependencies.
