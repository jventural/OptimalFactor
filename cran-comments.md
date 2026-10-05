## Submission summary

New submission of OptimalFactor (version 1.5.0). The package provides an
iterative item-selection algorithm for exploratory and confirmatory factor
analysis (boosting with an adaptive composite fit index), a heuristic CFA
specification search, and tools to evaluate the resampling stability of the
item selection and the recovery of a known or theoretical structure.

An earlier version was prepared in 2026 but held back because it imported
'PsyMetricTools' (same maintainer), then not on CRAN. That dependency has been
removed: the exploratory factor analysis engine is internal and relies only on
lavaan.

## Test environments

* Local: Windows 11 Pro (x86_64), R 4.4.1 (R CMD check --as-cran
  --run-donttest, PDF manual built with pdflatex)
* win-builder: R Under development (unstable) (2026-09-30 r90605 ucrt):
  1 NOTE

## R CMD check results

0 errors | 0 warnings | notes:

* NOTE: "New submission". Expected for a first submission.
* The same note lists "Possibly misspelled words in DESCRIPTION": MacCallum,
  McCoach, Shi, Maydeu and Olivares are surnames of the cited authors, WLSMV
  is the name of an estimator (weighted least squares, mean- and
  variance-adjusted) and "interfactor" is the standard term for correlations
  between factors. They are correct.

## Notes for the reviewer

* Examples are self-contained and use the package datasets or simulated
  data, with at most one core. Slow examples are wrapped in \donttest{}.
  Only the calls to an external large language model service (which need a
  user-supplied API key) are in \dontrun{}; the rest of those examples run.
* Functions do not set a fixed seed (seed = NULL by default), do not change
  options(), par() or the working directory, and do not write to the user's
  file space unless a file path is supplied.
* Console output of non-print functions can be suppressed with verbose = FALSE.

## Downstream dependencies

There are currently no downstream dependencies.
