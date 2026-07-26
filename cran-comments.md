## Submission summary

New submission of OptimalFactor (version 1.3.0). The package provides an
iterative optimization algorithm for Exploratory and Confirmatory Factor
Analysis (EFA/CFA) using a machine-learning inspired boosting approach, with
adaptive composite fit indices, automatic detection of problematic items and
a heuristic CFA specification search. Version 1.3.0 adds resampling stability
of the item selection, Monte Carlo recovery of a known structure, and an
optional reliability floor.

The 1.2.2 submission was held back because the package imported
'PsyMetricTools' (same maintainer), which is not on CRAN. That dependency has
been removed: the exploratory factor analysis engine is now internal and
relies only on lavaan. The package no longer has any dependency outside
mainstream repositories, so the previous WARNING is gone.

## Test environments

* Local: Windows 11 Pro (x86_64), R 4.4.1 (R CMD check --as-cran)

## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE (on CRAN servers only): "New submission" — expected for a
  first-time submission.

## Downstream dependencies

There are currently no downstream dependencies.
