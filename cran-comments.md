## Submission summary

New submission of OptimalFactor (version 1.2.2). The package provides an
iterative optimization algorithm for Exploratory and Confirmatory Factor
Analysis (EFA/CFA) using a machine-learning inspired boosting approach, with
adaptive composite fit indices, automatic detection of problematic items and
a heuristic CFA specification search.

IMPORTANT — DO NOT SUBMIT YET: OptimalFactor imports 'PsyMetricTools'
(same maintainer), which is not yet available on CRAN (its 1.2.0 submission
of 2026-07-01 was archived by the CRAN team). The local check therefore
reports the WARNING "Strong dependencies not in mainstream repositories:
PsyMetricTools". This package must be submitted only AFTER PsyMetricTools
is accepted on CRAN; at that point the WARNING disappears and no changes
to this package are needed.

## Test environments

* Local: Windows 11 Pro (x86_64), R 4.4.1 (R CMD check --as-cran)

## R CMD check results

0 errors | 1 warning | 1 note

* WARNING: "Strong dependencies not in mainstream repositories:
  PsyMetricTools" — see note above; resolves itself once PsyMetricTools
  is on CRAN.
* NOTE: "unable to verify current time" — local environment only (offline);
  unrelated to the package.
* NOTE (on CRAN servers only): "New submission" — expected for a
  first-time submission.

## Downstream dependencies

There are currently no downstream dependencies.
