# Launch the OptimalFactor Wizard (v2)

Opens a guided 7-phase Shiny wizard built on top of
[`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
and
[`cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
with reliability and external-validity steps. Designed to match the
wizard style of the EasyValidation package so users moving between the
two feel at home.

The 7 phases are: (1) Data, (2) Configuration, (3) Initial diagnostic
(parallel analysis + MAP + Kaiser), (4) EFA boosting with post-boost
review, (5) optional CFA boosting, (6) reliability (omega + alpha by
factor), (7) external validity (Pearson correlations between factor
scores and selected numeric covariates).

The original studio app remains available via
[`run_efa_boosting`](https://jventural.github.io/OptimalFactor/reference/run_efa_boosting.md);
this wizard does not replace it.

## Usage

``` r
run_efa_boosting_wizard(launch.browser = TRUE)
```

## Arguments

- launch.browser:

  Logical. If `TRUE` (default) the app opens in the system browser;
  otherwise the app object is returned for manual launch.

## Value

Invisibly returns the Shiny app object; primarily called for side
effects.

## Required packages

shiny, shinydashboard, DT. Optional but recommended: readxl, psych,
lavaan, semTools.

## Quick start


      library(OptimalFactor)
      run_efa_boosting_wizard()

## See also

[`run_efa_boosting`](https://jventural.github.io/OptimalFactor/reference/run_efa_boosting.md),
[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
[`report_efa_results`](https://jventural.github.io/OptimalFactor/reference/report_efa_results.md),
[`report_cfa_results`](https://jventural.github.io/OptimalFactor/reference/report_cfa_results.md).
