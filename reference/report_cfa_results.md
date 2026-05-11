# Report the CFA boosting results

Counterpart of
[`report_efa_results`](https://jventural.github.io/OptimalFactor/reference/report_efa_results.md)
for objects returned by
[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md).
Side-effect: writes a human-readable report to the console. Return
value: an invisible `list` (class `"cfa_boost_report"`) with the same
information structured for programmatic use, including a `text` field
with the formatted lines so any frontend can re-render exactly what the
console showed.

## Usage

``` r
report_cfa_results(res, show_plot = TRUE, print = TRUE)
```

## Arguments

- res:

  A list produced by
  [`cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md).
  Expected fields include `removed_items`, `added_covariances`,
  `fit_indices`, `targets_met`, `iterations`, `standardized_loadings`,
  `factor_correlations`, `reliability`, `steps_log`, `final_syntax`.

- show_plot:

  Logical. If `TRUE` (default), include ASCII charts of RMSEA / CFI /
  SRMR evolution.

- print:

  Logical. If `TRUE` (default), write the formatted report to the
  console. Set to `FALSE` when only the structured list is needed.

## Value

An invisible `list` with class `"cfa_boost_report"` — see Details below
for fields.

## Details

Fields of the returned list:

- type:

  `"cfa_boosting"`

- summary:

  Single-row list with iterations, n_removed_items, n_added_covariances,
  targets_all_met

- fit_indices:

  Final-model rmsea, cfi, tli, srmr, chisq, df

- targets_met:

  Logical flags by index from `res$targets_met`

- removed_items:

  Character vector

- added_covariances:

  Character vector with "x \~~ y" strings

- standardized_loadings:

  Loadings data.frame

- factor_correlations:

  Phi matrix

- reliability:

  Reliability table (composite/AVE/etc.)

- steps_log:

  Per-iteration log

- final_syntax:

  The final lavaan syntax used

- text:

  Character vector — same lines that were printed

## See also

[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
[`report_efa_results`](https://jventural.github.io/OptimalFactor/reference/report_efa_results.md)

## Examples

``` r
# \donttest{
data(Data_Personality, package = "OptimalFactor")
# Run CFA boosting first to obtain an object suitable for the reporter.
res <- cfa_boosting(
  data        = Data_Personality,
  name_items  = "P",
  n_factors   = 3,
  estimator   = "WLSMV",
  rotation    = "oblimin",
  fit_targets = list(cfi = 0.95, tli = 0.95,
                      rmsea = 0.06, srmr = 0.08))
#> Error in cfa_boosting(data = Data_Personality, name_items = "P", n_factors = 3,     estimator = "WLSMV", rotation = "oblimin", fit_targets = list(cfi = 0.95,         tli = 0.95, rmsea = 0.06, srmr = 0.08)): unused arguments (name_items = "P", n_factors = 3, estimator = "WLSMV", rotation = "oblimin", fit_targets = list(cfi = 0.95, tli = 0.95, rmsea = 0.06, srmr = 0.08))

# Pretty print to the console (default).
report_cfa_results(res)
#> Error: object 'res' not found

# Capture the structured output without printing — useful inside Shiny
# apps or scripts that need the data programmatically.
rep <- report_cfa_results(res, print = FALSE)
#> Error: object 'res' not found
str(rep, max.level = 1)
#> function (x, ...)  
rep$fit_indices
#> Error in rep$fit_indices: object of type 'special' is not subsettable
rep$reliability
#> Error in rep$reliability: object of type 'special' is not subsettable
cat(paste(rep$text, collapse = "\n"))
#> Error in rep$text: object of type 'special' is not subsettable
# }
```
