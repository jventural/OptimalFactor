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
if (FALSE) { # \dontrun{
data(Data_Personality, package = "OptimalFactor")
# Run CFA boosting first to obtain an object suitable for the reporter.
model <- '
F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ4 + PPTQ5
F2 =~ PPTQ6 + PPTQ7 + PPTQ8 + PPTQ9 + PPTQ10
F3 =~ PPTQ11 + PPTQ12 + PPTQ13 + PPTQ14 + PPTQ15
'
res <- cfa_boosting(Data_Personality, model)

# Pretty print to the console (default).
report_cfa_results(res)

# Capture the structured output without printing — useful inside Shiny
# apps or scripts that need the data programmatically.
rep <- report_cfa_results(res, print = FALSE)
str(rep, max.level = 1)
rep$fit_indices
rep$reliability
cat(paste(rep$text, collapse = "\n"))
} # }
```
