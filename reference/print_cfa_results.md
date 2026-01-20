# Print Results from AI‐Assisted CFA Refinement

Prints a structured report of key outputs from `optimal_cfa_with_ai`
applied to a Confirmatory Factor Analysis (CFA), including available
components, modification log, final model syntax, removed items, fit
indices, standardized loadings, and reliability.

## Usage

``` r
print_cfa_results(res)
```

## Arguments

- res:

  A `list` returned by `optimal_cfa_with_ai` (CFA version), containing
  at minimum the components: `log`, `final_model`, `removed_items`,
  `final_rmsea`, `final_cfi`, and `final_fit`.

## Details

The function prints the following sections to the console:

1.  **Available components**: names of all elements in `res`.

2.  **Modification log**: data frame `res\$log` with each removal step.

3.  **Final model**: the CFA model syntax stored in `res\$final_model`.

4.  **Removed items**: list of items removed during refinement.

5.  **Final fit measures**: `res\$final_rmsea` and `res\$final_cfi`.

6.  **Additional fit indices**: retrieved via
    `lavaan::fitMeasures(res$final_fit, c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled"))`,
    including the scaled chi-square statistic, scaled degrees of
    freedom, SRMR, WRMR, scaled CFI, scaled TLI, and scaled RMSEA.

7.  **Standardized loadings**: extracted from
    `lavaan::standardizedsolution(res$final_fit)` and filtered to
    measurement paths (`op == "=~"`), showing the final factor loadings
    for each indicator.

8.  **Reliability**: composite reliability estimates computed with
    `semTools::compRelSEM(res$final_fit, tau.eq = FALSE, ord.scale = TRUE)`,
    indicating the internal consistency of each factor under an ordinal
    measurement model.

## Value

Invisibly returns `NULL`. The primary purpose is to print results.

## Author

Dr. José Ventura‐León

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume you have run a CFA with optimal_cfa_with_ai:
# res_cfa <- optimal_cfa_with_ai(...)

# Print the organized results:
print_cfa_results(res_cfa)
} # }
```
