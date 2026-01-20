# Console Report for EFA Optimization Results

Prints a compact, Spanish-language console report summarizing the
iterative optimization performed by
[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md).
The report includes a process summary table, three independent ASCII
evolution plots (RMSEA, SRMR, CFI), a stepwise removal log, the final
thresholded loading structure, and common fit indices with rule-of-thumb
interpretations. Designed for quick inspection in interactive sessions;
returns `invisible(NULL)`.

## Usage

``` r
report_efa_results(res, show_plot = TRUE)
```

## Arguments

- `res`:

  A result list as returned by
  [`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
  expected to contain:

  - `$final_structure`: data frame of thresholded factor loadings and
    item labels.

  - `$removed_items`: character vector of removed items in order.

  - `$steps_log`: data frame with columns `step`, `removed_item`,
    `reason`, and—if available—`rmsea`, `srmr`, `cfi`.

  - `$final_rmsea`: final scaled RMSEA.

  - `$iterations`: number of iterations executed.

  - `$bondades_original`: fit indices (columns: `chisq.scaled`,
    `df.scaled`, `rmsea.scaled`, `cfi.scaled`, `tli.scaled`, `srmr`).

  - `$inter_factor_correlation`: optional factor correlation matrix.

  - `$interfactor_check`: optional list describing correlation-threshold
    checks.

- `show_plot`:

  Logical; if `TRUE`, prints three ASCII evolution bars (RMSEA, SRMR,
  CFI).

## Details

**Headers and language.** All headings and labels are printed in Spanish
(e.g., “RESUMEN DEL PROCESO”, “EVOLUCIÓN DEL RMSEA”, “EVOLUCIÓN DEL
SRMR”, “EVOLUCIÓN DEL CFI”, “ÍNDICES DE AJUSTE DEL MODELO FINAL”).

**Process summary.** The function computes and prints:

- Initial item count (`n_initial`) and final count (`n_final`).

- Percent retained.

- Initial and final RMSEA.

- Percent reduction in RMSEA.

- Iterations executed.

These appear in a compact two-column table.

**ASCII evolution plots (RMSEA, SRMR, CFI).** When `show_plot = TRUE`,
the function prints three separate bar-style ASCII diagnostics:

- *RMSEA evolution:* bar length increases with RMSEA, scaled to the
  reference range 0.05–0.13.

- *SRMR evolution:* bar length increases with SRMR, scaled to 0.03–0.10.

- *CFI evolution:* bar length increases with CFI *(higher = better)*,
  scaled linearly between 0.70 and 1.00.

Each block includes an ASCII axis guide:

- RMSEA: ticks at 0.05, 0.08, 0.10, 0.13.

- SRMR: ticks at 0.03, 0.05, 0.08, 0.10.

- CFI: ticks at 0.70, 0.80, 0.90, 0.95, 1.00.

Bars are drawn using `█` and capped at a configurable length (default:
20 characters).

**Item removal log.** The stepwise elimination table mirrors
`steps_log`, formatting available fit indices (RMSEA, SRMR, CFI) to
three decimals.

**Final structure.** Prints the thresholded loading matrix produced by
[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
(loadings \< 0.30 appear as 0).

**Final fit indices and qualitative labels.** Extracts the last row of
`bondades_original` and prints:

- Scaled \\\chi^2\\, scaled df, RMSEA, CFI, TLI, and SRMR.

- Quick qualitative labels:

  - RMSEA: `<= .05 = Excelente`, `<= .08 = Bueno`, `<= .10 = Mediocre`,
    else `Pobre`.

  - CFI/TLI: `>= .95 = Excelente`, `>= .90 = Bueno`, else `Pobre`.

  - SRMR: `<= .08 = Bueno`, else `Pobre`.

These labels are intended for rapid screening and should be interpreted
in context.

**Unicode.** The report uses UTF-8 box characters (`█`) and accented
text. If your console lacks UTF-8 support, glyphs may degrade.

## Value

`invisible(NULL)`. This function prints to the console and does not
return structured output.

## See also

[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
[`print_conceptual_analysis`](https://jventural.github.io/OptimalFactor/reference/print_conceptual_analysis.md),
[`export_conceptual_analysis`](https://jventural.github.io/OptimalFactor/reference/export_conceptual_analysis.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(123)
X <- as.data.frame(matrix(rnorm(300 * 9), ncol = 9))
names(X) <- paste0("DP", 1:9)

res <- efa_boosting(
  data = X,
  name_items = "DP",
  item_range = c(1, 9),
  n_factors = 3,
  thresholds = list(
    rmsea = 0.08, loading = 0.30, min_items_per_factor = 3,
    heywood_tol = 1e-6, near_heywood = 0.015
  ),
  model_config = list(estimator = "WLSMV", rotation = "oblimin")
)

report_efa_results(res, show_plot = TRUE)
} # }
```
