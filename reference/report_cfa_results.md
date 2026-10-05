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

An invisible `list` with class `"cfa_boost_report"` - see Details below
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

  Character vector - same lines that were printed

## See also

[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
[`report_efa_results`](https://jventural.github.io/OptimalFactor/reference/report_efa_results.md)

## Examples

``` r
# \donttest{
data(Data_Personality, package = "OptimalFactor")
# Run CFA boosting first to obtain an object suitable for the reporter.
model <- '
F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ4 + PPTQ5
F2 =~ PPTQ6 + PPTQ7 + PPTQ8 + PPTQ9 + PPTQ10
F3 =~ PPTQ11 + PPTQ12 + PPTQ13 + PPTQ14 + PPTQ15
'
res <- cfa_boosting(Data_Personality, model,
                    model_config = list(estimator = "MLR", ordered = FALSE),
                    verbose = FALSE)
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F1 (PPTQ1 
#>    -> PPTQ5)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F1 (PPTQ1 
#>    -> PPTQ5)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F1 (PPTQ1 
#>    -> PPTQ5)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F1 (PPTQ1 
#>    -> PPTQ5)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F1 (PPTQ1 
#>    -> PPTQ5)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3

# Pretty print to the console (default).
report_cfa_results(res)
#> 
#> ─────────────────────────────────────────────────────────────────
#> ANÁLISIS FACTORIAL CONFIRMATORIO - REPORTE DE OPTIMIZACIÓN
#> ─────────────────────────────────────────────────────────────────
#> 
#> RESUMEN DEL PROCESO
#> 
#>  Metrica                  Valor
#>  Iteraciones              8    
#>  Items eliminados         5    
#>  Covarianzas agregadas    2    
#>  Cumple todos los targets No   
#> 
#> Items eliminados: PPTQ13, PPTQ8, PPTQ7, PPTQ14, PPTQ4
#> Covarianzas agregadas: PPTQ9 ~~ PPTQ10; PPTQ11 ~~ PPTQ12
#> 
#> 
#> EVOLUCIÓN DEL RMSEA
#> 
#>   Paso  0 [0.083] ████████
#>   Paso  1 [0.093] ███████████
#>   Paso  2 [0.100] █████████████
#>   Paso  3 [0.096] ████████████
#>   Paso  4 [0.093] ███████████
#>   Paso  5 [0.082] ████████
#>   Paso  6 [0.069] █████
#>   Paso  7 [0.067] ████
#>                     └─────┴─────┴─────┴─────┘
#>                      0.05  0.08  0.10  0.13
#> 
#> 
#> EVOLUCIÓN DEL CFI
#> 
#>   Paso  0 [0.687] 
#>   Paso  1 [0.674] 
#>   Paso  2 [0.677] 
#>   Paso  3 [0.732] ██
#>   Paso  4 [0.777] █████
#>   Paso  5 [0.831] █████████
#>   Paso  6 [0.883] ████████████
#>   Paso  7 [0.894] █████████████
#>                     └─────┴─────┴─────┴─────┴─────┘
#>                      0.70  0.80  0.90  0.95  1.00
#> 
#> 
#> EVOLUCIÓN DEL SRMR
#> 
#>   Paso  0 [0.098] ███████████████████
#>   Paso  1 [0.101] ████████████████████
#>   Paso  2 [0.104] ████████████████████
#>   Paso  3 [0.099] ████████████████████
#>   Paso  4 [0.094] ██████████████████
#>   Paso  5 [0.089] █████████████████
#>   Paso  6 [0.085] ████████████████
#>   Paso  7 [0.082] ███████████████
#>                     └─────┴─────┴─────┴─────┘
#>                      0.03  0.05  0.08  0.10
#> 
#> 
#> ÍNDICES DE AJUSTE DEL MODELO FINAL
#> 
#>        Indice Valor    Estado
#>  Chi-cuadrado 43.41          
#>            gl    30          
#>         RMSEA 0.067        OK
#>           CFI 0.894 NO CUMPLE
#>           TLI 0.841          
#>          SRMR 0.082 NO CUMPLE
#> 
#> 
#> CARGAS ESTANDARIZADAS (modelo final)
#> 
#>    lhs    rhs est.std    se pvalue
#> 1   F1  PPTQ1   0.502 0.084  0.000
#> 2   F1  PPTQ2  -0.316 0.120  0.008
#> 3   F1  PPTQ3   0.337 0.115  0.003
#> 4   F1  PPTQ5   0.725 0.099  0.000
#> 5   F2  PPTQ6   0.387 0.113  0.001
#> 6   F2  PPTQ9   0.202 0.107  0.059
#> 7   F2 PPTQ10   0.503 0.160  0.002
#> 8   F3 PPTQ11   0.263 0.119  0.027
#> 9   F3 PPTQ12  -0.278 0.124  0.025
#> 10  F3 PPTQ15   0.486 0.172  0.005
#> 
#> 
#> FIABILIDAD COMPUESTA
#> 
#> $F1
#> 
#> Composite `F1` is composed of observed variables:
#>  PPTQ1, PPTQ2, PPTQ3, PPTQ5
#> True-score variance is represented by common factor(s):
#>  F1
#> Total variance of composite `F1` determined from the unrestricted model.
#> The proportion attributable to "true" scores is its model-based estimate of reliability ("omega"):
#> 
#> [1] 0.3
#> 
#> $F2
#> 
#> Composite `F2` is composed of observed variables:
#>  PPTQ6, PPTQ9, PPTQ10
#> True-score variance is represented by common factor(s):
#>  F2
#> Total variance of composite `F2` determined from the unrestricted model.
#> The proportion attributable to "true" scores is its model-based estimate of reliability ("omega"):
#> 
#> [1] 0.271
#> 
#> $F3
#> 
#> Composite `F3` is composed of observed variables:
#>  PPTQ11, PPTQ12, PPTQ15
#> True-score variance is represented by common factor(s):
#>  F3
#> Total variance of composite `F3` determined from the unrestricted model.
#> The proportion attributable to "true" scores is its model-based estimate of reliability ("omega"):
#> 
#> [1] 0.055
#> 
#> 
#> 
#> MODELO FINAL (sintaxis lavaan)
#> 
#> F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ5
#> F2 =~ PPTQ6 + PPTQ9 + PPTQ10
#> F3 =~ PPTQ11 + PPTQ12 + PPTQ15
#> PPTQ9 ~~ PPTQ10
#> PPTQ11 ~~ PPTQ12
#> 
#> ─────────────────────────────────────────────────────────────────

# Capture the structured output without printing - useful inside Shiny
# apps or scripts that need the data programmatically.
rep <- report_cfa_results(res, print = FALSE)
str(rep, max.level = 1)
#> List of 12
#>  $ type                 : chr "cfa_boosting"
#>  $ summary              :List of 4
#>  $ fit_indices          :List of 6
#>  $ targets_met          :List of 4
#>  $ removed_items        : chr [1:5] "PPTQ13" "PPTQ8" "PPTQ7" "PPTQ14" ...
#>  $ added_covariances    : chr [1:2] "PPTQ9 ~~ PPTQ10" "PPTQ11 ~~ PPTQ12"
#>  $ standardized_loadings:Classes ‘lavaan.data.frame’ and 'data.frame':   10 obs. of  5 variables:
#>  $ factor_correlations  :Classes ‘lavaan.data.frame’ and 'data.frame':   3 obs. of  4 variables:
#>  $ reliability          :List of 3
#>  $ steps_log            :'data.frame':   8 obs. of  7 variables:
#>  $ final_syntax         : chr "F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ5\nF2 =~ PPTQ6 + PPTQ9 + PPTQ10\nF3 =~ PPTQ11 + PPTQ12 + PPTQ15\nPPTQ9 ~~ PPT"| __truncated__
#>  $ text                 : chr [1:131] "" "─────────────────────────────────────────────────────────────────" "ANÁLISIS FACTORIAL CONFIRMATORIO - REPORTE DE OPTIMIZACIÓN" "─────────────────────────────────────────────────────────────────" ...
#>  - attr(*, "class")= chr [1:2] "cfa_boost_report" "list"
# }
```
