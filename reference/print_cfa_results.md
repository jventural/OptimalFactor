# Print Results from AI-Assisted CFA Refinement

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

Dr. José Ventura-León

## Examples

``` r
data(Data_Personality)
model <- '
F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ4 + PPTQ5
F2 =~ PPTQ6 + PPTQ7 + PPTQ8 + PPTQ9 + PPTQ10
F3 =~ PPTQ11 + PPTQ12 + PPTQ13 + PPTQ14 + PPTQ15
'
res_cfa <- optimal_cfa_with_ai(model, Data_Personality, max_steps = 5,
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
print_cfa_results(res_cfa)
#> ===== Componentes disponibles =====
#> [1] "final_model"         "final_fit"           "log"                
#> [4] "removed_items"       "final_rmsea"         "final_cfi"          
#> [7] "conceptual_analysis"
#> 
#> ===== Log de modificaciones =====
#>   step modification mi_value      rmsea
#> 1    1 F1 =~ PPTQ15  15.5407 0.08866544
#> 2    2 F1 =~ PPTQ15  15.5407 0.08866544
#> 3    3 F1 =~ PPTQ15  15.5407 0.08866544
#> 4    4 F1 =~ PPTQ15  15.5407 0.08866544
#> 5    5 F1 =~ PPTQ15  15.5407 0.08866544
#> 
#> ===== Modelo final =====
#> F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ4 + PPTQ5
#> F2 =~ PPTQ6 + PPTQ7 + PPTQ8 + PPTQ9 + PPTQ10
#> F3 =~ PPTQ11 + PPTQ12 + PPTQ13 + PPTQ14 + PPTQ15 
#> 
#> ===== Ítems eliminados =====
#> Ítems eliminados: F1, F1, F1, F1, F1 
#> 
#> ===== Medidas finales =====
#>   • RMSEA: 0.08866544 
#>   • CFI:   0.7010782 
#> 
#> ===== Ajuste final =====
#>  srmr  wrmr 
#> 0.098 1.138 
#> 
#> ===== Carga factoriales finales =====
#>    lhs op    rhs est.std    se      z pvalue ci.lower ci.upper
#> 1   F1 =~  PPTQ1   0.427 0.088  4.833  0.000    0.254    0.600
#> 2   F1 =~  PPTQ2  -0.332 0.095 -3.509  0.000   -0.517   -0.147
#> 3   F1 =~  PPTQ3   0.302 0.096  3.133  0.002    0.113    0.490
#> 4   F1 =~  PPTQ4   0.512 0.082  6.261  0.000    0.351    0.672
#> 5   F1 =~  PPTQ5   0.684 0.067 10.155  0.000    0.552    0.816
#> 6   F2 =~  PPTQ6   0.486 0.088  5.534  0.000    0.314    0.658
#> 7   F2 =~  PPTQ7  -0.184 0.097 -1.907  0.057   -0.374    0.005
#> 8   F2 =~  PPTQ8   0.109 0.098  1.115  0.265   -0.083    0.301
#> 9   F2 =~  PPTQ9   0.336 0.093  3.615  0.000    0.154    0.518
#> 10  F2 =~ PPTQ10   0.569 0.085  6.715  0.000    0.403    0.736
#> 11  F3 =~ PPTQ11   0.212 0.090  2.349  0.019    0.035    0.389
#> 12  F3 =~ PPTQ12  -0.264 0.098 -2.698  0.007   -0.456   -0.072
#> 13  F3 =~ PPTQ13   0.103 0.076  1.347  0.178   -0.047    0.253
#> 14  F3 =~ PPTQ14  -0.235 0.094 -2.513  0.012   -0.419   -0.052
#> 15  F3 =~ PPTQ15   0.366 0.112  3.278  0.001    0.147    0.586
#> 
#> ===== Fiabilidad =====
#> $F1
#> 
#> Composite `F1` is composed of observed variables:
#>  PPTQ1, PPTQ2, PPTQ3, PPTQ4, PPTQ5
#> True-score variance is represented by common factor(s):
#>  F1
#> Total variance of composite `F1` determined from the unrestricted model.
#> The proportion attributable to "true" scores is its model-based estimate of reliability ("omega"):
#> 
#> [1] 0.361
#> 
#> $F2
#> 
#> Composite `F2` is composed of observed variables:
#>  PPTQ6, PPTQ7, PPTQ8, PPTQ9, PPTQ10
#> True-score variance is represented by common factor(s):
#>  F2
#> Total variance of composite `F2` determined from the unrestricted model.
#> The proportion attributable to "true" scores is its model-based estimate of reliability ("omega"):
#> 
#> [1] 0.249
#> 
#> $F3
#> 
#> Composite `F3` is composed of observed variables:
#>  PPTQ11, PPTQ12, PPTQ13, PPTQ14, PPTQ15
#> True-score variance is represented by common factor(s):
#>  F3
#> Total variance of composite `F3` determined from the unrestricted model.
#> The proportion attributable to "true" scores is its model-based estimate of reliability ("omega"):
#> 
#> [1] 0.002
#> 
```
