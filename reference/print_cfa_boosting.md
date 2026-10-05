# Print CFA-Boosting Results

Formatted console printer for the object returned by
[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md).
Displays fit indices, the iteration trail, factor loadings (above a
threshold), latent correlations, reliability coefficients and the final
lavaan model syntax.

## Usage

``` r
print_cfa_boosting(
  result,
  show_loadings = TRUE,
  show_correlations = TRUE,
  show_reliability = TRUE,
  show_steps = TRUE,
  show_model = TRUE,
  loading_threshold = 0.30,
  digits = 3
)
```

## Arguments

- result:

  Output from
  [`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md).

- show_loadings:

  Logical. Print standardized factor loadings of the final model.
  Default `TRUE`.

- show_correlations:

  Logical. Print latent factor correlations. Default `TRUE`.

- show_reliability:

  Logical. Print omega and alpha by factor. Default `TRUE`.

- show_steps:

  Logical. Print the per-iteration log of move / drop / cov operations
  and the resulting fit indices. Default `TRUE`.

- show_model:

  Logical. Print the final lavaan model syntax. Default `TRUE`.

- loading_threshold:

  Numeric. Loadings below this absolute value are hidden in the printed
  table. Default `0.30`.

- digits:

  Integer. Number of decimal places. Default `3`.

## Value

Invisibly returns `result`; called for its side effects on the console.

## See also

[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
[`export_cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/export_cfa_boosting.md)

## Examples

``` r
# \donttest{
data(Data_Personality, package = "OptimalFactor")
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

# Full printout (default).
print_cfa_boosting(res)
#> 
#> ====================================================================== 
#>    RESULTADOS CFA BOOSTING
#> ====================================================================== 
#> 
#> --- RESUMEN ---
#> 
#> Iteraciones realizadas: 8 
#> Items eliminados: PPTQ13, PPTQ8, PPTQ7, PPTQ14, PPTQ4 
#> Covarianzas agregadas: 2 
#> 
#> --- ÍNDICES DE AJUSTE ---
#> 
#> Índice              Valor   Criterio     Estado
#> -------------------------------------------------- 
#> RMSEA                0.067    <= 0.08         OK
#> CFI                  0.894    >= 0.95  NO CUMPLE
#> TLI                  0.841    >= 0.95  NO CUMPLE
#> SRMR                 0.082    <= 0.08  NO CUMPLE
#> Chi-cuadrado          43.4                      
#> df                      30                      
#> 
#> *** MODELO NO CUMPLE TODOS LOS CRITERIOS ***
#> 
#> --- CARGAS FACTORIALES ESTANDARIZADAS ---
#> 
#> 
#>   F1:
#>   Item        Carga       SE        p         
#>   --------------------------------------------- 
#>   PPTQ1       0.502    0.084    <.001         
#>   PPTQ2      -0.316    0.120    0.008         
#>   PPTQ3       0.337    0.115    0.003         
#>   PPTQ5       0.725    0.099    <.001         
#> 
#>   F2:
#>   Item        Carga       SE        p         
#>   --------------------------------------------- 
#>   PPTQ6       0.387    0.113    <.001         
#>   PPTQ9       0.202    0.107    0.059   * BAJA
#>   PPTQ10      0.503    0.160    0.002         
#> 
#>   F3:
#>   Item        Carga       SE        p         
#>   --------------------------------------------- 
#>   PPTQ11      0.263    0.119    0.027   * BAJA
#>   PPTQ12     -0.278    0.124    0.025   * BAJA
#>   PPTQ15      0.486    0.172    0.005         
#> 
#>   * Cargas < 0.3 se consideran bajas
#> 
#> --- CORRELACIONES ENTRE FACTORES ---
#> 
#>   Factor 1     Factor 2            r        p
#>   --------------------------------------------- 
#>   F1           F2              1.365    <.001
#>   F1           F3              1.430    <.001
#>   F2           F3              1.401    0.004
#> 
#> --- FIABILIDAD (Omega) ---
#> 
#>   Factor             Omega Interpretación
#>   ---------------------------------------- 
#>   F1                 0.300      Pobre
#>   F2                 0.271      Pobre
#>   F3                 0.055      Pobre
#> 
#> --- HISTORIAL DE OPTIMIZACIÓN ---
#> 
#>   Paso Acción          Detalle                          RMSEA     CFI    SRMR
#>   --------------------------------------------------------------------------- 
#>   0    Inicial          Base                             0.083   0.687   0.098
#>   1    Remove Item (below loading floor) - PPTQ13 (F3, λ=0.103)          0.093   0.674   0.101
#>   2    Remove Item (below loading floor) - PPTQ8 (F2, λ=0.111)           0.100   0.677   0.104
#>   3    Remove Item (below loading floor) - PPTQ7 (F2, λ=-0.180)          0.096   0.732   0.099
#>   4    Remove Item (below loading floor) - PPTQ14 (F3, λ=-0.218)         0.093   0.777   0.094
#>   5    Elim. item       - PPTQ4 (F1) PPTQ1 ~~ PPTQ4      0.082   0.831   0.089
#>   6    Agregar cov      + PPTQ9 ~~ PPTQ10 (MI=4.5)       0.069   0.883   0.085
#>   7    Agregar cov      + PPTQ11 ~~ PPTQ12 (MI=4.2)      0.067   0.894   0.082
#> 
#> --- COVARIANZAS DE ERROR AGREGADAS ---
#> 
#>    PPTQ9 ~~ PPTQ10 
#>    PPTQ11 ~~ PPTQ12 
#> 
#> --- SINTAXIS DEL MODELO FINAL (para lavaan) ---
#> 
#> F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ5
#> F2 =~ PPTQ6 + PPTQ9 + PPTQ10
#> F3 =~ PPTQ11 + PPTQ12 + PPTQ15
#> PPTQ9 ~~ PPTQ10
#> PPTQ11 ~~ PPTQ12 
#> 
#> --- ADVERTENCIAS ---
#> 
#>   !  Items con cargas bajas: PPTQ9, PPTQ11, PPTQ12 
#>   !  Factores con fiabilidad < 0.70: F1, F2, F3 
#> 
#> ====================================================================== 

# Compact printout - hide steps log, raise the loading threshold, and
# show fewer decimals when reporting in a slide deck.
print_cfa_boosting(res,
  show_steps        = FALSE,
  loading_threshold = 0.40,
  digits            = 2)
#> 
#> ====================================================================== 
#>    RESULTADOS CFA BOOSTING
#> ====================================================================== 
#> 
#> --- RESUMEN ---
#> 
#> Iteraciones realizadas: 8 
#> Items eliminados: PPTQ13, PPTQ8, PPTQ7, PPTQ14, PPTQ4 
#> Covarianzas agregadas: 2 
#> 
#> --- ÍNDICES DE AJUSTE ---
#> 
#> Índice              Valor   Criterio     Estado
#> -------------------------------------------------- 
#> RMSEA                 0.07    <= 0.08         OK
#> CFI                   0.89    >= 0.95  NO CUMPLE
#> TLI                   0.84    >= 0.95  NO CUMPLE
#> SRMR                  0.08    <= 0.08  NO CUMPLE
#> Chi-cuadrado          43.4                      
#> df                      30                      
#> 
#> *** MODELO NO CUMPLE TODOS LOS CRITERIOS ***
#> 
#> --- CARGAS FACTORIALES ESTANDARIZADAS ---
#> 
#> 
#>   F1:
#>   Item        Carga       SE        p         
#>   --------------------------------------------- 
#>   PPTQ1       0.502    0.084    <.001         
#>   PPTQ2      -0.316    0.120    0.008   * BAJA
#>   PPTQ3       0.337    0.115    0.003   * BAJA
#>   PPTQ5       0.725    0.099    <.001         
#> 
#>   F2:
#>   Item        Carga       SE        p         
#>   --------------------------------------------- 
#>   PPTQ6       0.387    0.113    <.001   * BAJA
#>   PPTQ9       0.202    0.107    0.059   * BAJA
#>   PPTQ10      0.503    0.160    0.002         
#> 
#>   F3:
#>   Item        Carga       SE        p         
#>   --------------------------------------------- 
#>   PPTQ11      0.263    0.119    0.027   * BAJA
#>   PPTQ12     -0.278    0.124    0.025   * BAJA
#>   PPTQ15      0.486    0.172    0.005         
#> 
#>   * Cargas < 0.4 se consideran bajas
#> 
#> --- CORRELACIONES ENTRE FACTORES ---
#> 
#>   Factor 1     Factor 2            r        p
#>   --------------------------------------------- 
#>   F1           F2              1.365    <.001
#>   F1           F3              1.430    <.001
#>   F2           F3              1.401    0.004
#> 
#> --- FIABILIDAD (Omega) ---
#> 
#>   Factor             Omega Interpretación
#>   ---------------------------------------- 
#>   F1                 0.300      Pobre
#>   F2                 0.271      Pobre
#>   F3                 0.055      Pobre
#> 
#> --- COVARIANZAS DE ERROR AGREGADAS ---
#> 
#>    PPTQ9 ~~ PPTQ10 
#>    PPTQ11 ~~ PPTQ12 
#> 
#> --- SINTAXIS DEL MODELO FINAL (para lavaan) ---
#> 
#> F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ5
#> F2 =~ PPTQ6 + PPTQ9 + PPTQ10
#> F3 =~ PPTQ11 + PPTQ12 + PPTQ15
#> PPTQ9 ~~ PPTQ10
#> PPTQ11 ~~ PPTQ12 
#> 
#> --- ADVERTENCIAS ---
#> 
#>   !  Items con cargas bajas: PPTQ2, PPTQ3, PPTQ6, PPTQ9, PPTQ11, PPTQ12 
#>   !  Factores con fiabilidad < 0.70: F1, F2, F3 
#> 
#> ====================================================================== 
# }
```
