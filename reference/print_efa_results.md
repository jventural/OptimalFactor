# Print Results from AI-Assisted EFA Refinement

Prints a structured summary of the key outputs from
`optimal_efa_with_ai`, including the removed items, final RMSEA,
iteration count, final factor structure, step-by-step log, and final fit
indices.

## Usage

``` r
print_efa_results(res)
```

## Arguments

- res:

  A `list` returned by `optimal_efa_with_ai`, containing at least the
  components `removed_items`, `final_rmsea`, `iterations`,
  `final_structure`, `steps_log`, and `bondades_original`.

## Details

The function prints to the console in the following order:

1.  A “Summary” block with:

    - `removed_items`: the items removed during refinement.

    - `final_rmsea`: the scaled RMSEA after the last iteration.

    - `iterations`: total number of iterations performed.

2.  “Factor Structure” showing the final loadings/data frame.

3.  “Step Log” displaying the data frame of each removal step.

4.  “Final Fit Indices” printing the `bondades_original` object as
    returned by the package's internal EFA engine.

## Value

invisibly returns `NULL`. Used for its printing side effect.

## Author

Dr. José Ventura-León

## Examples

``` r
# \donttest{
data(Data_Expectativas)
res_efa <- optimal_efa_with_ai(Data_Expectativas, items = paste0("EAF", 1:10),
                               name_items = "EAF", n_factors = 2,
                               verbose = FALSE)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.425287e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -3.961038e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -3.021074e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -4.029361e-18) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.146884e-16) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.639397e-18) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -9.911103e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.312914e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.093761e-16) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 5.167954e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.804227e-18) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -4.086421e-18) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.168290e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.769096e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.467097e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 1.929053e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -4.557424e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 1.006430e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.186043e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.615004e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -7.866306e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.174611e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -3.021074e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -4.029361e-18) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -4.372041e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.263301e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
print_efa_results(res_efa)
#> 
#> ===== Resumen =====
#> Ítems eliminados: EAF1, EAF7 
#> RMSEA final: 0.069
#> Iteraciones:   2
#> ===== Estructura factorial (EFA UD) =====
#>   Items        f1        f2
#> 1  EAF6 0.8488680 0.0000000
#> 2  EAF3 0.7337088 0.0000000
#> 3  EAF5 0.6924937 0.0000000
#> 4  EAF9 0.0000000 0.9562020
#> 5  EAF8 0.0000000 0.9091477
#> 6 EAF10 0.0000000 0.8093825
#> 7  EAF4 0.0000000 0.6706339
#> 8  EAF2 0.0000000 0.6649142
#> 
#> ===== Log de pasos =====
#>   step removed_item        reason rmsea
#> 1    1         EAF1  Mejora RMSEA 0.064
#> 2    2         EAF7 Cross-loading 0.064
#> 
#> ===== Bondades de ajuste finales =====
#>   Factores chisq.scaled df.scaled  srmr  wrmr cfi.scaled tli.scaled
#> 1       f1      128.538        20 0.109 1.216      0.918      0.886
#> 2       f2       19.080        13 0.034 0.374      0.995      0.990
#>   rmsea.scaled
#> 1        0.234
#> 2        0.069
# }
```
