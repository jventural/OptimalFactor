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
#>    appear to be positive definite! The smallest eigenvalue (= -5.215148e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 6.753579e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -8.371825e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 9.921827e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -7.517662e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 5.373698e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -3.669894e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 1.126304e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.590754e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 7.722432e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -7.433259e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 8.937305e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -8.384103e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.275047e-17) 
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
#>    appear to be positive definite! The smallest eigenvalue (= -3.685278e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.773741e-19) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -5.200061e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 1.214989e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.872234e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.282450e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -3.921132e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -4.540120e-18) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -8.371825e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 9.921827e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.837673e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 2.327427e-18) 
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
