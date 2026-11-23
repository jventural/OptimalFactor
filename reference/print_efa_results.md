# Print Results from an EFA Refinement

Prints a structured summary of the key outputs from
[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
or
[`efa_optimizer`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md),
including the removed items, final RMSEA, iteration count, final factor
structure, step-by-step log, and final fit indices.

## Usage

``` r
print_efa_results(res)
```

## Arguments

- res:

  A `list` returned by
  [`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  or
  [`efa_optimizer`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md),
  containing at least the components `removed_items`, `final_rmsea`,
  `iterations`, `final_structure`, `steps_log`, and `bondades_original`.

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
res_efa <- efa_boosting(Data_Expectativas, name_items = "EAF", n_factors = 2,
                        verbose = FALSE)
#> Boosting iter 1: 10 items activos, 0 eliminados hasta ahora
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.771684e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.138620e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Boosting iter 2: 9 items activos, 1 eliminados hasta ahora
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -3.259124e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.935731e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#>   [iter 2] Greedy: evaluando 9 candidatos para mejorar el ajuste...
#>     cand 1/9: probando sin 'EAF1'...
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.303154e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#>     cand 2/9: probando sin 'EAF2'...
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 1.711715e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#>     cand 3/9: probando sin 'EAF3'...
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.853374e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#>     cand 4/9: probando sin 'EAF4'...
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 1.744928e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#>     cand 5/9: probando sin 'EAF5'...
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.148593e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#>     cand 6/9: probando sin 'EAF6'...
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.905596e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#>     cand 7/9: probando sin 'EAF8'...
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 5.626241e-20) 
#>    is close to zero. This may be a symptom that the model is not identified.
#>     cand 8/9: probando sin 'EAF9'...
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 6.291789e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#>     cand 9/9: probando sin 'EAF10'...
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.152214e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Boosting iter 3: 8 items activos, 2 eliminados hasta ahora
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.492169e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.303154e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
print_efa_results(res_efa)
#> 
#> ===== Resumen =====
#> Ítems eliminados: EAF7, EAF1 
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
#>   step removed_item                   reason rmsea  srmr   cfi
#> 1    1         EAF7 Cross-loading (priority) 0.088 0.042 0.989
#> 2    2         EAF1            Composite fit 0.069 0.034 0.995
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
