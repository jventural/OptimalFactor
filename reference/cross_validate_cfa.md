# Split-Half Cross-Validation of a Factor Model

Assesses the out-of-sample stability of a one-factor scale by repeated
split-half cross-validation. The sample is randomly split many times; on
each split the one-factor model is fitted and its fit and reliability
are recorded on independent subsamples. Two modes are available: (a)
confirm a **fixed** item set (default), and (b) test the **procedure**
by re-deriving a short form with
[`redundancy_short_form`](https://jventural.github.io/OptimalFactor/reference/redundancy_short_form.md)
in the calibration half and confirming it in the validation half (set
`derive_k`). Mode (b) also returns how often each item is selected,
exposing whether a data-driven short form capitalizes on chance.

## Usage

``` r
cross_validate_cfa(
  data,
  items,
  n_splits = 200,
  derive_k = NULL,
  groups = NULL,
  min_per_group = 3,
  estimator = "WLSMV",
  targets = c(cfi = 0.95, rmsea = 0.08),
  seed = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame with the item responses.

- items:

  Character vector with the item names of the scale to validate (fixed
  mode) or the candidate pool (derivation mode).

- n_splits:

  Number of random splits. Default 200.

- derive_k:

  If `NULL` (default), the fixed `items` set is confirmed across holdout
  subsamples. If an integer, a `derive_k`-item short form is re-derived
  in each calibration half and confirmed in the validation half.

- groups, min_per_group:

  Passed to
  [`redundancy_short_form`](https://jventural.github.io/OptimalFactor/reference/redundancy_short_form.md)
  when `derive_k` is set (to preserve content coverage). Default `NULL`,
  3.

- estimator:

  Estimator passed to
  [`lavaan::cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html). Default
  `"WLSMV"`.

- targets:

  Named numeric vector of pass thresholds for the fraction of subsamples
  meeting fit. Default `c(cfi = 0.95, rmsea = 0.08)`.

- seed:

  Optional integer seed for reproducibility; NULL (default) leaves the
  RNG untouched.

- verbose:

  Logical. If `TRUE` (default), prints the summary tables to the
  console.

## Value

A list. In fixed mode: `summary` (P10/P50/P90 of cfi, rmsea, srmr, omega
across holdout subsamples) and `pct_meeting` (fraction meeting all
`targets`). In derivation mode it adds `selection_freq` (per-item
selection frequency) and the confirmation summary of the derived forms.

## References

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490–504.

## See also

[`redundancy_short_form`](https://jventural.github.io/OptimalFactor/reference/redundancy_short_form.md)

## Examples

``` r
# \donttest{
data(Data_Expectativas)
items <- paste0("EAF", 1:10)
# confirm a fixed item set out of sample
cross_validate_cfa(Data_Expectativas, items, n_splits = 3, seed = 1)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.739570e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.546582e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.526277e-16) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -3.933067e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -8.050545e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.023916e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Cross-validacion split-half de 10 items en 6 submuestras (n~50)
#>       cfi rmsea  srmr omega
#> 10% 0.928 0.121 0.100 0.928
#> 50% 0.952 0.155 0.107 0.941
#> 90% 0.965 0.229 0.119 0.952
#> % submuestras que cumplen CFI>=0.95 y RMSEA<=0.08: 0.0%
# test the derivation procedure itself
cross_validate_cfa(Data_Expectativas, items, derive_k = 6, n_splits = 3,
                   groups = list(A = paste0("EAF", 1:5),
                                 B = paste0("EAF", 6:10)),
                   seed = 1)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.739570e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 6.634269e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 2.273436e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.526277e-16) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.080510e-16) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.524565e-16) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -7.148280e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -8.050545e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.480139e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.335023e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.581580e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Estabilidad del procedimiento: derivar 6 items en calibracion y confirmar en validacion (3 derivaciones)
#> Frecuencia de seleccion por item:
#>  EAF1  EAF8  EAF9  EAF2  EAF4  EAF3  EAF5  EAF6  EAF7 EAF10 
#>  1.00  1.00  1.00  0.67  0.67  0.33  0.33  0.33  0.33  0.33 
#> Ajuste de las formas derivadas, confirmadas fuera de muestra:
#>       cfi rmsea  srmr omega
#> 10% 0.974 0.025 0.045 0.901
#> 50% 0.988 0.124 0.084 0.904
#> 90% 0.998 0.150 0.086 0.924
#> % que cumplen CFI>=0.95 y RMSEA<=0.08: 33.3%
# }
```
