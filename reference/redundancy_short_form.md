# Redundancy-Guided Short Form of a Unidimensional Scale

Builds a short form of an (essentially) unidimensional scale by
iteratively removing the most locally-dependent item. At each step a
one-factor model is fitted, the pair of items with the largest residual
correlation is located, and the item of that pair with the *lower*
loading is dropped (keeping the stronger indicator). Pruning stops when
the target length `k` is reached or when no residual correlation exceeds
`threshold`. This addresses the common situation where a scale is
unidimensional but the one-factor model misfits because of
near-duplicate items (local dependence).

## Usage

``` r
redundancy_short_form(
  data,
  items,
  k = NULL,
  groups = NULL,
  min_per_group = 3,
  threshold = 0.15,
  min_omega = NULL,
  estimator = "WLSMV",
  ordered = TRUE
)
```

## Arguments

- data:

  Data frame with the item responses.

- items:

  Character vector with the candidate item names.

- k:

  Target number of items. If `NULL`, prunes until the largest residual
  correlation drops below `threshold`. Default `NULL`.

- groups:

  Optional named list mapping content groups to items (e.g. theoretical
  dimensions), used only to preserve at least `min_per_group` items per
  group during pruning. Default `NULL` (no constraint).

- min_per_group:

  Minimum items kept per group in `groups`. Default 3.

- threshold:

  Residual-correlation stopping threshold when `k = NULL`. Default 0.15.

- min_omega:

  Reliability floor. When set (e.g. `0.80`), an item is only dropped if
  the resulting form keeps McDonald's omega at or above this value, so
  the short form cannot buy fit at the cost of internal consistency. A
  scale that already sits below the floor is not frozen: for it the
  effective bar is its current omega, so removals that do not reduce
  reliability are still allowed. `NULL` (default) disables the check.

- estimator:

  Estimator passed to
  [`lavaan::cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html). Default
  `"WLSMV"`.

- ordered:

  Logical; treat items as ordered. Default `TRUE`.

## Value

A list with `items` (retained items), `trajectory` (data frame with
n_items, cfi, tli, rmsea, srmr, omega and the item dropped at each
step), `fit` (final one-factor `lavaan` object), `loadings`
(standardized), `omega` (McDonald's omega of the final form) and
`stop_reason` (`"target_k"`, `"threshold"`, `"min_items"`,
`"min_per_group"` or `"min_omega"`).

## Details

Removing redundant items is preferred over piling up residual
covariances, which capitalizes on chance (MacCallum, Roznowski &
Necowitz, 1992). The resulting short form should be cross-validated on
an independent sample (see
[`cross_validate_cfa`](https://jventural.github.io/OptimalFactor/reference/cross_validate_cfa.md)).

## References

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490–504.

## See also

[`cross_validate_cfa`](https://jventural.github.io/OptimalFactor/reference/cross_validate_cfa.md)

## Examples

``` r
data(Data_Expectativas)
sf <- redundancy_short_form(Data_Expectativas, paste0("EAF", 1:10), k = 6,
        groups = list(A = paste0("EAF", 1:5), B = paste0("EAF", 6:10)))
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.160079e-16) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 2.580688e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 2.873136e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.241411e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -7.291260e-18) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -7.291260e-18) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
sf$trajectory; sf$items; sf$omega
#>   n_items   cfi   tli rmsea  srmr omega dropped
#> 1      10 0.934 0.916 0.182 0.095 0.938       -
#> 2       9 0.961 0.948 0.156 0.067 0.940    EAF6
#> 3       8 0.991 0.988 0.082 0.044 0.937    EAF5
#> 4       7 0.996 0.994 0.060 0.037 0.931    EAF2
#> 5       6 1.000 1.004 0.000 0.026 0.920   EAF10
#> [1] "EAF1" "EAF3" "EAF4" "EAF7" "EAF8" "EAF9"
#> [1] 0.92
```
