# Heuristic Specification Search for CFA Models (deprecated)

**\[Deprecated\]** This fit-only search has been superseded by
[`specification_search_theory`](https://jventural.github.io/OptimalFactor/reference/specification_search_theory.md),
which adds a theory-congruence term to the loss and avoids drifting
toward models that fit well but break the intended factor structure.
`specification_search()` is kept for backward compatibility and emits a
deprecation warning on every call; new code should use
[`specification_search_theory()`](https://jventural.github.io/OptimalFactor/reference/specification_search_theory.md)
(set `theory_weight = 0` there to reproduce the fit-only behaviour).

Performs a heuristic specification search over CFA models in the spirit
of MacCallum (1986). For each seed configuration (with a fixed number of
factors), the algorithm runs a hill-climbing loop that evaluates three
local operations at each step: **move** an item between factors,
**drop** an item, and **cov** (add a residual covariance suggested by
modification indices). Optionally, a bifactor variant is fitted for
every seed with k \>= 2. The function returns every configuration
evaluated, the subset that meets the user-supplied fit targets, and the
best model under a composite loss.

## Usage

``` r
specification_search(
  data,
  items,
  seeds = NULL,
  max_factors = 4,
  min_items_factor = 2,
  cfi_target = 0.95,
  rmsea_target = 0.08,
  srmr_target = 0.08,
  max_iter_per_config = 40,
  max_covs = 5,
  max_items_removed = 6,
  try_bifactor = TRUE,
  operations = c("move", "drop", "cov"),
  estimator = "WLSMV",
  ordered = TRUE,
  std.lv = TRUE,
  mi_min = 10,
  mi_top = 3,
  loss_weights = c(rmsea = 0.5, cfi = 0.3, srmr = 0.2),
  patience = 8,
  early_stop_after_meet = 3,
  n_cores = 1,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame with the observed item responses.

- items:

  Character vector with the names of the items to be searched over.

- seeds:

  Named list of initial configurations. Names are the number of factors
  as character (e.g. `"3"`). Each element is a list of seeds, where
  every seed is a named list `list(F1 = c("it1","it2"), ...)`. If
  `NULL`, default block-partition seeds are generated for k = 1 to
  `max_factors`.

- max_factors:

  Maximum number of factors when `seeds = NULL`.

- min_items_factor:

  Minimum items per factor preserved during move/drop.

- cfi_target, rmsea_target, srmr_target:

  Fit thresholds. A model is flagged as successful if CFI \>=
  `cfi_target` and RMSEA \<= `rmsea_target`.

- max_iter_per_config:

  Maximum hill-climbing iterations per seed.

- max_covs:

  Maximum residual covariances added per configuration.

- max_items_removed:

  Maximum items removed across all factors per configuration.

- try_bifactor:

  If `TRUE`, fit bifactor variant for every k \>= 2 seed.

- operations:

  Subset of `c("move","drop","cov")` controlling enabled ops.

- estimator:

  Estimator passed to
  [`lavaan::cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- ordered:

  If `TRUE`, items are treated as ordered.

- std.lv:

  If `TRUE`, latent variances are fixed to one.

- mi_min:

  Minimum modification-index value considered for covariances.

- mi_top:

  Maximum number of top-MI candidates examined at each step.

- loss_weights:

  Named numeric vector with weights for the composite loss.

- patience:

  Iterations without improvement that stop the hill climb.

- early_stop_after_meet:

  Extra iterations allowed after targets are met.

- n_cores:

  Number of CPU cores for parallel evaluation of the candidate models
  within each greedy iteration (they are independent). Default 1
  (sequential). Values \> 1 use a PSOCK cluster (works on Windows, where
  `fork` is unavailable); results are identical to the sequential run,
  only faster. A practical choice is `parallel::detectCores() - 1`.

- verbose:

  Print progress and the MacCallum warning.

## Details

**Warning (MacCallum, 1986).** Specification search capitalizes on
chance: the more configurations explored, the more likely the resulting
model is sample-specific. If you publish results obtained with this
function you should (a) report the procedure transparently as
exploratory, (b) cross-validate the chosen model with an independent
sample or via bootstrap, and (c) justify each accepted modification on
substantive theoretical grounds. The function prints this warning on
every call unless `verbose = FALSE`.

## Value

An object of class `specification_search`, a list with:

- `table`: data frame, one row per configuration, ordered by loss.

- `successful`: subset of `table` that meets the targets.

- `best`: the best result (`fit`, `syntax`, `factors`, `covs`,
  `indices`, `bifactor`, `meets`, `loss`).

- `results`: every configuration evaluated.

- `call`: the matched call.

## References

MacCallum, R. C. (1986). Specification searches in covariance structure
modeling. *Psychological Bulletin, 100*(1), 107–120.

Saris, W. E., Satorra, A., & van der Veld, W. M. (2009). Testing
structural equation models or detection of misspecifications?
*Structural Equation Modeling, 16*(4), 561–582.

## See also

[`specification_search_theory`](https://jventural.github.io/OptimalFactor/reference/specification_search_theory.md)
(recommended replacement),
[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md)

## Examples

``` r
# \donttest{
data(Data_Expectativas)
items <- paste0("EAF", 1:10)

res <- specification_search(
  data                = Data_Expectativas,
  items               = items,
  max_factors         = 2,
  max_iter_per_config = 5,
  estimator           = "MLR",
  ordered             = FALSE,
  try_bifactor        = FALSE,
  verbose             = TRUE
)
#> Warning: specification_search() is deprecated: use specification_search_theory(), which adds a theory-congruence penalty to the loss (theory_weight = 0 reproduces the fit-only search).
#> 
#> ========================================================================
#>  Specification Search (MacCallum, 1986)
#> ========================================================================
#>  WARNING: specification search capitalizes on chance. Use this
#>  function only as an EXPLORATORY device. Recommended practice:
#>    (1) Report the procedure transparently as exploratory.
#>    (2) Cross-validate the chosen model with an independent
#>        sample or via bootstrap.
#>    (3) Justify each accepted modification on theoretical grounds.
#> ========================================================================
#> 
#>  Items: 10 | Max factors: 2 | Operations: move/drop/cov | Bifactor: FALSE 
#>  Targets: CFI >= 0.95  | RMSEA <= 0.08  | SRMR <= 0.08 
#> 
#> --- k = 1 factor(s) ---
#>   Config: k1_s1 (standard)
#>     [k1_s1] iter 1: DROP EAF6 from G -> CFI=0.9061 RMSEA=0.0972 SRMR=0.0672 loss=0.7245
#>     [k1_s1] iter 2: DROP EAF5 from G -> CFI=0.9752 RMSEA=0.0550 SRMR=0.0453 loss=0.0000
#> --- k = 2 factor(s) ---
#>   Config: k2_s1 (standard)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>     [k2_s1] iter 1: DROP EAF6 from F2 -> CFI=0.9136 RMSEA=0.0950 SRMR=0.0647 loss=0.6131
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>     [k2_s1] iter 2: DROP EAF5 from F1 -> CFI=0.9713 RMSEA=0.0608 SRMR=0.0448 loss=0.0000
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> 
#> Models evaluated: 2 

print(res, top = 5)
#> Specification search: 2 configurations evaluated
#> Successful (meets CFI/RMSEA targets): 2 
#> 
#> Top 2 by composite loss:
#>  config n_factors bifactor n_items n_covs    cfi    tli  rmsea   srmr chisq df
#>   k1_s1         1    FALSE       8      0 0.9752 0.9653 0.0550 0.0453 26.06 20
#>   k2_s1         2    FALSE       8      0 0.9713 0.9577 0.0608 0.0448 26.02 19
#>  loss meets
#>     0  TRUE
#>     0  TRUE
#> 
#> Best model (k1_s1):
#>   CFI=0.9752 | RMSEA=0.0550 | SRMR=0.0453 | loss=0.0000 | meets=TRUE
#>   Factor assignment:
#>     G: EAF1, EAF2, EAF3, EAF4, EAF7, EAF8, EAF9, EAF10
res$successful
#>   config n_factors bifactor n_items n_covs    cfi    tli  rmsea   srmr chisq df
#> 1  k1_s1         1    FALSE       8      0 0.9752 0.9653 0.0550 0.0453 26.06 20
#> 2  k2_s1         2    FALSE       8      0 0.9713 0.9577 0.0608 0.0448 26.02 19
#>   loss meets
#> 1    0  TRUE
#> 2    0  TRUE
# }
```
