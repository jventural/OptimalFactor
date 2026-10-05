# Theory-Guided Specification Search for CFA Models

Theory-aware extension of
[`specification_search`](https://jventural.github.io/OptimalFactor/reference/specification_search.md).
The classic specification search (MacCallum, 1986) is driven purely by
fit: its loss only looks at RMSEA/CFI/SRMR, so it "capitalizes on
chance" and can drift toward models that fit but break the intended
theory (moving items to the wrong factor, or dropping core items of the
construct). This function adds a **theory-congruence** term to the loss
that penalizes departures from the theoretical item-to-factor assignment
and penalizes dropping theoretical items. A single parameter
`theory_weight` grades how much theory counts relative to fit (`0`
reproduces the fit-only search; larger values make the search
progressively more conservative).

## Usage

``` r
specification_search_theory(
  data,
  items,
  theory,
  theory_weight = 0.5,
  tw_move = 0.6,
  tw_drop = 0.4,
  tw_k = 0,
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
  only_within_factor = FALSE,
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

  Character vector with the item names to search over (must all exist in
  `data`).

- theory:

  Named list encoding the theoretical structure, of the form
  `list(FactorA = c("it1","it2",...), FactorB = c(...))`. Items not
  listed are treated as having no theoretical factor (neither rewarded
  nor penalized).

- theory_weight:

  Non-negative scalar weighting the theory term against the fit term.
  `0` reproduces the fit-only search; typical values 0.5-1 recover
  theory-congruent models with equivalent fit. Default 0.5.

- tw_move, tw_drop, tw_k:

  Internal weights of the theory loss: penalty for misassigned items,
  for dropped theoretical items, and for deviating from the theoretical
  number of factors. Defaults 0.6, 0.4, 0.0.

- seeds, max_factors, min_items_factor, cfi_target, rmsea_target,
  srmr_target:

  As in
  [`specification_search`](https://jventural.github.io/OptimalFactor/reference/specification_search.md).
  If `seeds = NULL`, the theoretical structure plus block-partition
  seeds are generated.

- max_iter_per_config, max_covs, max_items_removed, try_bifactor,
  operations:

  As in
  [`specification_search`](https://jventural.github.io/OptimalFactor/reference/specification_search.md).

- estimator, ordered, std.lv, mi_min, mi_top, loss_weights, patience,
  early_stop_after_meet, verbose:

  As in
  [`specification_search`](https://jventural.github.io/OptimalFactor/reference/specification_search.md).

- only_within_factor:

  Restrict the `cov` operation to pairs of items belonging to the same
  factor. Default `FALSE`, which reproduces the behaviour of earlier
  versions.

  A residual correlation between items of *different* factors is rarely
  what it looks like. It is usually the signature of an omitted
  cross-loading, a misassigned item or a missing factor, and it absorbs
  covariance that belongs at the factor level: the estimated interfactor
  correlation drops and discriminant validity looks better than it is.
  Measured on a two-factor scale, freeing two cross-factor residuals
  moved the factor correlation from .956 to .919 and pulled the upper
  bound of its confidence interval from 1.031 down to .999, which is the
  difference between two factors that cannot be told apart and two that
  can.

  `TRUE` is the conservative choice. It does not make cross-factor
  residuals wrong in principle, since item adjacency and shared method
  can justify them, but it forces those to be added deliberately rather
  than by modification index.

- n_cores:

  Number of CPU cores for parallel evaluation of the candidate models
  within each greedy iteration (which are independent). Default 1
  (sequential). Values \> 1 use a PSOCK cluster (works on Windows, where
  `fork` is unavailable); results are identical to the sequential run,
  only faster. A practical choice is `parallel::detectCores() - 1`.

## Value

An object of class `specification_search` (so
`print.specification_search` applies), a list with `table` (one row per
evaluated configuration, including the extra columns `congruence` and
`theory_loss`), `successful`, `best` (the lowest-total-loss model, with
its `congruence` and `theory_loss`), `results`, `theory_weight` and
`call`.

## Details

The total loss minimized for each candidate model is \$\$L = L\_{fit} +
\code{theory\\weight} \cdot L\_{theory}\$\$ where \\L\_{fit}\\ is the
composite fit loss of `specification_search` \$\$L\_{fit} =
w\_{rmsea}\max(0,(RMSEA-t\_{rmsea})/0.03) +
w\_{cfi}\max(0,(t\_{cfi}-CFI)/0.03) +
w\_{srmr}\max(0,(SRMR-t\_{srmr})/0.03)\$\$ and the theory loss is
\$\$L\_{theory} = \code{tw\\move}\cdot p\_{mis} + \code{tw\\drop}\cdot
p\_{drop} + \code{tw\\k}\cdot \|k - k\_{theory}\|/k\_{theory}\$\$ with
\\p\_{mis}\\ the proportion of retained items assigned to a factor that
does not match their theoretical dimension, and \\p\_{drop}\\ the
proportion of theoretical items dropped from the model. Empirical
factors are aligned to theoretical dimensions greedily by maximum item
overlap (each factor inherits the most frequent theoretical label among
its items), the same criterion used by the project's loading maps. The
reported `congruence` is \\1 - p\_{mis}\\ (1 = the model fully respects
the theory). Default seeds include the theoretical structure itself as a
starting point.

**Warning.** Like the fit-only search, this remains an EXPLORATORY
device; cross-validate the chosen model on an independent sample.

## References

MacCallum, R. C. (1986). Specification searches in covariance structure
modeling. *Psychological Bulletin, 100*(1), 107–120.

## See also

[`specification_search`](https://jventural.github.io/OptimalFactor/reference/specification_search.md)

## Examples

``` r
# \donttest{
data(Data_Expectativas)
items  <- paste0("EAF", 1:10)
theory <- list(F1 = paste0("EAF", 1:5), F2 = paste0("EAF", 6:10))
guided <- specification_search_theory(Data_Expectativas, items, theory,
            theory_weight = 0.5, max_factors = 2, max_iter_per_config = 5,
            try_bifactor = FALSE, estimator = "MLR", ordered = FALSE)
#> 
#> ==========================================================================
#>  Specification Search TEORICA (MacCallum 1986 + congruencia teorica)
#> ==========================================================================
#>  theory_weight = 0.50  | k_teorico = 2 | items teoricos = 10
#>  ADVERTENCIA: sigue siendo exploratorio; valide en muestra independiente.
#> ==========================================================================
#> 
#> --- k = 1 factor(s) ---
#>   Config: k1_s1 (standard)
#>     [k1_s1] iter 1: DROP EAF6 from G -> CFI=0.906 RMSEA=0.097 congr=0.56 loss=0.878
#>     [k1_s1] iter 2: COV EAF3 ~~ EAF5 (MI=21.5) -> CFI=0.960 RMSEA=0.064 congr=0.56 loss=0.153
#>     [k1_s1] iter 3: DROP EAF7 from G -> CFI=0.958 RMSEA=0.075 congr=0.62 loss=0.152
#>     [k1_s1] iter 4: DROP EAF8 from G -> CFI=0.993 RMSEA=0.032 congr=0.71 loss=0.146
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
#>     [k2_s1] iter 1: DROP EAF6 from F2 -> CFI=0.914 RMSEA=0.095 congr=1.00 loss=0.633
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>     [k2_s1] iter 2: COV EAF3 ~~ EAF5 (MI=19.7) -> CFI=0.960 RMSEA=0.066 congr=1.00 loss=0.020
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
#> Modelos evaluados: 2 
guided$table[, c("config", "cfi", "rmsea", "congruence", "loss")]
#>   config    cfi  rmsea congruence   loss
#> 1  k2_s1 0.9604 0.0656      1.000 0.0200
#> 2  k1_s1 0.9932 0.0319      0.714 0.1457
guided$best$factors
#> $F1
#> [1] "EAF1" "EAF2" "EAF3" "EAF4" "EAF5"
#> 
#> $F2
#> [1] "EAF7"  "EAF8"  "EAF9"  "EAF10"
#> 
# }
```
