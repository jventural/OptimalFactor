# Split-Half Stability of Any Item-Selection Algorithm

Answers the question that decides between purification algorithms: does
the algorithm make the same decision on a different sample, and does
that decision hold on data it has never seen? The data are split at
random into two halves many times. On each split the algorithm runs on
the derivation half, and the structure it returns is fitted as a CFA on
the validation half. Unlike
[`item_stability`](https://jventural.github.io/OptimalFactor/reference/item_stability.md)
(tied to
[`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md))
and
[`cross_validate_cfa`](https://jventural.github.io/OptimalFactor/reference/cross_validate_cfa.md)
(one factor only), the algorithm here is any function, so every routine
in the package can be compared on the same footing.

## Usage

``` r
algorithm_stability(
  data,
  algorithm,
  n_splits = 30,
  theory = NULL,
  reference = NULL,
  estimator = "WLSMV",
  ordered = TRUE,
  targets = c(cfi = 0.95, rmsea = 0.08, srmr = 0.08),
  phi_max = 0.9,
  seed = NULL,
  n_cores = 1,
  timeout = 300,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame with the item responses.

- algorithm:

  A function of one argument, the data of the derivation half, returning
  the structure it selects: a named list factor -\> items, a loading
  matrix, or the object returned by
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
  [`cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
  [`discriminant_boosting()`](https://jventural.github.io/OptimalFactor/reference/discriminant_boosting.md),
  [`specification_search_theory()`](https://jventural.github.io/OptimalFactor/reference/specification_search_theory.md)
  or
  [`local_fit_search()`](https://jventural.github.io/OptimalFactor/reference/local_fit_search.md).
  The residual covariances these routines free are kept in the
  validation CFA. Functions from other packages must be called with
  `pkg::fun` inside it when `n_cores > 1`.

- n_splits:

  Number of random splits. Default 30.

- theory:

  Optional named list with the theoretical key, as in
  [`theory_recovery`](https://jventural.github.io/OptimalFactor/reference/theory_recovery.md).

- reference:

  Optional structure used as reference for `jaccard`. Default `NULL`
  runs `algorithm` once on the complete sample.

- estimator, ordered:

  Estimation of the validation CFA. Defaults `"WLSMV"` and `TRUE` (items
  treated as ordinal).

- targets:

  Named vector with the fit targets used for `meets`. Default
  `c(cfi = .95, rmsea = .08, srmr = .08)`.

- phi_max:

  Interfactor correlation at or above which two factors are considered
  indistinguishable; part of `meets`. Default .90.

- seed:

  Optional integer seed for reproducibility; NULL (default) leaves the
  RNG untouched.

- n_cores:

  Number of cores. Values above 1 run the splits on a PSOCK cluster
  (works on Windows). Default 1.

- timeout:

  Seconds allowed for the algorithm on one split; a split that exceeds
  it is counted as failed instead of stalling the whole run. Small
  derivation halves can send an iterative search into a loop that never
  ends. Requires the R.utils package; `NULL` or `Inf` disables it.
  Default 300.

- verbose:

  Print progress. Default `TRUE`.

## Value

An object of class `algorithm_stability`: a list with `summary` (means
over successful splits, plus `meets_rate` and `success_rate`), `splits`
(one row per split), `item_retention`, `reference` (the reference
partition) and `call`.

## Details

An algorithm that purifies items on the same data used to judge them
capitalises on chance (MacCallum, Roznowski & Necowitz, 1992): the fit
it reports is optimistic, and a different sample may have led it
elsewhere. Two kinds of evidence are therefore recorded on every split:

- Replication of the decision:

  `jaccard`, the overlap between the items retained on the derivation
  half and those retained by the reference solution (the algorithm run
  once on the complete sample), and `ari_reference`, the agreement of
  the two partitions on their common items. `item_retention` gives, for
  each item, the proportion of splits in which it survived: an item
  dropped in 29 of 30 splits is a defensible removal, one dropped in 16
  is a coin flip.

- Out-of-sample adequacy:

  CFI, TLI, RMSEA, SRMR and the largest interfactor correlation of the
  derived structure fitted on the validation half, and `meets`, whether
  all the targets are met there.

When `theory` is given,
[`theory_recovery`](https://jventural.github.io/OptimalFactor/reference/theory_recovery.md)
is applied to every derived structure, so the stability of the recovery
of the theory is reported as well.

The halves are drawn on the master, so results are reproducible with
`seed` and identical for any `n_cores`.

## References

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.
[doi:10.1037/0033-2909.111.3.490](https://doi.org/10.1037/0033-2909.111.3.490)

## See also

[`theory_recovery`](https://jventural.github.io/OptimalFactor/reference/theory_recovery.md),
[`item_stability`](https://jventural.github.io/OptimalFactor/reference/item_stability.md).

## Examples

``` r
# \donttest{
data(Data_Expectativas)
items  <- paste0("EAF", 1:10)
theory <- list(F1 = paste0("EAF", 1:5), F2 = paste0("EAF", 6:10))
model  <- paste(sapply(names(theory), function(f)
            paste(f, "=~", paste(theory[[f]], collapse = " + "))), collapse = "\n")

st <- algorithm_stability(
  Data_Expectativas[, items],
  algorithm = function(d) cfa_boosting(d, model = model, verbose = FALSE,
                model_config = list(estimator = "MLR", ordered = FALSE)),
  n_splits = 3, theory = theory, estimator = "MLR", ordered = FALSE,
  seed = 1)
#> Reference: running the algorithm on the complete sample (N = 100)...
#> Running 3 splits (derivation n = 50, validation n = 50)...
#>   [                          ]   0% (0/3)  elapsed 0s  left ~?       [=========                 ]  33% (1/3)  elapsed 1s  left ~2s       [=================         ]  67% (2/3)  elapsed 3s  left ~1s       [==========================] 100% (3/3)  elapsed 5s  left ~0s     
st
#> Split-half stability of the item selection
#> Splits: 3 (successful: 100%)
#> 
#> Means over successful splits (fit on the validation half):
#>  k n_items   cfi   tli rmsea  srmr phi_max jaccard ari_reference recovery
#>  2       7 0.888 0.819 0.128 0.074   0.887   0.857             1      0.7
#>  ari_theory meets_rate success_rate
#>           1      0.333            1
#> 
#> Items whose fate is not settled (retained in 20-80% of splits):
#>   item in_reference retention
#>   EAF5        FALSE      0.33
#>  EAF10        FALSE      0.67
# }
```
