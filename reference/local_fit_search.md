# Local-Fit Search for a Unidimensional Model

Searches for a specification of a one-factor model that reaches the fit
targets, combining two actions — dropping items and freeing residual
covariances — and then cleaning up the result.

## Usage

``` r
local_fit_search(
  data,
  items,
  factor_name = "F",
  item_text = NULL,
  rmsea_target = 0.08,
  cfi_target = 0.95,
  srmr_target = 0.08,
  omega_min = 0.7,
  min_loading = 0.35,
  min_items = 5,
  max_ec = 4,
  mi_min = 5,
  start_ec = character(0),
  estimator = "WLSMV",
  ordered = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame with the item responses.

- items:

  Character vector of candidate item names.

- factor_name:

  Name given to the latent variable. Default `"F"`.

- item_text:

  Optional data frame with columns `Item` and `Texto` (or `Text`); used
  only to print the wording of the items involved in each retained
  covariance, so that its substantive plausibility can be judged.

- rmsea_target, cfi_target, srmr_target:

  Fit targets. Defaults `0.08`, `0.95` and `0.08`.

- omega_min:

  Reliability floor. Default `0.70`.

- min_loading:

  Loading floor. Default `0.35`.

- min_items:

  Minimum number of retained items. Default `5`.

- max_ec:

  Maximum number of residual covariances. Default `4`.

- mi_min:

  Minimum modification index for a covariance to be considered. Default
  `5`.

- start_ec:

  Character vector of residual covariances to start from, in lavaan
  syntax (e.g. `"IT1 ~~ IT2"`). Default none.

- estimator, ordered:

  Passed to [`cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- verbose:

  Logical; print progress. Default `TRUE`.

## Value

An object of class `local_fit_search`, a list with `best` (the retained
model and its indices), `log` (one row per action, with its phase),
`ec_detail` (the retained covariances with sign, interval and wording),
`meets` and `config`.

## Details

Applies to the scale that is essentially unidimensional but whose
one-factor model misfits. Where
[`redundancy_short_form`](https://jventural.github.io/OptimalFactor/reference/redundancy_short_form.md)
attacks a single cause (local dependence between near-duplicate items)
by pruning, this routine also considers keeping the pair and modelling
its residual covariance, and decides between the two by their effect on
fit.

Four phases, three of which encode decisions usually taken by hand.

**Phase 0 — sanitation.** Items below the loading floor are removed
*without* requiring that fit improve. An item that does not load cannot
stay, and dropping it often worsens RMSEA temporarily because the
degrees of freedom lost outweigh the chi-square gained. A rule that
demanded immediate improvement would reject the removal and stall the
search before it starts.

**Phase 1 — greedy search.** At each step both actions are evaluated for
every candidate — drop an item, or free one residual covariance — and
the one that most reduces RMSEA is taken.

**Phase 2 — consolidation.** If an item takes part in two or more
residual covariances, the item is the problem and not the relations:
dropping it is attempted, which typically resolves both at once and buys
back a degree of freedom.

**Phase 3 — parsimony.** Each retained covariance is removed in turn;
those that are not needed to keep meeting the targets are discarded.
Every freed parameter is a debt that has to be justified.

What the function does *not* do is judge whether a covariance makes
substantive sense. It returns its estimate, confidence interval, sign
and — when `item_text` is supplied — the wording of both items, so that
decision stays with whoever knows the construct. A *negative* residual
covariance between items at opposite poles of a construct is often the
trace of a dimension that the unidimensional model absorbed, and is
worth reading before treating it as noise.

*Warning.* This is a data-driven search and capitalises on chance
(MacCallum, Roznowski & Necowitz, 1992). It establishes whether an
admissible specification exists; it does not replace validation on an
independent sample.

## References

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.

Saris, W. E., Satorra, A., & van der Veld, W. M. (2009). Testing
structural equation models or detection of misspecifications?
*Structural Equation Modeling, 16*(4), 561-582.

## See also

[`redundancy_short_form`](https://jventural.github.io/OptimalFactor/reference/redundancy_short_form.md),
[`discriminant_boosting`](https://jventural.github.io/OptimalFactor/reference/discriminant_boosting.md),
[`cross_validate_cfa`](https://jventural.github.io/OptimalFactor/reference/cross_validate_cfa.md)

## Examples

``` r
# \donttest{
data(Data_Expectativas)
res <- local_fit_search(Data_Expectativas, paste0("EAF", 1:10),
         factor_name = "Expectations", estimator = "MLR", ordered = FALSE)
#> 
#> [start] n=10 ec=0 | CFI=0.838 RMSEA=0.121 omega=0.851 min_loading=0.419
#> 
#> == Phase 0: sanitation (items below the loading floor) ==
#>    every loading reaches the floor
#> 
#> == Phase 1: greedy search (drop item / add covariance) ==
#>    drop EAF6                n= 9 ec=0 | CFI=0.906 RMSEA=0.097
#>    drop EAF5                n= 8 ec=0 | CFI=0.975 RMSEA=0.055
#>    targets reached
#> 
#> == Phase 2: consolidation (item in 2+ covariances) ==
#>    nothing to consolidate
#> 
#> == Phase 3: parsimony (remove dispensable covariances) ==
#>    no covariances
res
#> 
#> Local-Fit Search
#> ----------------------------------------------------------
#> Solution: 8 items, 0 covariance(s) | MEETS the targets
#>   chi2(20) = 26.1 | CFI = 0.975 | TLI = 0.965 | RMSEA = 0.055 | SRMR = 0.045
#>   omega = 0.875 | minimum loading = 0.580
#>   Items: EAF1, EAF2, EAF3, EAF4, EAF7, EAF8, EAF9, EAF10 
#> 
#> Data-driven search: cross-validate on an independent sample.
res$log
#>       phase    action n_items n_ec   cfi   tli rmsea  srmr omega meets
#> 1  0. start     start      10    0 0.838 0.791 0.121 0.090 0.851 FALSE
#> 2 1. search drop EAF6       9    0 0.906 0.875 0.097 0.067 0.868 FALSE
#> 3 1. search drop EAF5       8    0 0.975 0.965 0.055 0.045 0.875  TRUE
# }
```
