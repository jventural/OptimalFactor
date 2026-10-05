# Resampling Stability of the EFA-Boosting Item Selection

Runs the whole
[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
pipeline on many resamples of the data and records how often each item
survives, how often it is dropped, and how stable its factor assignment
is. This is the empirical answer to the classic objection against
data-driven item purification: that it capitalizes on chance (MacCallum,
Roznowski & Necowitz, 1992). An item removed in 97 out of 100 resamples
is a defensible removal; one removed in 55 is a coin flip dressed as a
decision.

## Usage

``` r
item_stability(
  data,
  name_items,
  n_factors = 3,
  R = 100,
  method = c("subsample", "bootstrap"),
  subsample_frac = 0.8,
  reference = NULL,
  seed = NULL,
  n_cores = 1,
  timeout = 120,
  verbose = TRUE,
  ...
)

# S3 method for class 'item_stability'
print(x, digits = 3, ...)
```

## Arguments

- data:

  Data frame with the item responses.

- name_items:

  Item name prefix, as in
  [`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md).

- n_factors:

  Number of factors to extract. Default 3.

- R:

  Number of resamples. Default 100. Runtime is roughly `R` times the
  cost of a single
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  call, so start small.

- method:

  Resampling scheme: `"subsample"` (default) or `"bootstrap"`.

  Subsampling without replacement is the default because it is what the
  stability selection literature uses (Meinshausen & Buhlmann, 2010) and
  because resampling with replacement damages ordinal data specifically:
  duplicated cases distort the polychoric correlations and thresholds
  the estimator depends on. On a sample of 100 a bootstrap draw can
  leave 63 distinct cases, and the near singular matrix that follows
  sends WLSMV into fits that run for minutes. Measured on
  `Data_Personality` with `R = 40` over 8 cores: 5m46s with 3
  replications hitting the timeout under bootstrap, against 2m31s with
  none under subsampling.

  The cost is that each replication sees `subsample_frac` of the data,
  so the instability reported is mildly conservative: it errs towards
  calling a decision unstable, never towards endorsing one.

- subsample_frac:

  Fraction of rows drawn when `method = "subsample"`. Default 0.8.

- reference:

  Optional
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  result on the full sample, used as the alignment reference. If `NULL`
  (default) it is computed internally.

- seed:

  Optional integer seed for reproducibility; NULL (default) leaves the
  RNG untouched.

- n_cores:

  Number of parallel workers. Default 1 (sequential). Values above 1 use
  a PSOCK cluster and require the package to be installed, not merely
  loaded with
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html).

- timeout:

  Seconds allowed per replication. Default 120; `NULL` removes the cap.
  Resampling puts the pipeline on datasets nobody would analyse by hand:
  a bootstrap draw of `N = 100` can repeat rows until only 63 remain
  distinct, and a WLSMV fit on such a sample can grind for many minutes
  while every other replication waits. A capped replication returns the
  best model reached so far, with `stop_reason = "timeout"`, and is
  counted in the printed summary. Needs the `R.utils` package.

- verbose:

  Print a progress line per replication. Default `TRUE`.

- ...:

  Ignored.

- x:

  An `item_stability` object.

- digits:

  Number of digits for the printed rates. Default 3.

## Value

An object of class `item_stability`: a list with `retention` (one row
per item: times retained, retention and removal rates, modal factor and
factor agreement), `stop_reasons` (table of the stop reason across
replications), `n_removed` (distribution of how many items each
replication dropped), `reference` (the full-sample result), `n_valid`,
`n_failed` and `call`.

## Details

Two resampling schemes are available. `"bootstrap"` draws `n` cases with
replacement, which is the usual choice for assessing sampling
variability. `"subsample"` draws `subsample_frac * n` cases without
replacement, which is more conservative with ordinal data because it
cannot duplicate rare response patterns and therefore triggers fewer
empty categories under WLSMV.

Because rotation labels factors arbitrarily, the factor assignment of
each replication is aligned to a reference solution (the pipeline run
once on the complete sample) before agreement is computed. Alignment is
greedy: factors are matched by the largest overlap of retained items,
most overlapping pair first. `factor_agreement` is then the proportion
of replications, among those where the item was retained, in which the
item landed on its modal factor.

Replications that fail (non-convergence, empty response categories after
resampling) are counted in `n_failed` and excluded from the rates, so
every proportion is computed over successful replications only.

## References

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490–504.

## See also

[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
[`cross_validate_cfa`](https://jventural.github.io/OptimalFactor/reference/cross_validate_cfa.md),
[`simulate_recovery`](https://jventural.github.io/OptimalFactor/reference/simulate_recovery.md)

## Examples

``` r
# \donttest{
data(Data_Expectativas)
st <- item_stability(Data_Expectativas, "EAF", n_factors = 2, R = 3,
                     timeout = 15, seed = 1)
#> Fitting the reference solution on the full sample...
#> Fitting 3 resamples sequentially...
#>   [                          ]   0% (0/3)  elapsed 0s  left ~?       [=========                 ]  33% (1/3)  elapsed 7s  left ~15s       [=================         ]  67% (2/3)  elapsed 9s  left ~5s       [==========================] 100% (3/3)  elapsed 25s  left ~0s     
#>   3 of 3 resamples converged.
#>   1 hit the 15 s cap and report the model reached so far.
st                      # printed summary
#> 
#> EFA-Boosting item stability
#> ------------------------------------------------------------
#> Method: subsample | replications: 3 valid, 0 failed
#> Reference solution removed: EAF7, EAF1
#> 
#> Retention rate per item (lowest first):
#>   item times_retained retention_rate times_removed removal_rate modal_factor
#>   EAF7              0          0.000             3        1.000           NA
#>   EAF1              2          0.667             1        0.333            2
#>   EAF4              2          0.667             1        0.333            2
#>  EAF10              3          1.000             0        0.000            2
#>   EAF2              3          1.000             0        0.000            2
#>   EAF3              3          1.000             0        0.000            1
#>   EAF5              3          1.000             0        0.000            1
#>   EAF6              3          1.000             0        0.000            1
#>   EAF8              3          1.000             0        0.000            2
#>   EAF9              3          1.000             0        0.000            2
#>  factor_agreement in_reference
#>                NA        FALSE
#>                 1        FALSE
#>                 1         TRUE
#>                 1         TRUE
#>                 1         TRUE
#>                 1         TRUE
#>                 1         TRUE
#>                 1         TRUE
#>                 1         TRUE
#>                 1         TRUE
#> 
#> Items removed per replication:
#> 
#> 1 2 3 
#> 1 1 1 
#> 
#> Stop reasons:
#> 
#>               all_criteria_met min_items_per_factor_protected 
#>                              1                              1 
#>                        timeout 
#>                              1 
#> 
#> Unstable decisions (retained in 25-75% of resamples): EAF1, EAF4
st$retention            # per-item detail
#>     item times_retained retention_rate times_removed removal_rate modal_factor
#> 1   EAF7              0          0.000             3        1.000           NA
#> 2   EAF1              2          0.667             1        0.333            2
#> 3   EAF4              2          0.667             1        0.333            2
#> 4  EAF10              3          1.000             0        0.000            2
#> 5   EAF2              3          1.000             0        0.000            2
#> 6   EAF3              3          1.000             0        0.000            1
#> 7   EAF5              3          1.000             0        0.000            1
#> 8   EAF6              3          1.000             0        0.000            1
#> 9   EAF8              3          1.000             0        0.000            2
#> 10  EAF9              3          1.000             0        0.000            2
#>    factor_agreement in_reference
#> 1                NA        FALSE
#> 2                 1        FALSE
#> 3                 1         TRUE
#> 4                 1         TRUE
#> 5                 1         TRUE
#> 6                 1         TRUE
#> 7                 1         TRUE
#> 8                 1         TRUE
#> 9                 1         TRUE
#> 10                1         TRUE
# }
```
