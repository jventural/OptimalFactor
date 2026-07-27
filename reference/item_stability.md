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
  seed = 2026,
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

  Random seed. Default 2026.

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
if (FALSE) { # \dontrun{
  data(Data_Personality)
  st <- item_stability(Data_Personality, "PPTQ", n_factors = 3, R = 100)
  st                      # printed summary
  st$retention            # per-item detail

  # with the reliability floor active
  item_stability(Data_Personality, "PPTQ", n_factors = 3, R = 100,
                 thresholds = list(min_omega = 0.70))
} # }
```
