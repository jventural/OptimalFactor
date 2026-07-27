# Monte Carlo Recovery of a Known Structure with CFA-Boosting

The confirmatory counterpart of
[`simulate_recovery`](https://jventural.github.io/OptimalFactor/reference/simulate_recovery.md).
Data are simulated from a known population, a simple structure model
over every item is handed to
[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
and the question is how often the pipeline removes the items that
deserve removal and only those.

## Usage

``` r
simulate_cfa_recovery(
  n = 500,
  loading = 0.65,
  items_per_factor = 5,
  n_factors = 3,
  phi = 0.3,
  n_cross = 1,
  cross_loading = 0.4,
  n_low = 1,
  low_loading = 0.2,
  n_categories = 5,
  skew = c("symmetric", "skewed"),
  n_reps = 100,
  seed = 2026,
  verbose = TRUE,
  n_cores = 1,
  timeout = 120,
  ...
)

# S3 method for class 'simulate_cfa_recovery'
print(x, ...)
```

## Arguments

- n:

  Sample size(s). Vector values are crossed into conditions. Default
  500.

- loading:

  Population loading(s) of the good items. Default 0.65.

- items_per_factor:

  Number of good items per factor. Default 5.

- n_factors:

  Number of factors in the population model. Default 3.

- phi:

  Interfactor correlation. Default 0.30.

- n_cross:

  Number of cross-loading items added to the pool (they load
  `cross_loading` on factors 1 and 2). Default 1.

- cross_loading:

  Loading of the cross-loading items on both factors. Default 0.40.

- n_low:

  Number of weak items added to the pool. Default 1.

- low_loading:

  Loading of the weak items. Default 0.20.

- n_categories:

  Number of ordered response categories. Default 5.

- skew:

  Category threshold shape: `"symmetric"` (default) or `"skewed"`.

- n_reps:

  Replications per condition. Default 100.

- seed:

  Random seed. Default 2026.

- verbose:

  Print progress per condition. Default `TRUE`.

- n_cores:

  Number of worker processes used to fit the replications of a
  condition. Default 1 (sequential). A crossed design is hundreds of
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  runs, so this is usually the difference between minutes and hours;
  `parallel::detectCores() - 1` is a practical choice. The datasets are
  always generated sequentially from `seed`, so results do not depend on
  the number of cores.

- timeout:

  Seconds allowed per replication. Default 120; `NULL` removes the cap.
  A single pathological dataset can otherwise grind for many minutes and
  stall the condition it belongs to. A capped replication is recorded
  with `stop_reason = "timeout"`. Needs the `R.utils` package.

- ...:

  Ignored.

- x:

  A `simulate_cfa_recovery` object.

## Value

An object of class `simulate_cfa_recovery`: a list with `summary` (one
row per condition), `replications`, `conditions`, `population`, `model`
(the syntax handed to the pipeline) and `call`.

## Details

The question is not the same one the exploratory simulation asks, and
the difference is not cosmetic. In
[`simulate_recovery`](https://jventural.github.io/OptimalFactor/reference/simulate_recovery.md)
the structure is discovered, so recovery means every retained item
landed on its true factor. Here the structure is imposed by the
researcher, so the assignment cannot be wrong by construction and what
is at stake is the item selection: the specified model deliberately
includes the contaminated items, and a pipeline that works removes them
without taking good items with them.

Two further quantities are worth watching. `covs_added` counts the
residual covariances the pipeline introduced: the population has none,
so every one of them is a false positive, which is the modification
index objection of MacCallum, Roznowski and Necowitz (1992) turned into
a number. And `exact` is deliberately strict, requiring the removed set
to equal the contaminated set exactly, because sensitivity and
specificity can both look respectable while no single replication got
the answer right.

The cross loading item is specified on the first factor and the weak
item as well, which is what a researcher would do when writing the model
without knowing which items are faulty.

## See also

[`simulate_recovery`](https://jventural.github.io/OptimalFactor/reference/simulate_recovery.md),
[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  sim <- simulate_cfa_recovery(n = c(200, 500), loading = c(0.50, 0.70),
                               n_reps = 50, n_cores = 6)
  sim
  plot(sim, metric = "sensitivity")
} # }
```
