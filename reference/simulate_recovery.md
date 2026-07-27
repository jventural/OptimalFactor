# Monte Carlo Recovery of a Known Factor Structure

Simulates ordinal data from a known population model, runs the
[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
pipeline on each replication, and reports how often the algorithm
recovers the true structure. Conditions can be crossed over sample size,
loading size and number of items per factor, which is what turns this
into evidence about *when* the method works rather than a single
anecdote.

## Usage

``` r
simulate_recovery(
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
  n_factors_fitted = NULL,
  ...
)

# S3 method for class 'simulate_recovery'
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

- n_factors_fitted:

  Number of factors the pipeline is told to extract. Default `NULL`,
  meaning the true `n_factors`. Vector values are crossed into
  conditions, which is how the question a reviewer will ask gets
  answered: every other condition here hands the algorithm the true
  number of factors, and in practice nobody knows it. Fitting fewer
  factors than the population has cannot recover the structure by
  definition, so `recovery_rate` goes to zero and the informative
  columns become `sensitivity` and `specificity`: what the pipeline does
  to the items when the dimensionality is wrong.

- ...:

  Ignored.

- x:

  A `simulate_recovery` object.

## Value

An object of class `simulate_recovery`: a list with `summary` (one row
per condition: recovery rate, mean sensitivity and specificity, mean
items retained, convergence rate), `replications` (the raw
per-replication data frame), `conditions`, `population` (the loading
matrix, factor correlations and item roles of the last condition) and
`call`.

## Details

The population model has `n_factors` correlated factors (correlation
`phi`), `items_per_factor` good items with loading `loading`, plus
optional contaminated items: `n_cross` items that load `cross_loading`
on two factors and `n_low` items whose loading is `low_loading`.
Continuous responses are drawn from the implied correlation matrix and
discretized into `n_categories` ordered categories, either symmetrically
or with a skew.

Four quantities are recorded per replication:

- `recovered`:

  TRUE when every retained good item sits on the factor it truly belongs
  to, after aligning labels (rotation names factors arbitrarily), and no
  factor was lost.

- `sensitivity`:

  proportion of contaminated items correctly removed. `NA` when the
  condition has no contaminated items.

- `specificity`:

  proportion of good items correctly retained.

- `n_retained`, `rmsea`, `stop_reason`:

  final size, fit and why the pipeline stopped.

Sensitivity and specificity are the honest way to read the pipeline: an
algorithm that removes everything scores a perfect sensitivity and a
terrible specificity, so both must be read together.

## See also

[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
[`item_stability`](https://jventural.github.io/OptimalFactor/reference/item_stability.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # single condition, quick check
  simulate_recovery(n = 300, n_reps = 20)

  # crossed design for a validation paper
  sim <- simulate_recovery(n = c(200, 500, 1000), loading = c(0.50, 0.70),
                           items_per_factor = 5, n_reps = 200)
  sim$summary
} # }
```
