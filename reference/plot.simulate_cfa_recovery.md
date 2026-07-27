# Plot a CFA Recovery Simulation

Plot a CFA Recovery Simulation

## Usage

``` r
# S3 method for class 'simulate_cfa_recovery'
plot(
  x,
  metric = c("exact_rate", "sensitivity", "specificity", "convergence_rate",
    "mean_retained", "mean_covs_added"),
  reference = 0.9,
  ...
)
```

## Arguments

- x:

  A `simulate_cfa_recovery` object.

- metric:

  Which column of `x$summary` to draw. Default `"exact_rate"`.

- reference:

  Horizontal reference line for the proportion metrics. Default 0.90;
  `NULL` removes it.

- ...:

  Ignored.

## Value

A `ggplot` object.

## See also

[`simulate_cfa_recovery`](https://jventural.github.io/OptimalFactor/reference/simulate_cfa_recovery.md)
