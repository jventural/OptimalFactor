# Plot a Recovery Simulation

Draws the recovery curve of a
[`simulate_recovery`](https://jventural.github.io/OptimalFactor/reference/simulate_recovery.md)
design: the chosen metric against sample size, one line per population
loading and one panel per number of items per factor. This is the figure
a validation paper reports, so the crossed design is worth running with
enough replications for the curve to be stable.

## Usage

``` r
# S3 method for class 'simulate_recovery'
plot(
  x,
  metric = c("recovery_rate", "sensitivity", "specificity", "convergence_rate",
    "mean_retained"),
  reference = 0.9,
  ...
)
```

## Arguments

- x:

  A `simulate_recovery` object.

- metric:

  Which column of `x$summary` to draw: `"recovery_rate"` (default),
  `"sensitivity"`, `"specificity"`, `"convergence_rate"` or
  `"mean_retained"`.

- reference:

  Horizontal reference line, drawn only for the proportion metrics.
  Default 0.90; `NULL` removes it.

- ...:

  Ignored.

## Value

A `ggplot` object.

## See also

[`simulate_recovery`](https://jventural.github.io/OptimalFactor/reference/simulate_recovery.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  sim <- simulate_recovery(n = c(200, 500, 1000), loading = c(0.50, 0.70),
                           n_reps = 200, n_cores = 6)
  plot(sim)
  plot(sim, metric = "sensitivity")
} # }
```
