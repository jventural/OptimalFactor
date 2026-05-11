# Print CFA-Boosting Results

Formatted console printer for the object returned by
[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md).
Displays fit indices, the iteration trail, factor loadings (above a
threshold), latent correlations, reliability coefficients and the final
lavaan model syntax.

## Usage

``` r
print_cfa_boosting(
  result,
  show_loadings = TRUE,
  show_correlations = TRUE,
  show_reliability = TRUE,
  show_steps = TRUE,
  show_model = TRUE,
  loading_threshold = 0.30,
  digits = 3
)
```

## Arguments

- result:

  Output from
  [`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md).

- show_loadings:

  Logical. Print standardized factor loadings of the final model.
  Default `TRUE`.

- show_correlations:

  Logical. Print latent factor correlations. Default `TRUE`.

- show_reliability:

  Logical. Print omega and alpha by factor. Default `TRUE`.

- show_steps:

  Logical. Print the per-iteration log of move / drop / cov operations
  and the resulting fit indices. Default `TRUE`.

- show_model:

  Logical. Print the final lavaan model syntax. Default `TRUE`.

- loading_threshold:

  Numeric. Loadings below this absolute value are hidden in the printed
  table. Default `0.30`.

- digits:

  Integer. Number of decimal places. Default `3`.

## Value

Invisibly returns `result`; called for its side effects on the console.

## See also

[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
[`export_cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/export_cfa_boosting.md)
