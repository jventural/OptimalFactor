# Export CFA Boosting Results to Structured Data Frames

Export CFA Boosting results to structured data frames suitable for
saving as CSV, embedding in a manuscript table, or further analysis.

## Usage

``` r
export_cfa_boosting(result)
```

## Arguments

- result:

  Output from
  [`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md).

## Value

A named list of data frames (`fit_indices`, `standardized_loadings`,
`factor_correlations`, `reliability`, `steps_log`).

## See also

[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
[`print_cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/print_cfa_boosting.md)

## Examples
