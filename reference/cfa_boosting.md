# CFA-Boosting Optimization

Performs iterative optimization of Confirmatory Factor Analysis (CFA)
models using modification indices to improve model fit. The algorithm
automatically adds error covariances based on modification indices while
respecting configurable constraints.

## Usage

``` r
cfa_boosting(
  data,
  model,
  n_sample = NULL,
  thresholds = list(...),
  model_config = list(...),
  mod_indices_config = list(...),
  performance = list(...),
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame containing the observed variables.

- model:

  Character string specifying the CFA model in lavaan syntax.

- n_sample:

  Sample size. If NULL, auto-detected from data.

- thresholds:

  List of fit thresholds:

  - `loading`: Minimum acceptable loading (default 0.30)

  - `min_items_per_factor`: Minimum items per factor (default 3)

  - `rmsea_target`: Target RMSEA value (default 0.08)

  - `cfi_target`: Target CFI value (default 0.95)

  - `srmr_target`: Target SRMR value (default 0.08)

- model_config:

  Model configuration:

  - `estimator`: Estimation method (default "WLSMV")

  - `ordered`: Whether variables are ordered (default TRUE)

- mod_indices_config:

  Modification indices configuration using the Saris, Satorra & van der
  Veld (2009) framework (MI + EPC + Power):

  - `max_covs_to_add`: Maximum covariances to evaluate per iteration
    (default 10)

  - `only_within_factor`: Only consider within-factor covariances
    (default TRUE)

  - `delta`: Minimum misspecification size to detect (default 0.10)

  - `power_threshold`: Threshold for high/low power classification
    (default 0.75)

  - `alpha`: Significance level for the MI test (default 0.05)

- performance:

  Performance settings:

  - `max_iterations`: Maximum optimization iterations (default 30)

  - `timeout_cfa`: Timeout per CFA run in seconds (default 60)

- verbose:

  Logical. Print progress information.

## Value

A list containing:

- `final_model`: The optimized lavaan CFA model object

- `fit_indices`: Final fit indices

- `added_covariances`: List of error covariances added

- `iterations`: Number of iterations performed

- `history`: History of fit indices across iterations

## See also

[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
`print_cfa_boosting`

## Examples

``` r
if (FALSE) { # \dontrun{
# Define CFA model
model <- "
  F1 =~ item1 + item2 + item3 + item4
  F2 =~ item5 + item6 + item7 + item8
"

result <- cfa_boosting(
  data = my_data,
  model = model,
  verbose = TRUE
)
} # }
```
