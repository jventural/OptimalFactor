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
  thresholds = list(loading = 0.3, min_items_per_factor = 3,
    rmsea_target = 0.08, cfi_target = 0.95, srmr_target = 0.08,
    enforce_loading = TRUE, cross_loading = 0.3,
    enforce_simple_structure = TRUE),
  model_config = list(estimator = "WLSMV", ordered = TRUE),
  mod_indices_config = list(max_covs_to_add = 10, only_within_factor = TRUE,
    delta = 0.1, power_threshold = 0.75, alpha = 0.05),
  performance = list(max_iterations = 30, timeout_cfa = 60),
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

  - `enforce_loading`: Treat `loading` as an admissibility floor rather
    than a hint (default `TRUE`). An item whose standardized loading
    falls below it is removed even when the fit targets are already met,
    unless removing it would breach `min_items_per_factor`. `FALSE`
    restores the behaviour of versions up to 1.3.0, where the loop
    stopped as soon as the fit targets were satisfied and the loadings
    were never inspected. Global fit does not reveal a weak item: in
    simulation an item loading .20 in the population sat comfortably
    inside a model with RMSEA = .03. Enforcing the floor can leave
    global fit slightly worse while making the retained set defensible,
    which is the trade this option makes explicit.

  - `cross_loading`: Standardized magnitude above which an item is
    declared to load on a foreign factor (default 0.30).

  - `enforce_simple_structure`: Remove items that load on a factor other
    than their own (default `TRUE`). The loading floor catches the item
    that measures nothing; this catches the one that measures two
    things, which the floor cannot see because a cross-loading item
    still loads acceptably on its own factor, and which global fit does
    not reveal either: an omitted cross-loading is absorbed by the
    interfactor correlation, so in simulation six items cross-loading at
    .60 left RMSEA at .053 and CFI at .997. Detection uses the
    modification index of the absent loading, but never significance
    alone: the standardized expected parameter change must also reach
    `cross_loading`, since a significant index with a trivial EPC is
    precisely the capitalization on chance this is meant to avoid.

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
[`print_cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/print_cfa_boosting.md)

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
