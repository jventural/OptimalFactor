# Heuristic Specification Search for CFA Models

Performs a heuristic specification search over CFA models in the spirit
of MacCallum (1986). For each seed configuration (with a fixed number of
factors), the algorithm runs a hill-climbing loop that evaluates three
local operations at each step: **move** an item between factors,
**drop** an item, and **cov** (add a residual covariance suggested by
modification indices). Optionally, a bifactor variant is fitted for
every seed with k \>= 2. The function returns every configuration
evaluated, the subset that meets the user-supplied fit targets, and the
best model under a composite loss.

## Usage

``` r
specification_search(
  data,
  items,
  seeds = NULL,
  max_factors = 4,
  min_items_factor = 2,
  cfi_target = 0.95,
  rmsea_target = 0.08,
  srmr_target = 0.08,
  max_iter_per_config = 40,
  max_covs = 5,
  max_items_removed = 6,
  try_bifactor = TRUE,
  operations = c("move", "drop", "cov"),
  estimator = "WLSMV",
  ordered = TRUE,
  std.lv = TRUE,
  mi_min = 10,
  mi_top = 3,
  loss_weights = c(rmsea = 0.5, cfi = 0.3, srmr = 0.2),
  patience = 8,
  early_stop_after_meet = 3,
  verbose = TRUE
)

# S3 method for class 'specification_search'
print(x, top = 10, ...)
```

## Arguments

- data:

  Data frame with the observed item responses.

- items:

  Character vector with the names of the items to be searched over.

- seeds:

  Named list of initial configurations. Names are the number of factors
  as character (e.g. `"3"`). Each element is a list of seeds, where
  every seed is a named list `list(F1 = c("it1","it2"), ...)`. If
  `NULL`, default block-partition seeds are generated for k = 1 to
  `max_factors`.

- max_factors:

  Maximum number of factors when `seeds = NULL`.

- min_items_factor:

  Minimum items per factor preserved during move/drop.

- cfi_target, rmsea_target, srmr_target:

  Fit thresholds. A model is flagged as successful if CFI \>=
  `cfi_target` and RMSEA \<= `rmsea_target`.

- max_iter_per_config:

  Maximum hill-climbing iterations per seed.

- max_covs:

  Maximum residual covariances added per configuration.

- max_items_removed:

  Maximum items removed across all factors per configuration.

- try_bifactor:

  If `TRUE`, fit bifactor variant for every k \>= 2 seed.

- operations:

  Subset of `c("move","drop","cov")` controlling enabled ops.

- estimator:

  Estimator passed to
  [`lavaan::cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- ordered:

  If `TRUE`, items are treated as ordered.

- std.lv:

  If `TRUE`, latent variances are fixed to one.

- mi_min:

  Minimum modification-index value considered for covariances.

- mi_top:

  Maximum number of top-MI candidates examined at each step.

- loss_weights:

  Named numeric vector with weights for the composite loss.

- patience:

  Iterations without improvement that stop the hill climb.

- early_stop_after_meet:

  Extra iterations allowed after targets are met.

- verbose:

  Print progress and the MacCallum warning.

- x:

  An object returned by `specification_search`.

- top:

  Number of rows displayed by the print method.

- ...:

  Ignored.

## Details

**Warning (MacCallum, 1986).** Specification search capitalizes on
chance: the more configurations explored, the more likely the resulting
model is sample-specific. If you publish results obtained with this
function you should (a) report the procedure transparently as
exploratory, (b) cross-validate the chosen model with an independent
sample or via bootstrap, and (c) justify each accepted modification on
substantive theoretical grounds. The function prints this warning on
every call unless `verbose = FALSE`.

## Value

An object of class `specification_search`, a list with:

- `table`: data frame, one row per configuration, ordered by loss.

- `successful`: subset of `table` that meets the targets.

- `best`: the best result (`fit`, `syntax`, `factors`, `covs`,
  `indices`, `bifactor`, `meets`, `loss`).

- `results`: every configuration evaluated.

- `call`: the matched call.

## References

MacCallum, R. C. (1986). Specification searches in covariance structure
modeling. *Psychological Bulletin, 100*(1), 107–120.

Saris, W. E., Satorra, A., & van der Veld, W. M. (2009). Testing
structural equation models or detection of misspecifications?
*Structural Equation Modeling, 16*(4), 561–582.

## See also

[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(Data_Personality)
items <- paste0("PPTQ", 1:15)

res <- specification_search(
  data         = Data_Personality,
  items        = items,
  max_factors  = 3,
  estimator    = "MLR",
  ordered      = FALSE,
  try_bifactor = TRUE,
  verbose      = TRUE
)

print(res, top = 8)
res$successful
summary(res$best$fit)
} # }
```
